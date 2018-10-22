library(readr) ## read in the csvs faster
library(dplyr)
library(openxlsx)
source('generalCode/estimationFunctions.r')
source('generalCode/median.r')



#1) a simple breakdown of current enrollment, and completion data, across type of institution (4 year colleges, community colleges, etc) using all the 'type of institution' data we have, so that would give us some nice descriptives and allow us to make a final decision on how we want to categorize 'community colleges and 2-year institutions'

## Enrollment & Completion (ages 25-45, ages 25-64)

## Some College (less 1y v more than 1y)
## Associates Degree
## Bachelor's Degree
## Professional/Graduate
## Doctorate

## With AA degrees...
## By disability type
## Race & ethnicity
## Field of degree
## Median income
## Employment rates by field

## load in dataset
source('make5yrDat.r')
save(sdat,file='fiveYearDat.RData')

sdat$raceEth <- factor(sdat$raceEth)
sdat$fod <- factor(sdat$fod)

sdat <- mutate(sdat, young=agep<46)

deaf <- filter(sdat,deaf==1)
hear <- filter(sdat,deaf==0)

rm(sdat); gc()

### overall attainment estimates
attain25.64 <- list(deaf=list(),hear=list())
for(lev in c('hs','sc','cc','ba','grad','doc')){
    attain25.64$deaf[[lev]] <-
        estSE(deaf[[lev]],deaf$pwgtp,deaf[,paste0('pwgtp',1:80)])
    attain25.64$hear[[lev]] <-
        estSE(hear[[lev]],hear$pwgtp,hear[,paste0('pwgtp',1:80)])
}

attain25.45 <- list(deaf=list(),hear=list())
for(lev in c('hs','sc','cc','ba','grad','doc')){
    attain25.45$deaf[[lev]] <-
        estSEstr(lev,'pwgtp',paste0('pwgtp',1:80),'young',deaf)
    attain25.45$hear[[lev]] <-
        estSEstr(lev,'pwgtp',paste0('pwgtp',1:80),'young',hear)
}
gc()

formatRes <- function(x,deaf=TRUE){
    res <- do.call('rbind',x)
    res <- cbind(res[,1:2],c(1-res[1,1],1-res[-1,1]/res[-nrow(res),1]),res[,3])
    colnames(res) <- paste(ifelse(deaf,'Deaf','Hearing'),
                        c('Percent','SE','Percent Lost','n'))
    res[,grep('Percent|SE',colnames(res))] <-
        round(res[,grep('Percent|SE',colnames(res))]*100,1)
    res
}

attain <- function(res){
    res <- cbind(formatRes(res$deaf,deaf=TRUE),
                 formatRes(res$hear,deaf=FALSE))
    res <- as.data.frame(res)
    rownames(res) <- c('HS/GED','Some College','Associates Degree',
                       'Bachelors Degree','Graduate/Professional Degree',
                       'Doctorate')
    res
}

write.xlsx(list("Age 25-64"=attain(attain25.64),"Age 25-45"=attain(attain25.45)),
           'EducationalAttainment2012-2016.xlsx',colWidths=c("auto","auto"),
           row.names=TRUE)



#### break it down, associates degree

print(nrow(deafAssP <- filter(deaf,cc)))
print(nrow(deafAss <- filter(deaf,attain=='Associates degree')))
print(nrow(hearAssP <- filter(hear,cc)))
print(nrow(hearAss <- filter(hear,attain=='Associates degree')))

rm(deaf,hear); gc()

## for sex, race/eth, disability type, field of degree
## estimate percent, med. income, employment

propEst <- function(subsets,sdat,groupNames){
    out <- NULL
    for(subst in subsets)
        out <- rbind(out,estSEstr(subst,sdat=sdat)*c(100,100,1))
    out <- as.data.frame(out)
    names(out) <- c('Percent','SE','Sample Size')
    out <- cbind(subgroup=if(missing(groupNames)) subsets else groupNames,out)
    out
}

moneyEst <- function(subsets,sdat,groupNames){
    if(missing(groupNames)) groupNames <- subsets
    earn <- svby('earn',subsets=subsets,FUN=medStr,sdat=sdat)
    colnames(earn) <- c('Subgroup Percent','Median Earnings','Earnings SE','n')
    inc <- svby('inc',subsets=subsets,FUN=medStr,sdat=sdat,prop=FALSE)
    colnames(inc) <- c('Median Income','Income SE','n')
    out <- as.data.frame(cbind(earn[,-ncol(earn)],inc))
    out <- cbind(Subgroup=groupNames,out)
    rownames(out) <- NULL
    out
}

employEst <- function(subsets,sdat){
    groupNames <- if(!is.null(names(subsets))) names(subsets) else subsets

    ocs <- c('employed','unemployed','esr==6','fulltime')

    out <- sapply(ocs,function(oo) svby(oo,subsets=subsets,FUN=estSEstr,sdat=sdat),simplify=FALSE)

    p <- out[[1]][,1]
    n <- out[[1]][,ncol(out[[1]])]
    out <- sapply(out,function(x) x[,-c(1,ncol(x))],simplify=FALSE)
    out <- do.call('cbind',out)*100
    colnames(out) <- outer(c('Percent','SE'),Hmisc::capitalize(gsub('esr==6','unemployed',ocs)),paste)
    out <- as.data.frame(out)
    out <- cbind(Subgroup=groupNames,`Subgroup Percent`=p,out,`Sample Size`=n)
    rownames(out) <- NULL

    out
}


eachOut <- function(domain,subsets,outcome,young){
    if(young) subsets <- paste0(subsets,'&young')
    fun <- switch(outcome,'Subgroup Proportions'=propEst,'Income'=moneyEst,'Employment'=employEst)
    dfend <- switch(domain,"Associates Degree"="Ass","Associates Degree Or Higher"="AssP")
    dd <- fun(subsets,get(paste0('deaf',dfend),envir=.GlobalEnv))
    hh <- fun(subsets,get(paste0('hear',dfend),envir=.GlobalEnv))

    dd[,sapply(dd,is.numeric)] <- round(dd[,sapply(dd,is.numeric)],1)
    hh[,sapply(hh,is.numeric)] <- round(hh[,sapply(hh,is.numeric)],1)

    rbind(
        c('','Deaf',rep('',ncol(dd)-2),'Hearing',rep('',ncol(hh)-2)),
        c(names(dd),names(hh)[-1]),
        cbind(as.matrix(dd),as.matrix(hh)[,-1]))
}

raceEthSubs <- paste0('raceEth=="',levels(deafAss$raceEth),'"')
names(raceEthSubs) <- levels(deafAss$raceEth)

fodSubs <- paste0('fod=="',levels(deafAss$fod),'"')
names(fodSubs) <- levels(deafAss$fod)
fodSubs <- c(fodSubs,'STEM Field'='sciengp==1','STEM-Related Field'='sciengrlp==1')

for(domain in c('Associates Degree','Associates Degree Or Higher')){
    for(outcome in c('Subgroup Proportions','Income','Employment')){
        for(young in c(TRUE,FALSE)){
            cat(domain,' ',outcome,' ',young,'\n')
            out <- list(
                "By Sex"=eachOut(domain=domain,subsets=c(Male="sex==1",Female="sex==2"),outcome=outcome,young=young),
                "By Race/Ethnicity"=eachOut(domain=domain,subsets=raceEthSubs,outcome=outcome,young=young),
                "By Disability Type"=eachOut(domain=domain,
                                             subsets=c('No Other Disability'="diss==0",
                                                       'Any Other Disability'="diss==1",
                                                       'Blind'='blind==1'),outcome=outcome,young=young))#,
           # "By Field of Degree"=eachOut(domain=domain,fodSubs,outcome=outcome))
        write.xlsx(out,paste0(domain,'-',outcome,'Ages ',ifelse(young,'25-45','25-64')),colNames=FALSE,rowNames=FALSE,
                   colWidths=rep('auto',length(out)))
    }
}


deafAss <- filter(deafAss,young)
deafAssP <- filter(deafAssP,young)
hearAss <- filter(hearAss,young)
hearAss <- filter(hearAssP,young)

for(domain in c('Associates Degree','Associates Degree Or Higher')){
    for(outcome in c('Subgroup Proportions','Income','Employment')){
        out <- list(
            "By Sex"=eachOut(domain=domain,subsets=c(Male="sex==1",Female="sex==2"),outcome=outcome),
            "By Race/Ethnicity"=eachOut(domain=domain,subsets=raceEthSubs,outcome=outcome),
            "By Disability Type"=eachOut(domain=domain,
                                         subsets=c('No Other Disability'="diss==0",
                                                   'Any Other Disability'="diss==1",
                                                   'Blind'='blind==1'),outcome=outcome),
            "By Field of Degree"=eachOut(domain=domain,fodSubs,outcome=outcome))
        write.xlsx(out,paste0(domain,'-',outcome,'Ages 25-45'),colNames=FALSE,rowNames=FALSE,
                   colWidths=rep('auto',length(out)))
    }
}
