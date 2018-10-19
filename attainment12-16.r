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
        estSEstr(lev,'pwgtp',paste0('pwgtp',1:80),young,deaf)
    attain25.45$hear[[lev]] <-
        estSEstr(lev,'pwgtp',paste0('pwgtp',1:80),young,hear)
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


deafEx <- list()
hearEx <- list()

### overall
deafEx$overall <- estExpr(attain=='Associates degree',subst=DEAR==1)
hearEx$overall <- estExpr(attain=='Associates degree',subst=DEAR==2)

## 25--64
deafEx$a25.64 <- estExpr(attain=='Associates degree',subst=DEAR==1&AGEP>24&AGEP<65)
hearEx$a25.64 <- estExpr(attain=='Associates degree',subst=DEAR==2&AGEP>24&AGEP<65)

## age range
sdat <- sdat%>%mutate(ageRange=factor(ifelse(AGEP<25,'20-25',
                                      ifelse(AGEP<30,'25-29',
                                      ifelse(AGEP<40,'30-39',
                                      ifelse(AGEP<50,'40-49',
                                      ifelse(AGEP<65,'50-64','64+')))))))
for(age in levels(sdat$ageRange)){
    deafEx[[age]] <- estExpr(attain=='Associates degree',subst=DEAR==1&ageRange==age)
    hearEx[[age]] <- estExpr(attain=='Associates degree',subst=DEAR==2&ageRange==age)
}

deafCum <- list()
hearCum <- list()

### overall
deafCum$overall <- estExpr(attain>='Associates degree',subst=DEAR==1)
hearCum$overall <- estExpr(attain>='Associates degree',subst=DEAR==2)

## 25--64
deafCum$a25.64 <- estExpr(attain>='Associates degree',subst=DEAR==1&AGEP>24&AGEP<65)
hearCum$a25.64 <- estExpr(attain>='Associates degree',subst=DEAR==2&AGEP>24&AGEP<65)

## age range

for(age in levels(sdat$ageRange)){
    deafCum[[age]] <- estExpr(attain>='Associates degree',subst=DEAR==1&ageRange==age)
    hearCum[[age]] <- estExpr(attain>='Associates degree',subst=DEAR==2&ageRange==age)
}

save(deafEx,hearEx,deafCum,hearCum,file='associates.RData')

ex <- cbind(do.call('rbind',deafEx),do.call('rbind',hearEx))
ex <- as.data.frame(round(ex,1))
names(ex) <- c('Deaf','Deaf SE','Deaf N','Hearing','Hearing SE','Hearing N')
ex <- cbind(Age=c('20+','25-64',rownames(ex)[-c(1:2)]),ex)




cum <- cbind(do.call('rbind',deafCum),do.call('rbind',hearCum))
cum <- as.data.frame(round(cum,1))
names(cum) <- c('Deaf','Deaf SE','Deaf N','Hearing','Hearing SE','Hearing N')
cum <- cbind(Age=c('20+','25-64',rownames(cum)[-c(1:2)]),cum)


openxlsx::write.xlsx(list(`Highest Level Asc.`=ex,`At Least Asc.`=cum),'AssociatesDegrees.xlsx')
