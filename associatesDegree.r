library(gridExtra)
library(ggplot2)
library(cluster)
library(readr) ## read in the csvs faster
library(survey)
library(dplyr)
library(openxlsx)
states <- read.csv('../../data/acs5yr2016/states.csv')

#1) a simple breakdown of current enrollment, and completion data, across type of institution (4 year colleges, community colleges, etc) using all the 'type of institution' data we have, so that would give us some nice descriptives and allow us to make a final decision on how we want to categorize 'community colleges and 2-year institutions'



makeDat <- function(){

    pVars <- c('SERIALNO','DEAR','ST','AGEP','SCHL','SCH','PWGTP',paste0('pwgtp',1:80))



## need: DEAR, attain, employment,PERNP, fulltime

    sdat <- read_csv('../csv_pus/ss16pusa.csv')
    sdat <- sdat[,pVars]
    for(nn in names(sdat)) if(is.character(sdat[[nn]])) sdat[[nn]] <- parse_integer(sdat[[nn]])
    gc()
    sdat2 <-  read_csv('../csv_pus/ss16pusb.csv')
    sdat2 <- sdat2[,pVars]
    for(nn in names(sdat)) if(is.character(sdat2[[nn]])) sdat2[[nn]] <- parse_integer(sdat2[[nn]])
    sdat <- rbind(sdat,sdat2)
    rm(sdat2)
    gc()


    sdat$state <- states$abb[match(sdat$ST,states$x)]

    sdat <- sdat[sdat$AGEP>=20,]

    sdat <- droplevels(sdat)

		## 01 .No schooling completed
		## 02 .Nursery school, preschool
		## 03 .Kindergarten
		## 04 .Grade 1
		## 05 .Grade 2
		## 06 .Grade 3
		## 07 .Grade 4
		## 08 .Grade 5
		## 09 .Grade 6
		## 10 .Grade 7
		## 11 .Grade 8
		## 'Grade 9
		## 'Grade 10
		## 'Grade 11
    ## '

    edlevs <- c(
        '<Grade 10',
        'Grade 10',
        'Grade 11',
        '12th grade - no diploma',
        'Regular high school diploma',
        'GED or alternative credential',
        'Some college, but less than 1 year',
        '1 or more years of college credit, no degree',
        'Associates degree',
        'Bachelors degree',
        'Masters degree',
        'Professional degree beyond a bachelors degree',
        'Doctorate degree')

    sdat$attain <- ifelse(sdat$SCHL<13,1,sdat$SCHL-11)
    sdat$attain <- factor(edlevs[sdat$attain],levels=edlevs,ordered=TRUE)

    sdat
}

### overall


svmean <- function(x,w,na.rm=TRUE){
    w <- w/sum(w)
    sum(x*w,na.rm=na.rm)
}

estSE <- function(x,w1,wrep,na.rm=TRUE){
    est <- svmean(x,w1,na.rm)

    reps <- apply(wrep,2,function(w) svmean(x,w,na.rm))

    se <- sqrt(mean((reps-est)^2)*4)

    n <- if(na.rm) sum(!is.na(x)) else length(x)

    c(est*100,se*100,n)
}

estExpr <- function(expr,subst,na.rm=TRUE){
    expr <- enquo(expr)

    if(!missing(subst)){
        subst <- enquo(subst)
        sdat <- filter(sdat,!!subst)
    }

    x <- transmute(sdat,x=!!expr)$x

    estSE(x,sdat$PWGTP,sdat[,paste0('pwgtp',1:80)],na.rm)
}


### data
sdat <- makeDat()

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
