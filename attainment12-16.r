library(readr) ## read in the csvs faster
library(dplyr)
library(openxlsx)



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
