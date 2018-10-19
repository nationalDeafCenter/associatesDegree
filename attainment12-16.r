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
print(nrow(hearAssP <- filter(hear,attain=='Associates degree')))

rm(deaf,hear); gc()

## for sex, race/eth, disability type, field of degree
## estimate percent, med. income, employment



