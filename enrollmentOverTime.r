###'current enrollment', year by year from 2008 to 2016 and get us the % of currently enrolled 2-year college students who are deaf and how that has changed over time. (within currently enrolled students at 2-year institutions)
### actually, since we can't differentiate between 2-year and 4-year students, we'll pool them together

library(readr)
library(dplyr)
library(ggplot2)
library(scales)
source('generalCode/estimationFunctions.r')

varNames <- tolower(c('PWGTP',paste0('PWGTP',1:80),'AGEP','DEAR','SCH','SCHG','SCHL'))



loadDatOneYear <- function(YR){
    yr <- if(is.integer(YR)){ if(YR<10) paste0('0',YR) else(paste0(YR)) } else YR
    firstTry <- read_csv(paste0('../../data/byYear/ss',yr,'pusa.csv'), n_max=5)
    colTypes <- paste(ifelse(tolower(names(firstTry))%in%varNames,'i','-'),collapse='')

    datA <- read_csv(paste0('../../data/byYear/ss',yr,'pusa.csv'),col_types=colTypes)
    names(datA) <- tolower(names(datA))
    stopifnot(all.equal(sort(names(datA)),sort(varNames)))

    datB <- read_csv(paste0('../../data/byYear/ss',yr,'pusb.csv'),col_types=colTypes)
    names(datB) <- tolower(names(datB))
    dat <- rbind(datA[,varNames],datB[,varNames])

    rm(datA,datB); gc()

    dat <- dat%>%filter(agep>17, agep<65,sch>1,schg==15)%>%
        mutate( young=agep<46, deaf=dear==1)


    attr(dat,'year') <- as.integer(YR)


    dat
}

estOneYear <- function(dat){
    out <- list(full=list(),young=list())

    out$full <- list(
        totEnrolled=svTot(dat),
        totDeaf=svTot(dat,'dear==1'),
        percDeaf=estSEstr('dear==1',sdat=dat),
        tot1stYear=svTot(dat,'schl<=18'),
        totDeaf1stYear=svTot(dat,'schl<=18&dear==1'),
        percDeaf1stYear=estSEstr('dear==1',subst='schl<=18',sdat=dat))
    out$young <- list(
        totEnrolled=svTot(dat,'young'),
        totDeaf=svTot(dat,'dear==1&young'),
        percDeaf=estSEstr('dear==1',sdat=dat,subst='young'),
        tot1stYear=svTot(dat,'schl<=18&young'),
        totDeaf1stYear=svTot(dat,'schl<=18&dear==1&young'),
        percDeaf1stYear=estSEstr('dear==1',subst='schl<=18&young',sdat=dat))
    out$year <- attr(dat,'year')
    out
}

oneYear <- function(yr){
    gc()
    dat <- loadDatOneYear(yr)
    estOneYear(dat)
}


makeYearEsts <- function()
    lapply(8:17,oneYear)



transformVarb <- function(varb,young,yearests){
    out <- do.call('rbind',
                   lapply(yearests, function(est)
                       c(year=est$year,
                         if(young) est$young[[varb]] else est$full[[varb]])))
    out <- as.data.frame(out)
    names(out)[2:3] <- c('est','se')
    out
}

transformEsts <- function(yearests)
    out <- list(
        full=sapply(names(yearests[[1]]$full),transformVarb,young=FALSE,yearests=yearests,simplify=FALSE),
        young=sapply(names(yearests[[1]]$full),transformVarb,young=TRUE,yearests=yearests,simplify=FALSE)
    )



plotTrend <- function(td,ylabel,Title,m,b){
    p <- ggplot(td,aes(year,est))+geom_point()+
        geom_errorbar(aes(ymin=est-se,ymax=est+se),size=0.75,width=0)+
        geom_errorbar(aes(ymin=est-2*se,ymax=est+2*se),size=0.5,width=0)+
        scale_y_continuous(labels=if(max(td$est)<1) percent else scientific)+
        scale_x_continuous(breaks=8:17,labels=c('2008','2009',paste0('20',10:17)))+
        theme(axis.text.x= element_text(angle=45))
    p <- p+if(!missing(m)) geom_abline(slope=m,intercept=b) else geom_smooth()
    if(!missing(ylabel))
        p <- p+ylab(ylabel)
    if(!missing(Title))
        p <- p+ ggtitle(Title)
    p
}

estTrend <- function(td){
    mod <- lm(est~year,data=td,weights=se)
    summary(mod)$coef[2,]
}

yearEsts <- makeYearEsts()
save(yearEsts,file='enrollmentByYear.RData')

ests <- transformEsts(yearEsts)

full <- NULL
results <- list()
for(y in c('full','young')){
    pdf(if(y=='full') 'Ages18-64.pdf' else 'Ages18-45.pdf')
    res <- 2008:2017
    for(xx in names(ests[[y]])){
        ylabel <- if(startsWith(xx,'tot')) 'Number' else 'Percent Deaf'
        Title <- switch(xx,'totEnrolled'='Total Enrollment',
                        'totDeaf'='Total Deaf Enrollment',
                        'percDeaf'='Deaf Enrollment Share',
                        'tot1stYear'='Total First-Year Enrollment',
                        'totDeaf1stYear'='Total Deaf First-Year Enrollment',
                        'percDeaf1stYear'='Deaf 1st-Year Enrollment Share')
        plot(plotTrend(ests[[y]][[xx]],ylabel,Title))
        res <- cbind(res,ests[[y]][[xx]][,'est'])
        if(max(res[,ncol(res)])<1) res[,ncol(res)] <- round(100*res[,ncol(res)],2)
        colnames(res)[ncol(res)] <- Title
    }
    results[[y]] <- res
    dev.off()
}

openxlsx::write.xlsx(results,'enrollment.xlsx')
