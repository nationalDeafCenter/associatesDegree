states <- read.csv('../../data/states.csv')
stemCodes <- read.csv('stemCodes.csv',colClasses = "character")
stemRelatedCodes <- read.csv('stemRelatedCodes.csv',colClasses = "character")

## need: DEAR, attain, employment,PERNP, fulltime
pVars <- c('SERIALNO','DEAR','DDRS','DEYE','DOUT','DPHY','DRATX','DREM','RAC1P','HISP','SEX','FOD1P','SCIENGP','SCIENGRLP','ST','AGEP','SCHL','SCH','ADJINC','ESR','WKW','WKHP','WAGP','PERNP','PINCP','SSIP','OCCP','PWGTP',paste0('PWGTP',1:80))

## exclude institutional (as before)
hVars <- c('SERIALNO','TYPE')

### read in person-level data
firstTry <- read_csv('../../data/acs5yr2016/ss16pusa.csv',n_max=2)
ccc <- ifelse(names(firstTry)%in%pVars,
       ifelse(names(firstTry)=='OCCP','c',
       ifelse(names(firstTry)=='SERIALNO','d','i')),'-')

## code checking:
table(ccc)
length(pVars)

ccc <- paste(ccc,collapse='')

sdat <- read_csv('../../data/acs5yr2016/ss16pusa.csv',col_types=ccc)
for(pp in c('b','c','d')){
    sdat2 <- read_csv(paste0('../../data/acs5yr2016/ss16pus',pp,'.csv'),col_types=ccc)
    sdat <- rbind(sdat[,pVars],sdat2[,pVars])
}
rm(sdat2)
gc()
names(sdat) <- tolower(names(sdat))

### read in HH level data (just serialno & type)
firstTryH <- read_csv('../../data/acs5yr2016/ss16husa.csv',n_max=2)
ccc <- ifelse(names(firstTryH)%in%hVars,ifelse(names(firstTry)=='SERIALNO','d','i'),'-')
ccc <- paste(ccc,collapse='')

hdat <- read_csv('../../data/acs5yr2016/ss16husa.csv',col_types=ccc)


for(pp in c('b','c','d')){
    hdat2 <- read_csv(paste0('../../data/acs5yr2016/ss16hus',pp,'.csv'),col_types=ccc)
    hdat <- rbind(hdat[,hVars],hdat2[,hVars])
}
names(hdat) <- tolower(names(hdat))
sdat$type <- hdat$type[match(sdat$serialno,hdat$serialno)]
rm(hdat,hdat2); gc()

sdat$state <- states$abb[match(sdat$st,states$x)]

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

sdat$attain <- ifelse(sdat$schl<13,1,sdat$schl-11)
sdat$attain <- factor(edlevs[sdat$attain],levels=edlevs,ordered=TRUE)

sdat$adj <- sdat$adjinc/1e6

sdat <- sdat%>%filter(agep>24,agep<65,type!=2)%>%
    mutate(
        deaf=ifelse(dear==1,1,0),
        enrolled=sch>1,
        hs = schl>=16,
        sc = schl>=18,
        cc = schl>=20,
        ba = schl>=21,
        grad = schl>21,
        doc = schl==24,
        employed = esr%in%c(1,2,4,5),
        unemployed = esr==3,
        fulltime=(wkw==1 & wkhp>=35),
        earn=pernp*adj,
        inc=pincp*adj,

        raceEth=ifelse(hisp>1,"Hispanic",
                ifelse(rac1p==2,"African American",
                ifelse(rac1p==6| rac1p==7,"Asian/PacIsl",
                ifelse(rac1p%in%c(3,4,5),'American Indian',
                ifelse(rac1p==1,"White","Other"))))),

        diss=ifelse(ddrs==1|deye==1|dout==1|dphy==1|dratx==1|drem==1,1,0),
        blind=ifelse(deye==1,1,0),

        fod= ifelse(fod1p%in%c(2100,2102,2103,2105,2106,2107,2199,3700,3701,3702,3705,3799),
                    'computers',
             ifelse(fod1p%in%c(1100:1106,1199,1301,1302,1303,3600:3611,3699),'bio',
             ifelse(fod1p%in%c(5000:5008,5098,5099),'physics',
             ifelse(fod1p%in%c(5200:5206,5299),'psych',
             ifelse(fod1p%in%c(1501,5500:5507,5599,6401),'socialScience',
             ifelse(fod1p%in%c(2400:2420,2499),'engineering',
             ifelse(fod1p%in%c(4000:4007),'multidisciplinary',
             ifelse(fod1p%in%c(1401,2101,2104,2302,2305,2308,2500:2504,2599,5101,
                               5102,6100,6102:6111,6199),'scienceRelated',
             ifelse(fod1p%in%c(3201,6101,6200:6212,6299),'business',
             ifelse(fod1p%in%c(2300,2301,2303,2304,2306,2307,2309,2310,2311,
                               2312,2313,2314,2399),'education',
             ifelse(fod1p%in%c(2601:2603,3301,3302),'literature',
             ifelse(fod1p%in%c(3401,3402,4801,4901,6402,6403), 'liberalArts',
             ifelse(fod1p%in%c(6000:6008,6099),'visualArts',
             ifelse(fod1p%in%c(1901,1902,1903,1904,2001),'communications',
             ifelse(fod1p%in%c(2201,2901,3202,3501,3801,4101,5301,5401:5404,5601,5701,5801,5901),'other',
             ifelse(is.na(fod1p),NA,'huh'))))))))))))))))

    )

## tests
xtabs(~attain+hs,data=sdat,addNA=TRUE)
xtabs(~attain+sc,data=sdat,addNA=TRUE)
xtabs(~attain+cc,data=sdat,addNA=TRUE)
xtabs(~attain+ba,data=sdat,addNA=TRUE)
xtabs(~attain+grad,data=sdat,addNA=TRUE)
xtabs(~attain+doc,data=sdat,addNA=TRUE)

xtabs(~dear+deaf,data=sdat,addNA=TRUE)

xtabs(~raceEth,data=sdat,addNA=TRUE)

