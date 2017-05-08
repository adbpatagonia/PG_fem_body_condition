library(lubridate)
library(doBy)
library(tibble)
library(dplyr)
setwd("D:/Buren_files/GitHub/PG_fem_body_condition")
#### define cutoff data for early puppers
## 46: Feb 15  / 59: Feb 28
cutoff <- 51


infile <- 'data/raw/female_harps_maturity.csv'

maturity <- as_data_frame(read.csv(infile,header=T))

maturity$sex <- "F"

## day for this seal was blank. Assigned it   25 so as to avoid having abortions in 1979. Assigned day 1 to the rest of blank dates
maturity[maturity$idsex=='19790209F','day'] <- 25
maturity[which(is.na(maturity$day)),'day'] <- 1



## eliminate 20160009F as no data was obtained from the ovaries sent y Bert Winters
maturity <- filter(maturity, idsex != '20160009F')

## define maturity and pregnancy codes  ----  0=FALSE  --- 1=TRUE
maturity$maturity <-NA
maturity$maturity <- ifelse(maturity$codematurity==1,0,NA)
ind <- which(is.na(maturity$maturity))
maturity$maturity[ind] <- ifelse(maturity$codematurity[ind]>1,1,10)
maturity$pregnancy <-NA
maturity$pregnancy <- ifelse(maturity$codeembryo==2,0,NA)
ind <- which(is.na(maturity$pregnancy))
maturity$pregnancy[ind] <- ifelse(maturity$codeembryo[ind]==1,1,10)
ind <- which(is.na(maturity$codeembryo))
maturity$pregnancy[ind] <- ifelse(maturity$codematurity[ind]==2 | maturity$codematurity[ind]==3,1,0)



maturity$cohyear <- as.integer(substr(maturity$idsex,1,4))


###########################################################################
#################Data manipulation on entire data set################################
###########################################################################
## define early puppers
maturity$doy <- yday(with(maturity, as.Date(paste(formatC(day, width=2, flag="0"),'/',formatC(month, width=2, flag="0"),'/',year,sep=''),format='%d/%m/%Y')))

maturity[which(maturity$doy>cutoff & maturity$doy<150 & maturity$codematurity==8),'pregnancy'] <- 1
maturity[which(maturity$doy>cutoff & maturity$doy<150 & maturity$codematurity==8),'maturity'] <- 1
maturity[which(maturity$doy>cutoff & maturity$doy<150 & maturity$codematurity==8),'mat'] <- 2

maturity$EP <- ifelse(maturity$mat==8,1,0)

## remove YOY and foetuses
# the 2nd line of this section removes all records based on Age: it removes all age 0s, '90' codes (i.e. foetus, starvling), and records that contain NAs in the column age
# the first line of this section stores the records that contain NAs in age in a separate object (there are multiple seals that have not been aged)
# the last line puts the 2 objects together
oy <- maturity[which(is.na(maturity$age)),]
maturity <- maturity[which(maturity$age>0 & maturity$age<90),]
maturity <- rbind(maturity,oy)

