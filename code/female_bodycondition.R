#The data for this analysis is retrieved from the database through the query
#
#D:\Buren_files\MEGA\mmdb\mm_db_queries.accdb
#qsel_female_bodycondition
#
#Filters used were:
#
#1. sp = 1   harp seals
#2. sex = F

## Filters used in this script
#3. Code Female Maturity > 1      (mature individuals)

#setwd("D:/Buren_files/GitHub/PG_fem_body_condition")
#packrat::init()
library(tibble)
library(lubridate)
library(ggplot2)
library(dplyr)


femcond <- as_data_frame(read.csv('data/fembodycondition.csv', header=T,as.is=T))

femcond$sex <- "F"

## assign the first of the month for these seals for which we have no data on day
femcond[which(is.na(femcond$day)),'day']   <- 1

## Individual 20062138F was a yellow pup shot ofshore on Feb 20, 2006 - there was a mix up with ovary data
## and coded as mature - change it to immature
femcond$codefemalematurity[which(femcond$idsex=='20062138F')]  <- 1   
femcond$matur <- as.integer(ifelse(femcond$codefemalematurity==1, 0,1))


lmlenweight <- (lm(log(bodyweight)~log(bodylength), data=filter(femcond,age>0)))
femcond$expweight <- exp(coef(lmlenweight)[1])* ((femcond$bodylength)^ coef(lmlenweight)[2])
with(femcond, plot(bodylength,bodyweight))
I1 <- order(femcond$bodylength)
with(femcond, lines(bodylength[I1],expweight[I1],col='red'))

femcond$relcondition <- femcond$bodyweight/femcond$expweight

mogive <- femcond %>%
            group_by(year, age, matur) %>%
            summarise (n = n()) %>%
             mutate(freq = n / sum(n))        
             
            
ogive <- subset(mogive, matur==1 & age<99 & n>3) 
ggplot(ogive , aes(age, freq))  + geom_line() + facet_wrap(~year) + scale_x_continuous(limits=c(1, 15),breaks=1:15) + theme_bw()
ggplot(ogive , aes(age, freq, colour=as.factor(year)))  + geom_line()   + scale_x_continuous(limits=c(1, 15),breaks=1:15)  + theme_bw()
ggplot(ogive , aes(age, freq, colour=as.factor(year)))  + geom_point()  + scale_x_continuous(limits=c(1, 15),breaks=1:15)  + theme_bw()


mature <- subset(femcond, codefemalematurity>1)
mature$dates <- ymd(paste(mature$year,"-",mature$month,"-",mature$day,sep=""))
mature$mdates <- ymd(paste('3000',"-",mature$month,"-",mature$day,sep=""))

## plot
ggplot(mature, aes(dates, blubberdepth))  + geom_point()


## plot   without those 2 seals
ggplot(mature , aes(dates, blubberdepth))  + geom_point()

## calculate condition index
mature$condition <- with(mature, blubberdepth*sqrt(bodylength/bodyweight))

## plot   without those 2 seals
ggplot(mature , aes(dates, condition))  + geom_point()

## plot   without those 2 seals
# p <- ggplot(subset(mature,blubberdepth<20 & year<1991) , aes(as.POSIXct(dates), condition))  + geom_point()
# p + scale_x_datetime(date_breaks="3 months") +theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Date")
# 
# p <- ggplot(subset(mature,blubberdepth<20 & year<2001 & year>1990) , aes(as.POSIXct(dates), condition))  + geom_point()
# p + scale_x_datetime(date_breaks="3 months") +theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Date")
# 
# p <- ggplot(subset(mature,blubberdepth<20 & year<2011 & year>2000) , aes(as.POSIXct(dates), condition))  + geom_point()
# p + scale_x_datetime(date_breaks="3 months") +theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Date")
# 
# p <- ggplot(subset(mature,blubberdepth<20 & year>2010) , aes(as.POSIXct(dates), condition))  + geom_point()
# p + scale_x_datetime(date_breaks="3 months") +theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Date")



with(mature,table(year,month ))

p2 <- ggplot(mature , aes(as.POSIXct(mdates), condition))  + geom_point()
p2 <- p2 + scale_x_datetime(date_breaks="1 months") +theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Date")


p3 <- ggplot(mature , aes(as.POSIXct(mdates), blubberdepth))  + geom_point()
p3 <- p3 + scale_x_datetime(date_breaks="1 months") +theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Date")

print(p2)
p2 + facet_wrap(~year)
print(p3)
p3 + facet_wrap(~year)

## 
par(mfrow=c(1,2))
with(filter(femcond,age<90), plot( age, bodylength))
with(filter(femcond,age<90), plot( age, bodyweight))
with(femcond, plot( bodylength, bodyweight))
with(mature, plot( bodylength, bodyweight))

plot(mature$dates, mature$blubberdepth)  

 plot(mature$dates, mature$relcondition)  
 
 
## create data frames to store the information on density distribution and measures of central tendency for each year and nafo div
conddens <- data.frame('x'=NA,'y'=NA,'year'=NA,'age'=NA)
centraltendency <- data.frame('meank'=NA,'mediank'=NA,'year'=NA,'age'=NA)

## loop through years and age divisions to obtain density distributions, means and medians of condition
for (i in min(unique(mature$year)):max(unique(mature$year))){
   for (j in na.omit(unique(mature$age))){
      dat <- na.omit(subset(mature, year==i & age==j, select=c(year,age,relcondition)))
      if (nrow(dat)>2){
         yy <- density(dat$relcondition)
         yy <- data.frame('x'=yy$x,'y'=yy$y)
         yy$year <- rep(i,nrow(yy))
         yy$age <- rep(j,nrow(yy))
         conddens <- rbind(conddens,yy)
         ct <- data.frame('meank'=mean(dat$relcondition),'mediank'=  median(dat$relcondition),'year'=i,'age'=j)
         centraltendency <- rbind(centraltendency,ct)
      }
   }
}
 

## obtain the max density per year and age and merge it with density distribution
maxy <- aggregate(conddens$y, by=list(conddens$age,conddens$year),FUN=max)
names(maxy) <- c ('age','year','maxy')
conddens <- merge(conddens, maxy)

## merge data frames with information on density distributions and centraltendency
conddens <- merge(conddens,centraltendency)

## sum year to the max density per year and age so that each histogram is ploted along the x-axis in the right year.
## The max density each curve takes is slightly shifted down so that distribution in year t does not overlap with distribution in year t+1
conddens$yy <- with(conddens,year+(y/(maxy+maxy*.02)))

## obtain the range of conditions that encompass 95% of the data per year and nfo div and merge it with density distribution data
bounds <- aggregate(conddens$x, by=list(conddens$age,conddens$year),FUN= function(x) quantile(x, probs = c(0.025, 0.975)))
bounds <- data.frame(bounds[,1:2],bounds[,3][,1],bounds[,3][,2])
names(bounds) <- c ('age','year','lb','ub')
conddens <- merge(conddens, bounds)



## Start ggplot

 ## define working dataset, x and y variables
 p <- ggplot(conddens, aes(yy, x))
 ## this draws the histograms
 p <- p + geom_line()
 ## Plot by age
 p <- p + facet_grid(age ~ .)
 ## Add x and y axis labels
 p <- p + xlab("Year") + ylab("Relative Condition")
 ## Add the grey ribbons that encompass 95% of data. Argument alpha controls the amount of transparency
 p <- p + geom_ribbon(data = conddens,
                       aes(x = yy,
                           ymax = ub,
                           ymin = lb),
                           alpha = 0.15)
 ## Add the median per year and age
# p <- p + geom_line(data=conddens,aes(x=yy,y=mediank),colour='red',lwd=1)
 ## the next 3 lines plot Lambert and Dutil's benchmark values. I commented them  out
# hline.data <- data.frame(age = rep(c('2J','3K','3L'),each=2),z = rep(c(0.7,0.85),3))
# colores <-  rep(c('darkgreen','red'),3)
# p <- p + geom_hline(data=hline.data, aes(yintercept = z,colour=colores),linetype='longdash')

 ## Aesthetic options
 p <- p  + theme(axis.text.x = element_text(angle = 90,  vjust = 0.5, hjust=0.5))  +  theme(legend.position="none")
 p <- p + scale_x_continuous(breaks=seq(from=1978,to=2012,by=2),limits=c(1978, 2012))
 p <- p + scale_y_continuous(breaks=seq(from=0.3,to=1.5,by=0.6))
 p <- p + theme_bw()
 
 
 ##print plot
 print(p)
pdf(file='output/plots/condovertime.pdf',width=9.5,height=6.5, family = "sans", bg = "white",   pointsize = 8)
   print(p)
dev.off()


for (i in seq(min(conddens$age),max(conddens$age)-5,by=5)){
   cd <- subset(conddens, age>=i & age<i+5 )
   ## define working dataset, x and y variables
   p <- ggplot(cd, aes(yy, x))
   ## this draws the histograms
   p <- p + geom_line()
   ## Plot by age
   p <- p + facet_grid(age ~ .)
   ## Add x and y axis labels
   p <- p + xlab("Year") + ylab("Relative Condition")
   ## Add the grey ribbons that encompass 95% of data. Argument alpha controls the amount of transparency
   p <- p + geom_ribbon(data = cd,
                         aes(x = yy,
                             ymax = ub,
                             ymin = lb),
                             alpha = 0.15)
   ## Add the median per year and age
  # p <- p + geom_line(data=cd,aes(x=yy,y=mediank),colour='red',lwd=1)
   ## the next 3 lines plot Lambert and Dutil's benchmark values. I commented them  out
  # hline.data <- data.frame(age = rep(c('2J','3K','3L'),each=2),z = rep(c(0.7,0.85),3))
  # colores <-  rep(c('darkgreen','red'),3)
  # p <- p + geom_hline(data=hline.data, aes(yintercept = z,colour=colores),linetype='longdash')
  
   ## Aesthetic options
   p <- p  + theme(axis.text.x = element_text(angle = 90,  vjust = 0.5, hjust=0.5))  +  theme(legend.position="none")
   p <- p + scale_x_continuous(breaks=seq(from=1978,to=2014,by=2),limits=c(1978, 2014))
   p <- p + scale_y_continuous(breaks=seq(from=0.3,to=1.5,by=0.6))
   p <- p + theme_bw()
   
   
   ##print plot
   print(p)
   pdf(file='output/plots/condovertime.pdf',width=9.5,height=6.5, family = "sans", bg = "white",   pointsize = 8)
     print(p)
  dev.off()

}
