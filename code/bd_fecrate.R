## run after running female_bodycondition.R

infile <- 'data/fecundity_2014-feb20.csv'


fecun <- as_data_frame(read.csv(infile,header=T))

fecun$fecrate <- fecun$preg/fecun$mature
fecun$totpreg <- with(fecun,EP+preg)
fecun$abrate <- fecun$EP/fecun$totpreg


fecun <- select(fecun, cohort_year, fecrate, abrate)
names(fecun) <- c("year", "fecrate", "abrate")
fecun$dates <- ymd(paste(fecun$year,"-",3,"-",1,sep=""))
ff <- filter(fecun,year>1978)
mm <- select(mature, year, month, dates, blubberdepth)
mm <- filter(mm, month==12 | month<3)
mm <- select(mm, year,  dates, blubberdepth)

mm <- bind_rows(mm,data.frame(year=c(1979,2014),dates=c(ymd(paste(1979,"-",1,"-",1,sep="")),ymd(paste(2014,"-",12,"-",31,sep=""))),blubberdepth=c(NA,NA)))
ff <- bind_rows(ff,data.frame(year=c(1979,2014),dates=c(ymd(paste(1979,"-",1,"-",1,sep="")),ymd(paste(2014,"-",12,"-",31,sep=""))),abrate=c(NA,NA),fecrate=c(NA,NA)))


mdbd <- mm %>%
        na.omit() %>%
        group_by(year) %>% 
        summarise(median.bd = median(blubberdepth))  
mbd <- mm %>%
        na.omit() %>%
        group_by(year) %>% 
        summarise(mean.bd = mean(blubberdepth))          
         

        
mbd$dates <- ymd(paste(mbd$year,"-",2,"-",1,sep=""))      
mdbd$dates <- ymd(paste(mdbd$year,"-",2,"-",1,sep=""))          

pdf(file='output/plots/medianblubberdepth_fecrate.pdf',width=9.5,height=6.5, family = "sans", bg = "white",   pointsize = 8)
   par(mfrow=c(1,1))
  par(bty='o')
  par(mar=c(5,4,0.5,5))  # set margins(bottom,left,top,right)
  
  with(mm, plot( dates, blubberdepth,xaxt='n', xlab='Date',ylab='Blubber depth (cm)')) 
     axis(1,at=(c(as.Date("1980/02/01"),as.Date("1982/02/01"),as.Date("1984/02/01"),as.Date("1986/02/01"),as.Date("1988/02/01"),as.Date("1990/02/01"),as.Date("1992/02/01"),as.Date("1994/02/01"),as.Date("1996/02/01") ,as.Date("1998/02/01"),as.Date("2000/02/01"),as.Date("2002/02/01"),as.Date("2004/02/01") ,as.Date("2006/02/01"),as.Date("2008/02/01"),as.Date("2010/02/01"),as.Date("2012/02/01") ,as.Date("2014/02/01"),as.Date("2016/02/01"))),labels=c('1980/02','','1984/02','','1988/02','','1992/02','','1996/02','','2000/02','','2004/02','','2008/02','','2012/02','','2016/02'),tck=-.01,col="#0000ff00",col.ticks='black')
     
     
  with (mdbd, lines(dates,median.bd, col='red', pch=18,type='o'))
#  with (mbd, lines(dates,mean.bd, col='purple', pch=18,type='o'))
  par(new=T)
  with(ff, plot( dates, fecrate, type='o',pch=16,col='blue',xaxt='n',yaxt='n',xlab='',ylab='',lwd=2,ylim=c(0,1)))   
     axis(4,at=seq(0,1,by=.2),labels=T,tck=-.01,col="#0000ff00",col.ticks='black')
  mtext(expression("Fecundity rate"), 4, 3,cex=1, col="black",outer=F)                                              
     legend(as.Date("1980/01/01"), 1,cex=0.8, legend=c('blubber depth','median annual blubber depth','Fecundity rate'),bty='n',col=c('black','red','blue'),lty=c(NA,1,1),pch=c(1,18,16),lwd=2)
dev.off()     
   
   
   
 pdf(file='output/plots/blubberdepth_fecrate2.pdf',width=9.5,height=6.5, family = "sans", bg = "white",   pointsize = 8)
   par(mfrow=c(1,1))
  par(bty='o')
  par(mar=c(5,4,0.5,5))  # set margins(bottom,left,top,right)
  with(mm, plot( dates, blubberdepth,xaxt='n', xlab='Date',ylab='Blubber depth (cm)')) 
     axis(1,at=(c(as.Date("1980/02/01"),as.Date("1982/02/01"),as.Date("1984/02/01"),as.Date("1986/02/01"),as.Date("1988/02/01"),as.Date("1990/02/01"),as.Date("1992/02/01"),as.Date("1994/02/01"),as.Date("1996/02/01") ,as.Date("1998/02/01"),as.Date("2000/02/01"),as.Date("2002/02/01"),as.Date("2004/02/01") ,as.Date("2006/02/01"),as.Date("2008/02/01"),as.Date("2010/02/01"),as.Date("2012/02/01") ,as.Date("2014/02/01"),as.Date("2016/02/01"))),labels=c('1980/02','','1984/02','','1988/02','','1992/02','','1996/02','','2000/02','','2004/02','','2008/02','','2012/02','','2016/02'),tck=-.01,col="#0000ff00",col.ticks='black')  

  with (mbd, lines(dates,mean.bd, col='red', pch=18,type='p',cex=1.5))
  par(new=T)
  with(ff, plot( dates, fecrate, type='o',pch=16,col='blue',xaxt='n',yaxt='n',xlab='',ylab='',lwd=2,ylim=c(0,1)))   
     axis(4,at=seq(0,1,by=.2),labels=T,tck=-.01,col="#0000ff00",col.ticks='black')
  mtext(expression("Fecundity rate"), 4, 3,cex=1, col="black",outer=F)                                              
     legend(as.Date("1980/01/01"), 1,cex=0.8, legend=c('blubber depth','median annual blubber depth','Fecundity rate'),bty='n',col=c('black','red','blue'),lty=c(NA,NA,1),pch=c(1,18,16),lwd=2)
dev.off()     

 pdf(file='output/plots/blubberdepth_fecrate3.pdf',width=9.5,height=6.5, family = "sans", bg = "white",   pointsize = 8)
   par(mfrow=c(1,1))
  par(bty='o')
  par(mar=c(5,4,0.5,5))  # set margins(bottom,left,top,right)
  
    with(mm, plot( dates, blubberdepth,xaxt='n', xlab='Date',ylab='Blubber depth (cm)')) 
     axis(1,at=(c(as.Date("1980/02/01"),as.Date("1982/02/01"),as.Date("1984/02/01"),as.Date("1986/02/01"),as.Date("1988/02/01"),as.Date("1990/02/01"),as.Date("1992/02/01"),as.Date("1994/02/01"),as.Date("1996/02/01") ,as.Date("1998/02/01"),as.Date("2000/02/01"),as.Date("2002/02/01"),as.Date("2004/02/01") ,as.Date("2006/02/01"),as.Date("2008/02/01"),as.Date("2010/02/01"),as.Date("2012/02/01") ,as.Date("2014/02/01"),as.Date("2016/02/01"))),labels=c('1980/02','','1984/02','','1988/02','','1992/02','','1996/02','','2000/02','','2004/02','','2008/02','','2012/02','','2016/02'),tck=-.01,col="#0000ff00",col.ticks='black')
  with (mbd, lines(dates,mean.bd, col='red', pch=18,type='n',cex=1.5))
  par(new=T)
  with(ff, plot( dates, fecrate, type='o',pch=16,col='blue',xaxt='n',yaxt='n',xlab='',ylab='',lwd=2,ylim=c(0,1)))   
     axis(4,at=seq(0,1,by=.2),labels=T,tck=-.01,col="#0000ff00",col.ticks='black')
  mtext(expression("Fecundity rate"), 4, 3,cex=1, col="black",outer=F)                                              
     legend(as.Date("1980/01/01"), 1,cex=0.8, legend=c('blubber depth','','Fecundity rate'),bty='n',col=c('black','red','blue'),lty=c(NA,NA,1),pch=c(1,NA,16),lwd=2)
dev.off()     

     