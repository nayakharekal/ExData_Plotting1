plot3 <- function()
{
  ##Get household power consumption data from file. Remove first row from data frame.
  pc <- read.table("household_power_consumption.txt",sep=";")
  pcsub <- subset(pc,V1 != "Date")
  
  ##Create a new column with Date-Time in dd/mm/yyyyy hh:mm:ss format.
  pcsub$V10 <-paste(pcsub$V1,pcsub$V2)
  pcsub$V10 <- strptime(pcsub$V10,"%d/%m/%Y %H:%M:%S")
  
  ##Get data for 1st and 2nd of FEB 2007 from main data set.
  startdate <- strptime("2007-02-01 00:00:00","%Y-%m-%d %H:%M:%S")
  enddate <- strptime("2007-02-03 00:00:00","%Y-%m-%d %H:%M:%S")
  pcsub <- subset(pcsub, V10 >= startdate & V10 < enddate)
  
  ##Get numeric data for initial factor data
  pcsub$V7 <- as.numeric(as.character(pcsub$V7))
  pcsub$V8 <- as.numeric(as.character(pcsub$V8))
  pcsub$V9 <- as.numeric(as.character(pcsub$V9))
  
  ##Save plot of date-time vs Energy sub metering in png file
  png(file = "plot3.png",width=480,height=480,units="px")
  with(pcsub,plot(V10,V7,type="n",ylab="Energy sub metering",xlab=""))
  points(pcsub$V10,pcsub$V7,col="black",type="l")
  points(pcsub$V10,pcsub$V8,col="red",type="l")
  points(pcsub$V10,pcsub$V9,col="blue",type="l")
  legend("topright",lty=c(1,1),col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  dev.off()
}