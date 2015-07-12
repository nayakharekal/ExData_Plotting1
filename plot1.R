plot1 <- function()
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
  pcsub$V3 <- as.numeric(as.character(pcsub$V3))
  
  ##Save histogram of Global Active Power in png file
  png(file = "plot1.png",width=480,height=480,units="px")
  with(pcsub,hist(V3,col="red",xlab="Global Active Power (Kilowatts)",main="Global Active Power"))
  dev.off()
}