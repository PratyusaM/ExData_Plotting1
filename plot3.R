plot3 <- function() {
  ##Read file - skip records read only 1/2/2007 & 2/2/2007
  pdata <- read.table("household_power_consumption.txt", header=FALSE,sep=";",skip=66638,nrows=2880)
  
  ##create the date and time columns as a datetime object
  datetime <- as.POSIXct(strptime(paste(pdata[,1],pdata[,2]), "%d/%m/%Y %H:%M:%S"))
  
  ##Create a data frame with the datetime and the rest of the columns
  df <- data.frame(datetime=datetime, gap=pdata[,3], grp=pdata[,4], Voltage=pdata[,5], smtr1=pdata[,7], smtr2=pdata[,8], smtr3=pdata[,9])
  
  ##Define the output file
  png("plot3.png", width=480, height=480)
  
  ##Plot the graph 
  plot(smtr1 ~ datetime, df, type="n", ylab="Energy sub metering", xlab="")
  lines(smtr1 ~ datetime, df)
  lines(smtr2 ~ datetime, df, col="red")
  lines(smtr3 ~ datetime, df, col="blue")
  legend("topright", lty=c("solid", "solid", "solid"), col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),cex=0.75)
  
  ##Define the axis
  axis.POSIXct(1,at=seq(from=round(df$datetime[1],"day"),to=tail(df$datetime,1),by="1 day"), format="%a")
  
  dev.off()
}