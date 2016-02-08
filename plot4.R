plot4 <- function() {
  ##Read file - skip records read only 1/2/2007 & 2/2/2007
  pdata <- read.table("household_power_consumption.txt", header=FALSE,sep=";",skip=66638,nrows=2880)
  
  ##Create the date and time columns as a datetime object
  datetime <- as.POSIXct(strptime(paste(pdata[,1],pdata[,2]), "%d/%m/%Y %H:%M:%S"))
  
  ##Create a data frame with the datetime and the rest of the columns
  df <- data.frame(datetime=datetime, gap=pdata[,3], grp=pdata[,4], Voltage=pdata[,5], smtr1=pdata[,7], smtr2=pdata[,8], smtr3=pdata[,9])
  
  ##Define the png output file
  png("plot4.png", width=480, height=480)
  
  ##Plot the graphs
  par(mfrow = c(2,2))
  
  with (df, {
    plot(gap ~ datetime, xaxt="n", type="l", ylab="Global Active Power", xlab="")
    axis.POSIXct(1,at=seq(from=round(df$datetime[1],"day"),to=tail(df$datetime,1),by="1 day"), format="%a")
    plot(Voltage ~ datetime, type="l", ylab="Voltage")
    plot(smtr1 ~ datetime, type="n", ylab="Energy sub metering", xlab="")
    lines(smtr1 ~ datetime)
    lines(smtr2 ~ datetime, col="red")
    lines(smtr3 ~ datetime, col="blue")
    legend("topright", lty=c("solid", "solid", "solid"), col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),cex=0.75,bty="n")
    plot(grp ~ datetime, type="l", ylab="Global_reactive_power")
  }
  ) 
  
  dev.off()
}