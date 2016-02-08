plot1 <- function(){
  ##Read file - skip records read only 1/2/2007 & 2/2/2007
  pdata <- read.table("household_power_consumption.txt", header=FALSE,sep=";",skip=66638,nrows=2880)
  
  ##Create the date and time columns as a datetime object
  datetime <- as.POSIXct(strptime(paste(pdata[,1],pdata[,2]), "%d/%m/%Y %H:%M:%S"))
  
  ## Create a data frame with the datetime and the rest of the columns
  df <- data.frame(datetime=datetime, gap=pdata[,3], grp=pdata[,4], Voltage=pdata[,5], smtr1=pdata[,7], smtr2=pdata[,8], smtr3=pdata[,9])
  
  ## Define the png output file
  png("plot1.png", width=480, height=480)
  
  ## Plot the histogram 
  hist(df[,3], col="red", main = "Global Active Power", xlab="Global Active Power(kilowatts)")
  
  dev.off()
}