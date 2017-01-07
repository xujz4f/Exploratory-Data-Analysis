#Load the data
data<-read.table("C:/Users/Jiadi/Desktop/CourseraDataScience/exploredata/week1/household_power_consumption.txt",header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

## Format date to Type Date
data$Date <- as.Date(data$Date, "%d/%m/%Y")

## Filter data set from Feb. 1, 2007 to Feb. 2, 2007
data1 <- subset(data, Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
Datetime<-paste(data1$Date,data1$Time)


#Plot 1
png(filename = "plot1.png", width = 480, height = 480)
hist(data1 $Global_active_power, col = "red", main = paste("Global Active Power"), xlab = "Global Active Power (kilowatts)")
dev.off()

#Plot 2
png(filename = "plot2.png", width = 480, height = 480)
plot(data1$Global_active_power ~ data1$Datetime, type = "l", ylab = "Global Active Power (kilowatts)")
dev.off()

#Plot 3
png(filename = "plot3.png", width = 480, height = 480)
with(data1, {
   plot(Sub_metering_1~Datetime, type="l",col="Black", ylab="Energy sub metering",xlab = "")
  lines(Sub_metering_2~Datetime,col='Red')
  lines(Sub_metering_3~Datetime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.off()

#Plot 4
png(filename = "plot4.png", width = 480, height = 480)
par(mfrow = c(2,2), mar = c(4,4,2,1), oma = c(0,0,1,1))
with(data1, {
  plot(Global_active_power ~ Datetime, type = "l", ylab = "Global Active Power", xlab = "")
  plot(Voltage ~ Datetime, type = "l", ylab = "Voltage", xlab = "datetime")
  plot(Sub_metering_1 ~ Datetime, type = "l", ylab = "Energy sub metering",xlab = "")
  lines(Sub_metering_2 ~ Datetime, col = 'Red')
  lines(Sub_metering_3 ~ Datetime, col = 'Blue')
  legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power ~ Datetime, type = "l", ylab = "Global_rective_power", xlab = "datetime")
})
dev.off()


