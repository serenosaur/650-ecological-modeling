### Hello World!
### Project goal: import data, clean data, create a graph

# activate ggplot2
library(ggplot2)

# import file into R, save as "strontium"
strontium <- read.csv(file.choose())

# rename columns
names(strontium) <- c("date","activity","error","team")

# create activity - error column
strontium$errorlow <- strontium$activity - strontium$error

# create activity + error column
strontium$errorhigh <- strontium$activity + strontium$error

# force date column into date type
strontium$date2 <- as.POSIXct(strontium$date, format = "%m/%d/%Y", origin="firstdate")

# force teams into factor type
strontium$team <- as.factor(strontium$team)

# find first sample date
firstdate = min(strontium$date2)

# create date column that is a minus of the first date
strontium$date3 <- as.POSIXct(strontium$date2 - firstdate)/60/60/24/365)

#make the fitdata

#plot the graph
ggplot(strontium, aes(x = date2, y = activity)) + 
  geom_point(size = 3.0) +
  geom_errorbar(aes(ymin = errorlow, ymax = errorhigh, width = 0.2)) + 
  geom_line(data=fitdata, aes(x=date, y=expfit, col="blue", group="1"))
  
line <- geom_smooth(method="glm", formula = (log(y) ~ x), aes(x=date2,group=1), linetype = 2)
  
line <- lm(log(strontium$activity)~strontium$date3)


#give up and go back to plot
attach(strontium)
names(strontium)

#make the predicted values
timevalues <- seq(0, 40.8, 0.1)
exponential.model <- lm(log(activity)~ date3)
expfit <- exp(predict(exponential.model, list(date3=timevalues)))
#make the fitdata dataframe
fitdata <- data.frame(expfit)
fitdata$xaxis <- timevalues
fitdata$date <- as.POSIXct(fitdata$expfit, origin=firstdate)


Counts.exponential2 <- exp(predict(exponential.model,list(date3=timevalues)))


plot(strontium$date2, strontium$activity, pch=16)
lines(as_datetime((timevalues*365*24*60*60), origin=firstdate), Counts.exponential2,lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")

attach(strontium)
