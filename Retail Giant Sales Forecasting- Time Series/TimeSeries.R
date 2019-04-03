rm(list=ls())
setwd("G:\\IIIT-Upgrad-PGDDS\\Time series\\Case Study Time Series")
getwd()

library(forecast)
library(tseries)
require(graphics)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stats)
library(tseries)

globalStore <- read.csv("Global Superstore.csv",stringsAsFactors = FALSE)
str(globalStore)
sum(is.na(globalStore)) # we find 41296 missing or NA values
colSums(is.na(globalStore)) 
#Upon checking the data column wise , we see that POSTAL.CODE has all the 41296 missing values
#Upon checking the sheet we find that non-us countries have no Postal code.
#Let us ignore this column as we do not use this column in our analysis.

#Let us now convert the OrderDate,ShipDate from Factor to Date format
globalStore$Order.Date<-as.Date(globalStore$Order.Date,"%d-%m-%Y")
globalStore$Ship.Date<-as.Date(globalStore$Ship.Date,"%d-%m-%Y")

###subset your data into 21 (7*3) buckets before analysing the data###
globalStore_subset <- group_by(globalStore,Segment,Market) #we use it in the below

#now we consider 3 attributes Sales,Profit,Quantity on Year & Month for monthly analysis,
#we cannot consider only Month because it gets calculated for every year without distinction

# Extracting Month,Year,Date from orderDate:
globalStore_subset <- separate(globalStore_subset,col = Order.Date, into=c("OrderYear","OrderMonth","OrderDate"),sep="\\-")

#Performing aggregation:
globalStore_subset_aggregated <- aggregate(globalStore_subset[,c("Sales","Profit","Quantity")],by=list(
  globalStore_subset$Market,globalStore_subset$Segment,globalStore_subset$OrderYear,globalStore_subset$OrderMonth)
  ,sum)
names(globalStore_subset_aggregated) <- c("Market","Segment","Year","Month","TotalSales","TotalProfit","TotalQty")

#Ordering the data based on year and month in ascending order.
orderedData_Monthly <- globalStore_subset_aggregated[
  order(globalStore_subset_aggregated$Year,globalStore_subset_aggregated$Month),]

#21 Subsetted data with Profit,Sales,Quantity of the OrderedData:
orderedData <- aggregate(orderedData_Monthly[,c("TotalSales","TotalProfit","TotalQty")]
                        ,by=list(orderedData_Monthly$Market,orderedData_Monthly$Segment)
                        ,sum)
orderedData$ProfilePercentage <- 100 * orderedData$TotalProfit/orderedData$TotalSales

#FInd SD and Mean for calculating CV :
Profit_SD <- aggregate(orderedData_Monthly$TotalProfit,by=list(orderedData_Monthly$Market,orderedData_Monthly$Segment),sd)
Profit_Mean <-  aggregate(orderedData_Monthly$TotalProfit,by=list(orderedData_Monthly$Market,orderedData_Monthly$Segment),mean)

Profit_CV <- as.data.frame(Profit_SD$x/Profit_Mean$x )

orderedData_withCV <- cbind(orderedData,Profit_CV)

orderedData_withCV <- orderedData_withCV[order(Profit_CV),]  #Lower the CV better the option


#As per the analysis we find Corporate and Consumer segments to have the lower CV value.
#Let us consider the below combinations:
# 1. EU - CONSUMER
# 2. APAC- CONSUMER

##### EU- CONSUMER SALES #######

#Filter the data EU and CONSUMER from Monthly & Yearly dataframe 

orderedData_EU_CONSUMER <- subset(orderedData_Monthly,(orderedData_Monthly$Market=="EU" & orderedData_Monthly$Segment=="Consumer"))


orderedData__EU_CONSUMER_MONTH <- subset(orderedData_EU_CONSUMER[,c(4,5)])
#As the every year covers 12 months we are putting an incrementer to the months
for(i in 1:nrow(orderedData__EU_CONSUMER_MONTH)){
  orderedData__EU_CONSUMER_MONTH[i,1] <-i
}


#Smoothing the Data:
cols <- c("red", "blue")

labels <- c("Raw", "Smoothed")

ylab1 <- c("Sales Prediction")

xlab1 <- c("Monthly sales")

title <- c("Time Series Case Study")

total_timeser <- ts(orderedData__EU_CONSUMER_MONTH$TotalSales)
plot(total_timeser,col="red",main="EU Consumer Sales Time Series")
indata <- orderedData__EU_CONSUMER_MONTH[1:42,]
timeser <- ts(indata[,2])
timeser
plot(timeser)
############

w <-1
smoothedSeries <- stats::filter(timeser, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedSeries[w+2] - smoothedSeries[w+1]
for (i in seq(w,1,-1)) {
  smoothedSeries[i] <- smoothedSeries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedSeries[n-w] - smoothedSeries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedSeries[i] <- smoothedSeries[i-1] + diff
}
plot(timeser, main=title, xlab = xlab1, 
     ylab = ylab1, col=cols[1])

############

lines(smoothedSeries, col=cols[2], lwd=2)
smootheddf <- as.data.frame(cbind(indata$Month,as.vector(smoothedSeries)))
names(smootheddf) <-c("Month","Sales")
str(smootheddf)
smootheddf$Month <-as.numeric(as.character(smootheddf[,1]))
smootheddf$Sales <-as.numeric(as.character(smootheddf[,2]))

#Blue is the best from the graph
#Global trend and Seasonal behaviour are observed

#Removing Trend and Seasonal behaviour before performing the Classical decomposition
#We also observe the amplitude to be increasing slightly so let us go for a multiplicative model

lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=c(1:42))
summary(global_pred)
lines(smootheddf$Month, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#====To confirm if its a white noise we perform the below tests=====
resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#WHat we got is a white noise based on P-value in dickey fuller test and KPSS test

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- orderedData__EU_CONSUMER_MONTH[43:48,]
outdata$Month <-as.numeric(outdata$Month)
timevals_out <- outdata$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")


#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")


### To Conclude , the AUTO ARIMA Model did better than a Classical Decomposition for EU CONSUMER SALES##
###The MAPE value in a Classical Decomposition is 92.9578 where as MAPE in Auto ARIMA is 28.9226###
###In our case the best fit is distinguished using a AUTO ARIMA Model ###

#-----Forecast - next six month Sales-----------
forecast(timeser,h=6)
forecast(autoarima,h=6)



######################################
##### EU- CONSUMER DEMAND #######

cols <- c("red", "blue")

labels <- c("Raw", "Smoothed")

ylab1 <- c("Demand Prediction")

xlab1 <- c("Monthly Demand")

title <- c("Time Series Case Study")

#Filter the data EU and CONSUMER from Monthly & Yearly dataframe 

orderedData_EU_CONSUMER_Demand <- subset(orderedData_Monthly,(orderedData_Monthly$Market=="EU" & orderedData_Monthly$Segment=="Consumer"))
orderedData__EU_CONSUMER_MONTH_Demand <- subset(orderedData_EU_CONSUMER_Demand[,c(4,7)])
#As the every year covers 12 months we are putting an incrementer to the months
for(i in 1:nrow(orderedData__EU_CONSUMER_MONTH_Demand)){
  orderedData__EU_CONSUMER_MONTH_Demand[i,1] <-i
}

#Smoothing the data
total_timeser_Qy <- ts(orderedData__EU_CONSUMER_MONTH_Demand$TotalQty)
plot(total_timeser_Qy,col="red",main="EU Consumer Demand Time Series")
indata_Qy <- orderedData__EU_CONSUMER_MONTH_Demand[1:42,]
timeser_Qy <- ts(indata_Qy[,2])
timeser_Qy
plot(timeser_Qy)
############

w <-1
smoothedSeries_Qy <- stats::filter(timeser_Qy, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedSeries_Qy[w+2] - smoothedSeries_Qy[w+1]
for (i in seq(w,1,-1)) {
  smoothedSeries_Qy[i] <- smoothedSeries_Qy[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_Qy)
diff_Qy <- smoothedSeries_Qy[n-w] - smoothedSeries_Qy[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedSeries_Qy[i] <- smoothedSeries_Qy[i-1] + diff_Qy
}
plot(timeser_Qy, main=title, xlab = xlab1, 
     ylab = ylab1, col=cols[1])

############

lines(smoothedSeries_Qy, col=cols[2], lwd=2)
smootheddf_Qy <- as.data.frame(cbind(indata$Month,as.vector(smoothedSeries_Qy)))
names(smootheddf_Qy) <-c("Month","Qty")
str(smootheddf_Qy)
smootheddf_Qy$Month <-as.numeric(as.character(smootheddf_Qy[,1]))
smootheddf_Qy$Qty <-as.numeric(as.character(smootheddf_Qy[,2]))

#Blue is the best from the graph
#Global trend and Seasonal behaviour are observed

#Removing Trend and Seasonal behaviour before performing the Classical decomposition
#We also observe the amplitude to be increasing slightly so let us go for a multiplicative model

lmfit1 <- lm(Qty ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf_Qy)
global_pred1 <- predict(lmfit1, Month=c(1:42))
summary(global_pred1)
lines(smootheddf_Qy$Month, global_pred1, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred1 <- timeser_Qy-global_pred1
plot(local_pred1, col='red', type = "l")
acf(local_pred1)
acf(local_pred1, type="partial")
armafit1 <- auto.arima(local_pred1)

tsdiag(armafit1)
armafit1

#====To confirm if its a white noise we perform the below tests=====
resi1 <- local_pred1-fitted(armafit1)

adf.test(resi1,alternative = "stationary")
kpss.test(resi)

#WHat we got is a white noise based on P-value in dickey fuller test and KPSS test

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_Qy<- orderedData__EU_CONSUMER_MONTH_Demand[43:48,]
outdata_Qy$Month <-as.numeric(outdata_Qy$Month)
timevals_out1 <- outdata_Qy$Month

global_pred_out1 <- predict(lmfit1,data.frame(Month =timevals_out1))

fcast1 <- global_pred_out1

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec1 <- accuracy(fcast1,outdata_Qy[,2])[5]
MAPE_class_dec1

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred1 <- c(ts(global_pred1),ts(global_pred_out1))
plot(total_timeser_Qy, col = "black")
lines(class_dec_pred1, col = "red")


#So, that was classical decomposition, now let's do an ARIMA fit

autoarima1 <- auto.arima(timeser_Qy)
autoarima1
tsdiag(autoarima1)
plot(autoarima1$x, col="black")
lines(fitted(autoarima1), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima1 <- timeser_Qy - fitted(autoarima1)

adf.test(resi_auto_arima1,alternative = "stationary")
kpss.test(resi_auto_arima1)

#Also, let's evaluate the model using MAPE
fcast_auto_arima1 <- predict(autoarima1, n.ahead = 6)

MAPE_auto_arima1 <- accuracy(fcast_auto_arima1$pred,outdata_Qy[,2])[5]
MAPE_auto_arima1

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred1 <- c(fitted(autoarima1),ts(fcast_auto_arima1$pred))
plot(total_timeser_Qy, col = "black")
lines(auto_arima_pred1, col = "red")


### To Conclude , Both AUTO ARIMA Model &  Classical Decomposition for EU CONSUMER Demand##
###The MAPE value in a Classical Decomposition is 30.397441 where as MAPE in Auto ARIMA is 30.13319###


#-----Forecast - next six month Sales-----------
forecast(timeser_Qy,h=6)
forecast(autoarima1,h=6)





##############################APAC CONSUMER SALES #################################

#Filter the data EU and CONSUMER from Monthly & Yearly dataframe 

orderedData_APAC_CONSUMER <- subset(orderedData_Monthly,(orderedData_Monthly$Market=="APAC" & orderedData_Monthly$Segment=="Consumer"))
orderedData_APAC_CONSUMER_MONTH <- subset(orderedData_APAC_CONSUMER[,c(4,5)])
#As the every year covers 12 months we are putting an incrementer to the months
for(i in 1:nrow(orderedData_APAC_CONSUMER_MONTH)){
  orderedData_APAC_CONSUMER_MONTH[i,1] <-i
}


#Smoothing the Data:
cols <- c("red", "blue")

labels <- c("Raw", "Smoothed")

ylab1 <- c("Sales Prediction")

xlab1 <- c("Monthly sales")

title <- c("Time Series Case Study")

total_timeser.APAC.Sales <- ts(orderedData_APAC_CONSUMER_MONTH$TotalSales)
plot(total_timeser.APAC.Sales,col="red",main="APAC Consumer Sales Time Series")
indata.APAC.Sales <- orderedData_APAC_CONSUMER_MONTH[1:42,]
timeser.APAC.Sales <- ts(indata.APAC.Sales[,2])
timeser.APAC.Sales
plot(timeser.APAC.Sales)
############

w <-1
smoothedSeries.APAC.Sales <- stats::filter(timeser.APAC.Sales, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff.APAC.Sales <- smoothedSeries.APAC.Sales[w+2] - smoothedSeries.APAC.Sales[w+1]
for (i in seq(w,1,-1)) {
  smoothedSeries.APAC.Sales[i] <- smoothedSeries.APAC.Sales[i+1] - diff.APAC.Sales
}

#Smoothing right end of the time series

n <- length(timeser.APAC.Sales)
diff <- smoothedSeries.APAC.Sales[n-w] - smoothedSeries.APAC.Sales[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedSeries.APAC.Sales[i] <- smoothedSeries.APAC.Sales[i-1] + diff.APAC.Sales
}
plot(timeser.APAC.Sales, main=title, xlab = xlab1, 
     ylab = ylab1, col=cols[1])

############

lines(smoothedSeries.APAC.Sales, col=cols[2], lwd=2)
smootheddf.APAC.Sales <- as.data.frame(cbind(indata.APAC.Sales$Month,as.vector(smoothedSeries.APAC.Sales)))
names(smootheddf.APAC.Sales) <-c("Month","Sales")
str(smootheddf.APAC.Sales)
smootheddf.APAC.Sales$Month <-as.numeric(as.character(smootheddf.APAC.Sales[,1]))
smootheddf.APAC.Sales$Sales <-as.numeric(as.character(smootheddf.APAC.Sales[,2]))

#Blue is the best from the graph
#Global trend and Seasonal behaviour are observed

#Removing Trend and Seasonal behaviour before performing the Classical decomposition
#We also observe the amplitude to be increasing slightly so let us go for a multiplicative model

lmfit.APAC.Sales <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf.APAC.Sales)
global_pred.APAC.Sales <- predict(lmfit.APAC.Sales, Month=c(1:42))
summary(global_pred.APAC.Sales)
lines(smootheddf.APAC.Sales$Month, global_pred.APAC.Sales, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred.APAC.Sales <- timeser.APAC.Sales-global_pred.APAC.Sales
plot(local_pred.APAC.Sales, col='red', type = "l")
acf(local_pred.APAC.Sales)
acf(local_pred.APAC.Sales, type="partial")
armafit.APAC.Sales <- auto.arima(local_pred.APAC.Sales)

tsdiag(armafit.APAC.Sales)
armafit.APAC.Sales

#====To confirm if its a white noise we perform the below tests=====
resi.APAC.Sales <- local_pred.APAC.Sales-fitted(armafit.APAC.Sales)

adf.test(resi.APAC.Sales,alternative = "stationary")
kpss.test(resi.APAC.Sales)

#WHat we got is a white noise based on P-value in dickey fuller test and KPSS test

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata.APAC.Sales <- orderedData_APAC_CONSUMER_MONTH[43:48,]
outdata.APAC.Sales$Month <-as.numeric(outdata.APAC.Sales$Month)
timevals_out.APAC.Sales <- outdata.APAC.Sales$Month

global_pred_out.APAC.Sales <- predict(lmfit.APAC.Sales,data.frame(Month =timevals_out.APAC.Sales))

fcast.APAC.Sales <- global_pred_out.APAC.Sales

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec.APAC.Sales <- accuracy(fcast.APAC.Sales,outdata.APAC.Sales[,2])[5]
MAPE_class_dec.APAC.Sales

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred.APAC.Sales <- c(ts(global_pred.APAC.Sales),ts(global_pred_out.APAC.Sales))
plot(total_timeser.APAC.Sales, col = "black")
lines(class_dec_pred.APAC.Sales, col = "red")


#So, that was classical decomposition, now let's do an ARIMA fit

autoarima.APAC.Sales <- auto.arima(timeser.APAC.Sales)
autoarima.APAC.Sales
tsdiag(autoarima.APAC.Sales)
plot(autoarima.APAC.Sales$x, col="black")
lines(fitted(autoarima.APAC.Sales), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima.APAC.Sales <- timeser.APAC.Sales - fitted(autoarima.APAC.Sales)

adf.test(resi_auto_arima.APAC.Sales,alternative = "stationary")
kpss.test(resi_auto_arima.APAC.Sales)

#Also, let's evaluate the model using MAPE
fcast_auto_arima.APAC.Sales <- predict(autoarima.APAC.Sales, n.ahead = 6)

MAPE_auto_arima.APAC.Sales <- accuracy(fcast_auto_arima.APAC.Sales$pred,outdata.APAC.Sales[,2])[5]
MAPE_auto_arima.APAC.Sales

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred.APAC.Sales<- c(fitted(autoarima.APAC.Sales),ts(fcast_auto_arima.APAC.Sales$pred))
plot(total_timeser.APAC.Sales, col = "black")
lines(auto_arima_pred.APAC.Sales, col = "red")


### To Conclude , Both AUTO ARIMA Model & Classical Decomposition gave the same performancefor APAC CONSUMER SALES##
###The MAPE value in a Classical Decomposition is 36.21943 where as MAPE in Auto ARIMA is 27.68952###


#-----Forecast - next six month Sales-----------
forecast(timeser.APAC.Sales,h=6)
forecast(autoarima.APAC.Sales,h=6)



######################################


##############################APAC CONSUMER DEMAND #################################

#Filter the data EU and CONSUMER from Monthly & Yearly dataframe 

orderedData_APAC_CONSUMER_Qy <- subset(orderedData_Monthly,(orderedData_Monthly$Market=="APAC" & orderedData_Monthly$Segment=="Consumer"))
orderedData_APAC_CONSUMER_MONTH_Qy <- subset(orderedData_APAC_CONSUMER_Qy[,c(4,7)])
#As the every year covers 12 months we are putting an incrementer to the months
for(i in 1:nrow(orderedData_APAC_CONSUMER_MONTH_Qy)){
  orderedData_APAC_CONSUMER_MONTH_Qy[i,1] <-i
}


#Smoothing the Data:
cols <- c("red", "blue")

labels <- c("Raw", "Smoothed")

ylab1 <- c("Demand Prediction")

xlab1 <- c("Monthly Demand")

title <- c("Time Series Case Study")

total_timeser.APAC.Qy<- ts(orderedData_APAC_CONSUMER_MONTH_Qy$TotalQty)
plot(total_timeser.APAC.Qy,col="red",main="APAC Consumer Sales Time Series")
indata.APAC.Qy<- orderedData_APAC_CONSUMER_MONTH_Qy[1:42,]
timeser.APAC.Qy<- ts(indata.APAC.Qy[,2])
timeser.APAC.Qy
plot(timeser.APAC.Qy)
############

w <-1
smoothedSeries.APAC.Qy<- stats::filter(timeser.APAC.Qy, 
                                       filter=rep(1/(2*w+1),(2*w+1)), 
                                       method='convolution', sides=2)

#Smoothing left end of the time series

diff.APAC.Qy<- smoothedSeries.APAC.Qy[w+2] - smoothedSeries.APAC.Qy[w+1]
for (i in seq(w,1,-1)) {
  smoothedSeries.APAC.Qy[i] <- smoothedSeries.APAC.Qy[i+1] - diff.APAC.Qy
}

#Smoothing right end of the time series

n <- length(timeser.APAC.Qy)
diff <- smoothedSeries.APAC.Qy[n-w] - smoothedSeries.APAC.Qy[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedSeries.APAC.Qy[i] <- smoothedSeries.APAC.Qy[i-1] + diff.APAC.Qy
}
plot(timeser.APAC.Qy, main=title, xlab = xlab1, 
     ylab = ylab1, col=cols[1])

############

lines(smoothedSeries.APAC.Qy, col=cols[2], lwd=2)
smootheddf.APAC.Qy<- as.data.frame(cbind(indata.APAC.Qy$Month,as.vector(smoothedSeries.APAC.Qy)))
names(smootheddf.APAC.Qy) <-c("Month","Sales")
str(smootheddf.APAC.Qy)
smootheddf.APAC.Qy$Month <-as.numeric(as.character(smootheddf.APAC.Qy[,1]))
smootheddf.APAC.Qy$Sales <-as.numeric(as.character(smootheddf.APAC.Qy[,2]))

#Blue is the best from the graph
#Global trend and Seasonal behaviour are observed

#Removing Trend and Seasonal behaviour before performing the Classical decomposition
#We also observe the amplitude to be increasing slightly so let us go for a multiplicative model

lmfit.APAC.Qy<- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                   + Month, data=smootheddf.APAC.Qy)
global_pred.APAC.Qy<- predict(lmfit.APAC.Qy, Month=c(1:42))
summary(global_pred.APAC.Qy)
lines(smootheddf.APAC.Qy$Month, global_pred.APAC.Qy, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred.APAC.Qy<- timeser.APAC.Qy-global_pred.APAC.Qy
plot(local_pred.APAC.Qy, col='red', type = "l")
acf(local_pred.APAC.Qy)
acf(local_pred.APAC.Qy, type="partial")
armafit.APAC.Qy<- auto.arima(local_pred.APAC.Qy)

tsdiag(armafit.APAC.Qy)
armafit.APAC.Qy

#====To confirm if its a white noise we perform the below tests=====
resi.APAC.Qy<- local_pred.APAC.Qy-fitted(armafit.APAC.Qy)

adf.test(resi.APAC.Qy,alternative = "stationary")
kpss.test(resi.APAC.Qy)

#WHat we got is a white noise based on P-value in dickey fuller test and KPSS test

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata.APAC.Qy<- orderedData_APAC_CONSUMER_MONTH[43:48,]
outdata.APAC.Qy$Month <-as.numeric(outdata.APAC.Qy$Month)
timevals_out.APAC.Qy<- outdata.APAC.Qy$Month

global_pred_out.APAC.Qy<- predict(lmfit.APAC.Qy,data.frame(Month =timevals_out.APAC.Qy))

fcast.APAC.Qy<- global_pred_out.APAC.Qy

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec.APAC.Qy<- accuracy(fcast.APAC.Qy,outdata.APAC.Qy[,2])[5]
MAPE_class_dec.APAC.Qy

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred.APAC.Qy<- c(ts(global_pred.APAC.Qy),ts(global_pred_out.APAC.Qy))
plot(total_timeser.APAC.Qy, col = "black")
lines(class_dec_pred.APAC.Qy, col = "red")


#So, that was classical decomposition, now let's do an ARIMA fit

autoarima.APAC.Qy<- auto.arima(timeser.APAC.Qy)
autoarima.APAC.Qy
tsdiag(autoarima.APAC.Qy)
plot(autoarima.APAC.Qy$x, col="black")
lines(fitted(autoarima.APAC.Qy), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima.APAC.Qy<- timeser.APAC.Qy- fitted(autoarima.APAC.Qy)

adf.test(resi_auto_arima.APAC.Qy,alternative = "stationary")
kpss.test(resi_auto_arima.APAC.Qy)

#Also, let's evaluate the model using MAPE
fcast_auto_arima.APAC.Qy<- predict(autoarima.APAC.Qy, n.ahead = 6)

MAPE_auto_arima.APAC.Qy<- accuracy(fcast_auto_arima.APAC.Qy$pred,outdata.APAC.Qy[,2])[5]
MAPE_auto_arima.APAC.Qy

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred.APAC.Qy<- c(fitted(autoarima.APAC.Qy),ts(fcast_auto_arima.APAC.Qy$pred))
plot(total_timeser.APAC.Qy, col = "black")
lines(auto_arima_pred.APAC.Qy, col = "red")


### To Conclude , Both AUTO ARIMA Model & Classical Decomposition gave the same performance for APAC CONSUMER DEMAND##
###The MAPE value in a Classical Decomposition is 98.56909 where as MAPE in Auto ARIMA is 98.71101###


#-----Forecast - next six month Sales-----------
forecast(timeser.APAC.Qy,h=6)
forecast(autoarima.APAC.Qy,h=6)


