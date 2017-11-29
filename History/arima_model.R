library(data.table)
library(DT)
library(dplyr)
library(ggplot2)
library(plyr)
library(forecast)
library(tseries)

#read train, test and submission dataset
train_dt <- fread(input = '../GummyLions/train.csv')
test_dt <- fread(input = '../GummyLions/test.csv')
sample_submission_dt <- fread(input = '../GummyLions/sample_submission.csv')

#read relevant data table
holidays_events_dt <- fread(input = '../GummyLions/holidays_events.csv')
items_dt <- fread(input = '../GummyLions/items.csv')
oil_dt <- fread(input = '../GummyLions/oil.csv')
stores_dt <- fread(input = '../GummyLions/stores.csv')
transactions_dt <- fread(input = '../GummyLions/transactions.csv')

#Sample data
item_groceryII <- items_dt[family == "GROCERY II", ]
sample_groceryII <- train_dt[item_nbr %in% item_groceryII$item_nbr, ]

#creating full range data for grocery
unique.dates <- unique(sample_groceryII$date)
unique.stores <- unique(sample_groceryII$store_nbr)
unique.items <- unique(sample_groceryII$item_nbr)
groceryII.train.all <- data.table(expand.grid(date = unique.dates, store_nbr = unique.stores, item_nbr = unique.items))
groceryII.train.all <- merge(groceryII.train.all, sample_groceryII, by = c("date" , "store_nbr", "item_nbr"), all.x = TRUE)
setorderv(groceryII.train.all, c("date", "store_nbr", "item_nbr") )
groceryII.train.all[, ':='(unit_sales, mapvalues(groceryII.train.all$unit_sales, c(NA), c(0)))]

#creating time series for one product
selection <- unique.items[2]
selection_data <- groceryII.train.all[item_nbr == selection & store_nbr == 1, ]
selection_data[unit_sales <0, ':='(unit_sales, 0)]
selection_data[, ':='(unit_sales, log(1+unit_sales))]
myts <- ts(selection_data$unit_sales[1306:1458], start = c(2016,1), end = c(2016,366), frequency = 366)
myts_test <- ts(selection_data$unit_sales[1458:1669], start = c(2017,1,1), frequency = 365.25)
plot(myts)

#testing stationarity
adf.test(selection_data$unit_sales, alternative = "stationary", k=0)
Acf(selection_data$unit_sales)
pacf(selection_data$unit_sales)

#simple arima auto fit trial
fit1 <- auto.arima(myts)
myts.predict <- predict(fit, n.ahead=212)
test <- groceryII.train.all[item_nbr == selection & store_nbr == 1, ][1458:1669,]
selection_data[unit_sales <0, ':='(unit_sales, 0)]
myts_predict <- data.table(myts.predict$pred)
myts_predict[, ':='(V1, exp(V1)-1)]
ts.plot(test$unit_sales, myts_predict$pred, gpars = list(col = c("black", "blue"))) #the prediction result does not seem so good
Error <- myts_predict$V1 - test$unit_sales
ts.plot(Error)
squared_error <- Error ** 2
MSE <- sum(squared_error)/212

#days of the week trial

