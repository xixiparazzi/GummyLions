library(data.table)
library(DT)
library(dplyr)
library(ggplot2)
library(plyr)
library(forecast)
library(tseries)

#read relevant data table
holidays_events_dt <- fread(input = '../GummyLions/holidays_events.csv')
items_dt <- fread(input = '../GummyLions/items.csv')
oil_dt <- fread(input = '../GummyLions/oil.csv')
stores_dt <- fread(input = '../GummyLions/stores.csv')
transactions_dt <- fread(input = '../GummyLions/transactions.csv')

#read and clean data
groceryII_dat <- fread(input = '../GummyLions/sample_groceryII.csv')
unique.dates <- unique(groceryII_dat$date)
unique.stores <- unique(groceryII_dat$store_nbr)
unique.items <- unique(groceryII_dat$item_nbr)
groceryII_dt <- data.table(expand.grid(date = unique.dates, store_nbr = unique.stores, item_nbr = unique.items))
groceryII_dt <- merge(groceryII_dt, groceryII_dat, by = c("date" , "store_nbr", "item_nbr"), all.x = TRUE)
setorderv(groceryII_dt, c("date", "store_nbr", "item_nbr"))
groceryII_dt[unit_sales < 0, ':='(unit_sales, 0)]
groceryII_dt[, ':='(unit_sales, mapvalues(groceryII_dt$unit_sales, c(NA), c(0)))]
groceryII_dt[, ':='(dow, weekdays(as.Date(date)))]

#split to train and test data set
groceryII_train_dt <- groceryII_dt[date >= '2016-08-01' & date < '2017-08-01', ]
groceryII_test_dt <- groceryII_dt[date >= '2017-08-01', ]
groceryII_train_dt[, ':='(unit_sales,log(1+unit_sales))]

#days of week demand factors
weekly_factor <- groceryII_train_dt[, lapply(.SD, mean), .SDcols = c("unit_sales"), by = c('store_nbr', 'item_nbr', 'dow')]
setnames(weekly_factor, "unit_sales", "average_dow")
weekly_factor[, ':='(average, mean(average_dow)), by = c('store_nbr', 'item_nbr')]
weekly_factor[, ':='(factor, average_dow/average)]
weekly_factor[, ':='(factor, mapvalues(weekly_factor$factor, c(NaN), c(0)))]

#moving averages
last_date <- as.Date('2017-07-31')
ma <- groceryII_train_dt[date == last_date, lapply(.SD, mean), .SDcols = "unit_sales", by = c('store_nbr', 'item_nbr')]
setnames(ma, "unit_sales", "ma0")
for (i in 1:10){
  date_cst <- last_date - as.difftime(i*7, unit = "days")
  temp <- groceryII_train_dt[date >= date_cst, lapply(.SD, mean), .SDcols = "unit_sales", by = c('store_nbr', 'item_nbr')]
  setnames(temp, "unit_sales", paste("ma", toString(i), sep =''))
  ma <- merge(ma, temp)
}
ma[, ':='(ma_median, apply(ma, 1, median))]

#calculate prediction for test data
groceryII_test_dt <- merge(groceryII_test_dt, ma[, c('store_nbr','item_nbr','ma_median')], by = c('store_nbr', 'item_nbr'), all.x = TRUE)
groceryII_test_dt <- merge(groceryII_test_dt, weekly_factor[, c('store_nbr','item_nbr', 'dow','factor')], by = c('store_nbr', 'item_nbr', 'dow'), all.x = TRUE)
groceryII_test_dt[, ':='(pred, exp(ma_median*factor)-1)]
groceryII_test_dt[, ':='(diff, pred-unit_sales)]
groceryII_test_dt[, ':='(squared_error, diff ** 2)]
groceryII_test_MSE <- groceryII_test_dt[, lapply(.SD, mean), .SDcols = c("squared_error"), by = c("store_nbr", "item_nbr")]
setnames(groceryII_test_MSE, "squared_error", "MSE")
summary(groceryII_test_MSE$MSE) #the summary shows that the prediction for some products are very good while others are very bad
