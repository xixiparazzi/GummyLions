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

#adding perishable factor
groceryII_dt[item_nbr %in% items_dt[perishable == 0,], ':='(perishable, 1)]
groceryII_dt[, ':='(perishable, mapvalues(groceryII_dt$perishable, c(NA), c(1.25)))]
#adding store information
groceryII_dt <- merge(groceryII_dt, stores_dt, by = c("store_nbr"))
#creating full trainable data
groceryII_dt <- merge(groceryII_dt, groceryII_dat[,!c("V1","id")], by = c("date" , "store_nbr", "item_nbr"), all.x = TRUE)
setorderv(groceryII_dt, c("date", "store_nbr", "item_nbr"))
groceryII_dt[unit_sales < 0, ':='(unit_sales, 0)]
groceryII_dt[, ':='(unit_sales, mapvalues(groceryII_dt$unit_sales, c(NA), c(0)))]
groceryII_dt[, ':='(dow, weekdays(as.Date(date)))]


#add holiday events
#add local holiday
holidays_events_local <- holidays_events_dt[locale == "Local" & transferred == FALSE, ]
groceryII_dt[date %in% holidays_events_local$date & city %in% holidays_events_local$locale_name, ':='(transferred, FALSE)]
#adding regional holidays
holidays_events_regional <- holidays_events_dt[locale == "Regional" & transferred == FALSE, ]
groceryII_dt[date %in% holidays_events_regional$date & state %in% holidays_events_regional$locale_name, ':='(transferred, FALSE)]
#adding national holidays
holidays_events_national <- holidays_events_dt[locale == "National" & transferred == FALSE, ]
groceryII_dt[date %in% holidays_events_national$date, ':='(transferred, FALSE)]

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
groceryII_test_dt[, ':='(NWSLE, perishable * (log(pred+1)-log(unit_sales+1))**2)]
groceryII_test_NWRMSLE <- groceryII_test_dt[, lapply(.SD, sum), .SDcols = c("perishable", "NWSLE"), by = c("store_nbr", "item_nbr")]
setnames(groceryII_test_NWRMSLE, c("perishable", "NWSLE"), c("w_sum","NWSLE_sum"))
groceryII_test_NWRMSLE[, ':='(NWRMSLE, sqrt(NWSLE_sum / w_sum))]
summary(groceryII_test_NWRMSLE$NWRMSLE)

#adding the simple effect of promotion, when promotion, sales_unit * p_factor
p_factor <- 1.5
groceryII_test_dt[,':='(onpromotion, mapvalues(groceryII_test_dt$onpromotion, c(NA), c(FALSE)))]
groceryII_test_dt[onpromotion == TRUE, ':='(pred_w_promo, pred*p_factor)]
groceryII_test_dt[onpromotion == FALSE, ':='(pred_w_promo, pred)]
groceryII_test_dt[,':='(NWSLE_wP, perishable * (log(pred_w_promo+1)-log(unit_sales+1))**2)]
groceryII_test_NWRMSLE_wP <- groceryII_test_dt[, lapply(.SD, sum), .SDcols = c("perishable", "NWSLE_wP"), by = c("store_nbr", "item_nbr")]
setnames(groceryII_test_NWRMSLE_wP, c("perishable", "NWSLE_wP"), c("w_sum_wP","NWSLE_sum_wP"))
groceryII_test_NWRMSLE_wP[, ':='(NWRMSLE_wP, sqrt(NWSLE_sum_wP / w_sum_wP))]
summary(groceryII_test_NWRMSLE_wP$NWRMSLE_wP) #better than without the promotion multiplier, median is 0.2502, 3rd Quantile 6.113, have large outliers on the right tail

#adding the simple effect of holiday, multiply sales by h_factor
h_factor <- 0.9
groceryII_test_dt[,':='(transferred, mapvalues(groceryII_test_dt$transferred, c(NA), c(TRUE)))]
groceryII_test_dt[transferred == TRUE, ':='(pred_w_h, pred)]
groceryII_test_dt[transferred == FALSE, ':='(pred_w_h, pred*h_factor)]
groceryII_test_dt[,':='(NWSLE_wH, perishable * (log(pred_w_h+1)-log(unit_sales+1))**2)]
groceryII_test_NWRMSLE_wH <- groceryII_test_dt[, lapply(.SD, sum), .SDcols = c("perishable", "NWSLE_wH"), by = c("store_nbr", "item_nbr")]
setnames(groceryII_test_NWRMSLE_wH, c("perishable", "NWSLE_wH"), c("w_sum_wH","NWSLE_sum_wH"))
groceryII_test_NWRMSLE_wH[, ':='(NWRMSLE_wH, sqrt(NWSLE_sum_wH / w_sum_wH))]
summary(groceryII_test_NWRMSLE_wH$NWRMSLE_wH)

#adding the effect of both holiday and promotion, multiply pred_w_promo by h_factor
groceryII_test_dt[transferred == TRUE, ':='(pred_w_h_promo, pred_w_promo)]
groceryII_test_dt[transferred == FALSE, ':='(pred_w_h_promo, pred_w_promo*h_factor)]
groceryII_test_dt[,':='(NWSLE_wPH, perishable * (log(pred_w_h_promo+1)-log(unit_sales+1))**2)]
groceryII_test_NWRMSLE_wPH <- groceryII_test_dt[, lapply(.SD, sum), .SDcols = c("perishable", "NWSLE_wPH"), by = c("store_nbr", "item_nbr")]
setnames(groceryII_test_NWRMSLE_wPH, c("perishable", "NWSLE_wPH"), c("w_sum_wPH","NWSLE_sum_wPH"))
groceryII_test_NWRMSLE_wPH[, ':='(NWRMSLE_wPH, sqrt(NWSLE_sum_wPH / w_sum_wPH))]
summary(groceryII_test_NWRMSLE_wPH$NWRMSLE_wPH)

