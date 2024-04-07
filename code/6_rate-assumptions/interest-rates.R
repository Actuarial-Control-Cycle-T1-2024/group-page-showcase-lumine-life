library(openxlsx)
historical_interest<-read.xlsx("soa-materials/srcsc-2024-lumaria-economic-data.xlsx", startRow = 12)
inflation<-ts(data=historical_interest$Inflation, start = 1962, end = 2023)
overnight<-ts(data = historical_interest$Government.of.Lumaria.Overnight.Rate, start = 1962, end = 2023)
oneyear<-ts(data = historical_interest$`1-yr.Risk.Free.Annual.Spot.Rate`, start = 1962, end = 2023)
tenyear<-ts(data = historical_interest$`10-yr.Risk.Free.Annual.Spot.Rate`, start = 1962, end = 2023)


#Construct ARMA models for each historical interest rate and forecast interest rates for the next 5 years with a 95% CI'

acf(inflation)
pacf(inflation)
#Use ACF and PACF to determine best model for each interest rate'
inflation_AR<-arima(inflation, order = c (1,0,0))
print(inflation_AR)
ts.plot(inflation)
inflation_AR_fit<-inflation - residuals(inflation_AR)
#Graphing fit'
points(inflation_AR_fit,type = "l", col = 2, lty = 2)
#Forecasting future interest rates and graphing'
ts.plot(inflation, xlim = c(1962,2028))
inflation_forecast<-predict(inflation_AR, n.ahead = 5)
points(inflation_forecast$pred, type = "l", col = 2)
points(inflation_forecast$pred+2*inflation_forecast$se, type = "l", col = 2, lty = 2)
points(inflation_forecast$pred-2*inflation_forecast$se, type = "l", col = 2, lty = 2)

#Repeating for all rates
acf(overnight)
pacf(overnight)
overnight_AR<-arima(overnight, order = c (3,0,0))
print(overnight_AR)
ts.plot(overnight)
overnight_AR_fit<-overnight - residuals(overnight_AR)
points(overnight_AR_fit,type = "l", col = 2, lty = 2)
ts.plot(overnight, xlim = c(1962,2028), ylim = c(-0.02,0.18))
overnight_forecast<-predict(overnight_AR, n.ahead = 5)
points(overnight_forecast$pred, type = "l", col = 2)
points(overnight_forecast$pred+2*overnight_forecast$se, type = "l", col = 2, lty = 2)
points(overnight_forecast$pred-2*overnight_forecast$se, type = "l", col = 2, lty = 2)


acf(oneyear)
pacf(oneyear)
oneyear_AR<-arima(oneyear, order = c (1,0,0))
print(oneyear_AR)
ts.plot(oneyear)
oneyear_AR_fit<-oneyear - residuals(oneyear_AR)
points(oneyear_AR_fit,type = "l", col = 2, lty = 2)
ts.plot(oneyear, xlim = c(1962,2028), ylim = c(-0.01,0.16))
oneyear_forecast<-predict(oneyear_AR, n.ahead = 5)
points(oneyear_forecast$pred, type = "l", col = 2)
points(oneyear_forecast$pred+2*oneyear_forecast$se, type = "l", col = 2, lty = 2)
points(oneyear_forecast$pred-2*oneyear_forecast$se, type = "l", col = 2, lty = 2)


acf(tenyear)
pacf(tenyear)
tenyear_AR<-arima(tenyear, order = c (4,0,0))
print(tenyear_AR)
ts.plot(tenyear)
tenyear_AR_fit<-tenyear - residuals(tenyear_AR)
points(tenyear_AR_fit,type = "l", col = 2, lty = 2)
ts.plot(tenyear, xlim = c(1962,2028), ylim = c(-0.01,0.171))
tenyear_forecast<-predict(tenyear_AR, n.ahead = 5)
points(tenyear_forecast$pred, type = "l", col = 2)
points(tenyear_forecast$pred+2*tenyear_forecast$se, type = "l", col = 2, lty = 2)
points(tenyear_forecast$pred-2*tenyear_forecast$se, type = "l", col = 2, lty = 2)



#Table form for use elsewhere
inflation_rates<-matrix(c(inflation_forecast$pred,inflation_forecast$pred-2*inflation_forecast$se,inflation_forecast$pred+2*inflation_forecast$se),nrow = 5, ncol = 3, dimnames = list(2024:2028, c("Forecasted Inflation Rate", "Lower 95% CI", "Upper 95% CI")))
overnight_rates<-matrix(c(overnight_forecast$pred,overnight_forecast$pred-2*overnight_forecast$se,overnight_forecast$pred+2*overnight_forecast$se),nrow = 5, ncol = 3, dimnames = list(2024:2028, c("Forecasted overnight Rate", "Lower 95% CI", "Upper 95% CI")))
oneyear_rates<-matrix(c(oneyear_forecast$pred,oneyear_forecast$pred-2*oneyear_forecast$se,oneyear_forecast$pred+2*oneyear_forecast$se),nrow = 5, ncol = 3, dimnames = list(2024:2028, c("Forecasted oneyear Rate", "Lower 95% CI", "Upper 95% CI")))
tenyear_rates<-matrix(c(tenyear_forecast$pred,tenyear_forecast$pred-2*tenyear_forecast$se,tenyear_forecast$pred+2*tenyear_forecast$se),nrow = 5, ncol = 3, dimnames = list(2024:2028, c("Forecasted tenyear Rate", "Lower 95% CI", "Upper 95% CI")))





#All rates appear to be highly cross-correlated at 0 lag so can group forecast, lower , upper interest rates in scenario'
ccf(inflation, overnight)
ccf(inflation, oneyear)
ccf(inflation, tenyear)
ccf(overnight, oneyear)
ccf(overnight, tenyear)
ccf(oneyear, tenyear)

#Storing data
inflation_rates<-as.data.frame(inflation_rates)
output_data<-data.frame(year = c(1962:2028, "Long-term rate of inflation"), rate = append(historical_interest$Inflation,c(inflation_rates$`Forecasted Inflation Rate`, inflation_AR$coef[2])))
write.csv(output_data, file = "data/inflation-rates.csv")
write.xlsx(list("Inflation" = inflation_rates, "LumariaOvernightRate" = overnight_rates,"1-yrRiskFreeAnnualSpotRate" = oneyear_rates,"10-yrRiskFreeAnnualSpotRate" = tenyear_rates), file = "interest-rate-projections.xlsx")
