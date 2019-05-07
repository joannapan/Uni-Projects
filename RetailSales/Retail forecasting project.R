# Turnover of Victorian supermarket and groceries

retailts <- ts((read.csv("RetailData.csv", skip = 2)), frequency = 12, start = c(1982,4))[,23]
plot(retailts)
seasonplot(retailts)
#### ADF KPSS TEST ####

adf.test(retailts)
kpss.test(retailts) # alt hypothesis: non-stationary
#unit root tests indicate that retaildata is non-stationary

# Transform data
lambda <- BoxCox.lambda(retailts)
retail.bc <- BoxCox(retailts, lambda = lambda)
plot(retail.bc)
tsdisplay(retail.bc)

# Differencing
nsdiffs(retailts)
ndiffs(retailts)

## GIVEN DATA ##

retail1 <- window(retailts, end = 2009.99)
retail2 <- window(retailts, start = 2010)

retail1.bc <- BoxCox(retail1, lambda = lambda)
retail2.bc <- BoxCox(retail2, lambda = lambda)

###########################

###### ARIMA ######

ret.arima <- auto.arima(retail.bc)
# auto.ARIMA(2,1,1)(2,0,0) [12]
# ARIMA1(1,1,1)(2,1,1)

# ARIMA models
arima1 <- Arima(retail.bc, order = c(1,1,1), seasonal = c(2,1,1))
plot(forecast(arima1, h = 24))
tsdisplay(residuals(arima1))
tsdisplay(residuals(ret.arima))

arima2 <- Arima(retail.bc, order = c(2,1,1), seasonal = c(2,1,0))
plot(forecast(arima2, h = 24))
tsdisplay(residuals(arima2))

arima3 <- Arima(retail.bc, order = c(1,1,1), seasonal = c(1,1,1))

arima1

################

# Test ARIMA models

a0 <- forecast(Arima(retail1.bc, order = c(2,1,1), seasonal = c(2,0,0)), h = 24)
a1 <- forecast(Arima(retail1.bc, order = c(1,1,1), seasonal = c(2,1,1)), h = 24)
a2 <- forecast(Arima(retail1.bc, order = c(2,1,1), seasonal = c(2,1,0)), h = 24)
a3 <- forecast(Arima(retail1.bc, order = c(1,1,1), seasonal = c(1,1,1)), h = 24)


accuracy(a0, retail2.bc)
accuracy(a1, retail2.bc)
accuracy(a2, retail2.bc)
accuracy(a3, retail2.bc)

# Choose arima1

# Diagnostics arima1
res.a1 <- residuals(a1)
Box.test(res.a1, lag=12, fitdf=12, type="Ljung")

###########################

# autoARIMA and ETS comparison plot
plot(forecast(ret.arima, h = 24))
lines(retail.ets$mean, col ="red")

###########################

###### ETS ######

retail.ets <- forecast(ets(retail.bc), h = 24)
# ETS(A,A,A)

plot(retail.ets)
ls(retail.ets)
retail.ets$method

e0 <- forecast(ets(retail1, "AAA"), h = 24)   # AICc = 4035.589 (auto)
e1 <- forecast(ets(retail1, "MAA"), h = 24)   # AICc = 4060.981
e2 <- forecast(ets(retail1, "MMM"), h = 24)   # AICc = 3943.237
e3 <- forecast(ets(retail1, "MAM"), h = 24)   # AICc = 3944.672


plot(e0)

accuracy(retail2, e0$mean)   # RMSE = 87.06782
accuracy(retail2, e1$mean)   # RMSE = 70.54786
accuracy(retail2, e2$mean)   # RMSE = 92.98789
accuracy(retail2, e3$mean)   # RMSE = 82.678
#choose e2

# Diagnostics e2
res.e2 <- residuals(e2)
Box.test(res.e2, lag=12, fitdf=12, type="Ljung")

#################
    #ABS#
#################

absretail <- ts(c(1737,1649,1759.3,1695.3,1705.7,1671,1748.2,1804,1764.4,1836.7,1878.9,2068.9,1886.5,1738.8,1951.5,1785,1832.6,1763.1,1818.5,1901.4,1814.6,1948.2,1963.8,2234.6), start = c(2012, 1), frequency = 12)

abs.arima <- forecast(Arima(retailts, order = c(1,1,1), seasonal = c(2,1,1), lambda = lambda), h = 24)

abs.ets <- forecast(ets(retailts, "MMM"), h = 24)

###### PLOT ETS ABS ######
plot(abs.ets$mean, lwd = 4)
lines(absretail, col = "grey", lwd = 4)
##########################

accuracy(abs.ets, absretail)

###### PLOT ARIMA ABS ######
plot(abs.arima)
lines(absretail, col = "red")
############################

accuracy(abs.arima, absretail)  









