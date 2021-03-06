library(readxl)
####Task 1####
R2 = function(y, fit_line){
  SSE = sum(residuals(fit_line)**2)
  SST = sum((y - mean(y))**2)
  R2 = 1 - (SSE/SST)
  return(R2)
}

#Defects####
#No supplier initiative####
dfDefects <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "Defects After Delivery", skip = 3)
Defects.y= c(dfDefects$`2010`, dfDefects$`2011`[1:8])
Defects.x = 1:length(Defects.y)
par(mfrow = c(2,2)) #graph display 2x2, 4 graphs in same window

# Linear
Defects.fit_lin = lm(Defects.y~Defects.x)
plot(Defects.x, Defects.y, main = c('Defects Linear',paste('R2 value:',R2(Defects.y, Defects.fit_lin))))
abline(Defects.fit_lin, col = 'red')

# Exponential alternative
f_exp = function(x,a,b) {a* exp(b*x)}
Defects.model_exp = lm(log(Defects.y)~Defects.x)
Defects.start_exp = list(a = exp(coef(Defects.model_exp)[1]), b = coef(Defects.model_exp)[2])
Defects.fit_exp = nls(Defects.y~f_exp(Defects.x, a, b), start = Defects.start_exp)
Defects.co_exp = coef(Defects.fit_exp)
plot(Defects.x, Defects.y, main = c('Defects Exponential',paste('R2 value:',R2(Defects.y, Defects.fit_exp)))) ###line fit 2?
curve(f_exp(x, a = Defects.co_exp[1], b = Defects.co_exp[2]), add = TRUE, col = 'red')

# Logarithmic
f_log = function(x,a,b){(a*log(x) + b)}
Defects.fit_log = nls(Defects.y ~ f_log(Defects.x,a,b), start = c(a=1,b=1))
Defects.co_log = coef(Defects.fit_log)
plot(Defects.x, Defects.y, main = c("Defects Logarithmic", paste('R2 value:',R2(Defects.y, Defects.fit_log))))
curve(f_log(x, a = Defects.co_log[1], b= Defects.co_log[2]), add = TRUE, col = 'red')

# Polynomial
f_poly = function(x,a,b,d,e){(a*x**3) + (b*x**2) + (d*x) + e}
Defects.fit_poly = nls(Defects.y ~ f_poly(Defects.x,a,b,d,e), start = c(a=1, b=1, d=1, e=1))
Defects.co_poly = coef(Defects.fit_poly)
plot(Defects.x, Defects.y, main = c('Defects 3rd Degree Polynomial', paste('R2 value:',R2(Defects.y, Defects.fit_poly))))
curve(f_poly(x, a = Defects.co_poly[1], b = Defects.co_poly[2], d = Defects.co_poly[3], e = Defects.co_poly[4]), add = TRUE, col = 'red')

#see which one has the best rsquared value, linear/expo/ etc

#Predicting defect values
par(mfrow=c(1,1))
Defects.fit_poly
defects.predict = data.frame(21:32)
colnames(defects.predict) = 'x'
defects.predict$y = f_poly(defects.predict$x, a = Defects.co_poly[1], b = Defects.co_poly[2], d = Defects.co_poly[3], e = Defects.co_poly[4])
defects.predict

plot(defects.predict, main = 'Predicted Defects\nNo Supplier Initiative', type = 'o')

#With Supplier Initiative####
Defects.y= c(dfDefects$`2010`, dfDefects$`2011`, dfDefects$`2012`,dfDefects$`2013`,dfDefects$`2014`)
Defects.x = 1:length(Defects.y)
par(mfrow = c(2,2)) #graph display 2x2, 4 graphs in same window

# Linear
Defects.fit_lin = lm(Defects.y~Defects.x)
plot(Defects.x, Defects.y, main = c('Defects Linear',paste('R2 value:',R2(Defects.y, Defects.fit_lin))))
abline(Defects.fit_lin, col = 'red')

# Exponential alternative
f_exp = function(x,a,b) {a* exp(b*x)}
Defects.model_exp = lm(log(Defects.y)~Defects.x)
Defects.start_exp = list(a = exp(coef(Defects.model_exp)[1]), b = coef(Defects.model_exp)[2])
Defects.fit_exp = nls(Defects.y~f_exp(Defects.x, a, b), start = Defects.start_exp)
Defects.co_exp = coef(Defects.fit_exp)
plot(Defects.x, Defects.y, main = c('Defects Exponential',paste('R2 value:',R2(Defects.y, Defects.fit_exp)))) ###line fit 2?
curve(f_exp(x, a = Defects.co_exp[1], b = Defects.co_exp[2]), add = TRUE, col = 'red')

# Logarithmic
f_log = function(x,a,b){(a*log(x) + b)}
Defects.fit_log = nls(Defects.y ~ f_log(Defects.x,a,b), start = c(a=1,b=1))
Defects.co_log = coef(Defects.fit_log)
plot(Defects.x, Defects.y, main = c("Defects Logarithmic", paste('R2 value:',R2(Defects.y, Defects.fit_log))))
curve(f_log(x, a = Defects.co_log[1], b= Defects.co_log[2]), add = TRUE, col = 'red')

# Polynomial
f_poly = function(x,a,b,d,e){(a*x**3) + (b*x**2) + (d*x) + e}
Defects.fit_poly = nls(Defects.y ~ f_poly(Defects.x,a,b,d,e), start = c(a=1, b=1, d=1, e=1))
Defects.co_poly = coef(Defects.fit_poly)
plot(Defects.x, Defects.y, main = c('Defects 3rd Degree Polynomial', paste('R2 value:',R2(Defects.y, Defects.fit_poly))))
curve(f_poly(x, a = Defects.co_poly[1], b = Defects.co_poly[2], d = Defects.co_poly[3], e = Defects.co_poly[4]), add = TRUE, col = 'red')

#see which one has the best rsquared value, linear/expo/ etc

#Predicting defect values
par(mfrow=c(1,1))
Defects.fit_poly
defects.predict = data.frame(61:72)
colnames(defects.predict) = 'x'
defects.predict$y = f_poly(defects.predict$x, a = Defects.co_poly[1], b = Defects.co_poly[2], d = Defects.co_poly[3], e = Defects.co_poly[4])
defects.predict

plot(defects.predict, main = 'Predicted Defects\nWith Supplier Initiative', type = 'o')


#Employee retention####
dfEmployee_Retention <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "Employee Retention", skip = 2)
dfEmployee_Retention = dfEmployee_Retention[1:4]

## Multiple Regression
fit = lm(dfEmployee_Retention$YearsPLE~ dfEmployee_Retention$YrsEducation+dfEmployee_Retention$`College GPA`+dfEmployee_Retention$Age)
summary(fit)

# Removing YrsEducation:
fit = lm(dfEmployee_Retention$YearsPLE~ dfEmployee_Retention$`College GPA`+dfEmployee_Retention$Age)
summary(fit)

# Removing College GPA:
fit = lm(dfEmployee_Retention$YearsPLE~ dfEmployee_Retention$Age)
summary(fit)

## Error Analysis
par(mfrow = c(2, 2))

fit = lm(dfEmployee_Retention$YearsPLE~ dfEmployee_Retention$Age)
plot(fit, main = 'Retention and Age')
par(mfrow = c(1, 1))
hist(rstandard(fit), main = 'Retention and Age', xlab = 'Standard Residuals')
mean(rstandard(fit))

#Engines####
dfEngines <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "Engines", skip = 2)
Engine.Sample = dfEngines$Sample
Production.Time = dfEngines$`Production Time (min)`
par(mfrow = c(2,2)) #graph display 2x2, 4 graphs in same window

# Linear
Engines.fit_lin = lm(Production.Time~Engine.Sample)
plot(Engine.Sample, Production.Time, main = c('Engines Linear',paste('R2 value:',R2(Production.Time, Engines.fit_lin))))
abline(Engines.fit_lin, col = 'red')

# Exponential
f_exp = function(x,a,b) {a* exp(b*x)}
Engines.model_exp = lm(log(Production.Time)~Engine.Sample)
Engines.start_exp = list(a = exp(coef(Engines.model_exp)[1]), b = coef(Engines.model_exp)[2])
Engines.fit_exp = nls(Production.Time~f_exp(Engine.Sample, a, b), start = Engines.start_exp)
Engines.co_exp = coef(Engines.fit_exp)
plot(Engine.Sample, Production.Time, main = c('Engines Exponential',paste('R2 value:',R2(Production.Time, Engines.fit_exp)))) ###line fit 2?
curve(f_exp(x, a = Engines.co_exp[1], b = Engines.co_exp[2]), add = TRUE, col = 'red')

# Logarithmic
f_log = function(x,a,b){(a*log(x) + b)}
Engines.fit_log = nls(Production.Time ~ f_log(Engine.Sample,a,b), start = c(a=1,b=1))
Engines.co_log = coef(Engines.fit_log)
plot(Engine.Sample, Production.Time, main = c("Engines Logarithmic",paste('R2 value:',R2(Production.Time, Engines.fit_log))))
curve(f_log(x, a = Engines.co_log[1], b= Engines.co_log[2]), add = TRUE, col = 'red')

# Polynomial
f_poly = function(x,a,b,d,e){(a*x**3) + (b*x**2) + (d*x) + e}
Engines.fit_poly = nls(Production.Time ~ f_poly(Engine.Sample,a,b,d,e), start = c(a=1, b=1, d=1, e=1))
Engines.co_poly = coef(Engines.fit_poly)
plot(Engine.Sample, Production.Time, main = c('Engines 3rd degree Polynomial',paste('R2 value:',R2(Production.Time, Engines.fit_poly))))
curve(f_poly(x, a = Engines.co_poly[1], b = Engines.co_poly[2], d = Engines.co_poly[3], e = Engines.co_poly[4]), add = TRUE, col = 'red')

Engines.fit_log

####Task 2####
dfMower_Unit_Sales <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "Mower Unit Sales", skip = 2)
dfTractor_Unit_Sales <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "Tractor Unit Sales", skip = 2)
dfIndustry_Mower_Total_Sales <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "Industry Mower Total Sales", skip = 2)
dfIndustry_Tractor_Total_Sales <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "Industry Tractor Total Sales", skip = 2)
dfUnit_Production_Costs <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "Unit Production Costs", skip = 2)

#Preparing data sets
names(dfMower_Unit_Sales)             = c("Month","NA","SA","Eur","Pacific","China","World")
names(dfTractor_Unit_Sales)           = c("Month","NA","SA","Eur","Pacific","China","World")
names(dfIndustry_Mower_Total_Sales)   = c("Month","NA","SA","Eur","Pacific","World")
names(dfIndustry_Tractor_Total_Sales) = c("Month","NA","SA","Eur","Pacific","China","World")

Mower_Forecasts = data.frame(matrix(nrow = 12,ncol = 0)) ######create data frame with 12 rows 
Tractor_Forecasts = data.frame(matrix(nrow = 12,ncol = 0))
years = c(2010:2015)

library(TTR)
library(forecast)

##Forecasting Mower Unit Sales####
par(mfrow=c(1,1))
for(region in c("NA","SA","Eur","Pacific","China","World")){
  ts.region=ts(dfMower_Unit_Sales[region], frequency = 12)
  plot(ts.region, main=paste('Time Series for',region), ylab='Number of Mower Unit Sales',xaxt = 'n')
  axis(1, at=1:6, labels = years)
  plot(decompose(ts.region))
}
#Regions with no trend and no seasonality####
#China
#Simple exponential smoothing
for(region in c("China")){
  ts.Mower_Unit_Sales.region=ts(dfMower_Unit_Sales[49:60,region], frequency = 12)
  
  hw.Mower_Unit_Sales.region<-HoltWinters(ts.Mower_Unit_Sales.region, beta = FALSE, gamma = FALSE)
  plot(hw.Mower_Unit_Sales.region, main=paste('Holt-Winters filtering\n Mower Unit Sales',region),xaxt = 'n')
  axis(1, at=1:2, labels = 2014:2015)
  legend("bottomleft", c('Observed', 'Simple exponential smoothing'), col = c('Black', ' Red'),lwd = 1, cex=0.8)
  cat('\n',region,'\n')
  print(hw.Mower_Unit_Sales.region)
  
  forecast.region = forecast(hw.Mower_Unit_Sales.region,12)
  cat(paste('\nHoltWinters Forecasts for',region,'\n'))
  print(forecast.region)
  Mower_Forecasts[paste('Unit_Sales',region)] = forecast.region$mean
  
  plot(forecast.region, main = paste('HoltWinters Forecasts\n Mower Unit Sales',region), xaxt = 'n')
  axis(1, at=1:2, labels = 2014:2015)
  
  cat(paste('\nHoltWinters Forecasts accuracy for',region,'\n'))
  print(accuracy(forecast.region))
}

#Alternative: SMA
ts.China = ts(dfMower_Unit_Sales$China[52:60], frequency = 12)

## Find best model
library(TTR)
sma_error = function(timeseries){
  for(i in 2:8){ #9 entries
    model = SMA(timeseries, i)
    model = append(model, NA, after = 0)
    timeseries.append = append(timeseries, NA, after = 9)
    ts.mad = mean(abs(timeseries.append - model), na.rm = TRUE)
    ts.mape = mean(abs(timeseries.append - model)/timeseries.append, na.rm = TRUE)
    ts.mse = mean((timeseries.append - model) **2, na.rm = TRUE)
    print(c(i,ts.mad, ts.mse, ts.mape))
  }
}

sma_error(ts.China)

# Identified 2 as least error
new_sma2 = SMA(ts.China, 2) #2month moving average, 1st forecast at position 2

#Adding NAs
ts.China = append(ts.China, NA, after = 9)
new_sma2 = append(new_sma2, NA, after = 0)

# plot different lines together
plot(ts.China, type = 'o', ylab = 'Rate', main = 'Mower Unit Sales in China', xaxt = 'n', xlab = 'Time')
axis(1, at = 1:9, labels = dfMower_Unit_Sales$Month[52:60])
lines(new_sma2, type = 'o', col = 'red')
legend("topright", c('Observed', '2-Month Moving Average'), col = c('Black', 'Red'), lwd = 1)

#Regions with    trend and no seasonality####
#Pacific
#Double exponential smoothing
for(region in c("Pacific")){
  ts.Mower_Unit_Sales.region=ts(dfMower_Unit_Sales[region], frequency = 12)
  
  hw.Mower_Unit_Sales.region<-HoltWinters(ts.Mower_Unit_Sales.region, gamma = FALSE)
  plot(hw.Mower_Unit_Sales.region, main=paste('Holt-Winters filtering\n Mower Unit Sales',region),xaxt = 'n')
  axis(1, at=1:6, labels = years)
  legend("topleft", c('Observed', 'Double exponential smoothing'), col = c('Black', ' Red'),lwd = 1, cex=0.8)
  cat('\n',region,'\n')
  print(hw.Mower_Unit_Sales.region)
  
  forecast.region = forecast(hw.Mower_Unit_Sales.region,12)
  cat(paste('\nHoltWinters Forecasts for',region,'\n'))
  print(forecast.region)
  Mower_Forecasts[paste('Unit_Sales',region)] = forecast.region$mean
  
  plot(forecast.region, main = paste('HoltWinters Forecasts\n Mower Unit Sales',region), xaxt = 'n')
  axis(1, at=1:6, labels = years)
  
  cat(paste('\nHoltWinters Forecasts accuracy for',region,'\n'))
  print(accuracy(forecast.region))
}

#Regions with no trend and    seasonality####
#NA, World
#Holt-Winters no-trend smoothing
for(region in c("NA","World")){
  ts.Mower_Unit_Sales.region=ts(dfMower_Unit_Sales[region], frequency = 12)
  
  hw.Mower_Unit_Sales.region<-HoltWinters(ts.Mower_Unit_Sales.region, beta = FALSE)
  plot(hw.Mower_Unit_Sales.region, main=paste('Holt-Winters filtering\n Mower Unit Sales',region),xaxt = 'n')
  axis(1, at=1:6, labels = years)
  legend("topleft", c('Observed', 'Holt-Winters no-trend smoothing'), col = c('Black', ' Red'),lwd = 1, cex=0.8)
  cat('\n',region,'\n')
  print(hw.Mower_Unit_Sales.region)

  forecast.region=forecast(hw.Mower_Unit_Sales.region,12)
  cat(paste('\nHoltWinters Forecasts for',region,'\n'))
  print(forecast.region)
  Mower_Forecasts[paste('Unit_Sales',region)] = forecast.region$mean
  
  plot(forecast.region, main = paste('HoltWinters Forecasts\n Mower Unit Sales',region), xaxt = 'n')
  axis(1, at=1:6, labels = years)

  cat(paste('\nHoltWinters Forecasts accuracy for',region,'\n'))
  print(accuracy(forecast.region))
}

#Regions with    trend and    seasonality####
#SA(add),Eur(add)
#Holt-Winters additive model
for(region in c("SA","Eur")){
  ts.Mower_Unit_Sales.region=ts(dfMower_Unit_Sales[region], frequency = 12)
  
  hw.Mower_Unit_Sales.region<-HoltWinters(ts.Mower_Unit_Sales.region)
  plot(hw.Mower_Unit_Sales.region, main=paste('Holt-Winters filtering\n Mower Unit Sales',region),xaxt = 'n')
  axis(1, at=1:6, labels = years)
  legend("topleft", c('Observed', 'Holt-Winters additive smoothing'), col = c('Black', ' Red'),lwd = 1, cex=0.8)
  cat('\n',region,'\n')
  print(hw.Mower_Unit_Sales.region)
  
  forecast.region=forecast(hw.Mower_Unit_Sales.region,12)
  cat(paste('\nHoltWinters Forecasts for',region,'\n'))
  print(forecast.region)
  Mower_Forecasts[paste('Unit_Sales',region)] = forecast.region$mean
  
  plot(forecast.region, main = paste('HoltWinters Forecasts\n Mower Unit Sales',region), xaxt = 'n')
  axis(1, at=1:6, labels = years)
  
  cat(paste('\nHoltWinters Forecasts accuracy for',region,'\n'))
  print(accuracy(forecast.region))
}

##Forecasting Tractor Unit Sales####
par(mfrow=c(1,1))
for(region in c("NA","SA","Eur","Pacific","China","World")){
  ts.region=ts(dfTractor_Unit_Sales[region], frequency = 12)
  plot(ts.region, main=paste('Time Series for',region), ylab='Number of Tractor Unit Sales',xaxt = 'n')
  axis(1, at=1:6, labels = years)
  plot(decompose(ts.region))
}

#Regions with    trend and no seasonality####
#China
#Double exponential smoothing
for(region in c("China")){
  ts.Tractor_Unit_Sales.region=ts(dfTractor_Unit_Sales[region], frequency = 12)
  
  hw.Tractor_Unit_Sales.region<-HoltWinters(ts.Tractor_Unit_Sales.region, gamma = FALSE)
  plot(hw.Tractor_Unit_Sales.region, main=paste('Holt-Winters filtering\n Tractor Unit Sales',region),xaxt = 'n')
  axis(1, at=1:6, labels = years)
  legend("topleft", c('Observed', 'Double exponential smoothing'), col = c('Black', ' Red'),lwd = 1,cex=0.8)
  cat('\n',region,'\n')
  print(hw.Tractor_Unit_Sales.region)
  
  forecast.region=forecast(hw.Tractor_Unit_Sales.region,12)
  cat(paste('\nHoltWinters Forecasts for',region,'\n'))
  print(forecast.region)
  Tractor_Forecasts[paste('Unit_Sales',region)] = forecast.region$mean
  
  plot(forecast.region, main = paste('HoltWinters Forecasts\n Tractor Unit Sales',region), xaxt = 'n')
  axis(1, at=1:6, labels = years)
  
  cat(paste('\nHoltWinters Forecasts accuracy for',region,'\n'))
  print(accuracy(forecast.region))
}

#Regions with no trend and    seasonality####
#Pacific
#Holt-Winters no-trend smoothing
for(region in c("Pacific")){
  ts.Tractor_Unit_Sales.region=ts(dfTractor_Unit_Sales[region], frequency = 12)
  
  hw.Tractor_Unit_Sales.region<-HoltWinters(ts.Tractor_Unit_Sales.region, beta = FALSE)
  plot(hw.Tractor_Unit_Sales.region, main=paste('Holt-Winters filtering\n Tractor Unit Sales',region),xaxt = 'n')
  axis(1, at=1:6, labels = years)
  legend("topleft", c('Observed', 'Holt-Winters no-trend smoothing'), col = c('Black', ' Red'),lwd = 1, cex=0.8)
  cat('\n',region,'\n')
  print(hw.Tractor_Unit_Sales.region)
  
  forecast.region = forecast(hw.Tractor_Unit_Sales.region,12)
  cat(paste('\nHoltWinters Forecasts for',region,'\n'))
  print(forecast.region)
  Tractor_Forecasts[paste('Unit_Sales',region)] = forecast.region$mean
  
  plot(forecast.region, main = paste('HoltWinters Forecasts\n Tractor Unit Sales',region), xaxt = 'n')
  axis(1, at=1:6, labels = years)
  
  cat(paste('\nHoltWinters Forecasts accuracy for',region,'\n'))
  print(accuracy(forecast.region))
}

#Regions with    trend and    seasonality####
#NA (mul),SA (additive),Eur(mul),World (mul)
#Holt-Winters additive model
for(region in c("SA")){
  ts.Tractor_Unit_Sales.region=ts(dfTractor_Unit_Sales[region], frequency = 12)
  
  hw.Tractor_Unit_Sales.region<-HoltWinters(ts.Tractor_Unit_Sales.region)
  plot(hw.Tractor_Unit_Sales.region, main=paste('Holt-Winters filtering\n Tractor Unit Sales',region),xaxt = 'n')
  axis(1, at=1:6, labels = years)
  legend("topleft", c('Observed', 'Holt-Winters additive smoothing'), col = c('Black', ' Red'),lwd = 1, cex=0.8)
  cat('\n',region,'\n')
  print(hw.Tractor_Unit_Sales.region)
  
  forecast.region=forecast(hw.Tractor_Unit_Sales.region,12)
  cat(paste('\nHoltWinters Forecasts for',region,'\n'))
  print(forecast.region)
  Tractor_Forecasts[paste('Unit_Sales',region)] = forecast.region$mean
  
  plot(forecast.region, main = paste('HoltWinters Forecasts\n Tractor Unit Sales',region), xaxt = 'n')
  axis(1, at=1:6, labels = years)
  
  cat(paste('\nHoltWinters Forecasts accuracy for',region,'\n'))
  print(accuracy(forecast.region))
}

#Holt-Winters multiplicative model
for(region in c("NA","Eur","World")){
  ts.Tractor_Unit_Sales.region=ts(dfTractor_Unit_Sales[region], frequency = 12)
  
  hw.Tractor_Unit_Sales.region<-HoltWinters(ts.Tractor_Unit_Sales.region, seasonal = 'mult')
  plot(hw.Tractor_Unit_Sales.region, main=paste('Holt-Winters filtering\n Tractor Unit Sales',region),xaxt = 'n')
  axis(1, at=1:6, labels = years)
  legend("topleft", c('Observed', 'Holt-Winters multiplicative smoothing'), col = c('Black', ' Red'),lwd = 1, cex=0.8)
  cat('\n',region,'\n')
  print(hw.Tractor_Unit_Sales.region)
  
  forecast.region=forecast(hw.Tractor_Unit_Sales.region,12)
  cat(paste('\nHoltWinters Forecasts for',region,'\n'))
  print(forecast.region)
  Tractor_Forecasts[paste('Unit_Sales',region)] = forecast.region$mean
  
  plot(forecast.region, main = paste('HoltWinters Forecasts\n Tractor Unit Sales',region), xaxt = 'n')
  axis(1, at=1:6, labels = years)
  
  cat(paste('\nHoltWinters Forecasts accuracy for',region,'\n'))
  print(accuracy(forecast.region))
}

##Forecasting Industry Mower Total Sales####
par(mfrow=c(1,1))
for(region in c("NA","SA","Eur","Pacific","World")){
  ts.region=ts(dfIndustry_Mower_Total_Sales[region], frequency = 12)
  plot(ts.region, main=paste('Time Series for',region), ylab='Number of Industry Mower Total Sales',xaxt = 'n')
  axis(1, at=1:6, labels = years)
  plot(decompose(ts.region))
}
#Regions with    trend and no seasonality####
#Pacific
#Double exponential smoothing
for(region in c("Pacific")){
  ts.Industry_Mower_Total_Sales.region=ts(dfIndustry_Mower_Total_Sales[region], frequency = 12)
  
  hw.Industry_Mower_Total_Sales.region<-HoltWinters(ts.Industry_Mower_Total_Sales.region, gamma = FALSE)
  plot(hw.Industry_Mower_Total_Sales.region, main=paste('Holt-Winters filtering\n Industry Mower Total Sales',region),xaxt = 'n')
  axis(1, at=1:6, labels = years)
  legend("topleft", c('Observed', 'Double exponential smoothing'), col = c('Black', ' Red'),lwd = 1, cex=0.8)
  cat('\n',region,'\n')
  print(hw.Industry_Mower_Total_Sales.region)
  
  forecast.region = forecast(hw.Industry_Mower_Total_Sales.region,12)
  cat(paste('\nHoltWinters Forecasts for',region,'\n'))
  print(forecast.region)
  Mower_Forecasts[paste('Total_Sales',region)] = forecast.region$mean
  
  plot(forecast.region, main = paste('HoltWinters Forecasts\n Industry Mower Total Sales',region), xaxt = 'n')
  axis(1, at=1:6, labels = years)
  
  cat(paste('\nHoltWinters Forecasts accuracy for',region,'\n'))
  print(accuracy(forecast.region))
}

#Regions with no trend and    seasonality####
#NA, SA, Eur, World
#Holt-Winters no-trend smoothing
for(region in c("NA","SA","Eur","World")){
  ts.Industry_Mower_Total_Sales.region=ts(dfIndustry_Mower_Total_Sales[region], frequency = 12)
  
  hw.Industry_Mower_Total_Sales.region<-HoltWinters(ts.Industry_Mower_Total_Sales.region, beta = FALSE)
  plot(hw.Industry_Mower_Total_Sales.region, main=paste('Holt-Winters filtering\n Industry Mower Total Sales',region),xaxt = 'n')
  axis(1, at=1:6, labels = years)
  legend("topleft", c('Observed', 'Holt-Winters no-trend smoothing'), col = c('Black', ' Red'),lwd = 1, cex=0.8)
  cat('\n',region,'\n')
  print(hw.Industry_Mower_Total_Sales.region)
  
  forecast.region=forecast(hw.Industry_Mower_Total_Sales.region,12)
  cat(paste('\nHoltWinters Forecasts for',region,'\n'))
  print(forecast.region)
  Mower_Forecasts[paste('Total_Sales',region)] = forecast.region$mean
  
  plot(forecast.region, main = paste('HoltWinters Forecasts\n Industry Mower Total Sales',region), xaxt = 'n')
  axis(1, at=1:6, labels = years)
  
  cat(paste('\nHoltWinters Forecasts accuracy for',region,'\n'))
  print(accuracy(forecast.region))
}

#Regions with    trend and    seasonality####
#Nothing
#Holt-Winters additive model
for(region in c()){
  ts.Industry_Mower_Total_Sales.region=ts(dfIndustry_Mower_Total_Sales[region], frequency = 12)
  
  hw.Industry_Mower_Total_Sales.region<-HoltWinters(ts.Industry_Mower_Total_Sales.region)
  plot(hw.Industry_Mower_Total_Sales.region, main=paste('Holt-Winters filtering\n Industry Mower Total Sales',region),xaxt = 'n')
  axis(1, at=1:6, labels = years)
  legend("topleft", c('Observed', 'Holt-Winters additive smoothing'), col = c('Black', ' Red'),lwd = 1, cex=0.8)
  cat('\n',region,'\n')
  print(hw.Industry_Mower_Total_Sales.region)
  
  forecast.region=forecast(hw.Industry_Mower_Total_Sales.region,12)
  cat(paste('\nHoltWinters Forecasts for',region,'\n'))
  print(forecast.region)
  Mower_Forecasts[paste('Total_Sales',region)] = forecast.region$mean
  
  plot(forecast.region, main = paste('HoltWinters Forecasts\n Industry Mower Total Sales',region), xaxt = 'n')
  axis(1, at=1:6, labels = years)
  
  cat(paste('\nHoltWinters Forecasts accuracy for',region,'\n'))
  print(accuracy(forecast.region))
}

##Forecasting Industry Tractor Total Sales####
par(mfrow=c(1,1))
for(region in c("NA","SA","Eur","Pacific","China","World")){
  ts.region=ts(dfIndustry_Tractor_Total_Sales[region], frequency = 12)
  plot(ts.region, main=paste('Time Series for',region), ylab='Number of Industry Tractor Total Sales',xaxt = 'n')
  axis(1, at=1:6, labels = years)
  plot(decompose(ts.region))
}

#Regions with    trend and no seasonality####
#China
#Double exponential smoothing
for(region in c("China")){
  ts.Industry_Tractor_Total_Sales.region=ts(dfIndustry_Tractor_Total_Sales[region], frequency = 12)
  
  hw.Industry_Tractor_Total_Sales.region<-HoltWinters(ts.Industry_Tractor_Total_Sales.region, gamma = FALSE)
  plot(hw.Industry_Tractor_Total_Sales.region, main=paste('Holt-Winters filtering\n Industry Tractor Total Sales',region),xaxt = 'n')
  axis(1, at=1:6, labels = years)
  legend("topleft", c('Observed', 'Double exponential smoothing'), col = c('Black', ' Red'),lwd = 1, cex=0.8)
  cat('\n',region,'\n')
  print(hw.Industry_Tractor_Total_Sales.region)
  
  forecast.region=forecast(hw.Industry_Tractor_Total_Sales.region,12)
  cat(paste('\nHoltWinters Forecasts for',region,'\n'))
  print(forecast.region)
  Tractor_Forecasts[paste('Total_Sales',region)] = forecast.region$mean
  
  plot(forecast.region, main = paste('HoltWinters Forecasts\n Industry Tractor Total Sales',region), xaxt = 'n')
  axis(1, at=1:6, labels = years)
  
  cat(paste('\nHoltWinters Forecasts accuracy for',region,'\n'))
  print(accuracy(forecast.region))
}

#Regions with no trend and    seasonality####
#Eur, Pacific
#Holt-Winters no-trend smoothing
for(region in c("Eur","Pacific")){
  ts.Industry_Tractor_Total_Sales.region=ts(dfIndustry_Tractor_Total_Sales[region], frequency = 12)
  
  hw.Industry_Tractor_Total_Sales.region<-HoltWinters(ts.Industry_Tractor_Total_Sales.region, beta = FALSE)
  plot(hw.Industry_Tractor_Total_Sales.region, main=paste('Holt-Winters filtering\n Industry Tractor Total Sales',region),xaxt = 'n')
  axis(1, at=1:6, labels = years)
  legend("topleft", c('Observed', 'Holt-Winters no-trend smoothing'), col = c('Black', ' Red'),lwd = 1, cex=0.8)
  cat('\n',region,'\n')
  print(hw.Industry_Tractor_Total_Sales.region)
  
  forecast.region = forecast(hw.Industry_Tractor_Total_Sales.region,12)
  cat(paste('\nHoltWinters Forecasts for',region,'\n'))
  print(forecast.region)
  Tractor_Forecasts[paste('Total_Sales',region)] = forecast.region$mean
  
  plot(forecast.region, main = paste('HoltWinters Forecasts\n Industry Tractor Total Sales',region), xaxt = 'n')
  axis(1, at=1:6, labels = years)
  
  cat(paste('\nHoltWinters Forecasts accuracy for',region,'\n'))
  print(accuracy(forecast.region))
}

#Regions with    trend and    seasonality####
#NA(mul),SA(add),World(mul)
#Holt-Winters additive model
for(region in c("SA")){
  ts.Mower_Total_Sales.region=ts(dfIndustry_Tractor_Total_Sales[region], frequency = 12)
  
  hw.Mower_Total_Sales.region<-HoltWinters(ts.Mower_Total_Sales.region)
  plot(hw.Mower_Total_Sales.region, main=paste('Holt-Winters filtering\n Industry Tractor Total Sales',region),xaxt = 'n')
  axis(1, at=1:6, labels = years)
  legend("topleft", c('Observed', 'Holt-Winters additive smoothing'), col = c('Black', ' Red'),lwd = 1, cex=0.8)
  cat('\n',region,'\n')
  print(hw.Mower_Total_Sales.region)
  
  forecast.region=forecast(hw.Mower_Total_Sales.region,12)
  cat(paste('\nHoltWinters Forecasts for',region,'\n'))
  print(forecast.region)
  Tractor_Forecasts[paste('Total_Sales',region)] = forecast.region$mean
  
  plot(forecast.region, main = paste('HoltWinters Forecasts\n Industry Tractor Total Sales',region), xaxt = 'n')
  axis(1, at=1:6, labels = years)
  
  cat(paste('\nHoltWinters Forecasts accuracy for',region,'\n'))
  print(accuracy(forecast.region))
}

#Holt-Winters multiplicative model
for(region in c("NA","World")){
  ts.Mower_Total_Sales.region=ts(dfIndustry_Tractor_Total_Sales[region], frequency = 12)
  
  hw.Mower_Total_Sales.region<-HoltWinters(ts.Mower_Total_Sales.region, seasonal = 'mult')
  plot(hw.Mower_Total_Sales.region, main=paste('Holt-Winters filtering\n Industry Tractor Total Sales',region),xaxt = 'n')
  axis(1, at=1:6, labels = years)
  legend("topleft", c('Observed', 'Holt-Winters multiplicative smoothing'), col = c('Black', ' Red'),lwd = 1, cex=0.8)
  cat('\n',region,'\n')
  print(hw.Mower_Total_Sales.region)
  
  forecast.region=forecast(hw.Mower_Total_Sales.region,12)
  cat(paste('\nHoltWinters Forecasts for',region,'\n'))
  print(forecast.region)
  Tractor_Forecasts[paste('Total_Sales',region)] = forecast.region$mean
  
  plot(forecast.region, main = paste('HoltWinters Forecasts\n Industry Tractor Total Sales',region), xaxt = 'n')
  axis(1, at=1:6, labels = years)
  
  cat(paste('\nHoltWinters Forecasts accuracy for',region,'\n'))
  print(accuracy(forecast.region))
}

##Forecasting Market Share####
for(region in c("NA","SA","Eur","Pacific","China","World")){
  if(region!="China"){
    Mower_Forecasts[paste('Market Share',region)]=Mower_Forecasts[paste('Unit_Sales',region)]/Mower_Forecasts[paste('Total_Sales',region)]
    plot(c(1:12),Mower_Forecasts[,paste('Market Share',region)], type='o',main = paste('HoltWinters mean Forecasts\n Mower Market Share',region), ylab='Market Share',xlab='Month',xaxt = 'n')
    axis(1, at=1:12, labels = month.name)
  }
  Tractor_Forecasts[paste('Market Share',region)]=Tractor_Forecasts[paste('Unit_Sales',region)]/Tractor_Forecasts[paste('Total_Sales',region)]
  plot(c(1:12),Tractor_Forecasts[,paste('Market Share',region)], type='o', main = paste('HoltWinters mean Forecasts\n Tractor Market Share',region), ylab='Market Share',xlab='Month',xaxt = 'n')
  axis(1, at=1:12, labels = month.name)
}

##Production Cost####
dfUnit_Production_Costs <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "Unit Production Costs", skip = 2)

#Identify trend and seasonality
par(mfrow=c(1,1))
for(product in c("Mower","Tractor")){
  ts.product=ts(dfUnit_Production_Costs[product], frequency = 12)
  plot(ts.product, main=paste('Time Series for',product), ylab='Unit Production Costs',xaxt = 'n')
  axis(1, at=1:6, labels = years)
  plot(decompose(ts.product))
}

#Forecast Unit Production Cost
#Trend and no seasonality####
#Double exponential smoothing
for(product in c("Mower","Tractor")){
  ts.Unit_Production_Costs.product=ts(dfUnit_Production_Costs[product], frequency = 12)
  
  hw.Unit_Production_Costs.product<-HoltWinters(ts.Unit_Production_Costs.product, gamma = FALSE)
  plot(hw.Unit_Production_Costs.product, main=paste('Holt-Winters filtering\n',product,'Unit Production Costs'),xaxt = 'n')
  axis(1, at=1:6, labels = years)
  legend("topleft", c('Observed', 'Double exponential smoothing'), col = c('Black', ' Red'),lwd = 1, cex=0.8)
  cat('\n',product,'\n')
  print(hw.Unit_Production_Costs.product)
  
  forecast.product = forecast(hw.Unit_Production_Costs.product,12)
  cat(paste('\nHoltWinters Forecasts for',product,'\n'))
  print(forecast.product)
  if(product=="Mower") Mower_Forecasts$'Unit_Production_Costs'= forecast.product$mean
  else Tractor_Forecasts$'Unit_Production_Costs'= forecast.product$mean
  
  plot(forecast.product, main = paste('HoltWinters Forecasts\n',product, 'Unit Production Costs'), xaxt = 'n')
  axis(1, at=1:6, labels = years)
  
  cat(paste('\nHoltWinters Forecasts accuracy for',product,'\n'))
  print(accuracy(forecast.product))
}

#Calculate forecasted Production cost####
Mower_Forecasts$`Unit_Sales China`=as.numeric(Mower_Forecasts$`Unit_Sales China`)

for(region in c("NA","SA","Eur","Pacific","China","World")){
  Mower_Forecasts[paste('Production Cost',region)]=Mower_Forecasts[paste('Unit_Sales',region)]*Mower_Forecasts['Unit_Production_Costs']
  plot(c(1:12),Mower_Forecasts[,paste('Production Cost',region)], type='o',main = paste('HoltWinters mean Forecasts\n',region, 'Mower Production Cost'), ylab='Production Cost',xlab='Month',xaxt = 'n')
  axis(1, at=1:12, labels = month.name)

  Tractor_Forecasts[paste('Production Cost',region)]=Tractor_Forecasts[paste('Unit_Sales',region)]*Tractor_Forecasts['Unit_Production_Costs']
  plot(c(1:12),Tractor_Forecasts[,paste('Production Cost',region)], type='o', main = paste('HoltWinters mean Forecasts\n',region, 'Tractor Production Cost'), ylab='Production Cost',xlab='Month',xaxt = 'n')
  axis(1, at=1:12, labels = month.name)
}

##Summary of Forecasted Values####
rownames(Mower_Forecasts) = month.name
rownames(Tractor_Forecasts) = month.name

####Task 3####
dfPurchasing_Survey <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "Purchasing Survey", skip = 2)
dfPurchasing_Survey = dfPurchasing_Survey[,1:9]
dfPurchasing_Survey$`Usage Level` = dfPurchasing_Survey$`Usage Level`/10
dfPurchasing_Survey$`Satisfaction Level` = dfPurchasing_Survey$`Satisfaction Level` /7*10

## Dendrograms
purchasing_survey.fit = hclust(dist(dfPurchasing_Survey[1:9], method = 'euclidean'), method = 'ward.D2')
plot(purchasing_survey.fit)

#Cutting Dendogram
rect.hclust(purchasing_survey.fit, k = 4, border = 2:5) 
cluster_cut = cutree(purchasing_survey.fit, k=4)
table(cluster_cut) # number of units in each cluster

# split elements into their various cluster
dfPurchasing_Survey$'Cluster Number' = NA
dfPurchasing_Survey$'Cluster Number' = cluster_cut

cluster1 = subset(dfPurchasing_Survey, dfPurchasing_Survey$`Cluster Number`==1)
cluster2 = subset(dfPurchasing_Survey, dfPurchasing_Survey$`Cluster Number`==2)
cluster3 = subset(dfPurchasing_Survey, dfPurchasing_Survey$`Cluster Number`==3)
cluster4 = subset(dfPurchasing_Survey, dfPurchasing_Survey$`Cluster Number`==4)

# find mean of each cluster
table1 <- t(aggregate(dfPurchasing_Survey[,1:9], list(cluster_cut), mean))
table1 <- table1[-1,]
table1 <- data.frame(table1)
colnames(table1) <- c(1:4)

# plot mean into line chart
chartcol=rainbow(4)
plot(table1$`1`, type = "o", col = chartcol[1], ylim = c(0, 10), main = "Clusters Averages", xlab = 'Attributes', ylab = "Average Rating", xaxt = 'n')
lines(table1$`2`, type = "o", col = chartcol[2])
lines(table1$`3`, type = "o", col = chartcol[3])
lines(table1$`4`, type = "o", col = chartcol[4])
legend("bottomright", legend = c("Group 1", "Group 2", "Group 3", "Group 4"), col = chartcol, lwd =1, cex = 0.75)
axis(1, at = c(1:9), labels = rownames(table1))

summary(cluster1)
summary(cluster2)
summary(cluster3)
summary(cluster4)

##Correlation Matrix
cor_mat = cor(dfPurchasing_Survey[8:9],dfPurchasing_Survey[1:7])
View(cor_mat)

library('corrplot')
corrplot(cor_mat, tl.srt = 45)

####Task 4####
set.seed(12345)
extra_shift = 0
initial = 100
total_extra_shift = c()

for(i in 1:10000){
  #demand = runif(10000, min=80, max=130)
  demand = sample(80:130, 260, replace = TRUE)
  #demand = floor(demand)
  
  for(i in 1:259){
    inventory =initial+100-demand[i]
    if(inventory<=50){
      extra_shift = extra_shift+1
      inventory = inventory+100
    }
    initial=inventory
  }
  total_extra_shift = append(total_extra_shift, extra_shift)
  extra_shift = 0
}
summary(total_extra_shift)
hist(total_extra_shift, main = 'Histogram of Total Extra Shifts')
ecdf(total_extra_shift)(15)