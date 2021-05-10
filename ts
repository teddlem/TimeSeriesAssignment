#I chose the data series from the IMF's WEO database, choosing "per capita GDP growth, constant international 2017 dollars" and "Net National Savings" for the 
#nation of Indonesia from 1980-2024 (with IMF predictions after 2020). I chose this dataset because of the Solow Growth model, which posits that a developing
#country with high savings rates and population growth will grow economically over time. Indonesia has strong population growth, and its national savings rate 
#has grown over time. So, I thought, let's test the theory, and see if income growth accelerated as the savings rate accelerated.

library(readxl)
library(fpp2)

IMFdatabase <- read_excel("C:/Users/teddl/Downloads/Indonesia.xls")
IMFdatabase <-t(IMFdatabase)
#View(IMFdatabase)
#View(indonesia_ts)
#View(indonesia_short)

#Managing data.frame and time series' column headings
IMFdatabase<-IMFdatabase[2:47,]
colnames(IMFdatabase) = IMFdatabase[1, ]
colnames(IMFdatabase)[1] = "gdp"
colnames(IMFdatabase)[2] = "savings"
print(colnames(IMFdatabase))
IMFdatabase = IMFdatabase[-1, ]
indonesia <-IMFdatabase
indonesia_ts<-ts(IMFdatabase[,1])

    #Creating a time-series to compare to real and predicted data
indonesia_short <- ts(indonesia_ts[1:36])
#str(indonesia_ts)

#plot
plot(indonesia)


#Linear model
indonesia$gdp <-as.numeric(indonesia$gdp)
indonesia$savings <-as.numeric(indonesia$savings)
indonesia <-data.frame(indonesia)
reg1 <- lm(gdp ~ savings, data = indonesia)
summary(reg1)
#Call:
#                                   lm(formula = gdp ~ savings, data = indonesia)
#                                 Residuals:
#                                   Min       1Q   Median       3Q      Max 
#                                -14.6246  -2.8942   0.0101   2.9899  13.7304 
#                                
#                                 Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)    
#                                 (Intercept)  2.43333    1.80318   1.349    0.184    
#                                 savings      0.89420    0.06827  13.098   <2e-16 ***
#                                   ---
#                                   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#                                 
#                                 Residual standard error: 5.948 on 43 degrees of freedom
#                                 Multiple R-squared:  0.7996,	Adjusted R-squared:  0.7949 
#                                 F-statistic: 171.6 on 1 and 43 DF,  p-value: < 2.2e-16
                                 
#Clearly, with an Adj.R^2 of 79%, there's a relationship between savings and GDP growth.

#Naive model
indonesia_short <- as.numeric(indonesia_short)
fc1 <- naive(indonesia_short)
fc1 %>% forecast() %>% autoplot(ylab="GDP per capita")
checkresiduals(fc1)
#Results: Not close to real results, though residuals are normally distributed. One large spike of 
#residuals at observation 19, during the 1998 Asian economic crisis.


#Random Walk Forest with Drift, which is much closer to the real results
fc2<-rwf(indonesia_short, h=10, drift=TRUE)
fc2 %>% forecast() %>% autoplot(ylab="GDP per capita")
checkresiduals(fc2)
#Closer to actual results, and residuals are normally distributed with no significant ACF spikes.
#  One large spike of residuals at observation 19, during the 1998 Asian economic crisis.

#Generic Forecast function/ETS(M,A,N)
fc3<-forecast(indonesia_short, h=10)
fc3 %>% forecast() %>% autoplot(ylab="GDP per capita")
checkresiduals(fc3)
# Similar to RWF, with residuals are normally distributed with no significant ACF spikes, but lower ACFs.
# Again, one large spike of residuals at observation 19, during the 1998 Asian economic crisis.


#ARIMA model
fc4<-auto.arima(indonesia_short, seasonal=FALSE)
fc4 %>% forecast() %>% autoplot(ylab="GDP per capita")
checkresiduals(fc4)
#Same as ETS and RWF, but with tighter ACFs, though only slightly.

#VAR model
IMFdatabase <- data.frame(IMFdatabase)
gdp <-IMFdatabase$gdp
savings <-IMFdatabase$savings
fc<-VAR(cbind(gdp,savings))
summary(fc)

# VAR Estimation Results:
#========================= 
#  Endogenous variables: gdp, savings 
#Deterministic variables: const 
#Sample size: 44 
#Log Likelihood: -284.093 
#Roots of the characteristic polynomial:
#  0.8672 0.2094
#Call:
#  VAR(y = cbind(gdp, savings))#
#
#Estimation results for equation gdp: 
#  ==================================== 
#  gdp = gdp.l1 + savings.l1 + const 
#Estimate Std. Error t value Pr(>|t|)    
#gdp.l1       0.7681     0.1755   4.377  8.1e-05 ***
#  savings.l1   0.1056     0.1766   0.598    0.553    
#const        2.9120     2.1718   1.341    0.187    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 6.84 on 41 degrees of freedom
#Multiple R-Squared: 0.7422,	Adjusted R-squared: 0.7297 
#F-statistic: 59.03 on 2 and 41 DF,  p-value: 8.506e-13 
#Estimation results for equation savings: 
#  ======================================== 
#  savings = gdp.l1 + savings.l1 + const 
#Estimate Std. Error t value Pr(>|t|)  
#gdp.l1       0.5243     0.2083   2.517   0.0158 *
#  savings.l1   0.3085     0.2095   1.472   0.1486  
#const        3.6937     2.5775   1.433   0.1594  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 8.118 on 41 degrees of freedom
#Multiple R-Squared: 0.6428,	Adjusted R-squared: 0.6254 
#F-statistic: 36.89 on 2 and 41 DF,  p-value: 6.839e-10 


#Autoplots of predictions vs. real results from 2016-20 (and 2021-24 predicted results)
autoplot(fc1)+
  autolayer(indonesia_ts)#This model was outside the confidence levels on the second year and underpredicted 
#per capita income growth.
autoplot(fc2)+
  autolayer(indonesia_ts)#This forecast was just outside the confidence intervals on the predicted years (2021-24)
autoplot(fc3)+
  autolayer(indonesia_ts)#This forecast (ETS) was pretty close to dead-on, and it's probably the 
#predictive model used by the IMF to predict 2021-24
autoplot(fc4)+
  autolayer(indonesia_ts) #This one didn't work well, graphically speaking.

