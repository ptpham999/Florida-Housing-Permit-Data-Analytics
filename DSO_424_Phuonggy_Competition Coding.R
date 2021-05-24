library(rvest)
library(dplyr)
library(forecast)
library(ggplot2)

url="https://fred.stlouisfed.org/series/FLBP1FH"
fred = url %>% read_html() %>% 
  html_nodes(xpath = '//*[@id="download-data-csv"]') %>% html_table() %>%
  data.frame()
fred
install.packages("readxl")
library(readxl)
fredata= read_excel("C:\\Users\\ADMIN\\Downloads\\Competition_Final.xlsx")
fredata
fredata.1 = fredata[,-1]
fredata.1
fredata.2 = fredata.1[,-1]
fredata.2
fredata.2 = as.matrix(fredata.2,ncol=12)
fredata.2
fredata.1 = as.numeric(fredata.2)
fredata.2
head(fredata,14)
tail(fredata)
y.ts = ts(fredata.2,start=c(1988,1),frequency = 12)
y.ts
y = y.ts
y
autoplot(y.ts) + geom_point() + theme_bw()

train1 = window(y, start = c(1988,1), end = c(2000,12))
test1 = window(y, start = c(2001,1), end = c(2005,12))

train2 = window(y, start = c(1988,1), end = c(2003,12))
test2 = window(y, start = c(2004,1), end = c(2008,12))

train3 = window(y, start = c(1988,1), end = c(2008,12))
test3 = window(y, start = c(2009,1), end = c(2013,12))

train4 = window(y, start = c(1988,1), end = c(2005,12))
test4 = window(y, start = c(2006,1), end = c(2010,12))

train5 = window(y, start = c(1988,1), end = c(2018,12))
test5 = window(y, start = c(2019,1), end = c(2020,12))

###Seasonal Naive: 
#Scenario1
Naive_1=snaive(train1,h=length(test1),level=95) 

autoplot(Naive_1) + autolayer(Naive_1$fitted,series="Fitted\nvalues")+
  autolayer(test1,series="Testing\nset") + 
  xlab('Year')
accuracy(Naive_1,test1)

tsdisplay(Naive_1$residuals)

#Scenario2
Naive_2=snaive(train2,h=length(test2),level=95) 

autoplot(Naive_2) + autolayer(Naive_2$fitted,series="Fitted\nvalues")+
  autolayer(test2,series="Testing\nset") + 
  xlab('Year')
accuracy(Naive_2,test1)

tsdisplay(Naive_2$residuals)

#Scenario3
Naive_3=snaive(train3,h=length(test3),level=95) 

autoplot(Naive_3) + autolayer(Naive_3$fitted,series="Fitted\nvalues")+
  autolayer(test3,series="Testing\nset") + 
  xlab('Year')
accuracy(Naive_3,test3)

tsdisplay(Naive_3$residuals)

#Scenario4
Naive_4=snaive(train4,h=length(test4),level=95) 

autoplot(Naive_4) + autolayer(Naive_4$fitted,series="Fitted\nvalues")+
  autolayer(test4,series="Testing\nset") + 
  xlab('Year')
accuracy(Naive_4,test4)

tsdisplay(Naive_4$residuals)

#Scenario5
Naive_5=snaive(train5,h=length(test5),level=95) 

autoplot(Naive_5) + autolayer(Naive_5$fitted,series="Fitted\nvalues")+
  autolayer(test5,series="Testing\nset") + 
  xlab('Year')
accuracy(Naive_5,test5)

tsdisplay(Naive_5$residuals)

#Average
mean(c(accuracy(Naive_1,test1)['Test set','MAPE'], accuracy(Naive_2, test2)['Test set','MAPE'], accuracy(Naive_5, test5)['Test set','MAPE'], accuracy(Naive_3, test3)['Test set','MAPE'], accuracy(Naive_4, test4)['Test set','MAPE']))

###Automatic Smoothing
#Scenraio1
?ets
AS_1 = ets(train1,damped=T,lambda="auto")
AS_1F=forecast(AS_1,h=length(test1),level=95)

autoplot(AS_1F) + autolayer(AS_1F$fitted,series="Fitted\nvalues")+
  autolayer(test1,series="Testing\nset") + 
  xlab('Year')

accuracy(AS_1F,test1)

tsdisplay(AS_1F$residuals)

#Scenraio2
AS_2 = ets(train2,damped=T,lambda="auto")
AS_2F=forecast(AS_2,h=length(test2),level=95)

autoplot(AS_2F) + autolayer(AS_2F$fitted,series="Fitted\nvalues")+
  autolayer(test2,series="Testing\nset") + 
  xlab('Year')

accuracy(AS_2F,test2)

tsdisplay(AS_2F$residuals)


#Scenraio3
AS_3 = ets(train3,damped=T,lambda="auto")
AS_3F=forecast(AS_3,h=length(test3),level=95)

autoplot(AS_3F) + autolayer(AS_3F$fitted,series="Fitted\nvalues")+
  autolayer(test3,series="Testing\nset") + 
  xlab('Year')

accuracy(AS_3F,test3)

tsdisplay(AS_3F$residuals)

#Scenraio4
AS_4 = ets(train4,damped=T,lambda="auto")
AS_4F=forecast(AS_4,h=length(test4),level=95)

autoplot(AS_4F) + autolayer(AS_4F$fitted,series="Fitted\nvalues")+
  autolayer(test4,series="Testing\nset") + 
  xlab('Year')

accuracy(AS_4F,test3)

tsdisplay(AS_4F$residuals)

#Scenraio5
AS_5 = ets(train5,damped=T,lambda="auto")
AS_5F=forecast(AS_5,h=length(test5),level=95)

autoplot(AS_5F) + autolayer(AS_5F$fitted,series="Fitted\nvalues")+
  autolayer(test5,series="Testing\nset") + 
  xlab('Year')

accuracy(AS_5F,test5)

tsdisplay(AS_5F$residuals)

###Arima
#Scenario1
Ari_1 = auto.arima(train1,lambda = "auto")
Ari_1F = forecast(Ari_1,h=length(test1),level=95)
autoplot(Ari_1F) + autolayer(Ari_1F$fitted,series="Fitted\nvalues")+
  autolayer(test1,series="Testing\nset") + 
  xlab('Year')

accuracy(Ari_1F,test1)

tsdisplay(Ari_1F$residuals)

#Scenario2
Ari_2 = auto.arima(train2,lambda = "auto")
Ari_2F = forecast(Ari_2,h=length(test1),level=95)
autoplot(Ari_2F) + autolayer(Ari_2F$fitted,series="Fitted\nvalues")+
  autolayer(test2,series="Testing\nset") + 
  xlab('Year')

accuracy(Ari_2F,test2)

tsdisplay(Ari_2F$residuals)

#Scenario3
?auto.arima
Ari_3 = auto.arima(train3,lambda = "auto")
Ari_3F = forecast(Ari_3,h=length(test3),level=95)
autoplot(Ari_3F) + autolayer(Ari_3F$fitted,series="Fitted\nvalues")+
  autolayer(test3,series="Testing\nset") + 
  xlab('Year')

accuracy(Ari_3F,test3)

tsdisplay(Ari_3F$residuals)

#Scenario4
?auto.arima
Ari_4 = auto.arima(train4,lambda = "auto")
Ari_4F = forecast(Ari_4,h=length(test4),level=95)
autoplot(Ari_4F) + autolayer(Ari_4F$fitted,series="Fitted\nvalues")+
  autolayer(test4,series="Testing\nset") + 
  xlab('Year')

accuracy(Ari_4F,test4)

tsdisplay(Ari_4F$residuals)

#Scenario5
Ari_5 = auto.arima(train5,lambda = "auto")
Ari_5F = forecast(Ari_5,h=length(test5),level=95)
autoplot(Ari_5F) + autolayer(Ari_5F$fitted,series="Fitted\nvalues")+
  autolayer(test5,series="Testing\nset") + 
  xlab('Year')

accuracy(Ari_5F,test5)

tsdisplay(Ari_5F$residuals)

###Neural networks
?nnetar
#Scenario1
M6_1 = nnetar(train1, lambda = 'auto', P=2, p=5)
M6_1F = forecast(M6_1, h = length(test1), level=95)

autoplot(M6_1F) + autolayer(M6_1F$fitted,series="Fitted\nvalues")+
  autolayer(test1,series="Testing\nset") + 
  xlab('Year')

accuracy(M6_1F,test1)

tsdisplay(M6_1F$residuals)


#Scenario2
M6_2 = nnetar(train2, lambda = 'auto', P=2, p=5)
M6_2F = forecast(M6_2, h = length(test2), level=95)

autoplot(M6_2F) + autolayer(M6_2F$fitted,series="Fitted\nvalues")+
  autolayer(test2,series="Testing\nset") + 
  xlab('Year')

accuracy(M6_2F,test2)

tsdisplay(M6_2F$residuals)

#Scenario3
M6_3 = nnetar(train3, lambda = 'auto', P=2, p=5)
M6_3F = forecast(M6_3, h = length(test3), level=95)

autoplot(M6_3F) + autolayer(M6_3F$fitted,series="Fitted\nvalues")+
  autolayer(test3,series="Testing\nset") + 
  xlab('Year')

accuracy(M6_3F,test3)

tsdisplay(M6_3F$residuals)

#Scenario4
M6_4 = nnetar(train4, lambda = 'auto', P=2, p=5)
M6_4F = forecast(M6_4, h = length(test4), level=95)

autoplot(M6_4F) + autolayer(M6_4F$fitted,series="Fitted\nvalues")+
  autolayer(test4,series="Testing\nset") + 
  xlab('Year')

accuracy(M6_4F,test4)

tsdisplay(M6_4F$residuals)

#Scenario5
M6_5 = nnetar(train5, lambda = 'auto', P=2, p=5)
M6_5F = forecast(M6_5, h = length(test5), level=95)

autoplot(M6_5F) + autolayer(M6_5F$fitted,series="Fitted\nvalues")+
  autolayer(test5,series="Testing\nset") + 
  xlab('Year')

accuracy(M6_5F,test5)

tsdisplay(M6_5F$residuals)






