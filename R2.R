
library(readxl)
library(forecast)
library(tidyverse)
library(tseries)
minimumwages<- read.csv("Minimum Wage Data old.csv")
minimumwages <- select(minimumwages,Year,State,State.Minimum.Wage,Federal.Minimum.Wage,Effective.Minimum.Wage,CPI.Average)
ggplot(minimumwages) + geom_line(aes(x=Year,y=Federal.Minimum.Wage,color="red")) +
  geom_line(aes(x=Year,y=log(CPI.Average),color="green")) +
  labs(y="CPI and Federal Minimum Wage")
ggplot(minimumwages)+geom_boxplot(aes(x=State,y=State.Minimum.Wage)) + coord_flip()

wages<-read_excel("emwages.xlsx")
wagestsd<-ts(wages,start = 1968,end = 2015,frequency = 1)
wagestsd
sum(is.na(wagestsd))
summary(wagestsd)
plot(wagestsd)
fmwages<-auto.arima(wagestsd)
fmwages
plot(fmwages$residuals)
predwages<-forecast(fmwages,level=c(95),h=5)
predwages
plot(predwages)
wages<-read_excel("emwages.xlsx")
wagestsd1<-ts(wages,start = 1968,end = 2020,frequency = 1)
wagestsd1
start(wagestsd1)
end(wagestsd1)
sum(is.na(wagestsd1))
summary(wagestsd1)
plot(wagestsd1)
fmwages1<-auto.arima(wagestsd1)
fmwages1
auto.arima(wagestsd1,trace = TRUE)
plot(fmwages1$residuals)
predwages1<-forecast(fmwages1,level=c(95),h=5)
predwages1
plot(predwages1)
summary(predwages1)

chain<-read_excel("Ccpi.xlsx")
tschain<-ts(chain,start = c(2000,1),end = c(2015,12),frequency = 12)
tschain
plot(tschain)
dchain<-decompose(tschain,"multiplicative")
plot(dchain)
#Stationarity
plot(tschain)
fmchain<-auto.arima(tschain)
fmchain
plot.ts(fmchain$residuals)
predchain<-forecast(fmchain,level = c(95),h=5*12)
plot(predchain)
predchain

chain<-read_excel("Ccpi.xlsx")
tschain1<-ts(chain,start = c(2000,1),end = c(2020,12),frequency = 12)
tschain1
dchain1<-decompose(tschain1,"multiplicative")
plot(dchain1)
plot(tschain1)
abline(reg = lm(tschain1~time(tschain1)))
cycle(tschain1)
###Get a boxplot by cycle
boxplot(tschain1~cycle(tschain1),xlab="Month",ylab="Consumer Price Index",
        main="Monthly CPI from 2000 to 2020")
fmchain1<-auto.arima(tschain1)
fmchain1
auto.arima(tschain1,ic="aic",trace=TRUE)
plot.ts(fmchain1$residuals)
predchain1<-forecast(fmchain1,level = c(95),h=5*12)
plot(predchain1)
predchain1
