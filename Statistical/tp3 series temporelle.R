
#.............................................................................

#importing the first database
data2<-read.csv("D:/Python-R/databases/daily_milk.csv")

#loading the packages needed 
library(scales)
library(ggplot2)
library(tidyverse)
library(zoo)

#editing the data 
data3<-data2%>%
  mutate(day=as.Date(date_time))%>%
  rename(milk=milk_prod_per_cow_kg)

#creating the time series and ploting it 
daily<-ts(data3$milk)
plot(daily)

#autocorrelation function  
fac=acf(daily)
print(data.frame(fac$lag,fac$acf)[1:10,])

#autocorrelation for gap=3
plot(1:length(daily),daily,type = "l")
points((1:length(daily))-3,daily,type = "l",col="red")

#manually calculating rho 3
n=118
k=3
mu_estim=mean (daily) 
sigma_estim=sd(daily) 
x1=daily[(k+1): 118] 
x2=daily[ 1:(118-k)] 
gamma_k3=mean((x1-mu_estim)*(x2-mu_estim))*((n-k)/(n))
rho_k3=gamma_k3/(sigma_estim^2)
print(rho_k3)

#comparing the two results
result_acf_k3=fac$acf[which(fac$lag==3)]
print(result_acf_k3)

#significance
mu=mu_estim
sigma=sigma_estim
gamma=rep(NA, 1000)
for (i in 1:1000){
  x3=rnorm(n, mu, sigma)
  x4=rnorm(n, mu, sigma)
  gamma [i]=mean( (x3-mu)*(x4-mu))/(sigma^2)
}
quantile(gamma, c(0.025,0.975))

print (1.96/sqrt(n)) 


#partial autocorrelation 
pac=pacf(daily)


print(data.frame(pac$lag,pac$acf)[1:10,])


#importing the second database for testing 
data1<-read.csv("D:/Python-R/Databases/monthly_milk.csv")

#editing
monthly<-data1%>%
  mutate(month=as.Date(month))%>%
  rename(milk=milk_prod_per_cow_kg)

#ploting the time series 
monthly%>%
  ggplot(aes(month,milk))+
  geom_line(aes(month,milk),col="blue")+
  theme_get()+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  labs(x=("Date"),y=("milk prod per cow"))
#converting to a zoo object
ts<-as.zoo(ts(monthly$milk,frequency = 12,start=c(1962,01,01)))
head(ts)

#Ljung-Box Test
Box.test(ts, lag = 1, type = "Ljung")

#Ljung-Box Test (seasonality)
Box.test(ts, lag = 12, type = "Ljung")








