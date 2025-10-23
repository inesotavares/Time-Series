setwd("/Users/inestavares/Desktop/  /st/project")
library(MASS)
library(ldsr)
library(fpp2)
library(ggfortify)
library(forecast)
library(tseries)
library(ggplot2)
library(lubridate)
library(astsa)
library(zoo)
library(dplyr)
library(wesanderson)
library(xtable)
library(data.table)
library(tableHTML)

data<-read.csv('fdny.csv')
attach(data)

db<-subset(data, INCIDENTBOROUGH=='Citywide' & 
             INCIDENTCLASSIFICATION=='All Fire/Emergency Incidents')
detach(data)
attach(db)
db<-db[-c(97:104),-c(2,3,5)]

class(YEARMONTH)
#install.packages("zoo")


#########1.Análise Exploratoria######
db$YEARMONTH <- as.Date(paste0(db$YEARMONTH, "/01"), format = "%Y/%m/%d")

class(db$YEARMONTH)

#Convert the data to time series
ts_data_original <- ts(data = db$INCIDENTCOUNT, 
              start = c(year(min(db$YEARMONTH)), 
                        as.numeric(format(min(db$YEARMONTH), "%m"))),
              end = c(year(max(db$YEARMONTH)), 
                      as.numeric(format(max(db$YEARMONTH), "%m"))),
              frequency = 12)


# Tendência
plot(x=ts_data_original,
     xlab = "Time (Month)", 
     ylab = "Occurrences",
     main = "Occurrences Over Time",
     col=wes_palette("GrandBudapest1")[2])
##Conclusão da análise do gráfico:
# 1.Podemos observar que o valor médio não é constante.
# 2.Também observamos que a variância não é constante.
# 3.There are no gaps or discontinuities or wrong values



ggplot(db, aes(as.factor(year(YEARMONTH)),INCIDENTCOUNT)) +
  geom_boxplot(fill=wes_palette("GrandBudapest1")[1]) +
  labs(x = "Year", y = "Occurrences")+
  theme(panel.background = element_blank())



lag1.plot(ts_data_original,12,
          main = "Occurrences Over Time",
          col=wes_palette("GrandBudapest1")[2],
          lwc = wes_palette("GrandBudapest1")[3])


# Sazonalidade
ggmonthplot(ts_data_original)+
  geom_line(color = wes_palette("GrandBudapest1")[4])+
  ylab("Occurrences") +
  xlab("Month")+
  ggtitle("Seasonal Plot")

#The blue lines represent the average of the corresponding month. But can´t be 
#consider because this average has no meaning since the data presents trend.

ggseasonplot(ts_data_original, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Total Number of Emergencies") +
  ggtitle("Seasonal plot: Emergencies")


# Detrending
#par(mfrow=c(3,1))
#ts.plot(ts_data_original)
#ts.plot(diff(ts_data_original))
#acf(diff(ts_data_original),96, main='ACF of thhe 1st difference')
# The acf presentes a cycle of 12 months
#ggmonthplot(diff(ts_data_original))+
#  geom_line(color = wes_palette("GrandBudapest1")[4])+
#  xlab("Month")+
#  ggtitle("")
#a sazonalidade esta mais evendiciada 



# Estabelizar a Variância
#Box Cox
(lambda <-BoxCox.lambda(ts_data_original)) #1.999924
bc<-BoxCox(ts_data_original,lambda)

#Day of the Week
parsed_date <- ym(YEARMONTH[-c(73:104)])
m <- month(parsed_date)
d <- numeric(length(m))
for (i in 1:length(m)) {
  if (m[i] %in% c(1, 3, 5, 7, 8, 10, 12)) {
    d[i] <- 0
  } else if (m[i] %in% c(4, 6, 9, 11)) {
    d[i] <- 1
  } else {
    d[i] <- 2
  }}

d[38]=1
wt <- numeric(length(d))
for (a in 1:length(d)) {
  wt[a] = ifelse(d[a]==0,ts_data_original[a]*365.25/12,ts_data_original[a]*365.25/12/d[a])
}

par(mfrow=c(3,1))
plot(x=ts_data_original,
     xlab = "Time (Month)", 
     ylab = "Occurrences",
     main = "Occurrences Over Time",
     col=wes_palette("GrandBudapest1")[3])
plot(x=bc,
     xlab = "Time (Month)", 
     ylab = "Box Cox Occurrences",
     main = "Box Cox Transformation",
     col=wes_palette("GrandBudapest1")[2])
plot(x=wt,type= 'l',
     xlab = "Time (Month)", 
     ylab = "Cases",
     main = "Length of the month Adjustment",
     col=wes_palette("GrandBudapest1")[4])

#####2.Decomposição####
#primeiro a sazonalidade depois a tendencia


d12<-diff(bc,12)
par(mfrow=c(3,1))
ts.plot(bc, ylab="Box Cox Occurrences", main="Box-Cox Series",
       col=wes_palette("GrandBudapest1")[3])
ts.plot(d12, main="Deseasoned",
        col=wes_palette("GrandBudapest1")[1])
monthplot(d12, 
          main=expression(paste("Season Plot of ", Delta[12], "BC series")),
          col="#FD6467")
wes_palette("GrandBudapest1")[2]

#Remover tendencia
d12_1 = diff(d12,1)
par(mfrow = c(2,1))
plot(d12, main="Deseasoned", col=wes_palette("GrandBudapest1")[2])
plot(d12_1, main="Deseasoned and Detrended",
     type='l',col=wes_palette("GrandBudapest1")[4])
lag1.plot(d12_1,12,
          main = expression(paste(Delta, Delta[12], "bc")),
          col=wes_palette("GrandBudapest1")[2],
          lwc = wes_palette("GrandBudapest1")[3])

#Test for unit root
kpss.test(d12_1)

#KPSS Level = 0.097035, Truncation lag parameter = 3, p-value =0.1
# Its stationary

adf.test(d12_1)
#Dickey-Fuller = -4.4992, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary
# Its stationary

d1<-diff(bc,1)
d12 <- diff(bc,12)
#####modelling
maxlag <- 72
par(mfrow=c(3,4), mar=c(3,3,4,2))

plot(bc, main = expression("bc"))
plot(d1, main = expression(paste(Delta, "bc")))
plot(d12, main = expression(paste(Delta[12], "bc")))
plot(d12_1, main = expression(paste(Delta, Delta[12], "bc")))

Acf(bc, type='correlation', lag=maxlag, ylab="", main=expression(paste("ACF for bc")))
Acf(d1, type='correlation', lag=maxlag, na.action=na.omit, ylab="", main=expression(paste("ACF for ", Delta,"bc")))
Acf(d12, type='correlation', lag=maxlag, na.action=na.omit, ylab="", main=expression(paste("ACF for ", Delta[12], "bc")))
Acf(d12_1, type='correlation', lag=maxlag, na.action=na.omit, ylab="", main=expression(paste("ACF for ", Delta, Delta[12], "bc")))

Acf(bc, type='partial', lag=maxlag, na.action=na.omit, ylab="", main=expression(paste("PACF for bc")))
Acf(d1, type='partial', lag=maxlag, na.action=na.omit, ylab="", main=expression(paste("PACF for ", Delta, "bc")))
Acf(d12, type='partial', lag=maxlag, na.action=na.omit, ylab="", main=expression(paste("PACF for ", Delta[12], "bc")))
Acf(d12_1, type='partial', lag=maxlag, na.action=na.omit, ylab="", main=expression(paste("PACF for ", Delta,Delta[12], "bc")))




####### Teste e Treino #######
lstM <- 2016+5/12
bc_train = window(bc, end=lstM)
bc_test = window(bc, start=lstM+1/12)


# looks like an ar seasonal
# d=1 and D=1 
# M1: M1=(0,1,0)(1,1,0) 12----------x
fit1=sarima(bc_train,0,1,0,1,1,0,12)
#residuals show very small correlation
# AICc=40.27359
# parameters are statisticaly sigificant

#### M2=(0,1,0)(1,1,1) 12-----------x

fit2=sarima(bc_train,0,1,0,1,1,1,12)
#residuals show very small correlation
# AICc=40.22432
# parameters are not statisticaly sigificant

### M3=(0,1,0)(2,1,0) 12------------maybe
fit3=sarima(bc_train,0,1,0,2,1,0,12)
#residuals show very small correlation
# AICc=40.24844
# parameters are statisticaly sigificant

### P=2, D=1, Q=0, d=1
par(mfrow=c(2,1))
acf(fit3$fit$residuals,84, main='ACF Residuals of the Model (0,1,0)(2,1,0)12',
    col=wes_palette(name = 'GrandBudapest1')[2])
pacf(fit3$fit$residuals,84, main='PACF Residuals of the Model (0,1,0)(2,1,0)12',
     col=wes_palette(name = 'GrandBudapest1')[2])
#looks like a ma

### M4=(1,1,0)(2,1,0) 12-------------maybe
fit4=sarima(bc_train,1,1,0,2,1,0,12)
#residuals don't show correlation
# AICc=40.16883
# parameters are statisticaly sigificant

### M5=(0,1,1)(2,1,0) 12---------maybe
fit5=sarima(bc_train,0,1,1,2,1,0,12)
#residuals don't show  correlation
# AICc=40.13796
# parameters are statisticaly sigificant

### M6=(1,1,1)(2,1,0) 12---------x
fit6=sarima(bc_train,1,1,1,2,1,0,12)
#residuals don't show correlation
# AICc=40.14947
# parameters are not statisticaly sigificant

### M7=(0,1,2)(2,1,0) 12--------------x
fit7=sarima(bc_train,0,1,2,2,1,0,12)
#residuals don't show correlationn
# AICc=40.15972
# parameters are not statisticaly sigificant

### M7=(0,1,2)(2,1,0) 12--------------x
fit7=sarima(bc_train,0,1,2,2,1,0,12)
#residuals don't show correlationn
# AICc=40.15972
# parameters are not statisticaly sigificant


### Best Model M5


## Analysis of residuals
fit.residuals<-fit5$fit$residuals
#1.Independence<- respondido na m5
# par(mfrow=c(2,1))
# acf(fit.residuals,84,
#     main='ACF Residuals of the Model (0,1,1)(2,1,0)12',
#     col=wes_palette(name = 'GrandBudapest1')[2])
# pacf(fit.residuals,84,
#      main='ACF Residuals of the Model (0,1,1)(2,1,0)12',
#      col=wes_palette(name = 'GrandBudapest1')[2])
# # true


#3. Average=0
t.test(fit.residuals)
#p-value = 0.6486
#TRUE

#4.Homeocedastecidade
plot(fit.residuals,fitted.values(fit5),
     main='Residuals vs Fitted Values',
     col=wes_palette(name='Cavalcanti1')[3],type='p')
#TRUE


##### prediction ####
model = arima(inv_boxcox(bc_train,lambda), order=c(0,1,1), seasonal=list(order=c(2,1,0), period=12))

test<-inv_boxcox(bc_test,lambda)
comparison<-model %>% forecast(h=12,level = 95)

ggplot(data = comparison) +
  geom_line(data=test,aes(x=Index,y=Data, color = "test")) +
  geom_line(aes(x= Index, y = Fitted, color = "fitted")) +
  geom_line(aes(x= Index, y = `Point Forecast`, color = "point forecast")) +
  geom_ribbon(aes(x= Index, ymin = `Lo 95`, ymax = `Hi 95`,  fill = "95"),  
              alpha = 0.2) +
  scale_fill_manual("Level of confidence", 
                    values = wes_palette(name = "Moonrise3")[2])+
  scale_color_manual("legend", 
                     values = c(wes_palette(name = "Moonrise3")[3],
                                wes_palette(name = "GrandBudapest1")[2:3]))+
  theme_minimal()


atualizado<-read.csv('FDNY_atualizado.csv')




#out-of-sample
model2<-arima(ts_data_original, order=c(0,1,1), seasonal=list(order=c(2,1,0), 
                                                              period=12))
forecast_24<-model2 %>%forecast(h=24,level = 95)
ggplot(data = forecast_24) +
  geom_line(aes(x= Index, y = Data, color = "raw")) +
  geom_line(aes(x= Index, y = Fitted, color = "fitted")) +
  geom_line(aes(x= Index, y = `Point Forecast`, color = "point forecast")) +
  geom_ribbon(aes(x= Index, ymin = `Lo 95`, ymax = `Hi 95`,  fill = "95"),  alpha = 0.2) +
  scale_fill_manual("Level of confidence", 
                    values = wes_palette(name = "Moonrise3")[2])+
  scale_color_manual("legend", 
                     values = c("#9C964A",
                                wes_palette(name = "GrandBudapest1")[c(2,4)]))+
  theme_minimal()
### exponential smooth
# ?ses()
# exp_sm<-ses(ts_data_original,24,level=95)
# round(accuracy(exp_sm),2)
# 
# ggplot(data = exp_sm) +
#   geom_line(aes(x= Index, y = Data, color = "raw")) +
#   geom_line(aes(x= Index, y = Fitted, color = "fitted")) +
#   geom_line(aes(x= Index, y = `Point Forecast`, color = "point forecast")) +
#   geom_ribbon(aes(x= Index, ymin = `Lo 95`, ymax = `Hi 95`,  fill = "95"),  alpha = 0.2) +
#   scale_fill_manual("Level of confidence", values = wes_palette(name = "Moonrise3")[2])+
#   scale_color_manual("Legend", values = c("#9C964A",
#                      wes_palette(name = "GrandBudapest1")[c(2,4)])) +
#   theme_minimal()

# #### bootstrap
# 
# prediction <- predict(model, n.ahead = 12)
# pred=prediction$pred
# 
# n=100
# m= n*length(pred)
# 
# boot=NULL
# boot$point =matrix(0,nrow= n, ncol= length(pred), byrow=T)
# boot$point[,1] = rep(pred[1],n) + sample(model$residuals,n,replace=TRUE)
# train<-inv_boxcox(bc_train,lambda)
# trainb=train
# for (i in 1:n)
# {
#   for (j in 2:(length(pred)))
#   {
#     trainb=c(trainb,boot$point[i,(j-1)])
#     boot$point[i,j] = sarima.for(trainb,1,1,1,0,2,1,0,12,plot=F)$pred[1]+ 
#       sample(model$residuals,1,replace=TRUE)
#   }
#   trainb=train
# }
# 
# boot$ps= apply(boot$point,2,quantile,probs=c(0.025,0.975))
# forecastsGvsB.table <- data.frame(
#   Month = as.yearmon(time(test)),
#   Observed = test,
#   Hi.B = round(boot$ps[2,],2),
#   Hi.G = round((pred+2*prediction$se), 2),
#   Lo.B = round(boot$ps[1,],2),
#   Lo.G = round((pred-2*prediction$se), 2),
#   CoveredBoot = between(test, boot$ps[1,],boot$ps[2,]),
#   CoveredGauss = between(test, (pred-1.96*prediction$se),(pred+1.96*prediction$se))
# )
# latex_table <- xtable(forecastsGvsB.table, caption = "Forecasts")
# htlmtable <- tableHTML(forecastsGvsB.table, caption = "Forecasts")
# htlmtable
# 
# plot(pred,col='#5B1A18',type="b",xlim=c(min(time(pred)), max(time(pred))), 
#      ylim=c(min(pred-2*prediction$se,boot$ps[1,]),
#             max(pred+2*prediction$se,boot$ps[2,])))
# points(time(pred),boot$ps[2,],type="b", col="#F1BB7B" )
# points(time(pred),boot$ps[1,],type="b", col="#F1BB7B" )
# points(time(pred),(pred-2*prediction$se),type="b", col="#FD6467" )
# points(time(pred),(pred+2*prediction$se),type="b", col="#FD6467")
# legend('bottomleft',c("Observed", "Bootstrap CI", "Gaussian CI"), 
#        col= c('#5B1A18','#F1BB7B',"#FD6467" ),cex = 0.7,lty=1)



#Bagging

etsfc <- train %>% ets() %>% forecast(h=12)
baggedfc <- train %>% baggedETS() %>% forecast(h=12)

ggplot(data = ts_data_original) +
  geom_line(aes(x=Index,y=Data, color = "raw")) +
  geom_line(data=baggedfc,aes(x= Index, y = `Point Forecast`, color = "bagging forecast")) +
  geom_line(data=etsfc ,aes(x= Index, y = `Point Forecast`, color = "ets forecast")) +
  scale_color_manual("legend", 
                     values = c(wes_palette(name = "GrandBudapest1")[2:3],
                                wes_palette(name = "Moonrise3")[3]))+
  theme_minimal()




etsfc <- ts_data_original %>% ets() %>% forecast(h=24)
baggedfc <- ts_data_original %>% baggedETS() %>% forecast(h=24)

ggplot(data = ts_data_original) +
  geom_line(aes(x=Index,y=Data, color = "raw")) +
  geom_line(data=baggedfc,aes(x= Index, y = `Point Forecast`, color = "bagging forecast")) +
  geom_line(data=etsfc ,aes(x= Index, y = `Point Forecast`, color = "ets forecast")) +
  scale_color_manual("legend", 
                     values = c(wes_palette(name = "GrandBudapest1")[2:3],
                                wes_palette(name = "Moonrise3")[3]))+
  theme_minimal()


#Optimal Values Found
(fit.ets <- ets(ts_data_original))
  