library(quantmod)
library(forecast)

##Getting Indexes from around the world from yahoo finance 

#USA
getSymbols('^GSPC', from = "2006-11-21", to = "2016-11-20")
GSPC_Adjusted = GSPC$GSPC.Adjusted
plot(GSPC_Adjusted, main="GSPC")
GSPC_Returns=dailyReturn(GSPC_Adjusted)
plot(GSPC_Returns, main="GSPC")
nrow(GSPC_Returns)
acf(GSPC_Returns) 
adf.test(GOOG.Ret) # This says that google returns are stationary

#CANADA
getSymbols('^GSPTSE', from = "2006-11-21", to = "2016-11-20")
GSPTSE_Adjusted = GSPTSE$GSPTSE.Adjusted
plot(GSPTSE_Adjusted, main="GSPTSE")
GSPTSE_Returns=dailyReturn(GSPTSE_Adjusted)
plot(GSPTSE_Returns, main="GSPTSE")
nrow(GSPTSE_Returns)

#SOUTH AMERICA
getSymbols('^BVSP', from = "2006-11-21", to = "2016-11-20")
BVSP_Adjusted = BVSP$BVSP.Adjusted
plot(BVSP_Adjusted, main="BVSP")
BVSP_Returns=dailyReturn(BVSP_Adjusted)
plot(BVSP_Returns, main="BVSP")
nrow(BVSP_Returns)

#EUROPE
getSymbols('^FTSE', from = "2006-11-21", to = "2016-11-20")
FTSE_Adjusted = FTSE$FTSE.Adjusted
plot(FTSE_Adjusted, main="FTSE")
FTSE_Returns=dailyReturn(FTSE_Adjusted)
plot(FTSE_Returns, main="FTSE")
nrow(FTSE_Returns)

#ISRAEL 
getSymbols('^TA100', from = "2006-11-21", to = "2016-11-20")
TA100_Adjusted = TA100$TA100.Adjusted
plot(TA100_Adjusted, main="TA100")
TA100_Returns=dailyReturn(TA100_Adjusted)
plot(TA100_Returns, main="TA100")
nrow(TA100_Returns)

#JAPAN
getSymbols('^N225', from = "2006-11-21", to = "2016-11-20")
N225_Adjusted = N225$N225.Adjusted
plot(N225_Adjusted, main="N225")
N225_Returns=dailyReturn(N225_Adjusted)
plot(N225_Returns, main="N225")
nrow(N225_Returns)

#INDIA
getSymbols('^BSESN', from = "2006-11-21", to = "2016-11-20")
BSESN_Adjusted = BSESN$BSESN.Adjusted
plot(BSESN_Adjusted, main="BSESN")
BSESN_Returns=dailyReturn(BSESN_Adjusted)
plot(BSESN_Returns, main="BSESN")
nrow(BSESN_Returns)

#AUSTRALIA 
getSymbols('^AXJO', from = "2006-11-21", to = "2016-11-20")
AXJO_Adjusted = AXJO$AXJO.Adjusted
plot(AXJO_Adjusted, main="AXJnO")
AXJO_Returns=dailyReturn(AXJO_Adjusted)
plot(AXJO_Returns, main="AXJO")
nrow(AXJO_Returns)

#Writing data to match with Temperature data 

write.csv(data.frame(GSPC_Returns),file="GSPC_Returns.csv")
write.csv(data.frame(GSPTSE_Returns),file="GSPTSE_Returns.csv")
write.csv(data.frame(BVSP_Returns),file="BVSP_Returns.csv")
write.csv(data.frame(FTSE_Returns),file="FTSE_Returns.csv")
write.csv(data.frame(TA100_Returns),file="TA100_Returns.csv")
write.csv(data.frame(N225_Returns),file="N225_Returns.csv")
write.csv(data.frame(BSESN_Returns),file="BSESN_Returns.csv")
write.csv(data.frame(AXJO_Returns),file="AXJO_Returns.csv")



#Reading Temperatures

usa=read.csv("1.USATemp.csv")
Can=read.csv("2.CanadaTemp.csv")
sa=read.csv("3.SouthAmericaTemp.csv")
Lon=read.csv("4.LondonTemp.csv")
Isr=read.csv("6.IsraelTemp.csv")
Jap=read.csv("7.JapanTemp.csv")
Ind=read.csv("5.IndiaTemp.csv")
Aus=read.csv("8.AustraliaTemp.csv")

#Getting matched data [returns +Temperatures](matched data)

TeRe_Usa=read.csv("1.GSPC_Returns+Usa.csv")
TeRe_Can=read.csv("2.GSPTSE_Returns+Can.csv")
TeRe_Sa=read.csv("3.BVSP_Returns+Sa.csv")
TeRe_Lon=read.csv("4.FTSE_Returns+Lon.csv")
TeRe_Isr=read.csv("6.TA100_Returns+Isr.csv")
TeRe_Jap=read.csv("7.N225_Returns+Jap.csv")
TeRe_Ind=read.csv("5.BSESN_Returns+Ind.csv")
TeRe_Aus=read.csv("8.AXJO_Returns+Aus.csv")

#Splitting data for appying AR model

USreturns <- TeRe_Usa[,3]
UStemps <- TeRe_Usa[,2]
Canadareturns <- TeRe_Can[,3]
Canadatemps <- TeRe_Can[,2]
SouthAmericareturns <- TeRe_Sa[,3]
SouthAmericatemps <- TeRe_Sa[,2]
Londonreturns <- TeRe_Lon[,3]
Londontemps <- TeRe_Lon[,2]
Israelreturns <- TeRe_Isr[,3]
Israeltemps <- TeRe_Isr[,2]
Japanreturns <- TeRe_Jap[,3]
Japantemps <- TeRe_Jap[,2]
Indiareturns <- TeRe_Ind[,3]
Indiatemps <- TeRe_Ind[,2]
Australiareturns <- TeRe_Aus[,3]
Australiatemps <- TeRe_Aus[,2]

#Ploting returns

plot(Indiareturns,type="l")
plot(USreturns,type="l")
plot(Canadareturns,type="l")
plot(SouthAmericareturns,type="l")
plot(Londonreturns,type="l")
plot(Israelreturns,type="l")
plot(Japanreturns,type="l")
plot(Australiareturns,type="l")

#Ploting temps

plot(Indiatemps)
plot(UStemps)
plot(Canadatemps)
plot(SouthAmericatemps)
plot(Londontemps)
plot(Israeltemps)
plot(Japantemps)
plot(Australiatemps)

#Tests

acf(Indiareturns)
Box.test(Indiareturns, type = "Ljung-Box", lag = 10)
acf(Australiareturns)
Box.test(Australiareturns, type = "Ljung-Box", lag = 10)
acf(USreturns)
Box.test(USreturns, type = "Ljung-Box", lag = 10)
acf(Israelreturns)
Box.test(Israelreturns, type = "Ljung-Box", lag = 10)
acf(SouthAmericareturns)
Box.test(SouthAmericareturns, type = "Ljung-Box", lag = 10)
acf(Japanreturns)
Box.test(Japanreturns, type = "Ljung-Box", lag = 10)
acf(Canadareturns)
Box.test(Canadareturns, type = "Ljung-Box", lag = 10)
acf(Londonreturns)
Box.test(Londonreturns, type = "Ljung-Box", lag = 10)

##AR Models

#AR_US
n1=length(UStemps)
resultsdiff = matrix(0,nrow=20,ncol=3)
for (i in 1:20)
{
  fit = arima(USreturns,order=c(i,0,0),xreg =UStemps)
  resultsdiff[i,2] = fit$aic
  resultsdiff[i,3] = resultsdiff[i,2] + (log(n1)-2)*i
  resultsdiff[i,1]=i
}

plot(resultsdiff[,1],resultsdiff[,2],xlab="p",ylab="criterion", cex.lab=1.35,cex.axis=1.35, main="AR fits for US returns and temperatures", cex.main=1.35,cex=2,pch="*",ylim=c(-14650,-14400))
points(resultsdiff[,1],resultsdiff[,3],pch="o",cex=2)
legend(12,-14575,c("AIC","BIC"),pch=c("*","o"),cex=1,box.lty=0)


#AR_Canada
n1=length(Canadatemps)
resultsdiff = matrix(0,nrow=20,ncol=3)
for (i in 1:20)
{
  fit = arima(Canadareturns,order=c(i,0,0),xreg =Canadatemps)
  resultsdiff[i,2] = fit$aic
  resultsdiff[i,3] = resultsdiff[i,2] + (log(n1)-2)*i
  resultsdiff[i,1]=i

  
}
fit
plot(resultsdiff[,1],resultsdiff[,2],xlab="p",ylab="criterion", cex.lab=1.35,cex.axis=1.35, main="AR fits for Canada returns and temperatures", cex.main=1.35,cex=2,pch="*",ylim=c(-15250,-15350))
points(resultsdiff[,1],resultsdiff[,3],pch="o",cex=2)
legend(12,-15300,c("AIC","BIC"),pch=c("*","o"),cex=1,box.lty=0)

#AR_SouthAmerica
n1=length(SouthAmericatemps)
resultsdiff = matrix(0,nrow=20,ncol=3)
for (i in 1:20)
{
  fit = arima(SouthAmericareturns,order=c(i,0,0),xreg =SouthAmericatemps)
  resultsdiff[i,2] = fit$aic
  resultsdiff[i,3] = resultsdiff[i,2] + (log(n1)-2)*i
  resultsdiff[i,1]=i
}
fit
plot(resultsdiff[,1],resultsdiff[,2],xlab="p",ylab="criterion", cex.lab=1.35,cex.axis=1.35, main="AR fits for southAmerica returns and temperatures", cex.main=1.35,cex=2,pch="*",ylim=c(-12700,-12850))
points(resultsdiff[,1],resultsdiff[,3],pch="o",cex=2)
legend(2,-12750,c("AIC","BIC"),pch=c("*","o"),cex=1,box.lty=0)

#AR_London
n1=length(Londontemps)
resultsdiff = matrix(0,nrow=20,ncol=3)
for (i in 1:20)
{
  fit = arima(Londonreturns,order=c(i,0,0),xreg =Londontemps)
  resultsdiff[i,2] = fit$aic
  resultsdiff[i,3] = resultsdiff[i,2] + (log(n1)-2)*i
  resultsdiff[i,1]=i
}
fit
plot(resultsdiff[,1],resultsdiff[,2],xlab="p",ylab="criterion", cex.lab=1.35,cex.axis=1.35, main="AR fits for London returns and temperatures", cex.main=1.35,cex=2,pch="*",ylim=c(-14800,-14950))
points(resultsdiff[,1],resultsdiff[,3],pch="o",cex=2)
legend(2,-14855,c("AIC","BIC"),pch=c("*","o"),cex=1,box.lty=0)

#AR_Israel
n1=length(Israeltemps)
resultsdiff = matrix(0,nrow=20,ncol=3)
for (i in 1:20)
{
  fit = arima(Israelreturns,order=c(i,0,0),xreg =Israeltemps)
  resultsdiff[i,2] = fit$aic
  resultsdiff[i,3] = resultsdiff[i,2] + (log(n1)-2)*i
  resultsdiff[i,1]=i
}
fit
plot(resultsdiff[,1],resultsdiff[,2],xlab="p",ylab="criterion", cex.lab=1.35,cex.axis=1.35, main="AR fits for Israel returns and temperatures", cex.main=1.35,cex=2,pch="*",ylim=c(-13050,-13200))
points(resultsdiff[,1],resultsdiff[,3],pch="o",cex=2)
legend(2,-13100,c("AIC","BIC"),pch=c("*","o"),cex=1,box.lty=0)

#AR_Japan
n1=length(Japantemps)
resultsdiff = matrix(0,nrow=20,ncol=3)
for (i in 1:20)
{
  fit = arima(Japanreturns,order=c(i,0,0),xreg =Japantemps)
  resultsdiff[i,2] = fit$aic
  resultsdiff[i,3] = resultsdiff[i,2] + (log(n1)-2)*i
  resultsdiff[i,1]=i
}
fit
plot(resultsdiff[,1],resultsdiff[,2],xlab="p",ylab="criterion", cex.lab=1.35,cex.axis=1.35, main="AR fits for Japan returns and temperatures", cex.main=1.35,cex=2,pch="*",ylim=c(-13050,-13200))
points(resultsdiff[,1],resultsdiff[,3],pch="o",cex=2)
legend(2,-13100,c("AIC","BIC"),pch=c("*","o"),cex=1,box.lty=0)

#AR_India
n2=length(Indiatemps)
bresultsdiff = matrix(0,nrow=20,ncol=3)
for (i in 1:20)
{
  fit = arima(Indiareturns,order=c(i,0,0),xreg = Indiatemps)
  resultsdiff[i,2] = fit$aic
  resultsdiff[i,3] = resultsdiff[i,2] + (log(n2)-2)*i
  resultsdiff[i,1]=i
}

fit
plot(resultsdiff[,1],resultsdiff[,2],xlab="p",ylab="criterion", cex.lab=1.35,cex.axis=1.35, main="AR fits for India returns and temperatures", cex.main=1.35,cex=2,pch="*",ylim=c(-12750,-12600))
points(resultsdiff[,1],resultsdiff[,3],pch="o",cex=2)
legend(1,-12600,c("AIC","BIC"),pch=c("*","o"),cex=1,box.lty=0)


#AR_Australia
n1=length(Australiatemps)
resultsdiff = matrix(0,nrow=20,ncol=3)
for (i in 1:20)
{
  fit = arima(Australiareturns,order=c(i,0,0),xreg =Australiatemps)
  resultsdiff[i,2] = fit$aic
  resultsdiff[i,3] = resultsdiff[i,2] + (log(n1)-2)*i
  resultsdiff[i,1]=i
}
fit
plot(resultsdiff[,1],resultsdiff[,2],xlab="p",ylab="criterion", cex.lab=1.35,cex.axis=1.35, main="AR fits for Australia returns and temperatures", cex.main=1.35,cex=2,pch="*",ylim=c(-15250,-15400))
points(resultsdiff[,1],resultsdiff[,3],pch="o",cex=2)
legend(2,-15300,c("AIC","BIC"),pch=c("*","o"),cex=1,box.lty=0)


##ARIMA



#USA
US = auto.arima(USreturns,xreg = UStemps, ic="aic")
acf(US$residuals)
Box.test(US$residuals)
US

#Canada
Canada = auto.arima(Canadareturns,xreg = Canadatemps, ic="aic")
acf(Canada$residuals)
Box.test(Canada$residuals)
Canada

#SouthAmerica
SouthAmerica = auto.arima(SouthAmericareturns,xreg = SouthAmericatemps, ic="aic")
acf(SouthAmerica$residuals)
Box.test(SouthAmerica$residuals)
SouthAmerica

#London
London = auto.arima(Londonreturns,xreg = Londontemps, ic="aic")
acf(London$residuals)
Box.test(London$residuals)
London

#Israel
Israel = auto.arima(Israelreturns,xreg = Israeltemps, ic="aic")
acf(Israel$residuals)
Box.test(Israel$residuals)
Israel

#Japan
Japan = auto.arima(Japanreturns,xreg = Japantemps, ic="aic")
acf(Japan$residuals)
Box.test(Japan$residuals)
Japan


#India
India = auto.arima(Indiareturns,xreg = Indiatemps, ic="aic")
acf(India$residuals)
Box.test(India$residuals)
India

#Australia
Australia = auto.arima(Australiareturns, xreg = Australiatemps, ic = "aic")
acf(Australia$residuals)
Box.test(Australia$residuals)
Australia




