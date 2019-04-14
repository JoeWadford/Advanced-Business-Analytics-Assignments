setwd("C:/Users/joewa/OneDrive/Documents/MBA/MBAD 6211/2- assignments/assignment 4")
data <- fread("ATT_Twitter.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

head(data)

colnames(data)

# 4.1
x = ts(data[,2])
plot(x)

z = log(x)
plot(z)

#4.2
PP.test(x)

#4.3
par(mfrow = c(1,2))
acf(x, main="ACF X")
pacf(x, main='PACF X')

#4.4
ARIMAfit = auto.arima(x, approximation=FALSE,trace=TRUE)
summary(ARIMAfit)

#5.1
spec1=ugarchspec(variance.model=list(model="sGARCH"),
                 mean.model=list(armaOrder=c(0,0)))

fit1=ugarchfit(data=x,spec=spec1)
show(fit1)

#5.2
spec2=ugarchspec(variance.model=list(model="sGARCH"),
                 mean.model=list(armaOrder=c(0,1)))

fit2=ugarchfit(data=x,spec=spec2)
show(fit2)

#6.1
spec3=ugarchspec(variance.model=list(model="apARCH"),
                 mean.model=list(armaOrder=c(0,0)))

fit3=ugarchfit(data=x,spec=spec3)
show(fit3)

#6.2
spec4=ugarchspec(variance.model=list(model="apARCH"),
                 mean.model=list(armaOrder=c(0,1)))

fit4=ugarchfit(data=x,spec=spec4)
show(fit4)

