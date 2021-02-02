A1 <- read.csv("~/Документы/инструментальная поддержка управления портфелем/exz/AMZN-RM_160123_210123.txt", check.names=FALSE)
A2 <- A1[6][,]
B1 <- read.csv('~/Документы/инструментальная поддержка управления портфелем/exz/BA-RM_160123_210123.txt', check.names=FALSE)
B2 <- B1[6][,]
C1 <- read.csv('~/Документы/инструментальная поддержка управления портфелем/exz/DIS-RM_160123_210123.txt', check.names=FALSE)
C2 <- C1[6][,]

#Графики котировок
plot(A2, type='l')
title("График котировок акций Амазона")
plot(B2, type='l')
title("График котировок акций Boeing")
plot(C2, type='l')
title("График котировок акций Дисней")


#Длина массива
T <- length(A2)-1
T


#Лог доходность 
A3 <- log(A2[2:(T+1)]/A2[1:T]) 
B3 <- log(B2[2:(T+1)]/B2[1:T])
C3 <- log(C2[2:(T+1)]/C2[1:T])



portf <- cbind(A3,B3,C3)

plot(A3, type='l')
title("Логарифмическая доходность акций Амазона")
plot(B3, type='l')
title("Логарифмическая доходность акций Boeing")
plot(C3, type='l')
title("Логарифмическая доходность акций Дисней")


# оценка параметров
library(gplots) 
library(ghyp)


#Метод Монте Карло
alpha<-0.05 #то есть уровень потерь - 95%
N=1000000
    #Находим наилучшую модель минимизируя информационный критерий Акаика 
aic.mv <- stepAIC.ghyp(portf, dist=c("gauss","t","ghyp"),symmetric=NULL,silent=TRUE)
summary(aic.mv$best.model)
   # Получили ассиметричное распределние Стьюдента 
w <- c(1/3, 1/3, 1/3)  # веса активов в портфеле
sim <- rghyp(n=N,object=aic.mv$best.model)
portf.sim <- w[1]*sim[,1]+w[2]*sim[,2]+w[3]*sim[,3]
portf.sim <- sort(portf.sim)
VaR <- portf.sim[alpha*N]
ES <- mean(portf.sim[1:(alpha*N-1)])
VaR #граница потерь 
ES #ожидаемые потери
portf


# Подбор обобщенного гиперболического распределения методом максимального правдоподобия
portf<-portf[complete.cases(portf), ]
portf.fit <- fit.ghypmv(portf,symmetric=FALSE,silent=TRUE)


#Портфель минимизирующий риск при заданном уровне доходности - 0.002
opt <- portfolio.optimize(portf.fit,
                          risk.measure="value.at.risk",type="target.return",
                          target.return=0.002,risk.free=NULL,level=0.95,silent=TRUE)
opt$opt.weights 
w_setP<- opt$opt.weights 
N <- 10^6; alpha <- 0.05
sim <- rghyp(n = N, object = portf.fit)
portf.sim <- w_setP[1]*sim[,1] + w_setP[2]*sim[,2] + w_setP[3]*sim[,3]
portf.sim <- sort(portf.sim)
VaR_setP <- portf.sim[alpha*N]
ES_setP <- mean(portf.sim[1:(alpha*N-1)])
print(VaR_setP)
print(ES_setP)



#Портфель минимизирующий риск 
opt <- portfolio.optimize(portf.fit,
                          risk.measure="value.at.risk",type="minimum.risk",
                          risk.free=NULL,level=0.95,silent=TRUE)
opt$opt.weights
w_minR<- opt$opt.weights 
N <- 10^6; alpha <- 0.05
sim <- rghyp(n = N, object = portf.fit)
portf.sim <- w_minR[1]*sim[,1] + w_minR[2]*sim[,2] + w_minR[3]*sim[,3]
portf.sim <- sort(portf.sim)
VaR_minR <- portf.sim[alpha*N]
ES_minR <- mean(portf.sim[1:(alpha*N-1)])
print(VaR_minR)
print(ES_minR)




# Портфель максимизирующий наклон «(ожидаемая доходность - безрисковая ставка) / риск».
opt <- portf.optimize(portf.fit,
                          risk.measure="value.at.risk",type="tangency",
                          target.return=0.002, risk.free=0,05,level=0.95,silent=TRUE)
opt$opt.weights
w_maxS<- opt$opt.weights 
N <- 10^6; alpha <- 0.05
sim <- rghyp(n = N, object = portf.fit)
portf.sim <- w_maxS[1]*sim[,1] + w_maxS[2]*sim[,2] + w_maxS[3]*sim[,3]
portf.sim <- sort(portf.sim)
VaR_maxS <- portf.sim[alpha*N]
ES_maxS <- mean(portf.sim[1:(alpha*N-1)])
print(VaR_maxS)
print(ES_maxS)


w <- c(0.4603911, 0.2200941, 0.3195148)
portf

VaR_curve <- numeric()
h <- 0.5 * 100
h
h.w <- matrix(ncol = 3, nrow = length(portf[,1]) - h)
h.w


for (i in (h+1) :length(portf[,1])) 
{
  h.portf <- portf[(i-h):(i-1),]
  
  h.portf.fit <- fit.ghypmv(h.portf,symmetric=FALSE,silent=TRUE)
  
  h.opt <- portfolio.optimize(h.portf.fit,
                              risk.measure="value.at.risk",type="minimum.risk",
                              target.return=NULL,risk.free=NULL,level=0.95,silent=TRUE)
  
  h.w[i-h,] <- h.opt$opt.weights # assets' weights in portfolio
  
  sim <- rghyp(n=N,object=h.portf.fit)
  h.portf.sim <- h.w[i-h,1]*sim[,1]+h.w[i-h,2]*sim[,2]
  h.portf.sim <- sort(h.portf.sim)
  VaR_curve[i-h] <- h.portf.sim[alpha*N]
}

fact <- portf[(h+1):length(portf[,1]),1]*h.w[,1] + portf[(h+1):length(portf[,2]),2]*h.w[,2]
plot(as.numeric(fact),type="l", ylab = "Return")
lines(VaR_curve,col="red")





#тест купика на пробите вар 
K <- sum(fact<VaR_curve); alpha0 <- K/length(portf)
S <- -2*log((1-alpha)^(length(portf)-K)*alpha^K)+2*log((1-alpha0)^(length(portf)-K)*alpha0^K)
p.value <- 1-pchisq(S,df=1)
p.value
alpha
alpha0

portf_return <- w[1]*mean(portf[,1])+
  w[2]*mean(portf[,2])+
  w[3]*mean(portf[,3])
portf_return