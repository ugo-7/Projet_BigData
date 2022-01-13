# Intro

require(HRW)
data <- WarsawApts
plot(data$construction.date, log(data$areaPerMzloty))

# Résidus pour fonctions poly

attach(data)
res.mod1 <- lm(areaPerMzloty~construction.date, data)

plot(fitted(res.mod1),residuals(res.mod1),xlab="Valeurs ajustées", ylab="Résidus",
     main="Résidus de la regression simple")
abline(h=0)


modelm2 <- lm(areaPerMzloty~poly(construction.date,2), data)
summary(modelm2)
plot(modelm2$fitted, modelm2$residuals,
     xlab="Valeurs ajustées", ylab="Résidus")
abline(h=0)

modelm3 <- lm(areaPerMzloty~poly(construction.date,3), data)
summary(modelm3)
plot(modelm3$fitted, modelm3$residuals, xlab="Valeurs ajustées", ylab="Résidus")
abline(h=0)

# Résultats fonctions poly

res.mod3 <- lm(areaPerMzloty~poly(construction.date,3), data)
res.mod4 <- lm(areaPerMzloty~poly(construction.date,4), data)

plot(predict(res.mod3),residuals(res.mod3),xlab="Valeurs ajustées", ylab="Résidus",
     main="Résidus de la regression cubique")
abline(h=0)

plot(predict(res.mod4),residuals(res.mod4),xlab="Valeurs ajustées", ylab="Résidus",
     main="Résidus de la regression quartique")
abline(h=0)

plot(construction.date,areaPerMzloty, main='Higher-degree polynomial fits to the LIDAR data')
lines(construction.date, fitted(res.mod3), type="l", lty=2, lwd=2)
lines(construction.date, predict(res.mod4), type="l", lty=1, lwd=2)
legend(420,-0.6,c("cubic model","quartic model"),lty=c(2,1), lwd=c(2,2))
detach(data)


##df=dimension K+m de la base
##degree=degre m-1 du polynome
##intercept=TRUE pour avoir des fonctions B-spline de somme=1

require(splines)
##nombre de noeuds=df-degree-1=3
Bbase <- bs(data$construction.date, df=6, degree=2, intercept=TRUE)
attr(Bbase,"knots")

res.bs<-lm(areaPerMzloty~bs(construction.date,df=6,degree=2, intercept=TRUE)-1, data)
summary(res.bs)
plot(data$construction.date, data$areaPerMzloty, xlab="range",ylab="logratio",main='B-spline fitting of LIDAR data with K=5 and m=3')
lines(data$construction.date, predict(res.bs), type="l", col="red")

# Exemples 

par(mfrow=c(3,2), mar = c(0,0,0,0))
for(i in seq(5, 30, 5)){
  res.bs <-lm(areaPerMzloty~bs(construction.date,df=i,degree=2, intercept=TRUE)-1,data=data)
  plot(data$construction.date,data$areaPerMzloty,xlab="range",ylab="areaPerMzloty")
  lines(data$construction.date,predict(res.bs),type="l",col="red")
  text(0.15,-0.8,labels="K=")
  text(0.2,-0.8,labels=i-3)
  text(0.16,-0.9,labels="m= 3")
}

#Penalisee

require(mgcv)

res.fit <- gam(areaPerMzloty~s(construction.date,bs="bs",k=5,m=c(3,2)), data)
summary(res.fit)








#MCO simple
x <- model.matrix(areaPerMzloty~surface+construction.date, data)[,-1]
y <- data$areaPerMzloty
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

lin.mco <- lm(areaPerMzloty~surface+construction.date, data)
coef(lin.mco)
res2 <- lm(areaPerMzloty~surface+construction.date, data=data[train,])
y.pred2 <- predict(res2, data[test,])
mean((y.pred2-y.test)^2)


#Noyau

epanech.fn = function(u){
  0.75*(1-u^2)*(abs(u)<1)
}

reg.noyau = function(yech,xech,x,h){
  w = epanech.fn((xech-x)/h)/sum(epanech.fn((xech-x)/h))
  yhat = sum(w*yech)
  yhat
}

yest = function(x, y, h){
  yhat = y
  for(i in 1:length(y)){
    yech = y[-i]
    xech = x[-i]
    yhat[i] = reg.noyau(yech, xech, x[i], h)
  }
  yhat
}

yhat <- yest(data$construction.date, data$areaPerMzloty, 1)
hist(yhat)
