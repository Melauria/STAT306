dat<-read.table("D:/2013data.txt",header = T)
reg<-lm(gc~pop+wage+stu+traffic+sales+hos+asp+sdh, data=dat)
summary(reg)

res<-resid(reg)
sigma<-sum$sigma
qqnorm(res,main="normal Q-Q plot of residuals")
qqline(res)
pred<-predict(reg)
plot(pred,res,xlab="predicted value",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
sum=summary(reg)


# traffic is transformed to log(traffic) due to the presence of outlier
#	hos is transformed to log(hos) due to the presence of outlier.
#	sdh is transformed to log(sdh) due to the presence of outliers.

dat$pop=dat$pop/100
dat$wage=dat$wage/1000
dat$traffic=log(dat$traffic/1000)
dat$sales=dat$sales/100
dat$hos=log(dat$hos)
dat$asp=dat$asp/1000
dat$sdh=log(dat$sdh/1000)
dat$gc=dat$gc/100


### plots
plot(dat$pop,res,xlab="pop",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
plot(dat$wage,res,xlab="wage",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
plot(dat$stu,res,xlab="stu",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
plot(dat$traffic,res,xlab="traffic",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
plot(dat$sales,res,xlab="sales",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
plot(dat$hos,res,xlab="log(hos)",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
plot(dat$asp,res,xlab="asp",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
plot(dat$sdh,res,xlab="sdh",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)

# All Variables
reg1<-lm(gc~pop+wage+stu+traffic+sales+hos+asp+sdh+iC+iE+iS+iNE+iSW+iNW,data = dat)
summary(reg1)

# Exhaustive of all variables
install.packages("leaps")
library(leaps)
s1 <- regsubsets(gc~pop+wage+stu+traffic+sales+hos+asp+sdh+iSW+iC+iE+iS+iNE+iNW, data=dat, method="exhaustive",nbest=1,nvmax=14)
ss1<-summary(s1)
ss1
ss1$cp
ss1$adjr2

# Cross Validation — leave-one-out
> ls.cvrmse <- function(ls.out)
  {
      res.cv <- ls.out$residuals / (1.0 - ls.diag(ls.out)$hat)
      is.na.res <- is.na(res.cv)
      res.cv <- res.cv[!is.na.res]
      cvrmse <- sqrt(sum(res.cv^2) / length(res.cv))
      return(cvrmse)
    }
model1<-lm(gc~pop+stu+traffic+sales+asp+sdh+iE+iS+iSW,data = dat)
model1.cvrmse <- ls.cvrmse(model1)
model2<-lm(gc~pop+stu+traffic+sales+hos+asp+sdh+iE+iS+iNE+iSW+iNW,data = dat)
model2.cvrmse <- ls.cvrmse(model2)
print(c(model1.cvrmse, model2.cvrmse))

# 5-fold cross validation
n <- nrow(dat)
sn <- floor(n/5)
set.seed(306)
B <- 500
errMx <- matrix(NA, B, 2) 
colnames(errMx) <- c("model1", "model2")
for (i in 1:B)
    {
        testInd <- sample(1:n, sn, replace=FALSE)
        tTestDat <- dat[testInd, ] #Treat the sampled index as testing set
        tTrainDat <- dat[-testInd, ] #The rest is training set.
        tmodel1 <- lm(gc~pop+stu+traffic+sales+asp+sdh+iE+iS+iSW, data = tTrainDat)
        tmodel1.pred <- predict(tmodel1, tTestDat)
        errMx[i, 1] <- sqrt(sum((tTestDat$gc - tmodel1.pred)^2)/sn)
        tmodel2 <- lm(gc~pop+stu+traffic+sales+hos+asp+sdh+iE+iS+iNE+iSW+iNW, data = tTrainDat)
        tmodel2.pred <- predict(tmodel2, tTestDat)
    }
            