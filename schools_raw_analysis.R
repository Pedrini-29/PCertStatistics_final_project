#
#analysis using as a response "score2" score of an age of 16
#

#Fit the full model
MLRs16<-lm(score2~.-ID,data=train_edata2)
summary(MLRs16)
plot(rstandard(MLRs16))


png("plots2/rsmlr16.png")
plot(rstandard(MLRs16),ylim=(c(-4.5,4.5)),pch=16,col=("blue"),ylab="Standard residual",cex=0.5,main="Standardised residuals plot MLR16")
abline(3,0, col=("orange"),lty=2)
abline(-3,0, col=("orange"),lty=2)
dev.off()

#removing outliers


outliers <- abs(rstandard(MLRs16)) > 3
train_edata2_s<-train_edata2[!outliers,]
MLRs16c<-lm(score2~.-ID,data=train_edata2_s)
summary(MLRs16c)
plot(rstandard(MLRs16c))

outliers <- abs(rstandard(MLRs16c)) > 3
train_edata2_ss<-train_edata2_s[!outliers,]
MLRs16cc<-lm(score2~.-ID,data=train_edata2_ss)
summary(MLRs16cc)


library(MASS)
enulls2<-lm(score2~1,data=train_edata2_ss)
stepAIC(enulls2,scope=list(lower=enulls2,upper=MLRs16cc), data= train_edata2_ss,direction='forward')
stepAIC(MLRs16cc,scope=list(lower=enulls2,upper=MLRs16cc), data= train_edata2_ss,direction='backward')


library(car)
model16_p1<-lm(score2~score1+income+free+spendspec,data=train_edata2_ss)
summary(model16_p1)
plot(rstandard(model16_p1))
plot(cooks.distance(model16_p1))
crPlots(model16_p1)

pred16<- predict(model16_p2,test_edata2)
data.prediction16 <- data.frame(actual = c(test_edata2$score2),
                              prediction = c(pred16))

library(MLmetrics)
#Mean Absolute Percentage Error Loss
MAPE(data.prediction16$prediction,data.prediction16$actual)

library(car)
model16_p2<-lm(score2~income+free,data=train_edata2_ss)
summary(model16_p2)
plot(rstandard(model16_p1))
plot(cooks.distance(model16_p1))












library(car)
vif(MLRs16)
#NO COLLINEARITY IS PRESENT

#proceed stepAIC analysis

library(MASS)
enulls2<-lm(score2~1,data=train_edata2)
stepAIC(enulls2,scope=list(lower=enulls2,upper=MLRs16), data= train_edata2,direction='forward')
#best model forward
#score2 ~ free + income + stratio + special + spendspec + ID + spendpp + ipad 
stepAIC(MLRs16,scope=list(lower=enulls2,upper=MLRs16), data= train_edata2,direction='backward')
#best model backwards
#score2 ~ ID + spendspec + spendpp + ipad + free + stratio + income

#both backward and forward stepAIC agree on the best model

models16_1<-lm(score2 ~ free + income + stratio + special + spendspec + spendpp + ipad,data=train_edata2)
summary(models16_1)
crPlots(models16_2)

#model shows that variable is not significant, we can rule it out of the model and refit

models16_2<-lm(score2~free+ income + stratio+ spendspec,data=train_edata2)
summary(models16_2)

#check for outliers and high leverage points

plot(rstandard(models16_2),ylim=(c(-4.5,4.5)),pch=16,col=("blue"),cex=0.5)
abline(3,0, col=("orange"),lty=2)
abline(-3,0, col=("orange"),lty=2)

plot(cooks.distance(models16_1))

#there are three outliers and they should be removed from the analysis
#there are no leverage points

#residualPlots and C+Rplot

residualPlots(models16_2)
#log translormation might be needed for variable noteng
#residuals show a pattern

crPlots(models16_2)
#quadratic transformation is needed for variable income

#model fit implementing suggested transformation

models16_3<-lm(score2 ~ free + income+I(income^2) + stratio + spendspec + spendpp +ipad+ I(ipad^2),data=train_edata2)
summary(models16_3)

#the square of income is not significant

models16_4<-lm(score2 ~ free + income + stratio + spendspec + spendpp + ipad + I(ipad^2),data=train_edata2)
summary(models16_4)

plot(rstandard(models16_2),ylim=(c(-4.5,4.5)),pch=16,col=("blue"),cex=0.5)
abline(3,0, col=("orange"),lty=2)
abline(-3,0, col=("orange"),lty=2)
residualPlots(models11_2)
crPlots(models11_2)

models16_5<-lm(score2 ~ free + income + stratio + spendspec + spendpp + ipad,data=train_edata2)
summary(models16_5)

models16_6<-lm(score2 ~ free + income + stratio + spendspec + spendpp,data=train_edata2)
summary(models16_6)
residualPlots(models16_6)
anova(models16_6,models16_4)

models16_7<-lm(score2 ~ free + income + stratio + spendspec,data=train_edata2)
summary(models16_7)
residualPlots(models16_7)
anova(models16_7,models16_4)

plot(rstandard(models16_7),ylim=(c(-4.5,4.5)),pch=16,col=("blue"),cex=0.5)
abline(3,0, col=("orange"),lty=2)
abline(-3,0, col=("orange"),lty=2)

#residuals sparesely distibute around the zero and show no pattern


#anova test to see if the complication of the model is useful or if it is redondant

anova(models16_4,models16_2)

#the complex model is more significant and explains more of the variability of the response

#boxcox analysis for the response

bc2<-boxCox(models16_4)

#no clear transformation on the y is suggested by the box cox function

#study of interactions
  
models16_5<-lm(score2 ~ free + income + stratio + spendspec + ID + spendpp + ipad + I(ipad^2)+free:income+free:stratio+free:spendspec+free:ID+free:spendpp+free:ipad,data=train_edata2)
summary(models16_5) 

#none of the interaction with free is significant

models16_6<-lm(score2 ~ free + income + stratio + spendspec + ID + spendpp + ipad + I(ipad^2)+income:stratio+income:spendspec+income:ID,,data=train_edata2)
summary(models16_6) 


par(mfrow = c(1, 1, 1, 1))
hist(log(edatas$noteng))
edatas$noteng2<-log(log(edatas$noteng+1))
hist(edatas$noteng2)

MLRs16<-lm(score2~.-score1-noteng,data=edatas)
summary(MLRs16)
hist(edatas$noteng2)



MLRsc16<-lm(score2~.,data=train_edata2)
summary(MLRsc16)

modelsc16_1<-lm(score2 ~ free + income + spendspec + ipad + score1,data=train_edata2)
summary(modelsc16_1)
crPlots(modelsc16_1)
vif(modelsc16_1)
plot(cooks.distance(modelsc16_1))

plot(rstandard(modelsc16_1),ylim=(c(-4.5,4.5)),pch=16,col=("blue"),cex=0.5)
abline(3,0, col=("orange"),lty=2)
abline(-3,0, col=("orange"),lty=2)

boxCox(modelsc16_1)

modelsc16_2<-lm(score2 ~ free + income + spendspec + score1,data=train_edata2)
summary(modelsc16_2)
plot(rstandard(modelsc16_2),ylim=(c(-4.5,4.5)),pch=16,col=("blue"),cex=0.5)
abline(3,0, col=("orange"),lty=2)
abline(-3,0, col=("orange"),lty=2)
