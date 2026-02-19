##
##final project exploratory analysis
##

##
##1
##

library(readxl)
edata<-read_excel("injury24.xlsx")
head(edata)

unique(is.na(edata))
#no NA are present in the dataset

#
#devide the dataset between training set and test set
#

smp_size<-floor(0.90*nrow(edata))
set.seed(432123)
train_ind<-sample(seq_len(nrow(edata)),size=smp_size)
train_edata1<-edata[train_ind,]
test_edata1<-edata[-train_ind,]
str(train_edata1)

#plot the data to check for correlations

plot(train_edata1)


#fit the MLR

eMLR1<-glm(fatality~.-ID,family='binomial', data = train_edata1)
summary(eMLR1)

library(car)
vif(eMLR1)

#step(eMLR1,trace=0)

#setup for the stepAIC function

library(MASS)
enull2<-glm(fatality~1,family='binomial',data=train_edata1)
stepmodel1<-stepAIC(enull2,scope=list(lower=enull2,upper=eMLR1), data= train_edata1,direction='forward')
stepmodel2<-stepAIC(eMLR1,scope=list(lower=enull2,upper=eMLR1), data= train_edata1,direction='backward')
stepmodel1
stepmodel2

#both the forward and backward stepAIC suggest fatality~age+race+area+inhal as the best model

emodel1fp1<-glm(fatality~age+race+area+inhal,family='binomial',data=train_edata1)
summary(emodel1fp1)
confint(emodel1fp1,level=0.95)
pR2fp1 = 1 - emodel1fp1$deviance / emodel1fp1$null.deviance

#now we study interactions between the variables

emodel1fp2<-glm(fatality~age+race+area+inhal+area:race,family='binomial',data=train_edata1)
summary(emodel1fp2)
pR2fp2 = 1 - emodel1fp2$deviance / emodel1fp2$null.deviance

anova(emodel1fp2,emodel1fp1,test="LRT")

emodel1fp3<-glm(fatality~age+race+area+inhal+area:race+age:inhal+inhal:area,family='binomial',data=train_edata1)
summary(emodel1fp3)
pR2fp3 = 1 - emodel1fp3$deviance / emodel1fp3$null.deviance

anova(emodel1fp3,emodel1fp1,test="LRT")

png("plots1/sr1fp3.png")
plot(rstandard(emodel1fp3),ylab="Standard residual",ylim=(c(-4.5,4.5)),pch=16,col=("blue"),cex=0.5,main="Standard residuals plot model emodel1fp3")
abline(3,0, col=("orange"),lty=2)
abline(-3,0, col=("orange"),lty=2)
dev.off()

png("plots1/cd1fp3_2.png")
plot(cooks.distance(emodel1fp3),ylab="Cook's distance",main="Cook's distances plot emodel1fp3")
dev.off()


emodel1fp4<-glm(fatality~age+area+inhal+age:inhal+inhal:area,family='binomial',data=train_edata1)
summary(emodel1fp4)
pR2fp4 = 1 - emodel1fp4$deviance / emodel1fp4$null.deviance

anova(emodel1fp3,emodel1fp4,test="LRT")

#emodel1fp5<-glm(fatality~age+area+inhal+age:inhal+inhal:area+age:area,family='binomial',data=train_edata1)
#summary(emodel1fp5)
#pR2fp5 = 1 - emodel1fp5$deviance / emodel1fp5$null.deviance

#anova(emodel1fp5,emodel1fp4,test="LRT")

#emodel1fp6<-glm(fatality~age+area+inhal+age:inhal+inhal:area,family='binomial',data=train_edata1)
#summary(emodel1fp6)
#pR2fp6 = 1 - emodel1fp6$deviance / emodel1fp6$null.deviance

#anova(emodel1fp3,emodel1fp6,test="LRT")

#
#predictions
#

prediction<- predict(emodel1fp3,test_edata1,type="response")

predict_classes <- ifelse(prediction >= 0.7, 1, 0)

data.prediction <- data.frame(actual = c(test_edata1$fatality),
                              prediction = c(predict_classes))

non_matching_count <- sum(data.prediction$actual != data.prediction$prediction)

accuracy<-abs((non_matching_count/nrow(data.prediction))-1)
accuracy

library(caret)
cm_model3<-confusionMatrix(data=as.factor(data.prediction$prediction),reference=as.factor(data.prediction$actual))
cm_model3

