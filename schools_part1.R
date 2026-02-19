##
## final project school draft analysis 
##

library(readxl)
edatas<-read_excel("schools24.xlsx")
head(edatas)
str(edatas)

unique(is.na(edatas))
#no NA are present in the dataset

#we check the data with a scatterplot matrix

png(file="splotschool.png")
plot(edatas)
dev.off()

#
#considerations about the scatter plot matrix
#
#there is linear correlation between score and income
#there is linear correlation between score and free lunch 
#better check for interation betweeen free lunch and income
#

#we devide the dataset between a train set and a test set with the help of sample function

smp_size_s<-floor(0.90*nrow(edatas))
set.seed(29476)
train_ind<-sample(seq_len(nrow(edatas)),size=smp_size_s)
train_edata2<-edatas[train_ind,]
test_edata2<-edatas[-train_ind,]
str(train_edata2)

#looking at the model I would devide it in two models, one searching for score at 11 and the second for the score at 16
#we expect that different predictors will have a different influence depending on the age of the students

#check NORMALITY of the response variables

hist(train_edata2$score1)
hist(train_edata2$score2)

shapiro.test(train_edata2$score1)
shapiro.test(train_edata2$score2)

#data for score 1 might need a transformation


#
#analysis using as a response "score1" score of an age of 11
#

#Fit the full model
MLRs11<-lm(score1~.-score2-ID,data=train_edata2)
summary(MLRs11)

#search for multicollinearity

library(car)
vif(MLRs11)
#NO COLLINEARITY IS PRESENT

#proceed stepAIC analysis

library(MASS)
enulls1<-lm(score1~1,data=train_edata2)
stepAIC(enulls1,scope=list(lower=enulls1,upper=MLRs11), data= train_edata2,direction='forward')
#best model forward
#score1 ~ free + income + stratio + special + noteng
stepAIC(MLRs11,scope=list(lower=enulls1,upper=MLRs11), data= train_edata2,direction='backward')
#best model backwards
#score1 ~ special + free + stratio + income + noteng

#both backward and forward stepAIC agree on the best model

models11_1<-lm(score1~special+free+stratio+income+noteng,data=train_edata2)
summary(models11_1)
plot(cooks.distance(models11_1))

#check for outliers and high leverage points

plot(rstandard(models11_1),ylim=(c(-4.5,4.5)),pch=16,col=("blue"),cex=0.5)
abline(3,0, col=("orange"),lty=2)
abline(-3,0, col=("orange"),lty=2)

plot(cooks.distance(models11_1))

#there are NO OUTLIERS or high leverage points

#residualPlots and C+Rplot

png("plots2/rpm11_1.png")
residualPlots(models11_1,main="Residual plots")
dev.off()
#log translormation might be needed for variable noteng
#residuals show a pattern

png("plots2/crm11_1.png")
crPlots(models11_1)
dev.off()
#quadratic transformation is needed for variable income

#model fit implementing suggested transformation

models11_2<-lm(score1~special+free+stratio+income+noteng+I(income^2),data=train_edata2)
summary(models11_2)


plot(models11_2$residuals)

png("plots2/rsm11_2.png")
plot(rstandard(models11_2),ylab="Standard residual",ylim=(c(-4.5,4.5)),pch=16,col=("blue"),cex=0.5,main="Standard residuals plot Model11_2")
abline(3,0, col=("orange"),lty=2)
abline(-3,0, col=("orange"),lty=2)
dev.off()
residualPlots(models11_2)
crPlots(models11_2)

png("plots2/cdm11_2.png")
plot(cooks.distance(models11_2),ylab="Cook's distance",main="Cook's distances plot models11_2")
dev.off()

#
#we spot the presence of an high leverage point
#

cdm11_2<-cooks.distance(models11_2)
infm11_2<-which(cdm11_2 > 1) 
train_edata2_c<-train_edata2[-infm11_2,]
models11_3<-lm(score1~special+free+stratio+income+noteng+I(income^2),data=train_edata2_c)
summary(models11_3)
png("plots2/cdm11_2.png")
plot(cooks.distance(models11_2),ylab="Cook's distance",main="Cook's distances plot models11_2")
dev.off()

library(car)
crPlots(models11_3) 

#residuals sparesely distibute around the zero and show no pattern, residuals are too high in value, high standard error

#anova analysis to see if the use of the quadratic transformation is relevant

anova(models11_1,models11_2)

#anova analysis shows that we should accept the more complex model

#boxcox analysis for the response

bc2<-boxCox(models11_2)

#no clear transformation on the y is suggested by the box cox function

#study of interactions

residualPlots(models11_3)
plot(models11_3$residuals)

shapiro.test(sqrt(train_edata2$score1))

models11_4<-lm(score1~special+free+stratio+noteng+income,data=train_edata2_c)
summary(models11_4)
library(car)
crPlots(models11_4)

png("plots2/srm11_4.png")
plot(rstandard(models11_4),ylim=(c(-4.5,4.5)),pch=16,col=("blue"),ylab="Standard residual",cex=0.5,main="Standardised residuals plot models11_4")
abline(3,0, col=("orange"),lty=2)
abline(-3,0, col=("orange"),lty=2)
dev.off()

png("plots2/cdm11_4.png")
plot(cooks.distance(models11_4),ylab="Cook's distance",main="Cook's distances plot models11_4")
dev.off()


summary(edatas$score1)


#
#predictions
#

pred11<- predict(models11_4,test_edata2)
data.prediction11 <- data.frame(actual = c(test_edata2$score1),
                              prediction = c(pred11))

library(MLmetrics)
#Mean Absolute Percentage Error Loss
MAPE(data.prediction11$prediction,data.prediction11$actual)

#
#
#
#
#
#

#no interactions are relevant

#with the actual model we can redact the following conclusion
#when kids are 11

#spending for students with special needs doesn't increase the score
#free lunch has a negative effect on the score
#increased students per teacher ratio has a negative effect on the score
#students from high income families tend to perform better
#if the percentage of non english students is higher the score is lower

#possible solutions

#it is not ethical to reduce spending for students in special need
#free lounch can removed from the shools, it has a negative effect on score and budget
#more teachers should be hired for increasing average score
#we can allocate the budget saved from the free lounches to hire more teachers

#
#analysis using as a response "score2" score of an age of 16
#

#Fit the full model
MLRs16<-lm(score2~.-score1,data=train_edata2)
summary(MLRs16)

#search for multicollinearity

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

