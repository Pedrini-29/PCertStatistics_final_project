#
MLRs16<-lm(score2~.-ID-score1,data=train_edata2)
summary(MLRs16)
png("plots2/srMLR16.png")
plot(rstandard(MLRs16),ylim=(c(-4.5,4.5)),pch=16,col=("blue"),cex=0.5)
abline(3,0, col=("orange"),lty=2)
abline(-3,0, col=("orange"),lty=2)
dev.off()
#
vif(MLRs16)

outliers <- abs(rstandard(MLRs16)) > 3
train_edata2_f<-train_edata2[!outliers,]
MLRs16c<-lm(score2~.-ID-score1,data=train_edata2_f)
summary(MLRs16c)
plot(rstandard(MLRs16c))

outliers <- abs(rstandard(MLRs16c)) > 3
train_edata2_ff<-train_edata2_f[!outliers,]
MLRs16cc<-lm(score2~.-ID-score1,data=train_edata2_ff)
summary(MLRs16cc)
png("plots2/srMLR16cc.png")
plot(rstandard(MLRs16cc),ylim=(c(-4.5,4.5)),pch=16,col=("blue"),cex=0.5)
abline(3,0, col=("orange"),lty=2)
abline(-3,0, col=("orange"),lty=2)
dev.off()

library(MASS)
enulls2<-lm(score2~1,data=train_edata2_ss)
stepAIC(enulls2,scope=list(lower=enulls2,upper=MLRs16cc), data= train_edata2_ff,direction='forward')
stepAIC(MLRs16cc,scope=list(lower=enulls2,upper=MLRs16cc), data= train_edata2_ff,direction='backward')

library(car)
model16_p1<-lm(score2~income+free+special+stratio,data=train_edata2_ff)
summary(model16_p1)
plot(rstandard(model16_p1))
plot(cooks.distance(model16_p1))
crPlots(model16_p1)

model16_p2<-lm(score2~income+free,data=train_edata2_ff)
summary(model16_p2)
png("plots2/srm16_p2.png")
plot(rstandard(model16_p2),ylim=(c(-4.5,4.5)),pch=16,col=("blue"),cex=0.5)
abline(3,0, col=("orange"),lty=2)
abline(-3,0, col=("orange"),lty=2)
dev.off()

png("plots2/cdm16_p2.png")
plot(cooks.distance(model16_p2))
dev.off()

png("plots2/crm16_3.png")
crPlots(model16_p3)
dev.off()



model16_p3<-lm(score2~income+free+I(income^2),data=train_edata2_ff)
summary(model16_p3)
crPlots(model16_p3)
png("plots2/srm16_p3.png")
plot(rstandard(model16_p3),ylim=(c(-4.5,4.5)),pch=16,col=("blue"),cex=0.5)
abline(3,0, col=("orange"),lty=2)
abline(-3,0, col=("orange"),lty=2)
dev.off()

png("plots2/cdm16_p3.png")
plot(cooks.distance(model16_p3))
dev.off()


anova(model16_p3,model16_p2)

pred16<- predict(model16_p3,test_edata2)
data.prediction16 <- data.frame(actual = c(test_edata2$score2),
                              prediction = c(pred16))

library(MLmetrics)
#Mean Absolute Percentage Error Loss
MAPE(data.prediction16$prediction,data.prediction16$actual)



