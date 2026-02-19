
##
##Esploratory analysis schools24
##

str(edatas)

plot(score1~salary,data=edatas)
plot(score2~salary,data=edatas)

#Plot relation between students elegible to free lunch and student score

png("plots2/esplots1.png")
plot(score1~free,data=edatas,col="blue",cex=1,pch=16,xlab="% students elegible for free lunch",ylab="Students score",cex.lab=0.8,cex.axis=0.8,ylim=c(640,750))
title("Relation between students elegible to free lunch and student score",cex.main=1)
points(score2~free,data=edatas, col="gold2",pch=16,cex=1)
legend(x="topright",legend=c("score1","score2"),col=c("blue","gold2"),pch=16,cex=1,bty="n")
dev.off()

plot(spendspec~special,data=edatas)
plot(score1~special,data=edatas)
plot(score2~special,data=edatas)

plot(score1~spendspec,data=edatas)
plot(score2~spendspec,data=edatas)

#Plot relation between income per student (€ x000s) and student score

png("plots2/esplots2.png")
plot(score1~income,data=edatas,col="blue",cex=1,pch=16,xlab="school average income per student (€ x000s)",ylab="Students score",cex.lab=0.8,cex.axis=0.8,ylim=c(640,750))
title("Relation between income per student (€ x000s) and student score",cex.main=1)
points(score2~income,data=edatas, col="gold2",pch=16,cex=1)
legend(x="bottomright",legend=c("score1","score2"),col=c("blue","gold2"),pch=16,cex=1,bty="n")
dev.off()


#Plot relation between income per student (€ x000s) and % of s. eligible for free lunch

png("plots2/esplots3.png")
plot(free~income,data=edatas,col="blue",cex=1,pch=16,xlab="school average income per student (€ x000s)",ylab="% students elegible for free lunch",cex.lab=0.8,cex.axis=0.8)
title("Relation between income per student (€ x000s) and % of s. eligible for free lunch",cex.main=1)
dev.off()

#Scatterplot matrix of dataset schools24

png("plots2/scplot2.png",width=6,height=6,units="in",res=600)
plot(edatas,main="Scatterplot matrix of dataset schools24",main.cex=0.8)
dev.off()


str(edatas)