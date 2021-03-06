# 품질과 가격 간 상관관계 없는 모형

# Simulate a data set for regression illustratoin

# true model logQ is funciton of logP, dummy1, dummy2,  a quality

set.seed(12876)

####
#### data generation
####

n = 5000  # number of observation
trueB = c(3,-3, 1.5, 0.7, 3)
err = rnorm(n,sd=3)  # error term simulation

u1 = runif(n)
u2 = runif(n)
u3 = runif(n)
u4 = runif(n)

logPr = u1 +  u2
quality = u3
dummy1 = (u4> 0.7)*1.0
dummy2 = ((u4<0.7)&(u4>0.3))*1.0

logQ= trueB[1] + trueB[2]*logPr + trueB[3]*dummy1 + trueB[4]*dummy2 + trueB[5]*quality + err

summary(cbind(logQ,logPr, dummy1, dummy2,quality))

####
#### plotting 
####

plot(x=logPr, y=logQ, col="blue", main="Correlation: \n log(Sales) vs. log(Price)",
     xlab="log(Price)", ylab = "log(Sales)", pch=16)
abline(h=mean(logQ),col="dark blue",lty="dotted")
abline(v=mean(logPr),col="dark blue",lty="dotted")


plot(x=dummy1, y=logQ, col="blue", main="Correlation: \n log(Sales) vs. Yellow Dummy",
     xlab="Yellow Dummy", ylab = "log(Sales)", pch=16)
abline(h=mean(logQ),col="dark blue",lty="dotted")
abline(v=mean(dummy1),col="dark blue",lty="dotted")

plot(x=dummy2, y=logQ, col="blue", main="Correlation: \n log(Sales) vs. Green Dummy",
     xlab="Green  Dummy", ylab = "log(Sales)", pch=16)
abline(h=mean(logQ),col="dark blue",lty="dotted")
abline(v=mean(dummy2),col="dark blue",lty="dotted")

plot(x=quality, y=logQ, col="blue", main="Correlation: \n log(Sales) vs. Quality Index",
     xlab="Quality Index", ylab = "log(Sales)", pch=16)
abline(h=mean(logQ),col="dark blue",lty="dotted")
abline(v=mean(quality),col="dark blue",lty="dotted")

####
#### correlation matrix
####

corrout = cor(cbind(logQ,logPr, dummy1, dummy2,quality))
print(round(corrout,digits=4))

####
#### regression output
####

regout_full = lm(logQ ~ logPr+dummy1+dummy2+quality)
print(summary(regout_full))

regout_short = lm(logQ ~ logPr+dummy1+dummy2)
print(summary(regout_short))