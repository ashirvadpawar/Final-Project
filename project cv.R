## Test for independence of Mobile Addiction and Gender :
x=c(48,44,21,23,12,12)
m=matrix(x,nrow=2,ncol=3)
m
chisq.test(m)


## Test for Independence of Mobile Addiction & Age:
x=c(77,15,39,5,20,4)
m=matrix(x,nrow=2,ncol=3)
m
chisq.test(m)
x=c(77,15,39,5,20,4)
m=matrix(x,nrow=2,ncol=3)
m
chisq.test(m)


##Test for Independence of Mobile Addiction & Profession:
x=c(70, 22,35,9,19,5)
m=matrix(x,nrow=2,ncol=3)
m
chisq.test(m)


## Test for normality (Required for paired t-test) :
x=c(-5,73,-7,-81,28,2,99,-15,33,36,-33,79,-1,20,22,-58,-54,9,15,33,-17,7,-14,-
      64,9,101,12,10,105,47,36,79,5,-5,-10,45,-5,6,2,18)
x
shapiro.test(x)


## Paired t-test :
x=c(158,306,117,177,103,51,215,185,142,315,157,262,121,133,126,99,321,74,
    131,179,126,83,261,159,129,386,142,99,193,100,292,189,51,100,201,276,248,
    117,93,840)
x
y=c(163,233,124,258,75,49,116,200,109,279,190,183,122,113,104,157,375,65,
    116,146,143,76,365,223,120,285,130,89,88,53,256,110,46,105,211,231,253,111,91,66)
y
t.test(x,y,paired=T,conf.level=0.95,alt="g")


## Alternative way ( Wilcoxon Signed Rank Test ):
d=c(-5,73,-7,-81,28,2,99,-15,33,36,-33,79,-1,20,22,-58,-54,9,15,33,-
        17,7,-14,-64,9,101,12,10,105,47,36,79,5,-5,-10,45,-5,6,2,18)
d
wilcox.test(d,mu=0,alter="greater")


## Fitting of Multiple Logistic Regression Model : 
data=read.csv("c:/users/My PC/Downloads/data.csv",header=T)
View(data)
model=glm(Y~X1+X2+X3+X4+X5+X6+X7+X8,data=data)
model
summary(model)


## Best Fitted Model :
Bestmodel=glm(Y~X3+X4+X6+X7+X8,data=data)
Bestmodel
summary(Bestmodel)
