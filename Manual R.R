#Mr. Siddha Raj Bhatta 
#E-mail : Siddhabhatta@gmail.com
#Manual Calculation of Regression Results in R
# This dataset has been Taken from "An Introduction to Econometrics by Christopher Doughetry

datafile<- read.csv("https://sites.google.com/site/siddhabhatta/data/data3.csv", header=T)

#beta=(X'X)^-1*X'Y
attach(datafile)
y = wage_rate 
X = cbind (1, yearsedu, exp, ASVABC)
y
X
Xt=t(X)#transpose of X matrix 
A=Xt%*%X #calculation of X transpose X 
B=Xt%*%y # calculation of X transpose Y 
Ainv=solve(A)# Calculating the inverse of X transpose X 
beta1=Ainv%*%B # Solving the equation beta=(X'X)-1*X'y
beta1
print(t(beta1), digits=7) # print the coefficient vector 

model<-lm(wage_rate~yearsedu+exp+ASVABC)
coefficients(model)

#single command
reg<-solve(t(X) %*% X) %*% (t(X) %*% y)
print(t(reg))
#------------------------------------------------------
# predicted values and residuals 
yhat=X%*%beta1 # yhat = Xbeta 

uhat=y-yhat # calculates error term 
RSS=sum(uhat^2) # 
t(uhat) %*% uhat
RSS
#---------------------------------------------------------
#manually calculate R squared 
TSS<-sum((wage_rate-mean(wage_rate))^2)
R2<-1-RSS/TSS
R2
round(R2,3)
#---------------------------------------------------
#SER Manually 
n<-nrow(datafile)
n
SER<-sqrt((RSS)/(n-ncol(X)))
SER 
SER^2
vare<-RSS/(n-ncol(X))
ncol(X)
summary(model)
vare
#------------------------------------------------------
#Standard Error of Regression Coefficients
varcor<-vare*solve((t(X) %*% X)) # var(e)(X'X)-1
varcor
stderror<-sqrt(diag(varcor))
beta1
stderror
t(stderror)
print(cbind(beta1, stderror))
summary(model)

#---------------------------------------------------------
#confidence intervals 
confifl=beta1-1.96*stderror
confifl
coinfifu=beta1+1.96*stderror
coinfifu
cbind(beta1,confifl, coinfifu)

confint(model) # CI from lm command 

#----------------------------------------------------
t<-beta1/stderror # beta/se
t
summary(model)

1-pt(5, 100)
1-pt(7.000, 539)
round(prob,7)

prob<-round(1-pt(t, df=(nrow(X)-1)), 7)
prob<-round(1-pt(abs(t), df=(nrow(X)-1)), 6)
prob

summary(model)

cbind(beta1, stderror, t, prob)

#-----------------------------------------------------------
#ANOVA test 

ESS<-TSS-RSS
MSE<-ESS/(ncol(X)-1)# no of parameter-1
MSR<-RSS/(nrow(X)-ncol(X)) # n-no of parameter
F<-MSE/MSR
F

# probability 
1-pf(2, 2, 5)
1-pf(F, (ncol(X)-1), (nrow(X)-ncol(X))) # k-1 and n-k are degrees of freedom for 
#numerator and denominator  # k=no of parameter.


