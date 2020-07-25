#Class code for 7/16.


###########################
#Doing Fisher's exact test.
###########################

mat=matrix(c(1,3,3,3),2,2)

fisher.test(mat) #For a two-tailed test.
fisher.test(mat,alternative='less') #For a one-tailed test.


######################
#Doing Theil's method.
######################

library(NSM3) #Package associated with the textbook.  Slow to load.

x=c(1:4,10,12)
y=c(9,15,19,20,45,55)

plot(x,y)

theil(x,y,slopes=T,alpha=0.06) #Reproducing our result from the notes.

out=lm(y~x) #The usual least squares analysis.

summary(out)

b1hat=4.03
se=0.1453
c(b1hat-qt(0.975,4)*se,b1hat+qt(0.975,4)*se) #(3.62,4.43)



####################################################
#Finding location estimates using robust estimators.
####################################################

library(MASS) #Loading a needed package.

x=c(1:10,100) #Data set with an outlier.

hist(x)

mean(x)

median(x)

rlm(x~1,psi=psi.huber) #Fitting models with just an intercept.

rlm(x~1,psi=psi.hampel)

rlm(x~1,psi=psi.bisquare)

lm(x~1) #Standard least squares.


###################
#Robust regression.
###################

############################
#Example with one predictor.

x=1:10
y=c(1,2,3,4,5,6,7,80,9,10)

plot(x,y)

fit1=lm(y~x)

lines(x,fit1$fitted.values,lty=1)

influence.measures(fit1)

fit2=rlm(y~x)

summary(fit2)

lines(x,fit2$fitted.values,lty=2)

#############################
#Example with two predictors.

set.seed(0)
x=runif(20,0,10)
y=runif(20,0,10)
z=x+y+rnorm(20) #True linear relationship is E[z]=x+y.
z[20]=z[20]+20 #Introducing an outlier.

cbind(x,y,z)

fit1=lm(z~x+y)

summary(fit1)

influence.measures(fit1)

fit2=rlm(z~x+y)

summary(fit2)

fit3=lm(z[-20]~x[-20]+y[-20]) #Removing the outlier.

summary(fit3)

#Using rlm is a bit like automatic outlier removal.  


##########################################################
#Comparing the Wald and Agresti-Coull confidence intervals
# for a binomial confidence interval.  Is it really true
# that the A-C intervals perform a lot bettter?
##########################################################

#Here we use simulation.



#Here we use direct calculations of the coverage probabilities.




