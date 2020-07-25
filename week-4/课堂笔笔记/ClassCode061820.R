#Code for class on 6/18/20.


##############################
#Some packages that we need.
#They must be installed first.
##############################

library(clinfun)
library(jmuOutlier) 
library(NSM3) #Associated with the textbook.
library(lsmeans) #For some of the ANOVA functions.


#############################
#Running k-sample tests in R.
#############################

#Here is the example from the class notes.

data=c(13,24.1,11.7,16.3,15.5,42,18,14,36,11.6,15.6,23.8,24.4,24)
groups=rep(1:3,c(5,5,4)) #Specifying which values are for which samples.

kruskal.test(data,groups) #Kruskal-Wallis test.

perm.f.test(data,groups) #Permutation F test and ANOVA F test. (jmuOutlier)

jonckheere.test(data,groups) #Jonckheere-Terpstra test. (clinfun)


#Alternate way of running the Kruskal-Wallis test.

x=c(13,24.1,11.7,16.3,15.5)
y=c(42,18,14,36,11.6)
z=c(15.6,23.8,24.4,24)

kruskal.test(list(x,y,z))


#######################################################
#Comparing the ANOVA F test and the Kruskal-Wallis test
# in terms of control of alpha and power.
#######################################################

########################################
#Code for getting ANOVA F test p-values:
#test=aov(data~factor(groups))
#summary(test)[[1]][["Pr(>F)"]][1]

#######################################
#How well does each test control alpha?
#Let's use equal sample sizes.

#For normal data.

##################
#Power comparison. 

#For normal data.



####################################################
#Studying the power of the Jonckheere-Terpstra test.
#What kinds of alternatives is it sensitive to?
####################################################




##########################################
#For testing the theory that Delta is 100.
#This is data from the notes.
##########################################

x=c(110,111,127,131)
y=c(12,25,33,42)

wilcox.test(x-100,y)

wilcox.test(x,y,mu=100)


############################################
#Confidence interval example from the notes.
############################################

x=c(5,7,10,11,16)
y=c(4,6)

wilcox.test(x,y,conf.int=T,conf.level=0.9)

wilcox.test(x,y,conf.int=T,conf.level=0.95) #Not available due to the small sample sizes.


################################################
#Confidence intervals for the shift parameter in 
# the two-sample problem.
################################################

set.seed(0)

x=rnorm(5)
y=rnorm(5)

wilcox.test(x,y,conf.int=T)

#Making a plot of the p-values.

t=seq(-4,4,by=0.01)
vec=double(length(t))
for (i in 1:length(t)){
  vec[i]=wilcox.test(x-t[i],y)$p.value}

plot(t,vec,type='l',xlab='Delta_0',ylab='p-value')
abline(h=0.05,lty=2) #The intersection points determine the CI.

#Finding all the differences.

diffvec=double(length(x)*length(y))
ct=0
for (i in 1:length(x)){
  for (j in 1:length(y)){
    ct=ct+1
    diffvec[ct]=x[i]-y[j]}}

sort(diffvec)

#The CI goes from the 3rd to the 23rd ordered difference.  









