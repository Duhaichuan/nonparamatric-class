#Code for class on 6/3/20.

library(BSDA) #For running the sign test.


#######################################################
#Using SIGN.test to get CIs for the population median.
#By default, it reports an interpolated interval, but
# also gives the intervals that were used in the 
# interpolation process.  It does linear interpolation.

set.seed(0)
x=rnorm(10)
sort(x) #Printing out the ordered values.
SIGN.test(x)


##############################
#Running the signed rank test.

set.seed(0)
x=rnorm(20) #Generating a sample.
wilcox.test(x) #
wilcox.test(x,exact=T)

sum(rank(abs(x))[x>0]) #Computing the test statistic from the definition.


########################
#Example from the notes.

x=c(5,7,-4)
wilcox.test(x)
wilcox.test(x,alternative='greater')
wilcox.test(x,alternative='less')


#####################################################
#Now comparing the three tests. Which has better power?
#Do they correctly maintain their alpha level?
#Modify this code to bring in the signed rank test!

#Normal case.

#First under the null hypothesis.

set.seed(0) #To make the code repeatable.
nruns=1000 #Number of random samples to generate.
nrej1=0 #Counting the number of rejections for each test. 
nrej2=0
nrej3=0 #For signed rank test.
for (run in 1:nruns){
  x=rnorm(20) #Random sample of size 20.
  if (t.test(x)$p.value<0.05){nrej1=nrej1+1}
  if (SIGN.test(x)$p.value<0.05){nrej2=nrej2+1}
  if (wilcox.test(x)$p.value<0.05){nrej3=nrej3+1}}
nrej1/nruns #Estimated alpha.
nrej2/nruns
nrej3/nruns #For signed rank test.

#Now under an alternative.  Here we're interested in the power.

shift=0.5 #Here we use a shift alternative.
set.seed(1) #To make the code repeatable.
nruns=1000
nrej1=0
nrej2=0
nrej3=0 #For signed rank test.
for (run in 1:nruns){
  x=rnorm(20,shift,1) #Random sample of size 20.
  if (t.test(x)$p.value<0.05){nrej1=nrej1+1}
  if (SIGN.test(x)$p.value<0.05){nrej2=nrej2+1}
  if (wilcox.test(x)$p.value<0.05){nrej3=nrej3+1}}
nrej1/nruns #Estimated power.
nrej2/nruns
nrej3/nruns


#Cauchy case.

#First under the null hypothesis.


set.seed(0) #To make the code repeatable.
nruns=1000 #Number of random samples to generate.
nrej1=0 #Counting the number of rejections for each test. 
nrej2=0
nrej3=0 #For signed rank test.
for (run in 1:nruns){
  x=rcauchy(20) #Random sample of size 20.
  if (t.test(x)$p.value<0.05){nrej1=nrej1+1}
  if (SIGN.test(x)$p.value<0.05){nrej2=nrej2+1}
  if (wilcox.test(x)$p.value<0.05){nrej3=nrej3+1}}
nrej1/nruns #Estimated alpha.
nrej2/nruns
nrej3/nruns #For signed rank test.

#Now under an alternative.  Here we're interested in the power.

shift=0.5 #Here we use a shift alternative.
set.seed(1) #To make the code repeatable.
nruns=1000
nrej1=0
nrej2=0
nrej3=0 #For signed rank test.
for (run in 1:nruns){
  x=rcauchy(20)+shift #Random sample of size 20.
  if (t.test(x)$p.value<0.05){nrej1=nrej1+1}
  if (SIGN.test(x)$p.value<0.05){nrej2=nrej2+1}
  if (wilcox.test(x)$p.value<0.05){nrej3=nrej3+1}}
nrej1/nruns #Estimated power.
nrej2/nruns
nrej3/nruns


#t with 3 df case.

#First under the null hypothesis.

set.seed(0) #To make the code repeatable.
nruns=1000 #Number of random samples to generate.
nrej1=0 #Counting the number of rejections for each test. 
nrej2=0
nrej3=0 #For signed rank test.
for (run in 1:nruns){
  x=rt(20,3) #Random sample of size 20.
  if (t.test(x)$p.value<0.05){nrej1=nrej1+1}
  if (SIGN.test(x)$p.value<0.05){nrej2=nrej2+1}
  if (wilcox.test(x)$p.value<0.05){nrej3=nrej3+1}}
nrej1/nruns #Estimated alpha.
nrej2/nruns
nrej3/nruns #For signed rank test.

#Now under an alternative.  Here we're interested in the power.

shift=0.5 #Here we use a shift alternative.
set.seed(1) #To make the code repeatable.
nruns=1000
nrej1=0
nrej2=0
nrej3=0 #For signed rank test.
for (run in 1:nruns){
  x=rt(20,3)+shift #Random sample of size 20.
  if (t.test(x)$p.value<0.05){nrej1=nrej1+1}
  if (SIGN.test(x)$p.value<0.05){nrej2=nrej2+1}
  if (wilcox.test(x)$p.value<0.05){nrej3=nrej3+1}}
nrej1/nruns #Estimated power.
nrej2/nruns
nrej3/nruns


#######################################################
#Writing our own function for doing a permutation test.
#######################################################

#Getting the code to work.

set.seed(0) 
x=rnorm(20) #Generating a sample.
nullmedian=0 #This is theta_H.
nperms=1000
store=double(nperms) 
xdiff=x-nullmedian #The differences we need.
obs=mean(xdiff) #Observed value for test statistic.
for (perm in 1:nperms){
  rperm=xdiff*sample(c(-1,1),20,replace=T) #The random permutation.
  store[perm]=mean(rperm)}
lower=(1+sum(store<=obs))/(1+nperms)
upper=(1+sum(store>=obs))/(1+nperms)
twotailed=min(1,2*min(lower,upper))
lower
upper
twotailed

hist(store)
abline(v=obs)

#Now making a function.

perm8452=function(x,nullmedian,nperms){
  
  store=double(nperms) 
  
  n=length(x) #Finding the sample size.
  xdiff=x-nullmedian #The differences we need.
  obs=mean(xdiff) #Observed value for test statistic.
  for (perm in 1:nperms){
    rperm=xdiff*sample(c(-1,1),n,replace=T) #The random permutation.
    store[perm]=mean(rperm)}
  lower=(1+sum(store<=obs))/(1+nperms)
  upper=(1+sum(store>=obs))/(1+nperms)
  twotailed=min(1,2*min(lower,upper))
  
  list(lower=lower,upper=upper,twotailed=twotailed)}


set.seed(0) 
x=rnorm(20) #Generating a sample.
nullmedian=0 #This is theta_H.
nperms=1000

perm8452(x,nullmedian,nperms)


#####################################
#Running the paired tests from class.
#####################################

x=c(-2,-3,-1,-4,6,-5)

SIGN.test(x)
t.test(x)
wilcox.test(x)
perm8452(x,0,1000)


##########################################################
#Power and robustness comparison for the rank sum test
# and the two-sample t test.  We'll use the pooled t test.
##########################################################

#############
#Normal case.

#Is the alpha level right?

set.seed(0)
n=20 #Sample size to use.
nruns=1000 #Number of runs to do.
nrej1=0 #For the t test.
nrej2=0 #For the rank sum test.
for (run in 1:nruns){
  samp1=rnorm(n) #Sample of size n.
  samp2=rnorm(n) #Second sample of size n.
  if (t.test(samp1,samp2,var.equal=T)$p.value<0.05){nrej1=nrej1+1}
  if (wilcox.test(samp1,samp2)$p.value<0.05){nrej2=nrej2+1}}
nrej1/nruns
nrej2/nruns

#How does the power compare?

shift=0.5
set.seed(0)
n=20 #Sample size to use.
nruns=1000 #Number of runs to do.
nrej1=0 #For the t test.
nrej2=0 #For the rank sum test.
for (run in 1:nruns){
  samp1=rnorm(n) #Sample of size n.
  samp2=rnorm(n)+shift #Second sample of size n.
  if (t.test(samp1,samp2,var.equal=T)$p.value<0.05){nrej1=nrej1+1}
  if (wilcox.test(samp1,samp2)$p.value<0.05){nrej2=nrej2+1}}
nrej1/nruns
nrej2/nruns

#############
#Cauchy case.

#Is the alpha level right?

set.seed(0)
n=20 #Sample size to use.
nruns=1000 #Number of runs to do.
nrej1=0 #For the t test.
nrej2=0 #For the rank sum test.
for (run in 1:nruns){
  samp1=rcauchy(n) #Sample of size n.
  samp2=rcauchy(n) #Second sample of size n.
  if (t.test(samp1,samp2,var.equal=T)$p.value<0.05){nrej1=nrej1+1}
  if (wilcox.test(samp1,samp2)$p.value<0.05){nrej2=nrej2+1}}
nrej1/nruns
nrej2/nruns

#How does the power compare?

shift=0.5
set.seed(0)
n=20 #Sample size to use.
nruns=1000 #Number of runs to do.
nrej1=0 #For the t test.
nrej2=0 #For the rank sum test.
for (run in 1:nruns){
  samp1=rcauchy(n) #Sample of size n.
  samp2=rcauchy(n)+shift #Second sample of size n.
  if (t.test(samp1,samp2,var.equal=T)$p.value<0.05){nrej1=nrej1+1}
  if (wilcox.test(samp1,samp2)$p.value<0.05){nrej2=nrej2+1}}
nrej1/nruns
nrej2/nruns

##############
#Uniform case.

#Is the alpha level right?

set.seed(0)
n=20 #Sample size to use.
nruns=1000 #Number of runs to do.
nrej1=0 #For the t test.
nrej2=0 #For the rank sum test.
for (run in 1:nruns){
  samp1=runif(n) #Sample of size n.
  samp2=runif(n) #Second sample of size n.
  if (t.test(samp1,samp2,var.equal=T)$p.value<0.05){nrej1=nrej1+1}
  if (wilcox.test(samp1,samp2)$p.value<0.05){nrej2=nrej2+1}}
nrej1/nruns
nrej2/nruns

#How does the power compare?

shift=0.25
set.seed(0)
n=20 #Sample size to use.
nruns=1000 #Number of runs to do.
nrej1=0 #For the t test.
nrej2=0 #For the rank sum test.
for (run in 1:nruns){
  samp1=runif(n) #Sample of size n.
  samp2=runif(n)+shift #Second sample of size n.
  if (t.test(samp1,samp2,var.equal=T)$p.value<0.05){nrej1=nrej1+1}
  if (wilcox.test(samp1,samp2)$p.value<0.05){nrej2=nrej2+1}}
nrej1/nruns
nrej2/nruns

##################
#Exponential case.

#Is the alpha level right?

set.seed(0)
n=20 #Sample size to use.
nruns=1000 #Number of runs to do.
nrej1=0 #For the t test.
nrej2=0 #For the rank sum test.
for (run in 1:nruns){
  samp1=rexp(n) #Sample of size n.
  samp2=rexp(n) #Second sample of size n.
  if (t.test(samp1,samp2,var.equal=T)$p.value<0.05){nrej1=nrej1+1}
  if (wilcox.test(samp1,samp2)$p.value<0.05){nrej2=nrej2+1}}
nrej1/nruns
nrej2/nruns

#How does the power compare?

shift=0.5
set.seed(0)
n=20 #Sample size to use.
nruns=1000 #Number of runs to do.
nrej1=0 #For the t test.
nrej2=0 #For the rank sum test.
for (run in 1:nruns){
  samp1=rexp(n) #Sample of size n.
  samp2=rexp(n)+shift #Second sample of size n.
  if (t.test(samp1,samp2,var.equal=T)$p.value<0.05){nrej1=nrej1+1}
  if (wilcox.test(samp1,samp2)$p.value<0.05){nrej2=nrej2+1}}
nrej1/nruns
nrej2/nruns







#################################
#Functions for permutation tests.
#################################

library(jmuOutlier) #For doing permutation tests.

set.seed(0)
x=rnorm(10)
y=rnorm(10)

perm.test(x,y)

perm.test(x,y,stat=median) #Test based on difference in sample medians.

help(perm.test) #For reading about the options.
