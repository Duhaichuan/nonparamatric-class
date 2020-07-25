#Code for class on 6/10/20.


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


##################################################
#Writing our own function to do permutation tests.
##################################################

#First getting the code to work.

set.seed(0)

x=rnorm(10)
y=rnorm(10)

nruns=1000
tsvec=double(nruns) #Storing test statistic values.

obs=mean(x)-mean(y) #Observed test statistic value.
combined=c(x,y) #Combined samples.
m=length(x) #Sample sizes.
n=length(y)
for (run in 1:nruns){
  new=sample(combined)
  newx=new[1:m] #Permutation x sample.
  newy=new[(m+1):(m+n)] #Permutation y sample.  
  tsvec[run]=mean(newx)-mean(newy)}

upper=(1+sum(tsvec>=obs))/(1+nruns)
lower=(1+sum(tsvec<=obs))/(1+nruns)
twotailed=min(1,2*min(upper,lower))

upper
lower
twotailed

hist(tsvec)
abline(v=obs)


#Now turning this into a function.

perm.test8452=function(x,y,nruns){
  
  tsvec=double(nruns) #Storing test statistic values.
  
  obs=mean(x)-mean(y) #Observed test statistic value. ###Change here if you change the statistic!
  combined=c(x,y) #Combined samples.
  m=length(x) #Sample sizes.
  n=length(y)
  for (run in 1:nruns){
    new=sample(combined)
    newx=new[1:m] #Permutation x sample.
    newy=new[(m+1):(m+n)] #Permutation y sample.  
    tsvec[run]=mean(newx)-mean(newy)} ###Change here if you change the test statistic!
  
  upper=(1+sum(tsvec>=obs))/(1+nruns)
  lower=(1+sum(tsvec<=obs))/(1+nruns)
  twotailed=min(1,2*min(upper,lower))
  
  list(upper=upper,lower=lower,twotailed=twotailed)}


#Trying out the function.

perm.test8452(x,y,1000)




#################################
#Running the Ansari-Bradley test.
#################################

#Example from class.

x=c(-0.53,-0.74,0.02,-0.47,0.44,0.13)
y=c(-5.18,-1.24,-4.66,1.07,-0.98,0.90)

ansari.test(y,x) #The test statistic is 13 as we computed.

var.test(y,x) #Running the normal-theory F test.


##################################################
#Comparing the Ansari-Bradley test and the F test.
##################################################

#Comparing alpha levels.

#For normal data.

set.seed(0)
n=10 #Common sample size.
nruns=1000 #Number of runs to do.
nrej1=0 #For Ansari-Bradley test.
nrej2=0 #For F test.
for (run in 1:nruns){
  samp1=rnorm(n)
  samp2=rnorm(n)
  if (ansari.test(samp1,samp2)$p.value<0.05){nrej1=nrej1+1}
  if (var.test(samp1,samp2)$p.value<0.05){nrej2=nrej2+1}}
nrej1/nruns
nrej2/nruns


#For exponential data.

set.seed(0)
n=10 #Common sample size.
nruns=1000 #Number of runs to do.
nrej1=0 #For Ansari-Bradley test.
nrej2=0 #For F test.
for (run in 1:nruns){
  samp1=rexp(n)
  samp2=rexp(n)
  if (ansari.test(samp1,samp2)$p.value<0.05){nrej1=nrej1+1}
  if (var.test(samp1,samp2)$p.value<0.05){nrej2=nrej2+1}}
nrej1/nruns
nrej2/nruns


#For Cauchy.

set.seed(0)
n=10 #Common sample size.
nruns=1000 #Number of runs to do.
nrej1=0 #For Ansari-Bradley test.
nrej2=0 #For F test.
for (run in 1:nruns){
  samp1=rcauchy(n)
  samp2=rcauchy(n)
  if (ansari.test(samp1,samp2)$p.value<0.05){nrej1=nrej1+1}
  if (var.test(samp1,samp2)$p.value<0.05){nrej2=nrej2+1}}
nrej1/nruns
nrej2/nruns


#Comparing power for normal data.

set.seed(0)

n=10 #Common sample size.
nruns=1000 #Number of runs to do.
nrej1=0 #For Ansari-Bradley test.
nrej2=0 #For F test.
for (run in 1:nruns){
  samp1=rnorm(n)
  samp2=rnorm(n)*2 #Doubling the standard deviation here.
  if (ansari.test(samp1,samp2)$p.value<0.05){nrej1=nrej1+1}
  if (var.test(samp1,samp2)$p.value<0.05){nrej2=nrej2+1}}
nrej1/nruns
nrej2/nruns



################################
#Running the normal scores test.
################################

library(snpar)

set.seed(0)
x=rnorm(10)
y=rnorm(10)

data=c(x,y)
groups=rep(1:2,c(length(x),length(y)))

ns.test(data,groups)


##########################################
#Simulating to estimate the normal scores.
##########################################

set.seed(0)
n=10 #Total sample size.
nruns=1000

mat=matrix(rnorm(n*nruns),nruns,10)
mat=apply(mat,1,sort)

estns=apply(mat,1,mean) #Estimated normal scores.
estns


####################################################################
#Comparing the power for the rank sum, normal scores, and K-S tests.
####################################################################

#Normal with a location shift.

set.seed(0)
shift=0.5
n=20 #Commmon sample size.
nruns=1000
nrej1=0
nrej2=0
nrej3=0
for (run in 1:nruns){
  samp1=rnorm(n)
  samp2=rnorm(n)+shift
  if (wilcox.test(samp1,samp2)$p.value<0.05){nrej1=nrej1+1}
  if (ns.test(c(samp1,samp2),rep(1:2,each=n))$p.value<0.05){nrej2=nrej2+1}
  if (ks.test(samp1,samp2)$p.value<0.05){nrej3=nrej3+1}}
nrej1/nruns
nrej2/nruns
nrej3/nruns

#Normal with a change in scale only（change std）

set.seed(0)
scalefactor=4
n=20 #Commmon sample size.
nruns=1000
nrej1=0
nrej2=0
nrej3=0
for (run in 1:nruns){
  samp1=rnorm(n)
  samp2=rnorm(n)*scalefactor
  if (wilcox.test(samp1,samp2)$p.value<0.05){nrej1=nrej1+1}
  if (ns.test(c(samp1,samp2),rep(1:2,each=n))$p.value<0.05){nrej2=nrej2+1}
  if (ks.test(samp1,samp2)$p.value<0.05){nrej3=nrej3+1}}
nrej1/nruns
nrej2/nruns
nrej3/nruns



