#Class code for 7/1/20.

#########################
#First bootstrap example.
#########################

set.seed(0)
samp=c(3,4,4,5,7,9,11,13,14,19)
n=length(samp)
B=1000 #Number of bootstrap samples to draw.
sdvec=double(B) #Saving the bootstrap standard deviations.
medvec=double(B) #Saving the bootstrap sample medians.
sd=sd(samp) #Observed standard deviation.
med=median(samp) #Observed sample median.
for (brun in 1:B){
  bsamp=sample(samp,n,replace=T) #Sample with replacement from the sample., 从sample里面取10次
  medvec[brun]=median(bsamp)
  sdvec[brun]=sd(bsamp)}

hist(medvec) #Plotting the bootstrap medians.
abline(v=med)

hist(sdvec) #Plotting the bootstrap standard deviations.
abline(v=sd)

#Now studying the median.

ehat=mean(medvec) #Mean of the bootstrap values.
varhat=var(medvec) #Estimated variance.
msehat=mean((medvec-med)^2) #Estimated MSE.
se=sqrt(msehat) #Bootstrap SE.
bhat=ehat-med #Estimated bias.
ehat
varhat
msehat
se
bhat

#Now studying the standard deviation.

ehat=mean(sdvec) #Mean of the bootstrap values.
varhat=var(sdvec) #Estimated variance.
msehat=mean((sdvec-sd)^2) #Estimated MSE.
se=sqrt(msehat) #Bootstrap SE.
bhat=ehat-sd #Estimated bias.
ehat
varhat
msehat
se
bhat


#######################################################
#Now getting bootstrap percentile confidence intervals.
#######################################################

set.seed(0)
samp=c(3,4,4,5,7,9,11,13,14,19)
n=length(samp)
B=1000 #Number of bootstrap samples to draw.
sdvec=double(B) #Saving the bootstrap standard deviations.
medvec=double(B) #Saving the bootstrap sample medians.
meanvec=double(B) #Saving the bootstrap sample means.
sd=sd(samp) #Observed standard deviation.
med=median(samp) #Observed sample median.
for (brun in 1:B){
  bsamp=sample(samp,n,replace=T) #Sample with replacement from the sample.
  medvec[brun]=median(bsamp)
  sdvec[brun]=sd(bsamp)
  meanvec[brun]=mean(bsamp)}

#95% CI for the population median.

quantile(medvec,c(0.025,0.975))

hist(medvec)
abline(v=quantile(medvec,0.025))
abline(v=quantile(medvec,0.975))

#95% CI for the population standard deviation.

quantile(sdvec,c(0.025,0.975))

hist(sdvec)
abline(v=quantile(sdvec,0.025))
abline(v=quantile(sdvec,0.975))

#95% for the population mean.

quantile(meanvec,c(0.025,0.975))

t.test(samp,conf.int=T)


################################################
#Now examining the  << performance>> of the percentile 
# confidence intervals for the-----> population mean.
################################################

#First for normal data.

set.seed(0)
B=1000
nruns=1000 #Number of runs to do.  
n=40 #Sample size.
ct=0 #Count the number of times the interval is correct.
for (run in 1:nruns){
  samp=rnorm(n) #Generating the sample. The true mean is 0. 
  meanvec=double(B) #Storing the bootstrap sample means.
  for (brun in 1:B){
    bsamp=sample(samp,n,replace=T)
    meanvec[brun]=mean(bsamp)}
  ci=quantile(meanvec,c(0.025,0.975)) #Finding the percentile CI.
  if (ci[1]<0 & ci[2]>0){ct=ct+1}}  # 因为norma的mean在0啊

ct/nruns #Estimated coverage probability.

#n=5: 0.861
#n=10: 0.900
#n=20: 0.940
#n=40: 0.934

#Second for exponential data.

set.seed(0)
B=1000
nruns=1000 #Number of runs to do.  
n=40 #Sample size.
ct=0 #Count the number of times the interval is correct.
for (run in 1:nruns){
  samp=rexp(n) #Generating the sample. The true mean is 1. 
  meanvec=double(B) #Storing the bootstrap sample means.
  for (brun in 1:B){
    bsamp=sample(samp,n,replace=T)
    meanvec[brun]=mean(bsamp)}
  ci=quantile(meanvec,c(0.025,0.975)) #Finding the percentile CI.
  if (ci[1]<1 & ci[2]>1){ct=ct+1}}
ct/nruns #Estimated coverage probability.

#n=5: 0.793
#n=10: 0.857
#n=20: 0.915
#n=40: 0.914


####################################################
#Now finding bootstrap pivotal confidence intervals.
####################################################

set.seed(0)
samp=c(3,4,4,5,7,9,11,13,14,19)
n=length(samp)
B=1000 #Number of bootstrap samples to draw.
meanvec=double(B) #Saving the bootstrap sample means.
varvec=double(B) #Saving the bootstrap sample variances.
tvec=double(B) #Storing the bootstrap t values.
x2vec=double(B) #Storing the bootstrap X^2 values.
xbar=mean(samp)
s2=var(samp)
for (brun in 1:B){
  bsamp=sample(samp,n,replace=T) #Sample with replacement from the sample.
  meanvec[brun]=mean(bsamp)
  varvec[brun]=var(bsamp)
  tvec[brun]=(mean(bsamp)-xbar)/(sd(bsamp)/sqrt(n))
  x2vec[brun]=(n-1)*var(bsamp)/s2}

#First find the percentile intervals.

quantile(meanvec,c(0.025,0.975)) #For the mean.
quantile(varvec,c(0.025,0.975)) #For the variance.

#Now the pivotal intervals.

tcrit=quantile(tvec,c(0.025,0.975))
x2crit=quantile(x2vec,c(0.025,0.975))
c(xbar-tcrit[2]*sqrt(s2)/sqrt(n),xbar-tcrit[1]*sqrt(s2)/sqrt(n)) #For the mean.
c((n-1)*s2/x2crit[2],(n-1)*s2/x2crit[1]) #For the variance.

#Normal-theory intervals.

t.test(samp,conf.int=T)
c((n-1)*s2/qchisq(0.975,n-1),(n-1)*s2/qchisq(0.025,n-1))


####################################################################
#Comparing the performance of percentile, pivotal, and normal-theory
# confidence intervals for the population variance.
####################################################################

#First for normal data.

set.seed(0)
B=1000
nruns=1000 #Number of runs to do.  
n=20 #Sample size.
ct1=0 #For the percentile CI.
ct2=0 #For the pivotal CI.
ct3=0 #For the normal-theory CI.
for (run in 1:nruns){
  samp=rnorm(n) #Generating the sample. 
  varvec=double(B) #Saving the bootstrap sample variances.
  x2vec=double(B) #Storing the bootstrap X^2 values.
  s2=var(samp)
  for (brun in 1:B){
    bsamp=sample(samp,n,replace=T) #Sample with replacement from the sample.
    varvec[brun]=var(bsamp)
    x2vec[brun]=(n-1)*var(bsamp)/s2}
  ci1=quantile(varvec,c(0.025,0.975)) #Percentile CI.
  x2crit=quantile(x2vec,c(0.025,0.975)) 
  ci2=c((n-1)*s2/x2crit[2],(n-1)*s2/x2crit[1]) #Pivotal CI.
  ci3=c((n-1)*s2/qchisq(0.975,n-1),(n-1)*s2/qchisq(0.025,n-1)) #Normal theory CI.
  if (ci1[1]<1 & ci1[2]>1){ct1=ct1+1}
  if (ci2[1]<1 & ci2[2]>1){ct2=ct2+1}
  if (ci3[1]<1 & ci3[2]>1){ct3=ct3+1}}

ct1/nruns
ct2/nruns
ct3/nruns

#n=5: 0.637,0.832,0.950
#n=10: 0.794,0.874,0.959
#n=20: 0.854,0.898,0.958 

#Now for exponential data.

set.seed(0)
B=1000
nruns=1000 #Number of runs to do.  
n=20 #Sample size.
ct1=0 #For the percentile CI.
ct2=0 #For the pivotal CI.
ct3=0 #For the normal-theory CI.
for (run in 1:nruns){
  samp=rexp(n) #Generating the sample. 
  varvec=double(B) #Saving the bootstrap sample variances.
  x2vec=double(B) #Storing the bootstrap X^2 values.
  s2=var(samp)
  for (brun in 1:B){
    bsamp=sample(samp,n,replace=T) #Sample with replacement from the sample.
    varvec[brun]=var(bsamp)
    x2vec[brun]=(n-1)*var(bsamp)/s2}
  ci1=quantile(varvec,c(0.025,0.975)) #Percentile CI.
  x2crit=quantile(x2vec,c(0.025,0.975)) 
  ci2=c((n-1)*s2/x2crit[2],(n-1)*s2/x2crit[1]) #Pivotal CI.
  ci3=c((n-1)*s2/qchisq(0.975,n-1),(n-1)*s2/qchisq(0.025,n-1)) #Normal theory CI.
  if (ci1[1]<1 & ci1[2]>1){ct1=ct1+1}
  if (ci2[1]<1 & ci2[2]>1){ct2=ct2+1}
  if (ci3[1]<1 & ci3[2]>1){ct3=ct3+1}}

ct1/nruns
ct2/nruns
ct3/nruns

#n=5: 0.464,0.773,0.818 
#n=10: 0.595,0.711,0.740
#n=20: 0.713,0.805,0.745


############################################
#Bivariate bootstrap example from the notes. 
############################################

set.seed(0)
B=1000 #Number of bootstrap runs.
mothers=c(70,69,65,64,66,65,64,66,60,70)
daughters=c(67,64,62,64,69,70,65,66,63,74)
n=length(mothers)
rvec=double(B)
r=cor(mothers,daughters) #Observed sample correlation. 
for (brun in 1:B){
  pairs=sample(1:n,n,replace=T)
  bmothers=mothers[pairs]
  bdaughters=daughters[pairs]
  rvec[brun]=cor(bmothers,bdaughters)}

hist(rvec)
abline(v=r)

r #Point estimate: 0.53

msehat=mean((rvec-r)^2)
sqrt(msehat) #SE 0.235

ehat=mean(rvec)
ehat-r #Estimated bias -0.010

qusampleantile(rvec,c(0.025,0.975)) #(-0.05,0.89) is the 95% bootstrap percentile CI. 

