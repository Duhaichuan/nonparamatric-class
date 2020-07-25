####################################################################
#Comparing the power for the rank sum, normal scores, and K-S tests.
####################################################################
library(snpar)
library(jmuOutlier) #For doing permutation tests.

#Normal without a location shift.
set.seed(58)
n=10 #Commmon sample size.
nruns=1000
nrej1=0
nrej2=0
for (run in 1:nruns){
  samp1=rnorm(n)
  samp2=rnorm(n)
  if (wilcox.test(samp1,samp2)$p.value<0.05){nrej1=nrej1+1}
  if (ks.test(samp1,samp2)$p.value<0.05){nrej2=nrej2+1}}
nrej1/nruns
nrej2/nruns

print(paste("Estimated alpha for Rank Sum is:", nrej1/nruns))
print(paste("Estimated alpha for Kolmogorov-Smirnow is:", nrej2/nruns))
samp1
samp2




set.seed(0)
n=10 #Common sample size.
nruns=1000 #Number of runs to do.
nrej1=0 #For Ansari-Bradley test.
nrej2=0 #For Kolmogorov test.
for (run in 1:nruns){
  samp1=rnorm(n)
  samp2=rnorm(n)
  if (ansari.test(samp1,samp2)$p.value<0.05){nrej1=nrej1+1}
  if (wilcox.test(samp1,samp2)$p.value<0.05){nrej2=nrej2+1}}
nrej1/nruns
nrej2/nruns
print(paste("Estimated alpha for Kolmogorov-Smirnow is:", nrej1/nruns))
print(paste("Estimated alpha for Rank Sum is:", nrej2/nruns))
samp1
samp2
