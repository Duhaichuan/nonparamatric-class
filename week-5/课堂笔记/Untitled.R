#Class code for 6/24/20.


##############################
#Some packages to load.
#They must be installed first.
##############################

library(clinfun)
library(jmuOutlier) 
library(testforDEP) #Testing for dependency using Hoeffding's test and others.


#############################################################
#Now looking at simultaneous confidence intervals for shifts.
#############################################################

#Making up some data.

x=1:6
y=3:8
z=6:11

##################################
#Now finding Tukey's pairwise CIs.

data=c(x,y,z)
group=rep(1:3,each=6)

fit=aov(data~factor(group)) #Fitting the ANOVA model.

anova(fit) #Displaying the ANOVA table.

TukeyHSD(fit)

#Compare these intervals to the t test confidence intervals.
#What's the difference, and why is there a difference?

t.test(y,x)$conf.int
t.test(z,x)$conf.int
t.test(z,y)$conf.int


########################################
#Now develop the nonparametric approach.
#First simulate to find the appropriate 
# level for the pairwise intervals.
#Second, compute the pairwise intervals 
# for the earlier set of data.

simultaneous8452=function(data,groups,conf.level){
  
  #Note: The groups must be numbered from 1 to k.
  
  k=max(groups) #Finding the number of groups.
  nvec=double(k)
  for (i in 1:k){
    nvec[i]=sum(groups==i)} #Finding the sample sizes.
  nruns=10000
  minalpha=double(nruns)
  N=sum(nvec) #Total sample size.
  for (run in 1:nruns){
    minalpha[run]=1
    simdata=rnorm(N)
    for (i in 1:(k-1)){
      for (j in (i+1):k){
        minalpha[run]=min(minalpha[run],wilcox.test(simdata[groups==i],simdata[groups==j])$p.value)}}}
  crit2=quantile(minalpha,1-conf.level)
  crit=max(minalpha[minalpha<crit2/1.000001])
  cov=1-mean(minalpha<=crit*1.000001)
  
  #Now finding the conservative intervals.
  
  ct=0
  for (i in 1:(k-1)){
    for (j in (i+1):k){
      if (ct==0){mat=c(i,j,wilcox.test(data[groups==i],data[groups==j],conf.int=T,dcl=(1-crit)*1.000001)$conf.int)}
      if (ct>0){mat=rbind(mat,c(i,j,wilcox.test(data[groups==i],data[groups==j],conf.int=T,dcl=(1-crit)*1.000001)$conf.int))}
      ct=ct+1}}
  mat
  
  list(crit=crit,cov=cov,mat=mat)}

#Here's an example without any ties.

set.seed(0)
groups=rep(1:3,each=6)
data=rnorm(length(group))

simultaneous8452(data,groups,0.90)



##################################################
#Tests for treatment differences using block data.
##################################################

#Friedman's test.

dmat=matrix(double(24),6,4) #Creates a matrix of zeros.
dmat[,1]=c(120,208,199,194,177,195) #Data from class notes.
dmat[,2]=c(207,188,181,164,155,175) #Columns correspond to treatments.
dmat[,3]=c(122,137,177,177,160,138) #Rows correspond to blocks.
dmat[,4]=c(128,128,160,142,157,179) #This is different from in the notes!

friedman.test(dmat) #Check that the df is correct!

#Code for runnning a permutation block F test.

set.seed(0)
obs=sum(apply(dmat,2,mean)^2)
nperms=20000
tsvec=double(nperms)
for (perm in 1:nperms){
  dmat2=apply(dmat,1,sample) #Permute the values row by row.  Note that the dimensions for dmat2 differ from those for dmat.
  tsvec[perm]=sum(apply(dmat2,1,mean)^2)}

pvalue=(sum(tsvec>obs-1.0e-10)+1)/(nperms+1)
pvalue

#Code for running the parametric F test.

data=c(dmat[,1],dmat[,2],dmat[,3],dmat[,4]) #The data.
group=rep(1:4,rep(6,4)) #The group labels.
block=rep(1:6,4) #The block labels. 
out=lm(data~factor(group)+factor(block))
anova(out)

#Now plotting the data appropriately.

plot(group,data,xlab='Group',ylab='Value')
for (b in 1:max(block)){
  lines(group[block==b],data[block==b])} #Connecting values from the same block.




#######################
#Tests for association.
#######################

set.seed(0)
x=rnorm(10) #Some data with no ties.
y=rnorm(10)

plot(x,y)

cor.test(x,y,method='pearson')
cor.test(x,y,method='spearman')
cor.test(x,y,method='kendall')

cor.test(x,exp(y),method='pearson') #Different!
cor.test(x,exp(y),method='spearman') #Same as before. 因为这2个only depede on rank 没有用其他的information
cor.test(x,exp(y),method='kendall') #Same as before.

x=c(1,2,2) #Some data with <ties>.
y=c(2,2,3)

perm.cor.test(x,y,method='pearson') #(jmuOutlier)
perm.cor.test(x,y,method='spearman') #(jmuOutlier)

cor.test(x,y,method='pearson')    
cor.test(x,y,method='spearman')  #这种方法和是perm.cor.test相比只是会有waring about tie, 所以有tie的话最好用permn那个， person不care tie无所谓
cor.test(x,y,method='kendall') 

#Hoeffding's test.

x=-6:6 #Creating some data with a non-linear association.
y=x^2

plot(x,y)

perm.cor.test(x,y,method='pearson') #(jmuOutlier)
perm.cor.test(x,y,method='spearman') #(jmuOutlier)

testforDEP(x,y,test='HOEFFD') #Based on simulation. (testforDEP)
testforDEP(x,y,test='PEARSON')


