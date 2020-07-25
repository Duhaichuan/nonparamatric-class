#Class code for 7/9/20.


##############################
#Two-sample bootstrap example.
##############################




##############################
#Reading in the baseball data.
##############################

data=read.table("http://www19.homepage.villanova.edu/jesse.frey/Math8452/ba2004np.txt")

ab=data$V1 #Number of at bats or attempts.
h=data$V2 #Number of hits or successes.  

hist(ab)

###########################
#Kernel density estimation.
###########################

#Different kernels.

par(mfrow=c(2,2))

fit1=density(ab,kernel='gaussian') #Using default bandwidth.
fit2=density(ab,kernel='epanechnikov')
fit3=density(ab,kernel='rectangular')
fit4=density(ab,kernel='biweight')

plot(fit1,main='Gaussian')
plot(fit2,main='Epanechnikov')
plot(fit3,main='Rectangular')
plot(fit4,main='Biweight')

#Looking at the kernels.

plot(density(c(0),kernel='gaussian',bw=1),main='Gaussian')
plot(density(c(0),kernel='epanechnikov',bw=1),main='Epanechnikov')
plot(density(c(0),kernel='rectangular',bw=1),main='Rectangular')
plot(density(c(0),kernel='biweight',bw=1),main='Biweight')

#Different bandwidths.  What will happen with extreme bandwidths?

plot(density(ab,kernel='gaussian',bw=0.1),main='')
plot(density(ab,kernel='gaussian',bw=25),main='')
plot(density(ab,kernel='gaussian',bw=50),main='')
plot(density(ab,kernel='gaussian',bw=1000000),main='')

#One possible bandwidth.

1.06*sd(ab)/length(ab)^0.2

#What happens if the numbers are different and the bandwidth is
# really really really really really small?

dev.off()

data=1:10
plot(density(data,kernel='gaussian',bw=0.1))



###################
#Kernel regression.
###################

###################################
#Looking at how the weights behave.

x=c(1,3,8)
y=c(0,1,0)

xvec=seq(-5,15,length=1000)
vec1=dnorm(xvec-x[1],0,2) #Choose to use sigma=2.  
vec2=dnorm(xvec-x[2],0,2)
vec3=dnorm(xvec-x[3],0,2)
tot=vec1+vec2+vec3
w1=vec1/tot
w2=vec2/tot
w3=vec3/tot

par(mfrow=c(2,1))

plot(xvec,w1,type='l',ylim=c(0,1),xlab='x values',ylab='weights')
lines(xvec,w2)
lines(xvec,w3)

plot(x,y,main='BW=5.4',cex=0.5,xlim=c(-5,15))
lines(ksmooth(x,y,kernel='normal',bandwidth=5.4,range.x=c(-5,15)))


################################
#Working with the baseball data.

#Reading in the data.

data=read.table("http://www19.homepage.villanova.edu/jesse.frey/Math8452/ba2004np.txt")

ab=data$V1
h=data$V2

ba=h/ab #Empirical success rate.

#Now doing the regression.

par(mfrow=c(1,1))

plot(ab,ba)

out=lm(ba~ab)

#Raw plot with best fit line.

plot(ab,ba)
abline(0.198464,0.000174)

#Plot of residuals against x values.

plot(ab,ba-0.198464-0.000174*ab,ylab='Residual')

#Trying out different bandwidths.

par(mfrow=c(3,2))

plot(ab,ba,main='BW=50',cex=0.5)
lines(ksmooth(ab,ba,kernel='normal',bandwidth=50))

plot(ab,ba,main='BW=100',cex=0.5)
lines(ksmooth(ab,ba,kernel='normal',bandwidth=100))

plot(ab,ba,main='BW=200',cex=0.5)
lines(ksmooth(ab,ba,kernel='normal',bandwidth=200))

plot(ab,ba,main='BW=400',cex=0.5)
lines(ksmooth(ab,ba,kernel='normal',bandwidth=400))

plot(ab,ba,main='BW=40000',cex=0.5)
lines(ksmooth(ab,ba,kernel='normal',bandwidth=40000))

plot(ab,ba,main='BW=1',cex=0.5)
lines(ksmooth(ab,ba,kernel='normal',bandwidth=1))

#Plotting just BW=50 and BW=100.

par(mfrow=c(2,1))

plot(ab,ba,main='BW=50',cex=0.5)
lines(ksmooth(ab,ba,kernel='normal',bandwidth=50))

plot(ab,ba,main='BW=100',cex=0.5)
lines(ksmooth(ab,ba,kernel='normal',bandwidth=100))


#Fitting a model with two connected linear pieces.

ab200=pmax(0,ab-200) #Allows the slope to change for ab>200. 

lm(ba~ab+ab200)

xseq=0:700
yseq=0.1760605+0.0004355*xseq-0.0003794*pmax(0,xseq-200)

dev.off()
plot(ab,ba,cex=0.2)
lines(xseq,yseq)
lines(ksmooth(ab,ba,kernel='normal',bandwidth=100),lty=2)


#####################################
#Doing a permutation chi-square test.
#####################################

data=matrix(0,2,3)
data[1,]=c(2,0,1)
data[2,]=c(1,1,0)

chisq.test(data) #Using the asymptotic approximation.

chisq.test(data,simulate.p.value=T) #Doing the permutation test.

data2=matrix(c(3,0,0,1,0,1),2,3)

chisq.test(data2)

chisq.test(data2,simulate.p.value=T)





