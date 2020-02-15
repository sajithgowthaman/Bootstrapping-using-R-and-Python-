install.packages("bootstrap") 
install.packages("boot") #this packages contains
#the 82 schools 
library(boot)
library(bootstrap)
law82
law
pop_corr = cor(law82$LSAT, law82$GPA)
pop_corr
sample_corr = cor(law$LSAT, law$GPA)
sample_corr
#Create a function to calculate the corr coef of the sample
theta.hat=function(i)
{
  cor(law[i,1],law[i,2])
}
theta.hat(1:15) #test the function to make sure it works

corr_bs=bootstrap(1:15, 1000, theta.hat)
mean(corr_bs$thetastar) #This gives the estimate
#of the bootstrapped corr coef

quantile(corr_bs$thetastar, c(0.16, 0.82))
hist(corr_bs$thetastar)
sd(corr_bs$thetastar)


mymedf=function(x)
{
  sample_med=median(x)
  return(sample_med)
}

x = rnorm(100,100, 5)
mymedf(x)

  
  
  
  
  