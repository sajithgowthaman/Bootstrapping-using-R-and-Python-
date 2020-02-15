
comp_sf_data=read.table("~/CSUEB/Spring 2019/STAT 641/sf_comp_data.txt")

sample_med=median(comp_sf_data$V1)
print(sample_med)
hist(comp_sf_data$V1)
#Let us draw a sample of 500 employees at random without replacement, 
#and let the median total compensation of the sampled employees serve as our estimate of the parameter.
our_sample = sample(comp_sf_data$V1, 500, replace = TRUE)
hist(our_sample)
est_median=median(our_sample)
est_median
#The sample size is large. By the law of averages, the distribution of the sample resembles that of the population, 
#and consequently the sample median is not very far from the population median (though of course it is not exactly the same).
#Here are the steps of the bootstrap method for generating another random sample that resembles the population:

##Treat the original sample as if it were the population.
##Draw from the sample, at random with replacement, the same number of times as the original sample size.

N=1000
my.boot = numeric(N)
for(i in 1:N)
{
  x=sample(our_sample, 500, TRUE)
  my.boot[i] = median(x)
}
hist(my.boot )
abline(v=sample_med, lwd = 2)
mean(my.boot)
sd(my.boot)

install.packages("ggplot2")

library(ggplot2)
qplot(my.boot, geom="histogram", binwidth = 1000) +
  geom_point()
