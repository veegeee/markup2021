set.seed(1996)
#empty dataframe for simulated mean, bias etc
simulated_data<-data.frame()
for(i in 1:100){
  x<-rnorm(1000, 0,1)
  mean_sample<-mean(x)
  abs_bias<-abs(mean_sample)
  se<-1/sqrt(1000)
  lower<-mean_sample-1.96*se
  upper<-mean_sample+1.96*se
  simulated_data[i,1]<-mean_sample
  simulated_data[i,2]<-abs_bias
  simulated_data[i,3]<-se
  simulated_data[i,4]<-lower
  simulated_data[i,5]<-upper
}
colnames(simulated_data)<-c('mean', 'abs bias', 'se', 'lower', 'upper')

simulated_data$covered<-ifelse(simulated_data$lower<0 & 0<simulated_data$upper, 1, 0)

library(ggplot2)
limits <- aes(ymax = upper, ymin = lower)
ggplot(simulated_data, aes(y=mean, x=1:100, colour = covered)) + 
  geom_hline(aes(yintercept = 0), color = "dark grey", size = 1) + 
  geom_pointrange(limits)

#apparently 98 out of 100 cis contained true value, lets verify
mean(simulated_data$covered)

#samples where  ci did not contain population value
no_cover<-simulated_data[simulated_data$covered==0,]
