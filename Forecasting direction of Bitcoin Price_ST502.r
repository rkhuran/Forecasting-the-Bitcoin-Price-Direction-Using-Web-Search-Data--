library(openxlsx)
library(fitdistrplus)
library(goftest)
library(GoFKernel)
library(NormalLaplace)
library(MASS)
library(ggplot2)

#Data Input
bitcoin= read.csv("C:/Users/rohit/Documents/NCSU/Spring2019/ST502/Final Project/bitcoinity_data.csv")
head(bitcoin)
summary(bitcoin)

#initial set of variables 
google_trends= read.csv("C:/Users/rohit/Documents/NCSU/Spring2019/ST502/Final Project/multiTimeline (1).csv")
head(google_trends)
summary(google_trends)

merged_data = merge(bitcoin, google_trends, by.x='Date',by.y='Week',sort= TRUE )
head(merged_data)
qqplot(merged_data$increase_str,merged_data$Close.., xlab = "Increase_str", ylab = "Bitcoin Price")
qqplot(merged_data$future_str, merged_data$Close.., xlab = "Future_str", ylab = "Bitcoin Price")

#improved version
google_trends= read.csv("C:/Users/rohit/Documents/NCSU/Spring2019/ST502/Final Project/new_multiTimeline.csv")
head(google_trends)
summary(google_trends)

merged_data = merge(bitcoin, google_trends, by.x='Date',by.y='Week',sort= TRUE )
head(merged_data)

qqplot(merged_data$bitcoin_str,merged_data$Close..)
qqplot(merged_data$bitcoin_price_str, merged_data$Close..)
qqplot(merged_data$usd_str, merged_data$Close..)

new<-merged_data[,-1] #dropping the date column

plot(new$Close..)

# Linear Regression model
lmod<-lm(as.numeric(Close..)~bitcoin_str+usd_str+bitcoin_price_str,data=new)
print(lmod)
summary(lmod)

#Add new column returns and direction
new$returns[2:260]<-log(as.numeric(new$Close..[2:260])/as.numeric(new$Close..[1:259]))
new$direction[2:260]<-0

for (i in 2:260)
{
  if(new$returns[i]>0) {new$direction[i]<-1}
  else {new$direction[i]<-0}
}

# LogistiC regression model with 3 independent variables
Y<-new$direction
X1<-new$bitcoin_str 
X2<-new$usd_str
X3<-new$bitcoin_price_str
logmdl.fit<-glm(Y~X1+X2+X3,family=binomial(link='logit'),data=new)
summary(logmdl.fit)

# LogistiC regression model with 2 independent variables
Y<-new$direction
#X1<-new$bitcoin_str # droppping X1 due to poor p value
X1<-new$usd_str
X2<-new$bitcoin_price_str

logmdl.fit<-glm(Y~X1+X2,family=binomial(link='logit'),data=new)
summary(logmdl.fit)

betahat <- coefficients(logmdl.fit)
print(betahat)
CI <- confint(logmdl.fit)
beta0.CI <- CI[1,]
beta1.CI <- CI[2,]
beta2.CI <- CI[3,]

# Parameteric Estimation
par("mar")
par(mar=c(1,1,1,1))

# distribution fitting
hist(new$usd_str)
X1<-new$usd_str
plotdist(X1, histo = TRUE, demp = TRUE)
descdist(X1, discrete = FALSE, boot = 500)

#USD_Normal
fit_1 <- fitdist(X1,"norm")
summary(fit_1)
ad.test(X1,"pnorm",mean=21.09, sd=4.75)

#Log(USD_str)
hist(log(new$usd_str))
fit_1 <- fitdist(log(X1),"norm")
summary(fit_1)
ad.test(log(X1),"pnorm",mean=3.0222314, sd=0.2352553)

#Bitcoin_price str
hist(new$bitcoin_price_str)
X2<-new$bitcoin_price_str
plotdist(X2, histo = TRUE, demp = TRUE)
descdist(X2, discrete = FALSE, boot = 500)

fit_2 <- fitdist(log(X2),"beta")
summary(fit_2)
ad.test(log(X1),"pnorm",mean=3.0222314, sd=0.2352553)


n=200 #n=100 also tried
# Non-Parametric Boot Strapping
B = 1000
set.seed(10)
np.bs.save <- NULL
for(bs in 1:B){
  bs.data <- new[sample(x=1:n,size=n,replace=TRUE),] # random sample from data
  bs.Y <- bs.data$direction
  bs.X1<- bs.data$usd_str
  bs.X2<- bs.data$bitcoin_price_str
  bs.fit <- glm(bs.Y~bs.X1+bs.X2,family=binomial(link='logit'),data=bs.data)
  bs.betahat <- coefficients(bs.fit)
  np.bs.save <- rbind(np.bs.save,bs.betahat)
}
np.bs.save <- data.frame(np.bs.save)

names(np.bs.save) <- c('beta0','beta1', 'beta2')
plotdist(np.bs.save$beta0, histo = TRUE, demp = TRUE)
descdist(np.bs.save$beta0, discrete = FALSE, boot = 500)

plotdist(np.bs.save$beta1, histo = TRUE, demp = TRUE)
descdist(np.bs.save$beta1, discrete = FALSE, boot = 500)
plotdist(np.bs.save$beta2, histo = TRUE, demp = TRUE)
descdist(np.bs.save$beta2, discrete = FALSE, boot = 500)

beta0.CI.np.bs <- quantile(np.bs.save$beta0,c(0.025,0.975))
beta1.CI.np.bs <- quantile(np.bs.save$beta1,c(0.025,0.975))
beta2.CI.np.bs <- quantile(np.bs.save$beta2,c(0.025,0.975))

# Compare beta0 CI
beta0.CI
beta0.CI.np.bs

# Compare beta1 CI
beta1.CI
beta1.CI.np.bs

# Compare beta2 CI
beta2.CI
beta2.CI.np.bs

# plot the prob of increase against the USD or bitcoin price.
plot_data<-NULL
x1_mean=mean(new$usd_str)
x2_mean=mean(new$bitcoin_price_str)

beta0=betahat[1]
beta1=betahat[2]
beta2=betahat[3]

#Plot - Change in probability vs Increase in USD_str, keeping bitcoin_price_str constant(mean)
for (i in 1:100)
{
  temp= exp(beta0 + beta1*i+ beta2*x2_mean)
  prob=temp/(1+temp)
  plot_data<- rbind(plot_data, prob)
}
plot(plot_data, xlab ="USD_STR", ylab= "Probability", main="Plot - Change in probability vs Increase in USD_str, keeping bitcoin_price_str constant(mean)")

# 2.	Plot - Change in probability vs Increase in bitcoin_price_str, keeping usd_str constant(mean)
plot_data<-NULL
for (i in 1:100)
{
  temp= exp(beta0 + beta1*x1_mean+ beta2*i)
  prob=temp/(1+temp)
  plot_data<- rbind(plot_data, prob)
}
plot(plot_data, xlab ="Bitcoin_Price_STR", ylab= "Probability", main="Plot - Change in probability vs Increase in bitcoin_price_str, keeping usd_str constant(mean)")

