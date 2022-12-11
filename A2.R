setwd("~/文件/Stochastic data processing and simulation")
set.seed(321)
# Ex1

# predict the B
attach(cars) 
m1 <- lm(dist~speed) # extract betahat
betahat_0 <- m1$coefficients[1]
betahat_1 <- m1$coefficients[2]

# obtain s
s <- summary(m1)$sigma

# compute predictions
B <- matrix(0,5000,5) 
set.seed(321)
for (i in 1:5000){
  B[i,1] <- betahat_0 + betahat_1 * 5 + rnorm(1, 0, s)
  B[i,2] <- betahat_0 + betahat_1 * 10 + rnorm(1, 0, s)
  B[i,3] <- betahat_0 + betahat_1 * 15 + rnorm(1, 0, s)
  B[i,4] <- betahat_0 + betahat_1 * 20 + rnorm(1, 0, s)
  B[i,5] <- betahat_0 + betahat_1 * 25 + rnorm(1, 0, s)
}
B <- as.data.frame(B,col.names = names('speed_5','speed_10','speed_15','speed_20', 'speed_25'))
colnames(B) <- c('speed_5','speed_10','speed_15','speed_20', 'speed_25')

# compute 2.5 and 97.5 empirical quantiles for each set
speed_5_int <- quantile(B$speed_5, c(.025, .975))
speed_10_int <- quantile(B$speed_10, c(.025, .975))
speed_15_int <- quantile(B$speed_15, c(.025, .975))
speed_20_int <- quantile(B$speed_20, c(.025, .975))
speed_25_int <- quantile(B$speed_25, c(.025, .975))

#upper and lower bounder
low_bound <- c(speed_5_int[1], speed_10_int[1], speed_15_int[1], speed_20_int[1], speed_25_int[1])
up_bound <- c(speed_5_int[2], speed_10_int[2], speed_15_int[2], speed_20_int[2], speed_25_int[2])

#Produce the scatterplot of the data and adding regress line
plot(speed, dist, main="Cars")
abline(m1, col="blue")

# adding the upper and lower bounders
x = seq(5,25, by = 5)
lines(x, low_bound, col="blue", lty=2)
lines(x, up_bound, col="blue", lty=2)


# Ex2(i)
Wage<-read.table("Wage.txt",header=TRUE)
# select training and test sample
N <- 3000
n <- floor(0.7* N)
set.seed(321)
indeces <- sample.int(N, n)

# classify the training and test sample
training <- Wage[indeces,]
testing <- Wage[-indeces,]

# gen matrix for MSE
pMSE <- seq(0,0, length.out = 10)
 for (i in 1:10){
  m2 <-lm(wage~poly(age,i,raw=TRUE), data = training)
  y_new_hat <- predict(m2, newdata = testing)
  sum_testing <- sum((testing$wage - y_new_hat)^2)
  pMSE[i] <- sum_testing / (N - n)
 }

# plot pMSE
plot(1:10, pMSE, ylab = 'pMSE', xlab = 'p')
lines(1:10, pMSE)
View(pMSE)

# Ex2(ii)
pMSE <- matrix(0,50,10)
colnames(pMSE) <- c(1:10)
set.seed(321)
for (m in 1:50){
  indeces <- sample.int(N, n)
  training <- Wage[indeces,]
  testing <- Wage[-indeces,]
  for (i in 1:10){
    m2 <-lm(wage~poly(age,i,raw=TRUE), data = training)
    y_new_hat <- predict(m2, newdata = testing)
    sum_testing <- sum((testing$wage - y_new_hat)^2)
    pMSE[m,i] <- sum_testing / (N - n)
  }
}

# plot figure
temp <- as.data.frame(as.table(pMSE))
drops <- c("Var1")
temp <- temp[ , !(names(temp) %in% drops)]
plot(temp, xlab = 'degree', ylab = 'pMSE')


# Ex2(iii)
# construct model with optimal and suboptimal p
xstar <- seq(18,80, length.out = 1000) # x vector

# p = 2
m3 <-lm(wage~poly(age,2,raw=TRUE), data = Wage)
mypred_1 <- predict(m3, newdata = data.frame(age=xstar), interval = "prediction")
plot (Wage) # plot figure
lines(xstar, predict(m3, data.frame(age=xstar)), col="blue")
lines(xstar, mypred_1[,2], col="blue")
lines(xstar, mypred_1[,3], col="blue")

# p = 3
m4 <-lm(wage~poly(age,3,raw=TRUE), data = Wage)
mypred_2 <- predict(m4, newdata = data.frame(age=xstar), interval = "prediction")
plot (Wage) # plot figure
lines(xstar, predict(m4, data.frame(age=xstar)), col="blue")
lines(xstar, mypred_2[,2], col="blue")
lines(xstar, mypred_2[,3], col="blue")

# p = 4
m5 <-lm(wage~poly(age,4,raw=TRUE), data = Wage)
mypred_3 <- predict(m5, newdata = data.frame(age=xstar), interval = "prediction")
plot (Wage) # plot figure
lines(xstar, predict(m5, data.frame(age=xstar)), col="blue")
lines(xstar, mypred_3[,2], col="blue")
lines(xstar, mypred_3[,3], col="blue")

# Q4 
Wage<-read.table("Wage.txt",header=TRUE)
a = 0.1 # significant level
n = 3000 # number of sample
B = 2000 # time of resample
boot_betahat_1 <- matrix(0,2000,4) # place to store our result
m6 <-lm(wage~poly(age,3,raw=TRUE), data = Wage)  # original regression

set.seed(321) # resampleing process
for (i in 1:B){
  x <- sample.int(n, size = n, replace = TRUE)
  resample <- Wage[x,]
  m7 <-lm(wage~poly(age,3,raw=TRUE), data = resample)
  boot_betahat_1[i,1] <- summary(m7)$coefficients[1]
  boot_betahat_1[i,2] <- summary(m7)$coefficients[2]
  boot_betahat_1[i,3] <- summary(m7)$coefficients[3]
  boot_betahat_1[i,4] <- summary(m7)$coefficients[4]
}

# gen confidence interval with boot and confint

boot_inter_beta0 <- quantile(boot_betahat_1[,1], c(a/2, 1-(a/2)))
boot_inter_beta1 <- quantile(boot_betahat_1[,2], c(a/2, 1-(a/2)))
boot_inter_beta2 <- quantile(boot_betahat_1[,3], c(a/2, 1-(a/2)))
boot_inter_beta3 <- quantile(boot_betahat_1[,4], c(a/2, 1-(a/2)))

inter_beta0 <- confint(m6, level = 0.9)[1,1:2]
inter_beta1 <- confint(m6, level = 0.9)[2,1:2]
inter_beta2 <- confint(m6, level = 0.9)[3,1:2]
inter_beta3 <- confint(m6, level = 0.9)[4,1:2]

