setwd("~/文件/Stochastic data processing and simulation")
data(cars)
attach(cars)
cars # check the dataset "car"

# Exercise 1
x_bar <- mean(speed) #first we create vriable of mean of distant and speed
y_bar <- mean(dist)
n <- 50

upper_beta1 <- (speed - x_bar) * dist # we calulate molecular and denominator for beta1_hat
lower_beta1 <- (speed - x_bar)^2

beta1_hat <- sum(upper_beta1) / sum(lower_beta1) # calulate beta1_hat and beta0_hat
beta0_hat <- y_bar - (beta1_hat * x_bar)

mym1 <- lm(dist~speed) # check with lm 
summary(mym1)

# Excercise 2
y_hat <- predict(mym1)  # calculate y_hat
n1 <- (dist - y_hat)^2  
s <- sqrt(sum(n1) / (50-2)) # calcute s

# Excercise 3
t1 <- qt(1-0.2/2,(50-2)) # calculate the student distrution with 0.80
n2 <- (speed - x_bar)^2  # calculate inner part of confident interval
n3 <- 1/n + (x_bar)^2 / sum(n2)

I_beta0_hat_up <- (beta0_hat + t1 * s * sqrt(n3)) # calculate confident interval for beta0_hat
I_beta0_hat_down <- (beta0_hat - t1 * s * sqrt(n3))

I_beta1_hat_up <- (beta1_hat + t1 * s / sqrt(sum(n2))) # calculate confident interval for beta1_hat
I_beta1_hat_down <- (beta1_hat - t1 * s / sqrt(sum(n2)))

confint(mym1, level = 0.8) # check with confint 

# the 80% condfident interval for beta1 means , giveing 100 times of drawing example and calculating for beta1_hat, around 80 will be inside and 20 be will outside the 80% of confident interval , meaning that 80% of beta1_hat we calculating will be inside the confident interval . However , beta1 is a actual number , it can only be inside or outside confident interval. Hence, we cannot say beta1 is 80% inside confident intercal , we can only said that " with 80% of "confident" beta1 is inside confident interval" 

# Excercise 4

# set real beta0_star and beta_1_star
beta0_star <- 0
beta1_star <- 10

# gen the new distant
formula_1 <- beta0_star + beta1_star * speed # part of beta0_star + beta1_star * speed

D_new <- matrix(, 50, 1000)  # repeat 1000 times to creat matrix with beta0_star + beta1_star * speed
for (i in 1:1000){
  D_new[ , i] <- formula_1
}

error <- matrix(,50,1000) # gen error part with sd=15.38
for (i in 1:50){
  for (j in 1:1000){
  error [  i , j  ] <- matrix (rnorm(1, 0 , 15.38)) 
  }
}

D_new <- D_new + error # adding error part
D_new <- as.data.frame(D_new) # trans to data form
names(D_new) <- gsub("V" , "D" , names (D_new)) # changing names
colnames(D_new) # check the name

attach(D_new)
attach(cars)

lm_list <- list() # list for linear model
for (i in 1:1000){
  lm_list[[i]]<- lm (D_new[, i] ~  speed)
}

coe_data <- data.frame(Beta_0=NA, Beta_1=NA) # gen data for coeffient 
for (i in 1:1000){
  coe_data[i,]<- coefficients( lm (D_new[ ,i ] ~  speed))
}


#otherway to approch this
nu<- 1000
my_lms <- lapply(1:nu, function(x) lm(D_new[, x] ~ speed))
beta_new <- sapply(my_lms, coef)


#calculate confident interval

D_new_mat <- as.matrix(D_new)   #matrix form for D_new and coe
coe_mat <- as.matrix(coe_data)


y_hat_new <- matrix(,1000,50) # gen matrix for y_hat_new
for (i in 1:1000){
  y_hat_new[i,] <- coe_mat[i,1] + coe_mat[i,2]*speed
}

y_hat_new <- t(y_hat_new) # transpost for calculation
s_new <- sqrt(colSums((D_new_mat - y_hat_new)^2)/(n-2))
s_new <- as.matrix(s_new)


t2 <- qt(1-0.05/2,(50-2)) # create matrix for confident interval for beta0 and beta1
con_mat <- matrix(,1000,4)
for (i in 1:1000){
  con_mat[i,1] <-  (coe_mat[i,1] + t2 * s_new[i,1] * sqrt(n3))
  con_mat[i,2] <-  (coe_mat[i,1] - t2 * s_new[i,1] * sqrt(n3))
  con_mat[i,3] <-  (coe_mat[i,2] + t2 * s_new[i,1] / sqrt(sum(n2)))
  con_mat[i,4] <-  (coe_mat[i,2] - t2 * s_new[i,1] / sqrt(sum(n2)))
}

# Check confidence interval with actual beta0 and beta1

check<-matrix(0,1000,2)  # build up a check martix
for (i in 1:1000){
  check[i,1] <- replace( check[i,1] , con_mat[i,1] >0  & con_mat[i,2] < 0, 1)
  check[i,2] <- replace( check[i,2] , con_mat[i,3] >10  & con_mat[i,4] < 10, 1)
}


precent <- colMeans(check)# calculate the precent of all confident interval contains beta0 and beta1
precent 

# Excercise 5

# produce density plots

plot(density(coe_mat[,1]), main = "Beta_0")
plot(density(coe_mat[,2]), main = "Beta_1")

# gen new database

speed_fake <- seq(4, 25 , length.out = 200)
D_new_2 <- matrix(, 200, 1000)
D_new_2[,1:1000] <- beta0_star + beta1_star * speed_fake

# adding error term
for (i in 1:1000){
  D_new_2 [,i] <- D_new_2[,i] + rnorm(200, 0, 15.38)
}

# gen parameter
coe_data_2 <- data.frame(Beta_0=NA, Beta_1=NA) # gen data for coeffient 
for (i in 1:1000){
  coe_data_2[i,]<- coefficients( lm (D_new_2[ ,i ] ~  speed_fake))
}

# plot density with 200 obs
plot(density(coe_data_2$Beta_0), main = "Beat_0_new")
plot(density(coe_data_2$Beta_1), main = "Beta_1_new")
