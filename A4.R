setwd("~/文件/Stochastic data processing and simulation")

install.packages("LearnBayes")
install.packages("dplyr")
library("LearnBayes")
library("dplyr")

set.seed(321)
# Q1(a) 
stock <- as.matrix(read.csv("stockvalues.txt"))

log_stock <- log(stock) # gen dif-log matrix
df_stock <- matrix(0, nrow = 1005, ncol = 7)
for (m in 1:7) {
  for (i in 1:1005){
    df_stock[i,m] <- log_stock[i+1,m] - log_stock[i,m]
  }
}
cov_sto <- cov(df_stock) #cov-martix
mean_sto <- colMeans(df_stock) # mean vector

V_q <- rmnorm(1000,100*mean_sto,100*cov_sto) # gen Vq

#Q1(b)
exp_Vq <- exp(V_q)
# stock recived equal investment
fun_1 <- function(k){
  mean((1/k)*(1 - (rowSums(1/7 * exp_Vq))^-k))
}

fun_1(-0.5)
# k = -0.5 ans 0.05282092
fun_1(0.5)
# k = 0.5 ans 0.04199535
fun_1(1.5)
# k = 1.5 ans 0.03191279

# all invest into best stock, which is forth stock
fun_2 <- function(k){
  mean((1/k)*(1 - exp_Vq[,4]^-k))
}
fun_2(-0.5)
# k = -0.5 ans 0.0686841
fun_2(0.5)
# k = 0.5 ans 0.0395914
fun_2(1.5)
# k = 1.5 ans 0.01209601

#Q1(c)
#gen function
fun_3 <- function(w){
 -1 * mean((1/k)*(1 - (w * exp_Vq[,3] + (1-w)*exp_Vq[,4])^-k))
}

# k = -0.5
k <- -0.5
optimize(fun_3, c(0,1), lower = 0, upper = 1)
# ans 0.6537733 with max 0.08534997

# k= 1.5
k <- 1.5
optimize(fun_3, c(0,1), lower = 0, upper = 1)
# ans 0.2957975 with max 0.02917283

#Q1(d)
# write in pdf

#Q2(a)
ma_kn <- read.table("matureKnee.txt") # read data
im_kn <- read.table("immatureKnee.txt")

ma_kn$new_col <- 1 # adding y
im_kn$new_col <- 0

knee <- rbind(ma_kn,im_kn)
knee <- rename(knee, "x" = "V1" , "y" = "new_col")

x <- knee$x - 18 # age and mature-knee
y <- knee$y

# MLE function
fun_4 <- function(a,b){
  p <- exp(a + b*x )/(1 + exp(a + b*x))
  out_1 <- prod(p^y * (1-p)^(1-y))
  return(out_1)
  }

# transform for max
fun_5 <- function(x) {
  a <- x[1]
  b <- x[2]
  -fun_4(a, b)
}
optim(c(2,2),fun_5) # find max number, max L is -1.614533e-07

#  input a and b
a <- 0.6848532
b <- 1.7217080
p <- exp(a + b*x)/(1 + exp(a + b*x)) # p value

plot(x,p, xlab = "age - 18") # plot of p and x

#Q2(b)
a <- seq(-0.5,2, length.out = 21) # gen interval of a and b
b <- seq(0.5,3, length.out = 21)

out_2 <- matrix(0,21,21) # gen matrix for outcome

# gen matrix to store outcome
for (i in 1:21){
  for (m in 1:21){
    out_2[i,m] <- outer(a[i],b[m], FUN = "fun_4")
  }
}  

image(a,b,out_2)# plot

#Q2(c)
a <- 0.6848532
b <- 1.7217080
# for simplization, B = 10
B <- 10

# gen function, we assumed that age at least 14
cost_1 <- function(age){
  pi <- dgamma(age-14, shape = shape, rate = rate) # distribution of age
  c_1 <- ifelse(age <= 18, B , 1) # cost_1 function
  fx <- exp(a + b*(age-18))/(1+exp(a + b* (age-18))) # p-value
  out <- pi * fx * c_1
  return(out)
}

cost_2 <- function(age){
  pi <- dgamma(age-14, shape = shape, rate = rate) # distribution of age
  c_2 <- ifelse(age <= 18, B*(18-age) , age - 18) # cost_2 function
  fx <- exp(a + b*(age-18))/(1+exp(a + b* (age-18))) # p-value
  out <- pi * fx * c_2
  return(out)
}

# in i
shape <- 3 # set up alpha and mu
mu <- 18.5
rate <- shape/(mu-14)
outcome_i <- c(0,0)
outcome_i[1] <- ifelse (integrate(cost_1,18,100)$value > integrate(cost_1,0,18)$value, 1,0)
outcome_i[2] <- ifelse (integrate(cost_2,18,100)$value > integrate(cost_2,0,18)$value, 1,0)
# with c_1, c_a > c_c 
# with c_2, c_a < c_c 


# in ii
shape <- 6 # set up alpha and mu
mu <- 19.5
rate <- shape/(mu-14)
outcome_ii <- c(0,0)
outcome_ii[1] <- ifelse (integrate(cost_1,18,100)$value > integrate(cost_1,0,18)$value, 1,0)
outcome_ii[2] <- ifelse (integrate(cost_2,18,100)$value > integrate(cost_2,0,18)$value, 1,0)
# with c_1, c_a > c_c 
# with c_2, c_a < c_c 


# in iii
shape <- 3 # set up alpha and mu
mu <- 20.58
rate <- shape/(mu-14)
outcome_iii <- c(0,0)
outcome_iii[1] <- ifelse (integrate(cost_1,18,100)$value > integrate(cost_1,0,18)$value, 1,0)
outcome_iii[2] <- ifelse (integrate(cost_2,18,100)$value > integrate(cost_2,0,18)$value, 1,0)
# with c_1, c_a > c_c 
# with c_2, c_a < c_c 

#Q2(d)
# I don't where to start :(((
