setwd("~/文件/Stochastic data processing and simulation")

# Q1 

lambda_1 = function(t) {1/ (2 *  t^(1/2))} # lambda for T1 to T3
lambda_2 = function(t) {t^(11/10)/50}   # lambda for T4

#caculate Survival function
q1_s1 <- function(t) {exp(-sqrt(t))} # survival function for T1 to T3
q1_s2 <- function(t) {exp((-1/105) * t^(21/10))} # survival function for T4
q1_s <- function(t) { (1 - (1 - exp(-sqrt(t)))^3) * (exp((-1/105) * t^(21/10)))} # survival function for network
integrate(q1_s, 0, Inf) # gen expected life length of the network , which is 3.184948


# Q2

q2_a <- expression ((1-(1-exp(-sqrt(t)))^3) * (exp((-1/105) * t^(21/10)))) # expression the survival functiob for network
D(q2_a,"t")

q2_b <- function (t) {(-1 * eval(D(q2_a,"t"))) / ((1 - (1 - exp(-sqrt(t)))^3) * (exp((-1/105) * t^(21/10))))} # derive lambda function for network

t <- seq(0, 30, length.out = 1001) # gen seq of time
plot(t, q2_b(t), xlab = "time", ylab = "lambda") # plot lambda and time


#Q3

q3_a <- expression(exp((-1/105) * t^(21/10))) # expression for T4 survival function
q3_T4_fail <- function(t) {-eval(D(q3_a,"t")) * (1 - (1 - exp(-sqrt(t)))^3)} # calculate fail % due to T4
integrate(q3_T4_fail, 0, Inf) # % of fail due to T4 is 0.2187526


#Q4

# trial k =  5
# cumulative distribution function
F_t <- expression( 1- ((t^0 / factorial(0)) * exp(-t) +(t^1 / factorial(1)) * exp(-t) + (t^2 / factorial(2)) * exp(-t) + (t^3 / factorial(3)) * exp(-t)+ (t^4 / factorial(4)) * exp(-t) + (t^5 / factorial(5)) * exp(-t)))

D(F_t,'t') # derive density function
T_cheap_fail <- function(t) {eval(D(F_t,'t')) * (1 - (1 - exp(-sqrt(t)))^3)} # fail rate due T4 fumction

integrate(T_cheap_fail, 0, Inf) # fail rate 0.2695477

# trial k =  6
# cumulative distribution function
F_t <- expression( 1- ((t^0 / factorial(0)) * exp(-t) +(t^1 / factorial(1)) * exp(-t) + (t^2 / factorial(2)) * exp(-t) + (t^3 / factorial(3)) * exp(-t)+ (t^4 / factorial(4)) * exp(-t) + (t^5 / factorial(5)) * exp(-t) + (t^6 / factorial(6)) * exp(-t) ))

D(F_t,'t')# derive density function
T_cheap_fail <- function(t) {eval(D(F_t,'t')) * (1 - (1 - exp(-sqrt(t)))^3)} # fail rate due T4 fumction

integrate(T_cheap_fail, 0, Inf) # fail rate 0.226034


# trial k =  7
# cumulative distribution function
F_t <- expression( 1- ((t^0 / factorial(0)) * exp(-t) +(t^1 / factorial(1)) * exp(-t) + (t^2 / factorial(2)) * exp(-t) + (t^3 / factorial(3)) * exp(-t)+ (t^4 / factorial(4)) * exp(-t) + (t^5 / factorial(5)) * exp(-t) + (t^6 / factorial(6)) * exp(-t) + (t^7 / factorial(7)) * exp(-t) )) 

D(F_t,'t') # derive density function
T_cheap_fail <- function(t) {eval(D(F_t,'t')) * (1 - (1 - exp(-sqrt(t)))^3)} # fail rate due T4 fumction

integrate(T_cheap_fail, 0, Inf) # fail rate 0.1912075

# trial k =  8
# cumulative distribution function
F_t <- expression( 1- ((t^0 / factorial(0)) * exp(-t) +(t^1 / factorial(1)) * exp(-t) + (t^2 / factorial(2)) * exp(-t) + (t^3 / factorial(3)) * exp(-t)+ (t^4 / factorial(4)) * exp(-t) + (t^5 / factorial(5)) * exp(-t) + (t^6 / factorial(6)) * exp(-t) + (t^7 / factorial(7)) * exp(-t)+ (t^8 / factorial(8)) * exp(-t) )) 

D(F_t,'t') # derive density function
T_cheap_fail <- function(t) {eval(D(F_t,'t')) * (1 - (1 - exp(-sqrt(t)))^3)} # fail rate due T4 fumction

integrate(T_cheap_fail, 0, Inf) # fail rate 0.1629912



# Q5

# trial k = 7
#Survival function
s_t <- function(t) (((t^0 / factorial(0)) * exp(-t) +(t^1 / factorial(1)) * exp(-t) + (t^2 / factorial(2)) * exp(-t) + (t^3 / factorial(3)) * exp(-t)+ (t^4 / factorial(4)) * exp(-t) + (t^5 / factorial(5)) * exp(-t) + (t^6 / factorial(6)) * exp(-t) + (t^7 / factorial(7)) * exp(-t) ) *  (1 - (1 - exp(-sqrt(t)))^3))

integrate(s_t, 0, Inf) # gen expected lifetime which is 3.290816


# trial k = 6
#Survival function
s_t <- function(t) (((t^0 / factorial(0)) * exp(-t) +(t^1 / factorial(1)) * exp(-t) + (t^2 / factorial(2)) * exp(-t) + (t^3 / factorial(3)) * exp(-t)+ (t^4 / factorial(4)) * exp(-t) + (t^5 / factorial(5)) * exp(-t) + (t^6 / factorial(6)) * exp(-t) ) *  (1 - (1 - exp(-sqrt(t)))^3))

integrate(s_t, 0, Inf)# gen expected lifetime which is 3.099609

# trial k = 5
#Survival function
s_t <- function(t) (((t^0 / factorial(0)) * exp(-t) +(t^1 / factorial(1)) * exp(-t) + (t^2 / factorial(2)) * exp(-t) + (t^3 / factorial(3)) * exp(-t)+ (t^4 / factorial(4)) * exp(-t) + (t^5 / factorial(5)) * exp(-t)  ) *  (1 - (1 - exp(-sqrt(t)))^3))

integrate(s_t, 0, Inf)# gen expected lifetime which is 2.873575


# Q6

# survival function for T1 to T3 = (1 - (1 - exp(-(t/mu)^(1/4)))) 
# survival function for T4 = (1 - (1 - exp(-(t/gama)^(1/4)))) 

q6_a <- function (t) (( 1 - (1 - exp(-(t/mu)^(1/4)))^3) * exp(-(t/gamma)^(1/4))) # survival function for network
q6_b <- function ( t,mu ) { ( ( 1 - (1 - exp(-1*(t/mu)^(1/4)))^3) * exp(-1 * (t/(10/3 - 3 * mu))^(1/4)))} # replace gamma with equation of mu

x.min <- optimize(q6_b, c(0.0001,1.1111), tol = 0.0001 , t = Inf, maximum = TRUE)  # calculate mu which max the survival function when time = Inf
x.min # chech mu , when mu = 1.1111 exist max 


q6_c <- function ( t ) { ( ( 1 - (1 - exp(-1*(t/1.1111)^(1/4)))^3) * exp(-1 * (t/(10/0.0001))^(1/4)))}
integrate(q6_c, 0, Inf) # input mu(1.1111) and gama(0.0001) to calculate expected life length, get 59.75446 expected life length

q6_c <- function ( t ) { ( ( 1 - (1 - exp(-1*(t/1)^(1/4)))^3) * exp(-1 * (t/(10/0.3333))^(1/4)))}
integrate(q6_c, 0, Inf)# input mu(1) and gama(0.3333) to calculate expected life length, get 15.44975expected life length

q6_c <- function ( t ) { ( ( 1 - (1 - exp(-1*(t/0.0001)^(1/4)))^3) * exp(-1 * (t/(10/3.3333))^(1/4)))}
integrate(q6_c, 0, Inf)# input mu(0.0001) and gama(3.3333) to calculate expected life length, get 0.005010734 expected life length
 


