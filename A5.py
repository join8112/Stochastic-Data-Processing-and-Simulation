#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 16 02:10:09 2020

@author: Zayne
"""

import random
import numpy as np
import matplotlib.pyplot as plt


random.seed()

mu = 0
sigma = 1

N = 20
sample_size = np.zeros(N)

for n in range(N):
    sample_size[n] = 2**(n+1)
    
print(sample_size)


average = np.zeros(N)

error_av = np.zeros(N)

average_1 = 0

for m in range(int(sample_size[N-1])):
    average_1 += random.gauss(mu, sigma)
    for n in range(N):
        if m == (int(sample_size[n])-1):
            average[n] = average_1/sample_size[n]
            error_av[n] = np.abs(average[n] - mu)


print(average)
print(error_av)


plt.loglog(sample_size,error_av)
plt.loglog(sample_size,1/np.sqrt(sample_size))

plt.savefig('normal error.pdf')


#Q1


#Q2
M = 1024 # set up M
theda = 0 # theda for MC method
theda_1 = 0

for n in range(M):
    u = np.random.uniform(0,1)
    theda_1 += 1/2*(u**4 + u**2)
    
theda_mc = theda_1/M
theda_mc = 2 * theda_mc
print(theda_mc) #result

#Q3
count = 0 # count for HM method
for n in range(M):
    u_1 = np.random.uniform(0,1)
    u_2 = np.random.uniform(0,1)
    if u_2 <= 1/2*(u_1**4 + u_1**2):
        count += 1
                           
theda_hm = count/M
theda_hm = 2 * theda_hm
print(theda_hm)  #result

#Q4

1/36 +1/14 + 1/20 - (1/10 + 1/6 )**2   #Var[f(U)]
(1/36 +1/14 + 1/20 - (1/10 + 1/6 )**2)*2/M # Var[theda] 
np.sqrt((1/36 +1/14 + 1/20 - (1/10 + 1/6 )**2)*2/M)  # L^2 error in mc



(1/10 + 1/6) - (1/10 + 1/6 )**2     # Var[Z]
((1/10 + 1/6) - (1/10 + 1/6 )**2)*2/M  #Var[theda]
np.sqrt(((1/10 + 1/6) - (1/10 + 1/6 )**2)*2/M) # L^2 error in hm

#Q5
random.seed()
N = 20
sample_size = np.zeros(N)

for n in range(N): # creat sample list
    sample_size[n] = 2**(n+1) 
print(sample_size) 


var_mc = np.zeros(N) #mc method
var_mc_1 = 0

for m in range(int(sample_size[N-1])):
    u = np.random.uniform(0,1)
    var_mc_1 += 1/4**(u**8 + 2*u**6 + u**6) - (1/2**(u**4 + u**2))**2 \
        #estimate f(u)
    for n in range(N):
        if m == (int(sample_size[n])-1):
            var_mc[n] = var_mc_1/sample_size[n]
            var_mc[n] = var_mc[n] * 2/sample_size[n]
                    
root_mc = np.sqrt(var_mc)

print(root_mc)  # root result in mc method


count = 0 #hm method
var_hm = np.zeros(N)
var_hm_1 = 0

for m in range(int(sample_size[N-1])):
    u_1 = np.random.uniform(0,1)
    u_2 = np.random.uniform(0,1)
    if u_2 <= 1/2*(u_1**4 + u_1**2):
        count += 1 #estimate p
    for n in range(N):
        if m == (int(sample_size[n])-1):
            var_hm[n] = count/sample_size[n] - (count/sample_size[n])**2
            var_hm[n] = var_hm[n] *2 /sample_size[n]
            
root_hm = np.sqrt(var_hm)

print(root_hm)  # root result in hm method

#Q6
random.seed()
N = 20
sample_size = np.zeros(N)

for n in range(N):
    sample_size[n] = 2**(n+1) # creat sample list
print(sample_size) 

theda_mc = np.zeros(N) # est with mc 
theda_mc_1 = 0
for m in range(int(sample_size[N-1])):
    u = np.random.uniform(0,1)
    theda_mc_1 += 1/2*(u**4 + u**2)
    for n in range(N):
        if m == (int(sample_size[n])-1):
            theda_mc[n] = theda_mc_1 / sample_size[n] 
            
error_mc =  np.abs(2*theda_mc - 2*(1/10 + 1/6)) # error in mc method

theda_hm = np.zeros(N) # est with hm 
theda_hm_1 = 0
for m in range(int(sample_size[N-1])):
    u_1 = np.random.uniform(0,1)
    u_2 = np.random.uniform(0,1)
    if u_2 <= 1/2*(u_1**4 + u_1**2):
        theda_hm_1 += 1
    for n in range(N):
       if m == (int(sample_size[n])-1):
            theda_hm[n] = theda_hm_1 / sample_size[n]
    
error_hm =  np.abs(2*theda_hm - 2*(1/10 + 1/6)) # error in mc method

plt.loglog(sample_size,error_mc,label="MC")
plt.loglog(sample_size,error_hm,label="HM")
plt.loglog(sample_size,1/np.sqrt(sample_size)) # plot graph
plt.legend()

#Q7
N = np.zeros(10)
for n in range(10):
    N[n] = 10 + n
M = 1024
theda_mc = np.zeros(10)  # est with mc

for i in range(10):
    for n in range(int(N[i])):
        for m in range(M):
          theda_mc_1 = 0
          u = np.random.uniform(0,1)
          theda_mc_1 += 1/2 * (u**4 + u**2)
    theda_mc[i] += (np.abs(2/M * theda_mc_1 - 2 * (1/10 + 1/6)))**2
    theda_mc[i] = np.sqrt(theda_mc[i]/int(N[i]))

print(theda_mc)


theda_hm = np.zeros(10)  # est with mc

for i in range(10):
    for n in range(int(N[i])):
        for m in range(M):
          theda_hm_1 = 0
          u_1 = np.random.uniform(0,1)
          u_2 = np.random.uniform(0,1)
          if u_2 <= 1/2 * (u_1**4 + u_1**2):
              theda_hm_1 += 1          
    theda_hm[i] += (np.abs(2/M * theda_hm_1 - 2 * (1/10 + 1/6)))**2
    theda_hm[i] = np.sqrt(theda_hm[i]/int(N[i]))

print(theda_hm)

plt.plot(N,theda_mc,label="MC")
plt.plot(N,theda_hm,label="HM")
plt.legend()



