#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Oct 18 00:27:15 2020

@author: Ko-Cheng Chang
"""

import numpy as np
import matplotlib.pyplot as plt
X0 = 1
mu = 1
sigma = 1
sigma2 = sigma**2
seed = 518


# Q1

N  = 2**(10)
h = 1/N
np.random.seed(seed)
inc_b = np.random.normal(0, 1, int(N))*np.sqrt(h) #increments
mot_b = np.cumsum(inc_b)  # brownian motion
mot_b = np.insert(mot_b, 0, 0) #adding W(0) = 0
xw = np.linspace(0, 1, len(mot_b))
plt.plot(xw, mot_b)
for o in range(9):
    inc_b = [sum(inc_b[i:i+2]) for i in range(0, len(inc_b), 2)]
    mot_b = np.cumsum(inc_b)
    mot_b = np.insert(mot_b, 0, 0) #adding W(0) = 0
    xw = np.linspace(0, 1, len(mot_b))
    plt.plot(xw, mot_b)
plt.title('Brownian Motion')
plt.savefig('Brownian Motion.pdf')

    
#Q2


N  = 2**(10)
h = 1/N
np.random.seed(seed)
inc_b = np.random.normal(0, 1, int(N))*np.sqrt(h) #increments
mot_b = np.cumsum(inc_b)  # brownian motion
mot_b = np.insert(mot_b, 0, 0) #adding W(0) = 0
x_h = [1]
for i in range(len(inc_b)):
    x_temp = (1+h*mu)*x_h[i] + sigma * x_h[i]*(inc_b[i])
    x_h.append(x_temp)
xw = np.linspace(0,1,len(x_h))
plt.plot(xw, x_h)
for o in range(9):
    inc_b = [sum(inc_b[i:i+2]) for i in range(0, len(inc_b), 2)]
    mot_b = np.cumsum(inc_b)  # brownian motion
    mot_b = np.insert(mot_b, 0, 0) #adding W(0) = 0
    x_h = [1]
    for ii in range(len(inc_b)):
        x_temp = (1+h*mu)*x_h[ii] + sigma * x_h[ii]*(mot_b[ii+1]-mot_b[ii])
        x_h.append(x_temp)
    xw = np.linspace(0,1,len(x_h))
    plt.plot(xw, x_h)
plt.title('path of X')
plt.savefig('path of X.pdf')


#Q3
err = []
for o in range(10):
   str_err = []
   for ii in range(5000):# Monte Carlo method
       N  = 2**(o+1)
       h = 1/N
       inc_b = np.random.normal(0, 1, int(N))*np.sqrt(h) #increments
       mot_b = np.cumsum(inc_b) # brownian motion
       mot_b = np.insert(mot_b, 0, 0)
       x_h = [1]
       for i in range(int(N)):
         x_temp = (1+h*mu)*x_h[i] + sigma*x_h[i]*(mot_b[i+1]-mot_b[i])
         x_h.append(x_temp)
       xh_1 = x_h[-1]
       x_1 = np.exp((mu - (sigma2/2))*1 + sigma*mot_b[-1])
       temp = (x_1 - xh_1)**2
       str_err.append(temp)
   str_err = np.mean(str_err)**(1/2)
   err.append(str_err) # gen list of strong error based on different i
h = []
for i in range(10):
    N = 2**(i+1)
    h.append(1/N)
plt.loglog(h,err,'s')
plt.loglog(h, np.sqrt(h))
plt.title('Strong error')
plt.savefig('Strong error.pdf')

#Q4
err = []
for o in range(10):
   weak_err = []
   for ii in range(5000): # Monte Carlo method
       N  = 2**(o+1)
       h = 1/N
       inc_b = np.random.normal(0, 1, int(N))*np.sqrt(h) #increments
       x_h = [1]
       for i in range(int(N)):
         x_temp = (1+h*mu)*x_h[i] + sigma*x_h[i]*inc_b[i]
         x_h.append(x_temp)
       xh_1 = x_h[-1]
       weak_err.append(xh_1)
   weak_err = np.mean(weak_err)
   err.append(np.abs(np.exp(mu) - weak_err)) # gen list of weak error based on different i
   
h = []
for i in range(10):
    N = 2**(i+1)
    h.append(1/N)
plt.loglog(h,err,'s')
plt.loglog(h, h)
plt.title('Weak error Q4')
plt.savefig('Weak error Q4.pdf')


#Q5
err = []
for o in range(10):
   weak_err = []
   mot_b = []
   for ii in range(5000):# Monte Carlo method
       N  = 2**(o+1)
       h = 1/N
       inc_b = np.random.normal(0, 1, int(N))*np.sqrt(h) #increments
       x_h = [1]
       mot_b.append(np.sum(inc_b))
       for i in range(int(N)):
         x_temp = (1+h*mu)*x_h[i] + sigma*x_h[i]*inc_b[i]
         x_h.append(x_temp)
       xh_1 = x_h[-1]**2
       weak_err.append(xh_1)

    