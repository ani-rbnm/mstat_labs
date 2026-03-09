#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Bank Data - Regression Forecast.
--
-- Description: In this script an OLS model is fitted for the bank data set.
--
-- Content:     0. Set-up
--              1. Data
--              2. Ordinary Least Squares
--              3. Publisher's Imprint
--
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Version  Date        Author    Major Changes
1.0      2020-02-13  ABZ       Initialization
1.1      2023-12-21  MLT       Updated version
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
"""

########################################################################################################################
# 0. Set-ups
########################################################################################################################

# Generally required
import pandas as pd
import numpy as np

# Plotting
import matplotlib
import seaborn as sns
import matplotlib.pyplot as plt
# matplotlib.use('Qt5Agg')
sns.set_style("whitegrid")

# OLS
from statsmodels.formula.api import ols
from statsmodels.tsa.api import Holt

########################################################################################################################
# 1. Data
########################################################################################################################

# Define file path
file = "Chapter 4 Data and Codes 2024/Chapter 4 Demonstrations Material/Bank.xls"

# Load bank data set
series = pd.read_excel(file, sheet_name='Data2', header=0, dtype=float).squeeze()

# Reading the original DEOM time series for all the 59 periods
DEOMfull0 = pd.read_excel(file, sheet_name='Data4', header=0, dtype=float).squeeze()

# reading the basic variables
DEOM = series.DEOM
AAA = series.AAA
Tto4 = series.Tto4
D3to4 = series.D3to4

########################################################################################################################
# 2. Holt's Linear Model
########################################################################################################################

# Forecasting for AAA using Holt's linear method
fit1 = Holt(AAA).fit(optimized=True)
fcast1 = fit1.forecast(6).rename("Additive 2 damped trend")

# Forecasting for Tto4 using Holt's linear method
fit2 = Holt(Tto4).fit(optimized=True)
fcast2 = fit2.forecast(6).rename("Additive 2 damped trend")

# Forecasting for D3to4 using Holt's linear method
fit3 = Holt(D3to4).fit(smoothing_level=0.8, smoothing_trend=0.2, optimized=False)
fcast3 = fit3.forecast(6).rename("Additive 2 damped trend")

# putting the fitted values of the forecasts of AAA, Tto4, and D3to4 in arrays
a1 = np.array(fit1.fittedvalues)
a2 = np.array(fit2.fittedvalues)
a3 = np.array(fit3.fittedvalues)

# putting the values of the forecasts of AAA, Tto4, and D3to4 in arrays
v1 = np.array(fcast1)
v2 = np.array(fcast2)
v3 = np.array(fcast3)

########################################################################################################################
# 4. OLS
########################################################################################################################

# Building the regression based forecast for main variable, DEOM
# Regression model(s)
formula = 'DEOM ~ AAA + Tto4 + D3to4'

# ols generate statistics and the parameters b0, b1, etc., of the model
results = ols(formula, data=series).fit()
results.summary()

b0 = results.params.Intercept
b1 = results.params.AAA
b2 = results.params.Tto4
b3 = results.params.D3to4

########################################################################################################################
# 5. DEOM Forecast
########################################################################################################################

# Building the fitted part of the forecast of DEOM
F = a1
for i in range(53):
    F[i] = b0 + a1[i]*b1 + a2[i]*b2 + a3[i]*b3

# Building the 6 values of the forecast ahead
E = v1
for i in range(6):
    E[i] = b0 + v1[i]*b1 + v2[i]*b2 + v3[i]*b3

# Joining the fitted values of the forecast and the points ahead
K = np.append(F, E)

########################################################################################################################
# 5. Forecasting Error
########################################################################################################################

# Evaluating the MSE to generate the confidence interval
DEOMfull = DEOMfull0.DEOMfull
values = DEOMfull[0:53]
Error = values - F
MSE = sum(Error ** 2) * 1.0 / len(F)

# Lower and upper bounds of forecasts for z=1.282; see equation (2.2) in Chap 2.
# LowerE = E - 1.282*MSE
# UpperE = E + 1.282*MSE

LowerE = DEOMfull0.LowerE
UpperE = DEOMfull0.UpperE

########################################################################################################################
# 3. Plotting
########################################################################################################################

fig, ax = plt.subplots(4, 1, figsize=(10, 15))

fit1.fittedvalues.plot(color='red', ax=ax[0])
fcast1.plot(color='red', legend=True, ax=ax[0])
AAA.plot(color='black', legend=True, ax=ax[0])
ax[0].set_title('Forecast of AAA with Holt linear method')

fit2.fittedvalues.plot(color='red', ax=ax[1])
fcast2.plot(color='red', legend=True, ax=ax[1])
Tto4.plot(color='black', legend=True, ax=ax[1])
ax[1].set_title('Forecast of 3to4 with Holt linear method')

fit3.fittedvalues.plot(color='red', ax=ax[2])
fcast3.plot(color='red', legend=True, ax=ax[2])
D3to4.plot(color='black', legend=True, ax=ax[2])
ax[2].set_title('Forecast of D3to4 with Holt linear method')

# Plotting the graphs of K and DEOMfull with legends
ax[3].plot(K, color='red', label='Forecast values')
ax[3].plot(DEOMfull, color='black', label='Original data')
ax[3].fill_between(range(0, 59), LowerE, UpperE, color='b', alpha=.1, label="Confidence Interval")
ax[3].legend()
ax[3].set_title('DEOM regression forecast with confidence interval')

plt.tight_layout()
plt.show()

# Proceeding as in other demos, forecasts
# can be generated for other scenarios; i.e., with different combinations of variables

########################################################################################################################
# 3. Publisher's Imprint
########################################################################################################################

__author__ = ["Alain Zemkoho"]
__credits__ = ["Marah-Lisanne Thormann"]
__version__ = "1.1"
__maintainer__ = "Alain Zemkoho"
__email__ = "A.B.Zemkoho@soton.ac.uk"

########################################################################################################################
########################################################################################################################
