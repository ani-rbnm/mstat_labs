#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Shampoo Sales - Holt Models.
--
-- Description: In this script the shampoo sales data is used to fit serval Holt models.
--
-- Content:     0. Set-up
--              1. Data
--              2. Holt Models
--              3. Plotting
--              4. Forecasting Error
--              5. Publisher's Imprint
--
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Version  Date        Author    Major Changes
1.0      2020-02-04  ABZ       Initialization
1.1      2023-12-22  MLT       Updated version
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
"""
########################################################################################################################
# 0. Set-ups
########################################################################################################################

# Read in data
import pandas as pd

# Plotting
import matplotlib
import seaborn as sns
import matplotlib.pyplot as plt
# matplotlib.use('Qt5Agg')
sns.set_style("whitegrid")

# Exponential Smoothing
from statsmodels.tsa.api import Holt

# Forecasting Error
from sklearn.metrics import mean_squared_error

########################################################################################################################
# 1. Data
########################################################################################################################

# Define file path
file = "Chapter 2 Data and Codes 2024/Chapter 2 Demonstrations Material/Demo 2.4/ShampooSales.xls"

# Load shampoo sales data
series = pd.read_excel(file, sheet_name='Data', header=0, index_col=0).squeeze()

# Add frequency to index
series.index = pd.DatetimeIndex(series.index.values, freq=series.index.inferred_freq)

########################################################################################################################
# 2. Holt Models
########################################################################################################################

fit1 = Holt(series).fit(smoothing_level=0.8, smoothing_trend=0.2, optimized=False)
fcast1 = fit1.forecast(12).rename("Holt's linear trend")

fit2 = Holt(series, exponential=True).fit(smoothing_level=0.8, smoothing_trend=0.2, optimized=False)
fcast2 = fit2.forecast(12).rename("Exponential trend")

fit3 = Holt(series, damped_trend=True).fit(smoothing_level=0.8, smoothing_trend=0.2)
fcast3 = fit3.forecast(12).rename("Additive damped trend")

fit4 = Holt(series).fit(optimized=True)
fcast4 = fit4.forecast(12).rename("Additive 2 damped trend")

########################################################################################################################
# 3. Plotting
########################################################################################################################

# Create figure
fig, ax = plt.subplots(1, 1, figsize=(15, 10))

fit1.fittedvalues.plot(color='blue', ax=ax)
fcast1.plot(color='blue', legend=True, ax=ax)

fit2.fittedvalues.plot(color='red', ax=ax)
fcast2.plot(color='red', legend=True, ax=ax)

fit3.fittedvalues.plot(color='green', ax=ax)
fcast3.plot(color='green', legend=True, ax=ax)

fit4.fittedvalues.plot(color='yellow', ax=ax)
fcast4.plot(color='yellow', legend=True, ax=ax)

series.plot(color='black', legend=True, ax=ax)

plt.tight_layout()
plt.show()

########################################################################################################################
# 4. Forecasting Error
########################################################################################################################

# Compute MSE per model
MSE1 = mean_squared_error(fit1.fittedvalues, series)
MSE2 = mean_squared_error(fit2.fittedvalues, series)
MSE3 = mean_squared_error(fit3.fittedvalues, series)

print('Summary of errors resulting from SES models 1, 2 & 3:')
summary = {'Model': ['MSE'],
           'LES model 1': [MSE1],
           'LES model 2': [MSE2],
           'LES model 3': [MSE3]
           }
AllErrors = pd.DataFrame(summary, columns=['Model', 'LES model 1', 'LES model 2', 'LES model 3'])
print(AllErrors)

########################################################################################################################
# 5. Publisher's Imprint
########################################################################################################################

__author__ = ["Alain Zemkoho"]
__credits__ = ["Marah-Lisanne Thormann"]
__version__ = "1.1"
__maintainer__ = "Alain Zemkoho"
__email__ = "A.B.Zemkoho@soton.ac.uk"

########################################################################################################################
########################################################################################################################

