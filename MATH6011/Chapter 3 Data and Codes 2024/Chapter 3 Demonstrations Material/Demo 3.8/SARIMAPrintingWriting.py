#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Printing & Writing Data - SARIMA.
--
-- Description: In this script a SARIMA model is fitted for the Printing & Writing data.
--
-- Content:     0. Set-up
--              1. Data
--              2. SARIMA
--              3. Plotting
--              4. Forecasting Error
--              5. Publisher's Imprint
--
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Version  Date        Author    Major Changes
1.0      2020-02-18  ABZ       Initialization
1.1      2024-01-02  MLT       Updated version
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
"""
########################################################################################################################
# 0. Set-ups
########################################################################################################################

# Generally required
import pandas as pd

# Plotting
import matplotlib
import seaborn as sns
import matplotlib.pyplot as plt
# matplotlib.use('Qt5Agg')
sns.set_style("whitegrid")

# SARIMA
import statsmodels.api as sm

########################################################################################################################
# 1. Data
########################################################################################################################

# Define file path
file = "Chapter 3 Data and Codes 2024/Chapter 3 Demonstrations Material/Demo 3.8/PrintingWriting.xls"

# Load printing and writing data
series = pd.read_excel(file, sheet_name='Data2', header=0, index_col=0, parse_dates=True).squeeze()

# Add frequency to index
series.index = pd.DatetimeIndex(series.index.values, freq=series.index.inferred_freq)

########################################################################################################################
# 2. SARIMA
########################################################################################################################

# ARIMA model with (p, d, q)=(1, 1, 1)
# mod = sm.tsa.statespace.SARIMAX(series, trend='c', order=(1, 1, 1))

# SARIMA model
mod = sm.tsa.statespace.SARIMAX(series, order=(1, 1, 1), seasonal_order=(1, 1, 1, 12))

# Fit model
results = mod.fit(disp=False)

# Print model summary
print(results.summary())

# this code requires the fitted forecasts (for accuracy evaluation) to start 01 Jan 1979.
pred = results.get_prediction(start=pd.to_datetime('1972-01-01'), dynamic=False)

# Get confidence intervals
pred_ci = pred.conf_int()

# Print confidence intervals
print(pred_ci)

########################################################################################################################
# 3. Plotting
########################################################################################################################

# graphical statistics of model (correlogram = ACF plot)
results.plot_diagnostics(figsize=(15, 12))
plt.show()

# Create figure
fig, ax = plt.subplots(1, 1, figsize=(12, 7))

# this code requires the whole plot to start in 1969 (start year of data)
series['1969':].plot(label='Original data', ax=ax)
pred.predicted_mean.plot(ax=ax, label='One-step ahead Forecast', alpha=.7)

# Add confidence interval
ax.fill_between(pred_ci.index, pred_ci.iloc[:, 0], pred_ci.iloc[:, 1], color='k', alpha=.2)

# get forecast 20 steps ahead in future
pred_uc = results.get_forecast(steps=20)

# Get confidence intervals of forecasts
pred_ci = pred_uc.conf_int()

# plotting forecasts ahead
pred_uc.predicted_mean.plot(ax=ax, label='Forecast values', title='Forecast plot with confidence interval')

# Add confidence intervall
ax.fill_between(pred_ci.index, pred_ci.iloc[:, 0], pred_ci.iloc[:, 1], color='k', alpha=.25)

# Add legend
ax.legend()

plt.tight_layout()
plt.show()

########################################################################################################################
# 4. Forecasting Error
########################################################################################################################

# MSE evaluation
y_forecasted = pred.predicted_mean
y_truth = series["1972-01-01":]

# Compute the mean square error
mse = ((y_forecasted - y_truth) ** 2).mean()
print('MSE of the forecasts is {}'.format(round(mse, 2)))

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
