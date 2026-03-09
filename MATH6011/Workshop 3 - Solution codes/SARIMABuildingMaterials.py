#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Building Materials Data - SARIMA.
--
-- Description: In this script an SARIMA model is fitted based on the building materials data.
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
1.0      2022-03-21  ABZ       Initialization
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
matplotlib.use('Qt5Agg')
sns.set_style("whitegrid")

# SARIMA model
import statsmodels.api as sm

########################################################################################################################
# 1. Data
########################################################################################################################

# Define file path
file = "Workshop 3 - Solution codes/BuildingMaterials.xls"

# Load Building Materials data set
series = pd.read_excel(file, sheet_name="Data", index_col=0, header=0, parse_dates=True).squeeze()

# Add frequency to index
series.index = pd.DatetimeIndex(series.index.values, freq=series.index.inferred_freq)

########################################################################################################################
# 2. SARIMA
########################################################################################################################

# Fitting the ARIMA model and printing related statistics
# ARIMA(0, 1, 1)(0, 1, 1)12 in this case;
# this one is based on MA1 model template

mod = sm.tsa.statespace.SARIMAX(series, order=(1, 1, 1), seasonal_order=(0, 1, 1, 12))
results = mod.fit(disp=False)
print(results.summary())

# printing the part of forecasts fitted to original data (for accuracy evaluation)
# the start date has to be provided; hence should be a time within the original time series;
# in this case, it is to start on 01 January 2000
pred = results.get_prediction(start=pd.to_datetime('2000-01-01'), dynamic=False)
pred_ci = pred.conf_int()

# Get forecast 20 steps ahead in future
pred20_uc = results.get_forecast(steps=20)
# Get confidence intervals of forecasts
pred20_ci = pred20_uc.conf_int()

########################################################################################################################
# 3. Plotting
########################################################################################################################

# Printing the graphical statistics of model (correlogram = ACF plot)
results.plot_diagnostics(figsize=(15, 12))
plt.show()

# Create figure
fig, ax = plt.subplots(1, 1, figsize=(12, 7))

# printing one-step ahead forecasts together with the original data set;
# hence, the starting point (year) of the data set is required
# in order to build the plot of original series
series['1986':].plot(label='Original data', ax=ax)
pred.predicted_mean.plot(ax=ax, label='One-step ahead Forecast', alpha=.7)
ax.fill_between(pred_ci.index, pred_ci.iloc[:, 0], pred_ci.iloc[:, 1], color='k', alpha=.2)

# plotting forecasts ahead
series.plot(label='Original data', ax=ax)
pred20_uc.predicted_mean.plot(ax=ax, label='Forecast values', title='Forecast plot with confidence interval')
ax.fill_between(pred20_ci.index, pred20_ci.iloc[:, 0], pred20_ci.iloc[:, 1], color='k', alpha=.25)

plt.legend()
plt.tight_layout()
plt.show()

########################################################################################################################
# 4. Forecasting Error
########################################################################################################################

# MSE evaluation
y_forecasted = pred.predicted_mean
y_truth = series['2000-01-01':]

# Compute the mean square error
MSE = ((y_forecasted - y_truth) ** 2).mean()

# Print MSE
print('MSE of the forecasts is {}'.format(round(MSE, 2)))

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
