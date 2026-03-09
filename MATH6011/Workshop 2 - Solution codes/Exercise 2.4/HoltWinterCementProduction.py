#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Cement Production - Exponential Smoothing.
--
-- Description: In this script the cement production data is used to fit serval models based on exponential smoothing.
--
-- Content:     0. Set-up
--              1. Data
--              2. Holt-Winter Method
--                  2.0 Model 1
--                  2.1 Model 2
--                  2.2 Model 3
--                  2.3 Model 4
--              3. Plotting
--                  3.0 Forecast
--                  3.1 Residuals
--                  3.2 ACF
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
matplotlib.use('Qt5Agg')
sns.set_style("whitegrid")

# Exponential Smoothing
from statsmodels.tsa.api import ExponentialSmoothing

# Forecasting error
from sklearn.metrics import mean_squared_error

# ACF
from statsmodels.graphics.tsaplots import plot_acf

########################################################################################################################
# 1. Data
########################################################################################################################

# Define file path
file = "Workshop 2 - Solution codes/Exercise 2.4/CementProduction.xls"

# Load cement production data
series = pd.read_excel(file, sheet_name='Data', header=0, index_col=0, parse_dates=True).squeeze()

# Add frequency to index
series.index = pd.DatetimeIndex(series.index.values, freq=series.index.inferred_freq)

########################################################################################################################
# 2. Holt-Winter Method
########################################################################################################################
######################################################################################################
# 2.0 Model 1
######################################################################################################

# Holt-Winter method with additive trend and seasonality
# Here, alpha = 0.3, beta=0.5, gamma=0.7
fit1 = ExponentialSmoothing(series, seasonal_periods=12, trend='add', seasonal='add').fit(smoothing_level=0.3,
                                                                                          smoothing_trend=0.5,
                                                                                          smoothing_seasonal=0.7)
fit1.fittedvalues.plot(color='red')

# Compute residuals
residuals1 = fit1.fittedvalues - series

######################################################################################################
# 2.1 Model 2
######################################################################################################

# Holt-Winter method with additive trend and multiplicative seasonality
# Here, alpha = 0.3, beta=0.5, gamma=0.7
fit2 = ExponentialSmoothing(series, seasonal_periods=12, trend='add', seasonal='mul').fit(smoothing_level=0.3,
                                                                                          smoothing_trend=0.5,
                                                                                          smoothing_seasonal=0.7)
fit2.fittedvalues.plot(color='blue')

# Compute residuals
residuals2 = fit2.fittedvalues - series

######################################################################################################
# 2.2 Model 3
######################################################################################################

# Holt-Winter method with additive trend and seasonality
# Here, the parameters alpha, beta, and gamma are optimized
fit3 = ExponentialSmoothing(series, seasonal_periods=12, trend='add', seasonal='add').fit()
fit3.fittedvalues.plot(color='green')

# Compute residuals
residuals3 = fit3.fittedvalues - series

######################################################################################################
# 2.3 Model 4
######################################################################################################

# Model 4: Holt-Winter method with additive trend and multiplicative seasonality
# Here, the parameters alpha, beta, and gamma are optimized
fit4 = ExponentialSmoothing(series, seasonal_periods=12, trend='add', seasonal='mul').fit()
fit4.fittedvalues.plot(color='yellow')

# Compute residuals
residuals4 = fit4.fittedvalues - series

########################################################################################################################
# 3. Plotting
########################################################################################################################
######################################################################################################
# 3.0 Forecast
######################################################################################################

# Create figure
fig, ax = plt.subplots(1, 1, figsize=(15, 10))

series.rename('Time plot of original series').plot(color='black', legend=True, ax=ax)
fit1.forecast(12).rename('Model 1: HW-additive seasonality').plot(color='red', legend=True, ax=ax)
fit2.forecast(12).rename('Model 2: HW-multiplicative seasonality').plot(color='blue', legend=True, ax=ax)
fit3.forecast(12).rename('Model 3: Opt HW-additive seasonality').plot(color='green', legend=True, ax=ax)
fit4.forecast(12).rename('Model 4: Opt HW-multiplicative seasonality').plot(color='yellow', legend=True, ax=ax)
plt.xlabel('Dates')
plt.ylabel('Values')
plt.title('HW method-based forecasts for cement production')

plt.tight_layout()
plt.show()

######################################################################################################
# 3.1 Residuals
######################################################################################################

# Create figure
fig, ax = plt.subplots(4, 1, figsize=(10, 15))

# Evaluating and plotting the residual series for each scenario
residuals1.rename('residual plot for model 1').plot(color='red', legend=True, ax=ax[0])
residuals2.rename('residual plot for model 2').plot(color='blue', legend=True, ax=ax[1])
residuals3.rename('residual plot for model 3').plot(color='green', legend=True, ax=ax[2])
residuals4.rename('residual plot for model 4').plot(color='yellow', legend=True, ax=ax[3])
ax[0].set_title('Residual plots for models 1-4')

plt.tight_layout()
plt.show()

######################################################################################################
# 3.2 ACF
######################################################################################################

# Create figure
fig, ax = plt.subplots(4, 1, figsize=(10, 15))

# Plot ACF for the residuals of all models
plot_acf(residuals1, title='Residual ACF for model 1', lags=50, ax=ax[0])
plot_acf(residuals2, title='Residual ACF for model 2', lags=50, ax=ax[1])
plot_acf(residuals3, title='Residual ACF for model 3', lags=50, ax=ax[2])
plot_acf(residuals4, title='Residual ACF for model 4', lags=50, ax=ax[3])

plt.tight_layout()
plt.show()

########################################################################################################################
# 4. Forecasting Error
########################################################################################################################

# Calculate MSE per model
MSE1 = mean_squared_error(fit1.fittedvalues, series)
MSE2 = mean_squared_error(fit2.fittedvalues, series)
MSE3 = mean_squared_error(fit3.fittedvalues, series)
MSE4 = mean_squared_error(fit4.fittedvalues, series)

# Printing the parameters and errors for each scenario
results = pd.DataFrame(index=[r"alpha", r"beta", r"gamma", r"l0", "b0", "MSE"])
params = ['smoothing_level', 'smoothing_trend', 'smoothing_seasonal', 'initial_level', 'initial_trend']
results["HW model 1"] = [fit1.params[p] for p in params] + [MSE1]
results["HW model 2"] = [fit2.params[p] for p in params] + [MSE2]
results["HW model 3"] = [fit3.params[p] for p in params] + [MSE3]
results["HW model 4"] = [fit4.params[p] for p in params] + [MSE4]
print(results)

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


