#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Airline Sales - Exponential Smoothing.
--
-- Description: In this script the airline sales data is used for exponential smoothing.
--
-- Content:     0. Set-up
--              1. Data
--              2. Exponential Smoothing
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
from statsmodels.tsa.api import ExponentialSmoothing

########################################################################################################################
# 1. Data
########################################################################################################################

# Define file path
file = "Chapter 2 Data and Codes 2024/Chapter 2 Demonstrations Material/Demo 2.5/AirlineSales.xls"

# Load airline sales data
series = pd.read_excel(file, sheet_name='Data', header=0, index_col=0, parse_dates=True).squeeze()

# Add frequency to index
series.index = pd.DatetimeIndex(series.index.values, freq=series.index.inferred_freq)

########################################################################################################################
# 2. Exponential Smoothing
########################################################################################################################

fit1 = ExponentialSmoothing(series, seasonal_periods=12, trend='add', seasonal='add').fit(smoothing_level=0.3,
                                                                                          smoothing_slope=0.5,
                                                                                          smoothing_seasonal=0.7)

fit2 = ExponentialSmoothing(series, seasonal_periods=12, trend='add', seasonal='mul').fit()

fit3 = ExponentialSmoothing(series, seasonal_periods=12, trend='add', seasonal='add', damped_trend=True).fit()

fit4 = ExponentialSmoothing(series, seasonal_periods=12, trend='add', seasonal='mul', damped_trend=True).fit()

########################################################################################################################
# 3. Plotting
########################################################################################################################

# Create figure
fig, ax = plt.subplots(1, 1, figsize=(15, 10))

series.rename('Time plot of original series').plot(color='black', legend=True, ax=ax)

fit1.fittedvalues.plot(color='red', ax=ax)
fit1.forecast(12).rename('Model 1: HW-additive seasonality').plot(color='red', legend=True, ax=ax)

fit2.forecast(12).rename('Model 2: HW-multiplicative seasonality').plot(color='blue', legend=True, ax=ax)
fit1.fittedvalues.plot(color='blue', ax=ax)

fit1.fittedvalues.plot(color='green', ax=ax)
fit3.forecast(12).rename('Model 3: Opt HW-additive seasonality').plot(color='green', legend=True, ax=ax)

fit1.fittedvalues.plot(color='yellow', ax=ax)
fit4.forecast(12).rename('Model 4: Opt HW-multiplicative seasonality').plot(color='yellow', legend=True, ax=ax)

plt.xlabel('Dates')
plt.ylabel('Values')
plt.title('HW method-based forecasts for cement production')

plt.tight_layout()
plt.show()

########################################################################################################################
# 4. Publisher's Imprint
########################################################################################################################

__author__ = ["Alain Zemkoho"]
__credits__ = ["Marah-Lisanne Thormann"]
__version__ = "1.1"
__maintainer__ = "Alain Zemkoho"
__email__ = "A.B.Zemkoho@soton.ac.uk"

########################################################################################################################
########################################################################################################################
