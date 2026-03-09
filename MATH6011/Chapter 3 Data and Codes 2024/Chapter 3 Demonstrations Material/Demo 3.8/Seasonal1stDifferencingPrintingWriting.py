#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Printing & Writing Data - First and Seasonal Difference.
--
-- Description: In this script the first and seasonal difference is calculated based on the Printing & Writing Data.
--
-- Content:     0. Set-up
--              1. Data
--              2. Plotting
--              3. Publisher's Imprint
--
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Version  Date        Author    Major Changes
1.0      2020-02-19  ABZ       Initialization
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

# ACF, PACF
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf

########################################################################################################################
# 1. Data
########################################################################################################################

# Define file path
file = "Chapter 3 Data and Codes 2024/Chapter 3 Demonstrations Material/Demo 3.8/PrintingWriting.xls"

# Load printing and writing data
series = pd.read_excel(file, sheet_name='Data2', header=0, index_col=0, parse_dates=True).squeeze()

#  Seasonal difference
X = series.values
SeasDiff = list()
for i in range(12, len(X)):
	value = X[i] - X[i - 12]
	SeasDiff.append(value)

# Seasonal + First difference
Y = SeasDiff
SeasFirstDiff = list()
for i in range(1, len(Y)):
	value = Y[i] - Y[i - 1]
	SeasFirstDiff.append(value)

########################################################################################################################
# 2. Plotting
########################################################################################################################

# Create figure
fig, ax = plt.subplots(3, 1, figsize=(10, 7))

# Time, ACF, and PACF plots for original data
ax[0].plot(series)
ax[0].set_title('Time plot original data')
plot_acf(series, title='ACF plot of original data', lags=50, ax=ax[1])
plot_pacf(series, title='PACF plot of original data', lags=50, ax=ax[2])
plt.tight_layout()
plt.show()

# Create figure
fig, ax = plt.subplots(3, 1, figsize=(10, 7))

# Time, ACF, and PACF plots for the seasonally differenced series
ax[0].plot(SeasDiff)
ax[0].set_title('Time plot seasonally differenced series')
plot_acf(SeasDiff, title='ACF plot of seasonally differenced series', lags=50, ax=ax[1])
plot_pacf(SeasDiff, title='PACF plot of seasonally differenced series', lags=50, ax=ax[2])
plt.tight_layout()
plt.show()

# Create figure
fig, ax = plt.subplots(3, 1, figsize=(10, 7))

ax[0].plot(SeasFirstDiff)
ax[0].set_title('Time plot seasonally + first differenced series')
plot_acf(SeasFirstDiff, title='ACF plot of seasonally + first differenced series', lags=50, ax=ax[1])
plot_pacf(SeasFirstDiff, title='PACF plot of seasonally + first differenced series', lags=50, ax=ax[2])
plt.tight_layout()
plt.show()

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

