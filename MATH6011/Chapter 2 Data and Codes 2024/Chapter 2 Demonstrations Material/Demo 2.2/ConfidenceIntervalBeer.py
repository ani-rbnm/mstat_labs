#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Beer Data - Confidence Intervals (CIs).
--
-- Description: In this script the beer data is used to calculate confidence intervals for the naive forecast 1 and 2.
--
-- Content:     0. Set-up
--              1. Data
--              2. Confidence Intervals
--              3. Plotting
--              4. Publisher's Imprint
--
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Version  Date        Author    Major Changes
1.0      2021-09-25  ABZ       Initialization
1.1      2023-12-14  MLT       Updated version
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
"""
########################################################################################################################
# 0. Set-ups
########################################################################################################################

# Read in data
import pandas as pd
import numpy as np

# Plotting
import matplotlib
import seaborn as sns
import matplotlib.pyplot as plt
# matplotlib.use('Qt5Agg')
sns.set_style("whitegrid")

########################################################################################################################
# 1. Data
########################################################################################################################

# Define file path
file = "Chapter 2 Data and Codes 2024/Chapter 2 Demonstrations Material/Demo 2.2/BeerErrorsData.xls"

# Define sheet name
s_name = 'NF1NF2'

# Australian Beer data set
AustralianBeer = pd.read_excel(file, sheet_name=s_name, usecols=[1], header=0, dtype=float).squeeze()

# Naive Forecast 1
NaiveF1 = pd.read_excel(file, sheet_name=s_name, usecols=[2], header=0, dtype=float).squeeze()

# Naive Forecast 2
NaiveF2 = pd.read_excel(file, sheet_name=s_name, usecols=[3], header=0, dtype=float).squeeze()

########################################################################################################################
# 2. Confidence Intervals
########################################################################################################################

# Evaluating the errors from both NF1 and NF2 methods
Error1 = AustralianBeer - NaiveF1
Error2 = AustralianBeer - NaiveF2

# Mean Squared Error
MSE1 = sum(Error1 ** 2) * 1.0 / len(NaiveF1)
MSE2 = sum(Error2 ** 2) * 1.0 / len(NaiveF2)

# Lower and Upper CI for Naive Forecast 1
LowerForecast1 = NaiveF1 - 1.645 * np.sqrt(MSE1)
UpperForecast1 = NaiveF1 + 1.645 * np.sqrt(MSE1)

# Lower and Upper CI for Naive Forecast 2
LowerForecast2 = NaiveF2 - 1.645 * np.sqrt(MSE2)
UpperForecast2 = NaiveF2 + 1.645 * np.sqrt(MSE2)

########################################################################################################################
# 3. Plotting
########################################################################################################################

# Create figure
fig, ax = plt.subplots(2, 1, figsize=(10, 7))

# Joint plot of original data and NF1 forecasts
AustralianBeer.plot(label='Original data', ax=ax[0])
NaiveF1.plot(label='NF1 forecast', ax=ax[0])

# Create CIs
ax[0].fill_between(np.arange(0, len(NaiveF1)), LowerForecast1, UpperForecast1, color='b', alpha=.1,
                   label="Confidence Interval")
ax[0].legend()

# Joint plot of original data and NF2 forecasts
AustralianBeer.plot(label='Original data', ax=ax[1])
NaiveF2.plot(label='NF2 forecast', ax=ax[1])

# Create CIs
ax[1].fill_between(np.arange(0, len(NaiveF2)), LowerForecast2, UpperForecast2, color='b', alpha=.1,
                   label="Confidence Interval")
ax[1].legend()
plt.tight_layout()
plt.show()

########################################################################################################################
# 4. Publisher's Imprint
########################################################################################################################

__author__ = ["Alain Zemkoho"]
__credits__ = ["Marah-Lisanne Thormann"]
__version__ = "1.0"
__maintainer__ = "Alain Zemkoho"
__email__ = "A.B.Zemkoho@soton.ac.uk"

########################################################################################################################
########################################################################################################################
