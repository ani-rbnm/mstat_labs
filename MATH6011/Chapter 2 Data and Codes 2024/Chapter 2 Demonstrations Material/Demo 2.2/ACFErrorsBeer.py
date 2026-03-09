#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Beer Data - Autocorrelation Function (ACF).
--
-- Description: Visualization of the ACF for the Beer data set.
--
-- Content:     0. Set-up
--              1. Data
--              2. Plotting
--              3. Publisher's Imprint
--
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Version  Date        Author    Major Changes
1.0      2020-02-06  ABZ       Initialization
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

# Autocorrelation Function
from pandas.plotting import autocorrelation_plot

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
# 2. Plotting
########################################################################################################################

# Create figure
fig, ax = plt.subplots(3, 1, figsize=(10, 7))

# Plot for the original data set
AustralianBeer.plot(label='Original data', legend=True, ax=ax[0])

# Evaluating the errors from both NF1 and NF2 methods
Error1 = AustralianBeer - NaiveF1
Error2 = AustralianBeer - NaiveF2

# Plot of the error time series
Error1.plot(label='NF1 error plot', legend=True, ax=ax[1])
Error2.plot(label='NF2 error plot', legend=True, ax=ax[1], linestyle="--")

# Creating an autocorrelation plot
autocorrelation_plot(Error1, ax=ax[2])
autocorrelation_plot(Error2,  linestyle="--", ax=ax[2])

# Tight layout and show plot
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
