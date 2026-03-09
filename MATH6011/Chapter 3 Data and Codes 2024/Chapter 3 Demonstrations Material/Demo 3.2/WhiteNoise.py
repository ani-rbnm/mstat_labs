#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- White Noise Model.
--
-- Description: In this script a white noice process is simulated and visualized.
--
-- Content:     0. Set-up
--              1. Data
--              2. Plotting
--              3. Publisher's Imprint
--
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Version  Date        Author    Major Changes
1.0      2020-02-18  ABZ       Initialization
1.1      2023-12-21  MLT       Updated version
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
"""
########################################################################################################################
# 0. Set-ups
########################################################################################################################

# Generally required
import pandas as pd
import random

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

# seed random number generator
random.seed(1)

# create white noise series
series = [random.gauss(0.0, 1.0) for i in range(1000)]

# Once created, we can wrap the list in a Pandas Series for convenience.
series = pd.Series(series)

# summary statistics of the artificially generated series
print('Statistics of the artificially generated series:')
print(series.describe())

########################################################################################################################
# 2. Plotting
########################################################################################################################

fig, ax = plt.subplots(4, 1, figsize=(10, 15))

# line plot of the artificially generated series
series.plot(title='Time plot of a white noise model', ax=ax[0])
plt.show()

# histogram plot of the artificially generated series
series.hist(ax=ax[1])

# ACF plot of an artificially generated white noise time series
plot_acf(series, title='ACF of a white noise model', lags=50, ax=ax[2])

# PACF plot of an artificially generated white noise time series
plot_pacf(series, title='PACF of a white noise model', lags=50, ax=ax[3])

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
