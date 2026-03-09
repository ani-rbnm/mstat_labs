#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- House Sales Data - Trend Estimation.
--
-- Description: In this script the trend estimation is performed for the House Sales data.
--
-- Content:     0. Set-up
--              1. Data
--              2. Plotting
--              3. Publisher's Imprint
--
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Version  Date        Author    Major Changes
1.0      2020-01-30  ABZ       Initialization
1.1      2023-12-22  MLT       Updated version
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

########################################################################################################################
# 1. Data
########################################################################################################################

# Define file path
file = "Chapter 1 Data and Codes 2024/Chapter 1 Demonstrations Material/Demo 1.6/HouseSales.xls"

# Load house sale data
series = pd.read_excel(file, sheet_name='Data', header=0, index_col=0, parse_dates=True).squeeze()

# Create empty Series for MA7
MA7 = pd.Series(index=series.index)

# Fill series with MA7
for i in np.arange(3, len(series) - 3):
    MA7[i] = np.mean(series[(i-3):(i+4)])

# Create empty Series for MA2x12
MA2x12 = pd.Series(index=series.index)

# Fill series with MA2x12
for i in np.arange(6, len(series) - 6):
    MA2x12[i] = np.sum(series[(i-6):(i+7)] * np.concatenate([[1/24], np.repeat(1/12, 11), [1/24]]))

########################################################################################################################
# 2. Plotting
########################################################################################################################

# Plot original time series
series.plot()

# Plot MA7
MA7.plot()

# Plot MA2x12
MA2x12.plot()

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
