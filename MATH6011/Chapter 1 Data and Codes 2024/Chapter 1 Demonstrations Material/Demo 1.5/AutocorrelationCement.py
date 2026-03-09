#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Cement Production Data - Seasonal Plot and Autocorrelation Function (ACF).
--
-- Description: In this script the seasonal plot and ACF for the Cement Production data set are visualized.
--
-- Content:     0. Set-up
--              1. Data
--              2. Seasonal Plot
--              3. Autocorrelation Function (ACF)
--              4. Publisher's Imprint
--
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Version  Date        Author    Major Changes
1.0      2021-09-22  ABZ       Initialization
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

# ACF
from pandas.plotting import autocorrelation_plot
from statsmodels.graphics.tsaplots import plot_acf

########################################################################################################################
# 1. Data
########################################################################################################################

# Define file path
file = "Chapter 1 Data and Codes 2024/Chapter 1 Demonstrations Material/Demo 1.5/CementProduction.xls"

# Load cement production data
series = pd.read_excel(file, sheet_name='Data', header=0, index_col=0, parse_dates=True).squeeze()

# Create empty DataFrame for seasonal data
seasonal_data = pd.DataFrame(index=np.unique(series.index.month), columns=np.unique(series.index.year))

# Fill DataFrame with seasonal data
for year in np.unique(series.index.year):
    seasonal_data.loc[:np.sum(series.index.year == year), year] = series[series.index.year == year].values

########################################################################################################################
# 2. Seasonal Plot
########################################################################################################################

# Select a few years
years = [2001, 2002, 2003, 2005, 2006, 2007]

# Create seasonal plot
seasonal_data.loc[:, years].plot(title="Cement Production", xlabel="Month",
                                 ylabel="Thousand Tonnes")
plt.show()

########################################################################################################################
# 3. Autocorrelation Function (ACF)
########################################################################################################################

# from pandas - generate ACF in curve format
autocorrelation_plot(series)

# from statsmodels - generates ACF in "lollipop plot" format
plot_acf(series, title='ACF plot of building materials time series', lags=60)

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
