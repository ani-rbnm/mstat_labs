#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- MA1 Model - ACF and PACF.
--
-- Description: In this script the autocorrelation function (ACF) and the partial autocorrelation function (PACF) for
--              the MA1 model are visualized.
--
-- Content:     0. Set-up
--              1. Data
--              2. Plotting
--              3. Publisher's Imprint
--
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Version  Date        Author    Major Changes
1.0      2020-02-20  ABZ       Initialization
1.1      2023-12-21  MLT       Updated version
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
file = "Chapter 3 Data and Codes 2024/Chapter 3 Demonstrations Material/Demo 3.7/DataMA1model.xls"

# Load MA1 data
series = pd.read_excel(file, sheet_name='MAdata', usecols=[1], header=0).squeeze()

########################################################################################################################
# 2. Plotting
########################################################################################################################

# Create figure
fig, ax = plt.subplots(3, 1, figsize=(10, 7))

# Original time series
ax[0].plot(series)
ax[0].set_title('Time plot MA1 data')

# ACF
plot_acf(series, title='ACF plot of MA1 data', lags=20, ax=ax[1])

# PACF
plot_pacf(series, title='PACF plot of MA1 data', lags=20, ax=ax[2])

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
