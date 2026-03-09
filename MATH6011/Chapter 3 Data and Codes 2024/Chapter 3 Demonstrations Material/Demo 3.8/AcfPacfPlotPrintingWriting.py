#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Printing & Writing Data - ACF and PACF.
--
-- Description: In this script the autocorrelation function (ACF) and the partial autocorrelation function (PACF) for
--              the DPrinting & Writing data set are visualized.
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

########################################################################################################################
# 2. Plotting
########################################################################################################################

# Create figure
fig, ax = plt.subplots(2, 1, figsize=(10, 7))

# ACF plot on 50 time lags
plot_acf(series, title='ACF printing & writing paper sales', lags=50, ax=ax[0])

# PACF plot on 50 time lags
plot_pacf(series, title='PACF printing & writing paper sales', lags=50, ax=ax[1])

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
