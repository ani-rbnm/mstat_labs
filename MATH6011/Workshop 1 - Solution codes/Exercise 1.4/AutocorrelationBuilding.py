#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Building Materials Data - Autocorrelation Function (ACF).
--
-- Description: In this script the ACF polt for the Building Materials data set is created.
--
-- Content:     0. Set-up
--              1. Data
--              2. ACF Plot
--              3. Publisher's Imprint
--
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Version  Date        Author    Major Changes
1.0      2020-01-29  ABZ       Initialization
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
matplotlib.use('Qt5Agg')
sns.set_style("whitegrid")

# ACF
from pandas.plotting import autocorrelation_plot
from statsmodels.graphics.tsaplots import plot_acf

########################################################################################################################
# 1. Data
########################################################################################################################

# Define file path
file = "Workshop 1 - Solution codes/Exercise 1.4/BuildingMaterials.xls"

# Load building materials data
series = pd.read_excel(file, sheet_name='Data', header=0, index_col=0, parse_dates=True).squeeze()

########################################################################################################################
# 2. ACF Plot
########################################################################################################################

# from pandas - generate ACF in curve format
autocorrelation_plot(series)
plt.title('ACF plot building materials - line format')

# from statsmodels - generates ACF in "lollipop plot" format
plot_acf(series, title='ACF plot building materials - histogram format', lags=100)
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


