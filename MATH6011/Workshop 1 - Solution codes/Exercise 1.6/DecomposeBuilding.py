#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Building Materials Data - Seasonal Decomposition.
--
-- Description: In this script the seasonal decomposition is performed for the Building Materials data set.
--
-- Content:     0. Set-up
--              1. Data
--              2. Seasonal Decomposition
--              3. Publisher's Imprint
--
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Version  Date        Author    Major Changes
1.0      2020-01-15  ABZ       Initialization
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

# Seasonal decomposition
from statsmodels.tsa.seasonal import seasonal_decompose

########################################################################################################################
# 1. Data
########################################################################################################################

# Define file path
file = "Workshop 1 - Solution codes/Exercise 1.6/BuildingMaterials.xls"

# Load building materials data
series = pd.read_excel(file, sheet_name='Data', header=0, index_col=0, parse_dates=True).squeeze()

########################################################################################################################
# 2. Seasonal Decomposition
########################################################################################################################

# Additive Seasonal Decomposition
result = seasonal_decompose(series, model='additive')

# Multiplicative Seasonal Decomposition
# result = seasonal_decompose(series, model='multiplicative')

# Plot decomposition
result.plot()
plt.show()

# the following optional commands can be used to extract the values of the decomposition components
residual = result.resid
seasonal = result.seasonal 
trend = result.trend

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

