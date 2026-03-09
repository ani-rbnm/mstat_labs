#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Building Materials & Cement Production Data - Correlation.
--
-- Description: In this script the correlation between the Building Material and Cement Production data is calculated.
--
-- Content:     0. Set-up
--              1. Data
--              2. Correlation
--              3. Plotting
--              4. Publisher's Imprint
--
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Version  Date        Author    Major Changes
1.0      2020-01-10  ABZ       Initialization
1.1      2024-01-02  MLT       Updated version
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
matplotlib.use('Qt5Agg')
sns.set_style("whitegrid")

########################################################################################################################
# 1. Data
########################################################################################################################

# Define file path
file1 = "Workshop 1 - Solution codes/Exercise 1.3/CementProduction.xls"

# Load cement production data
series1 = pd.read_excel(file1, sheet_name='Sheet1', header=None, index_col=0, parse_dates=True).squeeze()

# Define file path
file2 = "Workshop 1 - Solution codes/Exercise 1.3/BuildingMaterials.xls"

# Load building materials data
series2 = pd.read_excel(file2, sheet_name='Data', header=0, index_col=0, parse_dates=True).squeeze()

# Subset building materials series
series2 = series2["2000-09-01":"2008-02-01"]

########################################################################################################################
# 2. Correlation
########################################################################################################################

correlation_matrix = np.corrcoef(series1, series2)

correlation_coef = correlation_matrix[1, 0]

print('The correlation between building materials and cement production is:', correlation_coef)

########################################################################################################################
# 3. Plotting
########################################################################################################################

# Create scatter plot
plt.scatter(series2, series1)
plt.xlabel('Building Materials')
plt.ylabel('Cement Production')
plt.title('Cement Production/Building Materials Relationship in Australia')

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
