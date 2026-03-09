#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Building Material Data - Log & Sqrt Transformation.
--
-- Description: In this script a log and sqrt transformation is performed for the Building Material data.
--
-- Content:     0. Set-up
--              1. Data
--              2. Plotting
--              3. Publisher's Imprint
--
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Version  Date        Author    Major Changes
1.0      2020-01-18  ABZ       Initialization
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
file = "Workshop 3 - Solution codes/BuildingMaterials.xls"

# Load Building Materials data set
series = pd.read_excel(file, sheet_name="Data", index_col=0, header=0, parse_dates=True).squeeze()

# Transform series into DataFrame
df = pd.DataFrame(series.values, index=series.index, columns=["Building"])

# Perform log transformation
df["Log Building"] = np.log(df["Building"])

# Perform sqrt transformation
df["Sqrt Building"] = np.sqrt(df["Building"])

########################################################################################################################
# 2. Plotting
########################################################################################################################

# Create figure
fig, ax = plt.subplots(3, 2, figsize=(15, 10))

# Time plot of original time series
ax[0, 0].plot(series)
ax[0, 0].set_title('Time plot for Building Material data')

# Histogram of original time series
ax[0, 1].hist(series)
ax[0, 1].set_title('Histogram for Building Material data')

# Log transform

# Time plot of log transformed time series
ax[1, 0].plot(df["Log Building"])
ax[1, 0].set_title('Time plot for log transformed Building Material data')

# Histogram of log trandformed time series
ax[1, 1].hist(df["Log Building"])
ax[1, 1].set_title('Histogram for log transformed Building Material data')

# Sqrt transform

# Time plot of sqrt transformed time series
ax[2, 0].plot(df["Sqrt Building"])
ax[2, 0].set_title('Time plot for sqrt transformed Building Material data')

# Histogram of sqrt transformed time series
ax[2, 1].hist(df["Sqrt Building"])
ax[2, 1].set_title('Histogram for sqrt transformed Building Material data')

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
