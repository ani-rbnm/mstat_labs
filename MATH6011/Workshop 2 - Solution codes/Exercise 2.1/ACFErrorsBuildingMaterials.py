#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Building Materials Data - Autocorrelation Function (ACF).
--
-- Description: Visualization of the ACF for the Building Materials data set.
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
1.1      2024-01-02  MLT       Updated version
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
"""
########################################################################################################################
# 0. Set-ups
########################################################################################################################

# Read in data
import pandas as pd
import numpy as np

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
file = "Workshop 2 - Solution codes/Exercise 2.1/BuildingMaterialsNF1NF2.xls"

# Building Materials data set
series = pd.read_excel(file, sheet_name="Sheet1", index_col=0, header=0, parse_dates=True).squeeze()

# Transform series into DataFrame
df = pd.DataFrame(series.values, index=series.index, columns=["Material"])

# Naive Forecast 1
df["NF1"] = np.nan
df.iloc[1:, 1] = df.iloc[:(df.shape[0] - 1), 0]

# Create empty column for the year
df["Year"] = np.nan

# Fill Year column
i = 1
current_year = df.index.year[0]
for date in df.index:
    if date.year != current_year:
        i += 1
        current_year = date.year
    df.loc[date, "Year"] = i

# Create empty column for S(t)
df["S(t)"] = np.nan
df.iloc[:12, 3] = df.iloc[:12, 0]

# Fill S(t) column
for i in range(12, df.shape[0]):
    df.iloc[i, 3] = (df.iloc[i - 12, 2] * df.iloc[i - 12, 3] + df.iloc[i, 0]) / df.iloc[i, 2]

# Naive Forecast 2
df["NF2"] = np.nan
df.iloc[1:12, 4] = df.iloc[0:11, 0]

# Fill NF2 column
for i in range(12, df.shape[0]):
    df.iloc[i, 4] = df.iloc[i - 1, 0] - df.iloc[i - 1, 3] + df.iloc[i - 12, 3]

########################################################################################################################
# 2. Plotting
########################################################################################################################

# Create figure
fig, ax = plt.subplots(3, 1, figsize=(10, 7))

# Plot for the original data set
df["Material"][1:].plot(label='Original data', legend=True, ax=ax[0])

# Evaluating the errors from both NF1 and NF2 methods
Error1 = df["Material"][1:] - df["NF1"][1:]
Error2 = df["Material"][1:] - df["NF2"][1:]

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

