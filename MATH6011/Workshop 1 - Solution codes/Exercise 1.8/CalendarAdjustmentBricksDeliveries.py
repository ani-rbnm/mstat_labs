#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Bricks Deliveries Data - Calendar Adjustment.
--
-- Description: In this script a calendar adjustment for the Bricks Deliveries data is performed.
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

# Days in month
import calendar

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
file = "Workshop 1 - Solution codes/Exercise 1.8/BricksDeliveries.xls"

# Load bricks deliveries data
series = pd.read_excel(file, header=0, index_col=0, parse_dates=True).squeeze()

# Transform series into DataFrame
df = pd.DataFrame(series.values, index=series.index, columns=["Bricks"])

# Create empty column for days in the month
df["Days"] = np.nan

# Fill empty columns with the days per calendar month
for date in series.index:
    df.loc[date, "Days"] = calendar.monthrange(date.year, date.month)[1]

# Perform calendar adjustment
df["Adjusted Bricks"] = df["Bricks"] * 365.25 / (12 * df["Days"])

# Perform log transformation
df["Log Adjusted Bricks"] = np.log(df["Adjusted Bricks"])

# Perform sqrt transformation
df["Sqrt Adjusted Bricks"] = np.sqrt(df["Adjusted Bricks"])

########################################################################################################################
# 2. Plotting
########################################################################################################################

# Create figure
fig, ax = plt.subplots(3, 2, figsize=(15, 10))

OriginalData = df["Bricks"]
AdjustedData = df["Adjusted Bricks"]
OriginalData.plot(label='Original series', ax=ax[0, 0])
AdjustedData.plot(label='Adjusted series', ax=ax[0, 0])
ax[0, 0].set_title('Calendar adjustment for bricks deliveries data')
ax[0, 0].legend()

# Histogram of original time series
ax[0, 1].hist(AdjustedData)
ax[0, 1].set_title('Histogram for calendar adjusted brick deliveries data')

# Log transform

# Time plot of log transformed time series
ax[1, 0].plot(df["Log Adjusted Bricks"])
ax[1, 0].set_title('Time plot for log transformed calendar adjusted brick deliveries data')

# Histogram of log trandformed time series
ax[1, 1].hist(df["Log Adjusted Bricks"])
ax[1, 1].set_title('Histogram for log transformed calendar adjusted brick deliveries data')

# Sqrt transform

# Time plot of sqrt transformed time series
ax[2, 0].plot(df["Sqrt Adjusted Bricks"])
ax[2, 0].set_title('Time plot for sqrt transformed calendar adjusted brick deliveries data')

# Histogram of sqrt transformed time series
ax[2, 1].hist(df["Sqrt Adjusted Bricks"])
ax[2, 1].set_title('Histogram for sqrt transformed calendar adjusted brick deliveries data')

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
