#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Building Materials Data - Error Measures.
--
-- Description: In this script the forecasting error for the naive forecast 1 and 2 are calculated based on the Building
--              Materials data set.
--
-- Content:     0. Set-up
--              1. Data
--              2. Plotting
--              3. Error Measures
--              4. Publisher's Imprint
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
fig, ax = plt.subplots(2, 1, figsize=(10, 7))

# Joint plot of original data and NF1 forecasts
df["Material"][1:].plot(legend=True, ax=ax[0])
df["NF1"][1:].plot(legend=True, ax=ax[0])

# Joint plot of original data and NF2 forecasts
df["Material"][1:].plot(legend=True, ax=ax[1])
df["NF2"][1:].plot(legend=True, ax=ax[1])
plt.show()

########################################################################################################################
# 3. Error Measures
########################################################################################################################

# Evaluating the errors from both NF1 and NF2 methods
Error1 = df["Material"][1:] - df["NF1"][1:]
Error2 = df["Material"][1:] - df["NF2"][1:]

# Mean Error
ME1 = sum(Error1) * 1.0 / len(df["NF1"][1:])
ME2 = sum(Error2) * 1.0 / len(df["NF2"][1:])

# Mean Absolute Error
MAE1 = sum(abs(Error1)) * 1.0 / len(df["NF1"][1:])
MAE2 = sum(abs(Error2)) * 1.0 / len(df["NF2"][1:])

# Mean Squared Error
MSE1 = sum(Error1**2) * 1.0 / len(df["NF1"][1:])
MSE2 = sum(Error2**2) * 1.0 / len(df["NF2"][1:])

# Percentage Error
PercentageError1 = (Error1/df["Material"][1:]) * 100
PercentageError2 = (Error2/df["Material"][1:]) * 100

# Mean Percentage Error
MPE1 = sum(PercentageError1) * 1.0 / len(df["NF1"][1:])
MPE2 = sum(PercentageError2) * 1.0 / len(df["NF2"][1:])

# Mean Absolute Percentage Error
MAPE1 = sum(abs(PercentageError1)) * 1.0 / len(df["NF1"][1:])
MAPE2 = sum(abs(PercentageError2)) * 1.0 / len(df["NF2"][1:])


# Printing a summary of the errors in a tabular form
print('Summary of errors resulting from NF1 & NF2:')
cars = {'Errors': ['ME', 'MAE', 'MSE', 'MPE', 'MAPE'],
        'NF1': [ME1, MAE1, MSE1, MPE1, MAPE1],
        'NF2': [ME2, MAE2, MSE2, MPE2, MAPE2]
        }
AllErrors = pd.DataFrame(cars, columns=['Errors', 'NF1', 'NF2'])
print(AllErrors)

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
