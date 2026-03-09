#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2
"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Beer Data - Error Measures.
--
-- Description: In this script the forecasting error for the naive forecast 1 and 2 are calculated based on the beer
--              data.
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
1.1      2023-12-22  MLT       Updated version
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
"""
########################################################################################################################
# 0. Set-ups
########################################################################################################################

# Read in data
import pandas as pd

# Plotting
import matplotlib
import seaborn as sns
import matplotlib.pyplot as plt
# matplotlib.use('Qt5Agg')
sns.set_style("whitegrid")

########################################################################################################################
# 1. Data
########################################################################################################################

# Define file path
file = "Chapter 2 Data and Codes 2024/Chapter 2 Demonstrations Material/Demo 2.1/BeerErrorsData.xls"

# Define sheet name
s_name = 'NF1NF2'

# Australian Beer data set
AustralianBeer = pd.read_excel(file, sheet_name=s_name, usecols=[1], header=0, dtype=float).squeeze()

# Naive Forecast 1
NaiveF1 = pd.read_excel(file, sheet_name=s_name, usecols=[2], header=0, dtype=float).squeeze()

# Naive Forecast 2
NaiveF2 = pd.read_excel(file, sheet_name=s_name, usecols=[3], header=0, dtype=float).squeeze()

########################################################################################################################
# 2. Plotting
########################################################################################################################

# Create figure
fig, ax = plt.subplots(2, 1, figsize=(10, 7))

# Joint plot of original data and NF1 forecasts
AustralianBeer.plot(legend=True, ax=ax[0])
NaiveF1.plot(legend=True, ax=ax[0])

# Joint plot of original data and NF2 forecasts
AustralianBeer.plot(legend=True, ax=ax[1])
NaiveF2.plot(legend=True, ax=ax[1])
plt.show()

########################################################################################################################
# 3. Error Measures
########################################################################################################################

# Evaluating the errors from both NF1 and NF2 methods
Error1 = AustralianBeer - NaiveF1
Error2 = AustralianBeer - NaiveF2

# Mean Error
ME1 = sum(Error1) * 1.0 / len(NaiveF1)
ME2 = sum(Error2) * 1.0 / len(NaiveF2)

# Mean Absolute Error
MAE1 = sum(abs(Error1)) * 1.0 / len(NaiveF1)
MAE2 = sum(abs(Error2)) * 1.0 / len(NaiveF2)

# Mean Squared Error
MSE1 = sum(Error1**2) * 1.0 / len(NaiveF1)
MSE2 = sum(Error2**2) * 1.0 / len(NaiveF2)

# Percentage Error
PercentageError1 = (Error1/AustralianBeer) * 100
PercentageError2 = (Error2/AustralianBeer) * 100

# Mean Percentage Error
MPE1 = sum(PercentageError1) * 1.0 / len(NaiveF1)
MPE2 = sum(PercentageError2) * 1.0 / len(NaiveF2)

# Mean Absolute Percentage Error
MAPE1 = sum(abs(PercentageError1)) * 1.0 / len(NaiveF1)
MAPE2 = sum(abs(PercentageError2)) * 1.0 / len(NaiveF2)


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
