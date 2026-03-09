#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Bank Data - Ordinary Least Squares (OLS).
--
-- Description: In this script different OLS models are fitted for the bank data set.
--
-- Content:     0. Set-up
--              1. Data
--              2. Ordinary Least Squares
--              3. Publisher's Imprint
--
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Version  Date        Author    Major Changes
1.0      2020-02-13  ABZ       Initialization
1.1      2024-01-02  MLT       Updated version
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
"""

########################################################################################################################
# 0. Set-ups
########################################################################################################################

# Generally required
import pandas as pd

# OLS
from statsmodels.formula.api import ols

########################################################################################################################
# 1. Data
########################################################################################################################

# Define file path
file = "Chapter 4 Data and Codes 2024/Chapter 4 Demonstrations Material/Bank.xls"

# Load bank data set
df = pd.read_excel(file, sheet_name='Data2', header=0, dtype=float).squeeze()

########################################################################################################################
# 2. Ordinary Least Squares
########################################################################################################################

# Regression model(s)
formula1 = 'DEOM ~ AAA + Tto4 + D3to4'
formula2 = 'DEOM ~ AAA + Tto4 + D3to4 + D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10 + D11'
formula3 = 'DEOM ~ AAA + Tto4 + D3to4 + D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10 + D11 + time'

# Ordinary Least Squares (OLS)
results1 = ols(formula1, data=df).fit()
results2 = ols(formula2, data=df).fit()
results3 = ols(formula3, data=df).fit()

print(results1.summary())
print(results2.summary())
print(results3.summary())

# the results from IndividualSignificance.py, 
# StatsWithIndicatorsBank.py, 
# and StatsWithIndicatorsTimeBank.py are summarised 
# for easy comparison of the key statistics

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
