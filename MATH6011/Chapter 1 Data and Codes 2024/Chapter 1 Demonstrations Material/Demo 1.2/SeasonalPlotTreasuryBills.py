#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Treasury Bill Data - Seasonal Plot.
--
-- Description: In this script a seasonal polt for the Treasury Bill data set is created.
--
-- Content:     0. Set-up
--              1. Data
--              2. Seasonal Plot
--              3. Publisher's Imprint
--
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Version  Date        Author    Major Changes
1.0      2021-09-05  ABZ       Initialization
1.1      2023-12-22  MLT       Updated version
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
# matplotlib.use('Qt5Agg')
sns.set_style("whitegrid")

########################################################################################################################
# 1. Data
########################################################################################################################

# Define file path
file = "Chapter 1 Data and Codes 2024/Chapter 1 Demonstrations Material/Demo 1.2/TreasuryBills.xls"

# Load treasury bill data
series = pd.read_excel(file, sheet_name='USTREAS', header=0, index_col=0, parse_dates=True).squeeze()

series.index = series.index + 1

# Create empty DataFrame for seasonal data
seasonal_data = pd.DataFrame(index=np.arange(1, 13), columns=np.arange(1, 7))

# Fill DataFrame with seasonal data
month = 1
year = 1
i = 1
while year <= 6:
    seasonal_data.loc[month, year] = series[i]
    month += 1
    i += 1
    if month > 12:
        year += 1
        month = 1

########################################################################################################################
# 2. Seasonal Plot
########################################################################################################################

# Create seasonal plot
seasonal_data.plot(title="US Treasury Bills", xlabel="Month", ylabel="Price")
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
