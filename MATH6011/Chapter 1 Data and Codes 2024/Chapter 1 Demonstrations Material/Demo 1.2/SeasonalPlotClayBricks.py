#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Clay Brick Data - Seasonal Plot.
--
-- Description: In this script a seasonal polt for the Clay Brick data set is created.
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
file = "Chapter 1 Data and Codes 2024/Chapter 1 Demonstrations Material/Demo 1.2/ClayBricks.xls"

# Load clay bricks data
series = pd.read_excel(file, sheet_name='BRICKSQ', header=0, index_col=0, parse_dates=True).squeeze()

# Create empty DataFrame for seasonal data
seasonal_data = pd.DataFrame(index=np.unique(series.index.month), columns=np.unique(series.index.year))

# Fill DataFrame with seasonal data
for year in np.unique(series.index.year):
    seasonal_data.loc[:np.sum(series.index.year == year), year] = series[series.index.year == year].values

########################################################################################################################
# 2. Seasonal Plot
########################################################################################################################

# Select a few years
years = [1957, 1958, 1960, 1961, 1963, 1968]

# Create seasonal plot
seasonal_data.loc[:, years].plot(title="Australian Clay Brick Production", xlabel="Month", ylabel="Millions of units")
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
