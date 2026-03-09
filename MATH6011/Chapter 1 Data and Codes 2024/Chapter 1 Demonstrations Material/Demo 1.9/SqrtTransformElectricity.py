#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Electricity Data - Sqrt Transformation.
--
-- Description: In this script the sqrt transformation is performed for the Electricity data.
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
matplotlib.use('Qt5Agg')
sns.set_style("whitegrid")

########################################################################################################################
# 1. Data
########################################################################################################################

# Define file path
file = "Chapter 1 Data and Codes 2024/Chapter 1 Demonstrations Material/Demo 1.9/Electricity.xls"

# Load electricity data
series = pd.read_excel(file, sheet_name='Data', header=0, index_col=0, parse_dates=True).squeeze()

# Transform pd.Series into pd.DataFrame
df = pd.DataFrame(series.values, columns=['electricity'])

# Add log electricity to DataFrame
df['sqrt electricity'] = np.sqrt(df['electricity'])

########################################################################################################################
# 2. Plotting
########################################################################################################################

plt.figure(1)
# line plot
plt.subplot(211)
plt.plot(df['sqrt electricity'])
# histogram
plt.subplot(212)
plt.hist(df['sqrt electricity'])
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
