#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2

"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Bank Data - Time Plots.
--
-- Description: In this script time plots for different variables of the Bank data set are visualized.
--
-- Content:     0. Set-up
--              1. Data
--              2. Plotting
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
file = "Chapter 4 Data and Codes 2024/Chapter 4 Demonstrations Material/Bank.xls"

# Load bank data set
df = pd.read_excel(file, sheet_name='Data2', header=0, dtype=float).squeeze()

# reading the basic variables
DEOM = df.DEOM
AAA = df.AAA
Tto4 = df.Tto4
D3to4 = df.D3to4

########################################################################################################################
# 2. Plotting
########################################################################################################################

# Create figure
fig, ax = plt.subplots(4, 1, figsize=(10, 15), sharex=True)

# Plot DEOM
DEOM.plot(ax=ax[0], title='DEOM', ylabel="Difference end of month balance")

# Plot AAA
AAA.plot(ax=ax[1], title="AAA", ylabel='Composite AAA Bond rates')

# Plot Tto4
Tto4.plot(ax=ax[2], title="Tto4", ylabel='US Govt 3-4 year Bond rates')

# Plot D3to4
D3to4.plot(ax=ax[3], title="D3to4", xlabel="Time", ylabel="Difference US Govt 3-4 year Bond rates")

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
