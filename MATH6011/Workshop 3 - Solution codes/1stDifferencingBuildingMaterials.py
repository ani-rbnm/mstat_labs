#!/usr/bin/env python3  Line 1
# -*- coding: utf-8 -*- Line 2
"""
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
--
-- Building Materials Data - First Difference.
--
-- Description: In this script the first order difference for the Building Materials data is calculated.
--
-- Content:     0. Set-up
--              1. Data
--              2. Plotting
--              3. Publisher's Imprint
--
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Version  Date        Author    Major Changes
1.0      2020-02-19  ABZ       Initialization
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
matplotlib.use('Qt5Agg')
sns.set_style("whitegrid")

# ACF, PACF
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf

########################################################################################################################
# 1. Data
########################################################################################################################

# Define file path
file = "Workshop 3 - Solution codes/BuildingMaterials.xls"

# Load Building Materials data set
series = pd.read_excel(file, sheet_name="Data", index_col=0, header=0, parse_dates=True).squeeze()

# Create 1st difference
X = series.values
diff = list()
for i in range(1, len(X)):
	value = X[i] - X[i - 1]
	diff.append(value)

########################################################################################################################
# 2. Plotting
########################################################################################################################

# Create figure
fig, ax = plt.subplots(3, 1, figsize=(10, 7))

# Time plot
ax[0].plot(diff)
ax[0].set_title('Time plot of Building Materials 1st difference')

# ACF
plot_acf(diff, title='ACF of Building Materials 1st difference', lags=50, ax=ax[1])

# PACF
plot_pacf(diff, title='PACF of Building Materials 1st difference', lags=50, ax=ax[2])

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
