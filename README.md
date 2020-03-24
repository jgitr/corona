## Corona Calculations

# Workflow
'var.R' depends on some matrix or dataframe as input which is to be stored in the 'Data' directory.
One (or multiple) objects are stored in the same environment and bound into a main data table.
For this object, the following analysis is conducted:

- Descriptive Statistics: Autocorrelation, Correlation (Spearman, Pearson), Summary Stats
- Fit a Vector Auto Regression (VAR)
- Augmented Dickey Fuller Test
- Unit Root Test (Stability)
- Portmanteau Test
- Normality Test
- Granger Causality
- Forecast Error Variance Decomposition (To be done)
- Shock Simulation (To be done)