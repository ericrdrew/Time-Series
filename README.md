# TimeSeries
Collection of Time Series assignments from throughout the fall semester.

Table of Contents:

1. Time Series Homework 1(8/26/2022)
-Roll up hourly energy consumption data into daily, create time series object, decompose the data.

2. Time Series Homework 2(9/2/2022)
-Built off of Time Series Homework 1. Fit variety of ESM models, including SES, Holt ESM(additive/multiplicative), Damped Holt ESM, Holt-Winters Smoothing(additive/multiplicative). Predicted onto validation set and plotted forecast. Selected model based on validation MAPE/MAE, re-fit data on training & validation set and ran final forecast testing on test set.

3. TS2 HW1 Code(10/5/2022)
-Read in hourly energy consumption data, clean missing values where daylight savings was impacting the data, create time series objects, split data, fit variety of ESM and seasonal ARIMA models and compare their forecasts onto a validation set using MAE/MAPE for model selection.

4. TS2 HW2 Code(10/13/2022)
-Building off of TS2 HW1 Code but with additional, new real-time energy consumption data. Fit prophet model and neural network model on training datasets and calculated error metrics(MAE/MAPE) on forecasts onto a validation set. 

5. TSProject Code(IN PROGRESS: 10/25/2022)
-Re-fit ESM, ARIMA, prophet, and neural network models from TS2 HW1/HW2 with updated energy consumption data as well as create final 5th model using weighted/combined TS for prediction.

