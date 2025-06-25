## What is it ?
This study uses a dataset of electricity comsumption and outer-air temperature measurements for one 
building. Measurements are every 15 minutes from 1/1/2010 1:15 to 29/11/2010 23:45. In order to 
forecast the electricity consumption for 30/11/2010, several methods have been applied (exponential 
smoothing, SARIMA, neural network, random forest and XGBoost).

Results show that XGBoost methodology is the best to model and predict the electricity consumption. 
Moreover, results show that using the covariate outer-air temperature does not improve the 
modelling. 

A full report is availaible [report.pdf](https://github.com/JoanneAB/TimeSeriesForecasting/blob/master/report/report.pdf).

## How to use it ?

This study uses R scripts. All scripts and functions are available [here](https://github.com/JoanneAB/TimeSeriesForecasting/tree/master/src)

## Technology stack

### Data processing
- xts
- zoo
- readxl
- testcorr

### Graphical representation
- ggplot2

### Modeling
- forecast
- vars
- randomForest
- MTS
- xgboost

## Credits

This work has been developed as part of a project for the "Time series forecasting" class for Data Science Master's course in Data ScienceTech Institute (DSTI, France). Source codes and supplementary files are available at [https://github.com/JoanneAB/TimeSeriesForecasting](https://github.com/JoanneAB/TimeSeriesForecasting)

If you mention this project in a publication, please include the citations below.

 - Adam, J.M.-C. (2025). Electricity consumption forecasting. Master's project at DSTI, France. https://github.com/JoanneAB/TimeSeriesForecasting

## Licence

This work is under a GNU General Public License.
