# Extreme-Regression-on-Metabolic-Syndrome

# Overview of Files

This repository contains several files related to statistical modeling and data visualization. Below is a brief description of each file:

## Data cleaning.txt
This file outlines the process for loading and cleaning data, preparing it for analysis. It includes steps for transforming variables into numeric types, determining if a subject suffers from metabolic syndrome based on specific criteria, and setting up the predictor (`X`) and response (`Y`) variables. It also details the random division of the dataset into training and testing sets and the application of the `XReg` model to the training data.

## findn.txt
This script defines a function `findn` that calculates the proportion of a sample meeting a certain condition based on a given threshold `q`. It takes in variables such as the response variable `y`, explanatory variable `x`, model results, and the threshold `q`. The function outputs the number and proportion of extreme populations in the sample, as well as the inclusion of these populations and the explanatory variable thresholds.

## modelevaluation.txt
This file contains two functions for model evaluation. The first, `modelevaluation`, provides the model's accuracy and sensitivity (recall). It calculates these metrics based on the model results and outputs variables including accuracy, sensitivity, threshold, and estimated values of `y` by the model. The second function, `modelevaluation_test`, is similar but takes an additional threshold argument to evaluate the model's performance.

## XRfunctions.txt
This script includes the definition of the `XReg` function, which is used for extreme regression analysis. It allows for the specification of a functional form for the regression model and provides various parameters to control the regression analysis, such as limits, iteration steps, and step sizes. The file also contains auxiliary functions like `fastreg` for fast regression and `level.set` for determining decision rules based on thresholds.

## plot.txt
This R script is for generating various plots using the `ggplot2` package. It defines model parameters for different health metrics like waist circumference (`WC_1`, `WC_0`), fasting plasma glucose (`FPG`), systolic blood pressure (`SBP`), diastolic blood pressure (`DBP`), triglycerides (`TG`), and HDL-C levels. It then creates plots with shading effects based on the defined parameters and adds specific points with labels to the plots.

