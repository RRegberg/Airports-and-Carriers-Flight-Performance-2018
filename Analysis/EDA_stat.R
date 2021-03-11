##################################
# ITEC 621 PCR & PLS Regressions #
##################################

# Filename: ITEC621_PCR&PLS.R
# Prepared by J. Alberto Espinosa
# Last updated on 3/23/2019

# This script was prepared for an American University class ITEC 621 Predictive Analytics. It's use is intended specifically for class work for the MS Analytics program and as a complement to class lectures and lab practice.

# IMPORTANT: the material covered in this script ASSUMES that you have gone through the self-paced course KSB-999 R Overview for Business Analytics and are thoroughly familiar with the concepts illustrated in the RWorkshopScripts.R script. If you have not completed this workshop and script exercises, please stop here and do that first.


###########################################
#                  INDEX                  #
###########################################

## Dimension Reduction (PCR, PLS)

## Principal Components Regression (PCR)
# PCR Loadings and Scores
# PCR Coefficients
# Predictions with PCR
# PCR Example

## Partial Least Squares (PLS) Regression
# PLS Loadings and Scores
# PLS Coefficients
# Predictions with PLS
# PLS Example


###########################################
#              END OF INDEX               #
###########################################


## Dimension Reduction (PCR, PLS)

# Dimension reduction is a family of models that no only aim to  address dimensionality, but actually take advantage of it. The basic idea is this. If two predictors are uncorrelated, the scatter plot will show a somewhat spherical cloud of data point with no clear alignment in one direction or another. In contrast, if two predictors are highly correlated, as scatter plot of these two predictors will show a thin cloud of data points aligned in one direction. The direction of this alignment is the direction in which the variance in the data is the highest. 

# If you were to rotate the axes of the plot in that direction, one axis would be aligned in the direction of maximum variance and the other in the direction of minimal variance. The direction of maximum variance is called the "1st. Principal Components" (PC). If you have more than 2 predictors, you can then rotate the second axis to find the direction with the 2nd highest variance, which is called the "2nd. PC", and so on.

# The idea behind dimension reduction is that, for n predictors, to find the n PC's. These n PC's have some interesting properties:

# 1. The are perpendicular to each other, just like the axes (i.e., they are independent, or perfectly uncorrelated)

# 2. The PC's are nicely ranked from highest to lowest variance.

# 3. When the predictors are highly correlated, the first few PC's will explain a large proportion of variance in the data, and the last PC's will explain very little.

# 4. So, you can keep the firt m PC's that explain, say 70% or 80% of the variance in the data and disregard the remaining PC's. If m << p you will be achieving substantial dimension reduction, because you can now run a regression with m PC's, rather than with p predictors.

# 5. All m PC's are linear combinations of all p predictors, so all variables are represented in the PC's. More importantly, the PC's are uncorrelated, so the are truly independent variables.

# There a few dimension reduction methods, but the two most popular ones are: (1) Principal Components Regression (PCR) in which the predictors are rotated to find these PC's, without taking into account whether these dimensions help predict the outcome variable; and (2) Partial Least Squares (PLS) Regression; which is like PCR, but the axes are further rotated to improve their correlation with the response variable.

# PCR and PLS are similar, competing methods. Which one is better depends on the nature of the data. So, it is best to try both and select the one that gives better fit statistics.


## Principal Components Regression (PCR)


# We will use the {pls} package for both PCR and PLS

library(pls) # Has the Principal Components Regression pcr() function
library(ISLR) # Has the Hitters data set

set.seed(2) # To get repeatable results

# The pcr syntax is similar to lm

pcr.fit <- pcr(Salary~., data=Hitters, scale=T, validation="CV")

# Note: scale=TRUE is needed to standardize predictors, which is necessary when variables are in different scales (e.g., lbs, feet, etc.).

# Also, validation="CV" does 10-fold cross validation. validation="LOO" does leave-one-out cross validation

# Take a look at the results with CV scores and % variance explained for each factor

summary(pcr.fit) 

# Notes about the summary() of PCR results:

# Validation output:

# 1. CV values are SquareRoot(MSE), not the MSE, but you can square them if you wish to obtain the MSE. The adjCV is a "bias-corrected" CV. It makes very little difference for our purposes, but adjCV makes some statistical adjustment that may come from sampling bias in cross-validation testing.

# 2. CV's RMSE's go down as more components are included but notice that after the first few components, the further reduction in RMSE with one more component is not substantial and sometimes goes up slightly. The name of the game is to find the optimal number of components.

# % Variance explained

# 3. X shows how much of the variance of the original predictors (X) is explained by the components (5 components explain 84.29% of the variance in the predictors).

# 4. Salary show how much of the outcome variable variance is explained by the model (5 components explain 44.9% of the variance in Salary). Think of it as the R-squared for the 1, 2, 3, etc. component model

# One of the most effective ways to identify the optimal number of components in a PCR model is to use the "Scree Plot". In a scree plot, one can often see that the MSE or RootSquare(MSE) drops substantially up to a point where the curve "elbows". The optimal number of components is usually at these elbows:

validationplot(pcr.fit, val.type="MSEP", legendpos="topright") # Plots the MSE
validationplot(pcr.fit, val.type="RMSEP", legendpos="bottomright") # Plots the SquareRoot(MSE) 

# Notice how the scree plot elbows around 2 components, and again around 6 components.


# PCR Loadings and Scores

# The pcr() object is very complex and is full of information. Check it out:

str(pcr.fit) # Show all the contents of the pcr.fit object

# We can extract loadings:

pcr.fit$loadings # The linear weight of each variable on each component

# Note: the $loadings attribute in the pcr() object is a list with 2 elements: variables and components, containing the loadings for each variable/component combination. For example:

pcr.fit$loadings[3,2] # Loading of third variable on second component

sum(pcr.fit$loadings[1,]^2) # Sum of squared loadings for first variable = 1
sum(pcr.fit$loadings[,1]^2) # Sum of squared loadings for first component = 1

# The first command above squares and sums the loadings for one variable (i.e., row) and the second one does the same for one component (i.e., column). In these examples I chose row 1 and column 1. In both cases I got 1. 

pcr.fit$scores # Resulting from applying loadings to each data point


# PCR Coefficients

# If you wish to reconstruct coefficients for the actual variables, enter:

pcr.fit$coefficients # Coefficients for all components

# The $coefficients attribute of the pcr() object is not a single value, but it is actually a list of 3 sets of elements: n predictors, 1 response variable, m components. So, for example:

pcr.fit$coefficients[,,1] # Coefficients for the 1-component model
coef(pcr.fit, ncomp=1) # Same result different format

pcr.fit$coefficients[,,3] # Coefficients for the 3-component model
coef(pcr.fit, ncomp=3) # Same result different format

pcr.fit$coefficients[2,,3] # Coefficients for the second variable (Hits) of the 3-component model

# To list coefficients for, say 2 to 4 PCR component models:

pcr.fit$coefficients[,,2:4]
coef(pcr.fit, ncomp=2:4) # Same result different format


# Predictions with PCR

# To do predictions, you can use any data frame with values to feed to the predict() function. For this illustration we use 10% of the existing data to test our predictions. For this illustration, we use a model with 7 components for the prediction.

set.seed(2)
pred.test <- sample(1:nrow(Hitters), 0.10*nrow(Hitters))
pcr.pred <- predict(pcr.fit, Hitters[pred.test,], ncomp=7) 
pcr.pred

# Compute the MSE for these predictions

mean((pcr.pred-Hitters[pred.test,]$Salary)^2) # MSE
sqrt(mean((pcr.pred-Hitters[pred.test,]$Salary)^2)) # RMSE

# Once you decide on the optimal number of components, e.g., 6, you can fit that one model:

pcr.fit.2 <- pcr(Salary~., data=Hitters, scale=T, ncomp=6) 
summary(pcr.fit.2)


# PCR Example

# mtcars data set

set.seed(5)
pcr.mpg <- pcr(mpg~cyl+disp+hp+wt+gear, data=mtcars, scale=T, validation="CV")

validationplot(pcr.mpg)

summary(pcr.mpg)

pcr.mpg$loadings
print(pcr.mpg$coefficients[,,1:5], digits=2)
# You can also use coef(pcr.mpg, ncomp=1:5)


## Partial Least Squares (PLS) Regression


# For PLS we use the same {pls} library, but we use the plsr() function instead.

library(pls) # Contains the plsr() function
library(ISLR) # Has the Hitters data set

set.seed(1) # To get repeatable results

# The plsr() syntax is identical to the pcr() syntax

pls.fit <- plsr(Salary~., data=Hitters, scale=T, validation="CV")
# Default CV is 10FCV. Use validation="LOO" for LOOCV

summary(pls.fit)

# Scree plots

validationplot(pls.fit, val.type="MSEP", legendpos="topright") # w/MSE
validationplot(pls.fit, val.type="RMSEP", legendpos="topright") # w/RMSE


# PLS Loadings and Scores

str(pls.fit) # Show all the contents of the pcr.fit object

# We can extract loadings:

pls.fit$loadings # The linear weight of each variable on each component

# Note: the $loadings attribute in the pcr() object is a list with 2 elements: variables and components, containing the loadings for each variable/component combination. For example:

pls.fit$loadings[3,2] # Loading of third variable on second component

sum(pls.fit$loadings[1,]^2) # Sum of squared loadings for first variable = 1
sum(pls.fit$loadings[,1]^2) # Sum of squared loadings for first component = 1

# The first command above squares and sums the loadings for one variable (i.e., row) and the second one does the same for one component (i.e., column). In these examples I chose row 1 and column 1. In both cases I got 1. 

pls.fit$scores # Resulting from applying loadings to each data point


# PLS Coefficients

# If you wish to reconstruct coefficients for the actual variables, enter:
  
pls.fit$coefficients # Coefficients for all components

pls.fit$coefficients[,,1] # Coefficients for the 1-component model, or
coef(pls.fit, ncomp=1) # Same result different format

pls.fit$coefficients[,,3] # Coefficients for the 3-component model
coef(pls.fit, ncomp=3) # Same result different format, or

pls.fit$coefficients[2,,3] # Coefficients for the second variable of the 3-component model

# To list coefficients for, say 2 to 4 PLS component models:

pls.fit$coefficients[,,2:4]


# Predictions with PLS

# To do predictions, you can use any data frame with values to feed to the predict() function. For this illustration we use 10% of the existing data to test our predictions. For this illustration, we use a model with 7 components for the prediction.

set.seed(1)
pred.test <- sample(1:nrow(Hitters), 0.10*nrow(Hitters))

# Use the 2-component PLS model for predictions

pls.pred=predict(pls.fit, Hitters[pred.test,], ncomp=2) 
pls.pred

# Compute the MSE for these predictions

mean((pls.pred-Hitters[pred.test,]$Salary)^2) # MSE
sqrt(mean((pls.pred-Hitters[pred.test,]$Salary)^2)) # RMSE

# Display PCR and PLS RMSE together

cbind("PCR RMSE"=sqrt(mean((pcr.pred-Hitters[pred.test,]$Salary)^2)), "PLS RMSE"= sqrt(mean((pls.pred-Hitters[pred.test,]$Salary)^2)))

# Once you decide on the optimal number of components, e.g., 6, you can fit that one model:

pcr.fit.2 <- pcr(Salary~., data=Hitters, scale=T, ncomp=6) 
summary(pcr.fit.2)

# Now you can re-fit the model with the full data set and 2 components

pls.fit.2 <-plsr(Salary~., data=Hitters,scale=T,ncomp=2) 
summary(pls.fit.2)


# PLS Example

# mtcars data set

set.seed(5)
pls.mpg <- plsr(mpg~cyl+disp+hp+wt+gear, data=mtcars, scale=T, validation="CV")

validationplot(pls.mpg)

summary(pls.mpg)

pls.mpg$loadings
print(pls.mpg$coefficients[,,1:5], digits=2)
# You can also use coef(pls.mpg, ncomp=1:5)

