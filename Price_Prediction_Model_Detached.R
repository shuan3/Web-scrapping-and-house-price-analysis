#--------------------------------------------------------------------------------------
#################### Detached Houses - Price Prediction Modeling ######################
#--------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------
## Install and load the required packages
#--------------------------------------------------------------------------------------
if(!require(dplyr)) 
  install.packages("dplyr")
if(!require(fastDummies)) 
  install.packages("fastDummies")
if(!require(corrplot)) 
  install.packages("corrplot")
if(!require(ggplot2)) 
  install.packages("ggplot2")
if(!require(magrittr)) 
  install.packages("magrittr")
if(!require(tidyr)) 
  install.packages("tidyr")

library(dplyr)
library(fastDummies)
library(corrplot)
library(ggplot2)
library(magrittr)
library(tidyr)

#--------------------------------------------------------------------------------------
## Data Loading
#--------------------------------------------------------------------------------------
#full_data <- read.csv("clean.csv", header=TRUE)
#full_data <- read.csv("ontario_listings_clean_v2.csv", header=TRUE)
#full_data <- read.csv("ontario_listings_clean_v3.csv", header=TRUE)
#full_data <- read.csv("ontario_listings_clean_v4.csv", header=TRUE)
#full_data <- read.csv("ontario_listings_clean_v5.csv", header=TRUE)
#full_data <- read.csv("ontario_listings_clean_v6.csv", header=TRUE)
full_data  <- read.csv("ontario_listings.csv", header=TRUE)

dim(full_data) #--41368

colnames(full_data)

#str(full_data)

#--------------------------------------------------------------------------------------
## Data Summary
#--------------------------------------------------------------------------------------

# Count Summary by Area
full_data %>%
  group_by(Area) %>% 
  summarise(Count = n()) %>% arrange(desc(Count)) %>%
  mutate( Percent = Count/NROW(full_data) * 100 )

# Count Summary by Type
full_data %>%
  group_by(Type) %>%
  summarise(Count = n()) %>% arrange(desc(Count)) %>%
  mutate( Percent = Count/NROW(full_data) * 100 )

data_toronto <- full_data %>% subset(Area == "Toronto")
NROW(full_data)
NROW(data_toronto)
NROW(data_toronto)/NROW(full_data) * 100

data_toronto_detached <- data_toronto %>% subset(Type == "Detached")
NROW(data_toronto)
NROW(data_toronto_detached)
NROW(data_toronto_detached)/NROW(data_toronto) * 100

data_toronto_detached %>% subset(Status == "Sold") %>% nrow()
data_toronto_detached %>% subset(!is.na(data_toronto_detached$SoldPrice)) %>% nrow()

#--------------------------------------------------------------------------------------
## Data Selection for Price Prediction Modeling
#--------------------------------------------------------------------------------------

# We will focus on Detached houses in Toronto area:
# Toronto area has 31% (12,926 records) of overall Ontario sales volume.
# Detached houses has 30% (3,871) of Toronto Sales volume.
#--------------------------------------------------------------------------------------

house_data <- full_data %>%
  subset(Area == "Toronto" & 
         Type == "Detached" & 
         Status == "Sold" & !is.na(full_data$SoldPrice))

# After eliminating missing price value, the selected data has 3,718 records
NROW(house_data) #--3718

# Price Summary
house_data %>% 
  group_by(Type) %>%
  summarise(Count = n(), 
            min  = min(SoldPrice), max = max(SoldPrice), 
            median = median(SoldPrice), mean = mean(SoldPrice) )

#--------------------------------------------------------------------------------------
## Features Selection
#--------------------------------------------------------------------------------------

# The following attributes were identified as having an impact on 
# (are correlated to) the house price by our Data Analysis in section above.
#--------------------------------------------------------------------------------------

features_data <- house_data %>% 
  select(SoldPrice, Rooms, Bedrooms, Bathrooms, Taxes,  Age,
         ParkingTotal, FinishedBasement, NearSchool, NearPark)

dim(features_data) #--3718

summary(features_data)

#--------------------------------------------------------------------------------------
## Data Cleanup
#--------------------------------------------------------------------------------------

# Check missing values
summarise_all(features_data, funs( sum(is.na(.)) ) )

# Drop Age column - it has 73% (2,722 of 3,718) records missing value
features_data = select(features_data, -Age)
dim(features_data) #--3718

#Removing the outliers
#--------------------------------------------------------------------------------------

clean_data <- features_data
dim(clean_data) #--3718

# Check and remove Price outliers
#boxplot(clean_data$SoldPrice)
outliers <- boxplot.stats(clean_data$SoldPrice)$out
clean_data <- clean_data[-which(clean_data$SoldPrice %in% outliers),]
#boxplot(clean_data$SoldPrice)
dim(clean_data) #--3510

# Check and remove Taxes outliers
#boxplot(clean_data$Taxes)
clean_data <- clean_data %>% subset(Taxes > 1000)
outliers <- boxplot.stats(clean_data$Taxes)$out
clean_data <- clean_data[-which(clean_data$Taxes %in% outliers),]
#boxplot(clean_data$Taxes)
dim(clean_data) #--3441 #--3282

# Check and remove Rooms outliers
#boxplot(clean_data$Rooms)
outliers <- boxplot.stats(clean_data$Rooms)$out
clean_data <- clean_data[-which(clean_data$Rooms %in% outliers),]
#boxplot(clean_data$Rooms)
dim(clean_data) #--3176

# Check and remove Bedrooms outliers
#boxplot(clean_data$Bedrooms)
outliers <- boxplot.stats(clean_data$Bedrooms)$out
clean_data <- clean_data[-which(clean_data$Bedrooms %in% outliers),]
#boxplot(clean_data$Bedrooms)
dim(clean_data) #--3133

# Check and remove Bathrooms outliers
#boxplot(clean_data$Bathrooms)
outliers <- boxplot.stats(clean_data$Bathrooms)$out
clean_data <- clean_data[-which(clean_data$Bathrooms %in% outliers),]
#boxplot(clean_data$Bathrooms)
dim(clean_data) #--2946

# Check and remove ParkingTotal outliers
#boxplot(clean_data$ParkingTotal)
outliers <- boxplot.stats(clean_data$ParkingTotal)$out
clean_data <- clean_data[-which(clean_data$ParkingTotal %in% outliers),]
#boxplot(clean_data$ParkingTotal)
dim(clean_data) #--2925

NROW(features_data)
NROW(clean_data)
NROW(clean_data)/NROW(features_data) * 100

summary(clean_data)

# Check missing values and replace them with column mean/median values if any
#--------------------------------------------------------------------------------------

dim(clean_data) #--2925
summarise_all(clean_data, funs( sum(is.na(.)) ) )

summary(clean_data)

# Print correlation matrix for numeric attributes.
#--------------------------------------------------------------------------------------
dim(clean_data)
corr_matrix <- cor(clean_data)
corr_matrix
#corr_matrix[,ncol(corr_matrix)] %>% abs() %>% sort(decreasing=TRUE)

# Plot correlation matrix for numeric attributes.
#--------------------------------------------------------------------------------------
# Plot Correlation matrix
corrplot(cor(as.matrix(clean_data)), method="pie", type="lower")


#--------------------------------------------------------------------------------------
## Price Prediction Modeling
#--------------------------------------------------------------------------------------

detached_model_data <- clean_data

# Use 80/20 Train/Test Split
#--------------------------------------------------------------------------------------
set.seed(123)
ind<-sample(2, nrow(detached_model_data), replace=T, prob=c(0.8,0.2) )
train_data <- detached_model_data[ind==1,]
test_data  <- detached_model_data[ind==2,]

dim(detached_model_data) #--2925
dim(train_data) #--2347
dim(test_data)  #--578

attach(train_data)

# Create a Multiple Linear Regression Model with all relevant attributes
#--------------------------------------------------------------------------------------

detached_model1a <- lm(SoldPrice ~ Rooms+Bedrooms+Bathrooms+Taxes+FinishedBasement+NearSchool+NearPark)
# Check the Model summary
summary(detached_model1a)

# The output shows:
#
# 1) R-squared value is 0.8156 -- which is not too bad.
# This value means that predictors explain 78.74% of the variability in Sold Price.
#
# 2) Residual standard error: 175900 on 2339 degrees of freedom
# The Mean Squared Error is: 175900 - this is too large.


# Create a new Multiple Linear Regression Model - with less predictor variables
#--------------------------------------------------------------------------------------

detached_model1b <- lm(SoldPrice ~ Rooms+Bathrooms+FinishedBasement+Taxes)
# Check the Model summary
summary(detached_model1b)

# The output shows:
#
# 1) R-squared value is 0.8151 -- which is more or less same as before
#
# 2) Residual standard error: 176000 on 2342 degrees of freedom -- very similar to before


# Create a new Multiple Linear Regression Model - with different predictor variables
#--------------------------------------------------------------------------------------

detached_model1c <- lm(SoldPrice ~ Bedrooms+Bathrooms+FinishedBasement+Taxes)
# Check the Model summary
summary(detached_model1c)

# The output shows:
#
# 1) R-squared value is 0.8145 -- which is more or less same as before
#
# 2) Residual standard error: 176300 on 2342 degrees of freedom -- very similar to before


#--------------------------------------------------------------------------------------
# Transformations
#--------------------------------------------------------------------------------------

# Lets see if it is possible to improve our model by transforming some of our predictors, 
# either with a square root transformation or a log transformation.
#--------------------------------------------------------------------------------------

# Create a new Multiple Linear Regression Model - with log transformation
#--------------------------------------------------------------------------------------

detached_model2a <- lm(log(SoldPrice) ~ Bedrooms+Bathrooms+FinishedBasement+log(Taxes) )
# Check the model summary
summary(detached_model2a)

# The output shows:
#
# 1) R-squared value is 0.8134 -- which is more or less same as before
#
# 2) Residual standard error: 0.1358 on 2342 degrees of freedom
# The Mean Squared Error is much-much lower than the first model.


# Create a new Multiple Linear Regression Model - with square root transformation
#--------------------------------------------------------------------------------------

detached_model2b <- lm(sqrt(SoldPrice) ~ Bedrooms+Bathrooms+FinishedBasement+sqrt(Taxes) )
# Check the Model summary
summary(detached_model2b)

# The output shows:
#--------------------------------------------------------------------------------------
# 1) R-squared value is 0.8190 -- which is better than before
#
# 2) Residual standard error: 75.24 on 2342 degrees of freedom
# The Mean Squared Error is much lower the first model


## Call:
##   lm(formula = sqrt(SoldPrice) ~ Bedrooms + Bathrooms + FinishedBasement + 
##        sqrt(Taxes))
## 
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -579.64  -47.04   -7.90   36.66  529.63 
## 
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      161.6767    10.3240  15.660  < 2e-16 ***
##   Bedrooms          -0.6413     2.6763  -0.240  0.81066    
## Bathrooms         18.1031     2.2207   8.152 5.77e-16 ***
##   FinishedBasement  11.4356     4.0802   2.803  0.00511 ** 
##   sqrt(Taxes)       12.8840     0.1486  86.723  < 2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 75.24 on 2342 degrees of freedom
## Multiple R-squared:  0.819,	Adjusted R-squared:  0.8187 
## F-statistic:  2649 on 4 and 2342 DF,  p-value: < 2.2e-16


# We can check the Confidence Intervals of the Coefficients 
confint(detached_model2b, level=.95)

# Check the residuals
resid<- detached_model2b$residuals
hist(resid)

qqnorm(resid)
qqline(resid)


#--------------------------------------------------------------------------------------
## Using the Model
# We are going to use the last model above for our price prediction
#--------------------------------------------------------------------------------------

dim(test_data) #--578

head(test_data)

# Prediction with 95% confidence interval
pred_matrix <- predict(detached_model2b, test_data, interval="predict")

pred_df <- as.data.frame(pred_matrix)

pred_data <- test_data
pred_data$PredPrice     <- pred_df$fit ^ 2
pred_data$PredPrice_lwr <- pred_df$lwr
pred_data$PredPrice_upr <- pred_df$upr

pred_data <- pred_data %>% mutate( Pred_residuals = SoldPrice - PredPrice )
head(pred_data)

# Check the residuals
#resid <- pred_data$Pred_residuals
#hist(resid)

#qqnorm(resid)
#qqline(resid)

#--------------------------------------------------------------------------------------