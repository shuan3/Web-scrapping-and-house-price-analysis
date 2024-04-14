#--------------------------------------------------------------------------------------
#################### Condo - Price Prediction Modeling #####################
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

data_toronto_condo <- data_toronto %>% subset(Type == "Condo")
NROW(data_toronto)
NROW(data_toronto_condo)
NROW(data_toronto_condo)/NROW(data_toronto) * 100

data_toronto_condo %>% subset(Status == "Sold") %>% nrow()
data_toronto_condo %>% subset(!is.na(data_toronto_condo$SoldPrice)) %>% nrow()

#--------------------------------------------------------------------------------------
## Data Selection for Price Prediction Modeling
#--------------------------------------------------------------------------------------

# We will focus on Condo properties in Toronto area:
# Toronto area has 31% (12,926 records) of overall Ontario sales volume.
# Condo has 47% (6,109) of Toronto Sales volume.
#--------------------------------------------------------------------------------------

house_data <- full_data %>%
  subset(Area == "Toronto" & 
         Type == "Condo" & 
         Status == "Sold" & !is.na(full_data$SoldPrice))

# After eliminating missing price value, the selected data has 5,425 records
NROW(house_data) #--5425

# Price Summary
house_data %>% 
  group_by(Type) %>%
  summarise(Count = n(), 
            min   = min(SoldPrice), max = max(SoldPrice), 
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

dim(features_data) #--5425

summary(features_data)

#--------------------------------------------------------------------------------------
## Data Cleanup
#--------------------------------------------------------------------------------------

# Check missing values
summarise_all(features_data, funs( sum(is.na(.)) ) )

# Drop Age column - it has 56.5% (3,062 of 5,425) records missing value
features_data = select(features_data, -Age)
dim(features_data) #--5425

#Removing the outliers
#--------------------------------------------------------------------------------------

clean_data <- features_data
dim(clean_data) #--5425

# Check and remove Price outliers
#boxplot(clean_data$SoldPrice)
outliers <- boxplot.stats(clean_data$SoldPrice)$out
clean_data <- clean_data[-which(clean_data$SoldPrice %in% outliers),]
#boxplot(clean_data$SoldPrice)
dim(clean_data) #--5111

# Check and remove Taxes outliers
#boxplot(clean_data$Taxes)
clean_data <- clean_data %>% subset(Taxes > 1000)
outliers <- boxplot.stats(clean_data$Taxes)$out
clean_data <- clean_data[-which(clean_data$Taxes %in% outliers),]
#boxplot(clean_data$Taxes)
dim(clean_data) #--4496 #--4414

# Check and remove Rooms outliers
#boxplot(clean_data$Rooms)
outliers <- boxplot.stats(clean_data$Rooms)$out
clean_data <- clean_data[-which(clean_data$Rooms %in% outliers),]
#boxplot(clean_data$Rooms)
dim(clean_data) #--4133

# Check Bedrooms outliers
#boxplot(clean_data$Bedrooms)

# Check Bathrooms outliers
#boxplot(clean_data$Bathrooms)

# Check and remove ParkingTotal outliers
#boxplot(clean_data$ParkingTotal)
clean_data <- clean_data %>% subset(ParkingTotal <= 2)
#outliers <- boxplot.stats(clean_data$ParkingTotal)$out
#clean_data <- clean_data[-which(clean_data$ParkingTotal %in% outliers),]
#boxplot(clean_data$ParkingTotal)
dim(clean_data) #--4125

NROW(features_data)
NROW(clean_data)
NROW(clean_data)/NROW(features_data) * 100

summary(clean_data)

# Check missing values and replace them with column mean/median values if any
#--------------------------------------------------------------------------------------

dim(clean_data) #--4125
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

condo_model_data <- clean_data

# Use 80/20 Train/Test Split
#--------------------------------------------------------------------------------------
set.seed(123)
ind<-sample(2, nrow(condo_model_data), replace=T, prob=c(0.8,0.2) )
train_data <- condo_model_data[ind==1,]
test_data  <- condo_model_data[ind==2,]

dim(condo_model_data) #--4125
dim(train_data) #--3281
dim(test_data)  #--844

attach(train_data)

# Create a Multiple Linear Regression Model with all relevant attributes
#--------------------------------------------------------------------------------------

condo_model1a <- lm(SoldPrice ~ Rooms+Bedrooms+Bathrooms+Taxes+FinishedBasement+NearSchool+NearPark)
# Check the Model summary
summary(condo_model1a)

# The output shows:
#
# 1) R-squared value is 0.7953 -- which is not too bad.
# This value means that predictors explain 83.96% of the variability in Sold Price.
#
# 2) Residual standard error: 61080 on 3273 degrees of freedom
# The Mean Squared Error is: 61080 - this is too large.


# Create a new Multiple Linear Regression Model - with less predictor variables
#--------------------------------------------------------------------------------------

condo_model1b <- lm(SoldPrice ~ Rooms+Bathrooms+Taxes)
# Check the Model summary
summary(condo_model1b)

# The output shows:
#
# 1) R-squared value is 0.7928 -- which is more or less same as before
#
# 2) Residual standard error: 61420 on 3277 degrees of freedom -- very similar to before


# Create a new Multiple Linear Regression Model - with different predictor variables
#--------------------------------------------------------------------------------------

condo_model1c <- lm(SoldPrice ~ Bedrooms+Bathrooms+Taxes)
# Check the Model summary
summary(condo_model1c)

# The output shows:
#
# 1) R-squared value is 0.7941 -- which is more or less same as before
#
# 2) Residual standard error: 61420 on 3277 degrees of freedom -- very similar to before


#--------------------------------------------------------------------------------------
# Transformations
#--------------------------------------------------------------------------------------

# Lets see if it is possible to improve our model by transforming some of our predictors, 
# either with a square root transformation or a log transformation.
#--------------------------------------------------------------------------------------

# Create a new Multiple Linear Regression Model - with log transformation
#--------------------------------------------------------------------------------------

condo_model2a <- lm(log(SoldPrice) ~ Bedrooms+Bathrooms+log(Taxes) )
# Check the Model summary
summary(condo_model2a)

# The output shows:
#
# 1) R-squared value is 0.7956 which slightly better than before.
#
# 2) Residual standard error: 0.09639 on 3277 degrees of freedom
# The Mean Squared Error is much-much lower than the first model.


# Create a new Multiple Linear Regression Model - with square root transformation
#--------------------------------------------------------------------------------------

condo_model2b <- lm(sqrt(SoldPrice) ~ Bedrooms+Bathrooms+sqrt(Taxes) )
# Check the Model summary
summary(condo_model2b)

# The output shows:
#--------------------------------------------------------------------------------------
# 1) R-squared value is 0.7977 -- which is better than before
#
# 2) Residual standard error: 37.87 on 3277 degrees of freedom
# The Mean Squared Error is much lower the first model


## Call:
##   lm(formula = sqrt(SoldPrice) ~ Bedrooms + Bathrooms + sqrt(Taxes))
## 
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -251.721  -21.949   -0.407   22.044  208.669 
## 
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 248.7004     4.8117  51.686  < 2e-16 ***
##   Bedrooms     10.8988     1.5000   7.266  4.6e-13 ***
##   Bathrooms     3.9425     1.8646   2.114   0.0346 *  
##   sqrt(Taxes)  11.1311     0.1058 105.185  < 2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 37.87 on 3277 degrees of freedom
## Multiple R-squared:  0.7977,	Adjusted R-squared:  0.7975 
## F-statistic:  4308 on 3 and 3277 DF,  p-value: < 2.2e-16


# We can check the Confidence Intervals of the Coefficients 
confint(condo_model2b, level=.95)

# Check the residuals
resid<- condo_model2b$residuals
hist(resid)

qqnorm(resid)
qqline(resid)


#--------------------------------------------------------------------------------------
## Using the Model
# We are going to use the last model above for our price prediction
#--------------------------------------------------------------------------------------

dim(test_data) #--844

head(test_data)

# Prediction with 95% confidence interval
pred_matrix <- predict(condo_model2b, test_data, interval="predict")

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