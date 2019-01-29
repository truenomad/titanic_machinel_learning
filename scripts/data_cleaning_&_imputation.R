# Get all the pakcages you need
library(ggplot2) # visualisation
library(Amelia) # missing data visualisation
library(caret)
library(dplyr) # data manipulation
library(randomForest) # Classification algorithm 
library(epiR) # For measuring OR  


# Check and set the working directory
getwd()
setwd("/Users/mohamedyusuf/R/Kaggle/Titanic/Data/")

# Load data
train <- read.csv("/Users/mohamedyusuf/R/Kaggle/Titanic/Data/train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)

# Combine train and test data. Before combining makes sure the columns match, create a Survived variable in the test data
test$Survived <- NA   
# combine data
full <- rbind(train, test)

# Check the data
head(full, 30)
tail(full, 30)
# check the structure of the data
str(full)


# Any missing data?
colSums(is.na(full))

# Age has 263 missing values, fare has 1 missing values. 

# Any empty data?

colSums(full == "")
# Cabin has 1014 empty values whereas embarked has 2.

# Visualise missing data
missmap(full, main = "Missing values vs observed")

# Drop the cabin variable

full$Cabin <- NULL
# check to see if variable has been removed
head(full,5) # it has been removed

### Imputation for Embarked Variable

```{r, message=FALSE, warning=FALSE}
# Let us find the most common port so that we can impute it into the missing values
table(full$Embarked)
# it appears that S is the most common port

# Impute s into the missing values 
full[full$Embarked == "", "Embarked"] <- "S"

# Check to see what's changed 
table(full$Embarked)
# We don't have any missing values for Embarked anymore

### Random Sampling for Missing Age 

```{r, message=FALSE, warning=FALSE}

# Make new Age column
age <- full$Age
n = length(age)

# Replace missing value with a random sample from raw data
set.seed(1)
for(i in 1:n){
  if(is.na(age[i])){
    age[i] = sample(na.omit(full$Age),1)
  }
}

# Plot graph to see the effect of the change
ggplot(full, aes(age))+
  geom_histogram(fill="Dark Green", color = "Black") +
  labs(title="Before replacement") 
ggplot(full, aes(Age) ) +
  geom_histogram(fill="Dark Blue", color = "Black") +
  labs(title="After replacement")

full$Age <- age
colSums(is.na(full)) ###check
# The are 0 missing values for age now.

### Fare Imputation
```{r, message=FALSE, warning=FALSE}

# Find out other information on the individual with now fare data
full[is.na(full$Fare),]

# Passenger 1044 is from pclass 3 and has embarked from port 3
# Before we use the mean or median, we need to first have a look at the distribution 
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = 'light green', alpha=0.5)
# The data seems to be skewed to the right. 
# In this case the median is the best approach to help us impute the Fare value

# So we use the Pclass and Embarked values to help us impute the Fare Value for passenger 1044.
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

# Check to see if we have made any changes to the data.
full[1044,] # Yes we have!



### Categorical Casting and Data Splitting


# Make sure the variables are in the right structure
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Name', 'Survived')
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Split Data apart
# Split the train model
train.cl <- full[1:891,]

## plit the test model 
test.cl <- full[892:1309,]
# Check to see if it split right!
str(train.cl)
str(test.cl)
```