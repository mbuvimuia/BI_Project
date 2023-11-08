##STEP 1: Required Packages ----
##Install the required packages ----
### dplyr 
if (!is.element("dplyr", installed.packages()[, 1])) {
  install.packages("dplyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("dplyr")

## naniar
if (!is.element("naniar", installed.packages()[, 1])) {
  install.packages("naniar", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("naniar")

## ggplot2
# We require the "ggplot2" package to create more appealing visualizations
if (!is.element("ggplot2", installed.packages()[, 1])) {
  install.packages("ggplot2", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("ggplot2")

## MICE
# We use the MICE package to perform data imputation
if (!is.element("mice", installed.packages()[, 1])) {
  install.packages("mice", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("mice")

## Amelia
if (!is.element("Amelia", installed.packages()[, 1])) {
  install.packages("Amelia", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("Amelia")

if (require("stats")) {
  require("stats")
} else {
  install.packages("stats", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## mlbench 
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret 
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## MASS 
if (require("MASS")) {
  require("MASS")
} else {
  install.packages("MASS", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## glmnet 
if (require("glmnet")) {
  require("glmnet")
} else {
  install.packages("glmnet", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## e1071 
if (require("e1071")) {
  require("e1071")
} else {
  install.packages("e1071", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## kernlab 
if (require("kernlab")) {
  require("kernlab")
} else {
  install.packages("kernlab", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## rpart 
if (require("rpart")) {
  require("rpart")
} else {
  install.packages("rpart", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
## factoextra ----
if (require("factoextra")) {
  require("factoextra")
} else {
  install.packages("factoextra", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## FactoMineR ----
if (require("FactoMineR")) {
  require("FactoMineR")
} else {
  install.packages("FactoMineR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

##STEP 2: Load the dataset ----
library(readr)
loan_data_2015 <- read_csv("data/loan_data_2015.csv", 
                           col_types = cols(desc = col_skip(), next_pymnt_d = col_skip(), 
                                            ...54 = col_skip()))
View(loan_data_2015)

##STEP 3: Data Imputation ----
##Confirm the missingness of data before data imputation ----
# Are there missing values in the dataset?
any_na(loan_data_2015)

# How many?
n_miss(loan_data_2015)

# What is the percentage of missing data in the entire dataset?
prop_miss(loan_data_2015)

# How many missing values does each variable have?
loan_data_2015 %>% is.na() %>% colSums()

# What is the number and percentage of missing values grouped by
# each variable?
miss_var_summary(loan_data_2015)

# What is the number and percentage of missing values grouped by
# each observation?
miss_case_summary(loan_data_2015)

# Which variables contain the most missing values?
gg_miss_var(loan_data_2015)


# ##Remove Missing Data: Option 1
# loan_dataset_removed_obs <- loan_data_2015 %>% filter(complete.cases(.))
# 
# 
# # Are there missing values in the dataset after removing missing data?
# any_na(loan_dataset_removed_obs)
# dim(loan_dataset_removed_obs)

##Remove Missing Data: Option 2
loan_dataset_removed_vars <-
  loan_data_2015 %>% dplyr::select(-mths_since_last_record, -mths_since_last_major_derog, 
                           -emp_title, -emp_length, -last_pymnt_d, -revol_util, -title, -last_credit_pull_d)

dim(loan_dataset_removed_vars)


any_na(loan_dataset_removed_vars)

miss_var_summary(loan_dataset_removed_vars)

#Remove the observations
loan_removed_vars_obs <- loan_dataset_removed_vars %>% filter(complete.cases(.))

dim(loan_removed_vars_obs)

any_na(loan_removed_vars_obs)

miss_var_summary(loan_removed_vars_obs)

#Remove unwanted columns
df <- loan_removed_vars_obs[, -c(10, 14, 16, 17, 19, 20, 23, 43)]
loan_data <- df

##STEP 4: Basic Exploratory Data Analysis ----
###Install the required packages ----
if (!is.element("renv", installed.packages()[, 1])) {
  install.packages("renv", dependencies = TRUE)
}
require("renv")

if (!is.element("languageserver", installed.packages()[, 1])) {
  install.packages("languageserver", dependencies = TRUE)
}
require("languageserver")

if (!is.element("e1071", installed.packages()[, 1])) {
  install.packages("e1071", dependencies = TRUE)
}
require("e1071")

###Previewing the dataset and Identify the datatypes ----
dim(loan_data)
sapply(loan_data, class)

#DESCRIPTIVE STATISTICS ----
###Identify the number of instances that belong to each class----
#Count Categorical data
loan_data_purpose_freq <- loan_data$purpose
cbind(frequency = table(loan_data_purpose_freq),
      percentage = prop.table(table(loan_data_purpose_freq)) * 100)

loan_data_loanstatus_freq <- loan_data$loan_status
cbind(frequency = table(loan_data_loanstatus_freq),
      percentage = prop.table(table(loan_data_loanstatus_freq)) * 100)

loan_data_verificationstatus_freq <- loan_data$verification_status
cbind(frequency = table(loan_data_verificationstatus_freq),
      percentage = prop.table(table(loan_data_verificationstatus_freq)) * 100)


#MEASURE OF CENTRAL TENDENCY ----
##Calculate the mode----
loan_data_purpose_mode <- names(table(loan_data$purpose))[
  which(table(loan_data$purpose) == max(table(loan_data$purpose)))
]
print(loan_data_purpose_mode)


loan_data_loanstatus_mode <- names(table(loan_data$loan_status))[
  which(table(loan_data$loan_status) == max(table(loan_data$loan_status)))
]
print(loan_data_loanstatus_mode)



# MEASURES OF Distribution/Dispersion/Spread/Scatter/Variability ----
##Measure of distribution for each variable----
summary(loan_data)

##Measure the standard deviation for each variable----

sapply(loan_data[, 1:47], sd)

## Measure the variance for each variable----

sapply(loan_data[, 1:47], var)

## Measure the kurtosis of each variable----
sapply(loan_data [, c( -1,-2,-6,-9,-10,-11,
                                   -13,-14,-15,-16,-17,-18,-19,-20,
                                   -23, -30, -43)], kurtosis, type = 2)

##Measure the skewness of each variable----
sapply(loan_data [, c( -1,-2,-6,-9,-10,-11,
                                   -13,-14,-15,-16,-17,-18,-19,-20,
                                   -23, -30, -43)], skewness, type = 2)

## Measure the covariance between variables ----

loan_data_cov <- cov(loan_data[, c( -1,-2,-6,-9,-10,-11,
                                             -13,-14,-15,-16,-17,-18,-19,-20,
                                             -23, -30, -43)])
View(loan_data_cov)

## Measure the correlation between variables ----
loan_data_cor <- cor(loan_data[c( -1,-2,-6,-9,-10,-11,
                                              -13,-14,-15,-16,-17,-18,-19,-20,
                                              -23, -30, -43)])
View(loan_data_cor)

## Create histograms for each numeric attribute
pub_rec <- as.numeric(unlist(loan_data[, 20]))
open_acc <- as.numeric(unlist(loan_data[, 19]))
month_since_last_deliq <- as.numeric(unlist(loan_data[, 18]))
collections <- as.numeric(unlist(loan_data[,34]))
delinq_2yrs <- as.numeric(unlist(loan_data[,16]))
total_payment <- as.numeric(unlist(loan_data[,26]))

par(mfrow = c(1, 1))

hist(pub_rec, main = names(loan_data)[20], xlim = c(0,100), ylim = c(0,30))
hist(open_acc, main = names(loan_data)[19], xlim = c(0,100), ylim = c(0,30))
hist(month_since_last_deliq, main = names(loan_data)[18], xlim = c(0,100), ylim = c(0,30))
hist(collections, main = names(loan_data)[34], xlim = c(0,20), ylim = c(0,50))
hist(delinq_2yrs, main = names(loan_data)[16], xlim = c(0,40), ylim = c(0,20))
hist(total_payment, main = names(loan_data)[26], xlim = c(0,44000), ylim = c(0,20))


#STEP 5: Data Transformation ----
library(dplyr)

numerical_col <- loan_data %>% 
  select_if(is.numeric)