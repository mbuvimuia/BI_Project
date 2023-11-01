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
dim(loan_removed_vars_obs)
sapply(loan_removed_vars_obs, class)

#DESCRIPTIVE STATISTICS ----
###Identify the number of instances that belong to each class----
#Count Categorical data
loan_removed_vars_obs_purpose_freq <- loan_removed_vars_obs$purpose
cbind(frequency = table(loan_removed_vars_obs_purpose_freq),
      percentage = prop.table(table(loan_removed_vars_obs_purpose_freq)) * 100)

loan_removed_vars_obs_loanstatus_freq <- loan_removed_vars_obs$loan_status
cbind(frequency = table(loan_removed_vars_obs_loanstatus_freq),
      percentage = prop.table(table(loan_removed_vars_obs_loanstatus_freq)) * 100)

loan_removed_vars_obs_verificationstatus_freq <- loan_removed_vars_obs$verification_status
cbind(frequency = table(loan_removed_vars_obs_verificationstatus_freq),
      percentage = prop.table(table(loan_removed_vars_obs_verificationstatus_freq)) * 100)


#MEASURE OF CENTRAL TENDENCY ----
##Calculate the mode----
loan_removed_vars_obs_purpose_mode <- names(table(loan_removed_vars_obs$purpose))[
  which(table(loan_removed_vars_obs$purpose) == max(table(loan_removed_vars_obs$purpose)))
]
print(loan_removed_vars_obs_purpose_mode)


loan_removed_vars_obs_loanstatus_mode <- names(table(loan_removed_vars_obs$loan_status))[
  which(table(loan_removed_vars_obs$loan_status) == max(table(loan_removed_vars_obs$loan_status)))
]
print(loan_removed_vars_obs_loanstatus_mode)



# MEASURES OF Distribution/Dispersion/Spread/Scatter/Variability ----
##Measure of distribution for each variable----
summary(loan_removed_vars_obs)

##Measure the standard deviation for each variable----

sapply(loan_removed_vars_obs[, 1:47], sd)

## Measure the variance for each variable----

sapply(loan_removed_vars_obs[, 1:47], var)

## Measure the kurtosis of each variable----
sapply(loan_removed_vars_obs [, c( -1,-2,-6,-9,-10,-11,
                                   -13,-14,-15,-16,-17,-18,-19,-20,
                                   -23, -30, -43)], kurtosis, type = 2)

##Measure the skewness of each variable----
sapply(loan_removed_vars_obs [, c( -1,-2,-6,-9,-10,-11,
                                   -13,-14,-15,-16,-17,-18,-19,-20,
                                   -23, -30, -43)], skewness, type = 2)

## Measure the covariance between variables ----

loan_data_cov <- cov(loan_removed_vars_obs[, c( -1,-2,-6,-9,-10,-11,
                                             -13,-14,-15,-16,-17,-18,-19,-20,
                                             -23, -30, -43)])
View(loan_data_cov)

## Measure the correlation between variables ----
loan_data_cor <- cor(loan_removed_vars_obs[c( -1,-2,-6,-9,-10,-11,
                                              -13,-14,-15,-16,-17,-18,-19,-20,
                                              -23, -30, -43)])
View(loan_data_cor)

## Create histograms for each numeric attribute
# hist(loan_removed_vars_obs[, 8], main = names(loan_removed_vars_obs)[8])
# hist(loan_removed_vars_obs[, 12], main = names(loan_removed_vars_obs)[12])
# hist(loan_removed_vars_obs[, 21], main = names(loan_removed_vars_obs)[21])
# hist(loan_removed_vars_obs[, 22], main = names(loan_removed_vars_obs)[22])
# hist(loan_removed_vars_obs[, 24], main = names(loan_removed_vars_obs)[24])
# hist(loan_removed_vars_obs[, 25], main = names(loan_removed_vars_obs)[25])
# hist(loan_removed_vars_obs[, 26], main = names(loan_removed_vars_obs)[26])
# hist(loan_removed_vars_obs[, 27], main = names(loan_removed_vars_obs)[27])
# hist(loan_removed_vars_obs[, 28], main = names(loan_removed_vars_obs)[28])
# hist(loan_removed_vars_obs[, 29], main = names(loan_removed_vars_obs)[29])
# hist(loan_removed_vars_obs[, 31], main = names(loan_removed_vars_obs)[31])
# hist(loan_removed_vars_obs[, 32], main = names(loan_removed_vars_obs)[32])
# hist(loan_removed_vars_obs[, 33], main = names(loan_removed_vars_obs)[33])
# hist(loan_removed_vars_obs[, 34], main = names(loan_removed_vars_obs)[34])
# hist(loan_removed_vars_obs[, 35], main = names(loan_removed_vars_obs)[35])
# hist(loan_removed_vars_obs[, 36], main = names(loan_removed_vars_obs)[36])
# hist(loan_removed_vars_obs[, 37], main = names(loan_removed_vars_obs)[37])
# hist(loan_removed_vars_obs[, 38], main = names(loan_removed_vars_obs)[38])
# hist(loan_removed_vars_obs[, 39], main = names(loan_removed_vars_obs)[39])
# hist(loan_removed_vars_obs[, 40], main = names(loan_removed_vars_obs)[40])
# hist(loan_removed_vars_obs[, 41], main = names(loan_removed_vars_obs)[41])
# hist(loan_removed_vars_obs[, 42], main = names(loan_removed_vars_obs)[42])
# hist(loan_removed_vars_obs[, 44], main = names(loan_removed_vars_obs)[44])
# hist(loan_removed_vars_obs[, 45], main = names(loan_removed_vars_obs)[45])
# hist(loan_removed_vars_obs[, 46], main = names(loan_removed_vars_obs)[46])
# hist(loan_removed_vars_obs[, 47], main = names(loan_removed_vars_obs)[47])


#STEP 5: Data Transformation ----
library(dplyr)

numerical_col <- loan_removed_vars_obs %>% 
  select_if(is.numeric)