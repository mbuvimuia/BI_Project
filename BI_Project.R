##STEP 1: Load the dataset ----
library(readr)
loan_data_2015 <- read_csv("data/loan_data_2015.csv", 
                           col_types = cols(desc = col_skip(), next_pymnt_d = col_skip(), 
                                            ...54 = col_skip()))
View(loan_data_2015)

##STEP 3: Data Imputation ----
###Install the required packages ----
## dplyr 
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


##Remove Missing Data: Option 1
loan_dataset_removed_obs <- loan_data_2015 %>% filter(complete.cases(.))


# Are there missing values in the dataset after removing missing data?
any_na(loan_dataset_removed_obs)

dim(loan_dataset_removed_obs)

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
##STEP 3: Exploratory Data Analysis ----
###Install the required packages ----
if (!is.element("renv", installed.packages()[, 1])) {
  install.packages("renv", dependencies = TRUE)
}
require("renv")

if (!is.element("languageserver", installed.packages()[, 1])) {
  install.packages("languageserver", dependencies = TRUE)
}
require("languageserver")

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

## Measure the kurtosis for each variable----
if (!is.element("e1071", installed.packages()[, 1])) {
  install.packages("e1071", dependencies = TRUE)
}
require("e1071")

sapply(loan_removed_vars_obs[, c(-1,-2,-6,-9,-10,-11,-13,-14,-15,-16,-17,-18,-19,-20
                                -23, -30, -43)], kurtosis, type = 2)
##Measure the skewness for each variable----

sapply(loan_removed_vars_obs[, -6, -9, -10, -11, -13, -15, -16, 
                      -17, -18, -19, -20, -21, -22, -23, -24, -27, -36, -46, -48, -49, -53, ], skewness, type = 2)

# MEASURES THE COVARIANCE BETWEEN VARIABLES----
##Measures of relationship----
loan_removed_vars_obs_cov <- cov(loan_removed_vars_obs[, -6, -9, -10, -11, -13, -15, -16, 
                                         -17, -18, -19, -20, -21, -22, -23, -24, -27, -36, -46, -48, -49, -53, ])
View(loan_removed_vars_obs_cov)

