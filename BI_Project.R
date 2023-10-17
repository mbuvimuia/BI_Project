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
##Select 1000 observations to be included in the dataset ----
rand_ind <- sample(seq_len(nrow(loan_data_2015)), 1000)
loan_dataset <- loan_data_2015[rand_ind, ]

##Confirm the missingness of data before data imputation ----
# Are there missing values in the dataset?
any_na(loan_dataset)

# How many?
n_miss(loan_dataset)

# What is the percentage of missing data in the entire dataset?
prop_miss(loan_dataset)

# How many missing values does each variable have?
loan_dataset %>% is.na() %>% colSums()

# What is the number and percentage of missing values grouped by
# each variable?
miss_var_summary(loan_dataset)

# What is the number and percentage of missing values grouped by
# each observation?
miss_case_summary(loan_dataset)

# Which variables contain the most missing values?
gg_miss_var(loan_dataset)

# Where are missing values located (the shaded regions in the plot)?
vis_miss(loan_dataset) + theme(axis.text.x = element_text(angle = 80))

# Which combinations of variables are missing together?
gg_miss_upset(loan_dataset)


#Imputation Process
loan_daataset_numeric <- loan_dataset %>%
  select(loan_amnt, funded_amnt, funded_amnt_inv, 
         int_rate,installment, emp_title,emp_length,annual_inc,
        dti, delinq_2yrs, inq_last_6mths, mths_since_last_delinq,
        mths_since_last_record,open_acc, pub_rec, revol_bal, 
        revol_util, total_acc, out_prncp_inv,total_pymnt, total_pymnt_inv,
        total_rec_prncp, total_rec_int, total_rec_late_fee, recoveries, collection_recovery_fee
        , last_pymnt_d, last_pymnt_amnt, last_credit_pull_d, collections_12_mths_ex_med,
        mths_since_last_major_derog, policy_code, acc_now_delinq, tot_coll_amt,
        tot_cur_bal, total_rev_hi_lim, )

md.pattern(loan_daataset_numeric)




##STEP 2: Exploratory Data Analysis ----
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
dim(loan_data_2015)
sapply(loan_data_2015, class)

#DESCRIPTIVE STATISTICS ----
###Identify the number of instances that belong to each class----
#Count Categorical data
loan_data_2015_purpose_freq <- loan_data_2015$purpose
cbind(frequency = table(loan_data_2015_purpose_freq),
      percentage = prop.table(table(loan_data_2015_purpose_freq)) * 100)

loan_data_2015_title_freq <- loan_data_2015$title
cbind(frequency = table(loan_data_2015_title_freq),
      percentage = prop.table(table(loan_data_2015_title_freq)) * 100)

loan_data_2015_loanstatus_freq <- loan_data_2015$loan_status
cbind(frequency = table(loan_data_2015_loanstatus_freq),
      percentage = prop.table(table(loan_data_2015_loanstatus_freq)) * 100)

loan_data_2015_verificationstatus_freq <- loan_data_2015$verification_status
cbind(frequency = table(loan_data_2015_verificationstatus_freq),
      percentage = prop.table(table(loan_data_2015_verificationstatus_freq)) * 100)





#MEASURE OF CENTRAL TENDENCY ----
##Calculate the mode----
loan_data_2015_purpose_mode <- names(table(loan_data_2015$purpose))[
  which(table(loan_data_2015$purpose) == max(table(loan_data_2015$purpose)))
]
print(loan_data_2015_purpose_mode)

loan_data_2015_title_mode <- names(table(loan_data_2015$title))[
  which(table(loan_data_2015$title) == max(table(loan_data_2015$title)))
]
print(loan_data_2015_title_mode)

loan_data_2015_loanstatus_mode <- names(table(loan_data_2015$loan_status))[
  which(table(loan_data_2015$loan_status) == max(table(loan_data_2015$loan_status)))
]
print(loan_data_2015_loanstatus_mode)



# MEASURES OF Distribution/Dispersion/Spread/Scatter/Variability ----
##Measure of distribution for each variable----
summary(loan_data_2015)

##Measure the standard deviation for each variable----

sapply(loan_data_2015[, 1:74], sd)

## Measure the variance for each variable----

sapply(loan_data_2015[, 1:74], var)

## Measure the kurtosis for each variable----
if (!is.element("e1071", installed.packages()[, 1])) {
  install.packages("e1071", dependencies = TRUE)
}
require("e1071")

sapply(loan_data_2015[, -6, -9, -10, -11, -13, -15, -16, 
                      -17, -18, -19, -20, -21, -22, -23, -24, -27, -36, -46, -48, -49, -53, ], kurtosis, type = 2)
##Measure the skewness for each variable----

sapply(loan_data_2015[, -6, -9, -10, -11, -13, -15, -16, 
                      -17, -18, -19, -20, -21, -22, -23, -24, -27, -36, -46, -48, -49, -53, ], skewness, type = 2)

# MEASURES THE COVARIANCE BETWEEN VARIABLES----
##Measures of relationship----
loan_data_2015_cov <- cov(loan_data_2015[, -6, -9, -10, -11, -13, -15, -16, 
                                         -17, -18, -19, -20, -21, -22, -23, -24, -27, -36, -46, -48, -49, -53, ])
View(loan_data_2015_cov)

