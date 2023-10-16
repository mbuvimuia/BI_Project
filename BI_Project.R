##STEP 1: Load the dataset ----
library(readr)
loan_data_2015 <- read_csv("data/loan_data_2015.csv")
View(loan_data_2015)


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

