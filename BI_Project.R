##STEP 1: Required Packages ----
##Install the required packages ----
##dplyr -----
if (!is.element("dplyr", installed.packages()[, 1])) {
  install.packages("dplyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("dplyr")

## naniar ----
if (!is.element("naniar", installed.packages()[, 1])) {
  install.packages("naniar", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("naniar")

## ggplot2 ----
# We require the "ggplot2" package to create more appealing visualizations
if (!is.element("ggplot2", installed.packages()[, 1])) {
  install.packages("ggplot2", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("ggplot2")

## MICE ----
# We use the MICE package to perform data imputation
if (!is.element("mice", installed.packages()[, 1])) {
  install.packages("mice", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("mice")

## Amelia ----
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

## mlbench ----
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## MASS ----
if (require("MASS")) {
  require("MASS")
} else {
  install.packages("MASS", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## glmnet ----
if (require("glmnet")) {
  require("glmnet")
} else {
  install.packages("glmnet", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## e1071 ----
if (require("e1071")) {
  require("e1071")
} else {
  install.packages("e1071", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## kernlab ----
if (require("kernlab")) {
  require("kernlab")
} else {
  install.packages("kernlab", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## rpart----
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

## klaR ----
if (require("klaR")) {
  require("klaR")
} else {
  install.packages("klaR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## LiblineaR ----
if (require("LiblineaR")) {
  require("LiblineaR")
} else {
  install.packages("LiblineaR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## naivebayes ----
if (require("naivebayes")) {
  require("naivebayes")
} else {
  install.packages("naivebayes", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## pROC ----
if (require("pROC")) {
  require("pROC")
} else {
  install.packages("pROC", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
## randomForest ----
if (require("randomForest")) {
  require("randomForest")
} else {
  install.packages("randomForest", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caretEnsemble ---- for stacking
if (require("caretEnsemble")) {
  require("caretEnsemble")
} else {
  install.packages("caretEnsemble", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## C50 ---- for boosting
if (require("C50")) {
  require("C50")
} else {
  install.packages("C50", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## adabag ----
if (require("adabag")) {
  require("adabag")
} else {
  install.packages("adabag", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## RRF ----
if (require("RRF")) {
  require("RRF")
} else {
  install.packages("RRF", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## plumber ----
if (require("plumber")) {
  require("plumber")
} else {
  install.packages("plumber", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

##httr ----

if (require("httr")) {
  require("httr")
} else {
  install.packages("httr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## jsonlite ----
if (require("jsonlite")) {
  require("jsonlite")
} else {
  install.packages("jsonlite", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}


##STEP 2: Load the dataset ----
library(readr)
loan_data_2015 <- read_csv("data/loan_data_2015.csv", 
                           col_types = cols(desc = col_skip(), next_pymnt_d = col_skip(), 
                                            ...54 = col_skip()))
View(loan_data_2015)

##STEP 3: Data Imputation ----
###Confirm the missingness of data before data imputation ----
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

##Visualization of Missing Data----
if (!is.element("Amelia", installed.packages()[, 1])) {
  install.packages("Amelia", dependencies = TRUE)
}
require("Amelia")

missmap(loan_data_2015, col = c("red", "grey"), legend = TRUE)

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
###Count Categorical data
loan_data_purpose_freq <- loan_data$purpose
cbind(frequency = table(loan_data_purpose_freq),
      percentage = prop.table(table(loan_data_purpose_freq)) * 100)

loan_data_loanstatus_freq <- loan_data$loan_status
cbind(frequency = table(loan_data_loanstatus_freq),
      percentage = prop.table(table(loan_data_loanstatus_freq)) * 100)

loan_data_verificationstatus_freq <- loan_data$verification_status
cbind(frequency = table(loan_data_verificationstatus_freq),
      percentage = prop.table(table(loan_data_verificationstatus_freq)) * 100)

loan_data_term_freq <- loan_data$term
cbind(frequency = table(loan_data_term_freq),
      percentage  = prop.table(table(loan_data_term_freq))* 100)

loan_data_grade <- loan_data$grade
cbind(frequency = table(loan_data_grade),
      percentage = prop.table(table(loan_data_grade)) * 100 )

loan_data_home <- loan_data$home_ownership
cbind(frequency = table(loan_data_home),
      percentage = prop.table(table(loan_data_home)) * 100)


#MEASURE OF CENTRAL TENDENCY ----
###Calculate the mode----
loan_data_purpose_mode <- names(table(loan_data$purpose))[
  which(table(loan_data$purpose) == max(table(loan_data$purpose)))
]
print(loan_data_purpose_mode)


loan_data_loanstatus_mode <- names(table(loan_data$loan_status))[
  which(table(loan_data$loan_status) == max(table(loan_data$loan_status)))
]
print(loan_data_loanstatus_mode)

loan_data_grade_mode <- names(table(loan_data$grade))[
  which(table(loan_data$grade) == max(table(loan_data$grade)))
]
print(loan_data_grade_mode)

loan_data_home_mode <- names(table(loan_data$home_ownership))[
  which(table(loan_data$home_ownership) == max(table(loan_data$home_ownership)))
]
print(loan_data_home_mode)

loan_data_term_mode <- names(table(loan_data$term))[
  which(table(loan_data$term) == max(table(loan_data$term)))
]
print(loan_data_term_mode)




# MEASURES OF Distribution/Dispersion/Spread/Scatter/Variability ----
###Measure of distribution for each variable----
summary(loan_data)

###Measure the standard deviation for each variable----

sapply(loan_data[, 1:39], sd)

###Measure the variance for each variable----

sapply(loan_data[, 1:39], var)

###Measure the kurtosis of each variable----
sapply(loan_data [, c( -1, -2, -6, -9, -10, -12, -13, -14, -23, -35)], kurtosis, type = 2)

###Measure the skewness of each variable----
sapply(loan_data [, c( -1, -2, -6, -9, -10, -12, -13, -14, -23, -35)], skewness, type = 2)

###Measure the covariance between variables ----

loan_data_cov <- cov(loan_data[, c(-1, -2, -6, -9, -10, -12, -13, -14, -23, -35)])
View(loan_data_cov)

###Measure the correlation between variables ----
loan_data_cor <- cor(loan_data[c(-1, -2, -6, -9, -10, -12, -13, -14, -23, -35)])
View(loan_data_cor)


#BASIC VISUALIZATION----
##Univariate plots ----
### Create histograms for numeric attribute----
pub_rec <- as.numeric(unlist(loan_data[, 20]))
open_acc <- as.numeric(unlist(loan_data[, 19]))
month_since_last_deliq <- as.numeric(unlist(loan_data[, 18]))
collections <- as.numeric(unlist(loan_data[,34]))
delinq_2yrs <- as.numeric(unlist(loan_data[,16]))
total_rec_late_fee <- as.numeric(unlist(loan_data[, 30]))

par(mfrow = c(1, 1))

hist(pub_rec, main = names(loan_data)[20], xlim = c(0,100), ylim = c(0,30))
hist(open_acc, main = names(loan_data)[19], xlim = c(0,100), ylim = c(0,30))
hist(month_since_last_deliq, main = names(loan_data)[18], xlim = c(0,200), ylim = c(0,500))
hist(collections, main = names(loan_data)[34], xlim = c(0,15), ylim = c(0,100))
hist(delinq_2yrs, main = names(loan_data)[16], xlim = c(0,40), ylim = c(0,20))
hist(total_rec_late_fee, main = names(loan_data)[30], xlim = c(0,170), ylim = c(0,50))

### Create Bar plot for Categorical data ----
barplot(table(loan_data[, 10]), main = names(loan_data)[10])
barplot(table(loan_data[, 9]), main = names(loan_data)[9])
barplot(table(loan_data[, 6]), main = names(loan_data)[6])
barplot(table(loan_data[, 12]), main = names(loan_data)[12])
barplot(table(loan_data[, 10]), main = names(loan_data)[10])
barplot(table(loan_data[, 13]), main = names(loan_data)[13])
barplot(table(loan_data[, 14]), main = names(loan_data)[14])


#STEP 5: Data Transformation ----
###Scale Data Transform----
#BEFORE
summary(loan_data)

model_of_the_transform <- preProcess(loan_data, method = c("scale"))
print(model_of_the_transform)
loan_data_scale_transform <- predict(model_of_the_transform,
                                     loan_data)
#AFTER
summary(loan_data_scale_transform)

###Center Data Transform----
#BEFORE
summary(loan_data)

model_of_the_transform <- preProcess(loan_data, method = c("center"))
print(model_of_the_transform)
loan_data_center_transform <- predict(model_of_the_transform,
                                     loan_data)
#AFTER
summary(loan_data_center_transform)

###Standardize Data Transform----
#BEFORE
summary(loan_data)

sapply(loan_data[, -1, -2, -6, -9, -10, -12, -13, -14, -23, -35 ], sd)
model_of_the_transform <- preProcess(loan_data, method = c("scale", "center"))
print(model_of_the_transform)
loan_data_standardize_transform <- predict(model_of_the_transform,
                                     loan_data)

#AFTER
summary(loan_data_standardize_transform)
sapply(loan_data_standardize_transform[, -1, -2, -6, -9, -10, -12, -13, -14, -23, -35 ], sd)


#STEP 6: Training the Model ----
##ALGORITHM SELECTION ----
##LDA with k-fold Cross Validation----
###1.Splitting the dataset ----
# Define a 75:25 train:test data split of the dataset.
# That is, 75% of the original data will be used to train the model and
# 25% of the original data will be used to test the model.
train_index <- createDataPartition(loan_data_center_transform$loan_status,
                                   p = 0.75,
                                   list = FALSE)
loan_data_train <- loan_data[train_index, ]
loan_data_test <- loan_data[-train_index, ]

### 2.Classification: LDA with k-fold Cross Validation ----

set.seed(7)

train_control <- trainControl(method = "cv", number = 5)
loan_model_lda <- 
  caret::train(`loan_status` ~ loan_amnt + int_rate + grade + home_ownership + annual_inc + 
                 verification_status + dti + open_acc + revol_bal +
                 total_acc  + total_pymnt + total_rec_int + 
                 tot_cur_bal, data = loan_data_train,
               trControl = train_control, na.action = na.omit, method = "lda2",
               metric = "Accuracy")

### 3.a.Test the trained LDA model using the testing dataset ----
predictions_lda <- predict(loan_model_lda,
                           loan_data_test[, 1:39])

### 3.b.Display the model's details ----
print(loan_model_lda)






##Decision tree for a classification problem with caret ----
###1.Train the model ----
set.seed(7)
# We apply the 5-fold cross validation resampling method
train_control <- trainControl(method = "cv", number = 5)
loan_model_rpart <- train(`loan_status` ~ loan_amnt + int_rate + grade + home_ownership + annual_inc +
                                      verification_status + dti + open_acc + revol_bal +
                                      total_acc + total_pymnt + total_rec_int +
                                      tot_cur_bal, data = loan_data_train,
                                    method = "rpart", metric = "Accuracy",
                                    trControl = train_control)

###2.Display the model's details ----
print(loan_model_rpart)

###3.Make predictions ----
predictions <- predict(loan_model_rpart,
                       loan_data_test[, 1:39])


##MODEL PERFORMANCE COMPARISON ----
##Call the `resamples` Function ----
# We then create a list of the model results and pass the list as an argument
# to the `resamples` function.

results <- resamples(list(LDA = loan_model_lda, CART = loan_model_rpart))

##Display the Results ----
### 1. Table Summary ----
# This is the simplest comparison. It creates a table with one model per row
# and its corresponding evaluation metrics displayed per column.

summary(results)

### 2. Box and Whisker Plot ----
# This is useful for visually observing the spread of the estimated accuracies
# for different algorithms and how they relate.

scales <- list(x = list(relation = "free"), y = list(relation = "free"))
bwplot(results, scales = scales)

### 3. Dot Plots ----
# They show both the mean estimated accuracy as well as the 95% confidence
# interval (e.g. the range in which 95% of observed scores fell).

scales <- list(x = list(relation = "free"), y = list(relation = "free"))
dotplot(results, scales = scales)

### 4. Scatter Plot Matrix ----
# This is useful when considering whether the predictions from two
# different algorithms are correlated. If weakly correlated, then they are good
# candidates for being combined in an ensemble prediction.

splom(results)

### 5. Pairwise xyPlots ----
# You can zoom in on one pairwise comparison of the accuracy of trial-folds for
# two models using an xyplot.

# xyplot plots to compare models
xyplot(results, models = c("LDA", "CART"))

# or
# xyplot plots to compare models
xyplot(results, models = c("LDA", "CART"))

### 6. Statistical Significance Tests ----
# This is used to calculate the significance of the differences between the
# metric distributions of the various models.

#### Upper Diagonal ----
# The upper diagonal of the table shows the estimated difference between the
# distributions. If we think that LDA is the most accurate model from looking
# at the previous graphs, we can get an estimate of how much better it is than
# specific other models in terms of absolute accuracy.

#### Lower Diagonal ----
# The lower diagonal contains p-values of the null hypothesis.
# The null hypothesis is a claim that "the models are the same".
# A lower p-value is better (more significant).

diffs <- diff(results)

summary(diffs)



# STEP 7: Saving the Model----
saveRDS(loan_model_rpart, "./models/saved_loan_model_rpart.rds")

saveRDS(loan_model_lda, "./models/saved_loan_model_lda.rds")
###Load the Model ----
loaded_loan_model_rpart <- readRDS("./models/saved_loan_model_rpart.rds")
print(loaded_loan_model_rpart)

loaded_loan_model_lda <- readRDS("./models/saved_loan_model_lda.rds")
print(loaded_loan_model_lda)
###Make Predictions on New Data using the Saved Model ----
to_be_predicted <-
  data.frame(loan_amnt = 900000,int_rate = 13.69, grade = 'A' ,
             home_ownership = 'OWN' , annual_inc = 50000, total_pymnt = 9000.0 , verification_status ="Verified" , 
             dti = 8.24 ,open_acc = 13.0 , revol_bal =2932.0 , total_acc = 9.0 , total_rec_int = 525.11,
             tot_cur_bal = 2932.0 )


predict(loaded_loan_model_rpart, newdata = to_be_predicted)

predict(loaded_loan_model_lda, newdata = to_be_predicted)


#STEP 8: API ----

api <- plumber::plumb("API.R")

##Specify a constant localhost port to use ----
api$run(host = "127.0.0.1", port = 5022)

