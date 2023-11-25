
if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# STEP 1. Install and load the required packages ----
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
  
#* @apiTitle Loan Classification Model API

#* @apiDescription Used to predict the status of a loan.

#* @param arg_loan_amnt The amount of the loan
#* @param arg_int_rate The interest rate of the loan
#* @param arg_grade The Loan Grade
#* @param arg_home_ownership Home Ownership
#* @param arg_annual_inc Annual Income
#* @param arg_total_pymnt Total Payment
#* @param arg_verification_status Loan Verification Status
#* @param arg_dti DTI
#* @param arg_open_acc Open Account
#* @param arg_revol_bal Revol balance
#* @param arg_total_acc Total Account
#* @param arg_total_rec_int Total Rec Interest
#* @param arg_tot_cur_bal Total Cur Balance


#* @get /loan_status

predict_diabetes <-
  function(arg_loan_amnt, arg_int_rate, arg_grade, arg_home_ownership, arg_annual_inc,
           arg_total_pymnt,arg_verification_status, arg_dti, arg_open_acc, arg_revol_bal,
           arg_total_acc,arg_total_rec_int, arg_tot_cur_bal) {
    # Create a data frame using the arguments
    to_be_predicted <-
      data.frame(loan_amnt = as.numeric(arg_loan_amnt),
                 int_rate = as.numeric(arg_int_rate),
                 grade = (arg_grade),
                 home_ownership = (arg_home_ownership),
                 annual_inc = as.numeric(arg_annual_inc),
                 total_pymnt = as.numeric(arg_total_pymnt),
                 verification_status =(arg_verification_status),
                 dti = as.numeric(arg_dti),
                 open_acc = as.numeric(arg_open_acc),
                 revol_bal = as.numeric(arg_revol_bal),
                 total_acc = as.numeric(arg_total_acc),
                 total_rec_int = as.numeric(arg_total_rec_int),
                 tot_cur_bal = as.numeric(arg_tot_cur_bal))
    # Make a prediction based on the data frame
    predict(loaded_loan_model_lda, to_be_predicted)
  }




