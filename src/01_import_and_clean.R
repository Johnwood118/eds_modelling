# Required Packages 
library(readr)
library(data.table)

# Load data

loans_dt <- fread("data/raw/loans.csv", na.strings = "")

# Quick summary
sapply(mtcars, function(x) sum(is.na(x)))

# Distributions 

# income
loans_dt[, summary(loan_amount)]


# Change names
setnames(loans_dt, 
         c("year","issue_date", "nr_accounts"), 
         c("issue_year", "issue_month", "num_accounts"))

# Fix dates

loans_dt[, issue_month := paste0("01-", issue_month)]
loans_dt[, issue_month := as.IDate(issue_month, format = "%d-%b-%y")]

loans_dt[, earliest_credit_line := paste0("01-", earliest_credit_line)]
loans_dt[, earliest_credit_line := as.IDate(earliest_credit_line, format = "%d-%b-%y")]

# Convert character vars to numeric 

loans_dt[, term := as.numeric(gsub(" months", "", term))]

loans_dt[, emp_length := gsub(" years", "", employment_length)]
loans_dt[emp_length == "1 year", emp_length := "1"]
loans_dt[emp_length == "< 1 year", emp_length := "0"]
loans_dt[emp_length == "10+", emp_length := "10+"]

loans_dt[, emp_length := factor(emp_length, levels = c("0", "1", "2",
                                                       "3", "4", "5",
                                                       "6", "7", "8", 
                                                       "9", "10+"))]

loans_dt[, employment_length := NULL]

# Reduce purpose

loans_dt[purpose %in% c("home_improvement",
                        "house",
                        "moving"), purpose := "housing"]

# Assume NA employment is not-employed (inc retired)
# Set to unemployed if emp length equals NA

loans_dt[, employed := !is.na(job_title)]
# adjust for some  retired 
loans_dt[job_title %like% "retired", employed := FALSE]

# Create bad/good scenarios

loans_dt[, bad_status := loan_status %in% c("Charged Off",
                            "Late (> 90 days)",
                            "Default")]

# Summarise bads 
loans_dt[, summary(bad_status)]

# Bad rate of whole sample is 10.7%
loans_dt[bad_status == T, .N] / loans_dt[, .N]

# Bads to debt to income
# Bads to credit score bands
# Bads to open accounts

# Odd things to note:

# All last_derog_month are missing before issue_date of '2012-07-01'

