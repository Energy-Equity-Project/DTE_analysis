
library(readxl)
library(tidyverse)

outdir <- "outputs"

# Data was collected from the Michigan Public Service Commission
# Note: collected as a pdf and then manually translated into an Excel document
dte_data <- read_excel("../Data/DTE Shutoff Reports - Aug '20 - Nov '23 clean.xlsx")

# Getting all dates from DTE data reporting
dates <- dte_data[3, 3:43] %>%
  pivot_longer(everything(), names_to = "test", values_to = "date") %>%
  select(-test) %>%
  # Format dates to turn from characters to datetime objects
  mutate(date = gsub("_", "/", date)) %>%
  mutate(date = paste(date, "01", sep = "/")) %>%
  mutate(date = as.Date(date, "%Y/%m/%d"))

# Customer Payment Performance--------------------------------------------------

# Selecting Customer Payment Performance Data
customer_payment_performance <- dte_data[4:30, ]
# Removing comments
customer_payment_performance <- customer_payment_performance[, 2:43]
# Adding appropriate dates to column names
colnames(customer_payment_performance) <- c("variable", as.character(dates$date))

# Column of delinquency duration periods
delinquency_duration_col = data.frame(
  duration = c(
    rep("6 - 30 days", 3),
    rep("31 - 60 days", 3),
    rep("61 - 90 days", 3),
    rep("91 days or more", 3)
  )
)

# Number of customers delinquent
delinquencies_num_customers<- customer_payment_performance[3:14, ] %>%
  # adding delinquency durations
  cbind(delinquency_duration_col) %>%
  # categorizing value type (total, low income, non low income)
  mutate(income = case_when(
    str_detect(variable, "Number of customers delinquent") ~ "total",
    TRUE ~ substr(variable, 4, str_length(variable))
  )) %>%
  select(-variable) %>%
  # Pivoting all dates columns into one single date column
  pivot_longer(-c(income, duration), names_to = "date", values_to = "num_customers")

# Delinquencies by dollar amount (arrears)
delinquencies_usd <- customer_payment_performance[16:27, ] %>%
  # adding delinquency durations
  cbind(delinquency_duration_col) %>%
  # categorizing value type (total, low income, non low income)
  mutate(income = case_when(
    str_detect(variable, "Dollar amount for customers delinquent") ~ "total",
    TRUE ~ substr(variable, 22, str_length(variable))
  )) %>%
  select(-variable) %>%
  # Pivoting all dates columns into one single date column
  pivot_longer(-c(income, duration), names_to = "date", values_to = "arrears")


# Format to have disaggregated totals (ie non-low-income and seniors/low income as separate columns)
customer_payment_performance <- delinquencies_num_customers %>%
  # joining number of delinquent customers with their arrears usd values
  full_join(
    delinquencies_usd,
    by = c("duration", "income", "date")
  ) %>%
  # Note: there is no data for 2023-12-01 so we are removing these rows
  filter(date != "2023-12-01") %>%
  # formating columns to correct types
  mutate(num_customers = as.numeric(num_customers)) %>%
  mutate(arrears = as.numeric(arrears)) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  # calculating average arrears per customer
  mutate(avg_arrear_per_customer = arrears / num_customers)

# Writing out DTE customer payment performance
write.csv(customer_payment_performance, file.path(outdir, "dte_customer_payment_performance.csv"), row.names = FALSE)

# Shutoffs----------------------------------------------------------------------
shutoffs <- dte_data[47:109, ]
shutoffs <- shutoffs[, 2:43]
colnames(shutoffs) <- c("variable", as.character(dates$date))

disconnections_non_payment <- shutoffs[12:41, ]
# Adding appropriate dates to column names
colnames(disconnections_non_payment) <- c("variable", as.character(dates$date))

shutoff_types <- data.frame(
  utility = c(
    rep("Electric", 10),
    rep("Natural Gas", 10),
    rep("Combination", 10)
  )
)

income_types <- c(
  "total",
  rep("Low-income", 3),
  rep("Non-Low-income", 3),
  rep("Senior Non-Low-income", 3)
)

income_col <- data.frame(
  income = rep(income_types, 3)
)

housing_types <- c(
  "total",
  "Confirmed Occupied",
  "Unconfirmed Occupied"
)

housing_col <- data.frame(
  housing = rep(housing_types, 9)
)

disconnections_non_payment <- disconnections_non_payment %>%
  cbind(shutoff_types) %>%
  cbind(income_col) %>%
  # filter out totals to remove duplicates
  filter(income != "total") %>%
  cbind(housing_col) %>%
  # filter out totals to remove duplicates
  filter(housing != "total") %>%
  # turn all date columns into one single date column
  pivot_longer(-c(variable, utility, income, housing), names_to = "date", values_to = "num_hh") %>%
  # correct column types
  mutate(num_hh = as.numeric(num_hh))

write.csv(disconnections_non_payment, file.path(outdir, "disconnections_non_payment.csv"), row.names = FALSE)


