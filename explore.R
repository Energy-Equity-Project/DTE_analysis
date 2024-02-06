

library(readxl)
library(tidyverse)

dte_data <- read_excel("../Data/DTE Shutoff Reports - Aug '20 - Nov '23 clean.xlsx")

dates <- dte_data[3, 3:43] %>%
  pivot_longer(everything(), names_to = "test", values_to = "date") %>%
  select(-test) %>%
  mutate(date = gsub("_", "/", date)) %>%
  mutate(date = paste(date, "01", sep = "/")) %>%
  mutate(date = as.Date(date, "%Y/%m/%d"))

# Customer Payment Performance--------------------------------------------------
customer_payment_performance <- dte_data[4:30, ]
customer_payment_performance <- customer_payment_performance[, 2:43]
colnames(customer_payment_performance) <- c("variable", as.character(dates$date))
customer_payment_performance <- customer_payment_performance %>%
  pivot_longer(as.character(dates$date), names_to = "date", values_to = "value") %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  mutate(value = as.numeric(value)) %>%
  filter(str_detect(variable, "Dollar amount for customers delinquent") |
           str_detect(variable, "Number of customers delinquent"))

# Customer Payment Delinquency by USD
customer_payment_performance %>%
  filter(str_detect(variable, "Dollar amount for customers delinquent")) %>%
  mutate(variable = str_remove(variable, "Dollar amount for customers delinquent ")) %>%
  mutate(variable = str_remove(variable, " overall")) %>%
  mutate(variable = factor(variable,
                           levels = c(
                             "6 - 30 days",
                             "31 - 60 days",
                             "61 - 90 days",
                             "91 days or more"
                           ))) %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_line(stat="identity") +
  theme_bw() +
  theme(
    legend.position = "top"
  ) +
  labs(x = "Date", y = "Customer Payment Delinquency in USD", color = "Number of days delinquent")

# Customer Payment Delinquency by Number of Customers
customer_payment_performance %>%
  filter(str_detect(variable, "Number of customers delinquent")) %>%
  mutate(variable = str_remove(variable, "Number of customers delinquent ")) %>%
  mutate(variable = str_remove(variable, " overall")) %>%
  mutate(variable = factor(variable,
                           levels = c(
                             "6 - 30 days",
                             "31 - 60 days",
                             "61 - 90 days",
                             "91 days or more"
                           ))) %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_line(stat="identity") +
  theme_bw() +
  theme(
    legend.position = "top"
  ) +
  labs(x = "Date", y = "Number of Customers Delinquent", color = "Number of days delinquent")

# Shutoff Information-----------------------------------------------------------
shutoffs <- dte_data[47:109, ]
shutoffs <- shutoffs[, 2:43]
colnames(shutoffs) <- c("variable", as.character(dates$date))
shutoffs <- shutoffs %>%
  pivot_longer(as.character(dates$date), names_to = "date", values_to = "value") %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  mutate(value = as.numeric(value)) %>%
  filter(str_detect(variable, "Total discontinuation notices issued at end of month") |
           str_detect(variable, "Total of customers physically discontinued due to non-payment"))

shutoffs %>%
  filter(str_detect(variable, "Total discontinuation notices issued at end of month")) %>%
  ggplot(aes(x = date, y = value)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Date", y = "Total Discontinuation notices issues at the end of month")

shutoffs %>%
  filter(str_detect(variable, "Total of customers physically discontinued due to non-payment")) %>%
  ggplot(aes(x = date, y = value)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Date", y = "Total Disconnections due to non-payment")

# Delinquency USD by Total Discontinuation notices

delinq_usd_disc_non_pmt <- customer_payment_performance %>%
  filter(str_detect(variable, "Dollar amount for customers delinquent ")) %>%
  group_by(date) %>%
  summarize(delinquency_usd = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(delinquency_usd_millions = delinquency_usd / 1e6) %>%
  full_join(
    shutoffs %>%
      filter(str_detect(variable, "Total of customers physically discontinued due to non-payment")) %>%
      group_by(date) %>%
      summarize(disconnections_non_pmt = sum(value, na.rm = TRUE)) %>%
      ungroup(),
    by = c("date")
  )

delinq_usd_disc_non_pmt %>%
  mutate(ratio = delinquency_usd / disconnections_non_pmt) %>%
  ggplot(aes(x = date, y = ratio)) +
  geom_line() +
  theme_bw()

delinq_usd_disc_non_pmt %>%
  select(-c(delinquency_usd)) %>%
  filter(disconnections_non_pmt > 0) %>%
  pivot_longer(c(delinquency_usd_millions, disconnections_non_pmt), names_to = "variable", values_to = "value") %>%
  mutate(variable = case_when(
    variable == "delinquency_usd_millions" ~ "Dollar amount (millions) of customers delinquent",
    variable == "disconnections_non_pmt" ~ "Disconnections due to non-payment",
    TRUE ~ "error"
  )) %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_line(size = 2) +
  scale_color_manual(values = c("#c61911", "#78C091")) +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  facet_grid(variable ~ ., scales = "free") +
  labs(x = "Date", y = "")

# Delinquency USD by alternative shutoff protection plan enrollments
alt_shutoff_plan <- dte_data[41:45, ]
alt_shutoff_plan <- alt_shutoff_plan[, 2:43]
colnames(alt_shutoff_plan) <- c("variable", as.character(dates$date))
alt_shutoff_plan <- alt_shutoff_plan %>%
  pivot_longer(as.character(dates$date), names_to = "date", values_to = "value") %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  mutate(value = as.numeric(value)) %>%
  filter(str_detect(variable, "Total enrolled in program at end of month overall")) %>%
  rename(total_enrolled = value) %>%
  select(-c(variable)) %>%
  replace_na(list(total_enrolled = 0))


alt_shutoff_plan_disc_non_pmt <- alt_shutoff_plan %>%
  group_by(date) %>%
  summarize(total_enrolled = sum(total_enrolled, na.rm = TRUE)) %>%
  ungroup() %>%
  full_join(
    shutoffs %>%
      filter(str_detect(variable, "Total of customers physically discontinued due to non-payment")) %>%
      group_by(date) %>%
      summarize(disconnections_non_pmt = sum(value, na.rm = TRUE)) %>%
      ungroup(),
    by = c("date")
  ) %>%
  pivot_longer()

alt_shutoff_plan_disc_non_pmt %>%
  filter(disconnections_non_pmt > 0) %>%
  mutate(total_enrolled_k = total_enrolled / 1000) %>%
  select(-total_enrolled) %>%
  pivot_longer(c(total_enrolled_k, disconnections_non_pmt), names_to = "variable", values_to = "value") %>%
  mutate(variable = case_when(
    variable == "total_enrolled_k" ~ "Alternative Shutoff Protection Plan Enrollments (1000s)",
    variable == "disconnections_non_pmt" ~ "Disconnections due to non-payment"
  )) %>%
  mutate(variable = factor(
    variable,
    levels = c("Disconnections due to non-payment", "Alternative Shutoff Protection Plan Enrollments (1000s)")
  )) %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_line(size = 2) +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  facet_grid(variable ~ ., scales = "free") +
  labs(x = "Date", y = "")




