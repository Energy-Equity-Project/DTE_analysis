
library(tidyverse)
library(ggpubr)

eep_colors <- c(
  "#00274c",
  "#145DA0",
  "#2E8BC0",
  "#19381f",
  "#53a548",
  "#ffcb05"
)

eep_colors <- c(
  "#06283D",
  "#1363DF",
  "#B4D4FF",
  "#FCB97D"
)


# DTE---------------------------------------------------------------------------

# Total dollar amount in arrears disaggregated by duration
customer_payment_performance %>%
  filter(date >= "2020-09-01") %>%
  filter(income == "total") %>%
  mutate(duration = factor(duration, levels = c("6 - 30 days", "31 - 60 days", "61 - 90 days", "91 days or more"))) %>%
  ggplot(aes(x = date, y = arrears / 1e6, fill = duration)) +
  geom_area(stat = "identity", position = "stack") +
  scale_fill_manual(values = eep_colors) +
  theme_bw() +
  theme(
    legend.position = "top"
  ) +
  labs(x = "Date", fill = "Number of Days Delinquent", y = "Arrears (Millions)")

# Total dollar amount in arrears disagregated by income
customer_payment_performance %>%
  filter(date > "2020-09-01") %>%
  filter(income != "total") %>%
  group_by(date, income) %>%
  summarize(arrears = sum(arrears, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = arrears / 1e6, color = income)) +
  # geom_area(stat = "identity", position = "stack") +
  # geom_bar(stat= "identity", position = "dodge") +
  geom_point(size = 2) +
  geom_line(size = 1) +
  ylim(0, 135) +
  scale_color_manual(values = c("#1363DF", "#FCB97D")) +
  theme_bw()+
  theme(
    legend.position = "top"
  ) +
  labs(x = "Date", fill = "", y = "Arrears (USD millions)")

# Total dollar amount in arrears disaggregated by duration and income
customer_payment_performance %>%
  filter(income != "total") %>%
  mutate(duration = factor(duration, levels = c("6 - 30 days", "31 - 60 days", "61 - 90 days", "91 days or more"))) %>%
  ggplot(aes(x = date, y = arrears/1e6, fill = duration)) +
  geom_area(stat = "identity", position = "stack") +
  facet_grid(rows="income") +
  scale_fill_manual(values = eep_colors) +
  theme_bw() +
  theme(
    legend.position = "top"
  ) +
  labs(x = "Date", fill = "Number of Days Delinquent", y = "Arrears (Millions)")

# Average arrears per customer per duration per income level
customer_payment_performance %>%
  filter(avg_arrear_per_customer < 5000) %>%
  mutate(duration = factor(duration, levels = c("6 - 30 days", "31 - 60 days", "61 - 90 days", "91 days or more"))) %>%
  filter(income != "total") %>%
  ggplot(aes(x = date, y = avg_arrear_per_customer, color = duration)) +
  geom_line() +
  theme_bw() +
  facet_grid(rows="income") +
  theme(
    legend.position = "top"
  ) +
  labs(color = "Number of Days Delinquent", y = "Avg Arrears per Customer")

# Disconnections due to non-payment disaggregated by income level
disconnection_non_payment_p <- disconnections_non_payment %>%
  filter(date != "2023-12-01",
         date != "2020-08-01") %>%
  mutate(date = paste(date, "01", sep = "-")) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  group_by(date, income) %>%
  summarize(num_hh = sum(num_hh, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = num_hh, fill = income)) +
  geom_area(stat = "identity", position = "stack") +
  theme_bw() +
  theme(
    legend.position = "top"
  ) +
  labs(x = "Date", y = "Disconnections due to non-payment", fill = "Income")


# Disconnections due to non-payment disaggregated by income level and utility type
disconnections_non_payment %>%
  filter(date != "2023-12-01") %>%
  group_by(date, utility, income) %>%
  summarize(num_hh = sum(num_hh, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = paste(date, "01", sep = "-")) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  ggplot(aes(x = date, y = num_hh, color = utility)) +
  geom_line() +
  theme_bw() +
  facet_grid(rows="income") +
  theme(
    legend.position = "top"
  ) +
  labs(x = "Date", y = "Disconnections due to non-payment", color = "Utility")

# Alternative Shutoff Protection Plan enrollments
alt_shutoff_enrollment_p <- alt_shutoff_enrollments %>%
  filter(date != "2023-12-01",
         date != "2020-08-01") %>%
  mutate(date = paste(date, "01", sep = "-")) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  group_by(date, income) %>%
  summarize(num_enrollments = sum(num_enrollments, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(income = case_when(
    income == "low_income" ~ "Low-income",
    income == "non_senior_non_low_income" ~ "Non-Low-income",
    income == "seniors" ~ "Senior Non-Low-income"
  )) %>%
  ggplot(aes(x = date, y = num_enrollments, fill = income)) +
  geom_area(stat = "identity", position = "stack") +
  theme_bw() +
  theme(
    legend.position = "top"
  ) +
  labs(x = "Date", y = "Number of Alternative Shutoff Protection Plan Enrollments", fill = "Income")


# Side by side comparison of number of disconnections and enrollments in shutoff protection plan
ggarrange(
  disconnection_non_payment_p, alt_shutoff_enrollment_p,
  ncol = 1, nrow = 2,
  common.legend = TRUE
)

# Pulse Survey------------------------------------------------------------------

