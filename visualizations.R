

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


disconnections_non_payment %>%
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
  labs(x = "Date", y = "Disconnections due to non-payment", fill = "Income")

alt_shutoff_enrollments %>%
  filter(date != "2023-12-01",
         date != "2020-08-01") %>%
  mutate(date = paste(date, "01", sep = "-")) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  group_by(date, income) %>%
  summarize(num_enrollments = sum(num_enrollments, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = num_enrollments, fill = income)) +
  geom_area(stat = "identity", position = "stack") +
  theme_bw() +
  labs(x = "Date", y = "Number of Alternative Shutoff Protection Plan Enrollments", fill = "Income")
  



