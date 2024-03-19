
library(tidyverse)

doe_dir <- "outputs/DOE_lEAD/CSV"

doe <- read.csv(file.path(doe_dir, "mi_ami_0-30.csv")) %>%
  mutate(ami = "0-30%") %>%
  bind_rows(
    read.csv(file.path(doe_dir, "mi_ami_30-60.csv")) %>%
      mutate(ami = "30-60%")
  ) %>%
  bind_rows(
    read.csv(file.path(doe_dir, "mi_ami_60-80.csv")) %>%
      mutate(ami = "60-80%")
  ) %>%
  bind_rows(
    read.csv(file.path(doe_dir, "mi_ami_80-100.csv")) %>%
      mutate(ami = "80-100%")
  ) %>%
  bind_rows(
    read.csv(file.path(doe_dir, "mi_ami_100+.csv")) %>%
      mutate(ami = "100%+")
  ) %>%
  mutate(energy_burden_group = case_when(
    burden >= 0 & burden <= 3 ~ "0-3%",
    burden > 3 & burden <= 6 ~ "3-6%",
    burden > 6 & burden <= 9 ~ "6-9%",
    burden > 9 & burden <= 12 ~ "9-12%",
    burden > 12 & burden <= 15 ~ "12-15%",
    burden > 15 ~ "15+%",
    TRUE ~ "error"
  )) %>%
  mutate(energy_burden_class = case_when(
    energy_burden_group == "0-3%" ~ "Cheap",
    energy_burden_group == "3-6%" ~ "Affordable",
    energy_burden_group == "6-9%" ~ "Moderately Unaffordable",
    energy_burden_group == "9-12%" ~ "High",
    energy_burden_group == "12-15%" ~ "Severe",
    energy_burden_group == "15+%" ~ "Extreme",
    TRUE ~ "error"
  )) %>%
  mutate(ami = factor(ami,
                      levels = c("0-30%", "30-60%", "60-80%", "80-100%", "100%+"))) %>%
  mutate(
    energy_burden_group = factor(
      energy_burden_group,
      levels=c("0-3%", "3-6%", "6-9%", "9-12%", "12-15%", "15+%", "error")
    ),
    energy_burden_class = factor(
      energy_burden_class,
      levels=c("Cheap", "Affordable", "Moderately Unaffordable", "High", "Severe", "Extreme", "error")
    )
  )

# Only take into account MI census tracts within the DTE service territory
doe <- doe %>%
  filter(gi %in% unique(dte_tracts$gi))


energy_burdens <- doe %>%
  group_by(ami, energy_burden_group, energy_burden_class) %>%
  summarize(num_hh = sum(h_count, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(ami) %>%
  mutate(percent = 100 * (num_hh / sum(num_hh))) %>%
  ungroup()

eep_colors <- c(
  "#00274c",
           "#145DA0",
           "#2E8BC0",
           "#19381f",
           "#53a548",
           "#ffcb05"
)

energy_burden_colors <- c(
  "#53a548",
  "#06283D",
  "#1363DF",
  "#B4D4FF",
  "#FCB97D",
  "#ffcb05"
)

energy_burdens %>%
  ggplot(aes(x = ami, y = num_hh/1e5, fill = energy_burden_group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values=energy_burden_colors) +
  theme_bw() +
  theme(
    legend.position = c(0.12,0.77)
  ) +
  labs(x = "Area Median Income", y = "Number of Households (hundred thousands)", fill = "Energy Burdens")

ggsave(file.path(outdir, "energy_burdens_by_ami.png"), height = 5, width = 6)

# graph of how much they are paying (fill by amount that would be affordable vs amount that is unaffordable)
affordable_energy_burden <- 0.06
energy_costs <- doe %>%
  mutate(affordable_energy_costs = income * affordable_energy_burden) %>%
  mutate(total_affordable = h_count * affordable_energy_costs,
         total_income = h_count * income,
         total_energy_costs = h_count * yr_cost) %>%
  group_by(ami) %>%
  summarize(
    h_count = sum(h_count),
    income = sum(total_income),
    energy_costs = sum(total_energy_costs),
    affordable = sum(total_affordable)
  ) %>%
  ungroup() %>%
  mutate(difference = energy_costs - affordable)

energy_costs %>%
  pivot_longer(c(energy_costs, affordable), names_to = "cost_breakdown")



# Amount required to reduce everyone to below 6%
  # rate increase for those in cheap energy burden by how much as a rate discount for low income?

avg_hh_over_extended <- doe %>%
  mutate(affordable_energy_costs = income * affordable_energy_burden) %>%
  mutate(over_payment = yr_cost - affordable_energy_costs) %>%
  group_by(ami, energy_burden_group, energy_burden_class) %>%
  summarize(over_payment = weighted.mean(over_payment, h_count)) %>%
  ungroup()

avg_hh_over_extended %>%
  ggplot(aes(x = ami, y = over_payment, fill = energy_burden_group)) +
  geom_col(position = position_dodge2(width = 1, preserve = "single")) +
  scale_fill_manual(values = energy_burden_colors) +
  theme_bw() +
  theme(
    legend.position = c(0.15, 0.3)
  ) +
  labs(x = "Area Median Income",
       y = "Average HH Difference between actual energy costs\nand affordable energy costs (USD)",
       fill = "Energy Burden") +
  annotate("text", x = 5.5, y = 800, label = "Over Extended", angle = 270) +
  annotate("text", x = 5.5, y = -1800, label = "Can Afford to Pay More", angle = 270) +
  annotate("segment", x = 5.25, y = 100, xend = 5.25, yend = 2000, size = 2, linejoin = "mitre", arrow = arrow(type = "closed", length = unit(2, "mm")), color = "#9FB7B9") +
  annotate("segment", x = 5.25, y = -100, xend = 5.25, yend = -5000, size = 2, linejoin = "mitre", arrow = arrow(type = "closed", length = unit(2, "mm")), color = "#9FB7B9")

ggsave(file.path(outdir, "energy_costs_over_payments.png"), width = 6, height = 5, device = "png")

cumulative_over_extended <- doe %>%
  mutate(affordable_energy_costs = income * affordable_energy_burden) %>%
  mutate(over_payment = yr_cost - affordable_energy_costs) %>%
  mutate(total_over_payment = over_payment * h_count) %>%
  group_by(ami, energy_burden_group, energy_burden_class) %>%
  summarize(over_payment = sum(total_over_payment, na.rm = TRUE)) %>%
  ungroup()


cumulative_over_extended %>%
  ggplot(aes(x = ami, y = over_payment/1e9, fill = energy_burden_group)) +
  geom_col(position = position_dodge2(width = 1, preserve = "single")) +
  scale_fill_manual(values = energy_burden_colors) +
  theme_bw() +
  theme(
    legend.position = c(0.15, 0.3)
  ) +
  labs(x = "Area Median Income",
       y = "Cumulative Difference between actual energy costs\nand affordable energy costs (billion USD)",
       fill = "Energy Burden") +
  annotate("text", x = 5.47, y = 0.6, label = "Over\nExtended", angle = 270, size = 3) +
  annotate("text", x = 5.47, y = -3, label = "Can Afford to Pay More", angle = 270, size = 3) +
  annotate("segment", x = 5.25, y = 0.1, xend = 5.25, yend = 1, size = 2, linejoin = "mitre", arrow = arrow(type = "closed", length = unit(2, "mm")), color = "#9FB7B9") +
  annotate("segment", x = 5.25, y = -0.5, xend = 5.25, yend = -8, size = 2, linejoin = "mitre", arrow = arrow(type = "closed", length = unit(2, "mm")), color = "#9FB7B9")

ggsave(file.path(outdir, "cumulative_HEAG.png"), width = 6, height = 5, device = "png")


# Calculate total over extended amount for all households
total_unaffordable_amount <- cumulative_over_extended %>%
  filter(over_payment > 0) %>%
  summarize(total_over_payment = sum(over_payment, na.rm = TRUE)) %>%
  ungroup() %>%
  pull(total_over_payment)

# Total amount high income households could afford to pay more
total_high_income_buffer <- cumulative_over_extended %>%
  filter(over_payment < 0 &
         ami == "100%+" &
           energy_burden_group == "0-3%") %>%
  summarize(total_buffer = sum(over_payment, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total_buffer = total_buffer * -1) %>%
  pull(total_buffer)

# Compare total over extended bills amount to percentage of 0-3% 100%+ AMI group could afford to pay more
total_unaffordable_amount / total_high_income_buffer

# Overlay US Census AMI across DOE energy burdens to get more resolved AMI groups (100-120, 120-140, etc)

# Disgregate for Gas burden for Gas territory

# Disagregate for Electric burden for Electric territory

# DTE service areas instead of whole of MI

# Compare Detroit vs the rest of MI

