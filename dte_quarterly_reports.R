
rm(list=ls())

library(tidyverse)

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
datadir <- "outputs/DTE_quarter_reports_disconnections"
outdir <- "results"

reports <- list.files(datadir, full.names = TRUE)

q_disconnections <- data.frame()

for (i in 1:length(reports)) {
  
  print(reports[i])
  
  curr_report <- read.csv(reports[i])
  
  q_disconnections <- q_disconnections %>%
    bind_rows(curr_report)
}

print("DONE reading in reports")

q_disconnections <- q_disconnections %>%
  mutate(date = as.Date(date, "%d-%m-%Y"))

# Total number of disconnections per year
yearly_disconnections <- q_disconnections %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>%
  summarize(disconnections = sum(disconnections, na.rm = TRUE)) %>%
  ungroup()

month_disconnections <- q_disconnections %>%
  mutate(year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m"))) %>%
  group_by(year, month) %>%
  summarize(disconnections = sum(disconnections, na.rm = TRUE)) %>%
  ungroup()

quarter_disconnections <- q_disconnections %>%
  mutate(year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m"))) %>%
  group_by(year, month) %>%
  summarize(disconnections = sum(disconnections, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(quarter = case_when(
    month %in% c(1:3) ~ "Q1",
    month %in% c(4:6) ~ "Q2",
    month %in% c(7:9) ~ "Q3",
    month %in% c(10:12) ~ "Q4",
    TRUE ~ "error"
  )) %>%
  group_by(year, quarter) %>%
  summarize(disconnections = sum(disconnections, na.rm = TRUE)) %>%
  ungroup()

utility_colors <- c(
  "#145DA0",
  "#53a548"
)

covid_moratoria_area <- data.frame(
  start = as.Date("01-04-2020", "%d-%m-%Y"),
  end = as.Date("01-06-2020", "%d-%m-%Y"),
  disconnections = 22977,
  utility = 
)


ggplot() +
  geom_area(data = q_disconnections, aes(x = date, y = disconnections / 1000, fill = utility),
            stat = "identity", position = "stack") +
  scale_fill_manual(values = utility_colors) +
  geom_rect(
    aes(
      xmin = as.Date("01-03-2020", "%d-%m-%Y"),
      xmax = as.Date("01-07-2020", "%d-%m-%Y"),
      ymin = 0,
      ymax = max(month_disconnections$disconnections)/1e3
    ),
    fill = "orange",
    alpha = 0.5
  ) +
  scale_y_continuous(limits = c(0, max(month_disconnections$disconnections)/1e3),
                     expand = c(0,0.1)) +
  theme_bw() +
  theme(
    legend.position = "top"
  ) +
  labs(x = "", y = "DTE disconnections\ndue to non-payment (thousand)", fill = "Utility",
       caption = "*Based on DTE Quarterly Report to MPSC Case U-18120, Rule 460.151") +
  annotate(geom="text", x = as.Date("01-05-2020", "%d-%m-%Y"), y = 12, label = "COVID-19 Moratoria", angle = 90)
  

ggsave(file.path(outdir, "DTE_quarterly_disconnections.png"), dpi = 500, height = 4, width = 6, units = "in")

q_disconnections %>%
  ggplot(aes(x = date, y = disconnections / 1000, fill = utility, color = utility)) +
  # geom_area(stat = "identity", position = "stack") +
  # scale_fill_manual(values = utility_colors) +
  geom_smooth() +
  scale_color_manual(values = utility_colors) +
  scale_fill_manual(values = utility_colors) +
  theme_bw() +
  theme(
    legend.position = "top"
  ) +
  labs(x = "", y = "DTE disconnections\ndue to non-payment (thousand)", fill = "Utility", color = "Utility",
       caption = "*Based on DTE Quarterly Report to MPSC Case U-18120, Rule 460.151")

ggsave(file.path(outdir, "DTE_quarterly_disconnections_trends.png"), dpi = 500, height = 4, width = 6, units = "in")


  

