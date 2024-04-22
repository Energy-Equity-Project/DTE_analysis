
rm(list=ls())

library(tidyverse)

datadir <- "outputs/DTE_quarter_reports"
outdir <- "outputs/clean_DTE_quarter_data"

disconnection_files <- list.files(datadir, pattern = "disconnections", full.names = TRUE)
alt_shutoff_plan_files <- list.files(datadir, pattern = "alternative_shutoff_protection_plan", full.names = TRUE)
restorations_files <- list.files(datadir, pattern = "restorations", full.names = TRUE)
wpp_files <- list.files(datadir, pattern = "winter_protection_plan", full.names = TRUE)

disconnections <- data.frame()
restorations <- data.frame()
alt_shutoffs <- data.frame()
wpp <- data.frame()

num_files <- length(disconnection_files)

for (i in 1:num_files) {
  
  print(paste(i, "/", num_files))
  
  curr_disconnections <- read.csv(disconnection_files[i])
  curr_restorations <- read.csv(restorations_files[i])
  curr_alt_shutoffs <- read.csv(alt_shutoff_plan_files[i])
  curr_wpp <- read.csv(wpp_files[i])
  
  disconnections <- disconnections %>%
    bind_rows(curr_disconnections)
  
  restorations <- restorations %>%
    bind_rows(curr_restorations)
  
  alt_shutoffs <- alt_shutoffs %>%
    bind_rows(curr_alt_shutoffs)
  
  wpp <- wpp %>%
    bind_rows(curr_wpp)
}

print("DONE reading in reports")

disconnections <- disconnections %>%
  mutate(date = as.Date(date, "%d-%m-%Y"))

write.csv(disconnections, file.path(outdir, "disconnections.csv"), row.names = FALSE)

restorations <- restorations %>%
  mutate(date = as.Date(date, "%d-%m-%Y"))

write.csv(restorations, file.path(outdir, "restorations.csv"), row.names = FALSE)

alt_shutoffs <- alt_shutoffs %>%
  mutate(date = as.Date(date, "%d-%m-%Y"))

write.csv(alt_shutoffs, file.path(outdir, "alt_shutoff_protection_plan_enrollments.csv"), row.names = FALSE)

wpp <- wpp %>%
  mutate(date = as.Date(date, "%d-%m-%Y"))

write.csv(wpp, file.path(outdir, "winter_protection_plan_enrollments.csv"), row.names = FALSE)

