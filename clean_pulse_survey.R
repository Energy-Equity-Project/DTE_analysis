library(tidyverse)

# Directory where all raw Public Use Files (PUF) are available
# data taken from: https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html
datadir <- "../Data/Pulse_Survey"

# Get all zipped files
zipped_files <- sort(list.files(datadir, pattern = "\\.zip"))

# Unzip all files have havent been zipped yet
for(i in 1:length(zipped_files)) {
  # Get current zipped file
  curr_zip <- zipped_files[i]
  print(paste(curr_zip, i, "/", length(zipped_files)))
  
  # Get zipped output directory
  outdir <- substr(curr_zip, 1, str_length(curr_zip) - 4)
  outdir <- file.path(datadir, outdir)
  
  # Only unzip if this file hasn't been unzipped yet
  if (!dir.exists(outdir)) {
    dir.create(outdir)
    curr_zip_fp <- file.path(datadir, curr_zip)
    unzip(curr_zip_fp, exdir = outdir)
  }
}

print("DONE")

# All unzipped Pulse Survey data
unzipped_dirs <- list.dirs(path = datadir, recursive = FALSE)

# Collect all relevant Pulse Survey data
mi_energy_resps <- data.frame()
for (i in 1:length(unzipped_dirs)) {
  # Current zipped data folder
  curr_dir <- unzipped_dirs[i]
  print(curr_dir)
  # current PUF file
  survey_filename <- list.files(path = curr_dir, pattern = "pulse[0-9]{4}_puf_[0-9]{2}\\.csv")[1]
  curr_survey <- read.csv(file.path(curr_dir, survey_filename))
  
  # Only get data if energy questions were included in the survey
  if ("ENERGY" %in% colnames(curr_survey)) {
    curr_survey <- curr_survey %>%
      # Only include data for Michigan
      filter(EST_ST == 26) %>%
      # include the household weight, population weights, energy responses, heating responses
      select(SCRAM, WEEK, HWEIGHT, PWEIGHT, ENERGY, HSE_TEMP, ENRGY_BILL)
    
    # Accumulating data across all files
    mi_energy_resps <- mi_energy_resps %>%
      bind_rows(curr_survey)
  }
}

# Write out processed Pulse Survey
outdir <- "outputs"
write.csv(mi_energy_resps, file.path(outdir, "mi_energy_resps.csv"), row.names = FALSE)
