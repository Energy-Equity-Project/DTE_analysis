
rm(list=ls())

library(tidyverse)
library(pdftools)

datadir <- "../Data/DTE_Quarterly_Reporting_MPSC"
outdir <- "outputs/DTE_quarter_reports_disconnections"
quarter_reports <- sort(list.files(file.path(datadir)))

pdf_relevant_data <- function(fp) {
  # Read in pdf file as text
  data <- pdf_text(fp)
  # remove all commas (ie 1,500) and separate by new line character
  lines <-  gsub(",", "", data) %>%
    read_lines()
  
  # Keep relevant lines
  relevant_lines <- c()
  for (i in 1:length(lines)) {
    # Only keep table section headers and lines with data in them
    section_headers_regexpr <- "Customer Service|Customer Payment Performance|Winter Protection Plan \\(WPP\\)|Alternative Shutoff Protection Plan|Informal Hearings|Shutoff Information|Restoration Information"
    data_regexpr <- "(\\s+\\d+\\s+|\\s+\\d+\\s+\\d+\\s+|\\s+\\d+\\s+\\d+\\s+\\d+\\s+|\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+)"
    keep_line_pattern = paste(section_headers_regexpr, "|", data_regexpr, sep = "")
    if (str_detect(lines[i], pattern = keep_line_pattern)) {
      
      # if line is a section header then remove all leading white space
      if (str_detect(lines[i], section_headers_regexpr)) {
        section_header <- str_extract(lines[i], section_headers_regexpr)
        relevant_lines <- c(relevant_lines, section_header)
      } else {
        relevant_lines <- c(relevant_lines, lines[i])
      }
      
    }
  }
  
  return(relevant_lines)
}

get_shutoff_lines <- function(fp) {
  # Read in pdf file as text
  data <- pdf_text(fp)
  # remove all commas (ie 1,500) and separate by new line character
  lines <-  gsub(",", "", data) %>%
    read_lines()
  
  # Keep relevant lines
  shutoffs_line_regexpr <- "Total of customers physically discontinued due to non-payment"
  for (i in 1:length(lines)) {
    if (str_detect(lines[i], shutoffs_line_regexpr)) {
      return(lines[(i+1):(i+2)])
    }
  }
  return(NULL)
}

get_shutoff_data <- function(lines, year, quarter) {
  
  if (quarter == 1) {
    headers <- c(
      "utility",
      paste("01-01-", year, sep = ""),
      paste("01-02-", year, sep = ""),
      paste("01-03-", year, sep = "")
    )
  } else if (quarter == 2) {
    headers <- c(
      "utility",
      paste("01-04-", year, sep = ""),
      paste("01-05-", year, sep = ""),
      paste("01-06-", year, sep = "")
    )
  } else if (quarter == 3) {
    headers <- c(
      "utility",
      paste("01-07-", year, sep = ""),
      paste("01-08-", year, sep = ""),
      paste("01-09-", year, sep = "")
    )
  } else if (quarter == 4) {
    headers <- c(
      "utility",
      paste("01-10-", year, sep = ""),
      paste("01-11-", year, sep = ""),
      paste("01-12-", year, sep = "")
    )
  } else {
    return(data.frame())
  }
  
  electric_shutoffs <- unlist(str_extract_all(lines[1], "\\d+"))
  gas_shutoffs <- unlist(str_extract_all(lines[2], "\\d+"))
  
  shutoffs_df <- data.frame(
    utility = c("Electric", "Natural Gas"),
    a = c(electric_shutoffs[1], gas_shutoffs[1]),
    b = c(electric_shutoffs[2], gas_shutoffs[2]),
    c = c(electric_shutoffs[3], gas_shutoffs[3])
  )
  
  colnames(shutoffs_df) <- headers
  shutoffs_df <- shutoffs_df %>%
    pivot_longer(-c(utility), names_to = "date", values_to = "disconnections")
  
  
  return(shutoffs_df)
}


for (i in 1:length(quarter_reports)) {
  print(paste("processing", quarter_reports[i]))
  curr_year <- substr(quarter_reports[i], 1, 4)
  curr_quarter <- substr(quarter_reports[i], 6, 6)
  # relevant_lines <- pdf_relevant_data(file.path(datadir, quarter_reports[i]))
  shutoff_lines <- get_shutoff_lines(file.path(datadir, quarter_reports[i]))
  
  # shutoffs_line_regexpr <- "Total of customers physically discontinued due to non-payment"
  # shutoffs_line_idx <- 0
  # for (j in 1:length(relevant_lines)) {
  #   if (str_detect(relevant_lines[j], shutoffs_line_regexpr)) {
  #     shutoffs_line_idx <- j
  #     break
  #   }
  # }
  
  # shutoff_lines <- relevant_lines[(shutoffs_line_idx + 1):(shutoffs_line_idx + 2)]
  print(shutoff_lines)
  shutoffs <- get_shutoff_data(shutoff_lines, curr_year, curr_quarter)
  out_filename <- paste("disconnections_", curr_year, "_q", curr_quarter, ".csv", sep = "")
  write.csv(shutoffs, file.path(outdir, out_filename), row.names = FALSE)
  
  print(paste("writing", out_filename))
}

print("DONE")
