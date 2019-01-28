library(here)
library(fs)
library(purrr)

folder_names <- c("raw_data", "output_data", "rmd", "docs", "scripts")

map(folder_names, dir_create)
