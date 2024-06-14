#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(G2Sd)
  library(ggplot2)
  library(argparse)
  library(dplyr)
  library(tidyr)
  library(patchwork)
  library(readxl)
  library(qtl)
})
# you will need more libraries than those above

# given a plot, work out it's limits
get_plot_limits<-function(data_exel) {
  
  #
  data_exel <- select(data_exel, -sample_depth_cm)
  #Minimum and maximum values in core_16
  min_value <- min(data_exel)
  max_value <- max(data_exel)
  limits <- list(min_value, max_value)
} 

read_sed_data<-function(input_file) {
  # Read the Excel file and return the data
  sed_data <- read_excel(input_file)
  return(sed_data)
  
  
  # Define the path to the input file
  input_file <- "Raw Data/core_16.xlsx"
  sed_data <- read_sed_data(input_file)
  print(head(sed_data))
}


read_core_depths <- function(depth_file) {
  #Implement logic to read core depth data from file
  read.csv(depth_file)
}

calculate_grain_percentages<-function(grain_sizes, grain_stats, gs_data_matrix) {
  
  Silt_data <- rowSums(sed_data[ , c(2:64)])
  VCSilt_data <- rowSums(sed_data[ , c(65:70)])
  VFS_data <- rowSums(sed_data[ , c(71:76)])
  FS_data <- rowSums(sed_data[ , c(77:82)])
  MS_data <- rowSums(sed_data[ , c(83:88)])
  CS_data <- rowSums(sed_data[ , c(89:94)])
  VCS_data <- rowSums(sed_data[ , c(95:100)])
  VFG_data <- rowSums(sed_data[ , c(101)])
  
  
  sed_data_percentages <- data.frame(Silt = c(Silt_data),
                                     VCSilt = c(VCSilt_data),
                                     VFS = c(VFS_data),
                                     FS = c(FS_data),
                                     MS = c(MS_data),
                                     CS = c(CS_data),
                                     VCS = c(VCS_data),
                                     VFG = c(VFG_data))
}

main <- function(args) {
  
  #
  sed_data <- read_sed_data("Raw Data/core_16.xlsx")
  
  #
  Limits <- get_plot_limits(sed_data)
  
  #
  min_value <- Limits[[1]]
  max_value <- Limits[[2]]
  
  if (is.null(args$depth_data)) {
    # default depths file
    depth_file <- "depth_data (1).csv"
    #
    depth_data <- read_core_depths(depth_file)
  } else {
    depth_file <- args$depth_data 
  }
  
  #
  sed_data_percentages <- calculate_grain_percentages(sed_data)
  
  colsum
  # now use the gradistats package to get the mean, kurtosis, etc and the % 
  # in each standard grain size bucket
  grain_stats <- granstat(gs_data_matrix)
  
  # set-up the order in which we want grainsizes to be displayed (and which we want to be displayed)
  grain_sizes <- c("Silt",
                   "VCSilt",
                   "VFS",
                   "FS",
                   "MS",
                   "CS",
                   "VCS",
                   "VFG"
  )
  # Note, leaving off coarse grains
  
  # save your plot
}

if(sys.nframe() == 0) {
  
  # main program, called via Rscript
  parser = ArgumentParser(
    prog="Sediment Analysis",
    description="Plot graphs of sediment grain stats for a single core"
  )
  parser$add_argument("core_filename",
                      help="the file to read the core data from")
  parser$add_argument("output_filename",
                      help="the file to save the graphic to")
  parser$add_argument('-v', '--verbose',
                      action='store_true',
                      help="Print progress")
  parser$add_argument('-d', '--depth_file',
                      help="Optional file for depth data")
  
  args = parser$parse_args()  
  main(args)
}

