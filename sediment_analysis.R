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
  library(moments)
  library(e1071)
})
# you will need more libraries than those above

# given a plot, work out it's limits
get_plot_limits<-function(data_exel) {
  
  # Removes the 'sample_depth_cm' column from the 'data_exel' dataframe
  data_exel <- select(data_exel, -sample_depth_cm)
  #Minimum and maximum values in core_16
  min_value <- min(data_exel)
  max_value <- max(data_exel)
  # Store the min and max values in a list named 'limits'
  limits <- list(min_value, max_value)
} 

# Created the function 'read_sed_data' that takes an input file path as an argument.
read_sed_data<-function(input_file) {
  # Read the Excel file and return the data
  sed_data <- read_excel(input_file)
  return(sed_data)
  
  
  # Define the path to the input file
  input_file <- "Raw Data/core_16.xlsx"
  # Read the sediment data from the Excel file
  sed_data <- read_sed_data(input_file)
  # Print first few rows of data
  print(head(sed_data))
}


read_core_depths <- function(depth_file) {
  #Implement logic to read core depth data from file
  read.csv(depth_file)
}

calculate_grain_percentages<-function(grain_sizes, grain_stats, gs_data_matrix) {
  
  #row percentage sums for different sediment size categories
  Silt_data_percent <- rowSums(sed_data[ , c(2:64)])
  VCSilt_data_percent <- rowSums(sed_data[ , c(65:70)])
  VFS_data_percent <- rowSums(sed_data[ , c(71:76)])
  FS_data_percent <- rowSums(sed_data[ , c(77:82)])
  MS_data_percent <- rowSums(sed_data[ , c(83:88)])
  CS_data_percent <- rowSums(sed_data[ , c(89:94)])
  VCS_data_percent <- rowSums(sed_data[ , c(95:100)])
  VFG_data_percent <- rowSums(sed_data[ , c(101)])
  
  # Create new dataframe with the calculated percentages
  sed_data_percentages <- data.frame(Silt = c(Silt_data),
                                     VCSilt = c(VCSilt_data_percent),
                                     VFS = c(VFS_data_percent),
                                     FS = c(FS_data_percent),
                                     MS = c(MS_data_percent),
                                     CS = c(CS_data_percent),
                                     VCS = c(VCS_data_percent),
                                     VFG = c(VFG_data_percent))
}

main <- function(args) {
  
  # Read the sediment data from the Excel file
  sed_data <- read_sed_data("Raw Data/core_16.xlsx")
  
  # Get the plot limits from the sediment data
  Limits <- get_plot_limits(sed_data)
  
  # Extract minimum and maximum values from the plot limits
  min_value <- Limits[[1]]
  max_value <- Limits[[2]]
  
  if (is.null(args$depth_data)) {
    # default depths file
    depth_file <- "depth_data (1).csv"
    # Read the core depths from default depths file
    depth_data <- read_core_depths(depth_file)
  } else {
    depth_file <- args$depth_data 
  }
  
  # now use the gradistats package to get the mean, kurtosis, etc and the %
  # in each standard grain size bucket
  grain_stats <- granstat(gs_data_matrix)
  
  # Calculate grain size percentages from sediment data
  sed_data_percentages <- calculate_grain_percentages(sed_data)
  
  #Calculating mean values
  Silt_mean <- colMeans(sed_data[, 2:64])
  VCSilt_mean <- colMeans(sed_data[, 65:70])
  VFS_mean <- colMeans(sed_data[, 71:76])
  FS_mean <- colMeans(sed_data[, 77:82])
  MS_mean <- colMeans(sed_data[, 83:88])
  CS_mean <- colMeans(sed_data[89:94])
  VCS_mean <- colMeans(sed_data[95:100])
  VFG_mean <- colMeans(sed_data[101])
  
  # Print the lengths of the calculated mean vectors
  print(length(Silt_mean))    
  print(length(VCSilt_mean))  
  print(length(VFS_mean))     
  print(length(FS_mean))      
  print(length(MS_mean))      
  print(length(CS_mean))      
  print(length(VCS_mean))     
  print(length(VFG_mean))
  
  # Since they have different lengths, can't combine them directly into a data frame.
  # Code creats a list
  means_list <- list(
    Silt = Silt_mean,
    VCSilt = VCSilt_mean,
    VFS = VFS_mean,
    FS = FS_mean,
    MS = MS_mean,
    CS = CS_mean,
    VCS = VCS_mean,
    VFG = VFG_mean
  )
  print(means_list)
  
  
  # Calculate kurtosis for each specified range
  kurtosis_Silt <- apply(sed_data[ , c(2:64)], 1, moments::kurtosis)
  kurtosis_VCSilt <- apply(sed_data[ , c(65:70)], 1, moments::kurtosis)
  kurtosis_VFS <- apply(sed_data[ , c(71:76)], 1, moments::kurtosis)
  kurtosis_FS <- apply(sed_data[ , c(77:82)], 1, moments::kurtosis)
  kurtosis_MS <- apply(sed_data[ , c(83:88)], 1, moments::kurtosis)
  kurtosis_CS <- apply(sed_data[ , c(89:94)], 1, moments::kurtosis)
  kurtosis_VCS <- apply(sed_data[ , c(95:100)], 1, moments::kurtosis)
  kurtosis_VFG <- apply(sed_data[ , c(101)], 1, moments::kurtosis)
  
  # Combine the kurtosis results into a data frame
  sed_data_kurtosis <- data.frame(Silt = kurtosis_Silt,
                                  VCSilt = kurtosis_VCSilt,
                                  VFS = kurtosis_VFS,
                                  FS = kurtosis_FS,
                                  MS = kurtosis_MS,
                                  CS = kurtosis_CS,
                                  VCS = kurtosis_VCS,
                                  VFG = kurtosis_VFG)
  
  # Display the resulting data frame
  print(sed_data_kurtosis)
  
  # Calculate skewness for each specified range
  skewness_Silt <- apply(sed_data[ , c(2:64)], 1, moments::skewness)
  skewness_VCSilt <- apply(sed_data[ , c(65:70)], 1, moments::skewness)
  skewness_VFS <- apply(sed_data[ , c(71:76)], 1, moments::skewness)
  skewness_FS <- apply(sed_data[ , c(77:82)], 1, moments::skewness)
  skewness_MS <- apply(sed_data[ , c(83:88)], 1, moments::skewness)
  skewness_CS <- apply(sed_data[ , c(89:94)], 1, moments::skewness)
  skewness_VCS <- apply(sed_data[ , c(95:100)], 1, moments::skewness)
  skewness_VFG <- apply(sed_data[ , c(101)], 1, moments::skewness)
  
  # Combine skewness results into a data frame
  sed_data_skewness <- data.frame(Silt = skewness_Silt,
                                  VCSilt = skewness_VCSilt,
                                  VFS = skewness_VFS,
                                  FS = skewness_FS,
                                  MS = skewness_MS,
                                  CS = skewness_CS,
                                  VCS = skewness_VCS,
                                  VFG = skewness_VFG)
  
  # Calculate sorting (standard deviation) for each specified range
  sorting_Silt <- apply(sed_data[ , c(2:64)], 1, sd)
  sorting_VCSilt <- apply(sed_data[ , c(65:70)], 1, sd)
  sorting_VFS <- apply(sed_data[ , c(71:76)], 1, sd)
  sorting_FS <- apply(sed_data[ , c(77:82)], 1, sd)
  sorting_MS <- apply(sed_data[ , c(83:88)], 1, sd)
  sorting_CS <- apply(sed_data[ , c(89:94)], 1, sd)
  sorting_VCS <- apply(sed_data[ , c(95:100)], 1, sd)
  sorting_VFG <- apply(sed_data[ , c(101)], 1, sd)
  
  # Combine the sorting results into a data frame
  sed_data_sorting <- data.frame(Silt = sorting_Silt,
                                 VCSilt = sorting_VCSilt,
                                 VFS = sorting_VFS,
                                 FS = sorting_FS,
                                 MS = sorting_MS,
                                 CS = sorting_CS,
                                 VCS = sorting_VCS,
                                 VFG = sorting_VFG)
  
  
  
  
  
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
  
  
  #Because of time constraints, I was unable to plot the variables, however, all data compiled into data sets. 
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
