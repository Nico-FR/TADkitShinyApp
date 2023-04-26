
library(shiny)
library(dplyr)
library(rhdf5)

###############################################
#mcool input
mcool.path = "./Data/indiv.HAv3.1.mapq_10.2500.mcool"
###############################################

###############################################
#get metadata from mcool file
###############################################
#read resolutions
mcool.resolutions = (rhdf5::h5ls(mcool.path) %>% filter(group == "/resolutions"))$name %>% as.numeric() %>% sort(decreasing = TRUE) %>% format(scientific = FALSE)

#read chr.lst
chromosomes.df = data.frame(
  names = rhdf5::h5read(file = mcool.path, name = paste0("resolutions/", mcool.resolutions[1],"/chroms/name")),
  lengths = rhdf5::h5read(file = mcool.path, name = paste0("resolutions/", mcool.resolutions[1],"/chroms/length")))
###############################################
