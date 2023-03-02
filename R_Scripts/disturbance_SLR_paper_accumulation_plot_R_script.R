####Script to plot paper accumulation over time for the disturbance SLR
####Liam Langley
####Date created - 02/03/2023

## load libraries

library(tidyverse)
library(ggplot2)
library(here)


#----------------------------#
## Read in paper metadata ####
#----------------------------#

## set filepath for data read in

filepath <- here("Data", "Disturbance_papers_metadata_02March2023.csv")

## read in data
## metadata for all disturbance papers post abstract screening

df_meta <- read_csv(filepath)


#---------------------------------#
## Aggregate data for plotting ####
#---------------------------------#

## aggregate data so there is a single row for each paper
## create sum column to aggregate by

df_meta$sum <- 1

## aggregate data
## retain year and paper ID columns

df_meta2 <- with(df_meta, aggregate(sum, by = list(Paper_ID, Year), "sum"))

names(df_meta2)[1] <- "Paper_ID"
names(df_meta2)[2] <- "Year"
names(df_meta2)[3] <- "sum"

## aggregate to calculate cumulative number of papers per year
## first reset sum column

df_meta2$sum <- 1

## aggregate by year

df_year <- with(df_meta2, aggregate(sum, by = list(Year), "sum"))

names(df_year)[1] <- "Year"
names(df_year)[2] <- "NPapers"

## calculate cumulative number of papers

df_year$CumulPapers <- cumsum(df_year$NPapers)


#------------------------------------#
## Plot paper accumulation curves ####
#------------------------------------#

## plot accumulation of papers over time by year

acc_plot <- ggplot() +
             geom_line(data = df_year, aes(x = Year, y = CumulPapers), 
                       size = 1.2, col = "#FF9933") +
             labs(x = "Year", y = "Cumulative Papers") +
             theme(axis.text=element_text(colour="black"),
               ##Hide panel borders and remove grid lines
               legend.position = "none",
               panel.border = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.title.x = element_text(size = 15),
               axis.text.x = element_text(hjust=0.7),
               axis.title.y = element_text(angle=90, vjust = 0.4, size = 15),
               axis.text.y = element_text(hjust=0.7,vjust=0.3),
               strip.text.x = element_text(size = 15),
               strip.text.y = element_text(size = 15))


## Define parameters for reading out plot
## Define device to read plots out as e.g. tiff/jpeg

device <- "tiff"

## define units for plot size - usually mm

units <- "mm"

## define plot resolution in dpi - 300 usually minimum

dpi <- 300

## define filepath to read out plots 

out_path <- here("Outputs")

## save plot

ggsave(plot = acc_plot, filename = "disturbance_SLR_paper_acc_plot.tiff",
       device = device,
       path = out_path ,units = units, width = 200, height = 175, dpi = dpi,   
)


#-------------------#
## End of script ####
#-------------------#