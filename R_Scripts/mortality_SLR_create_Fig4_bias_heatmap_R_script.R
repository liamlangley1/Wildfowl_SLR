####Script to make figure 4 for mortality SLR
####Heatmap of papers which look for biases in different categories
####Liam Langley
####Date created - 05/04/2023

## load libraires

library(tidyverse)
library(ggplot2)
library(here)


#----------------------------#
## Read in and clean data ####
#----------------------------#

## set filepath for data read in

filepath <- here("Data", "Mortality_final_data_cleaned.csv")

## read in data
## metadata for all disturbance papers post abstract screening

df_papers <- read_csv(filepath)

## remove records excluded during full text screening
## filter for papers which just look at hunting biases

df_clean <- df_papers%>%
  filter(Paper_Retained == "Yes") %>%
  filter(Hunting_Bias %in% c("Age bias", "Sex bias", "Body condition"))


#---------------------------------#
## Aggregate data for plotting ####
#---------------------------------#

## create sum column to aggregate by

df_clean$sum <- 1

## aggregate by Response Type and Taxonomic Group
## functions as a check for the heatmap
## first need to aggregate by paper ID - multiple species in one paper

df_ID <- with(df_clean, aggregate(sum, by = list(Paper_ID, Hunting_Bias, Taxonomic_group), "sum"))

names(df_ID)[1] <- "Paper_ID"
names(df_ID)[2] <- "Hunting_Bias"
names(df_ID)[3] <- "Taxonomic_Group"
names(df_ID)[4] <- "NRows"

##reset sum column

df_ID$sum <- 1

##aggregate by Hunting Bias and Taxonomic Group

df_HBTG <- with(df_ID, aggregate(sum, by = list(Hunting_Bias, Taxonomic_Group), "sum"))

names(df_HBTG)[1] <- "Hunting_Bias"
names(df_HBTG)[2] <- "Taxonomic_Group"
names(df_HBTG)[3] <- "NPapers"

##create heatmap from df_ID

a <- ggplot() + 
      geom_bin2d(data = df_ID, aes(x = Hunting_Bias, y = Taxonomic_Group)) +
      ##add counts to cells
      stat_bin2d(data = df_ID, geom = "text", aes(x = Hunting_Bias, y = Taxonomic_Group, 
                                                  label = ..count..)) +
      ##colour cells using scale_colour_gradient2
      scale_fill_gradient(low = "#FFFF00", high = "#FF3300") +
      labs(x = "Hunting Bias", y = "Taxonomic Group", fill = "No. Papers") +
      ##make co-ordinates equal - perfect square cells
      coord_equal() +
      theme(axis.text=element_text(colour="black"),
            ##Hide panel borders and remove grid lines
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "#CCCCCC"),
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))


## Define parameters for reading out plot
## Define device to read plots out as e.g. tiff/jpeg

device <- "tiff"

## define units for plot size - usually mm

units <- "mm"

## define plot resolution in dpi - 300 usually minimum

dpi <- 300

## define filepath to read out plots 

out_path <- here("Outputs", "Mortality", "Manuscript Figures")

## save plot

ggsave(plot = a, filename = "figure_4.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


#------------------#
## End of script####
#------------------#