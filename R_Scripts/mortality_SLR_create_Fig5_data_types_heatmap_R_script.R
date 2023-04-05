####Script to create figure 5 for mortality SLR
####Heatmap of demographic data used and harvest data used by spatial scale
####Liam Langley
####Date created - 05/04/2023

## load libraires

library(tidyverse)
library(ggplot2)
library(here)
library(ggpubr)


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
  filter(Paper_Retained == "Yes")

## relevel Demographic and harvest data columns to convert All to multiple

df_clean$Demographic_data_used <- ifelse(df_clean$Demographic_data_used == "All", "Multiple",
                                         paste(df_clean$Demographic_data_used))

## shorten Unobserved harvest loss to Harvest Loss

df_clean$Harvest_data_used <- ifelse(df_clean$Harvest_data_used == "Unobserved harvest loss", "Harvest loss",
                                         paste(df_clean$Harvest_data_used))


#------------------------------------------------#
## Create heatmaps for demographic data types ####
#------------------------------------------------#

## subset to remove NAs

df_demog <- df_clean%>%
  filter(Demographic_data_used != "NA")

## create sum column to aggregate by

df_demog$sum <- 1

## aggregate by Response Type and Taxonomic Group
## functions as a check for the heatmap
## first need to aggregate by paper ID - multiple species in one paper

df_ID <- with(df_demog, aggregate(sum, by = list(Paper_ID, Demographic_data_used, Taxonomic_group), "sum"))

names(df_ID)[1] <- "Paper_ID"
names(df_ID)[2] <- "Demographic_data_used"
names(df_ID)[3] <- "Taxonomic_Group"
names(df_ID)[4] <- "NRows"

##reset sum column

df_ID$sum <- 1

##aggregate by Hunting Bias and Taxonomic Group

df_DDTG <- with(df_ID, aggregate(sum, by = list(Demographic_data_used, Taxonomic_Group), "sum"))

names(df_DDTG)[1] <- "Demographic_data_used"
names(df_DDTG)[2] <- "Taxonomic_Group"
names(df_DDTG)[3] <- "NPapers"

##create heatmap from df_ID

a <- ggplot() + 
      geom_bin2d(data = df_ID, aes(x = Demographic_data_used, y = Taxonomic_Group)) +
      ##add counts to cells
      stat_bin2d(data = df_ID, geom = "text", aes(x = Demographic_data_used, y = Taxonomic_Group, 
                                                  label = ..count..)) +
      ##colour cells using scale_colour_gradient2
      scale_fill_gradient(low = "#FFFF00", high = "#FF3300") +
      labs(x = "Demographic Data", y = "Taxonomic Group", fill = "No. Papers") +
      ggtitle("a.") +
      ##make co-ordinates equal - perfect square cells
      coord_equal() +
      theme(axis.text=element_text(colour="black"),
            ##Hide panel borders and remove grid lines
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "#CCCCCC"),
            legend.position = "bottom",
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))


#------------------------------------------#
## Create heatmap for harvest data types####
#------------------------------------------#

## subset to remove NAs
## also remove "banded recovery" - captured by CMR in demographic data

df_harv <- df_clean%>%
  filter(Harvest_data_used != "NA") %>%
  filter(Harvest_data_used != c("banded recovery")) %>%
  filter(Harvest_data_used != c("Banded recovery"))

## create sum column to aggregate by

df_harv$sum <- 1

## aggregate by Data Type and Taxonomic Group
## functions as a check for the heatmap
## first need to aggregate by paper ID - multiple species in one paper

df_ID2 <- with(df_harv, aggregate(sum, by = list(Paper_ID, Harvest_data_used, Taxonomic_group), "sum"))

names(df_ID2)[1] <- "Paper_ID"
names(df_ID2)[2] <- "Harvest_data_used"
names(df_ID2)[3] <- "Taxonomic_Group"
names(df_ID2)[4] <- "NRows"

##reset sum column

df_ID2$sum <- 1

##aggregate by Hunting Bias and Taxonomic Group

df_HDTG <- with(df_ID2, aggregate(sum, by = list(Harvest_data_used, Taxonomic_Group), "sum"))

names(df_HDTG)[1] <- "Demographic_data_used"
names(df_HDTG)[2] <- "Taxonomic_Group"
names(df_HDTG)[3] <- "NPapers"

##create heatmap from df_ID

b <- ggplot() + 
      geom_bin2d(data = df_ID2, aes(x = Harvest_data_used, y = Taxonomic_Group)) +
      ##add counts to cells
      stat_bin2d(data = df_ID2, geom = "text", aes(x = Harvest_data_used, y = Taxonomic_Group, 
                                                  label = ..count..)) +
      ##colour cells using scale_colour_gradient2
      scale_fill_gradient(low = "#FFFF00", high = "#FF3300") +
      labs(x = "Harvest Data", y = "Taxonomic Group", fill = "No. Papers") +
      ggtitle("b.") +
      ##make co-ordinates equal - perfect square cells
      coord_equal() +
      theme(axis.text=element_text(colour="black"),
            ##Hide panel borders and remove grid lines
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "#CCCCCC"),
            legend.position = "bottom",
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))


#----------------------------#
## Create combined figure ####
#----------------------------#

##combined demographic and harvest data plots using ggarrange

fig5 <- ggarrange(a , b, nrow = 1, ncol = 2, widths = c(1, 1))

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

ggsave(plot = fig5, filename = "figure_5.tiff",
       device = device,
       path = out_path ,units = units, width = 175, height = 150, dpi = dpi,   
)


#-------------------#
## End of script ####
#-------------------#
