####Script to make heatmaps for disturbance SLR
####Liam Langley
####Date created - 29/03/2023

## load libraries

library(tidyverse)
library(ggplot2)
library(here)


#------------------#
## Read in data ####
#------------------#

## set filepath for data read in

filepath <- here("Data", "Disturbance_final_data_cleaned.csv")

## read in data
## metadata for all disturbance papers post abstract screening

df_papers <- read_csv(filepath)

##remove records excluded during full text screening

df_clean <- df_papers%>%
  filter(Paper_Retained == "Yes")


#-----------------------------------------------------#
##Wildfowling papers - response by taxonomic group ####
#-----------------------------------------------------#

## subset data set for papers which look at wildfowling impacts only

df_wf <- df_clean %>%
  filter(Disturbance_Type == "Wildfowling")

## create sum column to aggregate by

df_wf$sum <- 1

## aggregate by Response Type and Taxonomic Group
## functions as a check for the heatmap
##first need to aggregate by paper ID - multiple behavioural responses in one paper

df_ID <- with(df_wf, aggregate(sum, by = list(Paper_ID, Response_Type, Taxonomic_group), "sum"))

names(df_ID)[1] <- "Paper_ID"
names(df_ID)[2] <- "Response_Type"
names(df_ID)[3] <- "Taxonomic_Group"
names(df_ID)[4] <- "NPapers"

##reset sum column

df_ID$sum <- 1

##aggregate by Response Type and Taxonomic Group

df_RTTG <- with(df_ID, aggregate(sum, by = list(Response_Type, Taxonomic_Group), "sum"))

names(df_RTTG)[1] <- "Response_Type"
names(df_RTTG)[2] <- "Taxonomic_Group"
names(df_RTTG)[3] <- "NPapers"

##create heatmap from raw data

a <- ggplot() + 
      geom_bin2d(data = df_ID, aes(x = Response_Type, y = Taxonomic_Group)) +
      ##add counts to cells
      stat_bin2d(data = df_ID, geom = "text", aes(x = Response_Type, y = Taxonomic_Group, 
                                                     label = ..count..)) +
      ##colour cells using scale_colour_gradient2
      scale_fill_gradient(low = "#FFFF00", high = "#FF3300") +
      labs(x = "Response Type", y = "Taxonomic Group", col = "No. Papers") +
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
            axis.text.y = element_text(hjust=0.7, angle = 90, vjust=0.3))


## Define parameters for reading out plot
## Define device to read plots out as e.g. tiff/jpeg

device <- "tiff"

## define units for plot size - usually mm

units <- "mm"

## define plot resolution in dpi - 300 usually minimum

dpi <- 300

## define filepath to read out plots 

out_path <- here("Outputs", "Heatmaps")

## save plot

ggsave(plot = a, filename = "dslr_wildfowling_taxonomic_group_by_response_type_heatmap.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 200, dpi = dpi,   
)


#------------------------------------------------------------------#
##All papers - response by taxonomic group and disturbance type ####
#------------------------------------------------------------------#

## aggregate by Response Type and Taxonomic Group
## functions as a check for the heatmap
##first create sum column to aggregate by

df_clean$sum <- 1

## aggregate by Response Type, Taxonomic Group and Disturbance Type
## functions as a check for the heatmap
##first need to aggregate by paper ID - multiple behavioural responses in one paper

df_ID2 <- with(df_clean, aggregate(sum, by = list(Paper_ID, Response_Type, Taxonomic_group,
                                                  Disturbance_Type), "sum"))

names(df_ID2)[1] <- "Paper_ID"
names(df_ID2)[2] <- "Response_Type"
names(df_ID2)[3] <- "Taxonomic_Group"
names(df_ID2)[4] <- "Disturbance_Type"
names(df_ID2)[5] <- "NPapers"

##reset sum column

df_ID2$sum <- 1

##aggregate by Response Type, Taxonomic Group and Disturbance Type

df_RTTGDT <- with(df_ID2, aggregate(sum, by = list(Response_Type, Taxonomic_Group, Disturbance_Type), "sum"))

names(df_RTTGDT)[1] <- "Response_Type"
names(df_RTTGDT)[2] <- "Taxonomic_Group"
names(df_RTTGDT)[3] <- "Disturbance_Type"
names(df_RTTGDT)[4] <- "NPapers"

##create heatmap

b <- ggplot() + 
      geom_bin2d(data = df_ID2, aes(x = Response_Type, y = Taxonomic_Group)) +
      ##add counts to cells
      stat_bin2d(data = df_ID2, geom = "text", aes(x = Response_Type, y = Taxonomic_Group, 
                                                  label = ..count..)) +
      ##facet wrap by disturbance type
      facet_wrap(~ Disturbance_Type) +
      ##colour cells using scale_colour_gradient2
      scale_fill_gradient(low = "#FFFF00", high = "#FF3300") +
      labs(x = "Response Type", y = "Taxonomic Group", col = "No. Papers") +
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

out_path <- here("Outputs", "Heatmaps")

## save plot

ggsave(plot = b, filename = "dslr_wildfowling_taxonomic_group_by_response_and_disturbance_type_heatmap.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 125, dpi = dpi,   
)


#------------------------------------------------------------------------------#
##Recreational disturbance papers - disturbance specific by taxonomic group ####
#------------------------------------------------------------------------------#

##subset data for recreational disturbance papers only

df_rec <- df_clean %>%
  filter(Disturbance_Type == "Recreational")

## create sum column to aggregate by

df_rec$sum <- 1

## aggregate by Response Type and Taxonomic Group
## functions as a check for the heatmap
##first need to aggregate by paper ID - multiple behavioural responses in one paper

df_ID3 <- with(df_rec, aggregate(sum, by = list(Paper_ID, Taxonomic_group, Disturbance_Sp), "sum"))

names(df_ID2)[1] <- "Paper_ID"
names(df_ID2)[2] <- "Response_Type"
names(df_ID2)[3] <- "Taxonomic_Group"
names(df_ID2)[4] <- "Disturbance_Type"
names(df_ID2)[5] <- "NPapers"

##reset sum column

df_ID2$sum <- 1

##aggregate by Response Type, Taxonomic Group and Disturbance Type

df_RTTGDT <- with(df_ID2, aggregate(sum, by = list(Response_Type, Taxonomic_Group, Disturbance_Type), "sum"))

names(df_RTTGDT)[1] <- "Response_Type"
names(df_RTTGDT)[2] <- "Taxonomic_Group"
names(df_RTTGDT)[3] <- "Disturbance_Type"
names(df_RTTGDT)[4] <- "NPapers"
