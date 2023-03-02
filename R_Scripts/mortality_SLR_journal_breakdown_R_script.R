####Script to plot a breakdown by journal for the mortality SLR
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

filepath <- here("Data", "Mortality_papers_metadata_02March2023.csv")

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
## retain Journal and paper ID columns

df_meta2 <- with(df_meta, aggregate(sum, by = list(Paper_ID, Journal), "sum"))

names(df_meta2)[1] <- "Paper_ID"
names(df_meta2)[2] <- "Journal"
names(df_meta2)[3] <- "sum"


##convert Journal column to a factor and check levels

df_meta2$Journal <- as.factor(as.vector(df_meta2$Journal))

levels(df_meta2$Journal)

## 40 levels - no duplicates
## aggregate by journal
## first reset sum column

df_meta2$sum <- 1

## aggregate by year

df_journal <- with(df_meta2, aggregate(sum, by = list(Journal), "sum"))

names(df_journal)[1] <- "Journal"
names(df_journal)[2] <- "NPapers"


#-------------------------------#
## Visualise paper breakdown ####
#-------------------------------#

## create barplot of NPapers per journal

journal_plot  <- ggplot(df_journal, aes(x = reorder(Journal, NPapers), y = NPapers)) +
                  coord_flip() +
                  geom_bar(stat = "identity", fill = "#FF9933", col = "black") +
                  labs(x = " Journal", y = "Number of Papers") +
                  theme_bw() +
                  theme(panel.grid.major = element_blank(), 
                        axis.line = element_line(colour = "black", size=0.7),
                        axis.text.x = element_text(size= 12),
                        axis.text.y = element_text(size = 12),
                        axis.title=element_text(size=15))


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

ggsave(plot = journal_plot, filename = "mortality_SLR_journal_breakdown.tiff",
       device = device,
       path = out_path ,units = units, width = 250, height = 300, dpi = dpi,   
)


#-------------------#
## End of script ####
#-------------------#