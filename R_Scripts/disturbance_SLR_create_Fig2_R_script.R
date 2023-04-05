####Script to create figure 2 for disturbance SLR
####Involves paper accumulation, journal breakdown and map
####Liam Langley
####Date created - 28/03/2023

## load libraries

library(tidyverse)
library(ggplot2)
library(here)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(ggpubr)
library(cowplot)

#----------------------------#
## Read in and clean data ####
#----------------------------#

## set filepath for data read in

filepath <- here("Data", "Disturbance_final_data_cleaned.csv")

## read in data
## metadata for all disturbance papers post abstract screening

df_papers <- read_csv(filepath)

##first remove papers excluded by full text screening

df_clean <- df_papers%>%
  filter(Paper_Retained == "Yes")


#--------------------------------------------#
## Aggregate data by journal for plotting ####
#--------------------------------------------#

## aggregate data so there is a single row for each paper
## create sum column to aggregate by

df_clean$sum <- 1

## aggregate data
## retain Journal and paper ID columns

df_journal <- with(df_clean, aggregate(sum, by = list(Paper_ID, Journal), "sum"))

names(df_journal)[1] <- "Paper_ID"
names(df_journal)[2] <- "Journal"
names(df_journal)[3] <- "sum"


##convert Journal column to a factor and check levels

df_journal$Journal <- as.factor(as.vector(df_journal$Journal))

levels(df_journal$Journal)

## 56 levels - no duplicates
## aggregate by journal
## first reset sum column

df_journal$sum <- 1

## aggregate by journal

df_journal2 <- with(df_journal, aggregate(sum, by = list(Journal), "sum"))

names(df_journal2)[1] <- "Journal"
names(df_journal2)[2] <- "NPapers"


#-------------------------------#
## Visualise paper breakdown ####
#-------------------------------#

## create barplot of NPapers per journal

journal_plot  <- ggplot(df_journal2, aes(x = reorder(Journal, NPapers), y = NPapers)) +
                  coord_flip() +
                  geom_bar(stat = "identity", fill = "#FF9933", col = "black") +
                  labs(x = " Journal", y = "Number of Papers") +
                  theme_bw() +
                  theme(panel.grid.major = element_blank(), 
                        axis.line = element_line(colour = "black", size=0.7),
                        axis.text.x = element_text(size= 10),
                        axis.text.y = element_text(size = 10),
                        axis.title=element_text(size=15))


## Define parameters for reading out plot
## Define device to read plots out as e.g. tiff/jpeg

device <- "tiff"

## define units for plot size - usually mm

units <- "mm"

## define plot resolution in dpi - 300 usually minimum

dpi <- 300

## define filepath to read out plots 

out_path <- here("Outputs", "Disturbance", "Plots")

## save plot

ggsave(plot = journal_plot, filename = "disturbance_SLR_journal_breakdown.tiff",
       device = device,
       path = out_path ,units = units, width = 250, height = 300, dpi = dpi,   
)


#-----------------------------------------#
## Aggregate data by year for plotting ####
#-----------------------------------------#

## aggregate data
## retain year and paper ID columns

df_year <- with(df_clean, aggregate(sum, by = list(Paper_ID, Year), "sum"))

names(df_year)[1] <- "Paper_ID"
names(df_year)[2] <- "Year"
names(df_year)[3] <- "sum"

## aggregate to calculate cumulative number of papers per year
## first reset sum column

df_year$sum <- 1

## aggregate by year

df_year2 <- with(df_year, aggregate(sum, by = list(Year), "sum"))

names(df_year2)[1] <- "Year"
names(df_year2)[2] <- "NPapers"

## calculate cumulative number of papers

df_year2$CumulPapers <- cumsum(df_year2$NPapers)


#------------------------------------#
## Plot paper accumulation curves ####
#------------------------------------#

## plot accumulation of papers over time by year

acc_plot <- ggplot() +
             geom_line(data = df_year2, aes(x = Year, y = CumulPapers), 
                       size = 1.2, col = "#FF9933") +
             labs(x = "Year", y = "Cumulative Papers") +
             ggtitle("a.") +
             theme_bw() +
             theme(panel.grid.major = element_blank(), 
                   ##panel.grid.minor = element_blank(), 
                   ##panel.border = element_blank(), 
                   axis.title.x = element_text(size = 12),
                   axis.text.x = element_text(hjust=1, angle = 45),
                   axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
                   axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))



## save plot

ggsave(plot = acc_plot, filename = "disturbance_SLR_paper_acc_plot.tiff",
       device = device,
       path = out_path ,units = units, width = 200, height = 175, dpi = dpi,   
)


#----------------------------------------------#
## Aggregate data by continent for plotting ####
#----------------------------------------------#

## aggregate data
## retain continent and paper ID columns

df_map <- with(df_clean, aggregate(sum, by = list(Paper_ID, Continent), "sum"))

names(df_map)[1] <- "Paper_ID"
names(df_map)[2] <- "Continent"
names(df_map)[3] <- "sum"

## aggregate to calculate cumulative number of papers per year
## first reset sum column

df_map$sum <- 1

## aggregate by continent

df_map2 <- with(df_map, aggregate(sum, by = list(Continent), "sum"))

names(df_map2)[1] <- "continent"
names(df_map2)[2] <- "NPapers"

## rename Australasia as Oceania to match world data

df_map2$continent <- ifelse(df_map2$continent == "Australasia", 
                            "Oceania", paste(df_map2$continent))

## read in map data from Rnaturalearth
## medium resolution

world <- ne_countries(scale = "medium", returnclass = "sf")

## bind world data to paper counts for plotting

map_totals <- world %>%
  full_join(df_map2)

#--------------------------------------------#
## Create heat map of papers by continent ####
#--------------------------------------------#

##heat map of number of papers by continent
##move legend to bottom

paper_map <- ggplot(data = map_totals) +
              geom_sf(aes(fill = NPapers)) +
              scale_fill_gradient(low = "#FFFF00", high = "#FF3300") +
              labs(fill = "No. Papers") +
              ggtitle("c.") +
              theme(axis.text=element_text(colour="black"),
                    ##Hide panel borders and remove grid lines
                    panel.border = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "#CCFFFF"),
                    legend.position = "bottom",
                    axis.title.x = element_text(size = 12),
                    axis.text.x = element_text(hjust=1, angle = 45),
                    axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
                    axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = paper_map, filename = "disturbance_SLR_paper_map.tiff",
       device = device,
       path = out_path ,units = units, width = 200, height = 175, dpi = dpi,   
)


#------------------------------------------------------------#
##Create bar plot of number of papers per taxonomic group ####
#------------------------------------------------------------#

## aggregate data
## retain taxonomic group and paper ID columns

df_TG <- with(df_clean, aggregate(sum, by = list(Paper_ID, Taxonomic_group), "sum"))

names(df_TG)[1] <- "Paper_ID"
names(df_TG)[2] <- "Taxonomic_Group"
names(df_TG)[3] <- "sum"

## aggregate to calculate cumulative number of papers per year
## first reset sum column

df_TG$sum <- 1

## aggregate by continent

df_TG2 <- with(df_TG, aggregate(sum, by = list(Taxonomic_Group), "sum"))

names(df_TG2)[1] <- "Taxonomic_Group"
names(df_TG2)[2] <- "NPapers"

##relevel taxonomic group so multiple last

df_TG2$Taxonomic_Group <- factor(df_TG2$Taxonomic_Group, levels = c("Crane", "Duck", "Geese", "Rails",
                                                                    "Swan", "Waders", "Multiple"))

##create barplot

tg_plot <- ggplot(df_TG2, aes(x = Taxonomic_Group, y = NPapers)) +
            geom_bar(stat = "identity", fill = "#FFCC00") +
            labs(x = "Response Type", y = "Number of Papers", fill = "Effect") +
            ggtitle("b.") +
            theme_bw() +
            theme(panel.grid.major = element_blank(), 
                  ##panel.grid.minor = element_blank(), 
                  ##panel.border = element_blank(), 
                  axis.title.x = element_text(size = 12),
                  axis.text.x = element_text(hjust=1, angle = 45),
                  axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
                  axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = tg_plot, filename = "disturbance_SLR_tg_barplot.tiff",
       device = device,
       path = out_path ,units = units, width = 200, height = 175, dpi = dpi,   
)


#-----------------------------#
##Make facet plot - Fig. 2 ####
#-----------------------------#

## make facet plot of paper accumulation, TG and map plots
## use cowplot
## first make plot for top row

top <- ggdraw() +
        draw_plot(acc_plot + tg_plot)

## now combine with map

fig2 <- ggarrange(top, paper_map, ncol = 1, nrow = 2,
                  widths = c(1, 1))

## change filepath to read out figure

out_path <- here("Outputs", "Disturbance", "Manuscript Figures")

## save plot

ggsave(plot = fig2, filename = "figure_2.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 175, dpi = dpi,   
)


#------------------#
##End of script ####
#------------------#