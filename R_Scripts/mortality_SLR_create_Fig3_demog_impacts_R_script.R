####Script to create figure 3 for mortality SLR
####Number of papers which find negative effects on abundance, survival and DD vs. total papers
####Liam Langley
####Date created - 05/04/2023

## load libraries

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
## also remove papers which look at FID - don't test for a specific effect
## remove papers about UAVs - don't test for a specific effect (2 papers)

df_clean <- df_papers%>%
  filter(Paper_Retained == "Yes") %>%
  filter(Discussion_Cat %in%  c("Surv", "Abun", "DD", "Prod"))

##relevel Demographic_Direction column to convert neutral to none

df_clean$Demographic_Direction <- ifelse(df_clean$Demographic_Direction == "Neutral", "None",
                                         paste(df_clean$Demographic_Direction))

##merge DD and prob levels of discussion cat - not all productivity studies explicitly mention DD

df_clean$Discussion_Cat <- ifelse(df_clean$Discussion_Cat == "DD", "Prod", paste(df_clean$Discussion_Cat))


#---------------------------------#
## Aggregate data for plotting ####
#---------------------------------#

## create a barplot of the number of papers which detected an effect (P/N/N) for any species tested
## on survival, abundance and DD
## and total number of papers
## facet by taxonomic group - ducks, geese, swans, waders, crane, multiple
## need to create aggregated data frame
## create sum column to aggregate by

df_clean$sum <- 1

## Create data frame

df_agg <- with(df_clean, aggregate(sum, by = list(Paper_ID, Discussion_Cat, Taxonomic_group, Demographic_Direction), "sum"))

names(df_agg)[1] <- "Paper_ID"
names(df_agg)[2] <- "Response_Type"
names(df_agg)[3] <- "Taxonomic_Group"
names(df_agg)[4] <- "Effect_PNN"
names(df_agg)[5] <- "NRows"

## reset sum column

df_agg$sum <- 1

## aggregate across papers

df_effect <- with(df_agg, aggregate(sum, by = list(Response_Type, Taxonomic_Group, Effect_PNN), "sum"))

names(df_effect)[1] <- "Response_Type"
names(df_effect)[2] <- "Taxonomic_Group"
names(df_effect)[3] <- "Effect_PNN"
names(df_effect)[4] <- "NPapers"

## need to aggregate for the total number of papers testing each combination of response, disturbance and TG

df_total <- with(df_agg, aggregate(sum, by = list(Response_Type, Taxonomic_Group), "sum"))

names(df_total)[1] <- "Response_Type"
names(df_total)[2] <- "Taxonomic_Group"
names(df_total)[3] <- "NPapers"

## create an Effect_YN column for df_total filled with "Total"

df_total$Effect_PNN <- "Total"

## merge data frames together for plotting

df_plot <- rbind(df_effect, df_total)

## add 0 values for combinations not represented in the data

df_expanded <- left_join(df_plot%>%
                           expand(Response_Type, Taxonomic_Group, Effect_PNN), df_plot)


#-------------------------------------------#
##Create plot - facet by Taxonomic Group ####
#-------------------------------------------#

## create a barplot of the n papers which detected an effect (Yes/No) in any species or specific responses 
## number of papers which didn't detect an effect in any species or specific responses tested
## and total number of papers for that combination
## by specific disturbance type
## facet by response type
## and taxonomic group - ducks, geese, swans, waders, crane, multiple

a <- ggplot(df_expanded, aes(x = Response_Type, y = NPapers, fill = Effect_PNN)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      facet_wrap(~ Taxonomic_Group) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00", "#3399FF")) +
      labs(x = "Demographic Response", y = "Number of Papers", fill = "Effect") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
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

out_path <- here("Outputs", "Mortality", "Plots")

## save plot

ggsave(plot = a, filename = "MSLR_effects_plot_by_demographic_response.tiff",
       device = device,
       path = out_path ,units = units, width = 175, height = 175, dpi = dpi,   
)


## very little data for cranes or demographic studies
## remove these panels and replot

df_reduced <- df_expanded %>%
  filter(Taxonomic_Group %in% c("Duck", "Geese"))

## almost all papers looked at ducks and geese
## subset down for these groups for figure

b <- ggplot(df_reduced, aes(x = Response_Type, y = NPapers, fill = Effect_PNN)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      facet_wrap(~ Taxonomic_Group) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00", "#3399FF")) +
      labs(x = "Demographic Response", y = "Number of Papers", fill = "Effect") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = b, filename = "MSLR_reduced_effects_plot_by_demographic_response.tiff",
       device = device,
       path = out_path ,units = units, width = 175, height = 175, dpi = dpi,   
)

## save out as figure 3
## change outpath

out_path <- here("Outputs", "Mortality", "Manuscript Figures")

## save plot

ggsave(plot = b, filename = "figure_3.tiff",
       device = device,
       path = out_path ,units = units, width = 175, height = 150, dpi = dpi,   
)


#------------------#
##End of script ####
#------------------#