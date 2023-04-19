####Script to make plots of disturbance effects
####Create barplots of proportion of papers that detected an effect
####Liam Langley
####Date created - 29/03/2023
####Date updated - 19/04/2023

## load libraries

library(tidyverse)
library(ggplot2)
library(here)


#----------------------------#
## Read in and clean data ####
#----------------------------#

## set filepath for data read in

filepath <- here("Data", "Disturbance_final_data_cleaned.csv")

## read in data
## metadata for all disturbance papers post abstract screening

df_papers <- read_csv(filepath)

## remove records excluded during full text screening
## also remove papers which look at FID - don't test for a specific effect
## remove papers about UAVs - don't test for a specific effect (2 papers)

df_clean <- df_papers%>%
  filter(Paper_Retained == "Yes") %>%
  filter(Response_Specific != "FID") %>%
  filter(Disturbance_Specific != "UAVs")


#--------------------------------#
##Aggregate data for plotting ####
#--------------------------------#

## create a barplot of the number of papers which detected an effect (Yes/No) in any species detected
## number of papers which didn't detect an effect in any species tested
## and total number of papers
## by specific disturbance source
## facet by response type - behavioural, demographic, physiological, site
## and taxonomic group - ducks, geese, swans, waders, crane, multiple
## need to create aggregated data frame
## create sum column to aggregate by

df_clean$sum <- 1

## Create data frame

df_agg <- with(df_clean, aggregate(sum, by = list(Paper_ID, Response_Type, Taxonomic_group, Disturbance_Specific,
                                                  Effect_YN), "sum"))

names(df_agg)[1] <- "Paper_ID"
names(df_agg)[2] <- "Response_Type"
names(df_agg)[3] <- "Taxonomic_Group"
names(df_agg)[4] <- "Disturbance_Type"
names(df_agg)[5] <- "Effect_YN"
names(df_agg)[6] <- "NRows"

## reset sum column

df_agg$sum <- 1

## aggregate across papers

df_effect <- with(df_agg, aggregate(sum, by = list(Response_Type, Taxonomic_Group, Disturbance_Type,
                                                   Effect_YN), "sum"))

names(df_effect)[1] <- "Response_Type"
names(df_effect)[2] <- "Taxonomic_Group"
names(df_effect)[3] <- "Disturbance_Type"
names(df_effect)[4] <- "Effect_YN"
names(df_effect)[5] <- "NPapers"

## need to aggregate for the total number of papers testing each combination of response, disturbance and TG

df_total <- with(df_agg, aggregate(sum, by = list(Response_Type, Taxonomic_Group, Disturbance_Type), "sum"))

names(df_total)[1] <- "Response_Type"
names(df_total)[2] <- "Taxonomic_Group"
names(df_total)[3] <- "Disturbance_Type"
names(df_total)[4] <- "NPapers"

## create an Effect_YN column for df_total filled with "Total"

df_total$Effect_YN <- "Total"

## merge data frames together for plotting

df_plot <- rbind(df_effect, df_total)

## add 0 values for combinations not represented in the data

df_expanded <- left_join(df_plot%>%
                           expand(Response_Type, Taxonomic_Group, Disturbance_Type, Effect_YN), df_plot)

## reorder disturbance type variable to group in order of broader disturbance categories

levels(as.factor(df_expanded$Disturbance_Type))

df_expanded$Disturbance_Type <- factor(df_expanded$Disturbance_Type, levels = c("Walking", "Angling", 
                                       "Bait digging", "Boating", "Cycling", "Dog-walking", "Nature watching",
                                       "Index", "Shooting", "Non-lethal disturbance"))

##convert Effect_YN to a factor for plotting and relevel

df_expanded$Effect_YN <- factor(df_expanded$Effect_YN, levels = c("Total", "Yes", "No"))

##relevel taxonomic group so multiple last

df_expanded$Taxonomic_Group <- factor(df_expanded$Taxonomic_Group, levels = c("Crane", "Duck", "Geese", "Rails",
                                                                              "Swan", "Waders", "Multiple"))


#-------------------------------------------------------------#
##Create plot - facet by Response Type and Taxonomic Group ####
#-------------------------------------------------------------#

## create a barplot of the n papers which detected an effect (Yes/No) in any species or specific responses 
## number of papers which didn't detect an effect in any species or specific responses tested
## and total number of papers for that combination
## by specific disturbance type
## facet by response type
## and taxonomic group - ducks, geese, swans, waders, crane, multiple

a <- ggplot(df_expanded, aes(x = Disturbance_Type, y = NPapers, fill = Effect_YN)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      facet_grid(rows = vars(Taxonomic_Group), cols = vars(Response_Type)) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "Disturbance Type", y = "Number of Papers", fill = "Effect") +
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

out_path <- here("Outputs", "Disturbance", "Plots")

## save plot

ggsave(plot = a, filename = "dslr_effects_plot_by_specific_disturbance_general_response_and_taxonomic_group.tiff",
       device = device,
       path = out_path ,units = units, width = 175, height = 175, dpi = dpi,   
)


## very little data for cranes or demographic studies
## remove these panels and replot

df_reduced <- df_expanded %>%
  filter(Taxonomic_Group != "Crane") %>%
  filter(Response_Type != "Demographic")

##remake plot

b <- ggplot(df_reduced, aes(x = Disturbance_Type, y = NPapers, fill = Effect_YN)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      facet_grid(rows = vars(Taxonomic_Group), cols = vars(Response_Type)) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "Disturbance Type", y = "Number of Papers", fill = "Effect") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = b, filename = "dslr_reduced_effects_plot_by_specific_disturbance_general_response_and_taxonomic_group.tiff",
       device = device,
       path = out_path ,units = units, width = 175, height = 175, dpi = dpi,   
)


## plots too complicated for manuscript
## split out and make separate plots for behavioural, physiological and site effects
## make separate data frame for behavioural effects

df_beh <- df_reduced %>%
  filter(Response_Type == "Behavioural")

##create plot

c <- ggplot(df_beh, aes(x = Disturbance_Type, y = NPapers, fill = Effect_YN)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      facet_wrap(~ Taxonomic_Group, nrow = 2, ncol = 3) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "Disturbance Type", y = "Number of Papers", fill = "Effect") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))


## save out as figure 10
## change outpath

out_path <- here("Outputs", "Disturbance", "Manuscript Figures")

## save figure

ggsave(plot = c, filename = "figure_10.tiff",
       device = device,
       path = out_path ,units = units, width = 200, height = 175, dpi = dpi,   
)

## make data frame for site-level effects

df_site <- df_reduced %>%
  filter(Response_Type == "Site") 

## create plot

d <- ggplot(df_site, aes(x = Disturbance_Type, y = NPapers, fill = Effect_YN)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      facet_wrap(~ Taxonomic_Group, nrow = 2, ncol = 3) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "Disturbance Type", y = "Number of Papers", fill = "Effect") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save figure

ggsave(plot = d, filename = "figure_11.tiff",
       device = device,
       path = out_path ,units = units, width = 200, height = 175, dpi = dpi,   
)


## make data frame for physiological effects
## also remove swans and multiple - no studies look at physiological effects in these categories

df_phy <- df_reduced %>%
  filter(Response_Type == "Physiological") %>%
  filter(Taxonomic_Group != "Swan") %>%
  filter(Taxonomic_Group != "Multiple")


## create plot

e <- ggplot(df_phy, aes(x = Disturbance_Type, y = NPapers, fill = Effect_YN)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      facet_wrap(~ Taxonomic_Group, nrow = 2, ncol = 2) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "Disturbance Type", y = "Number of Papers", fill = "Effect") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save figure

ggsave(plot = e, filename = "figure_12.tiff",
       device = device,
       path = out_path ,units = units, width = 200, height = 175, dpi = dpi,   
)

## remove plot objects

rm(a, b, c, d, e)

#------------------------------------------------------------#
##Repeat - specific response and general disturbance type ####
#------------------------------------------------------------#

## create a barplot of the n papers which detected an effect (Yes/No) in any species or specific disturbance type
## number of papers which didn't detect an effect in any species or specific disturbance type tested
## and total number of papers
## by specific disturbance response
## facet by disturbance type - wildfowling, scaring, recreational
## and taxonomic group - ducks, geese, swans, waders, crane, multiple
## need to create aggregated data frame

df_agg2 <- with(df_clean, aggregate(sum, by = list(Paper_ID, Response_Specific, Taxonomic_group, Disturbance_Type,
                                                  Effect_YN), "sum"))

names(df_agg2)[1] <- "Paper_ID"
names(df_agg2)[2] <- "Response_Type"
names(df_agg2)[3] <- "Taxonomic_Group"
names(df_agg2)[4] <- "Disturbance_Type"
names(df_agg2)[5] <- "Effect_YN"
names(df_agg2)[6] <- "NRows"

## reset sum column

df_agg2$sum <- 1

## aggregate across papers

df_effect2 <- with(df_agg2, aggregate(sum, by = list(Response_Type, Taxonomic_Group, Disturbance_Type,
                                                   Effect_YN), "sum"))

names(df_effect2)[1] <- "Response_Type"
names(df_effect2)[2] <- "Taxonomic_Group"
names(df_effect2)[3] <- "Disturbance_Type"
names(df_effect2)[4] <- "Effect_YN"
names(df_effect2)[5] <- "NPapers"

## need to aggregate for the total number of papers testing each combination of response, disturbance and TG

df_total2 <- with(df_agg2, aggregate(sum, by = list(Response_Type, Taxonomic_Group, Disturbance_Type), "sum"))

names(df_total2)[1] <- "Response_Type"
names(df_total2)[2] <- "Taxonomic_Group"
names(df_total2)[3] <- "Disturbance_Type"
names(df_total2)[4] <- "NPapers"

## create an Effect_YN column for df_total filled with "Total"

df_total2$Effect_YN <- "Total"

## merge data frames together for plotting

df_plot2 <- rbind(df_effect2, df_total2)

## add 0 values for combinations not represented in the data

df_expanded2 <- left_join(df_plot2%>%
                           expand(Disturbance_Type, Taxonomic_Group, Response_Type, Effect_YN), df_plot2)

## overwrite spelling error for Community composition

df_expanded2$Response_Type <- ifelse(df_expanded2$Response_Type == "Comminity composition", 
                                     "Community composition", paste0(df_expanded2$Response_Type))

## reorder response type variable to group in order of broader disturbance categories

levels(as.factor(df_expanded2$Response_Type))

df_expanded2$Response_Type <- factor(df_expanded2$Response_Type, levels = c("TAB", "Vigilance", "Feeding", 
                                      "Movement", "Habitat selection", "Body condition", "Heart rate", "TEB",
                                      "Local abundance", "Community composition", "Productivity (carry-over)"))


##convert Effect_YN to a factor for plotting and relevel

df_expanded2$Effect_YN <- factor(df_expanded2$Effect_YN, levels = c("Total", "Yes", "No"))

##relevel taxonomic group so multiple last

df_expanded2$Taxonomic_Group <- factor(df_expanded2$Taxonomic_Group, levels = c("Crane", "Duck", "Geese", "Rails",
                                                                              "Swan", "Waders", "Multiple"))


#-------------------------------------------------------------#
##Create plot - facet by Response Type and Taxonomic Group ####
#-------------------------------------------------------------#

## create a barplot of the n papers which detected an effect (Yes/No) in any species or specific disturbance source 
## number of papers which didn't detect an effect in any species or specific disturbance sources tested
## and total number of papers for that combination
## by specific disturbance type
## facet by response type
## and taxonomic group - ducks, geese, swans, waders, crane, multiple

a <- ggplot(df_expanded2, aes(x = Response_Type, y = NPapers, fill = Effect_YN)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      facet_grid(rows = vars(Taxonomic_Group), cols = vars(Disturbance_Type)) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "Response Type", y = "Number of Papers", fill = "Effect") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

##change outpath

out_path <- here("Outputs", "Disturbance", "Plots")

## save plot

ggsave(plot = a, filename = "dslr_effects_plot_by_specific_response_general_disturbance_and_taxonomic_group.tiff",
       device = device,
       path = out_path ,units = units, width = 175, height = 175, dpi = dpi,   
)


## very little data for cranes
## remove this panel

df_reduced2 <- df_expanded2 %>%
  filter(Taxonomic_Group != "Crane")

##remake plot

b <- ggplot(df_reduced2, aes(x = Response_Type, y = NPapers, fill = Effect_YN)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      facet_grid(rows = vars(Taxonomic_Group), cols = vars(Disturbance_Type)) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "Response Type", y = "Number of Papers", fill = "Effect") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = b, filename = "dslr_reduced_effects_plot_by_specific_response_general_disturbance_and_taxonomic_group.tiff",
       device = device,
       path = out_path ,units = units, width = 175, height = 175, dpi = dpi,   
)


## plots too complicated for manuscript
## make separate plots for recreational disturbance and non-lethal scaring
## make dataframe for recreational disturbance

df_rec <- df_reduced2 %>%
  filter(Disturbance_Type == "Recreational")

## create plot

c <- ggplot(df_rec, aes(x = Response_Type, y = NPapers, fill = Effect_YN)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      facet_wrap(~ Taxonomic_Group, nrow = 2, ncol = 3) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "Response Type", y = "Number of Papers", fill = "Effect") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))


## save out as figure 8
## change outpath

out_path <- here("Outputs", "Disturbance", "Manuscript Figures")

## save plot

ggsave(plot = c, filename = "figure_8.tiff",
       device = device,
       path = out_path ,units = units, width = 200, height = 175, dpi = dpi,   
)

## make data frame for lethal scaring
## also filter for geese - no data in opther panels

df_sca <- df_reduced2 %>%
  filter(Disturbance_Type == "Scaring")%>%
  filter(Taxonomic_Group == "Geese")

## create plot

d <- ggplot(df_sca, aes(x = Response_Type, y = NPapers, fill = Effect_YN)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      facet_wrap(~ Taxonomic_Group, nrow = 2, ncol = 3) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "Response Type", y = "Number of Papers", fill = "Effect") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save out as figure 9

ggsave(plot = d, filename = "figure_9.tiff",
       device = device,
       path = out_path ,units = units, width = 200, height = 175, dpi = dpi,   
)

## remove plot objects

rm(a, b, c, d)

#----------------------------------------------#
##Create a plot for wildfowling papers only ####
#----------------------------------------------#

## create a barplot of the n papers which detected an effect (Yes/No) in any species or specific disturbance type
## number of papers which didn't detect an effect in any species or specific disturbance type tested
## and total number of papers
## by specific disturbance response
## facet by taxonomic group - ducks, geese, swans, waders, crane, multiple
## need to create aggregated data frame
## first subset for wildfowling papers only

df_wf <- df_clean%>%
  filter(Disturbance_Type == "Wildfowling")

##aggregate data

df_agg3 <- with(df_wf, aggregate(sum, by = list(Paper_ID, Response_Specific, Taxonomic_group, Effect_YN), "sum"))

names(df_agg3)[1] <- "Paper_ID"
names(df_agg3)[2] <- "Response_Type"
names(df_agg3)[3] <- "Taxonomic_Group"
names(df_agg3)[4] <- "Effect_YN"
names(df_agg3)[5] <- "NRows"

## reset sum column

df_agg3$sum <- 1

## aggregate across papers

df_effect3 <- with(df_agg3, aggregate(sum, by = list(Response_Type, Taxonomic_Group, Effect_YN), "sum"))

names(df_effect3)[1] <- "Response_Type"
names(df_effect3)[2] <- "Taxonomic_Group"
names(df_effect3)[3] <- "Effect_YN"
names(df_effect3)[4] <- "NPapers"

## need to aggregate for the total number of papers testing each combination of response, disturbance and TG

df_total3 <- with(df_agg3, aggregate(sum, by = list(Response_Type, Taxonomic_Group), "sum"))

names(df_total3)[1] <- "Response_Type"
names(df_total3)[2] <- "Taxonomic_Group"
names(df_total3)[3] <- "NPapers"

## create an Effect_YN column for df_total filled with "Total"

df_total3$Effect_YN <- "Total"

## merge data frames together for plotting

df_plot3 <- rbind(df_effect3, df_total3)

## add 0 values for combinations not represented in the data

df_expanded3 <- left_join(df_plot3%>%
                            expand(Taxonomic_Group, Response_Type, Effect_YN), df_plot3)

##overwrite spelling error for Community composition

df_expanded3$Response_Type <- ifelse(df_expanded3$Response_Type == "Comminity composition", 
                                     "Community composition", paste0(df_expanded3$Response_Type))

## reorder response type variable to group in order of broader disturbance categories

levels(as.factor(df_expanded3$Response_Type))

df_expanded3$Response_Type <- factor(df_expanded3$Response_Type, levels = c("TAB", "Vigilance", "Feeding", 
                                                                            "Movement", "Habitat selection", "Body condition", "Heart rate", "TEB",
                                                                            "Local abundance", "Community composition", "Productivity (carry-over)"))


##convert Effect_YN to a factor for plotting and relevel

df_expanded3$Effect_YN <- factor(df_expanded3$Effect_YN, levels = c("Total", "Yes", "No"))

##relevel taxonomic group so multiple last

df_expanded3$Taxonomic_Group <- factor(df_expanded3$Taxonomic_Group, levels = c("Crane", "Duck", "Geese", "Rails",
                                                                                "Swan", "Waders", "Multiple"))


#-------------------------------------------#
##Create plot - facet by Taxonomic Group ####
#-------------------------------------------#

## create a barplot of the n papers which detected an effect (Yes/No) in any species or specific disturbance source 
## number of papers which didn't detect an effect in any species or specific disturbance sources tested
## and total number of papers for that combination
## by specific disturbance type
## facet by taxonomic group - ducks, geese, swans, waders, crane, multiple

a <- ggplot(df_expanded3, aes(x = Response_Type, y = NPapers, fill = Effect_YN)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      facet_wrap(~ Taxonomic_Group) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "Response Type", y = "Number of Papers", fill = "Effect") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## change outpath

out_path <- here("Outputs", "Disturbance", "Plots")

## save plot

ggsave(plot = a, filename = "dslr_wildfowling_effects_plot_by_specific_response_and_taxonomic_group.tiff",
       device = device,
       path = out_path ,units = units, width = 175, height = 175, dpi = dpi,   
)

## save out as figure 4
## change out path

out_path <- here("Outputs", "Disturbance", "Manuscript Figures")

## save figure

ggsave(plot = a, filename = "figure_4.tiff",
       device = device,
       path = out_path ,units = units, width = 175, height = 150, dpi = dpi,   
)


#------------------#
##End of script ####
#------------------#