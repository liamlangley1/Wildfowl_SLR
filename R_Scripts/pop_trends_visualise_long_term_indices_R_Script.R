####Script to visualise long-term waterbird population trends in England
####Using data from BTO 2019/20 WeBS report - Frost et al. 2021
####Liam Langley
####Date created - 31/03/2022

##load libraries

library(tidyverse)
library(ggplot2)
library(here)


#--------------------------------------------------------#
##Read in and merge trend data from different species ####
#--------------------------------------------------------#

## read in code heavily based on ExMove paper - Langley et al. 2021
## Set filepath for folder containing raw data files
## NB: this code will try to open all files matching the file pattern within this folder

filepath <- here("Data", "Pop_trends") 

## Define common file pattern to look for
## An asterisk (*) matches any character except a forward-slash
## e.g., "*.csv" will import all files within filepath folders that end with ".csv"
filepattern <- "*.csv" 

## View the file names
## This will include names of sub-folders
ls_filenames <- list.files(path = filepath, recursive = TRUE, pattern = filepattern)
ls_filenames

## Find BTO code from file name (excluding names of sub-folders)

IDstart <- 1 #start position of the ID in the filename 

IDend <- 2 #end position of the ID in the filename

## Inspect the data by reading in the top of the first data file as raw text

test <- fs::dir_ls(path = filepath, recurse = TRUE, type = "file",  glob = filepattern)[1]
read_lines(test, n_max = 5)  # change n_max to change the number of rows to read in

## number of lines at top of file to skip (e.g., if importing a text file with additional info at top)

skiplines <- 0

## By default, the below code will find column names from the first row of data: colnames <- TRUE

colnames <- TRUE

## Set delimiter to use within read_delim

user_delim <- ","
user_trim_ws <- TRUE # Should leading and trailing whitespace (ASCII spaces and tabs) be trimmed from each field before parsing it?

## Read in and merge camera data files

df_combined <- fs::dir_ls(path = filepath, glob = filepattern, #use our defined filepath and pattern
                          type = "file",  recurse = TRUE) %>% # recurse = T searches all sub-folders
  purrr::set_names(nm = basename(.)) %>% # removing path prefix (makes filename more manageable)
  purrr::map_dfr(read_delim, .id="filename", #read all the files in using filename as ID column
                 col_types = cols(.default = "c"), col_names = colnames, 
                 skip = skiplines, delim = user_delim, trim_ws = user_trim_ws,
                 skip_empty_rows = TRUE) %>% 
  mutate(BTO_Code = str_sub(string = filename, start = IDstart, end = IDend), #substring BTO code from the filename (start to end of substring)
         .after = filename) #position the new ID column after filename column
df_combined

##check column names

colnames(df_combined)

#------------------------#
##Merge with metadata ####
#------------------------#

## set file path to metadata

filepath_meta <- here("Data","Pop_trends_metadata.csv")

## Read in metadata file
df_metadata <- readr::read_csv(filepath_meta)
names(df_metadata)

## Merge metadata with raw trends data using BTO_Code column
df_trends <- df_combined %>%
  left_join(., df_metadata, by = "BTO_Code") 
head(df_trends)

## make new quarry species column

df_trends$Quarry_Species <- ifelse(df_trends$BASC_Quarry_Species == "Yes", "Quarry", "Non-Quarry")

##convert trends and smoothed trends to numeric

df_trends$IndexEn <- as.numeric(as.vector(df_trends$IndexEn))

df_trends$SmoothedIndexEn <- as.numeric(as.vector(df_trends$SmoothedIndexEn))

df_trends$WeBSYear <- as.numeric(as.vector(df_trends$WeBSYear))

## filter out rows with NA values
## filter out years before 1995 - last 25 years
## remove Goosander - gets culled so not appropriate non-target comparison
## remove shelduck - not a true dabbling duck

df_trends_clean <- df_trends %>%
  filter(IndexEn != "NA") %>%
  filter(WeBSYear >= 1995) %>%
  filter(BTO_Code != "GD") %>%
  filter(BTO_Code != "SU")

## some plots hard to interpret
## indices are relative to the population
## rare species with small populations have larger index values
## influences scale
## rescale smoothed indices using a Z-transformation
## plot them on a similar scale
## need to group by species to do this separately for each

df_trends_clean <- df_trends_clean %>%
  group_by(BTO_Code) %>%
  mutate(ScaledIndexEn = scale(SmoothedIndexEn, center = TRUE, scale = TRUE)) %>%
  ungroup()

#--------------------------------#
##Visualise population trends ####
#--------------------------------#

## make data frames for each taxonomic group

df_dab <- df_trends_clean %>%
  filter(df_trends_clean$Taxonomic_Group == "Dabbling Ducks")

df_dive <- df_trends_clean %>%
  filter(df_trends_clean$Taxonomic_Group == "Diving Ducks")

df_geese <- df_trends_clean %>%
  filter(df_trends_clean$Taxonomic_Group == "Geese")

df_swans <- df_trends_clean %>%
  filter(df_trends_clean$Taxonomic_Group == "Swans")

df_rails <- df_trends_clean %>%
  filter(df_trends_clean$Taxonomic_Group == "Rails")

df_large_waders <- df_trends_clean %>%
  filter(df_trends_clean$Taxonomic_Group == "Large Waders")

df_small_waders <- df_trends_clean %>%
  filter(df_trends_clean$Taxonomic_Group == "Small Waders")

df_crane <- df_trends_clean %>%
  filter(df_trends_clean$Taxonomic_Group == "Crane")


## plot smoothed long term trends for England for all species
## separately for each taxonomic group
## facet by BASC quarry vs. non-quarry
## make plot for dabbling ducks

a <- ggplot(df_dab, aes(x = WeBSYear, y = SmoothedIndexEn, col = Common_Name)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Smoothed Index", col = "Species") +
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

out_path <- here("Outputs", "Pop_trends", "Plots")

## save plot

ggsave(plot = a, filename = "dabbling_duck_trends_en.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


##make plot for diving ducks

b <- ggplot(df_dive, aes(x = WeBSYear, y = SmoothedIndexEn, col = Common_Name)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Smoothed Index", col = "Species") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = b, filename = "diving_duck_trends_en.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


##make plot for geese

c <- ggplot(df_geese, aes(x = WeBSYear, y = SmoothedIndexEn, col = Common_Name)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Smoothed Index", col = "Species") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = c, filename = "goose_trends_en.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


## create updated goose plot
## subset to remove bean geese
## also remove sub-populations 
## just plot single overall trend for each species
## list of BTO codes to remove

Remove <- c("JA", "JE", "QS", "QN", "YE", "BE", "XF", "EW")

## filter data

df_geese_clean <- df_geese %>%
  filter(!BTO_Code %in% Remove)

## create updated plot

d <- ggplot(df_geese_clean, aes(x = WeBSYear, y = SmoothedIndexEn, col = Common_Name)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Smoothed Index", col = "Species") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = d, filename = "goose_trends_cleaned_en.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


##make plot for swans

e <- ggplot(df_swans, aes(x = WeBSYear, y = SmoothedIndexEn, col = Common_Name)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Smoothed Index", col = "Species") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = e, filename = "swan_trends_en.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


## make plot for rails

f <- ggplot(df_rails, aes(x = WeBSYear, y = SmoothedIndexEn, col = Common_Name)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Smoothed Index", col = "Species") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = f, filename = "rail_trends_en.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


## make plot for large waders

g <- ggplot(df_large_waders, aes(x = WeBSYear, y = SmoothedIndexEn, col = Common_Name)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Smoothed Index", col = "Species") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = g, filename = "large_wader_trends_en.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


##make plot for large waders

h <- ggplot(df_small_waders, aes(x = WeBSYear, y = SmoothedIndexEn, col = Common_Name)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Smoothed Index", col = "Species") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = h, filename = "small_wader_trends_en.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


## plot trend for crane

i <- ggplot(df_crane, aes(x = WeBSYear, y = SmoothedIndexEn, col = Common_Name)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Smoothed Index", col = "Species") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = i, filename = "crane_trend_en.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


## remove plot objects

rm(a, b, c, d, e, f, g, h, i)


#-----------------------------------#
## Make plots with scaled indices####
#-----------------------------------#

## problem - rare species have large index values
## plot scaled values - z-transformed indices 
## z-transformation performed on each species separately
## using the scale() function
## plot scaled trends for England for all species
## separately for each taxonomic group
## facet by BASC quarry vs. non-quarry
## make plot for dabbling ducks

a <- ggplot(df_dab, aes(x = WeBSYear, y = ScaledIndexEn, col = Common_Name)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Scaled Index", col = "Species") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

#### define filepath to read out plots 

out_path <- here("Outputs", "Pop_trends", "Scaled_plots")

## save plot

ggsave(plot = a, filename = "scaled_dabbling_duck_trends_en.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


## make plot for diving ducks

b <- ggplot(df_dive, aes(x = WeBSYear, y = ScaledIndexEn, col = Common_Name)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Scaled Index", col = "Species") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = b, filename = "scaled_diving_duck_trends_en.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


## make plot for geese

c <- ggplot(df_geese, aes(x = WeBSYear, y = ScaledIndexEn, col = Common_Name)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Scaled Index", col = "Species") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = c, filename = "scaled_goose_trends_en.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


## create updated goose plot
## remove most sub-populations (except brent goose)
## just plot single overall trend for each species
## list of BTO codes to remove

Remove <- c("JA", "JE", "QS", "QN", "YE", "BE", "EW")

## filter data

df_geese_clean <- df_geese %>%
  filter(!BTO_Code %in% Remove)

## create updated plot

d <- ggplot(df_geese_clean, aes(x = WeBSYear, y = ScaledIndexEn, col = Common_Name)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Scaled Index", col = "Species") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = d, filename = "scaled_goose_trends_cleaned_en.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


## make plot for swans

e <- ggplot(df_swans, aes(x = WeBSYear, y = ScaledIndexEn, col = Common_Name)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Scaled Index", col = "Species") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = e, filename = "scaled_swan_trends_en.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


## make plot for rails

f <- ggplot(df_rails, aes(x = WeBSYear, y = ScaledIndexEn, col = Common_Name)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Scaled Index", col = "Species") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = f, filename = "scaled_rail_trends_en.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


## make plot for large waders

g <- ggplot(df_large_waders, aes(x = WeBSYear, y = ScaledIndexEn, col = Common_Name)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Scaled Index", col = "Species") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = g, filename = "scaled_large_wader_trends_en.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


##make plot for small waders

h <- ggplot(df_small_waders, aes(x = WeBSYear, y = ScaledIndexEn, col = Common_Name)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Scaled Index", col = "Species") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = h, filename = "scaled_small_wader_trends_en.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


## plot trend for crane

i <- ggplot(df_crane, aes(x = WeBSYear, y = ScaledIndexEn, col = Common_Name)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Scaled Index", col = "Species") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save plot

ggsave(plot = i, filename = "scaled_crane_trend_en.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 150, dpi = dpi,   
)


#------------------#
## End of script####
#------------------#
