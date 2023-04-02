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
## Therefore, it is best if this folder only contains the raw data files
filepath <- here("Data", "Pop_trends") 

## Define common file pattern to look for
## An asterisk (*) matches any character except a forward-slash
## e.g., "*.csv" will import all files within filepath folders that end with ".csv"
filepattern <- "*.csv" 

## Let's view the file names, to check that we have the files we want & find ID position
## This will include names of sub-folders
ls_filenames <- list.files(path = filepath, recursive = TRUE, pattern = filepattern)
ls_filenames

## Find ID number from file name (excluding names of sub-folders)
## This will only work if ID numbers are the same length and position in all file names to be imported

IDstart <- 1 #start position of the ID in the filename 

IDend <- 2 #end position of the ID in the filename

## Now, let's inspect the data by reading in the top of the first data file as raw text

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

##filter out rows with NA values

df_trends_clean <- df_trends %>%
  filter(IndexEn != "NA")


#--------------------------------#
##Visualise population trends ####
#--------------------------------#

## make a test data set of diving ducks

test <- subset(df_trends_clean, df_trends_clean$Taxonomic_Group == "Diving Ducks")

## plot smoothed long term trends for England for all species
## facet grid by major taxonomic group and quarry species

a <- ggplot(test, aes(x = WeBSYear, y = SmoothedIndexEn, col = BTO_Code)) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ Quarry_Species) +
      ##facet_grid(rows = vars(Taxonomic_Group), cols = vars(Quarry_Species)) +
      scale_fill_manual(values = c("#660099", "#CC0033", "#FFCC00")) +
      labs(x = "WeBS Year", y = "Smoothed Index", fill = "Effect") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            ##panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=1, angle = 45),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

