
#### The following script contains basic steps for clean and filter
#### biodiversity records after the downloading process.
#### Check
#### Adjust the parameters to perform your analysis.
#### Field names have to be adapted to the dataset type,
#### here it's an example of GBIF data using Darwin Core terms.

# Load packages ---------------------------------------------------------
library(data.table)  # big dataset manipulation
library(tidyverse)  # data manipulation
library(GADMTools)  # administrative units and GIS
library(CoordinateCleaner)  # data cleaning functions
library(lubridate) # dates
library(countrycode)  # country names standardization
library(bdc)
library(sf)

# Environment and data ----------------------------------------------------

rm(list = ls(all.names = TRUE))
wd <- ''  # Write working directory path
setwd(wd)

# Read occurrences csv and check dimensions
data <- data.table::fread(".csv", encoding ='UTF-8', sep="\t") 
dim(data)
head(data)

# Reduced version of the gbif dataset -------------------------------------
# Filter fields 
colnames(data)
fields <- c("gbifID", "family", "genus", "species", "infraspecificEpithet", 
             "taxonRank", "scientificName",
             "countryCode", "locality", "stateProvince", 
             "decimalLatitude",	"decimalLongitude", "coordinatePrecision", 
             "day", "month", "year", 
             "occurrenceStatus", "basisOfRecord", "recordedBy", "issue")
data <- data[ , fields, with = FALSE]
colnames(data)
rm(fields)

# Previous filters --------------------------------------------------------

# 1. Check and filter only presences (discard absences)
# RUN ONLY if you download data WITHOUT considering this argument
data <- data[occurrenceStatus == 'PRESENT', ]

# 2. Check Basis of Record field
# 3. What type of occurrences do we have?
unique(data$basisOfRecord)
# And how many in each basisOfRecord class?
data[, .N, by = basisOfRecord]

# 4. Remove records without appropriate basis of record:
data <- data[!basisOfRecord == 'FOSSIL_SPECIMEN', ]  # ! means exclude

####------------------ Taxonomic check ------------------#####

# 1. Is the record identified at a proper taxonomic rank?
# Let's see what do we have in our dataset:
unique(data$taxonRank)
# And how many of each taxonRank:
data[, .N, by = taxonRank]
# 2. Filter those records with appropriate taxonomic rank
data <- data[taxonRank =='SUBSPECIES' |
             taxonRank == 'VARIETY'|
             taxonRank == 'FORM'|
             taxonRank =='SPECIES', ]

# 3. Create a checklist with scientific names
checklist <- data[ ,c('family', 'genus', 'species', 'infraspecificEpithet', 
                      'taxonRank', 'scientificName') ]
checklist <- data.frame(unique(checklist)) # remove duplicated entries
checklist$acceptedName <- NULL # write here the accepted name
write.table(checklist, 'checklist.csv', sep="\t", row.names=FALSE) 

# Check (manually only when an affordable number of names) 
# if scientificName and species contains accepted names according to 
# the expert criteria (use references and checklist)
# If synonyms create a new column including their corresponding accepted name

data$acceptedName <- NULL ### join accepted names after the manual validation

####------------------ Geographical check -------------------------#####
# 1. Check coordinates values: ----
# Discard records with latitude/longitude values equals to zero, exact same value or NULL
data <- data[decimalLatitude!= 0 & decimalLongitude!= 0, ][decimalLatitude != decimalLongitude, ]

# 2. Check coordinates precision: ----
# Number of decimal digits of coordinates as a measure of precision 

## Function to count number of decimals:
data <- bdc::bdc_coordinates_precision(data = data, 
                                       lat ='decimalLatitude', 
                                       lon = 'decimalLongitude', 
                                       ndec = 1) # Filter coordinates with at least 1 decimal place
data <- data[.rou == 'TRUE', ]
data$.rou <- NULL


# 3. Check records that don't meet location criteria ----
# Label coordinates placed in centroids of the country
cap <- CoordinateCleaner::cc_cap (data,
                                 lon = "decimalLongitude",
                                 lat = "decimalLatitude",
                                 value = "flagged")
data <- cbind(data, cap)

# Label coordinates placed in gbif headquarters
gbif <- CoordinateCleaner::cc_gbif(data,
                                  lon = "decimalLongitude",
                                  lat = "decimalLatitude",
                                  value = "flagged")
data <- cbind(data, gbif)

# Label coordinates from museums, gardens, institutions, zoo's... 
inst <- CoordinateCleaner::cc_inst(data,
                                  lon = "decimalLongitude",
                                  lat = "decimalLatitude",
                                  value = "flagged")
data <- cbind(data, inst)

# Check records labelled as centroids 'TRUE' and filter to exclude them:
data <- data[cap == 'TRUE', ]
data <- data[inst == 'TRUE', ]
data <- data[gbif == 'TRUE', ]

# Create and save checkpoints of the dataset
saveRDS(data, 'datacheckpoint1')

# 4. Check if coordinates are placed in the assigned country ---- 
# Point in polygon by country analysis

# Import a shapefile of the study area with Adm. Units borders at country level 
# Download world shapefile from https://gadm.org/download_world.html
countries <- read_sf('.gpkg')
# Set the correct projection
st_crs(countries) <- 4326

# Transform occurrences into spatial points and project:
data$x <- data$decimalLongitude
data$y <- data$decimalLatitude

datapoints  <- st_as_sf(x = data,                         
                   coords = c("x", "y"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

# Point in polygon: join each point to a polygon based on position:
data <- st_join(datapoints, countries)

setnames(data,"NAME_0", "GADM_Location")  # rename new field

# Translate 'countryCode' information (ISO 3166-1-alpha-2 = "iso2c") 
# into country names of GADM 'countryCode'
data$countryName <- countrycode(data$countryCode, 
                                origin = "iso2c", 
                                destination = "country.name")

# Label those that didn't fall in any country polygon as 'SEA'
data$GADM_Location[is.na(data$GADM_Location)] <- "SEA"

# Check the names obtained
unique(data$countryName)
unique(data$GADM_Location)

# Make changes to match countryName and GADM-Location 
# SEE SOME EXAMPLES BELOW AND ADD THOSE NEEDED
data$countryName[data$countryName == "Czechia"] <- "Czech Republic" # Change 'Czechia'='Czech Republic'
data$countryName[data$countryName == "Svalbard & Jan Mayen"] <- 'Svalbard and Jan Mayen'
data$countryName[data$countryName == "Eswatini"] <- "Swaziland"
data$countryName[data$countryName == "St. Pierre & Miquelon"] <- "Saint Pierre and Miquelon"

# Match country names and label as 'FALSE' errors of location
data <- data %>% mutate(countryCheck = case_when(
                        GADM_Location != data$countryName ~ FALSE,
                        TRUE ~ TRUE))
# Subset and extract records located in country assigned by collector ('correct')
setDT(data)
data_gcorrect <- data[countryCheck == 'TRUE', ]

# Create and save checkpoints of the dataset
saveRDS(data_gcorrect, 'checkpoint2')


####------------------ Temporal check -------------------------#####
data_gcorrect$date <- paste(data_gcorrect$day, data_gcorrect$month, 
                            data_gcorrect$year, sep = '/')
# Filter temporal range IF needed
data_tcorrect <- data_gcorrect %>%
                        filter(year > 1980)
# OR Exclude records with no temporal information
data_tcorrect <- data_gcorrect %>%
                    filter(date == 'NA/NA/NA')

####------------------ Evaluate duplicates --------------------#####
# Here, a duplicate would correspond to 
# = species name + = coordinates
dataDuplic <- cc_dupl(data_tcorrect, 
                      species = 'acceptedName',
                      lon = 'decimalLongitude',  
                      lat = 'decimalLatitude', 
                      # additions = c('institutionCode', 'date') #add any field you may need
                      )

#### ---- SAVE RESULTS ------ ####
# dataset without redundant information
fwrite(dataDuplic, 'Data_dup.csv', sep = "\t")
# dataset with validated information
fwrite(data_tcorrect, 'Data.csv', sep='\t') 
