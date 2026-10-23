This repository contains an R script for the validation and filter species occurrences records from Global Biodiversity Information Facility.
The workflow follows the schema of OCCUR (Ronquillo et al. 2024 Methods in Ecology, see https://ecoinformatic.shinyapps.io/OCCUR/).

User can download data using rgbif. Find the script that you should run first in https://github.com/cRonFer/GBIF_OccurrencesDownload

Some filters may be redundant with the downloading process. Skip them if this is the case.

SUMMARY OF FILTERS AND VALIDATIONS
1. Check and filter only presences (discard absences).
2. Check Basis of Record (nature of the information collected: observations, preserved specimens, etc.)
3. Taxonomic validation.
4. Geographic validation.
5. Temporal validation (Optional by range or information included).
6. Check duplicates (= redundant information collected in multiple records).
