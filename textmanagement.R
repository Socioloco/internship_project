# Managing text file 
# load packages
library(haven)
library(dplyr)
library(stringr)
library(quanteda)


# Spltting individual articles from txt files 
splitting_function <- function(file_path){
  # Import from stellenportale ######
  # Initialize an empty list to store documents
  documents <- list()
  
  # Initialize an empty string to accumulate lines of the current document
  current_document <- ""
  
  # Read the file line by line
  lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  
  # Loop through each line
  for (line in lines) {
    # Check if the line contains a document identifier (your pattern may vary)
    if (grepl("^\\d{14}$", line)) {
      # If it's a new document, add the current_document to the documents list
      if (nchar(current_document) > 0) {
        documents <- c(documents, current_document)
      }
      # Start a new current_document
      current_document <- line
    } else {
      # Append the line to the current_document
      current_document <- paste(current_document, line, sep = "\n")
    }
  }
  
  # Add the last document (if any) to the documents list
  if (nchar(current_document) > 0) {
    documents <- c(documents, current_document)
  }
  
  # Now 'documents' contains each document as one long string
   print(rbind(documents) %>% t() %>% as.data.frame())
}

ads_stellenportale <- splitting_function("~/OneDrive - UvA/Job Advert Project/swissubase_669_8_0/669_SMM_Data_Job advertisements_v6.0.0/669_SMM_Data_stellenportale_v6.0.0.txt")
ads_unternehmen <- splitting_function("~/OneDrive - UvA/Job Advert Project/swissubase_669_8_0/669_SMM_Data_Job advertisements_v6.0.0/669_SMM_Data_unternehmenswebseiten_v6.0.0.txt")
ads_presse <- splitting_function("~/OneDrive - UvA/Job Advert Project/swissubase_669_8_0/669_SMM_Data_Job advertisements_v6.0.0/669_SMM_Data_presse_v6.0.0.txt")

ads <- rbind(ads_presse, ads_stellenportale, ads_unternehmen)

ads$documents <- as.character(ads$documents)
ads$documents <- gsub("\n", " ", ads$documents) %>%
  gsub("\\.", "", .)%>%
  gsub(":", " ", .) %>%
  gsub("-", " ", .)

ads$id <- str_extract(ads$documents,"[:digit:]{14}")

saveRDS(ads, file = "~/OneDrive - UvA/Job Advert Project/data/joined_ads.rds")










