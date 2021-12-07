library(pdftools)
library(tidyverse)

# function that takes a file name, loads the pdf, and does basic preprocessing to determin which lines to drop.
read_pdf <- function(fn){
  fp <- paste0('pdfs/', fn)
  app_text <- pdf_text(fp) %>%
    readr::read_lines() %>%
    str_squish() %>% 
    str_remove("[.]+$") %>%
    str_remove("[_]+$") %>%
    as_tibble() %>%
    select(app_text = value) %>%
    mutate(across(everything(), as.character))

  # read in meta data to get info on pdf
  md <- read.csv('data/metadata.csv')
  md_info <- md[md$file_name == fn,]
  app_text$company <- as.character(md_info$company_name)
  app_text$form <- as.character(md_info$form_name)
  app_text$year <- as.character(md_info$year)
  app_text$url <- as.character(md_info$url)
  
  return(app_text)
}

# function that finds all rows that have a period in the first 3 characters and capitalizes them. Otherwise, made to be lowercase
cap_text <- function(pdf_name, cap_pattern){
  pdf_name$app_text <- as.character(pdf_name$app_text)
  pdf_name$app_text <- iconv(pdf_name$app_text, "UTF-8", "ASCII", sub = "")
  pdf_name$app_text <- trimws(pdf_name$app_text, which = 'both')
  sw_index <- unlist(lapply(gregexpr(pattern =cap_pattern,pdf_name$app_text,fixed = TRUE), function(x) x[1]))
  cap_index <- which(sw_index>1 & sw_index <3)
  lw_index <- which(sw_index<=1 | sw_index >=3)
  pdf_name$app_text[cap_index] <- Hmisc::capitalize(pdf_name$app_text[cap_index])
  pdf_name$app_text[lw_index] <- tolower(pdf_name$app_text[lw_index])
  return(pdf_name)
}
