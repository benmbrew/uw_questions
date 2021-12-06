library(pdftools)
library(tidyverse)

# TODOS - when cleaning, make sure all questions begin with capital letters
file_names <- list.files('application_pdfs/')

# function that takes a file name, loads the pdf, and does basic preprocessing to determin which lines to drop.
read_pdf <- function(fn){
  fp <- paste0('application_pdfs/', fn)
  app_text <- pdf_text(fp) %>%
    readr::read_lines() %>%
    str_squish() %>% 
    str_remove("[.]+$") %>%
    str_remove("[_]+$") %>%
    str_subset(".+")
  app_text <- as.data.frame(app_text)
  app_text$company <- unlist(lapply(strsplit(fn, '/'), function(x) x[length(x)]))
  app_text$company <- gsub('.pdf', '', app_text$company, ignore.case = TRUE)
  app_text$company <- gsub('_', " ", app_text$company, fixed =TRUE)
  return(app_text)
}

# function that finds all rows that start with "x." - that have a period in the first 3 characters and capitalizes them. Otherwise, made to be lowercase
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

# 5_star_life_insurance_company.pdf
pdf1 <- read_pdf(fn = '5_star_life_insurance_company.pdf')
pdf1 <- pdf1[107:136,]

# make "ifs" lower case
pdf1$app_text <- gsub('If', 'if',pdf1$app_text)

# "aaa_life_insurance_company.PDF"
pdf2 <- read_pdf(fn = 'aaa_life_insurance_company.PDF')
pdf2 <- pdf2[49:55,]

# "american_family_life_insurance_company.pdf"
pdf3 <- read_pdf(fn = "american_family_life_insurance_company.pdf")
pdf3 <- pdf3[30:73,]

# isolate rows that start with a question and capitalize the first letter
pdf3 <- cap_text(pdf_name = pdf3, cap_pattern = '.')

# "american_general_life_insurance.pdf"
pdf4 <- read_pdf(fn ='american_general_life_insurance.pdf')
pdf4 <- pdf4[16:47,]
pdf4 <- cap_text(pdf4, cap_pattern = '.')

# "ameritas_life_insurance_corp.pdf"
pdf5 <- read_pdf(fn ='ameritas_life_insurance_corp.pdf')
pdf5 <- pdf5[32:87,]
pdf5 <- cap_text(pdf5, cap_pattern = '.')

# "amia_life_insurance_company.pdf"
pdf6 <- read_pdf(fn ='amia_life_insurance_company.pdf')
pdf6 <- pdf6[c(18:46,50:54),]
pdf6 <- cap_text(pdf6, cap_pattern = '.')

# "anthem_life_insurance_company.pdf"
pdf7 <- read_pdf(fn ='anthem_life_insurance_company.pdf')
pdf7 <- pdf7[156:171,]
pdf7 <- cap_text(pdf7, cap_pattern = '.')

# "axa_equitable_life_insurance_company.pdf"
pdf8 <- read_pdf(fn ='axa_equitable_life_insurance_company.pdf')
pdf8 <- pdf8[c(4:25, 27:45, 47:60),]
pdf8 <- cap_text(pdf8, cap_pattern = '.')

# "banner_life_insurance_company.pdf"
pdf9 <- read_pdf(fn ="banner_life_insurance_company.pdf")
pdf9 <- pdf9[c(130:145,226:238, 242:270, 275:295, 365:392, 400:441, 446:497, 526:533),]
pdf9 <- cap_text(pdf9, cap_pattern = '.')

# "cmfg_life_insurance_company.pdf"
# pdf10 <- read_pdf(fn = 'cmfg_life_insurance_company.pdf')
# pdf10 <- pdf10[c(41:62, 67:80, 94:96),]

# "colonial_life_and_accident_insurance_company.pdf"
pdf11 <- read_pdf(fn = 'colonial_life_and_accident_insurance_company.pdf')
pdf11 <- pdf11[c(30:56),]
pdf11 <- cap_text(pdf11, cap_pattern = '.')

# "fidelity_life_association_a_legal_reserve_life_insurance_company_electronic.pdf"
pdf12 <- read_pdf(fn = 'fidelity_life_association_a_legal_reserve_life_insurance_company_electronic.pdf')
pdf12 <- pdf12[c(61:66, 80:168, 175:194, 206:209),]
pdf12 <- cap_text(pdf12, cap_pattern = '.')

# "fidelity_life_association_a_legal_reserve_life_insurance_company.pdf"
pdf13 <- read_pdf(fn = 'fidelity_life_association_a_legal_reserve_life_insurance_company.pdf')
pdf13 <- pdf13[c(56:101, 106:134),]
pdf13 <- cap_text(pdf13, cap_pattern = '.')

# "genworth_life_and_annuity_insurance_company.pdf"
pdf14 <- read_pdf(fn = 'genworth_life_and_annuity_insurance_company.pdf')
pdf14 <- pdf14[c(4:31, 36:39, 45:66),]
pdf14 <- cap_text(pdf14, cap_pattern = '.')

# "gerber_life_insurance_company.pdf"
pdf15 <- read_pdf(fn = 'gerber_life_insurance_company.pdf')
pdf15 <- pdf15[c(17:29),]
pdf15 <- cap_text(pdf15, cap_pattern = ')')

# "greenhouse_life_insurance_company.pdf"
pdf16 <- read_pdf(fn = 'greenhouse_life_insurance_company.pdf')
pdf16 <- pdf16[c(100:118, 123:174, 178:271, 273:285, 289:307),]

# "liberty_life_assurance_company_of_boston.pdf"
pdf17 <- read_pdf(fn = 'liberty_life_assurance_company_of_boston.pdf')
pdf17 <- pdf17[c(17:92 ),]
pdf17 <- cap_text(pdf17, cap_pattern = '.')

# "lumico_life_insurance_company.pdf"
# pdf18 <- read_pdf(fn = 'lumico_life_insurance_company.pdf')
# pdf18 <- pdf18[c(17:92 ),]

# "massachusetts_mutual_life_insurance_company.pdf"
# 
# "minnesota_life_insurance_company.pdf"
# 
# "national_teachers_associates_life_insurance_company.pdf"
# 
# "national_western_life_insurance_company.pdf"
# 
# "north_american_company_for_life_and_health_insurance.pdf"
# 
# "phl_variable_insurance_company.pdf"
# 
# "phoenix_life_insurance_company.pdf"
# 
# "primerica_life_insurance_company.pdf"
# 
# "reliastar_life_insurance_company.pdf"
# 
# "s_usa_life_insurancce_company_inc.pdf"
# 
# "securian_life_insurance_company.pdf"
# 
# "security_mutual_life_insurance_company_of_new_york.pdf"
# 
# "shelter_life_insurance_company.pdf"
# 
# "standard_life_and_accident_insurance_company.pdf"
# 
# "state_farm_life_insurance_company.pdf"
# 
# "the_chesapeake_life_insurance_company.pdf"
# 
# "the_cincinnati_life_insurance_company.pdf"
# 
# "the_union_labor_life_insurance_company.pdf"
# 
# "thrivent_financial_for_lutherans.pdf"
# 
# "tiaa_cref_life_insurance_company.pdf"
# 
# "united_of_omaha_life_insurance_company.pdf"
# 
# "vantis_life_insurance_company.pdf"
# 
# "woodmen_of_the_world_life_insurance_society.pdf"
# 

pdfs <- rbind(pdf1, pdf2, pdf3, pdf4, pdf5, pdf6, pdf7, pdf8, pdf9, pdf11, pdf12, pdf13, pdf14, pdf15, pdf16, pdf17)
rm(pdf1, pdf2, pdf3, pdf4, pdf5, pdf6, pdf7, pdf8, pdf9, pdf11, pdf12, pdf13, pdf14, pdf15, pdf16, pdf17)
pdfs$app_text <- as.character(pdfs$app_text)
pdfs$company <- as.character(pdfs$company)
row.names(pdfs) <- 1:nrow(pdfs)
