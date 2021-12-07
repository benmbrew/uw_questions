source('helpers.R')

# "165280_12052012_StateFiling IC.pdf"  
pdf1<- read_pdf(fn ="165280_12052012_StateFiling IC.pdf")
pdf1 <- pdf1[c(24:47,50:61,86:96,99:101, 146:169,196:199),]
pdf1 <- cap_text(pdf1, cap_pattern = '.')

# "Application (New Business) (IIPRC) 02.pdf"   
pdf2<- read_pdf(fn ="Application (New Business) (IIPRC) 02.pdf")
pdf2 <- pdf2[30:73,]
pdf2 <- cap_text(pdf2, cap_pattern = '.')

# "Follow up Questions AUW Life - 5.21.21.pdf"
pdf3<- read_pdf(fn ="Follow up Questions AUW Life - 5.21.21.pdf")
ind1 <- which(grepl(']', pdf3$app_text, fixed = TRUE))
ind2 <- which(grepl('Top', pdf3$app_text))
ind3 <- which(grepl('Follow', pdf3$app_text))
ind <- Reduce(union,list(ind1, ind2, ind3))
pdf3 <- pdf3[sort(ind),]
pdf3$app_text <- gsub('[', '', pdf3$app_text, fixed = T)
pdf3$app_text <- gsub(']', '', pdf3$app_text, fixed = T)


# "ICC 16 1007857 IAT ICC App Final 20160426 Bracketed 20160428.pdf"    
pdf4<- read_pdf(fn ="ICC 16 1007857 IAT ICC App Final 20160426 Bracketed 20160428.pdf")
pdf4 <- pdf4[c(99:105,110:147, 151:190, 194:197),]
pdf4 <- cap_text(pdf4, cap_pattern = '.')

# "ICC10-1.1.pdf" 
pdf5<- read_pdf(fn ="ICC10-1.1.pdf")
pdf5 <- pdf5[c(7:81),]
pdf5 <- cap_text(pdf5, cap_pattern = '.')


# "ICC10-L18.3.pdf"
pdf6<- read_pdf(fn ="ICC10-L18.3.pdf")
pdf6 <- pdf6[c(5:39),]
pdf6 <- cap_text(pdf6, cap_pattern = '.')

# "ICC10-L18.4.pdf" 
pdf7<- read_pdf(fn ="ICC10-L18.4.pdf")
pdf7 <- pdf7[c(6:49),]
pdf7 <- cap_text(pdf7, cap_pattern = '.')

# "ICC12 I-Term App R912 (Generic) (IIPRC).pdf" 
pdf8<- read_pdf(fn ="ICC12 I-Term App R912 (Generic) (IIPRC).pdf")
pdf8 <- pdf8[c(107:137),]
pdf8 <- cap_text(pdf8, cap_pattern = '.')

# "ICC12 PLA-55 REV.pdf"   
pdf9 <- read_pdf(fn ="ICC12 PLA-55 REV.pdf")
pdf9 <- pdf9[c(6:55,117:169),]
pdf9 <- cap_text(pdf9, cap_pattern = '.')

# "ICC12-ALM-10-30.pdf" 
pdf10 <- read_pdf(fn ="ICC12-ALM-10-30.pdf")
pdf10 <- pdf10[c(18:30),]
pdf10 <- cap_text(pdf10, cap_pattern = '.')

# "ICC13 75-402 (6.13) rev 1 app only.pdf" 
pdf11 <- read_pdf(fn ="ICC13 75-402 (6.13) rev 1 app only.pdf")
pdf11 <- pdf11[c(36:46, 49:64),]
pdf11 <- cap_text(pdf11, cap_pattern = '.')
pdf11$app_text <- gsub('no', '', pdf11$app_text, ignore.case = T)
pdf11$app_text <- gsub('yes', '', pdf11$app_text, ignore.case = T)
# pdf11$app_text <- gsub('   ', ' ', pdf11$app_text)
pdf11$app_text <- trimws(pdf11$app_text, which = 'both')

# ICC17-1504b 24263-717-WA w brackets.pdf
pdf12 <- read_pdf(fn ="ICC17-1504b 24263-717-WA w brackets.pdf")
pdf12 <- pdf12[c(50:55),]
pdf12 <- cap_text(pdf12, cap_pattern = '.')

# SIT APP v20 COMPACT FILING.pdf
pdf13 <- read_pdf(fn ="SIT APP v20 COMPACT FILING.pdf")
pdf13 <- pdf13[c(16:47),]
pdf13 <- cap_text(pdf13, cap_pattern = '.')

# ICC20 ATSI 11-20_112020.pdf
pdf14 <- read_pdf(fn ="ICC20 ATSI 11-20_112020.pdf")
pdf14 <- pdf14[c(4:29, 32:87),]
pdf14 <- cap_text(pdf14, cap_pattern = '.')

# TERM Life Enrollment Application Anthem Life Insurance Company 2018 - 8-27-18.pdf
pdf15 <- read_pdf(fn ="TERM Life Enrollment Application Anthem Life Insurance Company 2018 - 8-27-18.pdf")
pdf15 <- pdf15[c(156:171),]
pdf15 <- cap_text(pdf15, cap_pattern = '.')

# Revised - Application Part 2 ICC07-AXALAPP2    1-22- 08.pdf
pdf16 <- read_pdf(fn ="Revised - Application Part 2 ICC07-AXALAPP2    1-22- 08.pdf")
pdf16 <- pdf16[pdf16$app_text !='',]
pdf16 <- pdf16[c(4:25, 27:45, 47:60),]
pdf16 <- cap_text(pdf16, cap_pattern = '.')

# "ICC13-CSITL-RE 2013-5-9.pdf"   
pdf17 <- read_pdf(fn ="ICC13-CSITL-RE 2013-5-9.pdf")
pdf17 <- pdf17[c(28:77, 81:85),]
pdf17 <- cap_text(pdf17, cap_pattern = '.')

# "ICC13-TERMAPP-S Rev.pdf"                                                          
# "ICC13SLTLA2.pdf"                                                                  
# "ICC14-LIA (2-14)  IndividualLifeInsuranceApplication.pdf"                         
# "ICC14-LU1267 (1-14) Part2ParamedicalExam.pdf"                                     
# "ICC14-LU1323 (1-14) AddlDetailsSuppAppPartI.pdf"                                  
# "ICC14-LU1324 (1-14) AddlDetailsSuppAppPartII.pdf"                                 
# "ICC14-LU1325 (1-14) AddlDetailsSuppME PartII.pdf"                                 
# "ICC14-LU1326 (1-14) AddlDetailsSupplME PartIII.pdf"                               
# "ICC14-LU1327 (1-14) Part2MedicalExaminerReport.pdf"                               
# "ICC14-TCApp_050114_statefile.pdf"                                                 
# "ICC14A2200.pdf"                                                                   
# "ICC15-50894 ML All bill Base 12-16-15.pdf"                                        
# "ICC15-F1058E_ Electronic Life Application_150724_0909.pdf"                        
# "ICC16 L2B-1 v4 (bracketed).pdf"                                                   
# "ICC16-777 (7-13) new.pdf"                                                         
# "ICC16-E2050 - 2016-08-25.pdf"                                                     
# "ICC17 01-9072-17.pdf"                                                             
# "ICC17 TCL-LTDRAPP.pdf"      
# "ICC17-APP-2017101 Rev 00-00 (8.31.17).pdf"                                        
# "ICC17-U-ME2WLTECS17 rev 3.14.17 clean.pdf"                                        
# "ICC18-11000_bpf_Term_App_New (rev 08 2018).pdf"                                   
# "ICC18-E1041_181128_clean.pdf"                                                     
# "ICC18-VLT4A Term Life.pdf"                                                        
# "ICC18A2200 0418.pdf"                                                              
# "ICC19-0014248XX 012019.pdf"                                                       
# "ICC19-A-01212 (03-19).pdf"                                                        
# "ICC19-ULLA-IUF-0719 - Fully Underwritten.pdf"                                     
# "ICC20-E2055 - 2020-05-18_1713 clean.pdf"                                          
# "ICC20-LUM-SI-APP-001 Rev CL 2020.09.18.pdf"                                       
# "ICC20L3236B Application_20200708.pdf"                                             
# "IIPRC Form CLI-1025-IIPRC 1-12 REVISED 5-3-12.pdf"                                
# "LUMICO ICC17-LUM-SI-APP-001 Clean 8.28.17.pdf"                                    
# "OL4296 11-21-07 with JOHN DOE - PLIC.pdf"                                         
# "OL4296 11-21-07 with JOHN DOE.pdf"                                                
# "Parachute_AUW_Application Amended - 5.18.21.pdf"                                  
# "Refliexive questions 8.30.2017 v2.pdf"                                            
# "REIN Life 18 ICC 83210 John Doe v1.pdf"                                           
# "RGX02.Application 2.0.20190613.pdf"                                               

pdfs <- rbind(pdf1, pdf2, pdf3, pdf4, pdf5, pdf6, pdf7, pdf8, pdf9, pdf10,pdf11, pdf12, pdf13, pdf14, pdf15, pdf16, pdf17)
rm(pdf1, pdf2, pdf3, pdf4, pdf5, pdf6, pdf7, pdf8, pdf9, pdf10,pdf11, pdf12, pdf13, pdf14, pdf15, pdf16, pdf17, ind, ind1, ind2, ind3)
# row.names(pdfs) <- 1:nrow(pdfs)

#############################################################33
# 
# # 5_star_life_insurance_company.pdf
# pdf1 <- read_pdf(fn = '5_star_life_insurance_company.pdf')
# pdf1 <- pdf1[107:136,]
# 
# # make "ifs" lower case
# pdf1$app_text <- gsub('If', 'if',pdf1$app_text)
# 
# # "aaa_life_insurance_company.PDF"
# pdf2 <- read_pdf(fn = 'aaa_life_insurance_company.PDF')
# pdf2 <- pdf2[49:55,]
# 
# # "american_family_life_insurance_company.pdf"
# pdf3 <- read_pdf(fn = "american_family_life_insurance_company.pdf")
# pdf3 <- pdf3[30:73,]
# 
# # isolate rows that start with a question and capitalize the first letter
# pdf3 <- cap_text(pdf_name = pdf3, cap_pattern = '.')
# 
# # "american_general_life_insurance.pdf"
# pdf4 <- read_pdf(fn ='american_general_life_insurance.pdf')
# pdf4 <- pdf4[16:47,]
# pdf4 <- cap_text(pdf4, cap_pattern = '.')
# 
# # "ameritas_life_insurance_corp.pdf"
# pdf5 <- read_pdf(fn ='ameritas_life_insurance_corp.pdf')
# pdf5 <- pdf5[32:87,]
# pdf5 <- cap_text(pdf5, cap_pattern = '.')
# 
# # "amia_life_insurance_company.pdf"
# pdf6 <- read_pdf(fn ='amia_life_insurance_company.pdf')
# pdf6 <- pdf6[c(18:46,50:54),]
# pdf6 <- cap_text(pdf6, cap_pattern = '.')
# 
# # "anthem_life_insurance_company.pdf"
# pdf7 <- read_pdf(fn ='anthem_life_insurance_company.pdf')
# pdf7 <- pdf7[156:171,]
# pdf7 <- cap_text(pdf7, cap_pattern = '.')
# 
# # "axa_equitable_life_insurance_company.pdf"
# pdf8 <- read_pdf(fn ='axa_equitable_life_insurance_company.pdf')
# pdf8 <- pdf8[c(4:25, 27:45, 47:60),]
# pdf8 <- cap_text(pdf8, cap_pattern = '.')
# 
# # "banner_life_insurance_company.pdf"
# pdf9 <- read_pdf(fn ="banner_life_insurance_company.pdf")
# pdf9 <- pdf9[c(130:145,226:238, 242:270, 275:295, 365:392, 400:441, 446:497, 526:533),]
# pdf9 <- cap_text(pdf9, cap_pattern = '.')
# 
# # "cmfg_life_insurance_company.pdf"
# # pdf10 <- read_pdf(fn = 'cmfg_life_insurance_company.pdf')
# # pdf10 <- pdf10[c(41:62, 67:80, 94:96),]
# 
# # "colonial_life_and_accident_insurance_company.pdf"
# pdf11 <- read_pdf(fn = 'colonial_life_and_accident_insurance_company.pdf')
# pdf11 <- pdf11[c(30:56),]
# pdf11 <- cap_text(pdf11, cap_pattern = '.')
# 
# # "fidelity_life_association_a_legal_reserve_life_insurance_company_electronic.pdf"
# pdf12 <- read_pdf(fn = 'fidelity_life_association_a_legal_reserve_life_insurance_company_electronic.pdf')
# pdf12 <- pdf12[c(61:66, 80:168, 175:194, 206:209),]
# pdf12 <- cap_text(pdf12, cap_pattern = '.')
# 
# # "fidelity_life_association_a_legal_reserve_life_insurance_company.pdf"
# pdf13 <- read_pdf(fn = 'fidelity_life_association_a_legal_reserve_life_insurance_company.pdf')
# pdf13 <- pdf13[c(56:101, 106:134),]
# pdf13 <- cap_text(pdf13, cap_pattern = '.')
# 
# # "genworth_life_and_annuity_insurance_company.pdf"
# pdf14 <- read_pdf(fn = 'genworth_life_and_annuity_insurance_company.pdf')
# pdf14 <- pdf14[c(4:31, 36:39, 45:66),]
# pdf14 <- cap_text(pdf14, cap_pattern = '.')
# 
# # "gerber_life_insurance_company.pdf"
# pdf15 <- read_pdf(fn = 'gerber_life_insurance_company.pdf')
# pdf15 <- pdf15[c(17:29),]
# pdf15 <- cap_text(pdf15, cap_pattern = ')')
# 
# # "greenhouse_life_insurance_company.pdf"
# pdf16 <- read_pdf(fn = 'greenhouse_life_insurance_company.pdf')
# pdf16 <- pdf16[c(100:118, 123:174, 178:271, 273:285, 289:307),]
# 
# # "liberty_life_assurance_company_of_boston.pdf"
# pdf17 <- read_pdf(fn = 'liberty_life_assurance_company_of_boston.pdf')
# pdf17 <- pdf17[c(17:92 ),]
# pdf17 <- cap_text(pdf17, cap_pattern = '.')
# 
# # "lumico_life_insurance_company.pdf"
# # pdf18 <- read_pdf(fn = 'lumico_life_insurance_company.pdf')
# # pdf18 <- pdf18[c(17:92 ),]
# 
# # "massachusetts_mutual_life_insurance_company.pdf"
# pdf19 <- read_pdf(fn = 'massachusetts_mutual_life_insurance_company.pdf')
# pdf19 <- pdf19[c(30:41, 42:45, 47:58, 61:80, 83:128, 130:139),]
# pdf19 <- cap_text(pdf19, cap_pattern = '.')
# 
# # "minnesota_life_insurance_company.pdf"
# pdf20 <- read_pdf(fn = 'minnesota_life_insurance_company.pdf')
# pdf20 <- pdf20[c(16:39),]
# pdf20 <- cap_text(pdf20, cap_pattern = '.')
# 
# # "national_teachers_associates_life_insurance_company.pdf"
# pdf21 <- read_pdf(fn = 'national_teachers_associates_life_insurance_company.pdf')
# pdf21 <- pdf21[c(36:46, 48:64),]
# pdf21 <- cap_text(pdf21, cap_pattern = '.')
# 
# # remove instances of "no" "yes"
# pdf21$app_text <- gsub('no', '', pdf21$app_text, ignore.case = T)
# pdf21$app_text <- gsub('yes', '', pdf21$app_text, ignore.case = T)
# pdf21$app_text <- trimws(pdf21$app_text, which = 'both')
# 
# # "national_western_life_insurance_company.pdf"
# 
# 
# # "north_american_company_for_life_and_health_insurance.pdf"
# # 
# # "phl_variable_insurance_company.pdf"
# # 
# # "phoenix_life_insurance_company.pdf"
# # 
# # "primerica_life_insurance_company.pdf"
# # 
# # "reliastar_life_insurance_company.pdf"
# # 
# # "s_usa_life_insurancce_company_inc.pdf"
# # 
# # "securian_life_insurance_company.pdf"
# # 
# # "security_mutual_life_insurance_company_of_new_york.pdf"
# # 
# # "shelter_life_insurance_company.pdf"
# # 
# # "standard_life_and_accident_insurance_company.pdf"
# # 
# # "state_farm_life_insurance_company.pdf"
# # 
# # "the_chesapeake_life_insurance_company.pdf"
# # 
# # "the_cincinnati_life_insurance_company.pdf"
# # 
# # "the_union_labor_life_insurance_company.pdf"
# # 
# # "thrivent_financial_for_lutherans.pdf"
# # 
# # "tiaa_cref_life_insurance_company.pdf"
# # 
# # "united_of_omaha_life_insurance_company.pdf"
# # 
# # "vantis_life_insurance_company.pdf"
# # 
# # "woodmen_of_the_world_life_insurance_society.pdf"
# # 
# 
# 
