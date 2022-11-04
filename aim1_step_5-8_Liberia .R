# Author: Francisco Rios 
# Purpose: Set up R for prepping UW PHI Vaccination Data
# Date: Last modified September 20, 2021

# Load required packages -----
library(data.table)
library(ggplot2)
library(readxl)
library(ggrepel)
library(haven)
library(table1)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(survival)
library(vtable)
library(stringr)
library(kableExtra)
install.packages("labelled")
library(labelled)
#Added this for the replace_NA function in step 6: 
install.packages("naniar")
library(naniar)
#added this one for the ggsurvplot function in step 7
install.packages("survminer")
library(survminer)

# library(GGally)
# library(plyr)
# library(cmprsk)
# library(survminer)
# Define important variables -----

# set shared team Google drive and code repo dynamically
g_drive  <- '/Users/cnondin/Library/CloudStorage/OneDrive-SharedLibraries-UW/og_phi_global_vaccination_improvement_project - General/'
code_dir <- 'C:/Documents/PHI TA job/aim 1/git_repo/uw-phi-vax/global_vac_index/'

#setwd(code_dir) # set the working directory to wherever code is stored
#^CAROLINE: doesn't work for me lol 
raw_data_dir <- paste0(g_drive,"Data/raw_data/") # location of raw data
prepped_data_dir <- paste0(g_drive,"Data/prepped_data/") # location of prepped data
codebook_directory <- paste0(g_drive,"Data/documentation/codebooks/aim_1/") # location of codebooks for interpreting data
#^CAROLINE: added aim_1 folder 
resDir <- paste0(g_drive, "Results/") # location of  any result outputs
visDir <- paste0(g_drive,"Visualizations/") # location where visualizations are saved

# Define important files referenced in multiple sheets
outputFile02 <- paste0(prepped_data_dir, "aim_1/01_vaccine_trends.RDS")
outputFile03 <- paste0(prepped_data_dir, "aim_1/02_sdi.RDS")
outputFile05 <- paste0(prepped_data_dir, "aim_1/03_raw_extracted_dhs.RDS")
outputFile06 <- paste0(prepped_data_dir, "aim_1/04_prepped_dhs_for_mov.RDS")
outputFile08 <- paste0(prepped_data_dir, "aim_1/05_disease_trends.RDS")
outputFile09 <- paste0(prepped_data_dir, "aim_1/06_merged_data_for_visuals.RDS")
#CAROLINE: this might be an issue if the files don't exist 
# Source shared functions -----
#source(paste0(code_dir, "functions/prep_vax_trend_data.R"))
#source(paste0(code_dir, "functions/prep_dx_trend_data.R"))
#source(paste0(code_dir, "functions/strip_chars.R"), encoding = "UTF-8")
#source(paste0(code_dir, "functions/extract_dhs_data.R"), encoding = "UTF-8")
#^CAROLINE: Not working 

########## extract_dhs_data function#############
# ----------------------------------------------
# AUTHOR: Francisco Rios
# PURPOSE: Extract dhs data from stata files
# DATE: Last updated August 20 2021

extract_dhs_data <- function(dir, inFile, containing_folder, dhs_version, loc){
  
  ### TROUBLESHOOTING HELP 
  # Uncomment lines below to run tests
  
  #dir = file_dir
  #inFile = file_list$file_name[i]
  # containing_folder = file_list$containing_folder[i]
  # dhs_version = file_list$data_source[i]
  # loc = file_list$location_name[i]
  
  # Load data
  if (dhs_version %in% c('dhs7', 'dhs6')){
    dhs_data = read_dta(file=paste0(dir, inFile))
    
  } else {
    stop('Not a valid file iteration value.')
  }
  
  ###############################################################
  # subset columns into id values and variables that need tidying
  ###############################################################
  # fix white space in caseid variable
  dhs_data$caseid <- gsub('\\s+', '', dhs_data$caseid) # unfortunately the spaces in between have meaning which might result in duplicate caseids
  
  #CAROLINE: making a if statement for the dif idVars (the location variable is different for dh6 and dh7)
  if (dhs_version=="dhs6"){
    idVars = c("caseid", "v009", "v010", "v011", "v000", "v005", "v007", "v006", "v016", "scounty")
  } else if (dhs_version=="dhs7"){
    idVars = c("caseid", "v009", "v010", "v011", "v000", "v005", "v007", "v006", "v016", "scountyc")
  }
  #CAROLINE: ok I think these are all good, the ones that start with v should all be consistent
  
  # add check to make sure 
  if (sum(duplicated(dhs_data %>% select(all_of(idVars))))>0){
    stop("The id variables are not uniquely identifying all variables in the dataset.")
  }
  
  if (dhs_version=="dhs7"){
    demoVars = c("v012", "v106", "v151", "v155", "v201", "v716", "v717", "v501", "v136", "v190", "v191", "v190a", "v191a", "v025") # demographic variables
  } else if (dhs_version=="dhs6"){
    demoVars = c("v012", "v106", "v151", "v155", "v201", "v716", "v717", "v501", "v136", "v190", "v191", "v025") # slighlty different demographic variables
  }
  
  dobyearVars = names(dhs_data)[grepl('b2_', names(dhs_data)) & !grepl('sh', names(dhs_data))] # child's year of birth
  dobmonthVars = names(dhs_data)[grepl('b1_', names(dhs_data)) & !grepl('sh', names(dhs_data))] # child's month of birth
  
  if (dhs_version=="dhs7"){
    dobdayVars = names(dhs_data)[grepl('b17_', names(dhs_data))] # child's day of birth only in dhs7
  }
  
  childsexVars = names(dhs_data)[grepl('b4', names(dhs_data))] # child's sex
  childlivingVars = names(dhs_data)[grepl('b5', names(dhs_data))] # child dead or living status
  childresidVars = names(dhs_data)[grepl('b9_', names(dhs_data))] # where the child is residing
  
  if (dhs_version=="dhs7"){
    vaxcardVars = names(dhs_data)[grepl('h1a', names(dhs_data))] # indicates whether child had vaccine card
    vaxcarddateVars = names(dhs_data)[grepl('h2_|h3_|h4_|h5_|h6_|h7_|h8_|h9_|h9a_|h0_|h50_|h51_|h52_|h53_|h54_|h55_|h56_|h57_|h58_|h59_|h60_|h61_|h62_|h63_|h64_|h65_|h66_', names(dhs_data))]
    vaxyearVars = names(dhs_data)[grepl('h.*y_', names(dhs_data)) & !grepl('h12|h32|h33|h37|h40', names(dhs_data))] # year vaccines given
    vaxmonthVars = names(dhs_data)[grepl('h.*m_', names(dhs_data)) & !grepl('h12|h15|h31|h32|h33|h36|h37|h40|h80', names(dhs_data))] # months vaccine given
    vaxdayVars = names(dhs_data)[grepl('h.*d_', names(dhs_data)) & !grepl('h12|h15|h31|h32|h33|h36|h37|h40|h80', names(dhs_data))] # day vaccine given
  } else if (dhs_version=="dhs6"){
    vaxcardVars = names(dhs_data[grepl('h1_', names(dhs_data))])
    
    if (loc=="Liberia"){ #CAROLINE: changed loc to Liberia
      vaxcarddateVars = names(dhs_data)[grepl('h2_|h3_|h4_|h5_|h6_|h7_|h8_|h9_|h0_|shb1_|shb2_|shb3_|syf_', names(dhs_data))] # indicator for whether date is recorded
      vaxyearVars = names(dhs_data)[grepl('h2y_|h3y_|h4y_|h5y_|h6y_|h7y_|h8y_|h9y_|h0y_|shb1y_|shb2y_|shb3y_|syfy_', names(dhs_data))] # year vaccines given
      vaxmonthVars = names(dhs_data)[grepl('h2m_|h3m_|h4m_|h5m_|h6m_|h7m_|h8m_|h9m_|h0m_|shb1m_|shb2m_|shb3m_|syfm_', names(dhs_data))] # months vaccine given
      vaxdayVars = names(dhs_data)[grepl('h2d_|h3d_|h4d_|h5d_|h6d_|h7d_|h8d_|h9d_|h0d_|shb1d_|shb2d_|shb3d_|syfd_', names(dhs_data))] # day vaccine given
    } else {
      vaxcarddateVars = names(dhs_data)[grepl('h2_|h3_|h4_|h5_|h6_|h7_|h8_|h9_|h0_', names(dhs_data))] # indicator for whether date is recorded
      vaxyearVars = names(dhs_data)[grepl('h.*y_', names(dhs_data))] # year vaccines given
      vaxmonthVars = names(dhs_data)[grepl('h.*m_', names(dhs_data))] # months vaccine given
      vaxdayVars = names(dhs_data)[grepl('h.*d_', names(dhs_data))] # day vaccine given
    }
  }
  
  ######################
  # dob year variable
  ######################
  dobyearData <- dhs_data[, c(idVars, dobyearVars), with=F]
  
  #CAROLINE: making an if statement cuz the location variables are dif for dh6 and dh7
  if (dhs_version=="dhs7"){
    dobyearData <- dobyearData %>%
    pivot_longer(
      !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scountyc),
      names_to = c("variable", "child"),
      names_sep = "_",
      values_to = "birth_year",
      values_drop_na = FALSE
    )
  }
  else if (dhs_version=="dhs6"){
    dobyearData <- dobyearData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scounty),
        names_to = c("variable", "child"),
        names_sep = "_",
        values_to = "birth_year",
        values_drop_na = FALSE
      )
  }
  
  
  # drop extra variable column
  dobyearData <- select(dobyearData, -(variable))
  
  ######################
  # dob month variable
  ######################
  dobmonthData <- dhs_data[, c(idVars, dobmonthVars), with=F]
  
  #CAROLINE: also changed this, same reason as above 
  if (dhs_version=="dhs7"){
    dobmonthData <- dobmonthData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scountyc),
        names_to = c("variable", "child"),
        names_sep = "_",
        values_to = "birth_month",
        values_drop_na = FALSE
      )
  }
  else if (dhs_version=="dhs6"){
    dobmonthData <- dobmonthData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scounty),
        names_to = c("variable", "child"),
        names_sep = "_",
        values_to = "birth_month",
        values_drop_na = FALSE
      )
  }
  
  # drop extra variable column
  dobmonthData <- select(dobmonthData, -(variable))
  
  # only dhs7 has a child day of birth variable
  if (dhs_version=="dhs7"){
    
    ######################
    # dob day variable
    ######################
    dobdayData <- dhs_data[, c(idVars, dobdayVars), with=F]
    
    dobdayData <- dobdayData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scountyc),
        names_to = c("variable", "child"),
        names_sep = "_",
        values_to = "birth_day",
        values_drop_na = FALSE
      )
    
    # drop extra variable column
    dobdayData <- select(dobdayData, -(variable))
    
  }
  
  
  ######################
  # child sex data
  ######################
  childsexData <- dhs_data[, c(idVars, childsexVars), with=F]
  
  #CAROLINE: also changed this 
  if (dhs_version=="dhs7"){
    childsexData <- childsexData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scountyc),
        names_to = c("remove", "child"),
        names_sep = "_",
        values_to = "sex_of_child",
        values_drop_na = FALSE
      )
  }
  else if (dhs_version=="dhs6"){
    childsexData <- childsexData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scounty),
        names_to = c("remove", "child"),
        names_sep = "_",
        values_to = "sex_of_child",
        values_drop_na = FALSE
      )
  }
  
  # remove extra column
  childsexData <- select(childsexData, -(remove))
  
  ######################
  # child living status
  ######################
  childlivingData <- dhs_data[, c(idVars, childlivingVars), with=F]
  
  #CAROLINE: same deal here
  if (dhs_version=="dhs7"){
    childlivingData <- childlivingData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scountyc),
        names_to = c("remove", "child"),
        names_sep = "_",
        values_to = "is_child_alive",
        values_drop_na = FALSE
      )
  }
  else if (dhs_version=="dhs6"){
    childlivingData <- childlivingData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scounty),
        names_to = c("remove", "child"),
        names_sep = "_",
        values_to = "is_child_alive",
        values_drop_na = FALSE
      )
  }
  
  # remove extra column
  childlivingData <- select(childlivingData, -(remove))
  
  ######################
  # who child resides with
  ######################
  childresidData <- dhs_data[, c(idVars, childresidVars), with=F]
  
  #CAROLINE: also same here
  if (dhs_version=="dhs7"){
    childresidData <- childresidData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scountyc),
        names_to = c("remove", "child"),
        names_sep = "_",
        values_to = "child_resid",
        values_drop_na = FALSE
      )
  }
  else if (dhs_version=="dhs6"){
    childresidData <- childresidData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scounty),
        names_to = c("remove", "child"),
        names_sep = "_",
        values_to = "child_resid",
        values_drop_na = FALSE
      )
  }
  
  # remove extra column
  childresidData <- select(childresidData, -(remove))
  
  ######################
  # vaccine card coverage data
  ######################
  vaxcardData <- dhs_data[, c(idVars, vaxcardVars), with=F]
  
  #CAROLINE: same deal here
  if (dhs_version=="dhs7"){
    vaxcardData <- vaxcardData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scountyc),
        names_to = c("remove", "child"),
        names_sep = "_",
        values_to = "has_health_card",
        values_drop_na = FALSE
      )
  }
  else if (dhs_version=="dhs6"){
    vaxcardData <- vaxcardData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scounty),
        names_to = c("remove", "child"),
        names_sep = "_",
        values_to = "has_health_card",
        values_drop_na = FALSE
      )
  }
  
  # remove extra column
  vaxcardData <- select(vaxcardData, -(remove))
  
  ######################
  # vaccines card has date recorded (yes or no)
  ######################
  vaxcarddateData <- as_tibble(dhs_data[, c(idVars, vaxcarddateVars), with=F])
  
  #CAROLINE: same here
  if (dhs_version=="dhs7"){
    vaxcarddateData <- vaxcarddateData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scountyc),
        names_to = c("vaccine", "child"),
        names_sep = "_",
        values_to = "is_date_recorded",
        values_drop_na = FALSE
      )
  }
  else if (dhs_version=="dhs6"){
    vaxcarddateData <- vaxcarddateData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scounty),
        names_to = c("vaccine", "child"),
        names_sep = "_",
        values_to = "is_date_recorded",
        values_drop_na = FALSE
      )
  }
  
  ######################
  # vaccine year variable
  ######################
  vaxyearData <- dhs_data[, c(idVars, vaxyearVars), with=F]
  
  if (dhs_version=="dhs7"){
    vaxyearData <- vaxyearData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scountyc),
        names_to = c("vaccine", "child"),
        names_sep = "_",
        values_to = "year",
        values_drop_na = FALSE
      )
  }
  else if (dhs_version=="dhs6"){
    vaxyearData <- vaxyearData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scounty),
        names_to = c("vaccine", "child"),
        names_sep = "_",
        values_to = "year",
        values_drop_na = FALSE
      )
  }
  
  # remove suffix specifying year vaccine given
  vaxyearData <- vaxyearData %>% 
    separate(vaccine, c("vaccine", "y"), -1)
  
  # drop extra column
  vaxyearData <- select(vaxyearData, -(y))
  
  ######################
  # vaccine month variable
  ######################
  vaxmonthData <- dhs_data[, c(idVars, vaxmonthVars), with=F]
  
  if (dhs_version=="dhs7"){
    vaxmonthData <- vaxmonthData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scountyc),
        names_to = c("vaccine", "child"),
        names_sep = "_",
        values_to = "month",
        values_drop_na = FALSE
      )
  }
  else if (dhs_version=="dhs6"){
    vaxmonthData <- vaxmonthData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scounty),
        names_to = c("vaccine", "child"),
        names_sep = "_",
        values_to = "month",
        values_drop_na = FALSE
      )
  }
  
  # remove suffix specifying year vaccine given
  vaxmonthData <- vaxmonthData %>% 
    separate(vaccine, c("vaccine", "m"), -1)
  
  # drop extra column
  vaxmonthData <- select(vaxmonthData, -(m))
  
  ######################
  # vaccine day variable
  ######################
  vaxdayData <- dhs_data[, c(idVars, vaxdayVars), with=F]
  
  if (dhs_version=="dhs7"){
    vaxdayData <- vaxdayData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scountyc),
        names_to = c("vaccine", "child"),
        names_sep = "_",
        values_to = "day",
        values_drop_na = FALSE
      )
  }
  else if (dhs_version=="dhs6"){
    vaxdayData <- vaxdayData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scounty),
        names_to = c("vaccine", "child"),
        names_sep = "_",
        values_to = "day",
        values_drop_na = FALSE
      )
  }
  
  # remove suffix specifying year vaccine given
  vaxdayData <- vaxdayData %>% 
    separate(vaccine, c("vaccine", "d"), -1)
  
  # drop extra column
  vaxdayData <- select(vaxdayData, -(d))
  
  ###############################################################
  # merge and tidy data in three different dataframes:
  # 1: demographic variables
  # 2. child health card coverage
  # 3. child vaccination dates
  ###############################################################
  
  ######################### 
  ##### 1. demographic variables
  #########################
  
  # create table of mother and household demographic variables
  demoData <- as_tibble(dhs_data[, c(idVars, demoVars), with=F])
  
  # merge the mother and household demographic variables with child child characteristics
  mergeCols <- c(idVars, "child")
  dt1 <- demoData %>% 
    full_join(childsexData, by = idVars) %>%
    full_join(childlivingData, by = mergeCols) %>%
    full_join(childresidData, by=mergeCols) %>% 
    full_join(dobyearData, by = mergeCols) %>% 
    full_join(dobmonthData,  by = mergeCols) 
  
  if (dhs_version=="dhs7"){
    dt1 <- dt1 %>% 
      full_join(dobdayData, by=mergeCols)
  }
  
  ######################### 
  ##### 2. vaccine health card variables
  #########################
  
  # merge the variables related to vaccine card coverage together
  dt2 <- vaxcardData %>% full_join(vaxcarddateData, by = mergeCols)
  
  # bcg, dpt, polio, mea (measles only), hepb, pent, pneu, rota, hepb, hib
  
  if (dhs_version=="dhs7"){
    dt2 <- dt2 %>% mutate(vaccine=recode(vaccine, 
                                         `h2`="bcg",
                                         `h3`="dpt1",
                                         `h4`="pol1",
                                         `h5`="dpt2",
                                         `h6`="pol2",
                                         `h7`="dpt3",
                                         `h8`="pol3",
                                         `h9`="mea1",
                                         `h9a`="mea2",
                                         `h0`="polbirth",
                                         `h50`="hepbbirth",
                                         `h51`="pent1",
                                         `h52`="pent2",
                                         `h53`="pent3",
                                         `h54`="pneu1",
                                         `h55`="pneu2",
                                         `h56`="pneu3",
                                         `h57`="rota1",
                                         `h58`="rota2",
                                         `h59`="rota3",
                                         `h60`="ipv",
                                         `h64`="hib1",
                                         `h65`="hib2",
                                         `h66`="hib3"))
    #CAROLINE: removed hepb variables 
    
  } else if (dhs_version=="dhs6"){
    if (loc=="Liberia"){
      dt2 <- dt2 %>% mutate(vaccine=recode(vaccine,
                                           `h2`="bcg",
                                           `h3`="dpt1",
                                           `h4`="pol1",
                                           `h5`="dpt2",
                                           `h6`="pol2",
                                           `h7`="dpt3",
                                           `h8`="pol3",
                                           `h9`="mea1",
                                           `h0`="polbirth"))
      #CAROLINE: removed hepB and yellow fever 
    } else {
      dt2 <- dt2 %>% mutate(vaccine=recode(vaccine, 
                                           `h2`="bcg",
                                           `h3`="dpt1",
                                           `h4`="pol1",
                                           `h5`="dpt2",
                                           `h6`="pol2",
                                           `h7`="dpt3",
                                           `h8`="pol3",
                                           `h9`="mea1",
                                           `h0`="polbirth"))      
    }
  }
  
  # revise vaccine_date_recorded to ensure it will be tidy variable name when pivoted wider
  dt2 <- dt2 %>% mutate(
    vaccine_date_recorded = paste(vaccine, "date_recorded", sep = "_")
  )
  
  # subset column names
  if (dhs_version=="dhs7"){
    dt2 <- dt2 %>% select(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scountyc, child, has_health_card, vaccine_date_recorded, is_date_recorded)
  }
  else if (dhs_version=="dhs6"){
    dt2 <- dt2 %>% select(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scounty, child, has_health_card, vaccine_date_recorded, is_date_recorded)
  }
  
  # pivot dataset wider
  dt2 <- dt2 %>% 
    pivot_wider(names_from = vaccine_date_recorded, values_from = is_date_recorded)
  
  
  #########################
  ##### 3. child vaccines dates 
  #########################
  
  # vaccine merge columns
  vaxmergeCols <- c(idVars, "child", "vaccine")
  
  # merge vaccine dates into one variable
  dt3 <- vaxyearData %>%
    full_join(vaxmonthData, by = vaxmergeCols) %>%
    full_join(vaxdayData, by = vaxmergeCols)
  
  # recode vaccine column to actual vaccine names 
  # bcg, dpt, polio, mea (measles only), hepb, pent, pneu, rota, hepb, hib
  
  if (dhs_version=="dhs7"){
    
    dt3 <- dt3 %>% mutate(vaccine=recode(vaccine, 
                                         `h2`="bcg",
                                         `h3`="dpt1",
                                         `h4`="pol1",
                                         `h5`="dpt2",
                                         `h6`="pol2",
                                         `h7`="dpt3",
                                         `h8`="pol3",
                                         `h9`="mea1",
                                         `h9a`="mea2",
                                         `h0`="polbirth",
                                         `h50`="hepbbirth",
                                         `h51`="pent1",
                                         `h52`="pent2",
                                         `h53`="pent3",
                                         `h54`="pneu1",
                                         `h55`="pneu2",
                                         `h56`="pneu3",
                                         `h57`="rota1",
                                         `h58`="rota2",
                                         `h59`="rota3",
                                         `h60`="ipv", #CAROLINE: removed hepb variables 
                                         `h64`="hib1",
                                         `h65`="hib2",
                                         `h66`="hib3"))
  } else if (dhs_version=="dhs6"){
    if (loc=="Liberia"){
      dt3 <- dt3 %>% mutate(vaccine=recode(vaccine,
                                           `h2`="bcg",
                                           `h3`="dpt1",
                                           `h4`="pol1",
                                           `h5`="dpt2",
                                           `h6`="pol2",
                                           `h7`="dpt3",
                                           `h8`="pol3",
                                           `h9`="mea1",
                                           `h0`="polbirth" 
                                           #CAROLINE: again got rid of hepb1-3 and yelfev
      ))} else {
        dt3 <- dt3 %>% mutate(vaccine=recode(vaccine, 
                                             `h2`="bcg",
                                             `h3`="dpt1",
                                             `h4`="pol1",
                                             `h5`="dpt2",
                                             `h6`="pol2",
                                             `h7`="dpt3",
                                             `h8`="pol3",
                                             `h9`="mea1",
                                             `h0`="polbirth"))
      }
  }
  
  # calculate single vaccination date variable
  dt3 <- dt3 %>% mutate(vaxdate := make_date(month=month, day=day, year=year))
  
  # subset relevant columns
  if (dhs_version=="dhs7"){
    dt3 <- dt3 %>% select(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scountyc, vaccine, child, vaxdate)
  }
  else if (dhs_version=="dhs6"){
    dt3 <- dt3 %>% select(caseid, v009, v010, v011, v000, v005, v007, v006, v016, scounty, vaccine, child, vaxdate)
  }
  
  # pivot vaccine dates wider
  dt3 <- dt3 %>% 
    pivot_wider(names_from = vaccine, values_from = vaxdate)
  
  ###############################################################
  # merge three main datasets together
  ###############################################################
  
  # re-code child variable in first datatable (dt1) in order to facilitate merge
  dt1 <- dt1 %>% mutate(child=recode(child,
                                     `01`="1",
                                     `02`="2",
                                     `03`="3",
                                     `04`="4",
                                     `05`="5",
                                     `06`="6"))
  
  # merge into new dataset called prepped_dhs_data
  prepped_dhs_data <- dt1 %>% full_join(dt2, by = mergeCols) %>%
    full_join(dt3, by = mergeCols)
  
  ###############################################################
  # calculate new variables
  ###############################################################
  
  # if dataset doesn't contain the birth year then add in a placeholder for middle of the month
  if ("birth_day"%in%names(prepped_dhs_data)==FALSE){
    prepped_dhs_data$birth_day <- 15
  }
  
  # date interview was conducted and date of birth (dob)
  prepped_dhs_data <- prepped_dhs_data %>%
    mutate(intv_date := make_date(month=v006, day=v016, year=v007)) %>%
    mutate(dob := make_date(month=birth_month, day=birth_day, year=birth_year))
  
  ###############################################################
  # subset rows
  ###############################################################
  
  # keep only info for the six most recent births
  prepped_dhs_data <- filter(prepped_dhs_data, child %in% c("1", "2", "3", "4", "5", "6"))
  
  # remove households without any children in them
  prepped_dhs_data <- filter(prepped_dhs_data, v201!=0)
  
  # keep only info for children alive at time of interview
  prepped_dhs_data <- filter(prepped_dhs_data, is_child_alive==1)
  
  # drop rows that have NA for key columns--indicating that row is not a actual data point
  check_na <- prepped_dhs_data %>% filter(is.na(sex_of_child) & is.na(is_child_alive) & is.na(child_resid))
  check_na_sum <- as.data.table(check_na)
  check_na_sum <- check_na_sum[,.(caseid, child, sex_of_child, is_child_alive, child_resid, has_health_card)]
  check_na_sum <- melt(check_na_sum, id.vars = c('caseid', 'child', 'sex_of_child', 'is_child_alive', 'child_resid'), value.name = "has_health_card")
  check_na_sum[, has_health_card:=as.numeric(has_health_card)]
  na_budget = check_na_sum[, sum(has_health_card, na.rm = TRUE)]
  if (na_budget!=0){
    stop("Some rows with  NA for all key variables still have health cards--review drop conditions before dropping NAs in key variables")
  }
  prepped_dhs_data <- prepped_dhs_data %>% filter(!is.na(sex_of_child) & !is.na(is_child_alive) & !is.na(child_resid))
  
  # re-code the dates of birth that take place after the date of interview to be one day before the interview
  # these are an artifact resulting from selecting the middle of the month for date of birth for surveys with missing day of the month
  prepped_dhs_data <- prepped_dhs_data %>%
    mutate(dob = case_when(intv_date<dob ~ intv_date-1,
                           TRUE ~ dob))
  
  # calculate the child's age in days
  prepped_dhs_data$age_in_days <- time_length(difftime(prepped_dhs_data$intv_date, prepped_dhs_data$dob), "days")
  
  # filter out children older than three--children older than three do not have vaccination data
  prepped_dhs_data <- filter(prepped_dhs_data, age_in_days<=1095)
  
  # recode impausible/inconsistent dates to NA given that we cannot use them in this coded format
  # this includes years of birth recorded as 9997 or 9998
  if (dhs_version=="dhs7"){
    prepped_dhs_data <- prepped_dhs_data %>% 
      mutate(bcg = replace(bcg, bcg>"9000-01-01", NA)) %>%
      mutate(dpt1 = replace(dpt1, dpt1>"9000-01-01", NA)) %>%
      mutate(pol1 = replace(pol1, pol1>"9000-01-01", NA)) %>%
      mutate(dpt2 = replace(dpt2, dpt2>"9000-01-01", NA)) %>%
      mutate(dpt3 = replace(dpt3, dpt3>"9000-01-01", NA)) %>%
      mutate(pol2 = replace(pol2, pol2>"9000-01-01", NA)) %>%
      mutate(pol3 = replace(pol3, pol3>"9000-01-01", NA)) %>%
      mutate(mea1 = replace(mea1, mea1>"9000-01-01", NA)) %>%
      mutate(mea2 = replace(mea2, mea2>"9000-01-01", NA)) %>%
      mutate(polbirth = replace(polbirth, polbirth>"9000-01-01", NA)) %>%
      mutate(hepbbirth = replace(hepbbirth, hepbbirth>"9000-01-01", NA)) %>%
      mutate(pent1 = replace(pent1, pent1>"9000-01-01", NA)) %>%
      mutate(pent2 = replace(pent2, pent2>"9000-01-01", NA)) %>%
      mutate(pent3 = replace(pent3, pent3>"9000-01-01", NA)) %>%
      mutate(pneu1 = replace(pneu1, pneu1>"9000-01-01", NA)) %>%
      mutate(pneu2 = replace(pneu2, pneu2>"9000-01-01", NA)) %>%
      mutate(pneu3 = replace(pneu3, pneu3>"9000-01-01", NA)) %>%
      mutate(rota1 = replace(rota1, rota1>"9000-01-01", NA)) %>%
      mutate(rota2 = replace(rota2, rota2>"9000-01-01", NA)) %>%
      mutate(rota3 = replace(rota3, rota3>"9000-01-01", NA)) %>%
      mutate(ipv = replace(ipv, ipv>"9000-01-01", NA)) %>%
      mutate(hib1 = replace(hib1, hib1>"9000-01-01", NA)) %>%
      mutate(hib2 = replace(hib2, hib2>"9000-01-01", NA)) %>%
      mutate(hib3 = replace(hib3, hib3>"9000-01-01", NA))
    #CAROLINE: removed hepb variables 
  } else if (dhs_version=="dhs6"){
    if (loc=="Liberia"){
      prepped_dhs_data <- prepped_dhs_data %>%
        mutate(bcg = replace(bcg, bcg>"9000-01-01", NA)) %>%
        mutate(dpt1 = replace(dpt1, dpt1>"9000-01-01", NA)) %>%
        mutate(pol1 = replace(pol1, pol1>"9000-01-01", NA)) %>%
        mutate(pol2 = replace(pol2, pol2>"9000-01-01", NA)) %>%
        mutate(dpt2 = replace(dpt2, dpt2>"9000-01-01", NA)) %>%
        mutate(dpt3 = replace(dpt3, dpt3>"9000-01-01", NA)) %>%
        mutate(pol3 = replace(pol3, pol3>"9000-01-01", NA)) %>%
        mutate(mea1 = replace(mea1, mea1>"9000-01-01", NA)) %>%
        mutate(polbirth = replace(polbirth, polbirth>"9000-01-01", NA))
        #removed hepb and yellow fever for dhs6
    } else {
      prepped_dhs_data <- prepped_dhs_data %>%
        mutate(bcg = replace(bcg, bcg>"9000-01-01", NA)) %>%
        mutate(dpt1 = replace(dpt1, dpt1>"9000-01-01", NA)) %>%
        mutate(pol1 = replace(pol1, pol1>"9000-01-01", NA)) %>%
        mutate(pol2 = replace(pol2, pol2>"9000-01-01", NA)) %>%
        mutate(dpt2 = replace(dpt2, dpt2>"9000-01-01", NA)) %>%
        mutate(dpt3 = replace(dpt3, dpt3>"9000-01-01", NA)) %>%
        mutate(pol3 = replace(pol3, pol3>"9000-01-01", NA)) %>%
        mutate(mea1 = replace(mea1, mea1>"9000-01-01", NA)) %>%
        mutate(polbirth = replace(polbirth, polbirth>"9000-01-01", NA))
    }
  }
  
  # recode vaccine dates listed as before the date of birth to the date of birth to account for our middle-of-month assumption for DHS6
  if (dhs_version=="dhs7"){
    prepped_dhs_data <- prepped_dhs_data %>% 
      mutate(bcg = replace(bcg, bcg<dob, NA)) %>%
      mutate(dpt1 = replace(dpt1, dpt1<dob, NA)) %>%
      mutate(pol1 = replace(pol1, pol1<dob, NA)) %>%
      mutate(dpt2 = replace(dpt2, dpt2<dob, NA)) %>%
      mutate(dpt3 = replace(dpt3, dpt3<dob, NA)) %>%
      mutate(pol2 = replace(pol2, pol2<dob, NA)) %>%
      mutate(pol3 = replace(pol3, pol3<dob, NA)) %>%
      mutate(mea1 = replace(mea1, mea1<dob, NA)) %>%
      mutate(mea2 = replace(mea2, mea2<dob, NA)) %>%
      mutate(polbirth = replace(polbirth, polbirth<dob, NA)) %>%
      mutate(hepbbirth = replace(hepbbirth, hepbbirth<dob, NA)) %>%
      mutate(pent1 = replace(pent1, pent1<dob, NA)) %>%
      mutate(pent2 = replace(pent2, pent2<dob, NA)) %>%
      mutate(pent3 = replace(pent3, pent3<dob, NA)) %>%
      mutate(pneu1 = replace(pneu1, pneu1<dob, NA)) %>%
      mutate(pneu2 = replace(pneu2, pneu2<dob, NA)) %>%
      mutate(pneu3 = replace(pneu3, pneu3<dob, NA)) %>%
      mutate(rota1 = replace(rota1, rota1<dob, NA)) %>%
      mutate(rota2 = replace(rota2, rota2<dob, NA)) %>%
      mutate(rota3 = replace(rota3, rota3<dob, NA)) %>%
      mutate(ipv = replace(ipv, ipv<dob, NA)) %>%
      mutate(hib1 = replace(hib1, hib1<dob, NA)) %>%
      mutate(hib2 = replace(hib2, hib2<dob, NA)) %>%
      mutate(hib3 = replace(hib3, hib3<dob, NA))
  } else if (dhs_version=="dhs6"){
    if (loc=="Liberia"){
      prepped_dhs_data <- prepped_dhs_data %>%
        mutate(
          bcg = case_when(bcg<dob & !is.na(bcg) ~ dob,
                          TRUE ~ bcg),
          dpt1 = case_when(dpt1<dob & !is.na(dpt1) ~ dob,
                           TRUE ~ dpt1),
          dpt2 = case_when(dpt2<dob & !is.na(dpt2) ~ dob,
                           TRUE ~ dpt2),
          dpt3 = case_when(dpt3<dob & !is.na(dpt3) ~ dob,
                           TRUE ~ dpt3),
          polbirth = case_when(polbirth<dob & !is.na(polbirth) ~ dob,
                               TRUE ~ polbirth),
          pol1 = case_when(pol1<dob & !is.na(pol1) ~ dob,
                           TRUE ~ pol1),
          pol2 = case_when(pol2<dob & !is.na(pol2) ~ dob,
                           TRUE ~ pol2),
          pol3 = case_when(pol3<dob & !is.na(pol3) ~ dob,
                           TRUE ~ pol3),
          mea1 = case_when(mea1<dob & !is.na(mea1) ~ dob,
                           TRUE ~ mea1))
      #CAROLINE: removed hepb and yellow fev
    } else {
      prepped_dhs_data <- prepped_dhs_data %>%
        mutate(
          bcg = case_when(bcg<dob & !is.na(bcg) ~ dob,
                          TRUE ~ bcg),
          dpt1 = case_when(dpt1<dob & !is.na(dpt1) ~ dob,
                           TRUE ~ dpt1),
          dpt2 = case_when(dpt2<dob & !is.na(dpt2) ~ dob,
                           TRUE ~ dpt2),
          dpt3 = case_when(dpt3<dob & !is.na(dpt3) ~ dob,
                           TRUE ~ dpt3),
          polbirth = case_when(polbirth<dob & !is.na(polbirth) ~ dob,
                               TRUE ~ polbirth),
          pol1 = case_when(pol1<dob & !is.na(pol1) ~ dob,
                           TRUE ~ pol1),
          pol2 = case_when(pol2<dob & !is.na(pol2) ~ dob,
                           TRUE ~ pol2),
          pol3 = case_when(pol3<dob & !is.na(pol3) ~ dob,
                           TRUE ~ pol3),
          mea1 = case_when(mea1<dob & !is.na(mea1) ~ dob,
                           TRUE ~ mea1))
    }
  }
  
  ###############################################################
  # subset columns
  ###############################################################
  # remove unnecessary columns
  if (dhs_version=="dhs7"){
    prepped_dhs_data <- prepped_dhs_data %>%
      select(-c(v007, v006, v016, 
                birth_day, birth_year, birth_month))
  } else if (dhs_version=="dhs6"){
    prepped_dhs_data <- prepped_dhs_data %>%
      select(-c(v007, v006, v016, birth_day, birth_year, birth_month))
  }
  
  # reorder columns to put demographic variables first
  prepped_dhs_data <- prepped_dhs_data %>% relocate(intv_date, .after = v025) %>%
    relocate(dob, .after = child)
  
  #Check column names, and that you have at least some valid data for the file.
  if (nrow(prepped_dhs_data)==0){
    stop(paste0("All data dropped for ", inFile))
  }
  
  # return the prepped data set
  return(prepped_dhs_data)
}


########### STEP 5####################
# AUTHOR: Francisco Rios
# PURPOSE: Extract all necessary DHS data
# DATE: Last updated September 2 2021

# Read in list of files to prep
file_list <- data.table(read_excel(paste0('/Users/cnondin/Library/CloudStorage/OneDrive-SharedLibraries-UW/og_phi_global_vaccination_improvement_project - General/Data/list_of_data_used.xlsx')))

# subset files to dhs data (and only Nigeria data for now)
file_list <- file_list[data_source%in%c("dhs6", "dhs7") & location_name=="Liberia",.(file_name, data_type, data_source, year, containing_folder, location_name)]
#CAROLINE: changed name to Liberia 

for(i in 1:nrow(file_list)){
  # Set up file path 
  file_dir <- paste0(raw_data_dir, file_list$data_type[i], '/dhs/', file_list$containing_folder[i], '/' )
  
  # set up arguments
  args <- list(file_dir, file_list$file_name[i], file_list$containing_folder[i], file_list$data_source[i], file_list$location_name[i])
  
  ### RUN THE PREP FUNCTION HERE ###
  tmpData = do.call(extract_dhs_data, args)
  
  #Add indexing data
  append_cols = file_list[i, .(file_name)]
  
  stopifnot(nrow(append_cols)==1)
  
  tmpData = cbind(tmpData, append_cols)
  
  #Bind data together 
  if(i==1){
    extracted_dhs_data = tmpData
  } else {
    extracted_dhs_data = plyr::rbind.fill(extracted_dhs_data, tmpData)
  }
  print("Now prepping:")
  print(paste0(i, " ", file_list$location_name[i], " ", file_list$data_source[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
}
# save raw extracted data 
saveRDS(extracted_dhs_data, outputFile05)

# print final statement
print("Step 05: Extracting dhs vaccination data completed.")

############ STEP 6####################
# AUTHOR: Francisco Rios 
# PURPOSE: Prep of DHS data for MOV analyses
# DATE: Last updated October 14 2021

# Load data -----
dt <- as_tibble(readRDS(outputFile05))

# =====
# Define variables necessary for risk analysis: -----
# Identify youth with health card -----
dt$has_health_card_bin <- as.character(dt$has_health_card)

dt <- dt %>%
  mutate(has_health_card_bin = na_if(has_health_card_bin, 9)) %>%
  mutate(has_health_card_bin=recode(has_health_card_bin,
                                    `0`=0,                                 
                                    `1`=1,
                                    `2`=0,
                                    `3`=1,
                                    `4`=0,
                                    `5`=1,
                                    `6`=1,
                                    `7`=1,
                                    `8`=0))

dt$has_health_card_bin <- factor(dt$has_health_card_bin,
                                 levels = c(0,1),
                                 labels = c("No", "Yes"))

# Assign the MINIMUM and MAXIMUM age at which each vaccine should be given -----
# Assumption: Vaccine is due at month X. We will accept vaccines given at month X-0.5 through X+1.5.

# Example: Measles due at 9 mos is acceptable 8.5-10.5 months of age.
dt$mea1_age_due_min <- 9*30.4 - 15.2 # 258 days
dt$mea1_age_due_max <- 10*30.4 +  15.2 # 319.2 days

# DPT (or pentavalent) (3 series) (due due at 6, 10, 14 weeks)
# timeliness considered appropriate if between 6th and end of the 7th week of life (with a range of 4 days before and 7 days after)
dt$dpt1_age_due_min <- 6*7 - 4  # 38 days
dt$dpt1_age_due_max <- 7*7 + 7  # 56 days

dt$dpt2_age_due_min <- 10*7 - 4 # 66 days
dt$dpt2_age_due_max <- 11*7 + 7 # 84 days

dt$dpt3_age_due_min <- 14*7 - 4 # 94 days
dt$dpt3_age_due_max <- 15*7 + 7 # 112 days

# Calculate the age of child in days -----
dt$age_in_days <- time_length(difftime(dt$intv_date, dt$dob), "days")

# Calculate the age at which child was vaccinated with measles-containing vaccine -----
dt$age_at_mea1 <- time_length(difftime(dt$mea1, dt$dob), "days")
dt$age_at_mea2 <- time_length(difftime(dt$mea2, dt$dob), "days")

# Calculate the age at which child was vaccinated with DPT vaccines -----
dt$age_at_dpt1 <- time_length(difftime(dt$dpt1, dt$dob), "days")
dt$age_at_dpt2 <- time_length(difftime(dt$dpt2, dt$dob), "days")
dt$age_at_dpt3 <- time_length(difftime(dt$dpt3, dt$dob), "days")

# Find when the last vaccination date took place -----
dt <- dt %>% mutate(oldest_visit = pmax(bcg, dpt1, pol1, dpt2, pol2, dpt3, pent1, pent2, pent3, pneu1, pneu2, pneu3, rota1, rota2, rota3, ipv,hib1, hib2, hib3,
                                        na.rm=TRUE))

# Calculate child's age at the oldest visit -----
dt$age_at_oldest_visit = time_length(difftime(dt$oldest_visit, dt$dob), "days")

# Add indicator for the difference in days between appropriate timing and the actual age at vaccination -----
# How to interpret: 
# (1) a value of <0 for mmr_age_minus_min means that they were vaccinated too early -- all of the days lived after the end of the mmr window will be considered days at risk; 
# (2) a value of >0 for mmr_age_minus_min and <0 for mmr_age_minus_max means that they were vaccinated in the proper time window and they will accrue 0 days at risk; 
# (3) a value of >0 for mmr_age_minus_max means that they lived days at risk after appropriate vaccination 

# measles
dt$mea1_age_minus_min <- dt$age_at_mea1 - dt$mea1_age_due_min
dt$mea1_age_minus_max <- dt$age_at_mea1 - dt$mea1_age_due_max

# dpt
dt$dpt1_age_minus_min <- dt$age_at_dpt1 - dt$dpt1_age_due_min
dt$dpt1_age_minus_max <- dt$age_at_dpt1 - dt$dpt1_age_due_max

dt$dpt2_age_minus_min <- dt$age_at_dpt2 - dt$dpt2_age_due_min
dt$dpt2_age_minus_max <- dt$age_at_dpt2 - dt$dpt2_age_due_max

dt$dpt3_age_minus_min <- dt$age_at_dpt3 - dt$dpt3_age_due_min
dt$dpt3_age_minus_max <- dt$age_at_dpt3 - dt$dpt3_age_due_max

# Identify variables related to timing of the measles-containing vaccine ----
dt <- dt %>% 
  mutate(
    # vaccinated too early
    early_mea1 = case_when(mea1_age_minus_min<0 ~ 1,
                           mea1_age_minus_min>0 ~ 0),
    
    # vaccinated within appropriate time frame
    mea1_within_interval = case_when(mea1_age_minus_min>=0 & mea1_age_minus_max<=0 ~ 1,
                                     mea1_age_minus_min<0 | mea1_age_minus_max>0 ~ 0),
    
    # vaccinated but too late
    mea1_late = case_when(mea1_age_minus_max>0 ~ 1,
                          mea1_age_minus_max<0 ~ 0),
    
    #  never received vaccine (1 is never received, 0 is did receive))                
    never_got_mea1 = case_when(!is.na(age_at_mea1) & has_health_card_bin=="Yes" ~ 0,
                               is.na(age_at_mea1) & has_health_card_bin=="Yes" ~ 1), 
    
    # transpose the age at which children that did receive measles were counted
    mea1_age_at_counted_vac = case_when(mea1_age_minus_min<0 ~ age_at_mea1,
                                        mea1_age_minus_min>=0 & mea1_age_minus_max<=0 ~ age_at_mea1,
                                        mea1_age_minus_max>0 ~ age_at_mea1),
    
    # variable that indicates how much time each child was at risk (for those that were vaccinated early or late)
    mea1_days_at_risk = case_when(never_got_mea1==1 & age_in_days>=mea1_age_due_max  ~ age_in_days - mea1_age_due_max,
                                  mea1_age_minus_min<0 & age_in_days>=mea1_age_due_max ~ age_in_days - mea1_age_due_max,
                                  mea1_age_minus_min>=0 & mea1_age_minus_max<=0 ~ 0,
                                  mea1_age_minus_max>0  ~ mea1_age_minus_max))

# Calculate how many doses of DPT were received -----
dt$tot_num_dpt <- NA
for (i in 1:nrow(dt)){
  dt$tot_num_dpt[i] <- sum(!is.na(dt$dpt1[i]), !is.na(dt$dpt2[i]), !is.na(dt$dpt3[i]))
}

## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ----
## ! REVIEW ! chunk is missing from here to account for DPT dates that are too close to each other!
## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! 

# Calculate variables related to timing among the DPT-containing vaccine (pentavalent) -----
dt <- dt %>% 
  mutate(
    # calculate variable indicating if never received dpt vaccine
    never_got_dpt = case_when(is.na(age_at_dpt1) & is.na(age_at_dpt2) & is.na(age_at_dpt3) & has_health_card_bin=="Yes" ~ 1,
                              !is.na(age_at_dpt1) | !is.na(age_at_dpt2) | !is.na(age_at_dpt3) & has_health_card_bin=="Yes" ~ 0),
    
    never_got_dpt1 = case_when(is.na(age_at_dpt1) & has_health_card_bin=="Yes" ~ 1,
                               !is.na(age_at_dpt1) & has_health_card_bin=="Yes" ~ 0),
    
    never_got_dpt2 = case_when(is.na(age_at_dpt2) & has_health_card_bin=="Yes" ~ 1,
                               !is.na(age_at_dpt2) & has_health_card_bin=="Yes" ~ 0),
    
    never_got_dpt3 = case_when(is.na(age_at_dpt3) & has_health_card_bin=="Yes" ~ 1,
                               !is.na(age_at_dpt3) & has_health_card_bin=="Yes" ~0),
    
    # calculate whether they got the first dose in sixth week
    dpt_dose_6wks = case_when(age_at_dpt1>=dpt1_age_due_min & age_at_dpt1<=dpt1_age_due_max & age_in_days>= dpt3_age_due_max ~ 1,
                              never_got_dpt1==1 ~ 0),
    
    # calculate age when first dose was received
    dpt_dose_6wks_when = case_when(age_at_dpt1>=dpt1_age_due_min & age_at_dpt1<=dpt1_age_due_max & age_in_days>= dpt3_age_due_max ~ age_at_dpt1))

# Find out how old child was at the first eligible dose during 6 weeks -----
dt$first_elig_dpt_after_6wks <- NA
i <- 1
for (i in 1:nrow(dt)){
  d <-sort(c(dt$age_at_dpt1[i], dt$age_at_dpt2[i], dt$age_at_dpt3[i]))
  
  y <- which(d >= dt$dpt1_age_due_min[i])[1]
  dt$first_elig_dpt_after_6wks[i] <- d[y]
}

# Only keep newly created values if age_in_days>=dpt3_age_due_max
dt <- dt %>% 
  mutate(first_elig_dpt_after_6wks = if_else(age_in_days>=dpt3_age_due_max, 
                                             first_elig_dpt_after_6wks, NA_real_))

dt <- dt %>% mutate(
  # calculate whether they got the second dose in the tenth week
  dpt_dose_10wks = case_when(age_at_dpt2>=dpt2_age_due_min & age_at_dpt2<=dpt2_age_due_max & age_in_days>= dpt3_age_due_max ~ 1,
                             never_got_dpt2==1 ~ 0),
  
  # calculate age when second dose was received
  dpt_dose_10wks_when = case_when(age_at_dpt2>=dpt2_age_due_min & age_at_dpt2<=dpt2_age_due_max & age_in_days>= dpt3_age_due_max ~ age_at_dpt2))

# Find out how old child was at the first eligible dose during 10 weeks -----
dt$first_elig_dpt_after_10wks <- NA
i <- 1
for (i in 1:nrow(dt)){
  d <-sort(c(dt$age_at_dpt1[i], dt$age_at_dpt2[i], dt$age_at_dpt3[i]))
  
  y <- which(d >= dt$dpt2_age_due_min[i] & d!=dt$first_elig_dpt_after_6wks[i])[1]
  dt$first_elig_dpt_after_10wks[i] <- d[y]
}

# Only keep newly created values if age_in_days>=dpt3_age_due_max
dt <- dt %>% 
  mutate(first_elig_dpt_after_10wks = if_else(age_in_days>=dpt3_age_due_max, 
                                              first_elig_dpt_after_10wks, NA_real_))

dt <- dt %>% mutate(    
  # calculate whether they got the third dose in the 14th week
  dpt_dose_14wks = case_when(age_at_dpt3>=dpt3_age_due_min & age_at_dpt3<=dpt3_age_due_max & age_in_days>= dpt3_age_due_max ~ 1,
                             never_got_dpt3==1 ~ 0),
  
  # calculate age when third dose was received
  dpt_dose_14wks_when = case_when(age_at_dpt3>=dpt3_age_due_min & age_at_dpt3<=dpt3_age_due_max & age_in_days>= dpt3_age_due_max ~ age_at_dpt3))

# Find out how old child was at the first eligible dose during 14 weeks -----
dt$first_elig_dpt_after_14wks <- NA
i <- 1
for (i in 1:nrow(dt)){
  d <-sort(c(dt$age_at_dpt1[i], dt$age_at_dpt2[i], dt$age_at_dpt3[i]))
  
  y <- which(d >= dt$dpt3_age_due_min[i] & d!=dt$first_elig_dpt_after_6wks[i] & d!=dt$first_elig_dpt_after_10wks[i])[1]
  dt$first_elig_dpt_after_14wks[i] <- d[y]
}

# only keep newly created values if age_in_days>=dpt3_age_due_max
dt <- dt %>% 
  mutate(first_elig_dpt_after_14wks = if_else(age_in_days>=dpt3_age_due_max, 
                                              first_elig_dpt_after_14wks, NA_real_))

dt <- dt %>% mutate(    
  # generate indicator for correct interval
  dpt_within_interval = case_when(
    
    # kids that did not have all visits during the interval, but had 3 doses with the right spacing
    tot_num_dpt==3 & first_elig_dpt_after_14wks>=dpt3_age_due_min & first_elig_dpt_after_14wks <=dpt3_age_due_max & age_in_days>=dpt3_age_due_max ~1,
    
    # kids that have perfect adherence
    dpt_dose_6wks==1 & dpt_dose_10wks==1 & dpt_dose_14wks==1 & age_in_days>=dpt3_age_due_max ~ 1,
    
    # kids without perfect adherence 
    # dpt_dose_6wks!=1 & dpt_dose_10wks!=1 & dpt_dose_14wks!=1 ~ 0
  ),
  
  # generate indicator for vaccine 3rd dose too late
  dpt_late = case_when(first_elig_dpt_after_14wks>dpt3_age_due_max & age_in_days>=dpt3_age_due_max ~ 1),
  
  # generate indicator for age at counted vaccine
  dpt_age_at_counted_vac = case_when(
    
    # kids that have perfect adherence
    dpt_within_interval==1 ~ dpt_dose_14wks_when,
    
    # kids that did not have all visits during the interval, but had 3 doses with the right spacing
    dpt_within_interval==1 ~ first_elig_dpt_after_14wks,
    
    # third dose was too late
    dpt_late==1 ~ first_elig_dpt_after_14wks),
  
  # assign indicator for too few dpt vacccines
  too_few_elig_dpt = case_when(is.na(first_elig_dpt_after_6wks) & tot_num_dpt>0 & age_in_days>=dpt3_age_due_max & has_health_card_bin=="Yes" ~ 1,
                               is.na(first_elig_dpt_after_10wks) & tot_num_dpt>0 & age_in_days>=dpt3_age_due_max & has_health_card_bin=="Yes" ~ 1,
                               is.na(first_elig_dpt_after_14wks) & tot_num_dpt>0 & age_in_days>=dpt3_age_due_max & has_health_card_bin=="Yes" ~ 1
  ),
  
  # assign days at risk
  dpt_days_at_risk = case_when(never_got_dpt==1 & age_in_days>=dpt3_age_due_max ~ age_in_days - dpt3_age_due_max,
                               dpt_within_interval==1 ~ 0,
                               dpt_late==1 ~ first_elig_dpt_after_14wks - dpt3_age_due_max,
                               too_few_elig_dpt==1 ~ age_in_days - dpt3_age_due_max))


# Calculate ages at other vaccines  -----
dt$age_at_bcg <- time_length(difftime(dt$bcg, dt$dob), "days")
dt$age_at_dpt1 <- time_length(difftime(dt$dpt1, dt$dob), "days")
dt$age_at_pol1 <- time_length(difftime(dt$pol1, dt$dob), "days")
dt$age_at_dpt2 <- time_length(difftime(dt$dpt2, dt$dob), "days")
dt$age_at_pol2 <- time_length(difftime(dt$pol2, dt$dob), "days")
dt$age_at_dpt3 <- time_length(difftime(dt$dpt3, dt$dob), "days")
dt$age_at_pol3 <- time_length(difftime(dt$pol3, dt$dob), "days")
dt$age_at_mea2 <- time_length(difftime(dt$mea2, dt$dob), "days")
dt$age_at_polbirth <- time_length(difftime(dt$polbirth, dt$dob), "days")
dt$age_at_hepbbirth <- time_length(difftime(dt$hepbbirth, dt$dob), "days")
dt$age_at_pent1 <- time_length(difftime(dt$pent1, dt$dob), "days")
dt$age_at_pent2 <- time_length(difftime(dt$pent2, dt$dob), "days")
dt$age_at_pent3 <- time_length(difftime(dt$pent3, dt$dob), "days")
dt$age_at_pneu1 <- time_length(difftime(dt$pneu1, dt$dob), "days")
dt$age_at_pneu2 <- time_length(difftime(dt$pneu2, dt$dob), "days")
dt$age_at_pneu3 <- time_length(difftime(dt$pneu3, dt$dob), "days")
dt$age_at_rota1 <- time_length(difftime(dt$rota1, dt$dob), "days")
dt$age_at_rota2 <- time_length(difftime(dt$rota2, dt$dob), "days")
dt$age_at_rota3 <- time_length(difftime(dt$rota3, dt$dob), "days")
dt$age_at_ipv <- time_length(difftime(dt$ipv, dt$dob), "days")
dt$age_at_hib1 <- time_length(difftime(dt$hib1, dt$dob), "days")
dt$age_at_hib2 <- time_length(difftime(dt$hib2, dt$dob), "days")
dt$age_at_hib3 <- time_length(difftime(dt$hib3, dt$dob), "days")

# Find out how old child was at the earliest possible visit (using all vaccination dates except measles) -----
dt$no_mea1_mop_age <- NA
i <- 1
for (i in 1:nrow(dt)){
  
  b <- sort(c(dt$age_at_bcg[i], dt$age_at_hepbbirth[i], dt$age_at_polbirth[i], 
              dt$age_at_pol1[i], dt$age_at_dpt1[i], dt$age_at_pneu1[i], dt$age_at_rota1[i], dt$age_at_hib1[i],
              dt$age_at_pol2[i], dt$age_at_dpt2[i], dt$age_at_pneu2[i], dt$age_at_hib2[i],
              dt$age_at_ipv[i], dt$age_at_pol3[i], dt$age_at_dpt3[i], dt$age_at_pneu3[i], dt$age_at_hib3[i], 
              dt$age_at_rota2[i], dt$age_at_rota3[i], dt$age_at_mea1[i], dt$age_at_mea2[i]))
  
  y <- which(b > dt$mea1_age_due_min[i])[1]
  dt$no_mea1_mop_age[i] <- b[y]
} 

# clear no_mea1_mop_age if not never_got_mea1 and if age_in_days is < mea1_age_due_max 
dt <- dt %>% 
  mutate(no_mea1_mop_age = if_else(age_in_days>=mea1_age_due_max & never_got_mea1==1 & !is.na(mea1_days_at_risk),
                                   no_mea1_mop_age, NA_real_))

# Case 2: The Child got Mea1 late, but there was another visit for another vaccine in between the mea1 due age and actual age at Mea1 ----
# find out how old child was at the earliest possible visit (using all vaccination dates except measles)
dt$earliest_visit_btwn_mea1 <- NA
i <- 1
for (i in 1:nrow(dt)){
  
  b <- sort(c(dt$age_at_bcg[i], dt$age_at_hepbbirth[i], dt$age_at_polbirth[i], 
              dt$age_at_pol1[i], dt$age_at_dpt1[i], dt$age_at_pneu1[i], dt$age_at_rota1[i], dt$age_at_hib1[i],
              dt$age_at_pol2[i], dt$age_at_dpt2[i], dt$age_at_pneu2[i], dt$age_at_hib2[i],
              dt$age_at_ipv[i], dt$age_at_pol3[i], dt$age_at_dpt3[i], dt$age_at_pneu3[i], dt$age_at_hib3[i], 
              dt$age_at_rota2[i], dt$age_at_rota3[i], dt$age_at_mea1[i], dt$age_at_mea2[i]))
  
  y <- which(b > dt$mea1_age_due_min[i] & b < dt$mea1_age_at_counted_vac[i])[1]
  dt$earliest_visit_btwn_mea1[i] <- b[y]
} 

# only keep newly created values if mea1_late==1 and !is.na(mea1_age_at_counted_vac) and age_in_days>=mea1_age_due_max
dt <- dt %>% 
  mutate(earliest_visit_btwn_mea1 = if_else(age_in_days>=mea1_age_due_max & mea1_late==1 & !is.na(mea1_age_at_counted_vac),
                                            earliest_visit_btwn_mea1, NA_real_))

# Calculate WHO HAS A MISSED OPPORTUNITY & WHAT IS POTENTIAL COVERAGE? -----
dt <- dt %>% mutate(
  
  # variable that indicates missed opportunities
  mea1_missed_opportunity = case_when(
    
    # CASE 1: the child never got measles vaccination but had another vaccination visit during or after measles vaccine was due
    never_got_mea1==1 & age_at_oldest_visit>mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_oldest_visit) ~ 1,
    
    # replace measles missed opportunity with 1 if they had an earlier visit that they could have attended
    !is.na(earliest_visit_btwn_mea1) & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) ~ 1,
    
    # default value for missed opportunities
    age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) ~ 0
    
  ))

dt <- dt %>% mutate(
  # we want to know how old the child was when they could have gotten Mea1
  mea1_age_at_mop_vac = case_when(
    !is.na(earliest_visit_btwn_mea1) ~ earliest_visit_btwn_mea1,
    is.na(earliest_visit_btwn_mea1) ~ no_mea1_mop_age))

# Create new variables of when children could have had potential dpt doses ----
dt$potential_dpt_6wks <- NA
dt$potential_dpt_10wks <- NA
dt$potential_dpt_14wks <- NA

# for each row i in dataframe create a vector of all the ages at which vaccines were distributed
for (i in 1:nrow(dt)){
  b <- sort(c(dt$age_at_bcg[i], dt$age_at_hepbbirth[i], dt$age_at_polbirth[i], 
              dt$age_at_pol1[i], dt$age_at_dpt1[i], dt$age_at_pneu1[i], dt$age_at_rota1[i], dt$age_at_hib1[i],
              dt$age_at_pol2[i], dt$age_at_dpt2[i], dt$age_at_pneu2[i], dt$age_at_hib2[i],
              dt$age_at_ipv[i], dt$age_at_pol3[i], dt$age_at_dpt3[i], dt$age_at_pneu3[i], dt$age_at_hib3[i], 
              dt$age_at_rota2[i], dt$age_at_rota3[i], dt$age_at_mea1[i], dt$age_at_mea2[i]))
  
  x <- which(b > dt$dpt1_age_due_min[i])[1]
  dt$potential_dpt_6wks[i] <- b[x]
  
  y <- which(b > dt$dpt2_age_due_min[i] & b > dt$potential_dpt_6wks[i])[1]
  dt$potential_dpt_10wks[i] <- b[y]
  
  z <- which(b>dt$dpt3_age_due_min[i] & b > dt$potential_dpt_10wks[i])[1]
  dt$potential_dpt_14wks[i] <- b[z]
}

# only keep newly created values if age_in_days>=dpt3_age_due_max and 
dt <- dt %>% 
  mutate(potential_dpt_6wks = if_else(age_in_days>=dpt3_age_due_max,
                                      potential_dpt_6wks, NA_real_),
         potential_dpt_10wks = if_else(age_in_days>=dpt3_age_due_max,
                                       potential_dpt_10wks, NA_real_),
         potential_dpt_14wks = if_else(age_in_days>=dpt3_age_due_max,
                                       potential_dpt_14wks, NA_real_))

# Calculate missed opportunities for DPT vaccines -----
dt <- dt %>% mutate(
  
  dpt_missed_opportunity=case_when(
    
    # Case 3: The child was vaccinated for DPT late and had an earlier dose (can this be combined with Case 2?)
    dpt_late==1 & potential_dpt_14wks < first_elig_dpt_after_14wks & !is.na(first_elig_dpt_after_14wks) & !is.na(potential_dpt_14wks) & has_health_card_bin=="Yes" & age_in_days>=dpt3_age_due_max ~ 1,
    
    # Case 1: The child was never vaccinated for DPT, but had at least 3 visits 4 weeks apart during or after the DPT due ages (by dose)  
    never_got_dpt==1 & !is.na(potential_dpt_14wks) & has_health_card_bin=="Yes" & age_in_days>=dpt3_age_due_max ~ 1,
    
    # Case 2: The child WAS vaccinated for DPT, but did not have 3 doses with correct spacing by age 6 mos 
    too_few_elig_dpt==1 & !is.na(potential_dpt_14wks) & has_health_card_bin=="Yes" & age_in_days>=dpt3_age_due_max ~ 1,
    
    # default value
    age_in_days>=dpt3_age_due_max & !is.na(dpt_days_at_risk) ~ 0
  ),
  
  dpt_age_at_mop_vac = case_when(
    
    # Case 3: The child was vaccinated for DPT late and had an earlier dose (can this be combined with Case 2?)
    dpt_late==1 & potential_dpt_14wks < first_elig_dpt_after_14wks & !is.na(first_elig_dpt_after_14wks) & !is.na(potential_dpt_14wks) & has_health_card_bin=="Yes" & age_in_days>= dpt3_age_due_max ~ potential_dpt_14wks,
    
    # Case 1: The child was never vaccinated for DPT, but had at least 3 visits 4 weeks apart during or after the DPT due ages (by dose)  
    never_got_dpt==1 & !is.na(potential_dpt_14wks) & has_health_card_bin=="Yes" & age_in_days>=dpt3_age_due_max ~ potential_dpt_14wks,
    
    # Case 2: The child WAS vaccinated for DPT, but did not have 3 doses with correct spacing by age 6 mos 
    too_few_elig_dpt==1 & !is.na(potential_dpt_14wks) & has_health_card_bin=="Yes" & age_in_days>=dpt3_age_due_max ~ potential_dpt_14wks))

# COMPUTE DAYS AT RISK IF OPPORTUNITY WAS NOT MISSED -----

# measles 
dt <- dt %>% mutate(
  mea1_mop_age_minus_max = case_when(
    # if child did have MO, then we need to calculate days at risk with our new number 
    !is.na(earliest_visit_btwn_mea1) & mea1_missed_opportunity==1 ~ earliest_visit_btwn_mea1 - mea1_age_due_max))

dt <- dt %>% mutate(
  mea1_days_at_risk_mop = case_when(
    # If the kid didn't have a missed opportunity, then the days at risk stays the same
    mea1_missed_opportunity==0 ~ mea1_days_at_risk,
    
    # if they did add the new days of risk
    mea1_mop_age_minus_max>0 & age_in_days>=mea1_age_due_max & mea1_missed_opportunity==1 ~ mea1_mop_age_minus_max))


# DPT3 

# if kid has missed opportunity, calculate a new total days at risk
dt <- dt %>% mutate(
  dpt_mop_age_minus_max = case_when(
    !is.na(dpt_age_at_mop_vac) & dpt_missed_opportunity==1 ~ dpt_age_at_mop_vac - dpt3_age_due_max))

dt <- dt %>% mutate(
  dpt_days_at_risk_mop = case_when(
    # if they did have a  MO, add new days of risk
    dpt_mop_age_minus_max>0 & age_in_days>=dpt3_age_due_max & dpt_missed_opportunity==1 ~ dpt_mop_age_minus_max,
    
    # if the child didn't have a missed opportunity then the days of risk stays the same
    dpt_missed_opportunity==0 ~ dpt_days_at_risk))

# Calculate dpt missed opportunities for each individual dpt vaccine (1-3)
dt <- dt %>% mutate(
  dpt1_missed_opportunity = case_when(
    # Case 1: the child did not receive the first vaccine but had another vaccine date in that early range
    is.na(age_at_dpt1) & !is.na(potential_dpt_6wks) & age_in_days>=dpt1_age_due_max & has_health_card_bin=="Yes" ~ 1,
    
    # the child did not receive the first dose but had another visit 
    # # default value for missed opportunity
    # is.na(age_at_dpt1) & !is.na(potential_dpt_6wks) & age+
  ),
  
  dpt2_missed_opportunity = case_when(
    # the child did not receive the second vaccine but had another vaccine visit during that range
    is.na(age_at_dpt2) & !is.na(potential_dpt_10wks) & age_in_days>=dpt2_age_due_max & has_health_card_bin=="Yes" ~ 1
    
  ),
  dpt3_missed_opportunity = case_when(
    # the child did not receive the third vaccine does but hat another visit during that range
    is.na(age_at_dpt3) & !is.na(potential_dpt_14wks) & age_in_days>=dpt3_age_due_max & has_health_card_bin=="Yes" ~ 1
  )
)

# Prep variables for plotting -----

# Factor mother's education -----
dt$edu <- as.character(dt$v106)
dt <- dt %>% mutate(edu = recode(edu,
                                 `0`=0,
                                 `1`=1,
                                 `2`= 2,
                                 `3`=2))
dt$edu <- factor(dt$edu,
                 levels = c(0,1,2),
                 labels = c("No education", "Primary", "Secondary or higher"))

# Factor mother's literacy levels ----
dt$literate <- as.character(dt$v155)

dt <- dt %>% replace_with_na(replace = list(literate = 9))

dt <- dt %>% mutate(literate = recode(literate,
                                      `0`=0,
                                      `1`=1,
                                      `2`=1,
                                      `3`=9,
                                      `4`=9))

dt$literate <- factor(dt$literate,
                      levels = c(0,1),
                      labels = c("Iliterate", "Literate"))

# Factor mother's age -----
dt$wom_agecat <- dt$v012

dt <- dt %>%
  mutate(wom_agecat=case_when(
    wom_agecat %in% 15:19 ~ "1",
    wom_agecat %in% 20:34 ~ "2",
    wom_agecat %in% 35:49 ~ "3"))

dt$wom_agecat <- factor(dt$wom_agecat,
                        levels = c(1,2,3),
                        labels = c("15-19", "20-34", "35-49"))

# Factor parity -----
dt$total_children_born <- dt$v201 

dt <- dt %>% 
  mutate(total_children_born=case_when(
    total_children_born %in% 1 ~ "1",
    total_children_born %in% 2:3 ~ "2",
    total_children_born %in% 4:5 ~ "3",
    total_children_born %in% 6:20 ~ "4"
  ))

dt$total_children_born <- factor(dt$total_children_born,
                                 levels = c(1,2,3,4),
                                 labels = c("1 child", "2-3 children", "4-5 children", "6+ children"))

# Factor marital status -----
dt$marital <- as.character(dt$v501)

dt <- dt %>% mutate(marital = recode(marital,
                                     `0`=1,
                                     `1`=2,
                                     `2`=3,
                                     `3`=4,
                                     `4`=4,
                                     `5`=4))

dt$marital<- factor(dt$marital,
                    levels = c(1,2,3,4),
                    labels = c("Single", "Married", "Union", "Divorced, seperated, widowed, or other"))

# Factor mother's occupation -----
dt$wom_occ <- dt$v717

dt <- dt %>% replace_with_na(replace = list(wom_occ = 99))

dt <- dt %>% 
  mutate(wom_occ=case_when(
    wom_occ %in% 0 ~ 1,
    wom_occ %in% 1:97 ~ 2
  ))

dt$wom_occ <- factor(dt$wom_occ,
                     levels = c(1,2),
                     labels = c("Not employed", "Employed"))

# Factor household assets -----
# each survey should have either v190a or v190 for the household assets
dt$assets <- ifelse(!is.na(dt$v190a), dt$v190a, dt$v190)

dt$assets <- factor(dt$assets,
                    levels = c(1,2,3,4,5),
                    labels = c("Quintile 1", "Quintile 2", "Quintile 3","Quintile 4", "Quintile 5"))

# Rename variable for household size -----
dt$hhsize <- dt$v136

# Factor urban/rural household -----
dt$urban <- abs(dt$v025-2)

dt$urban <-factor(dt$urban,
                  levels = c(0,1),
                  labels = c("Rural household", "Urban household"))

# Factor sex of head of household -----
dt$female_head <- dt$v151

dt$female_head <- factor(dt$female_head,
                         levels = c(1,2),
                         labels = c('Male', 'Female'))

# Factor sex of child -----
dt$sex_of_child <- factor(dt$sex_of_child,
                          levels=c(1,2),
                          labels=c("Male", "Female"))

# Factors kid's age -----
dt$kid_agecat <- round(time_length(difftime(dt$intv_date, dt$dob), "years"), digits = 0)
dt$kid_agecat <- factor(dt$kid_agecat, 
                        levels = c(0,1,2,3),
                        labels = c("0 years", "1 year", "2 years", "3 years"))

# Format measles missed opportunity variable -----
dt$mea1_missed_opportunity <-factor(dt$mea1_missed_opportunity,
                                    levels=c(0,1),
                                    labels=c("No", "Yes"))

# Factor year and location of DHS survey -----
dt$strata <- dt$v000

dt$strata <- factor(dt$strata, 
                    levels = c("LB7", "LB6"),
                    labels=c("2019", "2013"))
#CAROLINE: switched the labels for Liberia 

# Factor DPT missed opportunity-----
dt$dpt_missed_opportunity <-factor(dt$dpt_missed_opportunity,
                                   levels=c(0,1),
                                   labels=c("No", "Yes"))

# CAROLINE: replacing the values in scountyc with the values in scounty (so that dhs6 and dhs7 counties have the same label)
as.numeric(dt$scountyc)
as.numeric(dt$scounty)

#########################REMOVE#####################################
#dt$scountyc[dt$scountyc== 3]<- 1
#dt$scountyc[dt$scountyc== 6]<- 2
#dt$scountyc[dt$scountyc== 45]<- 3
#dt$scountyc[dt$scountyc== 9]<- 4
#dt$scountyc[dt$scountyc== 12]<- 5
#dt$scountyc[dt$scountyc== 15]<- 6
#dt$scountyc[dt$scountyc== 18]<- 7
#dt$scountyc[dt$scountyc== 21]<- 8
#dt$scountyc[dt$scountyc== 24]<- 9
#dt$scountyc[dt$scountyc== 27]<- 10
#dt$scountyc[dt$scountyc== 30]<- 11
#dt$scountyc[dt$scountyc== 33]<- 12
#dt$scountyc[dt$scountyc== 36]<- 13
#dt$scountyc[dt$scountyc== 42]<- 14
#dt$scountyc[dt$scountyc== 39]<- 15

#combining scounty and scountyc
#dt %>%
#  mutate(scountyc = coalesce(scountyc, scounty))

#dt %>% mutate(
#  scountyc[scountyc== 3] <- 1)
#dt <- dt %>% mutate(scountyc)
#dt %>%
#  mutate(scounty = coalesce(scounty, scountyc))

#dt$state <- as.numeric(dt$scounty)
########################REMOVE############################

dt <- dt %>% mutate(scounty=recode(scounty,
                                 `1`='Bomi',
                                 `2`='Bong',
                                 `3`='Gbarpolu',
                                 `4`='Grand Bassa',
                                 `5`='Grand Cape Mount',
                                 `6`='Grand Gedeh',
                                 `7`='Grand Kru',
                                 `8`='Lofa',
                                 `9`='Margibi',
                                 `10`='Maryland',
                                 `11`='Montserrado',
                                 `12`='Nimba',
                                 `13`='River Cess',
                                 `14`='River Gee',
                                 `15`='Sinoe'))
#} else if (dt$strata =="LB7"){
dt <- dt %>% mutate(scountyc=recode(scountyc,
                                   `12`='Grand Cape Mount',
                                   `15`='Grand Gedeh',
                                   `24`='Margibi',
                                   `30`='Montserrado',
                                   `18`='Grand Kru',
                                   `3`='Bomi',
                                   `21`='Lofa',
                                   `39`='Sinoe',
                                   `9`='Grand Bassa',
                                   `36`='River Cess',
                                   `45`='Gbarpolu',
                                   `6`='Bong',
                                   `42`='River Gee',
                                   `27`='Maryland',
                                   `33`='Nimba'))
#}

#combining scounty and scountyc
dt %>%
  mutate(scountyc = coalesce(scountyc, scounty))

dt$state <- dt$scountyc

#factor dt state
dt$state <- factor(dt$state)

# Factor Zone
#dt$zone <- as.numeric(dt$sstate)
#CAROLINE: we're going to try with the current state variable and see if it works: 
dt$zone <- dt$state

dt <- dt %>% mutate(
  # create zones from subnational state
  zone = case_when(state%in%c('Grand Cape Mount', 'Bomi', 'Gbarpolu') ~ 1, # North Western
                   state%in%c('Lofa', 'Bong', 'Nimba') ~ 2, # North Central
                   state%in%c('Montserrado', 'Margibi', 'Grand Bassa') ~ 3, # South Central 
                   state%in%c('River Cess', 'Sinoe', 'Grand Gedeh') ~ 4, # South Eastern A
                   state%in%c('River Gee', 'Grand Kru', 'Maryland') ~ 5, # South Eastern B
  ))
#CAROLINE: see if this causes an issue later on 

dt$zone <- factor(dt$zone,
                  levels = c(1,2,3,4,5),
                  labels = c("North Western", "North Central", "South Central", "South Eastern A", "South Eastern B"))

# Label newly created variables -----
label(dt$sex_of_child) <- "Child's sex"
label(dt$kid_agecat) <- "Child's age (in years)"
label(dt$edu) <- "Mother's education"
label(dt$literate) <- "Literacy"
label(dt$wom_agecat) <- "Mother's age (in years)"
label(dt$total_children_born) <- "Parity"
label(dt$marital) <- "Marital status"
label(dt$wom_occ) <- "Mother's occupation"
label(dt$hhsize) <- "Household size"
label(dt$female_head) <-"Sex of head of household"
label(dt$urban) <-"Urbanicity"
label(dt$mea1_missed_opportunity) <-"Missed measles opportunity"
label(dt$assets) <-"Household assets"
label(dt$strata) <- "DHS version"
label(dt$state) <- "State"
label(dt$zone) <- "Zone"

# Only keep the variable names that are in the codebook for consistency. -----
# This should constantly be reviewed. 

codebook <- as.data.table(read_xlsx(paste0(codebook_directory, 'dhs_mov_codebook.xlsx')))


dropped_vars = names(dt)[!names(dt)%in%codebook$Variable]
if (length(dropped_vars)!=0){
  print("Some variables are being dropped because they aren't in the codebook - Review to make sure these shouldn't be in the final data.")
  print(dropped_vars)
}
dt <- dt[, names(dt)%in%codebook$Variable, with=FALSE]

# Make visuals to explore the data -----

dataVariables = unique(codebook[Category=="derived variable" & Class=="numeric" & `Possible Values`=="many possible values"]$Variable)


histograms = lapply(dataVariables, function(v){
  ggplot(dt, aes_string(x=v)) + 
    geom_histogram(bins=30) +
    facet_wrap(~strata, scales = "free")
})


# save histograms as a PDF -----
outputImage09 <- paste0(visDir, "aim_1/missed_opportunities/Liberia/01_prepped_dhs_data_histograms_Lib.pdf") 
#CAROLINE: Changed file names 

print(paste('Saving:', outputImage09)) 
pdf(outputImage09, height=5.5, width=9)
for(i in seq(length(histograms))) { 
  print(histograms[[i]])
}
dev.off()

# view summary statistics for all variables
# pdf(file = paste0(visDir, "aim_1/missed_opportunities/summary_statistics_prepped_dhs_data.pdf"))
st(dt, file=paste0(visDir, "aim_1/missed_opportunities/Liberia/02_summary_stats_prepped_dhs_data_Lib"))

# Save output -----
saveRDS(dt, outputFile06)

# Print final statement -----
print("Step 09: Prepping of DHS data for analyses complete.")

#############step 7###################
# AUTHOR: Francisco Rios Casas
# PURPOSE: Creates survival curves and calculates hazard ratios for mov immunizations
# DATE: latest version October 13, 2021

# Part I -----
# Load prepped dataset for analyses

data <- readRDS(outputFile06)

# subset data to only Liberia
data <- data %>% filter(v000 %in% c("LB6", "LB7"))
# CAROLINE: changed the labels 

############## Part II: Explore what variables are associated with a missed opportunity ######

# this function will automatically add pvalue from chisquare test or t-test to a table
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

# make two datasets one for each year in Liberia 
data1 <- data %>% filter(v000=="LB7") # 2019
data2 <- data %>% filter(v000=="LB6") # 2013
#CAROLINE: Switched labels 

table1(~  sex_of_child + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + assets + hhsize + urban + female_head | mea1_missed_opportunity, data=data1, overall=F, extra.col=list(`P-value`=pvalue), topclass="Rtable1-zebra")
table1(~  sex_of_child + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + assets + hhsize + urban + female_head | mea1_missed_opportunity, data=data2, overall=F, extra.col=list(`P-value`=pvalue), topclass="Rtable1-zebra")

# calculate the chi-square for child's age seperately since children that are too young cannot have a missed opportunity yet
tes1 <- chisq.test(table(data1$kid_agecat, data1$mea1_missed_opportunity, exclude = "0 years"))$p.value
format.pval(tes1, digits=3, eps=0.001)

tes2 <- chisq.test(table(data2$kid_agecat, data2$mea1_missed_opportunity, exclude = c("0 years")))$p.value
format.pval(tes2, digits=3, eps=0.001)

##### repeat descriptive analysis with DPT vaccine #####

test <- table1( ~ sex_of_child + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + assets + hhsize + urban + female_head | dpt_missed_opportunity, data=data1, overall=F, extra.col=list(`P-value`=pvalue), topclass="Rtable1-zebra")

table1(~  sex_of_child + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + assets + hhsize + urban + female_head | dpt_missed_opportunity, data=data2, overall=F, extra.col=list(`P-value`=pvalue), topclass="Rtable1-zebra")

# png(filename = paste0(visDir, "aim_1/missed_opportunities/nigeria_2018_dpt_mov_table1.png") )
# test
# dev.off()
# write.table(test, file = paste0(visDir, "aim_1/missed_opportunities/nigeria_2018_dpt_mov_table1.txt"), sep = ",", quote = FALSE, row.names = F)
###### Part III: Compare days to vaccination (observed and potential coverage) #####
# Comparing DHS data from 2013 and 2018

##### 1. Measles #####

######### Observed ########

# keep kids that are older than the max age of measles and with vaccination card
obsmea1dat <- data %>% filter(age_in_days>=mea1_age_due_max,
                              has_health_card_bin=="Yes")

# calculate hazard days
obsmea1dat <- obsmea1dat %>% mutate(hazard_days_mea1 = case_when(
  # For the early group, this is just the total number of days since the start of the MMR interval that the child lived
  early_mea1==1 ~ mea1_days_at_risk + (mea1_age_due_max - mea1_age_due_min),
  
  # For the late group - this is the number of days lived from start of the interval to the date of vaccination. Use the already-calculated days at risk to help.
  mea1_late==1 ~ mea1_days_at_risk + (mea1_age_due_max - mea1_age_due_min),
  
  # For those who got vaccinated on time, the number of days to vaccination is just the age at vaccination minus the age at start of the interval
  mea1_within_interval==1 ~ mea1_age_at_counted_vac - mea1_age_due_min,
  
  # For those who never got a vaccine it is their age minus minimum interval bound,
  never_got_mea1==1 ~ age_in_days - mea1_age_due_min
))

# Now we need a failure indicator -- consider failure to be getting a vaccine.
# Kids who NEVER got measles or kids who got Measles early (and not again in the interval or late) will be censored at our observation days.
# Kids who got the vaccine in the interal or late will have known observation time.
obsmea1dat <- obsmea1dat %>% mutate(gotit = case_when(
  
  # censored
  never_got_mea1==1 | early_mea1==1 ~ 0,
  
  # known observation time
  mea1_late==1 | mea1_within_interval ~ 1,
))

######### Survival curve #########

# create survival object using observed data
observed.mea1 <- Surv(time=obsmea1dat$hazard_days_mea1, event=obsmea1dat$gotit)

# use survfit function to create survival curves based on a formula
f0 <- survfit(observed.mea1 ~ strata, data = obsmea1dat)

# Plot Survival Curves
plot1 <- ggsurvplot(
  fit=f0,
  data=obsmea1dat,
  xlab="Days of observation",
  ylab="",
  title="Observed number of days to measles vaccination",
  fun="event",
  conf.int = FALSE,
  censor=FALSE,
  legend = "bottom", 
  legend.title = "Year of Liberia DHS",
  ylim = c(0,1),
  ggtheme=theme_linedraw(), 
  risk.table = FALSE)
#CAROLINE: changed labels 

# save plot
png(filename = paste0(visDir, "aim_1/missed_opportunities/measles_observed_curve_Lib.png"),
    height = 8, width = 10, units = "in", res = 300)
plot1
dev.off()
#CAROLINE: changed labels 

# find the median survival time
f0

# statistical test to see if there was a difference in survival time according to year
sd <- survdiff(observed.mea1 ~ strata, data = obsmea1dat)
sd

##### Potential coverage #####

# keep kids older that the max age of measles1 and keep if kids have a vaccine card
potmea1dat <- data %>% filter(age_in_days>=mea1_age_due_max,
                              has_health_card_bin=="Yes")

# Calculate the days at risk for the hazard analysis  -- the first part is the same as non_MOP analysis
potmea1dat <- potmea1dat %>% mutate(hazard_days_mea1 = case_when(
  
  # Replace time at risk for MOP
  mea1_missed_opportunity=="Yes" & !is.na(mea1_age_at_mop_vac) ~ mea1_age_at_mop_vac - mea1_age_due_min,
  
  # For the early group, this is just the total number of days since the start of the MMR interval that the child lived
  early_mea1==1 ~ mea1_days_at_risk + (mea1_age_due_max-mea1_age_due_min),
  
  # For the late group - this is the number of days lived from start of the interval to the date of vaccination. Use the already-calculated days at risk to help.
  mea1_late==1 ~ mea1_days_at_risk + (mea1_age_due_max - mea1_age_due_min),
  
  # For those who got vaccinated on time, the number of days to vaccination is just the age at vaccination minus the age at start of the interval
  mea1_within_interval==1 ~ mea1_age_at_counted_vac - mea1_age_due_min,
  
  # For those who never got a vaccine it is their age minus minimum interval bound,
  never_got_mea1==1 ~ age_in_days - mea1_age_due_min
))

# # Now we need a failure indicator -- consider failure to be getting a vaccine. 
# # Kids who NEVER got measles or kids who got Measles early (and not again in the interval or late) will be censored at our observation days. 
# # Kids who got the vaccine in the interal or late will have known observation time.

potmea1dat <- potmea1dat %>% mutate(gotit = case_when(
  # known observation time
  mea1_late==1 | mea1_within_interval==1 | mea1_missed_opportunity=="Yes" ~ 1,
  
  # censored
  never_got_mea1==1 | early_mea1==1 ~ 0))

# use survfit function to create survival curves based on a formula
f1 <- survfit(Surv(time = hazard_days_mea1, event=gotit) ~ strata, data = potmea1dat)

plot2 <- ggsurvplot(
  fit=f1,
  data=potmea1dat,
  xlab="Days of observation",
  ylab="",
  title="Potential number of days to measles vaccination",
  fun="event",
  conf.int = FALSE,
  censor=FALSE,
  legend = "bottom", 
  legend.title = "Year of Liberia DHS",
  ylim = c(0,1),
  ggtheme=theme_linedraw())

# save plot
png(filename = paste0(visDir, "aim_1/missed_opportunities/measles_potential_curve_Lib.png"),
    height = 8, width = 10, units = "in", res = 300)
plot2
dev.off()
#CAROLINE: Changed labels 

########## 2. DPT ########## 

######### Observed ##########

obsdptdat <- data %>% filter(age_in_days>=dpt3_age_due_max, 
                             has_health_card_bin=="Yes")

# Calculate the days at risk for the hazard analysis
obsdptdat <- obsdptdat %>% mutate(hazard_days_dpt = case_when(
  # For the group with insufficient # of dpt, this is just the total number of days since the start of the interval
  too_few_elig_dpt==1 ~ dpt_days_at_risk + (dpt3_age_due_max - dpt3_age_due_min),
  
  # For the late group - this is the number of days lived from start of the interval to the date of vaccination. Use the already-calculated days at risk to help.
  dpt_late==1 ~ dpt_days_at_risk + (dpt3_age_due_max - dpt3_age_due_min),
  
  # For those who got vaccinated on time, the number of days to vaccination is just the age at vaccination minus the age at start of the interval
  dpt_within_interval==1 ~ dpt_age_at_counted_vac - dpt3_age_due_min,
  
  # For those who never got a vaccine it is their age minus minimum interval bound,
  never_got_dpt==1 ~ age_in_days - dpt3_age_due_min
))

# create a failure indicator
obsdptdat <- obsdptdat %>% mutate(gotit = case_when(
  # for those that never got dpt or kids who got dpt early and not again in the interval or late
  never_got_dpt==1 | too_few_elig_dpt==1 ~ 0,
  dpt_late==1 | dpt_within_interval==1 ~ 1
))

# test <- obsdptdat %>% filter(is.na(gotit))

####################################################
# Survival curve

# create survival object using observed data
observed.dpt <- Surv(time=obsdptdat$hazard_days_dpt, event=obsdptdat$gotit)

# use survfit function to create survival curves based on a formula
f2 <- survfit(observed.dpt ~ strata, data = obsdptdat)

# Plot Survival Curves
plot3 <- ggsurvplot(
  fit=f2,
  data = obsdptdat,
  xlab="Days of observation",
  ylab="",
  title="Observed number of days to DPT vaccination",
  fun="event",
  conf.int = FALSE,
  censor=FALSE,
  legend = "bottom", 
  legend.title = "Year of Liberia DHS",
  ylim = c(0,1),
  ggtheme=theme_linedraw()
)
#CAROLINE: changed labels 

# save plot
png(filename = paste0(visDir, "aim_1/missed_opportunities/dpt_observed_curve_lib.png"),
    height = 8, width = 10, units = "in", res = 300)
plot3
dev.off()

# find the median survival time
f2

# statistical test to see if there was a difference in survival time according to year
sd2 <- survdiff(observed.dpt ~ strata, data = obsdptdat)
sd2

####################################################
#### Potential coverage
####################################################

# keep kids older that the max age of dpt3 and keep if kids have a vaccine card
potdptdat <- data %>% filter(age_in_days>=dpt3_age_due_max,
                             has_health_card_bin=="Yes")

# Calculate the days at risk for the hazard analysis  -- the first part is the same as non_MOP analysis
potdptdat <- potdptdat %>% mutate(hazard_days_dpt = case_when(
  # for those with a missed opportunity, replace days at risk
  dpt_missed_opportunity=="Yes" & !is.na(dpt_age_at_mop_vac) ~ dpt_age_at_mop_vac - dpt3_age_due_min,
  
  # For the group with insufficient # of dpt, this is just the total number of days since the start of the interval
  too_few_elig_dpt==1 ~ dpt_days_at_risk + (dpt3_age_due_max - dpt3_age_due_min),
  
  # For the late group - this is the number of days lived from start of the interval to the date of vaccination. Use the already-calculated days at risk to help.
  dpt_late==1 ~ dpt_days_at_risk + (dpt3_age_due_max - dpt3_age_due_min),
  
  # For those who got vaccinated on time, the number of days to vaccination is just the age at vaccination minus the age at start of the interval
  dpt_within_interval==1 ~ dpt_age_at_counted_vac - dpt3_age_due_min,
  
  # For those who never got a vaccine it is their age minus minimum interval bound,
  never_got_dpt==1 ~ age_in_days - dpt3_age_due_min
))

# # Now we need a failure indicator -- consider failure to be getting a vaccine. 
# # Kids who NEVER got measles or kids who got Measles early (and not again in the interval or late) will be censored at our observation days. 
# # Kids who got the vaccine in the interval or late will have known observation time.

potdptdat <- potdptdat %>% mutate(gotit = case_when(
  # known observation time
  dpt_late==1 | dpt_within_interval==1 | dpt_missed_opportunity=="Yes" ~ 1,
  
  # censored
  never_got_dpt==1 | too_few_elig_dpt==1 ~ 0))

# create survival object using observed data
potential.dpt <- Surv(time=potdptdat$hazard_days_dpt, event=potdptdat$gotit)

# use survfit function to create survival curves based on a formula
f3 <- survfit(potential.dpt ~ strata, data = potdptdat)

plot4 <- ggsurvplot(
  fit=f3,
  data=potdptdat,
  xlab="Days of observation",
  ylab="",
  title="Potential number of days to dpt vaccination",
  fun="event",
  conf.int = FALSE,
  censor=FALSE,
  legend = "bottom", 
  legend.title = "Year of Liberia DHS",
  ylim = c(0,1),
  ggtheme=theme_linedraw()
)
#CAROLINE: Changed labels 

# save plots
png(filename = paste0(visDir, "aim_1/missed_opportunities/dpt_potential_curve_Lib.png"),
    height = 8, width = 10, units = "in", res = 300)
plot4
dev.off()

# find the median survival time
f3

# statistical test to see if there was a difference in survival time according to year
sd3 <- survdiff(potential.dpt ~ strata, data = potdptdat)
sd3

###### Create results table
dt <- as.data.table(data)
dt[, .(.N), by = .(v000)]

##############Step 8#######################
# Author: Francisco Rios Casas
# Purpose: create table of coverage cascade for vaccinations
# Date: Last modified October 13 2021

# Source set up file with required packages and file paths
#source("C:/Users/cnondin/Documents/uw-phi-vax/global_vac_index/aim_1/01_set_up_R.R")
#CAROLINE: already installed above 

# Load data
data <- readRDS(outputFile06)

# Subset data according to age and availability of vaccination document
data1 <- data %>% filter(age_in_days>=mea1_age_due_max,
                         has_health_card_bin=="Yes") # measles

data2 <- data %>% filter(age_in_days>=dpt3_age_due_max,
                         has_health_card_bin=="Yes") # all DPT doses

data3 <- data %>% filter(age_in_days>=dpt1_age_due_max,
                         has_health_card_bin=="Yes") # first dpt dose

data4 <- data %>% filter(age_in_days>=dpt2_age_due_max,
                         has_health_card_bin=="Yes") # second dpt dose

data5 <- data %>% filter(age_in_days>=dpt3_age_due_max,
                         has_health_card_bin=="Yes") # third dpt dose

# Create indicator for those that never got dpt or kids who got dpt early and not again in the interval, or late
data2 <- data2 %>% mutate(gotit = case_when(
  never_got_dpt==1 | too_few_elig_dpt==1 ~ 0,
  dpt_late==1 | dpt_within_interval==1 ~ 1))

# save as data table to making creating new variables simple
data1 <- as.data.table(data1)
data2 <- as.data.table(data2)
data3 <- as.data.table(data3)
data4 <- as.data.table(data4)
data5 <- as.data.table(data5)

# Part 1: Create vaccination coverage cascade for all vaccines

# Measles ----
dt1 <- data1[,.(total_with_card= .N), by = strata]

# # calculate how many children received mea1 according to health card only
dt2 <- data1[mea1_within_interval==1 | mea1_late==1,.(received_vaccine= .N), by = strata]

# calculate how many children did not receive the measles vaccine
dt3 <- data1[never_got_mea1==1 | early_mea1==1, .(no_vaccine=.N), by=strata]

# calculate how many children that were not vaccinated had a missed opportunity
dt4 <- data1[(never_got_mea1==1 | early_mea1==1) & mea1_missed_opportunity=="Yes", .(mop=.N), by=strata]

# merge datatables together
mea1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
mea1_dt[,vaccine:="mea1"]

mea1_dt

# statistical test 
mea1_test <- mea1_dt %>% select(strata, no_vaccine, mop)
mea1_test[,no_mop:=no_vaccine-mop]
mea1_test

chisq.test(mea1_test %>% select(mop, no_mop), correct = FALSE)

# statistical test of change in vaccine coverage
mea1_cov_test <- mea1_dt %>% select(strata, no_vaccine, received_vaccine)
mea1_cov_test
chisq.test(mea1_cov_test %>% select(no_vaccine, received_vaccine), correct = FALSE)

# DPT All -----

# calculate how many children has a vaccination card
dt1 <- data2[,.(total_with_card= .N), by = strata]

# calculate how many children received all dpt vaccines
dt2 <- data2[gotit==1,. (received_vaccine=.N), by=strata]

# calculate how many children did not receive all dpt doses
dt3 <- data2[gotit==0,. (no_vaccine=.N), by=strata]

# calculate how many of the children that did not receive all dpt doses have a dpt missed opportunity
dt4 <- data2[gotit==0 & dpt_missed_opportunity=="Yes",. (mop=.N), by=strata]

# merge dataset
dpt_all_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt_all_dt[,vaccine:="dpt_all"]
dpt_all_dt

# statistical test
dpt_all_test <- dpt_all_dt %>% select(strata, no_vaccine, mop)
dpt_all_test[,no_mop:=no_vaccine-mop]
dpt_all_test
chisq.test(dpt_all_test %>% select(mop, no_mop), correct = FALSE)

# statistical test of change in vaccine coverage
dpt_cov_test <- dpt_all_dt %>% select(strata, no_vaccine, received_vaccine)
dpt_cov_test
chisq.test(dpt_cov_test %>% select(no_vaccine, received_vaccine), correct = FALSE)

# DPT 1 -----

# calculate how many children have a vaccination card
dt1 <- data3[,.(total_with_card= .N), by = strata]

# calculate how many children received the dpt1 dose
dt2 <- data3[!is.na(age_at_dpt1),. (received_vaccine=.N), by=strata]

# calculate how many children did not receive dpt1 dose
dt3 <- data3[is.na(age_at_dpt1),. (no_vaccine=.N), by=strata]

# calculate how many of the children that did not receive dpt1 dose have a dpt1 missed opportunity
dt4 <- data3[is.na(age_at_dpt1) & dpt1_missed_opportunity==1,. (mop=.N), by=strata]

# merge data sets together
dpt1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt1_dt[,vaccine:="dpt1"]
dpt1_dt

# statistical test
dpt1_test <- dpt1_dt %>% select(strata, no_vaccine, mop)
dpt1_test[,no_mop:=no_vaccine-mop]
chisq.test(dpt1_test %>% select(mop, no_mop), correct = FALSE)

# DPT 2 -----

# calculate how many children have a vaccination card
dt1 <- data4[,.(total_with_card= .N), by = strata]

# # calculate how many children received dpt2
dt2 <- data4[!is.na(age_at_dpt2),. (received_vaccine=.N), by=strata]

# calculate how many children did not receive dpt2 dose
dt3 <- data4[is.na(age_at_dpt2),. (no_vaccine=.N), by=strata]

# calculate how many of the children that did not receive dpt2 dose have a dpt2 missed opportunity
dt4 <- data4[is.na(age_at_dpt2) & dpt2_missed_opportunity==1,. (mop=.N), by=strata]

# merge dataset
dpt2_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt2_dt[,vaccine:="dpt2"]
dpt2_dt

# statistical test
dpt2_test <- dpt2_dt %>% select(strata, no_vaccine, mop)
dpt2_test[,no_mop:=no_vaccine-mop]
dpt2_test
chisq.test(dpt2_test %>% select(mop, no_mop), correct = FALSE)

# DPT 3 -----

# # calculate how many children have a vaccination card
dt1 <- data5[,.(total_with_card= .N), by = strata]

# # calculate how many children received dpt3
dt2 <- data5[!is.na(age_at_dpt3),. (received_vaccine=.N), by=strata]

# calculate how many children did not receive dpt3 dose
dt3 <- data5[is.na(age_at_dpt3),. (no_vaccine=.N), by=strata]

# calculate how many of the children that did not receive dpt3 dose have a dpt3 missed opportunity
dt4 <- data5[is.na(age_at_dpt3) & dpt3_missed_opportunity==1,. (mop=.N), by=strata]

# merge dataset
dpt3_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt3_dt[,vaccine:="dpt3"]
dpt3_dt

# statistical test
dpt3_test <- dpt3_dt %>% select(strata, no_vaccine, mop)
dpt3_test[,no_mop:=no_vaccine-mop]
dpt3_test
chisq.test(dpt3_test %>% select(mop, no_mop), correct = FALSE)

# merge all vaccine data together
all_vax_data <- rbind(mea1_dt, dpt_all_dt, dpt1_dt, dpt2_dt, dpt3_dt)

# calculate vaccination coverage
all_vax_data[,vac_coverage:=round((received_vaccine/total_with_card)*100, 1)]

# calculate percent of children with a missed opportunity
all_vax_data[,percent_with_mop:=round((mop/no_vaccine)*100, 1)]

# calculate coverage if no missed opportunity
all_vax_data[,potential_coverage_with_no_mop:=round((mop+received_vaccine)/total_with_card*100, 1)]

setcolorder(all_vax_data, 
            c("strata", "total_with_card", "vaccine", "received_vaccine", "no_vaccine", 
              "vac_coverage", "mop", "percent_with_mop", "potential_coverage_with_no_mop"))
write.csv(all_vax_data, file=paste0(resDir, "aim_1/missed_opportunities/mop_vaccine_table_Lib.csv"))



# Part 2: Stratify coverage cascade by mother's education -----

# Measles -----
dt1 <- data1[,.(total_with_card= .N), by = .(strata, edu)]

# # calculate how many children received mea1 according to health card only
dt2 <- data1[mea1_within_interval==1 | mea1_late==1,.(received_vaccine= .N), by = .(strata, edu)]

# calculate how many children did not receive the measles vaccine
dt3 <- data1[never_got_mea1==1 | early_mea1==1, .(no_vaccine=.N), by=.(strata, edu)]

# calculate how many children that were not vaccinated had a missed opportunity
dt4 <- data1[(never_got_mea1==1 | early_mea1==1) & mea1_missed_opportunity=="Yes", .(mop=.N), by=.(strata, edu)]

# merge datatables together
mea1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
mea1_dt[,vaccine:="mea1"]
mea1_dt

# statistical test within each level of education
mea1_test <- mea1_dt %>% select(strata, edu, no_vaccine, mop)
mea1_test[,no_mop:=no_vaccine-mop]
mea1_test

chisq.test(mea1_test[edu=="No education",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[edu=="Primary",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[edu=="Secondary or higher",.(mop, no_mop)], correct = FALSE)

# DPT All -----
# calculate how many children has a vaccination card
dt1 <- data2[,.(total_with_card= .N), by = .(strata, edu)]

# calculate how many children received all dpt vaccines
dt2 <- data2[gotit==1,. (received_vaccine=.N), by=.(strata, edu)]

# calculate how many children did not receive all dpt doses
dt3 <- data2[gotit==0,. (no_vaccine=.N), by=.(strata, edu)]

# calculate how many of the children that did not receive all dpt doses have a dpt missed opportunity
dt4 <- data2[gotit==0 & dpt_missed_opportunity=="Yes",. (mop=.N), by=.(strata, edu)]

# merge dataset
dpt_all_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt_all_dt[,vaccine:="dpt_all"]
dpt_all_dt

# statistical test within each level of education
dpt_all_test <- mea1_dt %>% select(strata, edu, no_vaccine, mop)
dpt_all_test[,no_mop:=no_vaccine-mop]
dpt_all_test

chisq.test(dpt_all_test[edu=="No education",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[edu=="Primary",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[edu=="Secondary or higher",.(mop, no_mop)], correct = FALSE)

# DPT 1 -----

# calculate how many children have a vaccination card
dt1 <- data3[,.(total_with_card= .N), by = .(strata, edu)]

# calculate how many children received the dpt1 dose
dt2 <- data3[!is.na(age_at_dpt1),. (received_vaccine=.N), by=.(strata, edu)]

# calculate how many children did not receive dpt1 dose
dt3 <- data3[is.na(age_at_dpt1),. (no_vaccine=.N), by=.(strata, edu)]

# calculate how many of the children that did not receive dpt1 dose have a dpt1 missed opportunity
dt4 <- data3[is.na(age_at_dpt1) & dpt1_missed_opportunity==1,. (mop=.N), by=.(strata, edu)]

# merge data sets together
dpt1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt1_dt[,vaccine:="dpt1"]
dpt1_dt

# statistical test within each level of education
dpt1_test <- dpt1_dt %>% select(strata, edu, no_vaccine, mop)
dpt1_test[,no_mop:=no_vaccine-mop]
dpt1_test

chisq.test(dpt1_test[edu=="No education",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[edu=="Primary",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[edu=="Secondary or higher",.(mop, no_mop)], correct = FALSE)

# DPT 2 -----

# # calculate how many children have a vaccination card
dt1 <- data4[,.(total_with_card= .N), by = .(strata, edu)]

# # calculate how many children received dpt2
dt2 <- data4[!is.na(age_at_dpt2),. (received_vaccine=.N), by=.(strata, edu)]

# calculate how many children did not receive dpt2 dose
dt3 <- data4[is.na(age_at_dpt2),. (no_vaccine=.N), by=.(strata, edu)]

# calculate how many of the children that did not receive dpt2 dose have a dpt2 missed opportunity
dt4 <- data4[is.na(age_at_dpt2) & dpt2_missed_opportunity==1,. (mop=.N), by=.(strata, edu)]

# merge dataset
dpt2_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt2_dt[,vaccine:="dpt2"]
dpt2_dt

# statistical test within each level of education
dpt2_test <- dpt1_dt %>% select(strata, edu, no_vaccine, mop)
dpt2_test[,no_mop:=no_vaccine-mop]
dpt2_test

chisq.test(dpt2_test[edu=="No education",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[edu=="Primary",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[edu=="Secondary or higher",.(mop, no_mop)], correct = FALSE)

# DPT 3 ----

# # calculate how many children have a vaccination card
dt1 <- data5[,.(total_with_card= .N), by = .(strata, edu)]

# # calculate how many children received dpt3
dt2 <- data5[!is.na(age_at_dpt3),. (received_vaccine=.N), by=.(strata, edu)]

# calculate how many children did not receive dpt3 dose
dt3 <- data5[is.na(age_at_dpt3),. (no_vaccine=.N), by=.(strata, edu)]

# calculate how many of the children that did not receive dpt3 dose have a dpt3 missed opportunity
dt4 <- data5[is.na(age_at_dpt3) & dpt3_missed_opportunity==1,. (mop=.N), by=.(strata, edu)]

# merge dataset
dpt3_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt3_dt[,vaccine:="dpt3"]
dpt3_dt

# statistical test within each level of education
dpt3_test <- dpt3_dt %>% select(strata, edu, no_vaccine, mop)
dpt3_test[,no_mop:=no_vaccine-mop]
dpt3_test
chisq.test(dpt3_test[edu=="No education",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[edu=="Primary",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[edu=="Secondary or higher",.(mop, no_mop)], correct = FALSE)

# merge all vaccine data together
all_vax_data <- rbind(mea1_dt, dpt_all_dt, dpt1_dt, dpt2_dt, dpt3_dt)

# calculate vaccination coverage
all_vax_data[,vac_coverage:=round((received_vaccine/total_with_card)*100, 1)]

# calculate percent of children with a missed opportunity
all_vax_data[,percent_with_mop:=round((mop/no_vaccine)*100, 1)]

# calculate coverage if no missed opportunity
all_vax_data[,potential_coverage_with_no_mop:=round((mop+received_vaccine)/total_with_card*100, 1)]

setcolorder(all_vax_data, 
            c("strata", "edu", "total_with_card", "vaccine", "received_vaccine", "no_vaccine", 
              "vac_coverage", "mop", "percent_with_mop", "potential_coverage_with_no_mop"))

write.csv(all_vax_data, file=paste0(resDir, "aim_1/missed_opportunities/mop_vaccine_table_education_Lib.csv"))




# Part 3: Stratify coverage cascade by household assets -----
# Measles ----
dt1 <- data1[,.(total_with_card= .N), by = .(strata, assets)]

# # calculate how many children received mea1 according to health card only
dt2 <- data1[mea1_within_interval==1 | mea1_late==1,.(received_vaccine= .N), by = .(strata, assets)]

# calculate how many children did not receive the measles vaccine
dt3 <- data1[never_got_mea1==1 | early_mea1==1, .(no_vaccine=.N), by=.(strata, assets)]

# calculate how many children that were not vaccinated had a missed opportunity
dt4 <- data1[(never_got_mea1==1 | early_mea1==1) & mea1_missed_opportunity=="Yes", .(mop=.N), by=.(strata, assets)]

# merge datatables together
mea1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
mea1_dt[,vaccine:="mea1"]
mea1_dt

# statistical test within each level of household assets
mea1_test <- mea1_dt %>% select(strata, assets, no_vaccine, mop)
mea1_test[,no_mop:=no_vaccine-mop]
mea1_test

chisq.test(mea1_test[assets=="Quintile 1",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[assets=="Quintile 2",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[assets=="Quintile 3",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[assets=="Quintile 4",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[assets=="Quintile 5",.(mop, no_mop)], correct = FALSE)

# statistical test within each year for association with assets and mop
chisq.test(mea1_test[strata=="2013",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[strata=="2019",.(mop, no_mop)], correct = FALSE)
#CAROLINE: changed dates 
# DPT All -----

# calculate how many children has a vaccination card
dt1 <- data2[,.(total_with_card= .N), by = .(strata, assets)]

# calculate how many children received all dpt vaccines
dt2 <- data2[gotit==1,. (received_vaccine=.N), by=.(strata, assets)]

# calculate how many children did not receive all dpt doses
dt3 <- data2[gotit==0,. (no_vaccine=.N), by=.(strata, assets)]

# calculate how many of the children that did not receive all dpt doses have a dpt missed opportunity
dt4 <- data2[gotit==0 & dpt_missed_opportunity=="Yes",. (mop=.N), by=.(strata, assets)]

# merge dataset
dpt_all_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt_all_dt[,vaccine:="dpt_all"]
dpt_all_dt

# statistical test within each level of household assets
dpt_all_test <- dpt_all_dt %>% select(strata, assets, no_vaccine, mop)
dpt_all_test[,no_mop:=no_vaccine-mop]
dpt_all_test

chisq.test(dpt_all_test[assets=="Quintile 1",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[assets=="Quintile 2",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[assets=="Quintile 3",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[assets=="Quintile 4",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[assets=="Quintile 5",.(mop, no_mop)], correct = FALSE)

# DPT 1 -----

# calculate how many children have a vaccination card
dt1 <- data3[,.(total_with_card= .N), by = .(strata, assets)]

# calculate how many children received the dpt1 dose
dt2 <- data3[!is.na(age_at_dpt1),. (received_vaccine=.N), by=.(strata, assets)]

# calculate how many children did not receive dpt1 dose
dt3 <- data3[is.na(age_at_dpt1),. (no_vaccine=.N), by=.(strata, assets)]

# calculate how many of the children that did not receive dpt1 dose have a dpt1 missed opportunity
dt4 <- data3[is.na(age_at_dpt1) & dpt1_missed_opportunity==1,. (mop=.N), by=.(strata, assets)]

# merge data sets together
dpt1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt1_dt[,vaccine:="dpt1"]
dpt1_dt

# statistical test within each level of household assets
dpt1_test <- dpt1_dt %>% select(strata, assets, no_vaccine, mop)
dpt1_test[,no_mop:=no_vaccine-mop]
dpt1_test
chisq.test(dpt1_test[assets=="Quintile 1",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[assets=="Quintile 2",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[assets=="Quintile 3",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[assets=="Quintile 4",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[assets=="Quintile 5",.(mop, no_mop)], correct = FALSE)

# DPT 2 -----

# # calculate how many children have a vaccination card
dt1 <- data4[,.(total_with_card= .N), by = .(strata, assets)]

# # calculate how many children received dpt2
dt2 <- data4[!is.na(age_at_dpt2),. (received_vaccine=.N), by=.(strata, assets)]

# calculate how many children did not receive dpt2 dose
dt3 <- data4[is.na(age_at_dpt2),. (no_vaccine=.N), by=.(strata, assets)]

# calculate how many of the children that did not receive dpt2 dose have a dpt2 missed opportunity
dt4 <- data4[is.na(age_at_dpt2) & dpt2_missed_opportunity==1,. (mop=.N), by=.(strata, assets)]

# merge data set
dpt2_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt2_dt[,vaccine:="dpt2"]

# statistical test within each level of household assets
dpt2_test <- dpt2_dt %>% select(strata, assets, no_vaccine, mop)
dpt2_test[,no_mop:=no_vaccine-mop]
chisq.test(dpt2_test[assets=="Quintile 1",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[assets=="Quintile 2",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[assets=="Quintile 3",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[assets=="Quintile 4",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[assets=="Quintile 5",.(mop, no_mop)], correct = FALSE)

# DPT 3 -----

# # calculate how many children have a vaccination card
dt1 <- data5[,.(total_with_card= .N), by = .(strata, assets)]

# # calculate how many children received dpt3
dt2 <- data5[!is.na(age_at_dpt3),. (received_vaccine=.N), by=.(strata, assets)]

# calculate how many children did not receive dpt3 dose
dt3 <- data5[is.na(age_at_dpt3),. (no_vaccine=.N), by=.(strata, assets)]

# calculate how many of the children that did not receive dpt3 dose have a dpt3 missed opportunity
dt4 <- data5[is.na(age_at_dpt3) & dpt3_missed_opportunity==1,. (mop=.N), by=.(strata, assets)]

# merge dataset
dpt3_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt3_dt[,vaccine:="dpt3"]
dpt3_dt

# statistical test within each level of household assets
dpt3_test <- dpt3_dt %>% select(strata, assets, no_vaccine, mop)
dpt3_test[,no_mop:=no_vaccine-mop]
dpt3_test
chisq.test(dpt3_test[assets=="Quintile 1",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[assets=="Quintile 2",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[assets=="Quintile 3",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[assets=="Quintile 4",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[assets=="Quintile 5",.(mop, no_mop)], correct = FALSE)

# merge all vaccine data together
all_vax_data <- rbind(mea1_dt, dpt_all_dt, dpt1_dt, dpt2_dt, dpt3_dt)

# calculate vaccination coverage
all_vax_data[,vac_coverage:=round((received_vaccine/total_with_card)*100, 1)]

# calculate percent of children with a missed opportunity
all_vax_data[,percent_with_mop:=round((mop/no_vaccine)*100, 1)]

# # calculate coverage if no missed opportunity
all_vax_data[,potential_coverage_with_no_mop:=round((mop+received_vaccine)/total_with_card*100, 1)]

setcolorder(all_vax_data, 
            c("strata", "assets", "total_with_card", "vaccine", "received_vaccine", "no_vaccine", 
              "vac_coverage", "mop", "percent_with_mop", "potential_coverage_with_no_mop"))

write.csv(all_vax_data, file=paste0(resDir, "aim_1/missed_opportunities/mop_vaccine_table_assets_Lib.csv"))
#CAROLINE: added label 


# Part 4: Stratify coverage cascade by sub-national states -----
# Measles ----
dt1 <- data1[,.(total_with_card= .N), by = .(strata, state)]

# # calculate how many children received mea1 according to health card only
dt2 <- data1[mea1_within_interval==1 | mea1_late==1,.(received_vaccine= .N), by = .(strata, state)]

# calculate how many children did not receive the measles vaccine
dt3 <- data1[never_got_mea1==1 | early_mea1==1, .(no_vaccine=.N), by=.(strata, state)]

# calculate how many children that were not vaccinated had a missed opportunity
dt4 <- data1[(never_got_mea1==1 | early_mea1==1) & mea1_missed_opportunity=="Yes", .(mop=.N), by=.(strata, state)]

# merge datatables together
mea1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
mea1_dt[,vaccine:="mea1"]
mea1_dt


# DPT All -----

# calculate how many children has a vaccination card
dt1 <- data2[,.(total_with_card= .N), by = .(strata, state)]

# calculate how many children received all dpt vaccines
dt2 <- data2[gotit==1,. (received_vaccine=.N), by=.(strata, state)]

# calculate how many children did not receive all dpt doses
dt3 <- data2[gotit==0,. (no_vaccine=.N), by=.(strata, state)]

# calculate how many of the children that did not receive all dpt doses have a dpt missed opportunity
dt4 <- data2[gotit==0 & dpt_missed_opportunity=="Yes",. (mop=.N), by=.(strata, state)]

# merge dataset
dpt_all_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt_all_dt[,vaccine:="dpt_all"]
dpt_all_dt

# DPT 1 -----

# calculate how many children have a vaccination card
dt1 <- data3[,.(total_with_card= .N), by = .(strata, state)]

# calculate how many children received the dpt1 dose
dt2 <- data3[!is.na(age_at_dpt1),. (received_vaccine=.N), by=.(strata, state)]

# calculate how many children did not receive dpt1 dose
dt3 <- data3[is.na(age_at_dpt1),. (no_vaccine=.N), by=.(strata, state)]

# calculate how many of the children that did not receive dpt1 dose have a dpt1 missed opportunity
dt4 <- data3[is.na(age_at_dpt1) & dpt1_missed_opportunity==1,. (mop=.N), by=.(strata, state)]

# merge data sets together
dpt1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt1_dt[,vaccine:="dpt1"]
dpt1_dt

# DPT 2 -----

# # calculate how many children have a vaccination card
dt1 <- data4[,.(total_with_card= .N), by = .(strata, state)]

# # calculate how many children received dpt2
dt2 <- data4[!is.na(age_at_dpt2),. (received_vaccine=.N), by=.(strata, state)]

# calculate how many children did not receive dpt2 dose
dt3 <- data4[is.na(age_at_dpt2),. (no_vaccine=.N), by=.(strata, state)]

# calculate how many of the children that did not receive dpt2 dose have a dpt2 missed opportunity
dt4 <- data4[is.na(age_at_dpt2) & dpt2_missed_opportunity==1,. (mop=.N), by=.(strata, state)]

# merge data set
dpt2_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt2_dt[,vaccine:="dpt2"]
dpt2_dt

# DPT 3 -----

# # calculate how many children have a vaccination card
dt1 <- data5[,.(total_with_card= .N), by = .(strata, state)]

# # calculate how many children received dpt3
dt2 <- data5[!is.na(age_at_dpt3),. (received_vaccine=.N), by=.(strata, state)]

# calculate how many children did not receive dpt3 dose
dt3 <- data5[is.na(age_at_dpt3),. (no_vaccine=.N), by=.(strata, state)]

# calculate how many of the children that did not receive dpt3 dose have a dpt3 missed opportunity
dt4 <- data5[is.na(age_at_dpt3) & dpt3_missed_opportunity==1,. (mop=.N), by=.(strata, state)]

# merge dataset
dpt3_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt3_dt[,vaccine:="dpt3"]
dpt3_dt

# merge all vaccine data together
all_vax_data <- rbind(mea1_dt, dpt_all_dt, dpt1_dt, dpt2_dt, dpt3_dt)

# calculate vaccination coverage
all_vax_data[,vac_coverage:=round((received_vaccine/total_with_card)*100, 1)]

# calculate percent of children with a missed opportunity
all_vax_data[,percent_with_mop:=round((mop/no_vaccine)*100, 1)]

# # calculate coverage if no missed opportunity
all_vax_data[,potential_coverage_with_no_mop:=round((mop+received_vaccine)/total_with_card*100, 1)]

setcolorder(all_vax_data, 
            c("strata", "state", "total_with_card", "vaccine", "received_vaccine", "no_vaccine", 
              "vac_coverage", "mop", "percent_with_mop", "potential_coverage_with_no_mop"))

write.csv(all_vax_data, file=paste0(resDir, "aim_1/missed_opportunities/mop_vaccine_table_states_Lib.csv"))
#CAROLINE: added Lib



# Part 5: Stratify coverage cascade by sub-national zones -----
# Measles ----
dt1 <- data1[,.(total_with_card= .N), by = .(strata, zone)]

# # calculate how many children received mea1 according to health card only
dt2 <- data1[mea1_within_interval==1 | mea1_late==1,.(received_vaccine= .N), by = .(strata, zone)]

# calculate how many children did not receive the measles vaccine
dt3 <- data1[never_got_mea1==1 | early_mea1==1, .(no_vaccine=.N), by=.(strata, zone)]

# calculate how many children that were not vaccinated had a missed opportunity
dt4 <- data1[(never_got_mea1==1 | early_mea1==1) & mea1_missed_opportunity=="Yes", .(mop=.N), by=.(strata, zone)]

# merge datatables together
mea1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
mea1_dt[,vaccine:="mea1"]
mea1_dt

# statistical test within each zone
mea1_test <- mea1_dt %>% select(strata, zone, no_vaccine, mop)
mea1_test

mea1_test[,no_mop:=no_vaccine-mop]
chisq.test(mea1_test[zone=="North Central",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[zone=="North East",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[zone=="North West",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[zone=="South East",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[zone=="South South",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[zone=="South West",.(mop, no_mop)], correct = FALSE)

# DPT All -----

# calculate how many children has a vaccination card
dt1 <- data2[,.(total_with_card= .N), by = .(strata, zone)]

# calculate how many children received all dpt vaccines
dt2 <- data2[gotit==1,. (received_vaccine=.N), by=.(strata, zone)]

# calculate how many children did not receive all dpt doses
dt3 <- data2[gotit==0,. (no_vaccine=.N), by=.(strata, zone)]

# calculate how many of the children that did not receive all dpt doses have a dpt missed opportunity
dt4 <- data2[gotit==0 & dpt_missed_opportunity=="Yes",. (mop=.N), by=.(strata, zone)]

# merge dataset
dpt_all_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt_all_dt[,vaccine:="dpt_all"]
dpt_all_dt

# statistical test within each zone
dpt_all_test <- dpt_all_dt %>% select(strata, zone, no_vaccine, mop)
dpt_all_test[,no_mop:=no_vaccine-mop]
dpt_all_test
chisq.test(dpt_all_test[zone=="North Central",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[zone=="North East",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[zone=="North West",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[zone=="South East",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[zone=="South South",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[zone=="South West",.(mop, no_mop)], correct = FALSE)

# DPT 1 -----

# calculate how many children have a vaccination card
dt1 <- data3[,.(total_with_card= .N), by = .(strata, zone)]

# calculate how many children received the dpt1 dose
dt2 <- data3[!is.na(age_at_dpt1),. (received_vaccine=.N), by=.(strata, zone)]

# calculate how many children did not receive dpt1 dose
dt3 <- data3[is.na(age_at_dpt1),. (no_vaccine=.N), by=.(strata, zone)]

# calculate how many of the children that did not receive dpt1 dose have a dpt1 missed opportunity
dt4 <- data3[is.na(age_at_dpt1) & dpt1_missed_opportunity==1,. (mop=.N), by=.(strata, zone)]

# merge data sets together
dpt1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt1_dt[,vaccine:="dpt1"]
dpt1_dt

# statistical test within each zone
dpt1_test <- dpt1_dt %>% select(strata, zone, no_vaccine, mop)
dpt1_test[,no_mop:=no_vaccine-mop]
dpt1_test

chisq.test(dpt1_test[zone=="North Central",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[zone=="North East",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[zone=="North West",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[zone=="South East",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[zone=="South South",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[zone=="South West",.(mop, no_mop)], correct = FALSE)

# DPT 2 -----

# # calculate how many children have a vaccination card
dt1 <- data4[,.(total_with_card= .N), by = .(strata, zone)]

# # calculate how many children received dpt2
dt2 <- data4[!is.na(age_at_dpt2),. (received_vaccine=.N), by=.(strata, zone)]

# calculate how many children did not receive dpt2 dose
dt3 <- data4[is.na(age_at_dpt2),. (no_vaccine=.N), by=.(strata, zone)]

# calculate how many of the children that did not receive dpt2 dose have a dpt2 missed opportunity
dt4 <- data4[is.na(age_at_dpt2) & dpt2_missed_opportunity==1,. (mop=.N), by=.(strata, zone)]

# merge data set
dpt2_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt2_dt[,vaccine:="dpt2"]
dpt2_dt

# statistical test within each zone
dpt2_test <- dpt2_dt %>% select(strata, zone, no_vaccine, mop)
dpt2_test[,no_mop:=no_vaccine-mop]
dpt2_test

chisq.test(dpt2_test[zone=="North Central",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[zone=="North East",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[zone=="North West",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[zone=="South East",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[zone=="South South",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[zone=="South West",.(mop, no_mop)], correct = FALSE)

# DPT 3 -----

# # calculate how many children have a vaccination card
dt1 <- data5[,.(total_with_card= .N), by = .(strata, zone)]

# # calculate how many children received dpt3
dt2 <- data5[!is.na(age_at_dpt3),. (received_vaccine=.N), by=.(strata, zone)]

# calculate how many children did not receive dpt3 dose
dt3 <- data5[is.na(age_at_dpt3),. (no_vaccine=.N), by=.(strata, zone)]

# calculate how many of the children that did not receive dpt3 dose have a dpt3 missed opportunity
dt4 <- data5[is.na(age_at_dpt3) & dpt3_missed_opportunity==1,. (mop=.N), by=.(strata, zone)]

# merge dataset
dpt3_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt3_dt[,vaccine:="dpt3"]
dpt3_dt

# statistical test within each zone
dpt3_test <- dpt3_dt %>% select(strata, zone, no_vaccine, mop)
dpt3_test[,no_mop:=no_vaccine-mop]
dpt3_test
chisq.test(dpt3_test[zone=="North Central",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[zone=="North East",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[zone=="North West",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[zone=="South East",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[zone=="South South",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[zone=="South West",.(mop, no_mop)], correct = FALSE)

# merge all vaccine data together
all_vax_data <- rbind(mea1_dt, dpt_all_dt, dpt1_dt, dpt2_dt, dpt3_dt)

# calculate vaccination coverage
all_vax_data[,vac_coverage:=round((received_vaccine/total_with_card)*100, 1)]

# calculate percent of children with a missed opportunity
all_vax_data[,percent_with_mop:=round((mop/no_vaccine)*100, 1)]

# # calculate coverage if no missed opportunity
all_vax_data[,potential_coverage_with_no_mop:=round((mop+received_vaccine)/total_with_card*100, 1)]

# Save the results -----
setcolorder(all_vax_data,
            c("strata", "zone", "total_with_card", "vaccine", "received_vaccine", "no_vaccine",
              "vac_coverage", "mop", "percent_with_mop", "potential_coverage_with_no_mop"))

write.csv(all_vax_data, file=paste0(resDir, "aim_1/missed_opportunities/mop_vaccine_table_zones_Lib.csv"))
#CAROLINE: added Lib 

# check data for correlation between three variables: education, assets, zones -----
x <- table(data$edu, data$assets)
x
chi2 <- chisq.test(x, correct = FALSE)
chi2
sqrt(chi2$statistic / sum(x))

y <- table(data$assets, data$zone)
y
chi2 <- chisq.test(y, data$zone)
chi2
sqrt(chi2$statistic / sum(y))

z <- table(data$edu, data$zone)
z
chi2 <- chisq.test(z, correct = FALSE)
chi2
sqrt(chi2$statistic / sum(z))
