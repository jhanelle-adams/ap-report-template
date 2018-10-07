################################################################################
# Generate AP Report Data
# Date: 10/05/2018
# Author: Jared E. Knowles
# Prepared for OpenSDP
################################################################################
## 
source("R/ap_tools.R")
library(equate)
library(OpenSDPsynthR)
set.seed(0525212) # set the seed
library(magrittr)
library(stringr)

# The synthesizer needs some input paramaters
# As it is the defaults are not sufficient to give realistic assessment data
# These change those defaults to make the scores less deterministic

assess_adj <- sim_control()$assessment_adjustment

# Make scores spread out more
assess_adj$perturb_base <- function(x, sd) 
{
  mean_shift <- rnorm(1, sd = 3)
  y <- x + rnorm(1, mean_shift, sd * 0.8)
  return(y)
}

assess_adj$gender_list <- list("Male" = 1, 
                               "Female" = -1)

assess_adj$frl_list <- list("0" = 0.1, 
                            "1" = -0.1)

# Get defaults
assess_sim_par <- OpenSDPsynthR::sim_control()$assess_sim_par
# Increase score variance
assess_sim_par$error_var <- 15
# Increase coefficient effects
assess_sim_par$fixed_param <- assess_sim_par$fixed_param * 10
# Downgrade IEP difference
assess_sim_par$fixed_param[4] <- -0.75
# Downgrade LEP difference
assess_sim_par$fixed_param[5] <- -1
assess_sim_par$fixed_param[6] <- 0
assess_sim_par$fixed_param[7] <- 1
assess_sim_par$lvl1_err_params$mean <- 1
assess_sim_par$lvl1_err_params$sd <- 10
# Set group level variances
assess_sim_par$random_param$random_var <- c(0.4, 0.75)
# Set the school-grade size ranges
assess_sim_par$unbalanceRange <- c(100, 420)

grad_adj <- sim_control()$grad_adjustment
grad_adj$school_list <- NULL
grad_adj$perturb_school <- function(x, schid, schl_par = school_list){
  val_mean <- schl_par[[which(schid == names(schl_par))]]
  val_sd <- val_mean / 4
  val_sd <- abs(val_sd)
  y <- x + num_clip(rnorm(1, mean = val_mean, sd = val_sd), -0.45, 0.45)
  y <- ifelse(y <= 0, 0.01, y)
  y <- ifelse(y >= 1, 0.98, y)
  y <- num_clip(y, 0, 1)
  return(y)
}


# Conduct the simulation
simlist <- simpop(5000L, seed = 0525212, 
                  control = sim_control(nschls = 12L, n_cohorts = 4L, 
                                        assessment_adjustment = assess_adj,
                                        assess_sim_par = assess_sim_par, 
                                        grad_adjustment = grad_adj))



ap_report_cleaner <- function(simlist){
  out_data <- left_join(simlist$demog_master %>% 
                          select(sid, Sex, Race), simlist$stu_year)
  out_data <- left_join(out_data, simlist$schools %>% select(schid, name, lea_id))
  out_data <- left_join(out_data, simlist$stu_assess %>% dplyr::select(sid, math_ss, rdg_ss))
  out_data <- filter(out_data, grade %in% c("10", "11", "12"))
  out_data <- rename(out_data, SEX = Sex)
  out_data$SEX <- substr(out_data$SEX, 1, 1)
  out_data$Race <-  forcats::fct_expand(out_data$Race, 
                                        "MEXICAN", "PUERTORICAN", 
                                        "HISP_LAT", "NON_HISP_LAT", 
                                        "CUBAN")
  # Augment race variables
  out_data$Race[out_data$Race == "Hispanic or Latino Ethnicity"] <- 
    sample(c("MEXICAN", "PUERTORICAN", "HISP_LAT", "NON_HISP_LAT", "CUBAN"), length(out_data$Race[out_data$Race == "Hispanic or Latino Ethnicity"]), 
           replace = TRUE)
  
  
  # Factor expand
  
  out_data$Race <- forcats::fct_recode(out_data$Race, 
                      WHITE = "White", 
                      ASIAN = "Asian", 
                      AFRICANAMERICAN = "Black or African American", 
                      HAWAIIAN_PI = "Native Hawaiian or Other Pacific Islander", 
                      INDIAN_ALASKAN = "American Indian or Alaska Native", 
                      MULTIETHNIC = "Demographic Race Two or More Races")
  out_data$Race <- forcats::fct_drop(out_data$Race)
  
  out_data$RACE_ETH_ <- out_data$Race
  # 1 = "Y"
  # 0 = "N"
  dummyvars <- as.data.frame(model.matrix(~0 + RACE_ETH_, data = out_data))
  dummyvars <- as.data.frame(apply(dummyvars, 2, code_yn))
  
  out_data <- bind_cols(out_data, dummyvars)
  out_data$Race <- NULL
  
  # Derive race
  
  # School Code and name
  out_data <- rename(out_data, AI_CODE = schid)
  out_data <- rename(out_data, AI_NAME = name)
  out_data <- rename(out_data,  DISTRICT_NAME = lea_id)
  out_data$DISTRICT_NAME <- "AIUR"
  out_data <- rename(out_data, SECONDARY_ID = sid)
  
  # Projected Grad Date
  out_data$PROJ_GRAD_DATE <- paste0(out_data$cohort_grad_year, "-05")
  # LATEST_ASSESSMENT_DATE = M-DD-YYYY
  # LATEST_GRADE_LEVEL = integer
  # LATEST_REVISED = blank
  out_data$LATEST_ASSESSMENT_DATE <- paste0("3-22-", out_data$cohort_grad_year - 1)
  out_data$LATEST_GRADE_LEVEL <- 11
  out_data$LATEST_REVISED <- ""
  
  # Derive scores
  # LATEST_SAT_TOTAL = 400-1600
  # LATEST_SAT_EBRW = reading score 200-800
  # LATEST_SAT_MATH_SECTION = 200-800
  # Select only data with all_caps
  out_data <- select(out_data, names(out_data)[grepl("^[A-Z]{2,3}", names(out_data))])
  return(out_data)
}


out_data <- ap_report_cleaner(simlist)
