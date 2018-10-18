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
                  control = sim_control(nschls = 21L, n_cohorts = 4L, 
                                        assessment_adjustment = assess_adj,
                                        assess_sim_par = assess_sim_par, 
                                        grad_adjustment = grad_adj))



ap_report_cleaner <- function(simlist){
  out_data <- left_join(simlist$demog_master %>% 
                          select(sid, Sex, Race), simlist$stu_year)
  # Add school data from Georgia
  simlist$schools <- bind_cols(simlist$schools, gen_schl_roster())
  # Select AP school field naems
  out_data <- left_join(out_data, simlist$schools %>% select(schid, `AI Institution Name`, 
                                                             `AI Street Address 1`,
                                                             `AI Street Address 2`, 
                                                             `AI Street Address 3`, 
                                                             `AI State`, 
                                                             `AI Zip Code`))
  out_data <- inner_join(out_data, 
                        simlist$stu_assess %>% 
                          dplyr::select(sid, math_ss, rdg_ss, grade, year))
  out_data <- filter(out_data, grade %in% c("10", "11", "12"))
  out_data <- rename(out_data, SEX = Sex)
  out_data$Sex <- substr(out_data$SEX, 1, 1)
  out_data$SEX <- NULL
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
  
  # We need to add some duplication in here for students taking multiple 
  # assessments
  added_rows <- sample_frac(out_data, size = 0.1)
  out_data <- bind_rows(out_data, added_rows)
  # And repeat (to draw a new sample)
  added_rows <- sample_frac(out_data, size = 0.05)
  out_data <- bind_rows(out_data, added_rows)
  # Add some truly many test takers
  added_rows <- sample_frac(out_data, size = 0.05)
  added_rows_2 <- sample_frac(added_rows, size = 0.8)
  added_rows_3 <- sample_frac(added_rows_2, size = 0.25)
  out_data <- bind_rows(out_data, added_rows, 
                        added_rows_2, added_rows_3)
  rm(added_rows, added_rows_2, added_rows_3)
  
  out_data$`Exam Code` <- gen_ap_exam_codes(out_data$sid)
  out_data$`Exam Grade` <- scale_score_to_ap(sapply(out_data$rdg_ss, 
                                                    fuzz_score, fuzz = 20))
  out_data$`Admin Year` <- out_data$year - 2000
  out_data$`Award Year` <- out_data$year - 2000
  out_data$`Education Level` <- ap_grade_level(out_data$grade)
  
  # Rename
  out_data <- rename(out_data, `AI Code` = schid)
  out_data$`DISTRICT NAME` <- "AIUR"
  out_data <- rename(out_data, `AP Number` = sid)
  
  
  
  # TODO: Assign awards
  out_data <- out_data %>% group_by(`AP Number`, year) %>% 
    mutate(exams_taken = n(),
           `Award Type` = "None", 
           uniq_subj = n_distinct(`Exam Code`), 
           avg_score = mean(`Exam Grade`), 
           count_3_or_more = sum(`Exam Grade` >= 3),
           count_4_or_more = sum(`Exam Grade` >= 4)) 
  
  out_data$`Award Type` <- ifelse(out_data$exams_taken >= 3 & 
                                    out_data$count_3_or_more >=3, "01", out_data$`Award Type`)
  
  out_data$`Award Type` <- ifelse(out_data$avg_score >= 3.25 & 
                               out_data$count_3_or_more >= 4, "02", out_data$`Award Type`)
  
  out_data$`Award Type` <- ifelse(out_data$avg_score >= 3.5 & 
                                    out_data$count_3_or_more >= 5, "03", out_data$`Award Type`)
  
  out_data$`Award Type` <- ifelse(out_data$avg_score >= 4 & 
                                    out_data$count_4_or_more >= 8, "05", out_data$`Award Type`)
  
  
  # TODO: Reorder output
  # TODO: Export
  
  # Test reshaping wide
  
  out_data <- select(out_data, names(out_data)[grepl("^[A-Z]{1,3}", names(out_data))])
  out_data <- out_data %>% group_by(`AP Number`) %>% 
    arrange(year) %>%
    mutate(Exam_Number = str_pad(as.character(1:n()), width = 2, pad = "0"))
  
  out_data <- as.data.frame(out_data)
  # out_data$`AP Number` <- as.numeric(out_data$`AP Number`)
  
  # Reshape Wide
  out_w <- reshape(out_data, direction = "wide", v.names = c("Exam Code", "Exam Grade",
                                                           "Award Type",
                                                           "Admin Year", "Award Year"), 
                 idvar = c("AP Number"), 
                 drop = c("year", names(out_data)[3:12], "DISTRICT NAME", 
                          "Education Level"),
                 timevar = "Exam_Number", sep = " ")
  out_data_ids <- out_data %>% select(2:12, `Education Level`, `DISTRICT NAME`) %>% distinct()
  
  out_data <- left_join(out_data_ids, out_w)
  
  out_data <- out_data %>% select(`AP Number`, `Sex`, `Education Level`, 
                                  starts_with("Award Type"), 
                                  starts_with("AI Street Address"), 
                                  starts_with("AI State"), 
                                  starts_with("AI Zip"), 
                                  everything())
  
  return(out_data)
}


out_data <- ap_report_cleaner(simlist)

write.csv(out_data, file = "data/synthetic_ap.csv", row.names = FALSE)
