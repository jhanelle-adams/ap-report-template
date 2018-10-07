code_yn <- function(x) { 
  x <- as.character(x)
  x [x == "1"] <- "Y"
  x [x == "0"] <- "N"
  return(x)
}

trim_num <- function(x, floor, ceiling){
  x[x < floor] <- floor
  x[x > ceiling] <- ceiling
  return(x)
}

scale_score_to_ap <- function(x) {
  # example AP
  samp_ap <- rpois(10000, 3.6)
  samp_ap <- trim_num(samp_ap, 1, 5)
  
  rx <- equate::freqtab(samp_ap)
  ry <- equate::freqtab(round(x, digits = 0))
  ## Equate the new score with the simulated SAT score
  zzz <- equate::equate(ry, rx, type = "equipercentile", boot = TRUE, reps = 5)
  new_score <- equate::equate(x, y = zzz)
  new_score <- trim_num(new_score, 1, 5)
  new_score <- as.integer(new_score)
  return(new_score)
}

ap_grade_level <- function(x) {
  y <- as.character(x)
  y[is.na(y)] <- "11"
  y[!y %in% c("9", "10", "11", "12")] <- "3"
  y[y == "9"] <- "4"
  y[y == "10"] <- "5"
  y[y == "11"] <- "6"
  y[y == "12"] <- "7"
  y[y == "g13"] <- "8"
  return(y)
}

ap_award_type <- function(x) {
  y <- as.character(x)
  y[y == "01"] <- "AP Scholar"
  y[y == "02"] <- "AP Scholar with Honor"
  y[y == "03"] <- "AP Scholar with Distinction"
  y[y == "04"] <- "State AP Scholar"
  y[y == "05"] <- "National AP Scholar"
  y[y == "06"] <- "National AP Scholar (Canada)"
  y[y == "07"] <- "AP International Diploma"
  y[y == "08"] <- "DoDEA AP Scholar"
  y[y == "09"] <- "International AP Scholar"
  y[y == "10"] <- "AP Diploma"
  y[y == "11"] <- "AP International Diploma with Honors"
  y[y == "12"] <- "National AP Scholar (Bermuda)"
  y[y == "13"] <- "AP Capstone Diploma"
  y[y == "14"] <- "AP Seminar and Research Certificate"
  return(y)
}

ap_exam_codes <- function(x) {
  # take exam code numeric, conver to character
  y <- as.character(x)
  y[y == "07"] <- "United States History"
  y[y == "13"] <- "Art History"
  y[y == "14"] <- "Studio Art: Drawing Portfolio"
  y[y == "15"] <- "Studio Art: 2-D Design Portfolio"
  y[y == "16"] <- "Studio Art: 3-D Design Portfolio"
  y[y == "20"] <- "Biology"
  y[y == "22"] <- "Seminar"
  y[y == "23"] <- "Research"
  y[y == "25"] <- "Chemistry"
  y[y == "28"] <- "Chinese Language and Culture"
  y[y == "31"] <- "Computer Science A"
  y[y == "33"] <- "Computer Science AB"
  y[y == "34"] <- "Microeconomics"
  y[y == "35"] <- "Macroeconomics"
  y[y == "36"] <- "English Language and Composition"
  y[y == "37"] <- "English Literature and Composition"
  y[y == "40"] <- "Environmental Science"
  y[y == "43"] <- "European History"
  y[y == "48"] <- "French Language and Culture"
  y[y == "51"] <- "French Literature"
  y[y == "53"] <- "Human Geography"
  y[y == "55"] <- "German Language and Culture"
  y[y == "57"] <- "United State Government and Politics"
  y[y == "58"] <- "Comparative Government and Politics"
  y[y == "60"] <- "Latin"
  y[y == "61"] <- "Latin Literature"
  y[y == "62"] <- "Italian Language and Culture"
  y[y == "64"] <- "Japanese Language and Culture"
  y[y == "66"] <- "Calculus AB"
  y[y == "68"] <- "Calculus BC"
  y[y == "69"] <- "Calculus BC: AB Subscore"
  y[y == "75"] <- "Music Theory"
  y[y == "76"] <- "Music Aural Subscore"
  y[y == "77"] <- "Music Non-Aural Subscore"
  y[y == "80"] <- "Physics C: Mechanics"
  y[y == "82"] <- "Physics C: Electricity and Magnetism"
  y[y == "78"] <- "Physics B"
  y[y == "83"] <- "Physics 1"
  y[y == "84"] <- "Physics 2"
  y[y == "85"] <- "Psychology"
  y[y == "87"] <- "Spanish Language and Culture"
  y[y == "89"] <- "Spanish Literature and Culture"
  y[y == "90"] <- "Statistics"
  y[y == "93"] <- "World History"
  return(y)
}


compute_subscores <- function(x, scale = c(15L, 40L, 8L)) { 
  examp_score_40 <- round(rnorm(10000, 20, 5.25), 0)
  examp_score_40 <- trim_num(examp_score_40, floor = 10, ceiling = 40)
  examp_score_15 <- round(rnorm(10000, 8, 2.25), 0)
  examp_score_15 <- trim_num(examp_score_15, floor = 1, ceiling = 15)
  
  examp_score_8 <- rpois(10000, 5)
  examp_score_8 <- trim_num(examp_score_15, floor = 0, ceiling = 8)
  examp_score_8[examp_score_8 == 1] <- sample(c(0, 2), 
                                              length(examp_score_8[examp_score_8 == 1]), 
                                              replace = TRUE)
  
  ftab_obs <- equate::freqtab(x)
  
  if(scale == 40) {
    ftab_40 <- equate::freqtab(examp_score_40)
    eq_40 <- equate::equate(ftab_obs, ftab_40, type = "equipercentile", 
                            boot = TRUE, reps = 5)
    out <- equate::equate(sapply(x, fuzz_score, fuzz = 100), y = eq_40)
    out <- trim_num(out, 10, 40)
  } else if (scale == 15) {
    ftab_15 <- equate::freqtab(examp_score_15)
    eq_15 <- equate::equate(ftab_obs, ftab_15, type = "equipercentile", 
                            boot = TRUE, reps = 5)
    out <- equate::equate(sapply(x, fuzz_score, fuzz = 100), y = eq_15)
    out <- trim_num(out, 1, 15)
  } else if (scale == 8) {
    ftab_8 <- equate::freqtab(examp_score_8)
    eq_8 <- equate::equate(ftab_obs, ftab_8, type = "equipercentile", 
                           boot = TRUE, reps = 5)
    out <- equate::equate(sapply(x, fuzz_score, fuzz = 100), y = eq_8)
    out <- trim_num(out, 0, 8)
  }
  
  out <- round(out, digits = 0)
  return(out)
}

fuzz_score <- function(x, fuzz) {
  samp_seq <- seq(x - fuzz, x + fuzz, by = 1)
  x <- sample(samp_seq, 1)
  return(x)
}
