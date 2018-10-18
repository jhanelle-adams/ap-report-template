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

gen_ap_exam_codes <- function(x) {
  all_codes <- c("07", "13", "14", "15", "16", "20", "22", "23", "25", "28", 
                 "31", "33", "34", "35", "36", "37", "40", "43", "48", "51", 
                 "53", "55", "57", "58", "60", "61", "62", "64", "66", "68", 
                 "69", "75", "76", "77", "80", "82", "78", "83", "84", "85", 
                 "87", "89", "90", "93")
  code_probs <- c(0.15, 0.01, 0.005, 0.005, 0.005, 0.1, 0.0005, 0.0005, 0.04, 0.001, 
                  0.03, 0.00, 0.03, 0.03, 0.15, 0.05, 0.03, 0.04, 0.00, 0.01, 
                  0.01, 0.01, 0.1, 0.001, 0.01, 0.000, 0.005, 0.005, 0.1, 
                  0.02, 0.001, 0.06, 0.03, 0.001, 0.01, 0.01, 0.00, 0.1, 0.05, 0.05, 
                  0.1, 0.03, 0.1, 0.1)
  N <- length(x)
  sample(all_codes, N, replace = TRUE, prob = code_probs)
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
  if (is.na(x)) {
    return(NA)
  } else {
    samp_seq <- seq(x - fuzz, x + fuzz, by = 1)
    x <- sample(samp_seq, 1)
    return(x)
  }
}

scale_score_to_ap <- function(x) {
  # https://apscore.collegeboard.org/scores/about-ap-scores/score-distributions/
  samp_score_avg <- sample(5:1, 10000, replace = TRUE, 
                           prob = c(0.17, 0.25, 0.25, 0.2, 0.13))
  
  rx <- equate::freqtab(samp_score_avg)
  ry <- equate::freqtab(round(x, digits = 0))
  ## Equate the new score with the simulated SAT score
  zzz <- equate::equate(ry, rx, type = "equipercentile", boot = TRUE, reps = 5)
  new_score <- equate::equate(x, y = zzz)
  new_score <- round(new_score, digits = 0)
  return(new_score)
}

gen_schl_roster <- function() {
  structure(
    list(
      `AI Code` = c(
        112129L,
        111148L,
        110060L,
        110258L,
        110058L,
        111866L,
        110146L,
        111154L,
        110216L,
        112625L,
        111175L,
        111265L,
        112623L,
        110794L,
        111268L,
        119163L,
        113068L,
        110069L,
        110061L,
        110802L,
        119162L
      ),
      `AI Institution Name` = c(
        "Cambridge High School",
        "Northview High School",
        "Milton High School",
        "Westlake High School",
        "Alpharetta High School",
        "Johns Creek High School",
        "Chattahoochee High School",
        "North Springs Charter High Sch",
        "Riverwood International Charte",
        "Roswell High School",
        "Tri-Cities High School",
        "Creekside High School",
        "Centennial High School",
        "Banneker High School",
        "Langston Hughes High School",
        "Northwestern Middle School",
        "Hapeville Career Academy",
        "Fulton Science Charter High Sc",
        "Independence High School",
        "Frank McClarin High School",
        "River Trail Middle School"
      ),
      `AI Street Address 1` = c(
        "2845 Bethany Bend",
        "10625 Parsons Road",
        "13025 Birmingham Highway",
        "2400 Union Road Southwest",
        "3595 Webb Bridge Road",
        "5575 State Bridge Road",
        "5230 Taylor Road",
        "7447 Roswell Road Northeast",
        "5900 Raider Drive",
        "11595 King Road",
        "2575 Harris Street",
        "7405 Herndon Road",
        "9310 Scott Road",
        "6015 Feldwood Road",
        "7510 Hall Road",
        "12805 Birmingham Highway",
        "6045 Buffington Road",
        "4100 Old Milton Parkway",
        "86 School Drive",
        "3605 Main Street",
        "10795 Rogers Circle"
      ),
      `AI Street Address 2` = c(
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        "Suite 100",
        NA,
        NA,
        NA
      ),
      `AI Street Address 3` = c(
        "Milton",
        "Duluth",
        "Milton",
        "Atlanta",
        "Alpharetta",
        "Johns Creek",
        "Alpharetta",
        "Sandy Springs",
        "Sandy Springs",
        "Roswell",
        "East Point",
        "Fairburn",
        "Roswell",
        "College Park",
        "Fairburn",
        "Alpharetta",
        "Atlanta",
        "Alpharetta",
        "Alpharetta",
        "College Park",
        "Duluth"
      ),
      `AI State` = c(
        "GA",
        "GA",
        "GA",
        "GA",
        "GA",
        "GA",
        "GA",
        "GA",
        "GA",
        "GA",
        "GA",
        "GA",
        "GA",
        "GA",
        "GA",
        "GA",
        "GA",
        "GA",
        "GA",
        "GA",
        "GA"
      ),
      `AI Zip Code` = c(
        30004L,
        30097L,
        30004L,
        30331L,
        30005L,
        30022L,
        30022L,
        30328L,
        30328L,
        30075L,
        30344L,
        30213L,
        30076L,
        30349L,
        30213L,
        30004L,
        30349L,
        30005L,
        30009L,
        30337L,
        30097L
      )
    ),
    class = "data.frame",
    row.names = c(NA,-21L)
  )
  
}

