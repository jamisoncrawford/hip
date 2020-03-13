# PROJECT: MELLON GRANT INTERIM PROGRESS REPORT

    # 2020-03-11

    # 1) Compare HIP Scholars with peers in same majors
        # a) Current GPAs
        # b) Semesterly GPA trajectories
        # c) Course Choices Aggregated by Level
            # i. Credit Hours to Date
            # ii. Determine level (e.g. Junior, Senior)
            # iii. Course Levels (e.g. 1000, 2000, 3000, 4000)

    # 2) Comparison Group:
        # a) Students in same majors
        # b) Students with same matriculation term



# SET DIRECTORY, GLOBAL OPTIONS, LOAD PACKAGES

setwd("~/CASA HIP")

options(scipen = 999)

rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)
library(panthr)
library(readxl)
library(scales)
library(ggplot2)
library(stringr)
library(lubridate)



# READ IN & CLEAN DATA

hips <- read_excel("Student Demographics.xlsx") %>%
  field_case(case = "snake") %>%
  mutate(start = ifelse(start == "SUM2019",
                        "201905",
                        ifelse(start == "FALL 2019",
                               "201908",
                               ifelse(start == "SPRING 2020",
                                      "202001", NA))),
         end = as.character(ymd(end)),
         end = substr(end, 1, 7),
         end = gsub(x = end, pattern = "-", ""),
         end = gsub(x = end, pattern = "202002", "202001"),
         panther_number = substr(panther_number, 1, 9)) %>%
  select(start:end, gsu_email, panther_number) %>%
  rename(pantherid = panther_number,
         email = gsu_email)



# READ IN & JOIN GSU WAREHOUSE DATA: TREATMENT GROUP

keys <- read_csv("sql-export_treatment-keys.csv") %>% field_case(case = "snake")
main <- read_csv("sql-export_treatment-main.csv") %>% field_case(case = "snake")
demo <- read_csv("sql-export_treatment-demo.csv") %>% field_case(case = "snake")
regc <- read_csv("sql-export_treatment-regc.csv") %>% field_case(case = "snake")

hips <- hips %>%
  left_join(keys) %>%
  select(start:pidm)

rm(keys)

demo <- demo %>%
  select(term, whkey, ethnicity_code, race, sex, 
         citizen_status, veteran_ind:first_generation_ind) %>%
  mutate(ethnorace = decode_ethnorace(race = race, ethnicity = ethnicity_code),
         sex = decode_sex(sex),
         citizen_status = ifelse(citizen_status == "C",
                                 "Citizen",
                                 ifelse(citizen_status == "R",
                                        "Resident",
                                        ifelse(citizen_status == "A",
                                               "Alien",
                                               NA)))) %>%
  group_by(whkey, ethnorace, sex, citizen_status) %>%
  summarize(term = max(term, na.rm = TRUE),
            veteran_ind = ifelse(sum(!is.na(veteran_ind), na.rm = TRUE) > 0, TRUE, FALSE),
            stem_ind = ifelse(sum(!is.na(veteran_ind), na.rm = TRUE) > 0, TRUE, FALSE),
            pell_eligible_ind = ifelse(sum(!is.na(veteran_ind), na.rm = TRUE) > 0, TRUE, FALSE),
            first_generation_ind = ifelse(sum(!is.na(veteran_ind), na.rm = TRUE) > 0, TRUE, FALSE)) %>%
  ungroup()

hips <- hips %>%
  left_join(demo)

hips <- hips[-which(hips$whkey == "001122045"), ]      # 2005 Matriculation Removed

rm(demo)

main <- main %>%
  select(whkey, term, major, hours_earned, hours_total, 
         student_level_regents, gpa, matric_term) %>%
  mutate(gpa_disc = gpa_discretize(gpa),
         gpa = round(gpa, 3),
         student_level_regents = substr(student_level_regents, 1, 1)) %>%
  select(whkey, matric_term, term:major, student_level_regents, gpa, gpa_disc, hours_earned:hours_total)

main[which(main$hours_earned == 0), c("hours_earned", "gpa", "gpa_disc")] <- NA

main <- main[-which(main$whkey == "001122045"), ]    # 2005 Matriculation Removed

regc <- regc %>%
  select(whkey, term, course_department, crn, 
         course_prefix, course_number, letter_grade) %>%
  mutate(course_department = decode_department(course_department),
         course_level = as.numeric(paste0(substr(course_number, 1, 1), "000"))) %>%
  select(whkey:course_number, course_level, letter_grade)

regc <- regc[-which(regc$whkey == "001122045"), ]    # 2005 Matriculation Removed


# DETERMINE KEY TREATMENT GROUP IDENTIFIERS

uniq <- main %>%
  select(whkey, term, matric_term, major) %>%
  group_by(whkey) %>%
  summarize(matric_term = max(matric_term, na.rm = TRUE),
            term = min(term, na.rm = TRUE)) %>%
  ungroup()

uniq <- uniq %>%
  left_join(main) %>%
  select(whkey, matric_term, major)

majors <- unique(uniq$major)  
terms <- min(uniq$matric_term)



# READ IN CONTROL GROUP DATA

main_ctrl <- read_csv("sql-export_control-main.csv") %>%
  field_case(case = "snake")

regc_ctrl1 <- read_excel("sql-export_control-regc.xlsx",
                         sheet = "Export Worksheet")

regc_ctrl2 <- read_excel("sql-export_control-regc.xlsx", 
                         col_names = FALSE,
                         sheet = "Sheet1")

names(regc_ctrl2) <- names(regc_ctrl1)

regc_ctrl <- bind_rows(regc_ctrl1, regc_ctrl2) %>%
  field_case(case = "snake")

rm(regc_ctrl1, regc_ctrl2, majors, terms)



# CLEAN CONTROL & TREATMENT

regc_ctrl$letter_grade <- gsub(x = regc_ctrl$letter_grade, 
                               pattern = "%|^-|#|@|M|\\^R|\\*|~|I|P|V|U|G|GH|NR|S|WF", 
                               replacement = "")

regc_ctrl[regc_ctrl$letter_grade == "", "letter_grade"] <- NA

regc$letter_grade <- gsub(x = regc$letter_grade, 
                          pattern = "V|S|I", 
                          replacement = "")

regc[is.na(regc$letter_grade), "letter_grade"] <- ""

regc[regc$letter_grade == "", "letter_grade"] <- NA

regc <- regc %>%
  select(whkey, term, crn:letter_grade) %>%
  mutate(group = "Treatment")

regc_ctrl <- regc_ctrl %>%
  mutate(course_level = as.numeric(paste0(substr(course_number, 1, 1), "000"))) %>%
  select(whkey, term, crn:course_number, course_level, letter_grade) %>%
  mutate(term = as.numeric(term),
         crn = as.numeric(crn),
         course_number = as.numeric(course_number),
         group = "Control")

regc_ctrl <- regc_ctrl[-which(regc_ctrl$whkey %in% unique(regc$whkey)), ]

regc <- bind_rows(regc, regc_ctrl)

rm(regc_ctrl)

main_ctrl <- main_ctrl %>%
  mutate(student_level_regents = substr(student_level_regents, 1, 1),
         gpa_disc = gpa_discretize(gpa),
         group = "Control") %>%
  select(whkey, term, matric_term, major, 
         student_level_regents, gpa, gpa_disc, group)

main <- main %>%
  select(whkey, term, matric_term, major, student_level_regents, gpa, gpa_disc) %>%
  mutate(group = "Treatment")

main_ctrl <- main_ctrl[-which(main_ctrl$whkey %in% unique(main$whkey)), ]

main <- bind_rows(main, main_ctrl)

rm(main_ctrl, uniq)

regc[is.na(regc$letter_grade), "letter_grade"] <- ""

regc[regc$letter_grade == "W" | regc$letter_grade == "", "letter_grade"] <- NA

regc$numeric_grade <- NA

regc[!is.na(regc$letter_grade) & regc$letter_grade == "A+", "numeric_grade"] <- 4.33
regc[!is.na(regc$letter_grade) & regc$letter_grade == "A", "numeric_grade"] <- 4.00
regc[!is.na(regc$letter_grade) & regc$letter_grade == "A-", "numeric_grade"] <- 3.66
regc[!is.na(regc$letter_grade) & regc$letter_grade == "B+", "numeric_grade"] <- 3.33
regc[!is.na(regc$letter_grade) & regc$letter_grade == "B", "numeric_grade"] <- 3.00
regc[!is.na(regc$letter_grade) & regc$letter_grade == "B-", "numeric_grade"] <- 2.66
regc[!is.na(regc$letter_grade) & regc$letter_grade == "C+", "numeric_grade"] <- 2.33
regc[!is.na(regc$letter_grade) & regc$letter_grade == "C", "numeric_grade"] <- 2.00
regc[!is.na(regc$letter_grade) & regc$letter_grade == "C-", "numeric_grade"] <- 1.66
regc[!is.na(regc$letter_grade) & regc$letter_grade == "D", "numeric_grade"] <- 1.33
regc[!is.na(regc$letter_grade) & regc$letter_grade == "F", "numeric_grade"] <- 1.00



# ANALYSIS: MEAN

main_mn <- main %>%
  group_by(group, major, student_level_regents) %>%
  summarize(mean = mean(gpa, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(key = group, value = c(mean)) %>%
  mutate(Treatment = ifelse(is.nan(Treatment),
                            NA,
                            Treatment))

main_mn <- main_mn[which(complete.cases(main_mn)), ]

main_mn <- main_mn %>%
  mutate(Difference = (Treatment - Control) / mean(c(Treatment, Control)),
         Difference = percent(Difference, accuracy = 1),
         Difference = paste0("+ ", Difference),
         Difference = gsub(x = Difference, pattern = "\\+ \\-", replacement = "- "),
         major = decode_major(major)) %>%
  rename(Major = major,
         Level = student_level_regents)

main_mn[main_mn$Level == 1, "Level"] <- "Freshman"
main_mn[main_mn$Level == 2, "Level"] <- "Sophomore"
main_mn[main_mn$Level == 3, "Level"] <- "Junior"
main_mn[main_mn$Level == 4, "Level"] <- "Senior"

main_mn <- main_mn %>%
  mutate(Control = round(Control, 2),
         Treatment = round(Treatment, 2))



# ANALYSIS: COUNT

main_n <- main %>%
  group_by(group, major, student_level_regents) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  spread(key = group, value = c(n)) %>%
  mutate(Treatment = ifelse(is.nan(Treatment),
                            NA,
                            Treatment))

main_n <- main_n[which(complete.cases(main_n)), ]

main_n <- main_n %>%
  mutate(major = decode_major(major),
         Proportion = percent(Treatment / (Control + Treatment))) %>%
  rename(Major = major,
         Level = student_level_regents)

main_n[main_n$Level == 1, "Level"] <- "Freshman"
main_n[main_n$Level == 2, "Level"] <- "Sophomore"
main_n[main_n$Level == 3, "Level"] <- "Junior"
main_n[main_n$Level == 4, "Level"] <- "Senior"



# ANALYSIS: MEDIAN

main_md <- main %>%
  group_by(group, major, student_level_regents) %>%
  summarize(median = median(gpa, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(key = group, value = c(median)) %>%
  mutate(Treatment = ifelse(is.nan(Treatment),
                            NA,
                            Treatment))

main_md <- main_md[which(complete.cases(main_md)), ]

main_md <- main_md %>%
  mutate(Difference = (Treatment - Control) / mean(c(Treatment, Control)),
         Difference = percent(Difference, accuracy = 1),
         Difference = paste0("+ ", Difference),
         Difference = gsub(x = Difference, pattern = "\\+ \\-", replacement = "- "),
         major = decode_major(major)) %>%
  rename(Major = major,
         Level = student_level_regents)

main_md[main_md$Level == 1, "Level"] <- "Freshman"
main_md[main_md$Level == 2, "Level"] <- "Sophomore"
main_md[main_md$Level == 3, "Level"] <- "Junior"
main_md[main_md$Level == 4, "Level"] <- "Senior"

main_md <- main_md %>%
  mutate(Control = round(Control, 2),
         Treatment = round(Treatment, 2))

# ANALYSIS OVER TIME: MEAN BY YEAR & MAJOR

main_mn_ts <- main %>%
  mutate(year = substr(x = term, 1, 4)) %>%
  group_by(group, year, major, student_level_regents) %>%
  summarize(gpa = median(gpa, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(gpa = ifelse(is.nan(gpa), NA, gpa)) %>%
  spread(key = group, value = gpa) %>%
  mutate(major = decode_major(major)) %>%
  rename(Level = student_level_regents,
         Year = year,
         Major = major)

main_mn_ts <- main_mn_ts[which(complete.cases(main_mn_ts)), ]

main_mn_ts[main_mn_ts$Level == 1, "Level"] <- "Freshman"
main_mn_ts[main_mn_ts$Level == 2, "Level"] <- "Sophomore"
main_mn_ts[main_mn_ts$Level == 3, "Level"] <- "Junior"
main_mn_ts[main_mn_ts$Level == 4, "Level"] <- "Senior"

main_mn_ts <- main_mn_ts %>%
  mutate(Difference = (Treatment - Control) / mean(c(Treatment, Control)),
         Difference = percent(Difference, accuracy = 1),
         Difference = paste0("+ ", Difference),
         Difference = gsub(x = Difference, pattern = "\\+ \\-", replacement = "- ")) %>%
  mutate(Control = round(Control, 2),
         Treatment = round(Treatment, 2))



# ANALYSIS OVER TIME: MEDIAN BY YEAR & MAJOR

main_md_ts <- main %>%
  mutate(year = substr(x = term, 1, 4)) %>%
  group_by(group, year, major, student_level_regents) %>%
  summarize(gpa = median(gpa, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(gpa = ifelse(is.nan(gpa), NA, gpa)) %>%
  spread(key = group, value = gpa) %>%
  mutate(major = decode_major(major)) %>%
  rename(Level = student_level_regents,
         Year = year,
         Major = major)

main_md_ts <- main_md_ts[which(complete.cases(main_md_ts)), ]

main_md_ts[main_md_ts$Level == 1, "Level"] <- "Freshman"
main_md_ts[main_md_ts$Level == 2, "Level"] <- "Sophomore"
main_md_ts[main_md_ts$Level == 3, "Level"] <- "Junior"
main_md_ts[main_md_ts$Level == 4, "Level"] <- "Senior"

main_md_ts <- main_md_ts %>%
  mutate(Difference = (Treatment - Control) / mean(c(Treatment, Control)),
         Difference = percent(Difference, accuracy = 1),
         Difference = paste0("+ ", Difference),
         Difference = gsub(x = Difference, pattern = "\\+ \\-", replacement = "- ")) %>%
  mutate(Control = round(Control, 2),
         Treatment = round(Treatment, 2))



# ANALYSIS OVER TIME: MEAN BY YEAR

main_mn_ts_yr <- main %>%
  mutate(year = substr(x = term, 1, 4)) %>%
  group_by(group, year, student_level_regents) %>%
  summarize(gpa = mean(gpa, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(gpa = ifelse(is.nan(gpa), NA, gpa)) %>%
  spread(key = group, value = gpa) %>%
  rename(Level = student_level_regents,
         Year = year)

main_mn_ts_yr <- main_mn_ts_yr[which(complete.cases(main_mn_ts_yr)), ]

main_mn_ts_yr[main_mn_ts_yr$Level == 1, "Level"] <- "Freshman"
main_mn_ts_yr[main_mn_ts_yr$Level == 2, "Level"] <- "Sophomore"
main_mn_ts_yr[main_mn_ts_yr$Level == 3, "Level"] <- "Junior"
main_mn_ts_yr[main_mn_ts_yr$Level == 4, "Level"] <- "Senior"

main_mn_ts_yr <- main_mn_ts_yr %>%
  mutate(Difference = (Treatment - Control) / mean(c(Treatment, Control)),
         Difference = percent(Difference, accuracy = 1),
         Difference = paste0("+ ", Difference),
         Difference = gsub(x = Difference, pattern = "\\+ \\-", replacement = "- ")) %>%
  mutate(Control = round(Control, 2),
         Treatment = round(Treatment, 2))



# ANALYSIS OVER TIME: MEDIAN BY YEAR

main_md_ts_yr <- main %>%
  mutate(year = substr(x = term, 1, 4)) %>%
  group_by(group, year, student_level_regents) %>%
  summarize(gpa = median(gpa, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(gpa = ifelse(is.nan(gpa), NA, gpa)) %>%
  spread(key = group, value = gpa) %>%
  rename(Level = student_level_regents,
         Year = year)

main_md_ts_yr <- main_md_ts_yr[which(complete.cases(main_md_ts_yr)), ]

main_md_ts_yr[main_md_ts_yr$Level == 1, "Level"] <- "Freshman"
main_md_ts_yr[main_md_ts_yr$Level == 2, "Level"] <- "Sophomore"
main_md_ts_yr[main_md_ts_yr$Level == 3, "Level"] <- "Junior"
main_md_ts_yr[main_md_ts_yr$Level == 4, "Level"] <- "Senior"

main_md_ts_yr <- main_md_ts_yr %>%
  mutate(Difference = (Treatment - Control) / mean(c(Treatment, Control)),
         Difference = percent(Difference, accuracy = 1),
         Difference = paste0("+ ", Difference),
         Difference = gsub(x = Difference, pattern = "\\+ \\-", replacement = "- ")) %>%
  mutate(Control = round(Control, 2),
         Treatment = round(Treatment, 2))



# ANALYSIS OVER TIME: MEAN BY MAJOR

main_mn_ts_mj <- main %>%
  group_by(group, major, student_level_regents) %>%
  summarize(gpa = mean(gpa, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(gpa = ifelse(is.nan(gpa), NA, gpa)) %>%
  spread(key = group, value = gpa) %>%
  rename(Level = student_level_regents,
         Major = major)

main_mn_ts_mj <- main_mn_ts_mj[which(complete.cases(main_mn_ts_mj)), ]

main_mn_ts_mj[main_mn_ts_mj$Level == 1, "Level"] <- "Freshman"
main_mn_ts_mj[main_mn_ts_mj$Level == 2, "Level"] <- "Sophomore"
main_mn_ts_mj[main_mn_ts_mj$Level == 3, "Level"] <- "Junior"
main_mn_ts_mj[main_mn_ts_mj$Level == 4, "Level"] <- "Senior"

main_mn_ts_mj <- main_mn_ts_mj %>%
  mutate(Difference = (Treatment - Control) / mean(c(Treatment, Control)),
         Difference = percent(Difference, accuracy = 1),
         Difference = paste0("+ ", Difference),
         Difference = gsub(x = Difference, pattern = "\\+ \\-", replacement = "- ")) %>%
  mutate(Control = round(Control, 2),
         Treatment = round(Treatment, 2)) %>%
  mutate(Major = decode_major(Major))



# ANALYSIS OVER TIME: MEDIAN BY MAJOR

main_md_ts_mj <- main %>%
  group_by(group, major, student_level_regents) %>%
  summarize(gpa = median(gpa, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(gpa = ifelse(is.nan(gpa), NA, gpa)) %>%
  spread(key = group, value = gpa) %>%
  rename(Level = student_level_regents,
         Major = major)

main_md_ts_mj <- main_md_ts_mj[which(complete.cases(main_md_ts_mj)), ]

main_md_ts_mj[main_md_ts_mj$Level == 1, "Level"] <- "Freshman"
main_md_ts_mj[main_md_ts_mj$Level == 2, "Level"] <- "Sophomore"
main_md_ts_mj[main_md_ts_mj$Level == 3, "Level"] <- "Junior"
main_md_ts_mj[main_md_ts_mj$Level == 4, "Level"] <- "Senior"

main_md_ts_mj <- main_md_ts_mj %>%
  mutate(Difference = (Treatment - Control) / mean(c(Treatment, Control)),
         Difference = percent(Difference, accuracy = 1),
         Difference = paste0("+ ", Difference),
         Difference = gsub(x = Difference, pattern = "\\+ \\-", replacement = "- ")) %>%
  mutate(Control = round(Control, 2),
         Treatment = round(Treatment, 2)) %>%
  mutate(Major = decode_major(Major))



# ANALYSIS OVER TIME: MEAN BY LEVEL

main_mn_ts_all <- main %>%
  group_by(group, student_level_regents) %>%
  summarize(gpa = mean(gpa, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(gpa = ifelse(is.nan(gpa), NA, gpa)) %>%
  spread(key = group, value = gpa) %>%
  rename(Level = student_level_regents)

main_mn_ts_all <- main_mn_ts_all[which(complete.cases(main_mn_ts_all)), ]

main_mn_ts_all[main_mn_ts_all$Level == 1, "Level"] <- "Freshman"
main_mn_ts_all[main_mn_ts_all$Level == 2, "Level"] <- "Sophomore"
main_mn_ts_all[main_mn_ts_all$Level == 3, "Level"] <- "Junior"
main_mn_ts_all[main_mn_ts_all$Level == 4, "Level"] <- "Senior"

main_mn_ts_all <- main_mn_ts_all %>%
  mutate(Difference = (Treatment - Control) / mean(c(Treatment, Control)),
         Difference = percent(Difference, accuracy = 1),
         Difference = paste0("+ ", Difference),
         Difference = gsub(x = Difference, pattern = "\\+ \\-", replacement = "- ")) %>%
  mutate(Control = round(Control, 2),
         Treatment = round(Treatment, 2))



# ANALYSIS OVER TIME: MEDIAN BY LEVEL

main_md_ts_all <- main %>%
  group_by(group, student_level_regents) %>%
  summarize(gpa = median(gpa, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(gpa = ifelse(is.nan(gpa), NA, gpa)) %>%
  spread(key = group, value = gpa) %>%
  rename(Level = student_level_regents)

main_md_ts_all <- main_md_ts_all[which(complete.cases(main_md_ts_all)), ]

main_md_ts_all[main_md_ts_all$Level == 1, "Level"] <- "Freshman"
main_md_ts_all[main_md_ts_all$Level == 2, "Level"] <- "Sophomore"
main_md_ts_all[main_md_ts_all$Level == 3, "Level"] <- "Junior"
main_md_ts_all[main_md_ts_all$Level == 4, "Level"] <- "Senior"

main_md_ts_all <- main_md_ts_all %>%
  mutate(Difference = (Treatment - Control) / mean(c(Treatment, Control)),
         Difference = percent(Difference, accuracy = 1),
         Difference = paste0("+ ", Difference),
         Difference = gsub(x = Difference, pattern = "\\+ \\-", replacement = "- ")) %>%
  mutate(Control = round(Control, 2),
         Treatment = round(Treatment, 2))



# COURSES

regc <- regc %>%
  mutate(year = substr(term, 1, 4)) %>%
  select(whkey:term, year, crn:letter_grade, numeric_grade, group)

regc_mn <- regc %>%
  group_by(group, year, course_level) %>%
  summarize(mean = mean(numeric_grade, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(key = group, value = mean)

regc_mn <- regc_mn[which(complete.cases(regc_mn)), ] %>%
  mutate(Difference = (Treatment - Control) / mean(c(Treatment, Control)),
         Difference = percent(Difference, accuracy = 1),
         Difference = paste0("+ ", Difference),
         Difference = gsub(x = Difference, pattern = "\\+ \\-", replacement = "- ")) %>%
  rename(Year = year,
         Level = course_level) %>%
  mutate(Treatment = ifelse(is.nan(Treatment), NA, Treatment),
         Treatment = round(Treatment, 3),
         Control = round(Control, 3))

regc_md <- regc %>%
  group_by(group, year, course_level) %>%
  summarize(median = median(numeric_grade, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(key = group, value = median)

regc_md <- regc_md[which(complete.cases(regc_md)), ] %>%
  mutate(Difference = (Treatment - Control) / median(c(Treatment, Control)),
         Difference = percent(Difference, accuracy = 1),
         Difference = paste0("+ ", Difference),
         Difference = gsub(x = Difference, pattern = "\\+ \\-", replacement = "- ")) %>%
  rename(Year = year,
         Level = course_level) %>%
  mutate(Treatment = ifelse(is.nan(Treatment), NA, Treatment),
         Treatment = round(Treatment, 3),
         Control = round(Control, 3))

regc_mn_crs <- regc %>%
  group_by(group, year, course_prefix, course_level) %>%
  summarize(mean = mean(numeric_grade, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(key = group, value = mean)

regc_mn_crs <- regc_mn_crs[which(complete.cases(regc_mn_crs)), ] %>%
  mutate(Difference = (Treatment - Control) / mean(c(Treatment, Control)),
         Difference = percent(Difference, accuracy = 1),
         Difference = paste0("+ ", Difference),
         Difference = gsub(x = Difference, pattern = "\\+ \\-", replacement = "- ")) %>%
  rename(Year = year,
         Level = course_level) %>%
  mutate(Treatment = ifelse(is.nan(Treatment), NA, Treatment),
         Treatment = round(Treatment, 3),
         Control = round(Control, 3))

regc_md_crs <- regc %>%
  group_by(group, year, course_prefix, course_level) %>%
  summarize(mean = median(numeric_grade, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(key = group, value = mean)

regc_md_crs <- regc_md_crs[which(complete.cases(regc_md_crs)), ] %>%
  mutate(Difference = (Treatment - Control) / mean(c(Treatment, Control)),
         Difference = percent(Difference, accuracy = 1),
         Difference = paste0("+ ", Difference),
         Difference = gsub(x = Difference, pattern = "\\+ \\-", replacement = "- ")) %>%
  rename(Year = year,
         Level = course_level) %>%
  mutate(Treatment = ifelse(is.nan(Treatment), NA, Treatment),
         Treatment = round(Treatment, 3),
         Control = round(Control, 3))



# MERGING MAIN & COURSES

all <- main %>%
  left_join(regc)

all <- all %>%
  mutate(course_level_abb = as.numeric(substr(course_level, 1, 1)),
         student_level_regents = as.numeric(student_level_regents))

all <- all %>%
  mutate(level_diff = course_level_abb - student_level_regents)

lvls_diff <- all %>%
  group_by(group, level_diff) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  filter(!is.na(level_diff))

lvls_diff <- lvls_diff %>%
  group_by(group) %>%
  summarize(total = sum(n, na.rm = TRUE)) %>%
  ungroup() %>%
  right_join(lvls_diff) %>%
  rename(courses = n) %>%
  select(group, level_diff, courses, total) %>%
  mutate(proportion = courses / total)

lvls_diff_tbl <- lvls_diff %>%
  rename(Group = group,
         Difference = level_diff,
         Courses = courses,
         Total = total,
         Proportion = proportion) %>%
  mutate(Courses = comma(Courses),
         Total = comma(Total),
         Proportion = percent(Proportion))



lvls_diff_yr <- all %>%
  mutate(year = as.numeric(substr(term, 1, 4))) %>%
  group_by(group, year, level_diff) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  filter(!is.na(level_diff))

lvls_diff_yr <- lvls_diff_yr %>%
  group_by(group, year) %>%
  summarize(total = sum(n, na.rm = TRUE)) %>%
  ungroup() %>%
  right_join(lvls_diff_yr) %>%
  rename(courses = n) %>%
  select(group, year, level_diff, courses, total) %>%
  mutate(proportion = courses / total)

lvls_diff_yr_tbl <- lvls_diff_yr %>%
  mutate(courses = comma(courses),
         total = comma(total),
         proportion = percent(proportion)) %>%
  rename(Group = group,
         Year = year,
         Difference = level_diff,
         Courses = courses,
         Total = total,
         Proportion = proportion)



# VISUALIZATIONS

lvls_diff[which(lvls_diff$group == "Treatment"), "group"] <- "HIP"
lvls_diff[which(lvls_diff$group == "Control"), "group"] <- "Non-HIP"

viz1 <- ggplot(lvls_diff, aes(x = factor(level_diff), 
                      y = proportion, 
                      fill = group)) +
  geom_bar(stat = "identity", 
           position = "dodge", 
           alpha = 0.6) +
  labs(x = "Course Level Difference",
       y = "Proportion of Courses (%)",
       fill = "Affiliation",
       title = "Student Class Level v. Course Level",
       subtitle = "All Common Courses Between HIP & Non-HIP Students",
       caption = "Source: GSU Data Warehouse") +
  scale_y_continuous(labels = percent) +
  theme_minimal()

lvls_diff_yr[which(lvls_diff_yr$group == "Treatment"), "group"] <- "HIP"
lvls_diff_yr[which(lvls_diff_yr$group == "Control"), "group"] <- "Non-HIP"
lvls_diff_yr$year_abb <- paste0("'", substr(lvls_diff_yr$year, 3, 4))

all_sum <- all %>%
  group_by(group, year, whkey) %>%
  summarize(mean_diff = mean(level_diff, na.rm = TRUE),
            mean_gpas = mean(gpa, na.rm = TRUE)) %>%
  ungroup()

all_sum <- all_sum %>% filter(!is.na(year))
all_sum <- all_sum %>% filter(year != 2020)

all_sum_non <- all_sum %>% filter(group == "Control") %>% mutate(group = "Non-HIP")
all_sum_hip <- all_sum %>% filter(group == "Treatment") %>% mutate(group = "HIP")

all_sum[all_sum$group == "Control", "group"] <- "Non-HIP"
all_sum[all_sum$group == "Treatment", "group"] <- "HIP"

viz2 <- ggplot(all_sum, aes(x = mean_diff, 
                    y = mean_gpas,
                    color = group)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              size = 2,
              color = "white") +
  geom_smooth(method = "lm", 
              se = FALSE, 
              size = 1.1,
              color = alpha("grey45", 0.5)) +
  geom_smooth(aes(x = mean_diff, 
                  y = mean_gpas, 
                  color = group), 
              data = all_sum_hip,
              method = "lm",
              size = 2,
              color = "white",
              se = FALSE) +
  geom_smooth(aes(x = mean_diff, 
                  y = mean_gpas, 
                  color = group), 
              data = all_sum_hip,
              method = "lm",
              alpha = 0.3,
              size = 1.1,
              color = alpha("#F8766D", 0.45),
              se = FALSE) +
  geom_point(aes(x = mean_diff, y = mean_gpas), 
             data = all_sum_hip,
             color = "white",
             size = 3.5) +
  geom_point(aes(x = mean_diff, 
                 y = mean_gpas, 
                 color = group), 
             data = all_sum_hip,
             size = 2.5) +
  scale_x_continuous(limits = c(-4, 4), 
                     breaks = c(-4, -2, 0, 2, 4)) +
  labs(x = "Mean Class-Course Difference",
       y = "Mean GPA",
       color = "Affiliation",
       title = "Average GPA v. Average Difference Between Class & Course Level",
       subtitle = "All Common Courses Between HIP & Non-HIP Students",
       caption = "Source: GSU Data Warehouse") +
  theme_minimal() +
  scale_color_manual(values = c("#F8766D", "grey55", "grey55")) +
  facet_wrap(~year)


all_sum_hip <- all_sum %>% filter(group == "HIP")
all_sum_non <- all_sum %>% filter(group == "Non-HIP")

posn.j <- position_jitter(height = 0, width = 0.1)

viz3 <- ggplot(all_sum_non, aes(x = year, y = mean_gpas, color = group)) +
  geom_jitter() +
  geom_jitter(aes(x = year, y = mean_gpas), 
              color = "white",
              data = all_sum_hip,
              size = 3.75,
              alpha = 0.9, 
              position = posn.j) +
  geom_jitter(aes(x = year, y = mean_gpas, color = group), 
              data = all_sum_hip,
              size = 2.5,
              alpha = 0.9, 
              position = posn.j) +
  labs(x = "Year",
       y = "Mean GPA",
       color = "Affiliation",
       title = "Mean GPA by Calendar Year",
       subtitle = "HIP-Affiliated & Non-HIP Students",
       caption = "Source: GSU Data Warehouse") +
  theme_minimal() +
  scale_color_manual(values = c("#F8766D", alpha("grey70", 0.1)))


main_mn_ts_yr_viz <- main_mn_ts_yr %>%
  rename(HIP = Treatment,
         `Non-HIP` = Control) %>%
  gather(key = Affiliation, value = GPA, `Non-HIP`:HIP) %>%
  select(-Difference) %>%
  mutate(Year = as.numeric(Year),
         Level = factor(Level),
         Affiliation = factor(Affiliation))

viz4 <- ggplot(main_mn_ts_yr_viz, aes(x = Year, 
                              y = GPA, 
                              color = Affiliation)) +
  geom_path(size = 1.05) +
  facet_wrap(~ factor(x = Level, 
                      levels = c("Freshman",
                                 "Sophomore",
                                 "Junior",
                                 "Senior"), 
                      ordered = TRUE)) + 
  scale_y_continuous(limits = c(2, 4)) +
  labs(title = "Mean GPA by Calendar Year & Class",
       subtitle = "All Common Courses Between HIP & Non-HIP Students",
       caption = "Source: GSU Data Warehouse",
       y = "Mean GPA") +
  theme_minimal()



# Finalize Charts & Graphs

lvls_diff_tbl <- lvls_diff %>%
  select(-total, -proportion) %>%
  spread(key = group, value = courses) %>%
  mutate(hip_prop = HIP / sum(HIP, na.rm = TRUE),
         non_prop = `Non-HIP` / sum(`Non-HIP`, na.rm = TRUE)) %>%
  rename(hip_crs = HIP,
         non_crs = `Non-HIP`) %>%
  mutate(hip_crs = ifelse(is.na(hip_crs), 0, hip_crs),
         hip_prop = ifelse(is.na(hip_prop), 0, hip_prop),
         dif_prop = hip_prop - non_prop,
         hip_crs = comma(hip_crs),
         non_crs = comma(non_crs),
         hip_prop = percent(hip_prop),
         non_prop = percent(non_prop),
         dif_prop = percent(dif_prop),
         dif_prop = ifelse(grepl("^-", dif_prop), 
                           dif_prop, 
                           paste0("+", dif_prop)))  %>%
  select(level_diff, hip_crs, hip_prop, non_crs, non_prop, dif_prop) %>%
  rename("Course Difference" = level_diff,
         "HIP Courses" = hip_crs,
         "HIP Proportion (%)" = hip_prop,
         "Non-HIP Courses" = non_crs,
         "Non-HIP Proportion (%)" = non_prop,
         "Difference (%)" = dif_prop)
  
lvls_diff_yr_tbl <- lvls_diff_yr %>%
  select(-total, -proportion, -year_abb) %>%
  spread(key = group, value = courses) %>%
  rename(hip_crs = HIP,
         non_crs = `Non-HIP`) %>%
  mutate(hip_crs = ifelse(is.na(hip_crs), 0, hip_crs),
         non_crs = ifelse(is.na(non_crs), 0, non_crs),
         hip_prop = hip_crs / sum(hip_crs),
         non_prop = non_crs / sum(non_crs, na.rm = TRUE),
         diff = hip_prop - non_prop,
         hip_crs = comma(hip_crs),
         non_crs = comma(non_crs),
         hip_prop = percent(hip_prop),
         non_prop = percent(non_prop),
         diff = percent(diff),
         diff = ifelse(grepl("^-", diff), 
                       diff, 
                       paste0("+", diff))) %>%
  select(year, level_diff, hip_crs, hip_prop, non_crs, non_prop, diff) %>%
  rename("Year" = year,
         "Course Difference" = level_diff,
         "HIP Courses" = hip_crs,
         "HIP Proportion (%)" = hip_prop,
         "Non-HIP Courses" = non_crs,
         "Non-HIP Proportion (%)" = non_prop,
         "Difference (%)" = diff)


main_md_ts_all_tbl <- main_md_ts_all %>%
  rename("HIP Median" = Treatment,
         "Non-HIP Median" = Control,
         "Median Difference (%)" = Difference)

main_mn_ts_all_tbl <- main_mn_ts_all %>%
  rename("HIP Mean" = Treatment,
         "Non-HIP Mean" = Control,
         "Mean Difference (%)" = Difference)

gpa_mean_median_class_tbl <- left_join(main_mn_ts_all_tbl, 
                                       main_md_ts_all_tbl)

main_mn_ts_yr_tbl <- main_mn_ts_yr %>%
  rename("HIP Mean" = Treatment,
         "Non-HIP Mean" = Control,
         "Mean Difference (%)" = Difference) %>%
  mutate(`Mean Difference (%)` = gsub(" ", "", `Mean Difference (%)`))

main_md_ts_yr_tbl <- main_md_ts_yr %>%
  rename("HIP Mean" = Treatment,
         "Non-HIP Mean" = Control,
         "Mean Difference (%)" = Difference) %>%
  mutate(`Mean Difference (%)` = gsub(" ", "", `Mean Difference (%)`))

gpa_mean_median_class_yr_tbl <- left_join(main_mn_ts_yr_tbl, 
                                          main_md_ts_yr_tbl)

regc_md_tbl <- regc_md %>%
  rename("Course Level" = Level,
         "HIP Median" = Treatment,
         "Non-HIP Median" = Control,
         "Median Difference (%)" = Difference) %>%
  mutate(`Median Difference (%)` = gsub(" ", "", `Median Difference (%)`))

regc_mn_tbl <- regc_mn %>%
  rename("Course Level" = Level,
         "HIP Mean" = Treatment,
         "Non-HIP Mean" = Control,
         "Mean Difference (%)" = Difference) %>%
  mutate(`Mean Difference (%)` = gsub(" ", "", `Mean Difference (%)`))

regc_mean_med_tbl <- left_join(regc_mn_tbl, regc_md_tbl)



# FINAL TABLES & GRAPHICS

viz1
viz2
viz3
viz4

lvls_diff_tbl
lvls_diff_yr_tbl
gpa_mean_median_class_tbl
gpa_mean_median_class_yr_tbl
regc_mean_med_tbl

save.image(file = "hip_final.RDATA")



# WRITE FINAL OBJECTS

ggsave(filename = "viz1.jpg", plot = viz1, width = 8, height = 4)
ggsave(filename = "viz2.jpg", plot = viz2, width = 8, height = 5)
ggsave(filename = "viz3.jpg", plot = viz3, width = 8, height = 4)
ggsave(filename = "viz4.jpg", plot = viz4, width = 8, height = 4)

write_csv(lvls_diff_tbl, "lvls_diff_tbl.csv")
write_csv(lvls_diff_yr_tbl, "lvls_diff_yr_tbl.csv")
write_csv(gpa_mean_median_class_tbl, "gpa_mean_median_class_tbl.csv")
write_csv(gpa_mean_median_class_yr_tbl, "gpa_mean_median_class_yr_tbl.csv")
write_csv(regc_mean_med_tbl, "regc_mean_med_tbl.csv")
