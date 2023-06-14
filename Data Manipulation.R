library(tidyverse)
library(usethis)
library(ggplot2)
library(dplyr)
library(plotly)

nature_of_work <- read_csv("UKHSA dataset.csv", na = c("U", "Z0 - Missing data" ,"NA", "Not Known" ,"Not applicable", "-1","*",".", "", "NULL"))
#Taking a look at the dataset 
nature_of_work <- nature_of_work %>%
      mutate(
        f_zcohort = as.factor(f_zcohort),
        f_xwrk2020soc1 = factor(f_xwrk2020soc1, levels = c(
          "Managers, directors and senior officials",
          "Professional occupations",
          "Associate professional occupations",
          "Administrative and secretarial occupations",
          "Skilled trades occupations",
          "Caring, leisure and other service occupations",
          "Sales and customer service occupations",
          "Process, plant and machine operatives",
          "Elementary occupations"
        )),
        f_xwrk2007sic1 = factor(f_xwrk2007sic1, levels = c(
          "Agriculture, forestry and fishing",
          "Mining and quarrying",
          "Manufacturing",
          "Electricity, gas, steam and air conditioning supply",
          "Water supply; sewerage, waste management and remediation activities",
          "Construction",
          "Wholesale and retail trade; repair of motor vehicles and motorcycles",
          "Transportation and storage",
          "Accommodation and food service activities",
          "Information and communication",
          "Financial and insurance activities",
          "Real estate activities",
          "Professional, scientific and technical activities",
          "Administrative and support service activities",
          "Public administration and defence; compulsory social security",
          "Education",
          "Human health and social work activities",
          "Arts, entertainment and recreation",
          "Other service activities",
          "Activities of households as employers; undifferentiated goods-and services-producing activities of households for own use",
          "Activities of extraterritorial organisations and bodies"
        )),
        f_xempbasis = factor(f_xempbasis, levels = c(
          "On a permanent/open ended contract",
          "On a fixed-term contract lasting 12 months or longer",
          "On a fixed-term contract lasting less than 12 months",
          "Temping (including supply teaching)",
          "On a zero hours contract",
          "Volunteering",
          "On an internship",
          "Other",
          "Not known"
        )),
        f_providername = factor(f_providername)
      ) %>%
      mutate(study_mode = case_when(f_xqmode01 == "Part-time" ~ 2,
                                    f_xqmode01 == "Full-time" ~ 1)) %>%
      mutate(work_mean_num = case_when(f_wrkmean == "Strongly agree" ~ 5,
                                       f_wrkmean == "Agree" ~ 4,
                                       f_wrkmean == "Neither agree nor disagree" ~ 3,
                                       f_wrkmean == "Disagree" ~ 2,
                                       f_wrkmean == "Strongly disagree" ~ 1)) %>%
      mutate(work_skills_num = case_when(f_wrkskills == "Strongly agree" ~ 5,
                                         f_wrkskills == "Agree" ~ 4,
                                         f_wrkskills == "Neither agree nor disagree" ~ 3,
                                         f_wrkskills == "Disagree" ~ 2,
                                         f_wrkskills == "Strongly disagree" ~ 1)) %>%
      mutate(work_ontrack_num = case_when(f_wrkontrack == "Strongly agree" ~ 5,
                                          f_wrkontrack == "Agree" ~ 4,
                                          f_wrkontrack == "Neither agree nor disagree" ~ 3,
                                          f_wrkontrack == "Disagree" ~ 2,
                                          f_wrkontrack == "Strongly disagree" ~ 1)) %>%
      mutate(danow = (work_ontrack_num + work_skills_num + work_mean_num) / 3) %>%
      distinct(f_zanonymous, .keep_all = TRUE) %>%
      filter(study_mode == 1 | !is.na(work_ontrack_num) | !is.na(work_skills_num) | !is.na(work_mean_num))
  
  