#This R file covers all of the data manipulation steps done to the dataset
#all libraries are loaded in in this file for tidiness
library(tidyverse)
library(usethis)
library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(shiny)
library(shiny.router)
#Loading in the dataset, we recode missing data items to na, we then restrict our data set by removing variables that we are not using. This is done to improve the speed of the app. 
nature_of_work <- read_csv("UKHSA dataset.csv", na = c("U", "Z0 - Missing data" ,"NA", "Not Known" ,"Not applicable", "-1","*",".", "", "NULL"))
nature_of_work <- subset(nature_of_work, select = -c(f_ageonentry, f_imd, f_imd_england, f_imd_ni, f_imd_scotland, f_imd_wales, f_xinstcou01, f_zpitariff, f_zstate_marker, f_nhsorg, f_zmultipleact, zgolocation, zwrkloclaua, zwrklocgr, zwrklocn, zdomlaua, znoempband, xwrk2007sic2, xwrk2020soc3, f_xinstgou01))
#Here we begin manipulating our dataset
nature_of_work <- nature_of_work %>%
      mutate(
        f_zcohort = as.factor(f_zcohort), #Here we change the f_zcohort variable to a factor variable
        f_xwrk2020soc1 = factor(f_xwrk2020soc1, levels = c( #Here we change the f_xwrk2020soc1 variable to a factor variable, and specify the levels, this is so that the variables display in the correct order
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
        f_xwrk2007sic1 = factor(f_xwrk2007sic1, levels = c( #Here we change the f_xwrk2007sic1 variable to a factor variable, and specify the levels, this is so that the variables display in the correct order
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
        f_xempbasis = factor(f_xempbasis, levels = c(  #Here we change the f_xempbasis variable to a factor variable, and specify the levels, this is so that the variables display in the correct order
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
       f_xclass01 = factor(f_xclass01, levels = c( #Here we change the f_xclass01 variable to a factor variable, and specify the levels, this is so that the variables display in the correct order
                                     "First class honours",
                                    "Lower second class honours",
                                    "Third class honours/Pass",
                                    "Upper second class honours")),
        f_xglev501 = factor(f_xglev501), #Here we change the f_xglev501 variable to a factor variable
        f_xjacsa01 = factor(f_xjacsa01), #Here we change the f_xjacsa01 variable to a factor variable
        f_pared = factor(f_pared, levels = c( "Yes", #Here we change the f_pared variable to a factor variable, and specify the levels, this is so that the variables display in the correct order
                                              "No",
                                              "Don't know",
                                              "Information refused")),
        f_sexid = factor(f_sexid, levels = c("Female", #Here we change the f_sexid variable to a factor variable, and specify the levels, this is so that the variables display in the correct order
                                             "Male")),
        f_xethnic01 = factor(f_xethnic01),  #Here we change the f_xethnic01 variable to a factor variable
        f_providername = factor(f_providername) #Here we change the f_providername variable to a factor variable
      ) %>%
  
      mutate(study_mode = case_when(f_xqmode01 == "Part-time" ~ 2, #recoding the mode variable to a numeric value, this is to aid in subsetting the dataset
                                    f_xqmode01 == "Full-time" ~ 1)) %>%
      mutate(work_mean_num = case_when(f_wrkmean == "Strongly agree" ~ 5, #recoding the graduate voice questions to numeric values to create the design and nature of work score
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
      mutate(danow = (work_ontrack_num + work_skills_num + work_mean_num) / 3) %>% # creating the design and nature of work score
      mutate(salary = ifelse((f_xwrksalary >= 12012 & f_xwrksalary <= 80000) & f_zcohort == 2017, #restricting the salary variable 
                         f_xwrksalary,
                         ifelse((f_xwrksalary >= 10233.60 & f_xwrksalary <= 83000) & f_zcohort == 2018,
                                f_xwrksalary,
                                NA))) %>% 
      distinct(f_zanonymous, .keep_all = TRUE) %>% #resticting the dataset to show only distintct individuals
      filter(study_mode == 1 | !is.na(work_ontrack_num) | !is.na(work_skills_num) | !is.na(work_mean_num)) #subsetting the dataset to full time graduates who answered all the GV questions

  