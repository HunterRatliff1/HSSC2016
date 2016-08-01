require(googlesheets)
require(dplyr)
require(tidyr)
require(stringr)
require(lubridate)
require(rio)

################################################################################ 
##                  Read the raw data from the Google Sheets                  ##
################################################################################

## ~~~ Download the key for the HS Pre-Test ~~~
PreKey <- gs_title("HS Responses ('16)") %>% gs_read_csv("HS_Test-Pre Key")


## ~~~ Download the HS Pre-Test and score it ~~~
gs_title("HS Responses ('16)") %>% gs_read_csv("HS_Test-Pre") %>%
  # Drop sensetive columns & make the data tidy
  select(-Timestamp, -Score, -`Camper Name`, -`Cohort ID`) %>% 
  gather(QID, Response, -SID) %>%
  filter(QID!="Q08 [NS]") %>% # filter out duplicate
  
  # Join with the answer key, and preform scoring
  left_join(select(PreKey, QID, Answer)) %>%
  mutate(isCorrect = if_else(Response==Answer, T, F)) %>%
  mutate(Score     = if_else(Response==Answer, "Correct", "Wrong")) %>%
  
  # Seperate out the question topics
  mutate(
    Topic = str_extract(QID, "\\[..\\]"),
    Topic = str_replace_all(Topic, "\\[|\\]", ""),
    QID  = str_replace(QID, " \\[..\\]", "")) %>%
  left_join(
    data_frame(
      Topic = c("CP", "ID", "MB", "NS", "PH"),
      TopicFull = c("Cardiopulmonary", "Infectious Disease",
                    "Microbiology", "Neuroscience", "Pharmacy"))) %>% 
  rename(Topic.Abbv=Topic, Topic=TopicFull) %>% export("HS/Data/Pre/Test.csv")


## ~~~ Download the HS Post-Test and score it ~~~
gs_title("HS Responses ('16)") %>% gs_read_csv("HS_Test-Post") %>%
  # Drop sensetive columns & make the data tidy
  select(SID=`Camper ID`, `Q01 [PH]`:`Q20 [ID]`) %>% 
  gather(QID, Response, -SID) %>%
  filter(QID!="Q08 [NS] DONT ANSWER") %>% # filter out duplicate
  
  # Join with the answer key, and preform scoring
  left_join(select(PreKey, QID, Answer)) %>%
  mutate(isCorrect = if_else(Response==Answer, T, F)) %>%
  mutate(Score     = if_else(Response==Answer, "Correct", "Wrong")) %>%
  
  # Seperate out the question topics
  mutate(
    Topic = str_extract(QID, "\\[..\\]"),
    Topic = str_replace_all(Topic, "\\[|\\]", ""),
    QID  = str_replace(QID, " \\[..\\]", "")) %>%
  left_join(
    data_frame(
      Topic = c("CP", "ID", "MB", "NS", "PH"),
      TopicFull = c("Cardiopulmonary", "Infectious Disease",
                    "Microbiology", "Neuroscience", "Pharmacy"))) %>% 
  rename(Topic.Abbv=Topic, Topic=TopicFull) %>% export("HS/Data/Post/Test.csv")








## ~~~ Download the short answer data from HS Pre-Survey ~~~
gs_title("HS Responses ('16)") %>% 
  gs_read_csv("HS_Info-Pre") %>%
  select(SID, Q1:Q3, Q10, Q6a, Q6b) %>%
  export("HS/Data/Pre/Written.csv")

## ~~~ Download the AP data from HS Pre-Survey ~~~
gs_title("HS Responses ('16)") %>% 
  gs_read_csv("HS_Info-Pre") %>%
  select(SID, contains("Q9")) %>%
  gather(AP, Response, -SID) %>%
  mutate(AP = str_extract(AP, "\\[.+\\]")) %>%
  mutate(AP = str_replace_all(AP, "\\[|\\]", "")) %>%
  filter(Response!="", Response!="Not offered") %>%
  mutate(
    Have = if_else(Response=="Taken", T, F),
    Plan = if_else(Response=="Taken"|Response=="Plan to take", T, F)) %>%
  select(-Response) %>%
  export("HS/Data/AP.csv")

## ~~~ Determine the EO for each response ~~~
# Order of education
edu.levels <- c("N/A", "Primary Education only", "Some high school", 
                "High school graduate", "Some college", "Associate degree", 
                "Bachelor’s degree", "Post graduate degree", "Doctoral degree")

gs_title("HS Responses ('16)") %>% gs_read_csv("HS_Info-Pre") %>%
  # Select the education level 
  select(SID, Q13a, Q13c, Q14a, Q14c) %>%
  mutate(# Q15a = na_if(Q15a, "N/A"),
    Q13a = factor(Q13a, levels = edu.levels, ordered = T),
    Q14a = factor(Q14a, levels = edu.levels, ordered = T)) %>%
  # If single parent household
  mutate(SPH = if_else(Q13a=="N/A"|Q14a=="N/A", T, F)) %>%
  rename(Edu_Mom=Q13a, Edu_Dad=Q14a, 
         EO_Mom=Q13c, EO_Dad=Q14c) %>%
  # Tidy, spread, then cast the data 
  gather(key, value, -SID, -SPH) %>% 
  separate(key, c("Measure", "Person")) %>%
  spread(Measure, value, convert=T) %>%
  # Clean it up again & group by student ID
  mutate(Edu = factor(Edu, levels = edu.levels, ordered = T)) %>%
  filter(!is.na(EO)) %>% group_by(SID, SPH) %>% 
  # Collapse into two numeric vars and an ordered factor
  summarise(
    EO.sum = sum(EO),
    EO = max(EO), 
    Edu = max(Edu)) %>%
  mutate(Edu = factor(Edu, levels = edu.levels, ordered = T)) %>%
  
  # Write a RDS & CSV file
  export("HS/Data/EO.RDS")
import("HS/Data/EO.RDS") %>% export("HS/Data/EO.csv")
  



## ~~~ Download selected variables from HS Pre-Survey ~~~
gs_title("HS Responses ('16)") %>% 
  gs_read_csv("HS_Info-Pre") %>%
  # Select columns
  select(-Timestamp, -`Cohort ID`, -`Camper Name`) %>% 
  select(SID, 
         Q8, Q11, Q12, # Numerical scales
         Q4, Q5, Q17,  # Factors
         Q6a, Q7, Q16, Q18) %>% # CSV factors
         

## >> Languages Spoken at Home (Q16):
## Students listed the languages spoken in their home in decreasing order of 
## frequency. From these responses, five language variables were calculated: 
##   ML - Multilingual household
##   PHLOTE - Primary home language other than english
##   NES - Non-english speaker, meaning meaning english is *not* spoken at all
##   L1 - Primary language
##   L2 - Secondary language
  mutate(
    ML     = str_detect(Q16, ","),
    PHLOTE = if_else(grepl("^Eng", Q16), F, T),
    NES =  if_else(str_detect(tolower(Q16), "english"), F, T)) %>%
  
## >> Gender Identification (Q17): 
## Students were asked to select one or more boxes describing their gender 
## identification. The options the were provided are as follows: 'Female', 
## 'Male', and/or 'I'd prefer not answer'. Additionally, students were allowed 
## to write in their own responses. For the purpose of analysis, all respponses  
## other than male or female have been categorized as "Other".
  mutate(Gender = if_else(Q17!="Male"&Q17!="Female", "Other", Q17)) %>%
  
## > Race/Ethnicity Identification (Q18):
## Students were asked to select one or more boxes describing their racial/
## ethnic identification among the options 'White', 'Hispanic or Latina/o', 
## 'Asian', 'American Indian or Alaska Native', and 'Native Hawaiian or Other 
## Pacific Islander'. Similar to the Gender identification question, respondants 
## also were given the option to write-in responses or select 'I'd prefer not 
## answer'. Given the survey's limitations on this question (e.g. combining the 
## ethnicity question with the race question, the social construction of race)
## these data were simplified into simple factors using the following heuristic:
##  1. If the Hispanic/Latina ethnicity was selected, the response was 
##     categorized as `Hispanic (any combo)` regardless of race
##  2. If white, black, or Asian was the only response given, the response was 
##     categorized as `White alone`, `Black alone`, or `Asian alone`
##  3. All other responses (including write-ins) were aggregated into `Other/2+ 
##     Race ID`
## The detailed Race/Ethnicity ID data are available in the "Demographics" table
  mutate(
    Race = Q18,
    Race = if_else(Race=="White", "White alone", Race),
    Race = if_else(Race=="Black or African American", "Black alone", Race),
    Race = if_else(Race=="Asian", "Asian alone", Race),
    Race = if_else(str_detect(Race, "Hispanic or Latina/o"), 
                   "Hispanic (any combo)", Race),
    Race = if_else(str_detect(Race, ","), "2+ Race ID", Race),
    Race = if_else(str_detect(Race, "alone")|str_detect(Race, "Hispanic"), 
                   Race, "Other/2+ Race ID")) %>%
  
  # Drop these three old variables, make factors, and export
  select(-Q16, -Q17, -Q18) %>% rename(GradeLvl=Q5, School=Q4) %>%
  mutate(
    Race = as.factor(Race),
    Gender = as.factor(Gender),
    School = as.factor(School),
    GradeLvl = as.factor(GradeLvl)) %>% 
  
  # Join in the `EO` table for SES indicator
  left_join(import("HS/Data/EO.RDS")) %>%
  
  # Calculate pre-test scores and join resulting table
  left_join({import("HS/Data/Pre/Test.csv") %>% 
      group_by(SID) %>% summarise(Score = sum(isCorrect)/n())}) %>%
  
  # Calculate post-test scores and join resulting table
  left_join({import("HS/Data/Post/Test.csv") %>% 
      group_by(SID) %>% summarise(Score2 = sum(isCorrect)/n())}) %>%
  
  # Calculate planned AP courses and join
  left_join({import("HS/Data/AP.csv") %>% 
      group_by(SID) %>% summarise(AP = sum(Plan))}) %>%
  # Export
  export("HS/Data/Pre/Survey.RDS")


# PreI <- import("Data/2016/HS/Info-Pre.csv", setclass="tbl_df")

#~# TO DO
#~# 1. Codify jobs as EMP or SCL
#~# 2. Determine EO
#~# 3. Build suplemental table for race


# import("Data/2016/HS/PreTest.csv") %>% glimpse()
