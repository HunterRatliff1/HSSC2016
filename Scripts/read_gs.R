require(googlesheets)
require(dplyr)
require(tidyr)
require(stringr)
require(rio)

# ------------------------------------------------------------------------------
# First:  Grab all the test data from the Google Sheets 
## The correct answer key
gs_title("HS Responses ('16)") %>% gs_read_csv("Key") %>% export("Data/HS/Key.csv")

gs_title("MS Responses ('16)") %>% gs_read_csv("Key") %>% export("Data/MS/Key.csv")


## Pre-test results (make them tidy)
gs_title("HS Responses ('16)") %>% gs_read_csv("Test-Pre") %>% 
  select(UID, Q01:Q20, -Q08) %>% gather(QID, Pre, -UID) %>%
  export("Data/HS/Pre/Test.csv")

gs_title("MS Responses ('16)") %>% gs_read_csv("Test-Pre") %>% 
  select(UID, Q01:Q20, -Q13) %>% gather(QID, Pre, -UID) %>%
  export("Data/MS/Pre/Test.csv")


## Post-test results (make them tidy)
gs_title("HS Responses ('16)") %>% gs_read_csv("Test-Post") %>% 
  select(UID, Q01:Q20, -Q08) %>% gather(QID, Post, -UID) %>%
  export("Data/HS/Post/Test.csv")

gs_title("MS Responses ('16)") %>% gs_read_csv("Test-Post") %>% 
  select(UID, Q01:Q20, -Q13) %>% gather(QID, Post, -UID) %>%
  export("Data/MS/Post/Test.csv")


# ------------------------------------------------------------------------------
# Second:  Grab subsets of the survey data from the Google Sheets 
## Grab the free response data (Pre-survey)
gs_title("HS Responses ('16)") %>% gs_read_csv("Info-Pre") %>%
  select(UID, Q01, Q02, Q03, Q06a, Q06b, Q10) %>%
  export("Data/HS/Pre/Written.csv")

gs_title("MS Responses ('16)") %>% gs_read_csv("Info-Pre") %>%
  select(UID, Q01, Q02, Q03a, Q03b, Q09, Q15a, Q15b, Q16a, Q16b) %>%
  export("Data/MS/Pre/Written.csv")


## Grab the free response data (Post-survey)
gs_title("HS Responses ('16)") %>% gs_read_csv("Info-Post") %>%
  select(UID, Q03, Q04:Q09, Q11, Q18, Q19) %>%
  export("Data/HS/Post/Written.csv")

gs_title("MS Responses ('16)") %>% gs_read_csv("Info-Post") %>%
  select(UID, Q01, Q04, Q11) %>%
  export("Data/MS/Post/Written.csv")

###    ---  OPTIONAL  --- 
###
### Add the question's text to the data.frame as a label using `Hmisc` and
### save as a .RDS file. 
###   NOTE: In order to read in these files, `Hmisc` needs to be loaded
# require(Hmisc)
# hs0 <- import("Data/HS/Pre/Written.csv")
# label(hs0$Q01)  <- "Why attending camp"
# label(hs0$Q02)  <- "Expectaions for camp"
# label(hs0$Q03)  <- "Post-college plans"
# label(hs0$Q06a) <- "favSubject"
# label(hs0$Q06b) <- "favSubject why"
# label(hs0$Q10)  <- "Other camps"
# hs0 %>% export("Data/HS/Pre/Written.RDS")
# rm(hs0)
# 
# hs1 <- import("Data/HS/Post/Written.csv")
# label(hs1$Q03) <- "College major"
# label(hs1$Q04) <- "What'd you take away"
# label(hs1$Q05) <- "favActivity"
# label(hs1$Q06) <- "!favActivity"
# label(hs1$Q07) <- "Experience motivate persue HS"
# label(hs1$Q08) <- "Switch HS career"
# label(hs1$Q09) <- "haveShadowed"
# label(hs1$Q11) <- "Meeting topics"
# label(hs1$Q18) <- "Areas of improvement"
# label(hs1$Q19) <- "Anything else"
# hs1 %>% export("Data/HS/Post/Written.RDS")
# rm(hs1)
# 
# ms0 <- import("Data/MS/Pre/Written.csv")
# label(ms0$Q01)  <- "Why attending camp"
# label(ms0$Q02)  <- "Life plans"
# label(ms0$Q03a) <- "HS excited"
# label(ms0$Q03b) <- "HS afraid"
# label(ms0$Q09)  <- "Other camps"
# label(ms0$Q15a) <- "favSubject"
# label(ms0$Q15b) <- "favSubject why"
# label(ms0$Q16a) <- "!favSubject"
# label(ms0$Q16b) <- "!favSubject why"
# ms0 %>% export("Data/MS/Pre/Written.RDS")
# rm(ms0)
# 
# ms1 <- import("Data/MS/Post/Written.csv")
# label(ms1$Q01) <- "Switch HS career"
# label(ms1$Q04) <- "Areas of improvement"
# label(ms1$Q11) <- "Anything else"
# ms1 %>% export("Data/MS/Post/Written.RDS")
# rm(ms1)

# ------------------------------------------------------------------------------
# Third:  
## Start with HS pre-camp
gs_title("HS Responses ('16)") %>% gs_read_csv("Info-Pre") %>%
  select(UID, 
         # Factors/CSV factors
         HS=Q04, District=ISD, Gender=Q17, Edu.mom=Q13a, 
         Edu.dad=Q14a, Lang=Q16, Race=Q18, foundVia=Q07, 
         # Numeric
         EO.mom=Q13c, EO.dad=Q14c, PrHS=Q08, 
         CareerKnow=Q12, Prepared=Q11, Grade=Q05,
         # Text
         favSub=Q06a, Camps=Q10, Plans=Q03) %>%
  export("Data/HS/Pre/Survey.csv")

## HS post-camp
gs_title("HS Responses ('16)") %>% gs_read_csv("Info-Post") %>%
  select(UID, 
         # Numeric
         IncPrHS=Q14, NewCareers=Q13, ImportHS=Q15,
         Reccomend=Q16, Overall=Q17, MorePrep=Q12,
         # Factors/CSV factors
         Meeting.AM=Q10a, Meeting.PM=Q10b, 
         # Text
         Major=Q03) %>%
  export("Data/HS/Post/Survey.csv")

## MS pre-camp
gs_title("MS Responses ('16)") %>% gs_read_csv("Info-Pre") %>%
  select(UID, 
         # Factors/CSV factors
         MS=Q04,  District=ISD, Gender=Q13, Edu.mom=Q10a, 
         Edu.dad=Q11a, Lang=Q12, Race=Q14, foundVia=Q05, 
         # Numeric
         EO.mom=Q10c, EO.dad=Q11c, PrHS=Q07, 
         CareerKnow=Q06, Prepared=Q08, 
         # Text
         favSub=Q15a, Camps=Q09, Plans=Q02, HS.afraid=Q03b) %>%
  export("Data/MS/Pre/Survey.csv")

## MS post-camp
gs_title("MS Responses ('16)") %>% gs_read_csv("Info-Post") %>%
  select(UID, 
         # Numeric
         IncPrHS=Q07, NewCareers=Q05, ImportHS=Q08,
         Reccomend=Q09, Overall=Q10, focus=Q06) %>%
  export("Data/MS/Post/Survey.csv")


# ------------------------------------------------------------------------------
# Test:  Add in the test data
## High School
import("Data/HS/Pre/Survey.csv") %>%
  full_join({
    import("Data/HS/Pre/Test.csv", setclass="tbl_df") %>% 
      full_join(
        select(import("Data/HS/Key.csv"), QID, Ans)) %>%
      mutate(Score0=if_else(Pre==Ans, 1, 0)) %>%
      group_by(UID) %>%
      summarise(Score0 = sum(Score0, na.rm=T)/n()) %>%
      filter(!is.na(UID))
    }) %>% export("Data/HS/Pre/Survey.csv")
import("Data/HS/Post/Survey.csv") %>%
  full_join({
    import("Data/HS/Post/Test.csv", setclass="tbl_df") %>% 
      full_join(
        select(import("Data/HS/Key.csv"), QID, Ans)) %>%
      mutate(Score=if_else(Post==Ans, 1, 0)) %>%
      group_by(UID) %>%
      summarise(Score = sum(Score, na.rm=T)/n()) %>%
      filter(!is.na(UID))
  }) %>% export("Data/HS/Post/Survey.csv")
# import("Data/HS/Pre/Survey.csv") %>% glimpse()
# import("Data/HS/Post/Survey.csv") %>% glimpse()
# full_join(
#   import("Data/HS/Pre/Survey.csv"),
#   import("Data/HS/Post/Survey.csv")
# ) %>% View()
  
## Middle School
import("Data/MS/Pre/Survey.csv") %>%
  full_join({
    import("Data/MS/Pre/Test.csv", setclass="tbl_df") %>% 
      full_join(
        select(import("Data/MS/Key.csv"), QID, Ans)) %>%
      mutate(Score0=if_else(Pre==Ans, 1, 0)) %>%
      group_by(UID) %>%
      summarise(Score0 = sum(Score0, na.rm=T)/n()) %>%
      filter(!is.na(UID))
  }) %>% export("Data/MS/Pre/Survey.csv")
import("Data/MS/Post/Survey.csv") %>%
  full_join({
    import("Data/MS/Post/Test.csv", setclass="tbl_df") %>% 
      full_join(
        select(import("Data/MS/Key.csv"), QID, Ans)) %>%
      mutate(Score=if_else(Post==Ans, 1, 0)) %>%
      group_by(UID) %>%
      summarise(Score = sum(Score, na.rm=T)/n()) %>%
      filter(!is.na(UID))
  }) %>% export("Data/MS/Post/Survey.csv")
# import("Data/MS/Pre/Survey.csv") %>% glimpse()
# import("Data/MS/Post/Survey.csv") %>% glimpse()
# full_join(
#   import("Data/MS/Pre/Survey.csv"),
#   import("Data/MS/Post/Survey.csv")
# ) %>% View()




# theme_ <- theme_fivethirtyeight() + 
#   theme(axis.title=element_text(), strip.background=element_rect(fill="#DDDDDD")) 
# 
# getOpts <- function(source.df, col_name) {
#   require(dplyr); require(stringr);
#   
#   source.df[col_name] %>% unlist() %>% 
#     str_split(", ") %>% unlist() %>% unique() %>% 
#     na.omit() %>% as.character()
# }
