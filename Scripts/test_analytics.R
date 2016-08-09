require(googlesheets)
require(dplyr)
require(tidyr)
require(stringr)
require(lubridate)
require(rio)


################################################################################ 
##                   ~~~ Download the key for the test ~~~                    ##
################################################################################
Key <- gs_title("HS Responses ('16)") %>% gs_read_csv("HS_Test Key") %>% 
  select(QID, Answer, Question, A:`F`) %>%
  gather(Letter, Choice, A:`F`) %>% filter(!is.na(Choice)) %>%
  mutate(
    Topic = str_extract(QID, "\\[..\\]"),
    Topic = str_replace_all(Topic, "\\[|\\]", ""),
    QID  = str_replace(QID, " \\[..\\]", "")) %>% 
  mutate(Topic = str_replace_all(Topic, "\\[|\\]", "")) %>% 
  select(QID, Topic, Answer, Letter, Choice, Question) %>%
  left_join(
    data_frame(
      Topic = c("CP", "ID", "MB", "NS", "PH"),
      TopicFull = c("Cardiopulmonary", "Infectious Disease",
                    "Microbiology", "Neuroscience", "Pharmacy"))) %>% 
  select(QID, Topic=TopicFull, Answer, Letter, Choice, Question)
  
Key %>% export("HS/Data/Tests/Key.csv")

################################################################################ 
##                 ~~~ Download the HS pre & post test ~~~                    ##
################################################################################
Before <- gs_title("HS Responses ('16)") %>% gs_read_csv("HS_Test-Pre") %>%
  select(-Timestamp, -Score, -`Camper Name`, -`Cohort ID`) %>% 
  gather(QID, Response, -SID) %>%
  filter(QID!="Q08 [NS]") %>%  # filter out duplicate
  mutate(QID  = str_replace(QID, " \\[..\\]", ""))
Before %>% export("HS/Data/Tests/Before.csv")

After <- gs_title("HS Responses ('16)") %>% gs_read_csv("HS_Test-Post") %>%
  select(SID=`Camper ID`, `Q01 [PH]`:`Q20 [ID]`) %>% 
  gather(QID, Response, -SID) %>%
  filter(QID!="Q08 [NS] DONT ANSWER") %>%  # filter out duplicate
  mutate(QID  = str_replace(QID, " \\[..\\]", ""))
After %>% export("HS/Data/Tests/After.csv")


