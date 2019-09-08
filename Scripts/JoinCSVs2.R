require(rio)
require(dplyr)
require(tidyr)
require(Hmisc)
require(stringr)

HS <- import("Data/HS-Keys.csv", setclass="tbl_df") %>%
  left_join( import("Data/HS-Mon.csv") ) %>%
  left_join( import("Data/HS-Fri.csv") )

names(HS) <- c("Pre.ID", "Post.ID", "School", "Grade", "Sex", "Race",
               "Lang.Primary", "Lang.Spoken", "HS.Parents", "EO", "Persue.HS", 
               "_why.attending", "_future.plans", "Fav.Subject", 
               "_fav.subject_why", "Found.Via", "_expectaions", "AP.Taken", 
               "AP.Projected", "AP.Class.Taken", "AP.Class.Plan", "_exciting",
               "_wish", "_omit", "_fav.day", "_fav.day_why", "Days.Num",
               "Days.Length", "Refer.Peer", "College", "Major", "_plans", 
               "_HS.motivate", "HS.Before", "HS.After", "_HS.switch", "Shadowed",
               "Shadowed.Who", "Shadowed.Want", "_shadowed.text", "_else",
               "_improvement", "_other")

HS.text <- HS %>% select(Pre.ID, Post.ID, starts_with("_"))
HS      <- HS %>% select(-starts_with("_"))
names(HS.text) <- names(HS.text) %>% stringr::str_replace("_", "")


# HS %>% export("Data/Merged/HS.csv")
# HS.text %>% export("Data/Merged/HS-text.csv")

# label(HS$Lang.Primary) <- NA
label(HS.text$why.attending) <- "In your own words, why are you attending this camp?"
label(HS.text$future.plans)  <- "What do you think you might want to do when you graduate college?"
label(HS.text$expectaions)   <- "Briefly describe your expectations for this year's DMS Health Sciences?"
label(HS.text$exciting)      <- "What was the most exciting part of the camp?"
label(HS.text$wish)          <- "What would you like to have done in the camp?"
label(HS.text$omit) <- "What part of the camp was least rewarding or could be omitted?"
label(HS.text$plans) <- "Other future plans"
label(HS.text$HS.motivate) <- "Did your experience motivate you to pursue a career in the health professions?"
label(HS.text$HS.switch) <- "Did the camp experience make you switch from pursuing one health career to another health career?"
label(HS.text$shadowed.text) <- "Have you ever had an opportunity to shadow, work, or intern in one of the health professions?"
label(HS.text$`else`) <- "Anything else we should know about you?"
label(HS.text$improvement) <- "How can we make this camp better for the future?"

HS.text %>% export("Data/Merged/HS-text.csv")
HS.text %>% export("Data/Merged/HS-text.RDS")

HS$School <- as.factor(HS$School)
HS$Sex    <- as.factor(HS$Sex)
HS$Race   <- as.factor(HS$Race)
HS$Lang.Primary <- as.factor(HS$Lang.Primary)
HS$Lang.Spoken  <- as.factor(HS$Lang.Spoken)
HS$Fav.Subject  <- as.factor(HS$Fav.Subject)
HS$Found.Via    <- as.factor(HS$Found.Via)
HS$Days.Length  <- as.factor(HS$Days.Length)
HS$HS.Before    <- as.factor(HS$HS.Before)
HS$HS.After     <- as.factor(HS$HS.After)

HS$HS.Parents <- ifelse(HS$HS.Parents=="No", F, T)

HS %>% export("Data/Merged/HS.csv")
HS %>% export("Data/Merged/HS.RDS")

fxn <- function(source.df, col_name="AP.Class.Taken", id_col=NA){
  if(!require(stringr)) install.packages("stringr")
  
  # Parse the vector for the available options
  opts <- source.df[col_name] %>% unlist() %>% 
    str_split(", ") %>% unlist() %>% unique() 
  
  # Make a character vector from the list
  list.to.test <- source.df[col_name] %>% unlist() %>% as.character()
  
  # For each category that may be present in the list
  match.list <- opts %>% lapply(function(x) str_detect(list.to.test, x))
  
  # Name the list with each poternral category
  names(match.list) <- make.names(opts)
  
  # Make data.frame and return
  match.df <- match.list %>% data.frame(check.names=F) %>% tbl_df()
  
  if(is.na(id_col)) return(match.df)
  
  match.df[id_col] <- source.df[id_col]
  
  return(match.df)
}
# fxn(HS, "AP.Class.Taken", "Pre.ID") %>% tidyr::gather(AP_Taken, Class, -Pre.ID)%>%
#   filter(Class) %>% right_join(HS) %>%
#   qplot(data=., x=AP_Taken, fill=Sex)

