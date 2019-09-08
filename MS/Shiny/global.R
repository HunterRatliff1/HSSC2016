require(rio)
require(dplyr)
require(stringr)

require(ggplot2)
require(ggthemes)
require(scales)

require(shiny)
require(shinydashboard)
require(shinythemes)


theme_ <- theme_fivethirtyeight() + 
  theme(axis.title=element_text(), strip.background=element_rect(fill="#DDDDDD")) 

getOpts <- function(source.df, col_name) {
  require(dplyr); require(stringr);
  
  source.df[col_name] %>% unlist() %>% 
    str_split(", ") %>% unlist() %>% unique() %>% 
    na.omit() %>% as.character()
}

# source("ParseCSV.R")
# source("MS/Shiny/ParseCSV.R")

df <- import("Data/Survey.csv")
# df <- import("MS/Shiny/Data/Survey.csv")