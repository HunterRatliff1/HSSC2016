require(googlesheets)
require(dplyr)
require(tidyr)
require(stringr)
require(rio)

# First:  Grab all the data from the Google Sheet and rename some vars ---------
## The correct answer key
Key <- gs_title("MS Responses ('16)") %>% gs_read_csv("Key")


## Pre-test results (make them tidy)
t_pre <- gs_title("MS Responses ('16)") %>% gs_read_csv("Test-Pre") %>% 
  select(UID, Q01:Q20, -Q13) %>% gather(QID, Pre, -UID)


## Post-test results (make them tidy)
t_post <- gs_title("MS Responses ('16)") %>% gs_read_csv("Test-Post") %>% 
  select(UID, Q01:Q20, -Q13) %>% gather(QID, Post, -UID) 


## School district sheet
Schools <- gs_title("MS Responses ('16)") %>% gs_read_csv("Schools") %>% 
  select(UID, School, District) %>% tbl_df()

## Pre-camp survey (rename vars & drop the school name)
i_pre <- gs_title("MS Responses ('16)") %>% gs_read_csv("Info-Pre") %>% 
  rename(Counselor=`Your name`, Name=`Student name`, 
         Gender=Q13, Race=Q14, Lang=Q12,
         wEdu=Q10a, wOcc=Q10b, mEdu=Q11a, mOcc=Q11b, 
         subLike=Q15a, subDis=Q16a, subLike.why=Q15b, subDis.why=Q16b,
         prevCamps=Q09, foundVia=Q05, whyThere=Q01, plans=Q02,
         HS.excited=Q03a, HS.fear=Q03b, Knowledge=Q06,
         prCareer=Q07, Prepared=Q08) %>%
  select(-Timestamp, -Q15, -Q16, -Q04) %>% 
  inner_join(Schools) # Join in the school info

## Post-camp survey (rename vars & drop the school name)
i_post <- gs_title("MS Responses ('16)") %>% gs_read_csv("Info-Post") %>% 
  rename(Counselor=`Your name`, Name=`Student name`) %>%
  select(-Timestamp) %>% tbl_df()


# Second:  Export CSVs to the `Data` subdirectory of the `MS` directory --------
#    Note: This assumes that you're running scripts with the wd == `HSSSC2016`

## Save the key
Key %>% export("MS/Data/Key.csv")   

## Save the surveys, dropping the student's name
i_pre %>%  select(-Name) %>% export("MS/Data/Pre/Survey.csv")
i_post %>% select(-Name) %>% export("MS/Data/Post/Survey.csv")

## Save the test scores
t_pre %>%  export("MS/Data/Pre/Tests.csv")
t_post %>% export("MS/Data/Post/Tests.csv")



# Third:  Make a df with the before & after scores & save it -------------------
Tests <- t_pre %>%
  full_join(t_post) %>%
  inner_join(select(Key, QID, Ans)) %>%
  mutate(
    Score0 = if_else(Pre==Ans, 1, 0),
    ScoreF = if_else(Post==Ans, 1, 0)
  )

Tests %>% export("MS/Data/Tests.csv")

# Fourth: 
i_pre %>% select(-Name) %>%
  left_join({
    Tests %>% group_by(UID) %>% summarise(
      Score0=sum(Score0)/n(),
      ScoreF=sum(ScoreF)/n())
  }) %>%
  full_join(select(i_post, -starts_with("Q02"), -starts_with("Q03"))) %>%
  export("MS/Shiny/Data/Survey.csv")
  # View()


# df %>% export("MS/Data/Pre/Survey.csv")



# Score the pre-test
require(ggplot2)
import("MS/Data/Test.csv", setclass="tbl_df") %>% 
  mutate(Before = if_else(Pre==Ans, 1, 0), After = if_else(Post==Ans, 1, 0)) %>%
  group_by(UID) %>% 
  summarise(Before = sum(Before, na.rm=T)/n(), After = sum(After, na.rm=T)/n()) %>%
  # right_join(df) %>% 
  
  # qplot(data=., x=Before) + xlim(c(0.001, 1))
  qplot(data=., x=After) + xlim(c(0.001, 1))


# 
# source("MS/Shiny/ParseCSV.R")
# 
# ParseCSV(df, col_name="Q14", id_col = "UID")
# ParseCSV(df, col_name="Q14", id_col = "UID", tidy = T)
# 
# ParseCSV(df, col_name="Q12", tidy = T)
# 
# ParseCSV(df, col_name="Q15a", id_col = "UID") %>% gather(Favorite, Class, -UID) %>%
#   mutate(Class=as.numeric(Class)) %>% filter(!is.na(Class)) %>%
#   group_by(Favorite) %>% summarise(n=sum(Class))
# ParseCSV(df, col_name="Q14", id_col = "UID") %>% gather(Favorite, Class, -UID) %>%
#   mutate(Class=as.numeric(Class)) %>% filter(!is.na(Class)) %>%
#   spread(Favorite, Class) %>% 
#   select(-UID)

# devtools::install_github("timelyportfolio/d3vennR")
require(d3vennR)  
  
venn_tooltip <- function( venn ){
    venn$x$tasks[length(venn$x$tasks)+1] <- list(
      htmlwidgets::JS('
                      function(){
                      var div = d3.select(this);
                      
                      // add a tooltip
                      var tooltip = d3.select("body").append("div")
                      .attr("class", "venntooltip")
                      .style("position", "absolute")
                      .style("text-align", "center")
                      .style("width", 128)
                      .style("height", 16)
                      .style("background", "#333")
                      .style("color","#ddd")
                      .style("padding","2px")
                      .style("border","0px")
                      .style("border-radius","8px")
                      .style("opacity",0);
                      
                      div.selectAll("path")
                      .style("stroke-opacity", 0)
                      .style("stroke", "#fff")
                      .style("stroke-width", 0)
                      
                      // add listeners to all the groups to display tooltip on mousover
                      div.selectAll("g")
                      .on("mouseover", function(d, i) {
                      
                      // sort all the areas relative to the current item
                      venn.sortAreas(div, d);
                      
                      // Display a tooltip with the current size
                      tooltip.transition().duration(400).style("opacity", .9);
                      tooltip.text(d.size);
                      
                      // highlight the current path
                      var selection = d3.select(this).transition("tooltip").duration(400);
                      selection.select("path")
                      .style("stroke-width", 3)
                      .style("fill-opacity", d.sets.length == 1 ? .4 : .1)
                      .style("stroke-opacity", 1);
                      })
                      
                      .on("mousemove", function() {
                      tooltip.style("left", (d3.event.pageX) + "px")
                      .style("top", (d3.event.pageY - 28) + "px");
                      })
                      
                      .on("mouseout", function(d, i) {
                      tooltip.transition().duration(400).style("opacity", 0);
                      var selection = d3.select(this).transition("tooltip").duration(400);
                      selection.select("path")
                      .style("stroke-width", 0)
                      .style("fill-opacity", d.sets.length == 1 ? .25 : .0)
                      .style("stroke-opacity", 0);
                      });
                      }
                      ')
      )
    venn
    }  
  
  
venn_tooltip(d3vennR(
  data = list(
    list(sets = list("ELA"), size=9),
    list(sets = list("Engineering"), size=1),
    list(sets = list("History"), size=4),
    list(sets = list("Math"), size=29),
    list(sets = list("Science"), size=37),
    list(sets = list("Social Studies"), size=2),
    list(sets = list("Technology"), size=1),
    list(sets = list("VAPA"), size=3),
    list(sets = c("ELA", "Engineering"), size=0),
    list(sets = c("ELA", "History"), size=0),
    list(sets = c("ELA", "Math"), size=0),
    list(sets = c("ELA", "Science"), size=3),
    list(sets = c("ELA", "Social Studies"), size=0),
    list(sets = c("ELA", "Technology"), size=0),
    list(sets = c("ELA", "VAPA"), size=0),
    list(sets = c("Engineering", "History"), size=0),
    list(sets = c("Engineering", "Math"), size=1),
    list(sets = c("Engineering", "Science"), size=0),
    list(sets = c("Engineering", "Social Studies"), size=0),
    list(sets = c("Engineering", "Technology"), size=0),
    list(sets = c("Engineering", "VAPA"), size=0),
    list(sets = c("History", "Math"), size=0),
    list(sets = c("History", "Science"), size=1),
    list(sets = c("History", "Social Studies"), size=0),
    list(sets = c("History", "Technology"), size=0),
    list(sets = c("History", "VAPA"), size=0),
    list(sets = c("Math", "Science"), size=5),
    list(sets = c("Math", "Social Studies"), size=0),
    list(sets = c("Math", "Technology"), size=0),
    list(sets = c("Math", "VAPA"), size=1),
    list(sets = c("Science", "Social Studies"), size=0),
    list(sets = c("Science", "Technology"), size=0),
    list(sets = c("Science", "VAPA"), size=1),
    list(sets = c("Social Studies", "Technology"), size=0),
    list(sets = c("Social Studies", "VAPA"), size=0),
    list(sets = c("Technology", "VAPA"), size=0)
  )))
  


jsonExport <- jsonlite::toJSON(list(
  list("sets"= list(0), "label"= "American Indian or Alaska Native", "size"= 1),
  list("sets"= list(1), "label"= "Asian", "size"= 5),
  list("sets"= list(2), "label"= "Black or African American", "size"= 14),
  list("sets"= list(3), "label"= "Hispanic or Latina/o", "size"= 24),
  list("sets"= list(4), "label"= "Native Hawaiian or Other Pacific Islander", "size"= 1),
  list("sets"= list(5), "label"= "White", "size"= 30),
  list("sets"= list(6), "label"= "Some other option", "size"= 7),
  # American Indian or Alaska Native
  list("sets"= list(0, 1), "size"= 0),
  list("sets"= list(0, 2), "size"= 1),
  list("sets"= list(0, 3), "size"= 0),
  list("sets"= list(0, 4), "size"= 0),
  list("sets"= list(0, 5), "size"= 0),
  list("sets"= list(0, 6), "size"= 0),
  # Asian
  list("sets"= list(1, 2), "size"= 0),
  list("sets"= list(1, 3), "size"= 0),
  list("sets"= list(1, 4), "size"= 0),
  list("sets"= list(1, 5), "size"= 2),
  list("sets"= list(1, 6), "size"= 1),
  # Black or African American
  list("sets"= list(2, 3), "size"= 1),
  list("sets"= list(2, 4), "size"= 1),
  list("sets"= list(2, 5), "size"= 0),
  list("sets"= list(2, 6), "size"= 0),
  # Hispanic or Latina/o
  list("sets"= list(3, 4), "size"= 0),
  list("sets"= list(3, 5), "size"= 0),
  list("sets"= list(3, 6), "size"= 0),
  # Native Hawaiian or Other Pacific Islander
  list("sets"= list(4, 5), "size"= 1),
  list("sets"= list(4, 6), "size"= 1),
  # White
  list("sets"= list(5, 6), "size"= 1),
  
  list("sets"= list(2, 3, 4), "size"= 1)
))

jsonExport %>% write(file="MS/Data/Pre-Q14.JSON")

venn_tooltip(d3vennR(data = jsonlite::fromJSON("MS/Data/Pre-Q14.JSON", simplifyDataFrame=F)))
