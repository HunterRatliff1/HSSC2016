require(devtools)
require(dplyr)
require(rio)

# devtools::install_github("lmullen/tokenizers")
# devtools::install_github("juliasilge/tidytext")
require(tokenizers)
require(tidytext)

import("Data/Merged/HS-text.RDS") %>% 
  unnest_tokens(word, HS.motivate) %>%
  anti_join(stop_words) %>%
  tbl_df() %>%
  count(word, sort=T)

require(FactoMineR)
df <- HS %>% mutate(
    EO = as.factor(EO),
    Shadowed = as.factor(Shadowed)) %>%
  select(1,
  11,20,
  3,5:8,10,12,13,19,23:25) 

df[4:15] <- df[4:15] %>% lapply(as.character) %>% data.frame(stringsAsFactors=F)
df %>% select(-2, -3) %>% # str()
  MCA(quali.sup=c(2:10))
    # quanti.sup=c(2,3),    # vector of indexes of continuous supplementary variables
    

# df %>%  MCA(quali.sup=c(3:10)) 
df %>% MCA(quali.sup=c(1,10), quanti.sup=c(2,3)) %>%
  
  plot.MCA(invisible=c("ind","quali.sup"), cex=0.7)
  plot.MCA(invisible=c("ind"))
  plot.MCA(invisible=c("ind", "var"))
  