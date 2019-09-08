if(!require(dplyr)) install.packages("dplyr")
if(!require(stringr)) install.packages("stringr")

getOpts <- function(source.df, col_name) {
  require(dplyr); require(stringr);
  
  source.df[col_name] %>% unlist() %>% 
    str_split(", ") %>% unlist() %>% unique() %>% 
    na.omit() %>% as.character()
}





ParseCSV <- function(source.df, col_name="", id_col="UID", tidy=T){
  require(dplyr); require(stringr);
  
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
  
  if(id_col %in% names(source.df)) {
    ## If the id_col is present in parent data.frame, 
    ## add it to the begining of the list to be coerced 
    ## into our final output.
    match.df <- c(list(id_col=source.df[id_col]), match.list) %>%
     data.frame(check.names=F) %>% tbl_df() 
    
    if(tidy) {
      match.df <- gather(match.df, col_name, Found, 2:ncol(match.df)) 
      names(match.df) <- c(id_col, col_name, "Found")
    }
    return(match.df)
    
  } else({
    ## Otherwise, just coerce the data.frame & return
    warning(paste0("`", id_col, "` was not found in parent data.frame"))
    match.list %>% data.frame(check.names=F) %>% tbl_df() %>% return()
  })
}
