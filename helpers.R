# Author: Sharan Naribole
# Filename: helpers.R
# H-1B Visa Petitions Dashboard web application to enable exploratory data analysis
# on H-1B Visa applications disclosure data in the period 2011-2016

require(lazyeval)
require(dplyr)

job_filter <- function(df,input_vec) {
  # Function to filter only the rows from dataframe 
  # with Job titles provided in the inputs
  # Inputs:
  # df         : H-1B dataset dataframe
  # input_vec  : vector of job types input
  # Output     : filtered dataframe
  # If no match, returns an empty data frame
  # If the inputs are all equal to "", it returns the complete dataframe
  # A new column JOB_INPUT_CLASS is created to identify the Job Type
  # If multiple job type inputs match with a single row in the dataframe df, the 
  # output contains them in different rows each with distinct JOB_INPUT_CLASS
  
  # If input_vec is empty, return without any filtering
  if(length(input_vec) == 0) {
    return(df %>%
             mutate(JOB_INPUT_CLASS = JOB_TITLE))
  }
  
  new_df <- data.frame()
  
  for(value in input_vec){
    new_df <- rbind(new_df, df %>% 
                      filter(regexpr(value,JOB_TITLE,ignore.case=TRUE) != -1) %>%
                      mutate(JOB_INPUT_CLASS = toupper(value)))
  }
  return(unique(new_df))
}


employer_filter <- function(df, input_vec) {
  # Function to filter only the rows in dataframe with 
  # Employers provided in the inputs
  # Inputs:
  # df         : H-1B dataset dataframe
  # input_vec  : vector of job types input
  # Output     : filtered dataframe
  # If no match, returns an empty data frame
  # If the inputs are all equal to "", it returns the complete dataframe
  # Only difference from job_filter() is that there is no new column created
  if(length(input_vec) == 0) {
    return(df)
  }
  
  new_df <- data.frame()
  
  for(value in input_vec){
    new_df <- rbind(new_df, df %>% 
                      filter(regexpr(value,EMPLOYER_NAME,ignore.case=TRUE) != -1))
  }
  return(unique(new_df))
}
  
find_top <- function(df,x_feature,metric, Ntop = 3) {
  # Function to find the top values in x_feature based on metric value
  # Inputs:
  # df            : filtered dataframe from job_type, location, employer and year range inputs
  # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
  # metric        : metric for data comparison 
  # Output        : list of top values in x_feature based on metric
  arrange_criteria <- interp(~ desc(x), x = as.name(metric))
  
  df %>% 
    group_by_(x_feature) %>% 
    mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
    summarise(TotalApps = n(),
              Wage = median(PREVAILING_WAGE), 
              CertiApps = sum(certified),
              Share = CertiApps/850) %>%
    arrange_(arrange_criteria) -> top_df
  
  top_len <- min(dim(top_df)[1],Ntop)
  
  return(top_df[1:top_len,1])
}

plot_input <- function(df, x_feature, fill_feature, metric,filter = FALSE, ...) {
  # Function to transform the filtered dataframe to one with computed metrics
  # Inputs:
  # df            : filtered dataframe from job_type, location, employer and year range inputs
  # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
  # fill_feature  : additional level of classification; for e.g., Year
  # metric        : metric for data comparison 
  # filter        : logical operator that filters only the rows with x_feature value belonging to top_find() output
  # Output        : dataframe grouped by x_feature and fill_feature with metrics as columns
  
  #Finding out the top across the entire range independent of the fill_feature e.g. Year
  top_x <- unlist(find_top(df,x_feature,metric, ...))
  
  # lazyeval package interp () generates expression that interprets x_feature and metric arguments
  # this is fed into filter_ and arrange_ accordingly
  # Source: https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html
  
  filter_criteria <- interp(~x %in% y, .values = list(x = as.name(x_feature), y = top_x))
  arrange_criteria <- interp(~ desc(x), x = as.name(metric))

  if(filter == TRUE) {
    df %>%
      filter_(filter_criteria) -> df
  }
  
  #Grouping by not just x_feature but also fill_feature
  return(df %>% 
    group_by_(.dots=c(x_feature,fill_feature)) %>% 
    mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
      summarise(TotalApps = n(),
                CertiApps = sum(certified), 
                Wage = median(PREVAILING_WAGE),
                Share = CertiApps/850))
}
  
plot_output <- function(df, x_feature,fill_feature,metric, xlabb,ylabb) {  
  # Function to plot output
  # Inputs:
  # df            : dataframe output of plot_input()
  # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
  # fill_feature  : additional level of classification; for e.g., Year
  # metric        : metric for data comparison 
  # xlabb         : x label
  # ylabb         : y label
  # Output        : ggplot object
  
  # Prevents numbers on plot transforming into scientific notation
  options(scipen = 999)
  
  g <- ggplot(df, aes_string(x=x_feature,y=metric)) +
    geom_bar(stat = "identity", aes_string(fill = fill_feature), position = "dodge") + 
    coord_flip() + xlab(xlabb) + ylab(ylabb) + get_theme()
  
  return(g)
}


map_gen <- function(df,metric,USA,...) {
  # Function to generate map plot for given metric in df 
  # This is laid on top of USA map
  # Inputs:
  # df      : dataframe with metrics, lat, lon, WORKSITE columns
  # metric  : metric for data comparison 
  # USA     : dataframe for US maps with lat, long columns. map_data(map = "usa") from ggplot2
  # Output  : ggplot object
  
  
  # Creating Map Dataframe
  df %>%
    mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
    group_by(WORKSITE,lat,lon) %>%
    summarise(TotalApps = n(),CertiApps = sum(certified), Wage = median(PREVAILING_WAGE)) -> map_df
   
  # # Lat-Long Limits
  # df %>%
  #   summarise(lat_min = min(lat,na.rm=TRUE),
  #             lat_max = max(lat,na.rm=TRUE),
  #             long_min = min(lon,na.rm=TRUE),
  #             long_max = max(lon,na.rm=TRUE)) -> geo_coord

  # Finding top Locations for metric
  top_locations <- unlist(find_top(df,"WORKSITE",metric, ...))
  
  # First layer    : USA Map
  # Second layer   : geom_point() with point alpha and size varying with metric
  # Third layer    : points mapping to top locations using ggrepel package
  g <- ggplot(USA, aes(x=long, y=lat)) + 
    geom_polygon() + xlab("Longitude (deg)") + ylab("Latitude(deg)") + 
    geom_point(data=map_df, aes_string(x="lon", y="lat", label = "WORKSITE", alpha = metric, size = metric), color="yellow") + 
    geom_label_repel(data=map_df %>% filter(WORKSITE %in% top_locations),aes_string(x="lon", y="lat",label = "WORKSITE"),
                     fontface = 'bold', color = 'black',
                     box.padding = unit(0.0, "lines"),
                     point.padding = unit(1.0, "lines"),
                     segment.color = 'grey50',
                     force = 3) +
    # Zoom into the specific location input
    #coord_map(ylim = c(max(geo_coord$lat_min - 5,23), min(geo_coord$lat_max - 5,50)),xlim=c(max(geo_coord$long_min - 5,-130),min(geo_coord$long_max + 5,-65))) +
    # Using the whole USA map
    coord_map(ylim = c(23,50),xlim=c(-130,-65)) +
    get_theme()
  
  return(g)
}

get_theme <- function() {
  # Function for ggplot2 graphics parameters
  return(
    theme(axis.title = element_text(size = rel(1.5)),
          legend.position = "right",
          legend.text = element_text(size = rel(1.5)),
          legend.title = element_text(size=rel(1.5)),
          axis.text.y = element_text(size=rel(1.5),face="bold"),
          axis.text.x = element_text(size=rel(1.5),face="bold")) 
  )
}

split_first <- function(word, split = " ") {
  # Function to obtain first value ina  strsplit
  # Inputs:
  # word      : word to be split
  # split     : split parameter to be passed to strsplit
  return(strsplit(word,split= split)[[1]][1])
}
