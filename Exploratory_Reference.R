library(tidyverse)
library(ggplot2)
library(here)
library(lubridate)
library(readxl)
library(scales)

# Requires that the desired columns have name "metric"
plot_multiple <- function(keys, metric1_dict, metric2_dict, pull, metrics, 
                          met_names, coeff, study_data, IDs = NULL){
  if(is.null(IDs)){
    IDs = sample(keys, 9) 
  }
  coeff = 1.25
  df1 = NULL
  df2 = NULL
  dfdate = NULL
  for(ID in IDs){
    important1 = metric1_dict %>% filter(id == ID)
    important2 = metric2_dict %>% filter(id == ID)
    
    # Group together continuous runs of data (arg for this)
    dates_for_rects = important2 %>%
      arrange(datetime) %>%
      select(datetime) %>%
      mutate(hod = hour(datetime)) %>%
      mutate(night = case_when(hod <= 7 | hod >= 19 ~ 1,
                               TRUE ~ 0))
    
    dates_for_rects$groupings = cumsum(c(0,as.numeric(diff(dates_for_rects$night))!=0))
    
    dates_for_rects = dates_for_rects %>%
      filter(night == 1) %>%
      group_by(groupings) %>%
      summarize(from = first(datetime), to = last(datetime))
    
    important1 = important1 %>%
      arrange(datetime) %>%
      mutate(diff_times = difftime(datetime, lag(datetime), units = "mins")) %>%
      mutate(gap = case_when((diff_times <= 110) | is.na(diff_times) ~ 0,
                             TRUE ~ 1)) %>%
      mutate(grouping = cumsum(gap)) %>%
      select(-c(gap, diff_times))
    
    important2 = important2 %>%
      arrange(datetime) %>%
      mutate(diff_times = difftime(datetime, lag(datetime), units = "mins")) %>%
      mutate(gap = case_when((diff_times <= 7) | is.na(diff_times) ~ 0,
                             TRUE ~ 1)) %>%
      mutate(grouping = cumsum(gap)) %>%
      select(-c(gap, diff_times))
    
    important1$Metric = met_names[1]
    important2$Metric = met_names[2]
    
    # Find Relevant Header Values (arg for names)
    dat = study_data %>%
      filter(ingptid == ID) %>%
      select(pull)
    
    header_data = paste0(as.character(dat), collapse = ", ")
    important1$Head = header_data
    important2$Head = header_data
    dates_for_rects$Head = header_data
    
    capti = paste0(c("Label Data: ", paste0(pull, collapse = ",")), collapse = "")
    
    if(is.null(df1)){
      df1 = important1
      df2 = important2
      dfdate = dates_for_rects
    }
    else{
      df1 = rbind(df1, important1)
      df2 = rbind(df2, important2)
      dfdate = rbind(dfdate, dates_for_rects)
    }
  }
  
  df2[[metrics[2]]] = df2[[metrics[2]]]/coeff
  
  g = ggplot() + 
    geom_line(data = df1, aes_string(x="datetime", y = metrics[1], color = "Metric", group = "grouping")) + 
    geom_line(data = df2, aes_string(x="datetime", y = metrics[2], color = "Metric")) + 
    geom_rect(data = dfdate, aes(xmin = from-1, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.25) + 
    facet_wrap(~Head, scales = "free_x") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1)) +
    scale_x_datetime(breaks = date_breaks("4 hour"), labels = date_format("%H:%M")) +
    scale_y_continuous(
      # Features of the first axis
      name = met_names[1],
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*coeff, name=met_names[2])
    ) + 
    labs(title = "Metric Comparison", x = "Date/Time") + 
    labs(caption = capti)
  return(g)
}