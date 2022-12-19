make_captured_datetime_vector <- function(base_date = '2019/01/01 00:00',
                                          end_date = "none", 
                                          sampling_quantity_units_direction = 'none',
                                          HM_of_interest = 'none', 
                                          DoW_of_interest = 'none') {

  
  
  # You can either define it by the sample
 # parameter values  
  #base_date <- '2021/11/7 10:00'
  #sampling_quantity_units_direction = "3_weeks_forward"
 
  # 1a Catch some errors
  if (end_date == 'none' & sampling_quantity_units_direction == 'none') stop('pick a way to sample')
  if (end_date != 'none' & sampling_quantity_units_direction != 'none') stop('pick one way to sample')
  
  # 1b Change the base_date to POSIXct format
  base_date <- base_date %>% 
    lubridate::parse_date_time("ymd HM") 
 
  # 1c Change the end_date to POSIXct format
  if (end_date !='none') {
    end_date <- end_date %>% 
      lubridate::parse_date_time("ymd HM") 
  }
  
  # 1d Get end_date if end_date is defined by sampling_quantity_units_direction
  if (sampling_quantity_units_direction != 'none') {
    # split up the 
    quantity <- str_split_fixed(sampling_quantity_units_direction, '_', 3)[1] %>% as.numeric()
    sampling_units  <- str_split_fixed(sampling_quantity_units_direction, '_', 3)[2]
    direction <- str_split_fixed(sampling_quantity_units_direction, '_', 3)[3]
    
    if (is.na(quantity)) stop('quantity must be a number (e.g. 3 not three)')
    if (!sampling_units %in% c('hours', 'days', 'weeks', 'months', 'years')) stop('pick an allowed sampling unit: hours, days, weeks, months, years')
    if (!direction %in% c('forward', 'backward')) stop('pick an allowed sampling direction: forward, backward')
    
    if (sampling_units == 'hours') {
      sampling_duration <- duration(hours = quantity)
    } else if (sampling_units == 'days') {
      sampling_duration <- duration(days = quantity)
    } else if (sampling_units == 'weeks') {
      sampling_duration <- duration(weeks = quantity)
    } else if (sampling_units == 'months') {
      sampling_duration <- duration(months = quantity)
    } else if (sampling_units == 'years') {
      sampling_duration <- duration(years = quantity)
    }
    
    if(direction == 'forward') {
      end_date = base_date + sampling_duration
    } else if(direction == 'backwards') {
      end_date = base_date - sampling_duration
    }
  }
  

  # 1e get sampling times
  datetime_vector <- seq(base_date, end_date, 60*60) 
  
  # then later we can write code to define sampling criteria - eg only capture mondays
  #date_seq <- date_seq %>% 
   # filter(paste0(hour(captured_datetime), ':', minute(captured_datetime)) %in% HM_of_interest & 
    #         wday(captured_datetime) %in% DoW_of_interest)
  

  
  # 1g return vector
  return(datetime_vector)
}
                                          