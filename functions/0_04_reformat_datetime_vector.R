reformat_captured_datetime_vector <- function(captured_datetime_vector, gt_dir) {
  
  # gt_dir <- '/Users/jennishearston/Dropbox/CLEAN/2018/'
  # get datetime elements 
  dt_year <- lubridate::year(captured_datetime_vector) 
  dt_month <- lubridate::month(captured_datetime_vector) 
  dt_day <- lubridate::day(captured_datetime_vector) 
  dt_hour <- lubridate::hour(captured_datetime_vector) 
  dt_minute <- lubridate::minute(captured_datetime_vector) 
  
  # find an example gt_image_cat filename from your folder 
  filename_example <- list.files(path = gt_dir)[1]
  
  # create punctuation list 
  punctuation_list <- str_split(filename_example, '[0-9][0-9]')[[1]]
  
  filenames <- paste0(punctuation_list[1], two_digit_pad(dt_month), 
                      punctuation_list[2], two_digit_pad(dt_day), 
                      punctuation_list[3], str_sub(dt_year, 3, 4), 
                      punctuation_list[4], two_digit_pad(dt_hour), 
                      punctuation_list[5], two_digit_pad(dt_minute), 
                      punctuation_list[6])
  # return formatted filenames
  return(filenames)
}
  