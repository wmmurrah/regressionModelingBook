import_cogexp <- function(folder) {
  # Function to import richter_wuehr_cognition_exp* files into a data frame.
  temp = list.files(path = folder,  # get all file names
                    pattern="*.txt")
  orig_wd <- getwd()                # save current working directory (wd).
  setwd(folder)                     # change working dir to file folder
  
  # Names of variables from codebook
  nms <- c("program", "id", "block", "trial", "mapping", "stimulus",
           "task", "accurate", "correct_key", "registered_key", "rt")
  
  #  create a list of data frames for each participant
  myfiles = lapply(temp, 
                   FUN = function(x) read.table(x, header = FALSE,
                                                col.names = nms, 
                                                na.strings = 0,
                                                stringsAsFactors = TRUE))
  
# Bind all individual data frames into one.
 dat <-  do.call(rbind, myfiles)
 setwd(orig_wd) # return wd to original wd
 return(dat)
}
