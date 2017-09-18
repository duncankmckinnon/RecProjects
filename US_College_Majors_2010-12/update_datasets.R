update_datasets <- function(x = T)
{
  library('RCurl')
  library('stringr')
  fnames <- c("all-ages.csv", "majors-list.csv", "recent-grads.csv", "women-stem.csv", "grad-students.csv")
  file_list <- list()
  for(i in 1:length(fnames))
  {
    fname <- str_c('https://raw.githubusercontent.com/duncankmckinnon/RecProjects/master/US_College_Majors_2010-12/', fnames[i])
    file_list[[fnames[i]]] <- read.csv(url(fname), stringsAsFactors = F, skipNul = T, blank.lines.skip = T)
    if(x){write.csv(file, fnames[i])}
  }
  return(file_list)
}