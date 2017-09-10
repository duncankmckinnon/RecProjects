update_datasets <- function(x = T)
{
  library('RCurl')
  library('stringr')
  fnames <- c("all-ages.csv", "majors-list.csv", "recent-grads.csv", "women-stem.csv", "grad-students.csv")
  file_list <- list()
  for(i in 1:length(fnames))
  {
    fname <- str_c('https://raw.githubusercontent.com/duncankmckinnon/data/master/college-majors/', fnames[i])
    file_list[[fnames[i]]] <- read.csv(url(fname), stringsAsFactors = F)
    if(x){write.csv(file, fnames[i])}
  }
  return(file_list)
}