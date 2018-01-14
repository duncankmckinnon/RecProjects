update_datasets <- function(x = TRUE)
{
  require('data.world')
  require('stringr')
  dataset_key <- "https://data.world/fivethirtyeight/college-majors"
  tables_qry <- data.world::qry_sql("SELECT * FROM Tables")
  tables_df <- data.world::query(tables_qry, dataset = dataset_key)
  file_list <- list()
  for(i in 1:length(tables_df$tableName))
  {
    next_q <- data.world::qry_sql(sprintf("SELECT * FROM `%s`", tables_df$tableName[[i]]))
    file_list[[tables_df$tableName[[i]]]] <- data.world::query(next_q, dataset = dataset_key)
    if(x){write.csv(file_list[[i]], str_c("US_College_Majors_2010-12/", tables_df$tableName[[i]], ".csv"))}
  }
  return(file_list)
}



