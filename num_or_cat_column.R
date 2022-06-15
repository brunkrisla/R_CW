num_or_cat_column <- function(x)
{
  col_names <- c(colnames(x))
  num_cols <- c()
  cat_cols <- c()
  for(i in 1:length(colnames(url_data_complete))) {
    
    if (((length(unique(x[,i]))/length(x[,i]))*100) >= 0.02) {
      num_cols <- append(num_cols,col_names[i])
    } else {
      cat_cols <- append(cat_cols,col_names[i])
    }
    }

  return(list(num_cols,cat_cols))
}