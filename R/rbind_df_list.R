#' Bind lists and add element names
#'
#' @param lst A named list containing arrays with common column names
#' @param new_col The names of the new column to be added as the list names
#'
#' @return A data.frame object
#' @export
rbind_df_list <- function(lst, new_col = "dataset") {
  lst_with_newcol <- mapply(x=names(lst), y=lst, FUN = function(x, y){
    y[,new_col] <- x
    y
  }, SIMPLIFY = FALSE)
  Reduce(rbind, lst_with_newcol)
}
