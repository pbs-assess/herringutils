#' Function to add a new column indicating group ID based on sequential data
#'
#' @param vec the vector
#'
#' @return a vector where the ID depends on whether the value of x is
#'  sequential. For example, indicate whether a series of years is sequential,
#'  or if there are say three groups of sequential years.
#'  Break up the data by groups with consecutive values
consecutive_group <- function(vec) {
  d_unique_groups <- split(x = vec, f = cumsum(c(1, diff(vec) != 1)))
  for(g in seq_along(d_unique_groups)){
    d_unique_groups[[g]] <- rep(g, times = length(d_unique_groups[[g]]))
  }
  as.vector(unlist(d_unique_groups))
}

#' Add missing columns with names in `by` not in the column names of `tab`. Order
#' the table, ignoring the first column in the order given in the `by` vector.
#' Set all NAs to 0 in the data columns only.
#'
#' @param tab data frame with years as the first column, and N columns of data
#'  with names
#' @param by a vector of names, which correspond to the names of the columns in tab
#'
#' @importFrom gfutilities f
#' @return a modified data frame
add_cols_and_reorder <- function(tab,
                                 by){
  if(any(!by %in% colnames(tab))){
    tab[by[!by %in% colnames(tab)]] <- NA
  }
  tab[is.na(tab)] <- 0
  tab[-1] <- apply(tab[-1], c(1,2), f)
  tab_no_yr <- tab[-1]
  tab_no_yr <- tab_no_yr[,match(by, names(tab_no_yr))]
  cbind(tab[1], tab_no_yr)
}

