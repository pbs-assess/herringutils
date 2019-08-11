#' Insert a newline between words so that all single lines will be of length `len` or less.
#'
#' @param x string to insert newlines into
#' @param len length that each line must be
#'
#' @return string with newlines inserted
#' @importFrom stringr str_sub str_split
#' @export
#' @examples
#' str <- "This is a test of the newline_format function which is used to split text"
#' newline_format(str, 12)
newline_format <- function(x, len){
  stopifnot(!is.na(x),
            !is.null(x),
            is.character(x),
            !is.na(len),
            !is.null(len),
            len > 0)
  if(len < 2){
    return(x)
  }
  j <- str_split(x, " ")
  nchar_j <- lapply(j, nchar)[[1]]
  j <- j[[1]]
  cumj <- cumsum(nchar_j)
  line <- ""
  while(length(cumj)){
    if(length(j) == 1){
     line <- paste0(line, j)
     cumj <- numeric(0)
    }else{
      cumj_lt <- cumj < len
      line <- paste0(line, paste(j[1:length(cumj[cumj_lt])], collapse = " "), "\n")
      cumj <- cumj[!cumj_lt] - len
      j <- j[-(1:length(cumj[cumj_lt]))]
    }
  }
  if(str_sub(line, start = nchar(line), end = nchar(line)) == "\n"){
    line <- str_sub(line, start = 1, end = nchar(line) - 1)
  }
  line
}


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

