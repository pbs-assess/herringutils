#' Add or omit the x and y label and tick mark text to a ggplot.
#'
#' @param g a ggplot object
#' @param x_label_text text for the x label. Only added if `show_x_axis` is `TRUE`
#' @param y_label_text text for the x label. Only added if `show_y_axis` is `TRUE`
#' @param show_x_axis Logical for whether or not to show the x axis label and tick marks/tick text
#' @param show_y_axis Logical for whether or not to show the y axis label and tick marks/tick text
#' @param x_axis_label_size size of the font to use for the x axis label
#' @param x_axis_tick_label_size size of the font to use for the x axis tick mark labels
#' @param y_axis_label_size size of the font to use for the y axis label
#' @param y_axis_tick_label_size size of the font to use for the y axis tick mark labels
#'
#' @importFrom ggplot2 xlab ylab theme element_text element_blank
#' @export
#' @return the modified ggplot object
modify_axes_labels <- function(g,
                               x_label_text = "",
                               y_label_text = "",
                               show_x_axis = TRUE,
                               show_y_axis = TRUE,
                               x_axis_label_size = 8,
                               x_axis_tick_label_size = 8,
                               y_axis_label_size = 8,
                               y_axis_tick_label_size = 8){
  if(show_x_axis){
    g <- g +
      xlab(x_label_text) +
      theme(axis.text.x = element_text(size = x_axis_tick_label_size),
            axis.title.x = element_text(size = x_axis_label_size))
  }else{
    g <- g +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank())
  }
  if(show_y_axis){
    g <- g +
      ylab(y_label_text) +
      theme(axis.text.y = element_text(size = y_axis_tick_label_size),
            axis.title.y = element_text(size = y_axis_label_size))
  }else{
    g <- g +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank())
  }
  g
}

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
#' Optionally, set all NAs to 0s in the data columns only.
#'
#' @param tab data frame with years as the first column, and N columns of data
#'  with names
#' @param by a vector of names, which correspond to the names of the columns in tab
#' @param replace_na logical; replace NAs with zeros (default TRUE)
#'
#' @importFrom gfutilities f
#' @return a modified data frame
add_cols_and_reorder <- function(tab,
                                 by,
                                 replace_na=TRUE){
  if(any(!by %in% colnames(tab))){
    tab[by[!by %in% colnames(tab)]] <- NA
  }
  if(replace_na) tab[is.na(tab)] <- 0
  tab[-1] <- apply(tab[-1], c(1,2), f)
  tab_no_yr <- tab[-1]
  tab_no_yr <- tab_no_yr[,match(by, names(tab_no_yr))]
  cbind(tab[1], tab_no_yr)
}

