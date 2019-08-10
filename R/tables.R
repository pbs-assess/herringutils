#' Table showing input data to the herring assessment.
#'
#' @param tab data.frame as read in by [readr::read_csv()]
#' @param cap caption for table
#' @param translate Logical. Translate to french if TRUE
#'
#' @importFrom gfutilities firstup firstlower
#' @importFrom stringr str_extract
#' @importFrom rosettafish en2fr
#' @importFrom csasdown csas_table
#'
#' @export
#' @return a [csasdown::csas_table()]
input_data_table <- function(tab,
                             cap = "",
                             translate = FALSE){
  # Source column
  tab$Source <- en2fr(tab$Source, translate, allow_missing = TRUE)
  tmp <- tab$Source
  nonbracs <- str_extract(tmp, "[(\\w+ ) ]+(?= +\\()")
  bracs <- str_extract(tmp, "(?<=\\()\\w+(?=\\))")
  if(!all(is.na(bracs) == is.na(nonbracs))){
    warning("The match of bracketed items in the Source column of the Input data table was incorrect.")
  }
  tmp[!is.na(bracs)] <- paste0(en2fr(firstup(nonbracs[!is.na(nonbracs)]), translate),
                               " (",  firstlower(en2fr(firstup(bracs[!is.na(bracs)]), translate)), ")")
  tab$Source <- tmp
  # Data column
  tmp <- strsplit(tab$Data, ": *")
  tmp <- lapply(tmp, function(x){
    j <- firstup(x)
    j <- en2fr(j, translate = translate, allow_missing = TRUE)
    if(length(j) > 1){
      j <- c(j[1], tolower(j[-1]))
      j <- paste(j, collapse = ": ")
    }
    j
  })
  tab$Data <- unlist(tmp)
  # Years column
  if(translate){
    tmp <- tab$Years
    tmp <- strsplit(tab$Years, " *to *")
    tmp <- lapply(tmp, function(x){
      paste0("De ", x[1], " \U00E0 ", x[2])
    })
    tab$Years <- unlist(tmp)
  }

  names(tab) <- en2fr(names(tab), translate)
  csas_table(tab,
             format = "latex",
             caption = cap)

}

#' Table showing the total landed catch by area for herring
#'
#' @param tab data.frame as returned from [gfiscamutils::get_catch()]
#' @param by_vec a vector of names, which correspond to the names of the columns in tab
#' @param first_yr first year to show in table
#' @param cap caption for table
#' @param translate Logical. Translate to french if TRUE
#'
#' @importFrom dplyr filter select mutate as_tibble group_by ungroup summarize full_join lead
#' @importFrom reshape2 dcast
#' @importFrom rosettafish en2fr
#' @importFrom csasdown csas_table
#'
#' @export
#' @return a [csasdown::csas_table()]
total_landed_catch_table <- function(tab,
                                     by_vec,
                                     first_yr,
                                     cap = "",
                                     translate = FALSE){
  tab <- tab %>%
    filter(year >= first_yr) %>%
    select(c(year, value, region)) %>%
    group_by(year, region) %>%
    summarize(catch = sum(value) * 1000) %>%
    ungroup() %>%
    dcast(year ~ region, value.var = "catch") %>%
    rename(Year = year)
  tab <- add_cols_and_reorder(tab, by = by_vec)
  names(tab)[names(tab) == "Year"] <- en2fr("Year", translate)
  csas_table(tab,
             format = "latex",
             align = c("l", rep("r", 5)),
             caption = cap)
}

#' Table for the Spawn on kelp harvest for herring
#'
#' @param tab data.frame as read in by [readr::read_csv()]
#' @param by_vec a vector of names, which correspond to the names of the columns in tab
#' @param first_yr first year to show in table
#' @param cap caption for table
#' @param translate Logical. Translate to french if TRUE
#'
#' @importFrom dplyr filter select mutate as_tibble group_by ungroup summarize full_join lead
#' @importFrom reshape2 dcast
#' @importFrom rosettafish en2fr
#' @importFrom csasdown csas_table
#'
#' @export
#' @return a [csasdown::csas_table()]
sok_harvest_table <- function(tab,
                              by_vec,
                              first_yr,
                              cap = "",
                              translate = FALSE){
  tab <- tab %>%
    filter(Year >= first_yr) %>%
    select(c(Year, Harvest, Region)) %>%
    group_by(Year, Region) %>%
    summarize(catch = sum(Harvest) * 2.20462262185) %>%
    ungroup() %>%
    dcast(Year ~ Region, value.var = "catch") #%>%
    #select(-by_vec)
  tab <- add_cols_and_reorder(tab, by = by_vec)
  if(translate){
    tab$DPR[tab$Year == 2016] <- "DT"
  }else{
    tab$PRD[tab$Year == 2016] <- "WP"
  }
  names(tab)[names(tab) == "Year"] <- en2fr("Year", translate)
  csas_table(tab,
             format = "latex",
             align = c("l", rep("r", 5)),
             caption = cap)
}

#' Table for spawn index by area
#'
#' @param tab data.frame as read in by [readr::read_csv()]
#' @param cap caption for table
#' @param translate Logical. Translate to french if TRUE
#'
#' @export
#' @return a [csasdown::csas_table()]
spawn_index_by_area_table <- function(tab, cap = "", translate = FALSE){
  names(tab) <- gsub("&", "\\\\&", names(tab))
  tab[-c(1, 2)] <- apply(tab[-c(1, 2)], c(1,2), f, 3)
  tab[2] <- apply(tab[2], c(1,2), f)
  names(tab) <- en2fr(names(tab), translate, allow_missing = TRUE)
  csas_table(tab,
             format = "latex",
             align = c("l", rep("r", ncol(tab) - 1)),
             caption = cap)
}
