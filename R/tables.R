#' Table showing input data to the herring assessment.
#'
#' @param tab data.frame as read in by [readr::read_csv()]
#' @param cap caption for table
#' @param translate Logical. Translate to french if TRUE
#' @param ... arguments passed to [csas_table()]
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
                             translate = FALSE,
                             ...){
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
             caption = cap,
             ...)

}

#' Table showing the total landed catch by area for herring
#'
#' @param tab data.frame as returned from [gfiscamutils::get_catch()]
#' @param by_vec a vector of names, which correspond to the names of the columns in tab
#' @param first_yr first year to show in table
#' @param cap caption for table
#' @param translate Logical. Translate to french if TRUE
#' @param ... arguments passed to [csas_table()]
#'
#' @importFrom dplyr filter select mutate as_tibble group_by ungroup summarize full_join lead
#' @importFrom reshape2 dcast
#' @importFrom rosettafish en2fr
#' @importFrom csasdown csas_table
#'
#' @export
#' @importFrom kableExtra add_header_above
#' @return a [csasdown::csas_table()]
total_landed_catch_table <- function(tab,
                                     by_vec,
                                     first_yr,
                                     cap = "",
                                     translate = FALSE,
                                     ...){
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
  sar <- en2fr("SAR", translate)
  ahead <- c(" "=1, sar=(ncol(tab)-1))
  names(ahead) <- c(" ", sar)
  csas_table(tab,
             format = "latex",
             align = c("l", rep("r", 5)),
             caption = cap,
             ...) %>%
    add_header_above(ahead,
                     bold = TRUE)
}

#' Table for the Spawn on kelp harvest for herring
#'
#' @param tab data.frame as read in by [readr::read_csv()]
#' @param by_vec a vector of names, which correspond to the names of the columns in tab
#' @param first_yr first year to show in table
#' @param cap caption for table
#' @param translate Logical. Translate to french if TRUE
#' @param ... arguments passed to [csas_table()]
#'
#' @importFrom dplyr filter select mutate as_tibble group_by ungroup summarize full_join lead
#' @importFrom reshape2 dcast
#' @importFrom rosettafish en2fr
#' @importFrom csasdown csas_table
#'
#' @export
#' @importFrom kableExtra add_header_above
#' @return a [csasdown::csas_table()]
sok_harvest_table <- function(tab,
                              by_vec,
                              first_yr,
                              cap = "",
                              translate = FALSE,
                              ...){
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
  sar <- en2fr("SAR", translate)
  ahead <- c(" "=1, sar=(ncol(tab)-1))
  names(ahead) <- c(" ", sar)
  csas_table(tab,
             format = "latex",
             align = c("l", rep("r", 5)),
             caption = cap,
             ...)  %>%
    add_header_above(ahead,
                     bold = TRUE)
}

#' Table for spawn index by area
#'
#' @param tab data.frame as read in by [readr::read_csv()]
#' @param cap caption for table
#' @param translate Logical. Translate to french if TRUE
#' @param ... arguments passed to [csas_table()]
#'
#' @export
#' @importFrom kableExtra add_header_above
#' @return a [csasdown::csas_table()]
spawn_index_by_area_table <- function(tab,
                                      cap = "",
                                      translate = FALSE,
                                      ...){
  names(tab) <- gsub("&", "\\\\&", names(tab))
  tab[-c(1, 2)] <- apply(tab[-c(1, 2)], c(1,2), f, 3)
  tab[2] <- apply(tab[2], c(1,2), f)
  names(tab) <- en2fr(names(tab), translate, allow_missing = TRUE)
  csas_table(tab,
             format = "latex",
             align = c("l", rep("r", ncol(tab) - 1)),
             caption = cap,
             ...) %>%
    add_header_above(c("",
                       "",
                       "Proportion" = (ncol(tab) - 2)),
                     bold = TRUE)
}

#' Table for the spawn index in multiple areas
#'
#' @param tab data.frame as read in by [readr::read_csv()]
#' @param by_vec a vector of names, which correspond to the names of the columns in tab
#' @param first_yr first year to show in table
#' @param cap caption for table
#' @param translate Logical. Translate to french if TRUE
#' @param ... arguments passed to [csas_table()]
#'
#' @importFrom dplyr filter select rename mutate
#' @importFrom reshape2 dcast
#' @importFrom rosettafish en2fr
#' @importFrom csasdown csas_table
#'
#' @export
#' @importFrom kableExtra add_header_above
#' @return a [csasdown::csas_table()]
spawn_index_table <- function(tab,
                              by_vec,
                              first_yr,
                              cap = "",
                              translate = FALSE,
                              ...){
  tab <- tab %>%
    rename( Year=year ) %>%
    filter(Year >= first_yr) %>%
    mutate( value=value*1000 ) %>%
    select( Year, value, region) %>%
    dcast(Year ~ region, value.var = "value")
  tab <- add_cols_and_reorder(tab, by = by_vec, replace_na = FALSE)
  names(tab)[names(tab) == "Year"] <- en2fr("Year", translate)
  sar <- en2fr("SAR", translate)
  ahead <- c(" "=1, sar=(ncol(tab)-1))
  names(ahead) <- c(" ", sar)
  csas_table(tab,
             format = "latex",
             align = c("l", rep("r", 5)),
             caption = cap,
             ...)  %>%
    add_header_above(ahead,
                     bold = TRUE)
}

#' Make decision table based on MP data
#'
#' @param df data brought in from CSV files found in the herringsr project's data directory
#' @param xcaption caption to use for the table
#' @param xlabel latex label to use for the table
#' @param font.size size of font for table data
#' @param space.size space between rows of data in the table
#' @param last_col_header title text for the last column
#' @param placement latex placement for the table
#' @param translate logical. Translate to French if TRUE
#' @param perc_dec_pts number of decimal points to show for percentage columns
#' @param dec_pts number of decimal points to show for numerical non-percentage columns
#' @param col_align string for alignment of columns. c=center, r=right, l=left, |=place vertical bar between column
#' @param inc_mps include these MP numbers in the table. If NA, all will be included
#'
#' @return an [xtable::xtable()]
#' @export
#' @importFrom xtable xtable
#' @importFrom dplyr as_tibble mutate
#' @importfrom gfutilities latex.cline latex.amp latex.bold latex.mcol latex.nline latex.hline latex.size.str
decision_tables_mp <- function(df,
                               xcaption = "Default",
                               xlabel = "tab:default",
                               font.size = 9,
                               space.size = 10,
                               placement = "ht",
                               last_col_header = "TAC",
                               perc_dec_pts = 0,
                               dec_pts = 2,
                               col_align = "cc|c|c|c|c|c|c|c",
                               inc_mps = NA,
                               translate = FALSE){

  df$label <- gsub("_", "\\\\_", df$label)

  # If the conservation target is less than 75%, show -- for TAC and hr
  df <- df %>%
   mutate(tac = ifelse(obj1 < 0.75, " NA", tac),
          targ.hr = ifelse(obj1 < 0.75, " NA", targ.hr))

  wherena <- apply(df, c(1,2), function(x){grep(" *NA", x)}) == 1
  df[!is.na(wherena)] <- "--"
  df$tac[is.na(df$tac)] <- "--"
  df$targ.hr[is.na(df$targ.hr)] <- "--"

  df <- df %>%
    mutate(om,
           obj1 = paste0(f(obj1 * 100, dec.points = perc_dec_pts), "\\%"),
           obj2 = paste0(f(obj2 * 100, dec.points = perc_dec_pts), "\\%"),
           obj3 = f(obj3, dec.points = dec_pts),
           obj4 = f(obj4, dec.points = dec_pts),
           catch = paste0(f(catch * 100, dec.points = perc_dec_pts), "\\%"),
           tac = ifelse(tac == "--", "--", f(as.numeric(tac), dec.points = dec_pts)),
           targ.hr = ifelse(targ.hr == "--", "--", f(as.numeric(targ.hr), dec.points = dec_pts)))


  if(!is.na(inc_mps[1])){
    df <- df %>%
      filter(mp %in% inc_mps)
  }

  new_rows <- list()
  new_rows$pos <- list()
  new_rows$pos[[1]] <- -1
  new_rows$pos[[2]] <- -1
  new_rows$pos[[3]] <- -1
  new_rows$pos[[4]] <- -1
  new_rows$command <- c(paste0(latex.cline("1-9"),
                               latex.amp(2),
                               latex.bold(en2fr("Conservation", translate = translate)),
                               latex.amp(),
                               latex.bold(en2fr("Biomass", translate = translate)),
                               latex.amp(),
                               latex.mcol(3,
                                          "c|",
                                          latex.bold(en2fr("Yield", translate = translate))),
                               latex.amp(2),
                               latex.nline),
                        paste0(latex.mcol(2,
                                          "c|",
                                          latex.bold(en2fr("Scenario", translate = translate))),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Objective", translate = translate),
                                                 " 1 (",
                                                 en2fr("LRP", translate = translate),
                                                 ")")),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Objective", translate = translate),
                                                 " 2")),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Objective", translate = translate),
                                                 " 3")),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Objective", translate = translate),
                                                 " 4")),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Catch", translate = translate),
                                                 " < 650~t")),
                               latex.amp(),
                               latex.bold(last_col_header),
                               latex.amp(),
                               latex.bold(en2fr("HR", translate = translate)),
                               latex.nline),
                        paste0(latex.amp(2),
                               "$\\geq 75\\%$",
                               latex.amp(),
                               "$\\geq 50\\%$",
                               latex.amp(),
                               "$< 25\\%$",
                               latex.amp(),
                               "max",
                               latex.amp(),
                               "min",
                               latex.amp(),
                               latex.bold(paste0(en2fr("by", translate = translate),
                                                 " ",
                                                 en2fr("MP", translate = translate))),
                               latex.amp(),
                               latex.nline),
                        paste0(latex.cline("3-7"),
                               latex.bold(en2fr("OM", translate = translate)),
                               latex.amp(),
                               latex.bold(en2fr("MP", translate = translate)),
                               latex.amp(),
                               "$P(\\mli{SB}_t > 0.3\\mli{SB}_0)$",
                               latex.amp(),
                               "$P(\\mli{SB}_t \\geq 0.6\\mli{SB}_0)$",
                               latex.amp(),
                               en2fr("AAV", translate = translate),
                               latex.amp(),
                               "$\\overline{C}$",
                               latex.amp(),
                               "$P(C_t < 650~\\text{t})$",
                               latex.amp(),
                               latex.bold("(1000~t)"),
                               latex.amp(),
                               latex.nline,
                               latex.cline("1-9")))
  df$om <- en2fr(df$om, translate, allow_missing = TRUE)
  # Horizontal line locations for separating groups of OMs
  last_ddm <- which(df$om == "DDM")
  last_dim <- which(df$om == "DIM")
  last_conm <- which(df$om == "conM")
  last_ddm <- tail(last_ddm, 1)
  last_dim <- tail(last_dim, 1)
  last_conm <- tail(last_conm, 1)
  new_rows$pos[[5]] <- last_ddm
  new_rows$pos[[6]] <- last_dim
  new_rows$pos[[7]] <- last_conm
  new_rows$command <- c(new_rows$command,
                        latex.cline("1-9"),
                        latex.cline("1-9"),
                        latex.cline("1-9"))

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(df,
               caption = xcaption,
               label = xlabel,
               align = paste0("l", col_align)),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string ,
        add.to.row = new_rows,
        table.placement = placement,
        tabular.environment = "tabular",
        hline.after = NULL)
}
