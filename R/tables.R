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
    tab$DPR[tab$Year %in% c(2016, 2019)] <- "DT"
  }else{
    tab$PRD[tab$Year %in% c(2016, 2019)] <- "WP"
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
#' @param first_yr Earliest year to show in the table
#' @param translate Logical. Translate to french if TRUE
#' @param ... arguments passed to [csas_table()]
#'
#' @export
#' @importFrom kableExtra add_header_above
#' @importFrom dplyr filter
#' @return a [csasdown::csas_table()]
spawn_index_by_area_table <- function(tab,
                                      cap = "",
                                      first_yr,
                                      translate = FALSE,
                                      ...){
  tab <- filter( tab, Year>=first_yr )
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
decision_tables_mp <- function(df,
                               xcaption = "Default",
                               xlabel = "tab:default",
                               font.size = 11,
                               space.size = 15,
                               placement = "ht",
                               perc_dec_pts = 0,
                               dec_pts = 2,
                               inc_mps = NA,
                               translate = FALSE){

  df$label <- gsub("_", "\\\\_", df$label)

  # If the conservation target is less than 75%, show -- for TAC and hr
  df <- df %>%
    mutate(tac = ifelse(obj1 < 0.75, NA, tac),
           targ.hr = ifelse(obj1 < 0.75, NA, targ.hr))

  df <- df %>%
    mutate(om,
           obj1 = paste0(f(obj1 * 100, dec.points = perc_dec_pts), "\\%"),
           hiab = paste0(f(hiab * 100, dec.points = perc_dec_pts), "\\%"),
           obj2 = paste0(f(obj2 * 100, dec.points = perc_dec_pts), "\\%"),
           obj3 = f(obj3, dec.points = dec_pts),
           obj4 = f(obj4, dec.points = dec_pts),
           tac = ifelse(tac == "--", "--", f(tac, dec.points = dec_pts)),
           targ.hr = ifelse(targ.hr == "--", "--", f(targ.hr, dec.points = dec_pts)))
  df[is.na(df)] <- "--"

  col_align = paste0("ll", paste(rep("|c", times = ncol(df) - 2), collapse = ""))

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
  new_rows$command <- c(paste0(latex.cline(paste0("1-",ncol(df))),
                               latex.amp(2),
                               latex.bold(en2fr("Conservation", translate = translate)),
                               latex.amp(),
                               latex.mcol(2,
                                          "c|",
                                          latex.bold(en2fr("Biomass", translate = translate))),
                               latex.amp(),
                               latex.mcol(2,
                                          "c|",
                                          latex.bold(en2fr("Yield", translate = translate))),
                               latex.amp(),
                               latex.bold(""),
                               latex.nline),
                        paste0(latex.mcol(2,
                                          "c|",
                                          latex.bold(en2fr("Scenario", translate = translate))),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Obj", translate = translate),
                                                 " 1 (",
                                                 en2fr("LRP", translate = translate),
                                                 ")")),
                               latex.amp(),
                               latex.bold("HIAB"),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Obj", translate = translate),
                                                 " 2")),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Obj", translate = translate),
                                                 " 3")),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Obj", translate = translate),
                                                 " 4")),
                               latex.amp(),
                               latex.bold(""),
                               latex.amp(),
                               latex.bold(""),
                               latex.nline),
                        paste0(latex.amp(2),
                               "$P \\geq 75\\%$",
                               latex.amp(),
                               "$P \\geq 50\\%$",
                               latex.amp(),
                               "$P \\geq 50\\%$",
                               latex.amp(),
                               "$< 25\\%$",
                               latex.amp(),
                               "max",
                               latex.amp(),
                               latex.bold(assess_yr + 1),
                               latex.nline),
                        paste0(latex.cline("3-7"),
                               latex.bold(en2fr("OM", translate = translate)),
                               latex.amp(),
                               latex.bold(en2fr("MP", translate = translate)),
                               latex.amp(),
                               "$\\SB_t > 0.3\\SB_0$",
                               latex.amp(),
                               "$\\geq 0.4\\SB_0$",
                               latex.amp(),
                               "$\\geq 0.6\\SB_0$",
                               latex.amp(),
                               en2fr("AAV", translate = translate),
                               latex.amp(),
                               "$\\overline{C}$",
                               latex.amp(),
                               latex.bold(en2fr("TAC", translate = translate)),
                               latex.amp(),
                               latex.bold(en2fr("HR", translate = translate)),
                               latex.nline,
                               latex.cline(paste0("1-",ncol(df)))))
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
                        latex.cline(paste0("1-",ncol(df))),
                        latex.cline(paste0("1-",ncol(df))),
                        latex.cline(paste0("1-",ncol(df))))
  size.string <- latex.size.str(font.size, space.size)
  df$om <- en2fr(df$om, translate, allow_missing = TRUE)
  print(xtable(df,
               caption = xcaption,
               label = xlabel,
               align = paste0("l", col_align)),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = new_rows,
        table.placement = placement,
        tabular.environment = "tabular",
        hline.after = NULL)
}

#' Make decision table based on MP data with 3 extra Biomass columns
#'
#' @param df data brought in from CSV files found in the herringsr project's data directory
#' @param xcaption caption to use for the table
#' @param xlabel latex label to use for the table
#' @param font.size size of font for table data
#' @param space.size space between rows of data in the table
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
decision_tables_mp_add <- function(df,
                                   xcaption = "Default",
                                   xlabel = "tab:default",
                                   font.size = 11,
                                   space.size = 15,
                                   placement = "ht",
                                   perc_dec_pts = 0,
                                   dec_pts = 2,
                                   inc_mps = NA,
                                   translate = FALSE){

  df$label <- gsub("_", "\\\\_", df$label)

  # If the conservation target is less than 75%, show -- for TAC and hr
  df <- df %>%
    mutate(tac = ifelse(obj1 < 0.75, NA, tac),
           targ.hr = ifelse(obj1 < 0.75, NA, targ.hr))

  df <- df %>%
    mutate(om,
           obj1 = paste0(f(obj1 * 100, dec.points = perc_dec_pts), "\\%"),
           hiab = paste0(f(hiab * 100, dec.points = perc_dec_pts), "\\%"),
           obj2 = paste0(f(obj2 * 100, dec.points = perc_dec_pts), "\\%"),
           ncn1 = paste0(f(ncn1 * 100, dec.points = perc_dec_pts), "\\%"),
           ncn2 = paste0(f(ncn2 * 100, dec.points = perc_dec_pts), "\\%"),
           ncn3 = paste0(f(ncn3 * 100, dec.points = perc_dec_pts), "\\%"),
           obj3 = f(obj3, dec.points = dec_pts),
           obj4 = f(obj4, dec.points = dec_pts),
           tac = ifelse(tac == "--", "--", f(tac, dec.points = dec_pts)),
           targ.hr = ifelse(targ.hr == "--", "--", f(targ.hr, dec.points = dec_pts)))
  df[is.na(df)] <- "--"

  col_align = paste0("ll", paste(rep("|c", times = ncol(df) - 2), collapse = ""))

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
  new_rows$command <- c(paste0(latex.cline(paste0("1-",ncol(df))),
                               latex.amp(2),
                               latex.bold(en2fr("Conservation", translate = translate)),
                               latex.amp(),
                               latex.mcol(5,
                                          "c|",
                                          latex.bold(en2fr("Biomass", translate = translate))),
                               latex.amp(),
                               latex.mcol(2,
                                          "c|",
                                          latex.bold(en2fr("Yield", translate = translate))),
                               latex.amp(),
                               latex.bold(""),
                               latex.nline),
                        paste0(latex.mcol(2,
                                          "c|",
                                          latex.bold(en2fr("Scenario", translate = translate))),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Obj", translate = translate),
                                                 " 1 (",
                                                 en2fr("LRP", translate = translate),
                                                 ")")),
                               latex.amp(),
                               latex.bold("HIAB"),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Obj", translate = translate),
                                                 " 2")),
                               latex.amp(),
                               latex.mcol(3,
                                          "c|",
                                          latex.bold(paste0("NCN"))),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Obj", translate = translate),
                                                 " 3")),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Obj", translate = translate),
                                                 " 4")),
                               latex.amp(),
                               latex.bold(""),
                               latex.amp(),
                               latex.bold(""),
                               latex.nline),
                        paste0(latex.amp(2),
                               "$P \\geq 75\\%$",
                               latex.amp(),
                               "$P \\geq 50\\%$",
                               latex.amp(),
                               "$P \\geq 50\\%$",
                               latex.amp(),
                               "$P \\geq 50\\%$",
                               latex.amp(),
                               "$P \\geq 50\\%$",
                               latex.amp(),
                               "$P \\geq 75\\%$",
                               latex.amp(),
                               "$< 25\\%$",
                               latex.amp(),
                               "max",
                               latex.amp(),
                               latex.bold(assess_yr + 1),
                               latex.nline),
                        paste0(latex.cline("3-10"),
                               latex.bold(en2fr("OM", translate = translate)),
                               latex.amp(),
                               latex.bold(en2fr("MP", translate = translate)),
                               latex.amp(),
                               "$\\SB_t > 0.3\\SB_0$",
                               latex.amp(),
                               "$\\geq 0.4\\SB_0$",
                               latex.amp(),
                               "$\\geq 0.6\\SB_0$",
                               latex.amp(),
                               "$\\geq 0.65\\SB_0$",
                               latex.amp(),
                               "$\\geq 0.75\\SB_0$",
                               latex.amp(),
                               "$\\geq \\SB_\\AVE$",
                               latex.amp(),
                               en2fr("AAV", translate = translate),
                               latex.amp(),
                               "$\\overline{C}$",
                               latex.amp(),
                               latex.bold(en2fr("TAC", translate = translate)),
                               latex.amp(),
                               latex.bold(en2fr("HR", translate = translate)),
                               latex.nline,
                               latex.cline(paste0("1-",ncol(df)))))
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
                        latex.cline(paste0("1-",ncol(df))),
                        latex.cline(paste0("1-",ncol(df))),
                        latex.cline(paste0("1-",ncol(df))))
  size.string <- latex.size.str(font.size, space.size)
  df$om <- en2fr(df$om, translate, allow_missing = TRUE)
  print(xtable(df,
               caption = xcaption,
               label = xlabel,
               align = paste0("l", col_align)),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = new_rows,
        table.placement = placement,
        tabular.environment = "tabular",
        hline.after = NULL)
}
