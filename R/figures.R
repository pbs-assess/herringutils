#' Plot catch from a data frames as extracted from iscam data (dat) files
#'
#' @param df a data frame as constructed by [get_catch()]
#' @param xlim Limits for the years shown on the plot
#' @param translate Logical. If TRUE, translate to French
#'
#' @importFrom ggplot2 ggplot aes geom_bar scale_x_continuous expand_limits scale_fill_viridis_d theme labs facet_wrap
#' @importFrom rosettafish en2fr
#' @export
#' @return A ggplot object
plot_catch <- function(df,
                       xlim = c(1000, 3000),
                       translate = FALSE){
  df <- df %>%
    filter(year >= xlim[1])
  g <- ggplot(df, aes(x = year, y = value)) +
    geom_bar(stat = "identity", position = "stack", aes(fill = gear), width = 1) +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10)) +
    coord_cartesian(xlim) +
    expand_limits(x = xlim[1]:xlim[2]) +
    scale_fill_viridis_d( ) +
    theme(legend.position = "top") +
    labs(x = en2fr("Year", translate),
         y = paste(en2fr("Catch", translate), " (1000 t)"),
         fill = en2fr("Gear", translate)) +
    facet_wrap( ~ region, ncol = 2, dir = "v", scales = "free_y" )
  g
}


#' Plot weight-at-age time series from a data frames as extracted from iscam data (dat) files
#'
#' @param df a data frame as constructed by [get_wa()]
#' @param circle_age the age for which to add circles to plot
#' @param xlim Limits for the years shown on the plot
#' @param ylim limits for the weights shown on the plot
#' @param n_roll Number of years to calculate the rolling mean (window)
#' @param translate Logical. If TRUE, translate to French
#'
#' @importFrom dplyr filter as_tibble rename mutate group_by ungroup select %>%
#' @importFrom ggplot2 ggplot aes geom_line geom_point coord_cartesian expand_limits labs facet_wrap
#' @importFrom reshape2 melt
#' @importFrom rosettafish en2fr
#' @importFrom zoo rollmean

#' @export
#' @return A ggplot object
plot_wa <- function(df,
                    circle_age = 3,
                    xlim = c(1000, 3000),
                    ylim = c(0, NA),
                    n_roll=5,
                    translate = FALSE){
  df <- df %>%
    filter(year >= xlim[1], gear%in%c("Other", "RoeSN"))
  dfm <- melt(df, id.vars = c("year", "area", "group", "sex", "region", "gear")) %>%
    as_tibble() %>%
    rename(Year = year,
           Age = variable,
           Weight = value) %>%
    select(-c(area, group, sex)) %>%
    group_by(region, Age) %>%
    mutate(muWeight = rollmean(x = Weight, k = n_roll, align = "right", na.pad = TRUE)) %>%
    ungroup() %>%
    mutate(Age = factor(Age))
  dfm_circle_age <- dfm %>%
    filter(Age == circle_age)
  dfm <- dfm %>%
    filter(Age != circle_age)
  g <- ggplot(dfm) +
    geom_line(aes(x = Year,
                  y = muWeight,
                  group = Age),
              na.rm = TRUE) +
    geom_point(data = dfm_circle_age,
               aes(x = Year, y = Weight),
               shape = 1,
               size = 2,
               na.rm = TRUE) +
    geom_line(data = dfm_circle_age,
              aes(x = Year, y = muWeight),
              size = 1.25,
              na.rm = TRUE) +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10)) +
    coord_cartesian(xlim, ylim) +
    expand_limits(x = xlim[1]:xlim[2]) +
    labs(x = en2fr("Year", translate),
         y = paste0(en2fr("Weight-at-age", translate), " (kg)")) +
    facet_wrap( ~ region, ncol = 2, dir = "v" )
  g
}

#' Plot proportions-at-age time series from a data frames as extracted from iscam data (dat) files
#'
#' @param df a data frame as constructed by [get_pa()]
#' @param age_plus age plus group
#' @param conf confidence value for the envelope
#' @param xlim limits for the years shown on the plot
#' @param ylim limits for the ages shown on the plot
#' @param size_range vector of min and max for range of sizes of points
#' @param translate Logical. If TRUE, translate to French
#'
#' @importFrom dplyr filter as_tibble rename mutate group_by ungroup select summarize
#' @importFrom ggplot2 ggplot aes geom_point geom_path scale_size_continuous geom_ribbon
#' coord_cartesian expand_limits labs facet_wrap theme
#' @importFrom reshape2 melt
#' @importFrom rosettafish en2fr
#' @export
#' @return A ggplot object
plot_pa <- function(df,
                    age_plus = 10,
                    conf = 0.9,
                    xlim = c(1000, 3000),
                    ylim = c(0, NA),
                    size_range = c(0.5, 2),
                    translate = FALSE){
  df <- df %>%
    filter(year >= xlim[1], gear%in%c("Other", "RoeSN"))
  dfm <- melt(df, id.vars = c("year", "area", "group", "sex", "region", "gear")) %>%
    as_tibble() %>%
    rename(Region = region,
           Year = year,
           Age = variable,
           Number = value) %>%
    select(-c(area, group, sex)) %>%
    mutate(Age = as.numeric(as.character(Age)),
           Age = ifelse(Age > age_plus, age_plus, Age)) %>%
    group_by(Region, Year, Age) %>%
    summarize(Number = sum(Number)) %>%
    mutate(Proportion = Number / ifelse(all(is.na(Number)), NA, sum(Number, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(Age = factor(Age))

  # Determine weighted mean and approximate CI age by year
  dfm_ci <- dfm %>%
    select(Region, Year, Age, Proportion) %>%
    mutate(Age = as.numeric(Age)) %>%
    group_by(Region, Year) %>%
    summarize(MeanAge = weighted.mean(x = Age, w = Proportion),
              sBar = qnorm(1 - (1 - conf) / 2) * sum(sqrt(Proportion * (1 - Proportion)) / sqrt(Age)),
              Lower = exp(log(MeanAge) - log(sBar)),
              Upper = exp(log(MeanAge) + log(sBar))) %>%
    ungroup() %>%
    mutate(GroupID = consecutive_group(Year))

  g <- ggplot(dfm, aes(x = Year)) +
    geom_point(aes(y = Age,
                   size = ifelse(Proportion, Proportion, NA)),
                   na.rm = TRUE) +
    geom_path(data = dfm_ci,
              aes(y = MeanAge, group = GroupID),
              size = 1.25,
              na.rm = TRUE) +
    scale_size_continuous(range = size_range) +
    geom_ribbon(data = dfm_ci,
                aes(ymin = Lower, ymax = Upper, group = GroupID),
                alpha = 0.25) +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10)) +
    coord_cartesian(xlim, ylim) +
    expand_limits(x = xlim[1]:xlim[2]) +
    labs(size = en2fr("Proportion", translate),
         x = en2fr("Year", translate),
         y = en2fr("Age", translate)) +
    facet_wrap(~ Region, ncol = 2, dir = "v" ) +
    theme(legend.position = "top")
  g
}

#' Plot survey indices from data frames as extracted from iscam data (dat) files
#'
#' @param df a data frame as constructed by [get_surv_ind()]
#' @param xlim limits for the years shown on the plot
#' @param ylim limits for the ages shown on the plot
#' @param translate Logical. If TRUE, translate to French
#' @param new_surv_yr Year in which the survey type changed. Will be shown as a vertical line
#' @param new_surv_yr_type ggplot linetype for new_survey_yr
#' @param new_surv_yr_size ggplot line size for new_survey_yr
#'
#' @importFrom dplyr filter select mutate as_tibble rename
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_line geom_point coord_cartesian expand_limits scale_shape_manual
#'  labs xlab ylab facet_wrap geom_vline theme
#' @export
#' @return A ggplot object
plot_spawn_ind <- function(df,
                           xlim = c(1000, 3000),
                           ylim = NA,
                           new_surv_yr = NA,
                           new_surv_yr_type = "dashed",
                           new_surv_yr_size = 0.25,
                           translate = FALSE){
  stopifnot(!is.na(new_surv_yr),
            is.numeric(new_surv_yr),
            length(new_surv_yr) == 1)

  df <- df %>%
    filter(year >= xlim[1]) %>%
    mutate(gear = ifelse(year < new_surv_yr, "Surface", "Dive"),
           gear = factor(gear)) %>%
    select(-qind)

  dfm <- melt(df, id.vars = c("year", "area", "group", "sex", "region", "wt", "timing", "gear")) %>%
    as_tibble() %>%
    rename(Region = region,
           Year = year,
           Index = value) %>%
    select(-c(area, group, sex, wt, timing))

  g <- ggplot(dfm, aes(x = Year, y = Index)) +
    geom_point(aes(shape = gear),
               na.rm = TRUE) +
    geom_line(aes(group = gear),
              na.rm = TRUE) +
    scale_shape_manual(values = c(2, 1)) +
    geom_vline(xintercept = new_surv_yr - 0.5, linetype = new_surv_yr_type, size = new_surv_yr_size) +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10)) +
    expand_limits(x = xlim[1]:xlim[2]) +
    labs(shape = en2fr("Survey period", translate),
         x = en2fr("Year", translate),
         y = paste0(en2fr("Spawn index", translate), " (1000 t)")) +
    facet_wrap(~ Region, ncol = 2, dir = "v", scales = "free_y" ) +
    theme(legend.position="top")
  if(!is.na(ylim[1])){
    g <- g +
      coord_cartesian(xlim, ylim)
  }
  g
}

#' Title
#'
#' @param df a data frame as constructed by [get_catch()]
#' @param models a list of iscam model objects
#' @param regions a vector of regions names in the order they are to appear in the facets
#' @param line_size thickness of the mediat Ut line
#' @param ribbon_alpha transparency of the ribbon representing the  credible interval for Ut
#' @param ylim Limits to show on the y-axis. Implemented with [ggplot2::coord_cartesian()]
#' @param h_line horizontal line value to plot
#' @param translate Logical. If TRUE, translate to French
#'
#' @return a ggplot object
#' @importFrom dplyr arrange
#' @importFrom tibble tibble
#' @export
plot_harvest_rate <- function(df,
                              models,
                              regions,
                              line_size = 1,
                              ribbon_alpha = 0.35,
                              ylim = c(0, 1),
                              h_line = 0.2,
                              translate = FALSE){
  dfm <- df %>%
    group_by(year, region) %>%
    summarize(ct = sum(value)) %>%
    ungroup()

  ssb <- lapply(seq_along(models), function(x){
    j <-  t(models[[x]]$mcmccalcs$sbt.quants) %>%
      as_tibble(rownames = "year") %>%
      mutate(year = as.numeric(year))
    names(j) <- c("year", "lower", "median", "upper", "mpd")
    j <- j %>%
      full_join(dfm, by = "year") %>%
      filter(region == regions[[x]]) %>%
      mutate(utlower = ct / (ct + lower),
             ut = ct / (ct + median),
             utupper = ct / (ct + upper),
             region = as.character(region))
    yrs <- as.numeric(min(j$year):max(j$year))
    all_yrs <- tibble(year = yrs,
                  region = rep(regions[[x]], length(yrs)))
    j <- j %>%
      full_join(all_yrs, by = c("year", "region"))  %>%
      arrange(year)
  }) %>%
    bind_rows()

  # Needed to have facets appear in same order as regions vector
  ssb <- arrange(transform(ssb, region = factor(region, levels = regions)), region)

  g <- ggplot(ssb, aes(x = year, y = ut)) +
    geom_line(size = line_size,
              na.rm = TRUE) +
    geom_ribbon(aes(ymin = utlower, ymax = utupper), alpha = ribbon_alpha) +
    geom_hline(yintercept = h_line, linetype = "dashed") +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10)) +
    labs(x = en2fr("Year", translate),
         y = en2fr("Effective harvest rate", translate)) +
    facet_wrap(~ region, ncol = 2, dir = "v" )

  if(!is.na(ylim[1])){
    g <- g +
      coord_cartesian(ylim = ylim, expand = TRUE)
  }
  g
}

#' Plot density of MCMC posterior for projected biomass with quantiles and
#'  reference point (also with quantiles)
#'
#' @param models list of iscam model objects
#' @param regions a vector of regions names in the order they are to appear in the facets
#' @param yr year to plot. Must be in the table `models[[x]]$mcmccalcs$proj.quants``
#' @param tac tac to extract data for
#' @param refpt the reference point to plot, as found in `models[[x]]$mcmccalcs$proj.quants``
#' @param line_size thickness of the mediat Ut line
#' @param ribbon_alpha transparency of the ribbon representing the  credible interval for Ut
#' @param translate Logical. If TRUE, translate to French
#'
#' @return a ggplot object
#' @importFrom ggplot2 geom_density
#' @importFrom scales pretty_breaks
#' @export
plot_proj_biomass_density <- function(models,
                                      regions,
                                      yr,
                                      tac = 0,
                                      refpt = "X03B0",
                                      line_size = 1,
                                      ribbon_alpha = 0.35,
                                      translate = FALSE){

  get_qnt <- function(field){
    lapply(seq_along(models), function(x){
      j <- models[[x]]$mcmccalcs$proj.quants %>%
        as_tibble() %>%
        filter(TAC == tac) %>%
        select(field) %>%
        t() %>%
        as_tibble()
      names(j) <- c("lower", "median", "upper")
      j <- j %>%
        mutate(region = regions[[x]])
      j
    }) %>%
      bind_rows()
  }
  sb_quants <- get_qnt(paste0("B", yr))
  rp_quants <- get_qnt(refpt)

  proj <- lapply(seq_along(models), function(x){
    j <- models[[x]]$mcmc$proj %>%
      filter(TAC == tac) %>%
      select(paste0("B", yr)) %>%
      as_tibble() %>%
      rename(biomass = paste0("B", yr)) %>%
      mutate(region = regions[[x]])
  }) %>%
    bind_rows()

  # Needed to have facets appear in same order as regions vector
  proj <- arrange(transform(proj, region = factor(region, levels = regions)), region)
  sb_quants <- arrange(transform(sb_quants, region = factor(region, levels = regions)), region)
  rp_quants <- arrange(transform(rp_quants, region = factor(region, levels = regions)), region)

  g <- ggplot(proj) +
    geom_density(aes(x = biomass), fill = "grey") +
    scale_x_continuous(breaks = pretty_breaks(6), limits = c(0, NA)) +
    geom_vline(data = sb_quants,
               aes(xintercept = median),
               size = line_size) +
    geom_vline(data = sb_quants,
               aes(xintercept = lower),
               linetype = "dashed",
               size = line_size) +
    geom_vline(data = sb_quants,
               aes(xintercept = upper),
               linetype = "dashed",
               size = line_size) +
    geom_vline(data = rp_quants,
               aes(xintercept = median),
               color = "red",
               size = line_size) +
    geom_rect(data = rp_quants,
              aes(xmin = lower,
                  xmax = upper,
                  ymin = -Inf,
                  ymax = Inf),
              alpha = ribbon_alpha,
              color = "transparent",
              fill = "red") +
    labs(x = paste0(en2fr("Projected spawning biomass in", translate), " ", yr,
                    " (1000 t)"),
         y = en2fr("Density", translate)) +
    facet_wrap(~ region, ncol = 2, dir = "v", scales = "free")
  g
}

#' -----------------------------------------------------------------------------------------------
#' Plot the median SSB as a line, with points which are survey index scaled by catchability value
#' for the survey
#'
#' @param df Data frame of the survey estimates, as constructed by [get_surv_ind()]
#' @param model an iscam model object
#' @param gear a gear data frame containing `gear`, `gearname`, and `qind` columns
#' @param new_surv_yr the year when the survey changed from surface to dive
#' @param point_size size for points
#' @param line_size thickness of line
#' @param xlim x-limits for the plot. Implemented with [ggplot2::coord_cartesian()]
#' @param show_x_axis see [modify_axes_labels()]
#' @param show_y_axis see [modify_axes_labels()]
#' @param x_axis_label_size see [modify_axes_labels()]
#' @param x_axis_tick_label_size see [modify_axes_labels()]
#' @param y_axis_label_size see [modify_axes_labels()]
#' @param y_axis_tick_label_size see [modify_axes_labels()]
#' @param x_axis_label_newline_length see [newline_format()]
#' @param y_axis_label_newline_length see [newline_format()]
#' @param annot a character to place in parentheses in the top left of the plot.
#' If NA, nothing will appear
#' @param show_legend Logical
#' @param translate Logical. If TRUE, translate to french
#'
#' @importFrom dplyr filter select mutate as_tibble rename
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_shape_manual ylab annotate
#' xlab theme guides element_text element_blank
#' @export
#' @return A ggplot object
plot_scaled_abundance <- function(df,
                                  model,
                                  gear,
                                  new_surv_yr = NA,
                                  point_size = 2,
                                  line_size = 2,
                                  xlim = NA,
                                  show_x_axis = TRUE,
                                  show_y_axis = TRUE,
                                  x_axis_label_size = 8,
                                  x_axis_tick_label_size = 8,
                                  y_axis_label_size = 8,
                                  y_axis_tick_label_size = 8,
                                  x_axis_label_newline_length = 30,
                                  y_axis_label_newline_length = 20,
                                  annot = NA,
                                  show_legend = FALSE,
                                  translate = FALSE){
  if(length(unique(df$region)) > 1){
    stop("There is more than one region in the df data frame", call. = FALSE)
  }
  pars <- model$mcmccalcs$p.quants[2,]
  qs <- pars[grep("^q[0-9]$", names(pars))]
  names(qs) <- gsub("q", "", names(qs))
  qs <- qs %>%
    melt(qs) %>%
    as_tibble(rownames = "qind") %>%
    mutate(qind = as.numeric(qind))

  dfm <- full_join(df, qs, by = "qind") %>%
    rename(qmedian = value.y,
           spawn = value.x) %>%
    mutate(abundance = spawn / qmedian)

  proj <- model$mcmccalcs$proj.quants
  proj_yr <- as.numeric(gsub("B", "", colnames(proj)[2]))
  proj_sbt <- as.numeric(c(proj_yr, proj[,2]))

  ssb <- t(model$mcmccalcs$sbt.quants) %>%
    as_tibble(rownames = "year") %>%
    rename(median = `50%`) %>%
    mutate(survey = ifelse(year < new_surv_yr, "Surface", "Dive"),
           year = as.numeric(year)) %>%
    filter(year != proj_yr)

  g <- ggplot(dfm, aes(x = year, y = abundance)) +
    geom_point(aes(shape = gear),
               size = point_size,
               na.rm = TRUE) +
    scale_shape_manual(values = c(2, 1)) +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10)) +
    geom_line(data = ssb,
             aes(x = year, y = median, group = survey),
             size = line_size,
             na.rm = TRUE)
  if(!is.na(xlim[1])){
    g <- g +
      coord_cartesian(xlim = xlim, expand = TRUE)
  }
  if(!is.na(annot)){
    g <- g +
      annotate(geom = "text",
               x = -Inf,
               y = Inf,
               label = paste0("(", annot, ")"),
               vjust = 1.3,
               hjust = -0.1,
               size = 3)
  }
  if(!show_legend){
    g <- g +
      guides(shape = FALSE, linetype = FALSE)
  }
  g <- modify_axes_labels(g,
                          x_label_text = newline_format(en2fr("Year", translate),
                                                        x_axis_label_newline_length),
                          y_label_text = newline_format(paste0(en2fr("Scaled abundance", translate), " (1000 t)"),
                                                        y_axis_label_newline_length),
                          show_x_axis = show_x_axis,
                          show_y_axis = show_y_axis,
                          x_axis_label_size = x_axis_label_size,
                          x_axis_tick_label_size = x_axis_tick_label_size,
                          y_axis_label_size = y_axis_label_size,
                          y_axis_tick_label_size = y_axis_tick_label_size)
  g
}

#' Plot natural mortality mcmc median and credibility interval
#'
#' @param model an iscam model
#' @param line_size thickness of the median line
#' @param ribbon_alpha transparency of the credibility interval ribbon
#' @param xlim x-limits for the plot. Implemented with [ggplot2::coord_cartesian()]
#' @param show_x_axis see [modify_axes_labels()]
#' @param show_y_axis see [modify_axes_labels()]
#' @param x_axis_label_size see [modify_axes_labels()]
#' @param x_axis_tick_label_size see [modify_axes_labels()]
#' @param y_axis_label_size see [modify_axes_labels()]
#' @param y_axis_tick_label_size see [modify_axes_labels()]
#' @param x_axis_label_newline_length see [newline_format()]
#' @param y_axis_label_newline_length see [newline_format()]
#' @param annot a character to place in parentheses in the top left of the plot.
#' If NA, nothing will appear
#' @param translate Logical. If TRUE, translate to french
#'
#' @importFrom dplyr mutate as_tibble
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon ylab annotate
#' xlab theme element_text element_blank
#' @export
#' @return A ggplot object
plot_natural_mortality <- function(model,
                                   line_size = 2,
                                   ribbon_alpha = 0.5,
                                   xlim = NA,
                                   show_x_axis = TRUE,
                                   show_y_axis = TRUE,
                                   x_axis_label_size = 8,
                                   x_axis_tick_label_size = 8,
                                   y_axis_label_size = 8,
                                   y_axis_tick_label_size = 8,
                                   x_axis_label_newline_length = 30,
                                   y_axis_label_newline_length = 20,
                                   annot = NA,
                                   translate = FALSE){
  m <- model$mcmccalcs$nat.mort.quants %>%
    t() %>%
    as_tibble(rownames = "year") %>%
    mutate(year = as.numeric(year))
  names(m) <- c("year", "lower", "median", "upper")

  g <- ggplot(m, aes(x = year, y = median)) +
    geom_line(size = line_size,
              na.rm = TRUE) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10))
  if(!is.na(xlim[1])){
    g <- g +
       coord_cartesian(xlim = xlim, expand = TRUE)
  }
  if(!is.na(annot)){
    g <- g +
      annotate(geom = "text",
               x = -Inf,
               y = Inf,
               label = paste0("(", annot, ")"),
               vjust = 1.3,
               hjust = -0.1,
               size = 3)
  }
  g <- modify_axes_labels(g,
                          x_label_text = newline_format(en2fr("Year", translate),
                                                        x_axis_label_newline_length),
                          y_label_text = newline_format(en2fr("Instantaneous natural mortality", translate),
                                                        y_axis_label_newline_length),
                          show_x_axis = show_x_axis,
                          show_y_axis = show_y_axis,
                          x_axis_label_size = x_axis_label_size,
                          x_axis_tick_label_size = x_axis_tick_label_size,
                          y_axis_label_size = y_axis_label_size,
                          y_axis_tick_label_size = y_axis_tick_label_size)
  g
}

#' Plot recruitment as points and errorbars
#'
#' @param model an iscam model object
#' @param point_size Size of points for median recruitment
#' @param line_size thickness of errorbars
#' @param xlim x-limits for the plot. Implemented with [ggplot2::coord_cartesian()]
#' @param show_x_axis see [modify_axes_labels()]
#' @param show_y_axis see [modify_axes_labels()]
#' @param x_axis_label_size see [modify_axes_labels()]
#' @param x_axis_tick_label_size see [modify_axes_labels()]
#' @param y_axis_label_size see [modify_axes_labels()]
#' @param y_axis_tick_label_size see [modify_axes_labels()]
#' @param x_axis_label_newline_length see [newline_format()]
#' @param y_axis_label_newline_length see [newline_format()]
#' @param annot a character to place in parentheses in the top left of the plot.
#' If NA, nothing will appear
#' @param translate Logical. If TRUE, translate to french
#'
#' @importFrom dplyr mutate as_tibble
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar ylab annotate
#' xlab theme element_text element_blank
#' @export
#' @return A ggplot object
plot_recruitment <- function(model,
                             point_size = 2,
                             line_size = 2,
                             xlim = NA,
                             show_x_axis = TRUE,
                             show_y_axis = TRUE,
                             x_axis_label_size = 8,
                             x_axis_tick_label_size = 8,
                             y_axis_label_size = 8,
                             y_axis_tick_label_size = 8,
                             x_axis_label_newline_length = 30,
                             y_axis_label_newline_length = 20,
                             annot = NA,
                             translate = FALSE){

  rec <- model$mcmccalcs$recr.quants %>%
    t() %>%
    as_tibble(rownames = "year") %>%
    mutate(year = as.numeric(year))
  names(rec) <- c("year", "lower", "median", "upper", "mpd")
  rec <- rec %>%
    mutate(lower = lower / 1000,
           median = median / 1000,
           upper = upper / 1000,
           mpd = mpd / 1000)

  g <- ggplot(rec, aes(x = year, y = median)) +
    geom_point(size = point_size,
               na.rm = TRUE) +
    geom_errorbar(aes(ymin = lower, ymax = upper), size = line_size / 2, width = 0) +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10))
  if(!is.na(xlim[1])){
    g <- g +
      coord_cartesian(xlim = xlim, expand = TRUE)
  }
  if(!is.na(annot)){
    g <- g +
      annotate(geom = "text",
               x = -Inf,
               y = Inf,
               label = paste0("(", annot, ")"),
               vjust = 1.3,
               hjust = -0.1,
               size = 3)
  }
  g <- modify_axes_labels(g,
                          x_label_text = newline_format(en2fr("Year", translate),
                                                        x_axis_label_newline_length),
                          y_label_text = newline_format(paste(en2fr("Number of age-2 recruits", translate), " (1000 millions)"),
                                                        y_axis_label_newline_length),
                          show_x_axis = show_x_axis,
                          show_y_axis = show_y_axis,
                          x_axis_label_size = x_axis_label_size,
                          x_axis_tick_label_size = x_axis_tick_label_size,
                          y_axis_label_size = y_axis_label_size,
                          y_axis_tick_label_size = y_axis_tick_label_size)
  g

}

#' Plot estimated biommass median and credible interval, projected biomass with credible interval,
#' catch history, and LRP line with credible interval
#'
#' @param model an iscam model object
#' @param catch_df a data frame of catch as constructed by [get_catch()]
#' @param point_size size of point for projection year median biomass
#' @param errorbar_size thickness of errorbar for projection year median biomass
#' @param line_size thickness of the median and LRP lines
#' @param ribbon_alpha transparency value for the biomass credibility interval ribbon
#' @param lrp_ribbon_alpha transparency value for the LRP credibility interval ribbon
#' @param between_bars amount of space between catch bars
#' @param refpt_show which reference point to show. See `model$mcmccalcs$r.quants`` for choices
#' @param xlim x-limits for the plot. Implemented with [ggplot2::coord_cartesian()]
#' @param show_x_axis see [modify_axes_labels()]
#' @param show_y_axis see [modify_axes_labels()]
#' @param x_axis_label_size see [modify_axes_labels()]
#' @param x_axis_tick_label_size see [modify_axes_labels()]
#' @param y_axis_label_size see [modify_axes_labels()]
#' @param y_axis_tick_label_size see [modify_axes_labels()]
#' @param x_axis_label_newline_length see [newline_format()]
#' @param y_axis_label_newline_length see [newline_format()]
#' @param annot a character to place in parentheses in the top left of the plot.
#' If NA, nothing will appear
#' @param translate Logical. If TRUE, translate to french
#'
#' @importFrom dplyr filter select mutate as_tibble
#' @importFrom ggplot2 ggplot aes geom_hline geom_rect geom_line geom_ribbon geom_point
#' geom_errorbar geom_bar ylab annotate xlab theme guides element_text element_blank
#' position_nudge scale_fill_viridis_d
#' @export
#' @return A ggplot object
plot_biomass_catch <- function(model,
                               catch_df,
                               point_size = 3,
                               errorbar_size = 1,
                               line_size = 2,
                               ribbon_alpha = 0.5,
                               lrp_ribbon_alpha = 0.35,
                               between_bars = 0.5,
                               refpt_show = "0.3sbo",
                               xlim = NA,
                               show_x_axis = TRUE,
                               show_y_axis = TRUE,
                               x_axis_label_size = 8,
                               x_axis_tick_label_size = 8,
                               y_axis_label_size = 8,
                               y_axis_tick_label_size = 8,
                               x_axis_label_newline_length = 30,
                               y_axis_label_newline_length = 20,
                               annot = NA,
                               translate = FALSE){

  if(length(unique(catch_df$region)) > 1){
    stop("There is more than one region in the catch_df data frame", call. = FALSE)
  }
  proj <- model$mcmccalcs$proj.quants
  proj_yr <- as.numeric(gsub("B", "", colnames(proj)[2]))
  proj_sbt <- as.numeric(c(proj_yr, proj[,2]))
  names(proj_sbt) <- c("year", "lower", "median", "upper")
  proj_sbt <- as_tibble(t(proj_sbt))

  sbt <- model$mcmccalcs$sbt.quants %>%
    t() %>%
    as_tibble(rownames = "year") %>%
    mutate(year = as.numeric(year)) %>%
    filter(year != proj_yr)
  names(sbt) <- c("year", "lower", "median", "upper", "mpd")

  ct <- catch_df %>%
    select(-c(area, group, sex, type, region)) %>%
    group_by(year, gear) %>%
    summarize(median = sum(value)) %>%
    ungroup()

  lrp <- model$mcmccalcs$r.quants
  lrp <- lrp[,-1] %>%
    as_tibble(rownames = "refpt") %>%
    filter(refpt == refpt_show)
  names(lrp) <- c("year", "lower", "median", "upper")
  lrp[1,1] <- min(sbt$year) - 2
  lrp[,1] <- as.numeric(lrp[,1])

  g <- ggplot(sbt, aes(x = year, y = median)) +
    geom_hline(yintercept = lrp$median,
               color = "red",
               size = line_size) +
    geom_rect(data = lrp, aes(xmin = -Inf, xmax = Inf, ymin = lrp$lower, ymax = lrp$upper),
              alpha = lrp_ribbon_alpha,
              fill = "red") +
    geom_line(size = line_size,
              na.rm = TRUE) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
    geom_point(data = proj_sbt,
               size = point_size,
               na.rm = TRUE) +
    geom_errorbar(data = proj_sbt,
                  aes(ymin = lower, ymax = upper),
                  size = errorbar_size,
                  width = 0) +
    geom_bar(data = ct,
             stat = "identity",
             width = between_bars,
             position = "stack",
             aes(fill = gear)) +
    scale_fill_viridis_d( ) +
    guides( fill = FALSE ) +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10))
  if(!is.na(xlim[1])){
    g <- g +
      coord_cartesian(xlim = xlim, expand = TRUE)
  }
  if(!is.na(annot)){
    g <- g +
      annotate(geom = "text",
               x = -Inf,
               y = Inf,
               label = paste0("(", annot, ")"),
               vjust = 1.3,
               hjust = -0.1,
               size = 3)
  }
  g <- modify_axes_labels(g,
                          x_label_text = newline_format(en2fr("Year", translate),
                                                        x_axis_label_newline_length),
                          y_label_text = newline_format(paste0(en2fr("Spawning biomass", translate), " (1000 t)"),
                                                        y_axis_label_newline_length),
                          show_x_axis = show_x_axis,
                          show_y_axis = show_y_axis,
                          x_axis_label_size = x_axis_label_size,
                          x_axis_tick_label_size = x_axis_tick_label_size,
                          y_axis_label_size = y_axis_label_size,
                          y_axis_tick_label_size = y_axis_tick_label_size)
  g
}

#' Plot recruitment deviations as points and errorbars, with a running mean of the median
#' values
#'
#' @param model an iscam model object
#' @param run_mean_yrs number of years to set the running mean calculation to
#' @param point_size Size of points for median recruitment
#' @param line_size thickness of the line for the running mean
#' @param errorbar_size thickness of errorbars for recruitment deviations
#' @param zeroline_size thickness of guide line at zero deviation
#' @param zeroline_type type of  of guide line at zero deviation
#' @param xlim x-limits for the plot. Implemented with [ggplot2::coord_cartesian()]
#' @param show_x_axis see [modify_axes_labels()]
#' @param show_y_axis see [modify_axes_labels()]
#' @param x_axis_label_size see [modify_axes_labels()]
#' @param x_axis_tick_label_size see [modify_axes_labels()]
#' @param y_axis_label_size see [modify_axes_labels()]
#' @param y_axis_tick_label_size see [modify_axes_labels()]
#' @param x_axis_label_newline_length see [newline_format()]
#' @param y_axis_label_newline_length see [newline_format()]
#' @param annot a character to place in parentheses in the top left of the plot.
#' If NA, nothing will appear
#' @param translate Logical. If TRUE, translate to french
#'
#' @importFrom dplyr filter select mutate as_tibble
#' @importFrom ggplot2 ggplot aes geom_hline geom_line geom_point geom_errorbar
#' ylab annotate xlab theme element_text element_blank
#' @importFrom zoo rollmean
#' @export
#' @return A ggplot object
plot_recruitment_devs <- function(model,
                                  run_mean_yrs = 3,
                                  point_size = 2,
                                  line_size = 2,
                                  errorbar_size = 1,
                                  zeroline_size = 1,
                                  zeroline_type = "dashed",
                                  xlim = NA,
                                  show_x_axis = TRUE,
                                  show_y_axis = TRUE,
                                  x_axis_label_size = 8,
                                  x_axis_tick_label_size = 8,
                                  y_axis_label_size = 8,
                                  y_axis_tick_label_size = 8,
                                  x_axis_label_newline_length = 30,
                                  y_axis_label_newline_length = 20,
                                  annot = NA,
                                  translate = FALSE){

  recdev <- model$mcmccalcs$recr.devs.quants %>%
    t() %>%
    as_tibble(rownames = "year") %>%
    mutate(year = as.numeric(year))
  names(recdev) <- c("year", "lower", "median", "upper")

  recdev <- recdev %>%
    mutate(runmean = rollmean(x = median,
                              k = run_mean_yrs,
                              align = "right",
                              na.pad = TRUE))

  g <- ggplot(recdev, aes(x = year, y = median)) +
    geom_hline(yintercept = 0,
               size = zeroline_size,
               linetype = zeroline_type) +
    geom_point(size = point_size,
               na.rm = TRUE) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  size = errorbar_size,
                  width = 0) +
    #geom_line(aes(y = runmean),
           #   color = "red",
             # size = line_size) +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10))
  if(!is.na(xlim[1])){
    g <- g +
      coord_cartesian(xlim = xlim, expand = TRUE)
  }
  if(!is.na(annot)){
    g <- g +
      annotate(geom = "text",
               x = -Inf,
               y = Inf,
               label = paste0("(", annot, ")"),
               vjust = 1.3,
               hjust = -0.1,
               size = 3)
  }
  g <- modify_axes_labels(g,
                          x_label_text = newline_format(en2fr("Year", translate),
                                                        x_axis_label_newline_length),
                          y_label_text = newline_format(en2fr("Log recruitment deviations", translate),
                                                        y_axis_label_newline_length),
                          show_x_axis = show_x_axis,
                          show_y_axis = show_y_axis,
                          x_axis_label_size = x_axis_label_size,
                          x_axis_tick_label_size = x_axis_tick_label_size,
                          y_axis_label_size = y_axis_label_size,
                          y_axis_tick_label_size = y_axis_tick_label_size)
  g
}

#' Plot production using a phase plot with a path through time. Includes the reference point and its credible interval
#'
#' @param model an iscam model object
#' @param catch_df a data frame of catch as constructed by [get_catch()]
#' @param new_surv_yr year in which the survey changed from surface to dive
#' @param point_size size for points for years
#' @param line_size thickness of the straight lines on the plot
#' @param path_line_size thickness of the path line between points
#' @param text_size size for year marker text labels
#' @param zeroline_size thickness of the line across zero
#' @param zeroline_type type of the line across zero
#' @param lrp_ribbon_alpha transparency of the reference point credible interval ribbon
#' @param refpt_show which reference point to show. See `model$mcmccalcs$r.quants`` for choices
#' @param show_x_axis see [modify_axes_labels()]
#' @param show_y_axis see [modify_axes_labels()]
#' @param x_axis_label_size see [modify_axes_labels()]
#' @param x_axis_tick_label_size see [modify_axes_labels()]
#' @param y_axis_label_size see [modify_axes_labels()]
#' @param y_axis_tick_label_size see [modify_axes_labels()]
#' @param x_axis_label_newline_length see [newline_format()]
#' @param y_axis_label_newline_length see [newline_format()]
#' @param annot a character to place in parentheses in the top left of the plot.
#' If NA, nothing will appear
#' @param translate Logical. If TRUE, translate to french
#'
#' @importFrom dplyr filter select mutate as_tibble group_by ungroup summarize full_join lead
#' @importFrom ggplot2 ggplot aes geom_hline geom_vline geom_rect geom_point geom_path
#' scale_color_gradient expand_limits ylab annotate xlab theme guides element_text element_blank
#' @importFrom ggrepel geom_text_repel
#' @importFrom stats na.omit
#' @export
#' @return A ggplot object
plot_biomass_phase <- function(model,
                               catch_df,
                               new_surv_yr = NA,
                               point_size = 3,
                               line_size = 2,
                               path_line_size = 0.4,
                               text_size = 2,
                               zeroline_size = 1,
                               zeroline_type = "dashed",
                               lrp_ribbon_alpha = 0.35,
                               refpt_show = "0.3sbo",
                               show_x_axis = TRUE,
                               show_y_axis = TRUE,
                               x_axis_label_size = 8,
                               x_axis_tick_label_size = 8,
                               y_axis_label_size = 8,
                               y_axis_tick_label_size = 8,
                               x_axis_label_newline_length = 30,
                               y_axis_label_newline_length = 20,
                               annot = NA,
                               translate = FALSE){

  stopifnot(!is.na(new_surv_yr),
            is.numeric(new_surv_yr),
            length(new_surv_yr) == 1)
  if(length(unique(catch_df$region)) > 1){
    stop("There is more than one region in the catch_df data frame", call. = FALSE)
  }
  sbt <- model$mcmccalcs$sbt.quants %>%
    t() %>%
    as_tibble(rownames = "year") %>%
    mutate(year = as.numeric(year))
  names(sbt) <- c("year", "lower", "median", "upper", "mpd")

  ct <- catch_df %>%
    select(-c(area, group, sex, type, region)) %>%
    group_by(year) %>%
    summarize(catch = sum(value)) %>%
    ungroup()

  dd <- full_join(sbt, ct, by = "year") %>%
    mutate(catch = ifelse(is.na(catch), 0, catch),
           mediannext = lead(median),
           catchnext = lead(catch),
           production = mediannext - median + catchnext,
           prodrate = production / median) %>%
    na.omit() %>%
    filter(year >= new_surv_yr)

  dd <- dd[-nrow(dd),] %>%
    mutate(shp = factor(0))

  lrp <- model$mcmccalcs$r.quants
  lrp <- lrp[,-1] %>%
    as_tibble(rownames = "refpt") %>%
    filter(refpt == refpt_show)
  names(lrp) <- c("year", "lower", "median", "upper")
  lrp[1,1] <- min(sbt$year) - 2
  lrp[,1] <- as.numeric(lrp[,1])

  g <- ggplot(dd, aes(x = median,
                      y = production)) +
    geom_hline(yintercept = 0,
               size = zeroline_size,
               linetype = zeroline_type) +
    geom_vline(xintercept = lrp$median,
               color = "red",
               size = line_size) +
    geom_rect(data = lrp, aes(xmin = lrp$lower,
                              xmax = lrp$upper,
                              ymin = -Inf,
                              ymax = Inf),
              alpha = lrp_ribbon_alpha,
              fill = "red",
              inherit.aes = FALSE) +
    geom_point(data = filter(dd, year != max(year)),
               aes(color = year,
                   shape = shp),
               size = point_size,
               na.rm = TRUE) +
    geom_point(data = filter(dd, year == max(year)),
               shape = 24,
               color = "black",
               fill = "white",
               size = point_size,
               na.rm = TRUE) +
    scale_color_gradient(low = "lightgrey", high = "black") +
    geom_path(size = path_line_size,
              na.rm = TRUE) +
    geom_text_repel(aes(label = year),
                    segment.colour = "lightgrey",
                    size = text_size) +
    guides(color = FALSE, shape = FALSE) +
    expand_limits(x = 0)
  if(!is.na(annot)){
    g <- g +
      annotate(geom = "text",
               x = -Inf,
               y = Inf,
               label = paste0("(", annot, ")"),
               vjust = 1.3,
               hjust = -0.1,
               size = 3)
  }
  g <- modify_axes_labels(g,
                          x_label_text = newline_format(paste0(en2fr("Spawning biomass", translate), " (1000 t)"),
                                                        x_axis_label_newline_length),
                          y_label_text = newline_format(paste0(en2fr("Spawning biomass production", translate), " (1000 t)"),
                                                        y_axis_label_newline_length),
                          show_x_axis = show_x_axis,
                          show_y_axis = show_y_axis,
                          x_axis_label_size = x_axis_label_size,
                          x_axis_tick_label_size = x_axis_tick_label_size,
                          y_axis_label_size = y_axis_label_size,
                          y_axis_tick_label_size = y_axis_tick_label_size)
  g
}

#' Plot Harvest control rules as pairs plots
#'
#' @param hcr.lst A list of length equal to the length of the sbt list, of vectors of length two,
#'  containing the catch limit (tac) and associated harvest rate (hr)
#' @param sbt.lst a list of vectors of timeseries biomass values (typically many posteriors)
#' @param mp name of the management procedure to appear on the blank panel
#' @param region name for the region to appear on the blank panel
#' @param probs vector of length 2 for credible interval of median
#' @param show.medians show the median lines and credible intervals
#' @param show.means show the mean lines
#' @param show.x.axes if TRUE, axes labels, tick marks and tick labels will be shown on the lower plot's x axes
#' @param panel.text.size text size for the description panels
#' @param point.size size of points
#' @param line.width width ot thickness of all lines
#' @param axis.text.size size of text for axis labels
#'
#' @return a ggplot object
#' @export
#' @importFrom stats quantile
#' @importFrom grid unit
#' @importFrom cowplot plot_grid
plot_hcr <- function(hcr.lst,
                     sbt.lst,
                     mp = "",
                     region = "",
                     probs = c(0.025, 0.975),
                     point.size = 0.05,
                     line.width = 0.5,
                     show.medians = TRUE,
                     show.means = TRUE,
                     show.x.axes = FALSE,
                     axis.text.size = 7,
                     panel.text.size = 3){

  label <- paste0(region, "\n", gsub("_", "\n", mp))
  tac <- sapply(hcr.lst, "[[", 1)
  hr <- sapply(hcr.lst, "[[", 2)
  sbt <- sapply(sbt.lst, "[[", length(sbt.lst[[1]]))
  df <- tibble(tac = tac,
               hr = hr,
               sbt = sbt)
  if(all(is.na(df$hr)) || all(is.na(df$tac))){
    df <- data.frame()
    g <- ggplot(df) +
      geom_point() +
      annotate("text", x = 100, y = 20, label = "No data", size = panel.text.size) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      xlab("") +
      ylab("")
    h <- ggplot(df) +
      geom_point() +
      annotate("text", x = 100, y = 20, label = "No data", size = panel.text.size) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      xlab("") +
      ylab("")
    i <- ggplot(df) +
      geom_point() +
      annotate("text", x = 100, y = 20, label = "No data", size = panel.text.size) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      xlab("") +
      ylab("")
  }else{
    g <- ggplot(df, aes(x = tac, y = hr)) +
      geom_point(size = point.size,
                 na.rm = TRUE) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title = element_text(size = axis.text.size)) +
      ylab("") +
      xlab("TAC")

    h <- ggplot(df, aes(x = sbt, y = tac)) +
      geom_point(size = point.size,
                 na.rm = TRUE) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title = element_text(size = axis.text.size)) +
      xlab("") +
      ylab("TAC")

    i <- ggplot(df, aes(x = sbt, y = hr)) +
      geom_point(size = point.size,
                 na.rm = TRUE) +
      theme(axis.title = element_text(size = axis.text.size)) +
      xlab("Projected SBt") +
      ylab("Harvest Rate")

    if(show.medians){
      quants.tac <- as_tibble(t(as.data.frame(quantile(tac, probs = probs, na.rm = TRUE)))) %>%
        rename(lower = 1, upper = 2)
      quants.hr <- as_tibble(t(as.data.frame(quantile(hr, probs = probs, na.rm = TRUE)))) %>%
        rename(lower = 1, upper = 2)
      quants.sbt <- as_tibble(t(as.data.frame(quantile(sbt, probs = probs)))) %>%
        rename(lower = 1, upper = 2)
      g <- g +
        geom_vline(xintercept = median(tac),
                   color = "red",
                   size = line.width,
                   linetype = "dashed") +
        geom_rect(data = quants.hr,
                  aes(ymin = quants.hr$lower,
                      ymax = quants.hr$upper,
                      xmin = -Inf,
                      xmax = Inf),
                  inherit.aes = FALSE,
                  alpha = 0.2,
                  color = "transparent",
                  fill = "red") +
        geom_rect(data = quants.tac,
                  aes(xmin = quants.tac$lower,
                      xmax = quants.tac$upper,
                      ymin = -Inf,
                      ymax = Inf),
                  inherit.aes = FALSE,
                  alpha = 0.2,
                  color = "transparent",
                  fill = "red") +
        geom_hline(yintercept = median(hr),
                   color = "red",
                   size = line.width,
                   linetype = "dashed")
      h <- h +
        geom_hline(yintercept = median(tac),
                   color = "red",
                   size = line.width,
                   linetype = "dashed") +
        geom_vline(xintercept = median(sbt),
                   color = "red",
                   size = line.width,
                   linetype = "dashed") +
        geom_rect(data = quants.sbt,
                  aes(xmin = quants.sbt$lower,
                      xmax = quants.sbt$upper,
                      ymin = -Inf,
                      ymax = Inf),
                  inherit.aes = FALSE,
                  alpha = 0.2,
                  color = "transparent",
                  fill = "red") +
        geom_rect(data = quants.tac,
                  aes(ymin = quants.tac$lower,
                      ymax = quants.tac$upper,
                      xmin = -Inf,
                      xmax = Inf),
                  inherit.aes = FALSE,
                  alpha = 0.2,
                  color = "transparent",
                  fill = "red")
      i <- i +
        geom_hline(yintercept = median(hr),
                   color = "red",
                   size = line.width,
                   linetype = "dashed") +
        geom_vline(xintercept = median(sbt),
                   color = "red",
                   size = line.width,
                   linetype = "dashed") +
        geom_rect(data = quants.sbt,
                  aes(xmin = quants.sbt$lower,
                      xmax = quants.sbt$upper,
                      ymin = -Inf,
                      ymax = Inf),
                  inherit.aes = FALSE,
                  alpha = 0.2,
                  color = "transparent",
                  fill = "red") +
        geom_rect(data = quants.hr,
                  aes(ymin = quants.hr$lower,
                      ymax = quants.hr$upper,
                      xmin = -Inf,
                      xmax = Inf),
                  inherit.aes = FALSE,
                  alpha = 0.2,
                  color = "transparent",
                  fill = "red")
    }
    if(show.means){
      g <- g +
        geom_vline(xintercept = mean(tac),
                   color = "green",
                   size = line.width,
                   linetype = "dashed") +
        geom_hline(yintercept = mean(hr),
                   color = "green",
                   size = line.width,
                   linetype = "dashed")
      h <- h +
        geom_hline(yintercept = mean(tac),
                   color = "green",
                   size = line.width,
                   linetype = "dashed")
      i <- i +
        geom_hline(yintercept = mean(hr),
                   color = "green",
                   size = line.width,
                   linetype = "dashed")
    }
  }
  if(!show.x.axes){
    i <- i +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      xlab("")
    g <- g +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      xlab("")
  }
  ff <- unit(c(0, 0, 0, 0), "cm")
  g <- g +
    theme(plot.margin = ff)
  h <- h +
    theme(plot.margin = ff)
  i <- i +
    theme(plot.margin = ff)
  j <- ggplot() +
    annotate("text", x = 100, y = 20, label = label, size = panel.text.size) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("") +
    ylab("")  +
    theme(plot.margin = ff)

  plot_grid(h,
            j,
            i,
            g,
            nrow = 2,
            ncol = 2,
            align = "hv",
            axis = "tblr")
}

#' Plot Beverton-holt
#'
#' @param models iscam models list
#' @param regions the regions to include
#'
#' @export
#' @importFrom tibble enframe
plot_bh <- function(models,
                    regions){
  # Note sbt goes from start year to end year + 1
  # rt goes from start year + start age to end year

  # lapply !!!
  sbt <- model$mpd$sbt
  rt <- model$mpd$rt

  sbt_yrs <- model$mpd$yrs
  names(sbt) <- sbt_yrs
  sbt <- enframe(sbt) %>%
    rename(year = name)
  sage <- model$dat$start.age
  rt_yrs <- sbt_yrs[(sage + 1):(length(sbt_yrs) - 1)]
  names(rt) <- rt_yrs
  rt <- enframe(rt) %>%
    rename(year = name)

  d <- sbt %>%
    full_join(rt, by = "year") %>%
    rename(sbt = value.x,
           rt = value.y) #%>%
  #mutate(year = factor(year))
  g <- ggplot(d, aes(x = sbt, y = rt)) +
    geom_point(aes(color = year,
                   shape = year),
                   na.rm = TRUE) #+
  # geom_point( data=filter(bhSub, Year==max(yrRange)), shape=24,
  #             colour="black", fill="white") +
  # geom_point( data=bhPredSub, aes(x=sbo, y=ro), shape=8 ) +
  # geom_line( data=bhPredSub ) +

  g
}
