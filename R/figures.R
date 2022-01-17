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
                       translate = FALSE) {
  df <- df %>%
    filter(year >= xlim[1])
  g <- ggplot(df, aes(x = year, y = value)) +
    geom_bar(stat = "identity", position = "stack", aes(fill = gear), width = 1) +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10)) +
    coord_cartesian(xlim) +
    expand_limits(x = xlim[1]:xlim[2]) +
    scale_fill_viridis_d() +
    theme(legend.position = "top") +
    labs(
      x = en2fr("Year", translate),
      y = paste(en2fr("Catch", translate),
                ifelse(translate, " (1 000 t)", " (1,000 t)")),
      fill = en2fr("Gear", translate)
    ) +
    facet_wrap(vars(region), ncol = 1, scales = "free_y")
  g
}

#' Plot incidental catch from a data frame
#'
#' @param df Data frame of incidental catch.
#' @param xlim Year limits for the plot (or NA for none).
#' @param translate Translate to French?
#'
#' @importFrom ggplot2 ggplot aes geom_col labs facet_wrap vars theme
#'   scale_fill_viridis_d
#' @importFrom dplyr mutate
#' @importFrom rosettafish en2fr
#' @importFrom scales comma
#'
#' @export
#' @return A ggplot object.
plot_ic <- function(df,
                    xlim = c(NA, NA),
                    translate = FALSE) {
  df <- df %>%
    mutate(Number = Number / 1000)
  if(translate){
    df <- df %>%
      mutate(Type = en2fr(Type, translate, case = "sentence"))
  }
  if(!all(is.na(xlim))){
    df <- df %>%
      filter(Year %in% xlim[1]:xlim[2])
  }
  g <- ggplot(data = df, mapping = aes(x = Year, y = Number, fill = Type)) +
    geom_col(position = "dodge") +
    labs(x = en2fr("Year", translate, case = "sentence"),
         y = paste0(en2fr("Number of fish", translate, case = "sentence"),
                    ifelse(translate, " (x 1 000)", " (x 1,000)")),
         fill = en2fr("Type", translate, case = "sentence")) +
    scale_fill_viridis_d() +
    scale_y_continuous(labels = comma) +
    facet_wrap(vars(Region), ncol = 1, scales = "free_y") +
    theme(legend.position = "top")
  if(!all(is.na(xlim))){
    g <- g +
      expand_limits(x = xlim)
  }
  g
}

#' Plot weight-at-age time series from a data frames as extracted from iscam data (dat) files
#'
#' @param df a data frame as constructed by [get_wa()]
#' @param circle_age the age for which to add circles to plot
#' @param xlim Limits for the years shown on the plot
#' @param ylim limits for the weights shown on the plot
#' @param n_roll Number of years to calculate the rolling mean (window)
#' @param major Logical. Major SAR or not.
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
                    n_roll = 5,
                    major = TRUE,
                    translate = FALSE) {
  df <- df %>%
    filter(year >= xlim[1], gear %in% c(
      en2fr("Other", translate = french),
      en2fr("RoeSN", translate = french)
    ))
  dfm <- melt(df, id.vars = c("year", "area", "group", "sex", "region", "gear")) %>%
    as_tibble() %>%
    rename(
      Year = year,
      Age = variable,
      Weight = value
    ) %>%
    select(-c(area, group, sex)) %>%
    group_by(region, Age) %>%
    mutate(muWeight = rollmean(x = Weight, k = n_roll, align = "right", na.pad = TRUE)) %>%
    ungroup() %>%
    mutate(Age = factor(Age))
  dfm_circle_age <- dfm %>%
    filter(Age == circle_age)
  dfm <- dfm %>%
    filter(Age != circle_age)

  dshade <- data.frame(Age = c(2:10), Shade = c(.8, 1, .9, .8, .4, .3, .2 , .1, .1))
  dfm <- merge(dfm, dshade, by = "Age", all.x=TRUE)

  g <- ggplot(dfm) +
    geom_point(
      data = dfm_circle_age,
      aes(x = Year, y = Weight),
      shape = 1,
      size = 2,
      na.rm = TRUE
    ) +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10)) +
    coord_cartesian(xlim, ylim) +
    expand_limits(x = xlim[1]:xlim[2]) +
    labs(
      x = en2fr("Year", translate),
      y = paste0(en2fr("Weight-at-age", translate), " (kg)")
    ) +
    facet_wrap(vars(region), ncol = 1)
  if(major) {
    g <- g +
      geom_line(aes(x = Year, y = muWeight, group = Age, alpha = Shade), na.rm = TRUE ) +
      geom_line(
        data = dfm_circle_age, aes(x = Year, y = muWeight), size = .8,
        na.rm = TRUE
      ) +
      guides(alpha = "none")
  } else {
    g <- g +
      geom_line(aes(x = Year, y = Weight, group = Age, alpha = Shade), na.rm = TRUE ) +
      geom_line(
        data = dfm_circle_age, aes(x = Year, y = Weight), size = .8,
        na.rm = TRUE
      ) +
      guides(alpha = "none")
  }
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
                    size_range = c(0.5, 2.5),
                    translate = FALSE) {
  df <- df %>%
    filter(year >= xlim[1], gear %in% c(
      en2fr("Other", translate = french),
      en2fr("RoeSN", translate = french)
    ))
  dfm <- melt(df, id.vars = c("year", "area", "group", "sex", "region", "gear")) %>%
    as_tibble() %>%
    rename(
      Region = region,
      Year = year,
      Age = variable,
      Number = value
    ) %>%
    select(-c(area, group, sex)) %>%
    mutate(
      Age = as.numeric(as.character(Age)),
      Age = ifelse(Age > age_plus, age_plus, Age)
    ) %>%
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
    summarize(
      MeanAge = weighted.mean(x = Age, w = Proportion),
      sBar = qnorm(1 - (1 - conf) / 2) * sum(sqrt(Proportion * (1 - Proportion)) / sqrt(Age)),
      Lower = exp(log(MeanAge) - log(sBar)),
      Upper = exp(log(MeanAge) + log(sBar))
    ) %>%
    ungroup() %>%
    mutate(GroupID = consecutive_group(Year))

  g <- ggplot(dfm, aes(x = Year)) +
    geom_point(aes(
      y = Age,
      size = ifelse(Proportion, Proportion, NA),
      alpha =  ifelse(Proportion, Proportion, NA)
      ),
    na.rm = TRUE,
    show.legend = FALSE
    ) +
    scale_alpha(range = c(0.4, 1))+
    geom_path(
      data = dfm_ci,
      aes(y = MeanAge, group = GroupID),
      size = 1.25,
      na.rm = TRUE,
      alpha = .3
    ) +
    scale_size_continuous(range = size_range) +
    geom_ribbon(
      data = dfm_ci,
      aes(ymin = Lower, ymax = Upper, group = GroupID),
      alpha = 0.25
    ) +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10)) +
    coord_cartesian(xlim, ylim) +
    expand_limits(x = xlim[1]:xlim[2]) +
    labs(
      size = en2fr("Proportion", translate),
      x = en2fr("Year", translate),
      y = en2fr("Age", translate)
    ) +
    facet_wrap(vars(Region), ncol = 1) +
    guides(alpha = "none")
  g
}

#' Plot survey indices from data frames as extracted from iscam data (dat) files
#'
#' @param df a data frame as constructed by [get_surv_ind()]
#' @param yr_range Start and end year, or NA to include all the data
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
                           yr_range = NA,
                           new_surv_yr = NA,
                           new_surv_yr_type = "dashed",
                           new_surv_yr_size = 0.25,
                           translate = FALSE) {
  stopifnot(
    !is.na(new_surv_yr),
    is.numeric(new_surv_yr),
    length(new_surv_yr) == 1
  )
  if(!is.na(yr_range)) {
    df <- df %>%
      filter(year %in% (min(yr_range):max(yr_range))) %>%
      complete(year = min(yr_range):max(yr_range), region)
  }
  df <- df %>%
    complete(year = min(df$year):max(df$year), region) %>%
    mutate(
      gear = ifelse(year < new_surv_yr, "Surface", "Dive"),
      gear = factor(gear, levels=c("Surface", "Dive"))
    ) %>%
    select(-qind)

  dfm <- melt(df, id.vars = c("year", "area", "group", "sex", "region", "wt", "timing", "gear")) %>%
    as_tibble() %>%
    rename(
      Region = region,
      Year = year,
      Index = value
    ) %>%
    select(-c(area, group, sex, wt, timing))

  g <- ggplot(dfm, aes(x = Year, y = Index)) +
    geom_point(aes(shape = gear),
      na.rm = TRUE
    ) +
    geom_line(aes(group = gear),
      na.rm = TRUE
    ) +
    scale_shape_manual(values = c(1, 2)) +
    geom_vline(xintercept = new_surv_yr - 0.5, linetype = new_surv_yr_type, size = new_surv_yr_size) +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10)) +
    labs(
      shape = en2fr("Survey period", translate),
      x = en2fr("Year", translate),
      y = paste0(en2fr("Spawn index", translate),
                 ifelse(translate, " (1 000 t)", " (1,000 t)"))
    ) +
    facet_wrap(vars(Region), ncol = 1, scales = "free_y") +
    theme(legend.position = "top")
  if(!is.na(yr_range)){
    g <- g +
      expand_limits(x = yr_range)
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
                              translate = FALSE) {
  dfm <- df %>%
    group_by(year, region) %>%
    summarize(ct = sum(value)) %>%
    ungroup()

  ssb <- lapply(seq_along(models), function(x) {
    j <- t(models[[x]]$mcmccalcs$sbt.quants) %>%
      as_tibble(rownames = "year") %>%
      mutate(year = as.numeric(year))
    names(j) <- c("year", "lower", "median", "upper", "mpd")
    j <- j %>%
      full_join(dfm, by = "year") %>%
      filter(region == regions[[x]]) %>%
      mutate(
        utlower = ct / (ct + lower),
        ut = ct / (ct + median),
        utupper = ct / (ct + upper),
        region = as.character(region)
      )
    yrs <- as.numeric(min(j$year):max(j$year))
    all_yrs <- tibble(
      year = yrs,
      region = rep(regions[[x]], length(yrs))
    )
    j <- j %>%
      full_join(all_yrs, by = c("year", "region")) %>%
      arrange(year)
  }) %>%
    bind_rows()

  # Needed to have facets appear in same order as regions vector
  ssb <- arrange(transform(ssb, region = factor(region, levels = regions)), region)

  g <- ggplot(ssb, aes(x = year, y = ut)) +
    geom_line(
      size = line_size,
      na.rm = TRUE
    ) +
    geom_ribbon(aes(ymin = utlower, ymax = utupper), alpha = ribbon_alpha) +
    geom_hline(yintercept = h_line, linetype = "dashed") +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10)) +
    labs(
      x = en2fr("Year", translate),
      y = en2fr("Effective harvest rate", translate)
    ) +
    facet_wrap(vars(region), ncol = 1)

  if (!is.na(ylim[1])) {
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
                                      translate = FALSE) {
  get_qnt <- function(field) {
    lapply(seq_along(models), function(x) {
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

  proj <- lapply(seq_along(models), function(x) {
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
    geom_vline(
      data = sb_quants,
      aes(xintercept = median),
      size = line_size
    ) +
    geom_vline(
      data = sb_quants,
      aes(xintercept = lower),
      linetype = "dashed",
      size = line_size
    ) +
    geom_vline(
      data = sb_quants,
      aes(xintercept = upper),
      linetype = "dashed",
      size = line_size
    ) +
    geom_vline(
      data = rp_quants,
      aes(xintercept = median),
      color = "red",
      size = line_size
    ) +
    geom_rect(
      data = rp_quants,
      aes(
        xmin = lower,
        xmax = upper,
        ymin = -Inf,
        ymax = Inf
      ),
      alpha = ribbon_alpha,
      color = "transparent",
      fill = "red"
    ) +
    labs(
      x = paste0(
        en2fr("Projected spawning biomass in", translate), " ", yr,
        ifelse(translate, " (1 000 t)", " (1,000 t)")
      ),
      y = en2fr("Density", translate)
    ) +
    facet_wrap(~region, ncol = 1, dir = "v", scales = "free")
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
                                  point_size = 1,
                                  line_size = 0.75,
                                  xlim = NA,
                                  show_x_axis = TRUE,
                                  show_y_axis = TRUE,
                                  x_axis_label_size = 8,
                                  x_axis_tick_label_size = 8,
                                  y_axis_label_size = 8,
                                  y_axis_tick_label_size = 8,
                                  x_axis_label_newline_length = 50,
                                  y_axis_label_newline_length = 40,
                                  annot = "a",
                                  show_legend = FALSE,
                                  translate = FALSE) {
  if (length(unique(df$region)) > 1) {
    stop("There is more than one region in the df data frame", call. = FALSE)
  }
  pars <- model$mcmccalcs$p.quants[2, ]
  qs <- pars[grep("^q[0-9]$", names(pars))]
  names(qs) <- gsub("q", "", names(qs))
  qs <- qs %>%
    melt(qs) %>%
    as_tibble(rownames = "qind") %>%
    mutate(qind = as.numeric(qind))

  dfm <- full_join(df, qs, by = "qind") %>%
    rename(
      qmedian = value.y,
      spawn = value.x
    ) %>%
    mutate(abundance = spawn / qmedian)

  proj <- model$mcmccalcs$proj.quants
  proj_yr <- as.numeric(gsub("B", "", colnames(proj)[2]))
  proj_sbt <- as.numeric(c(proj_yr, proj[, 2]))

  ssb <- t(model$mcmccalcs$sbt.quants) %>%
    as_tibble(rownames = "year") %>%
    rename(median = `50%`) %>%
    mutate(
      survey = ifelse(year < new_surv_yr, "Surface", "Dive"),
      year = as.numeric(year)
    ) %>%
    filter(year != proj_yr)

  g <- ggplot(dfm, aes(x = year, y = abundance)) +
    geom_point(aes(shape = gear),
      size = point_size,
      na.rm = TRUE
    ) +
    scale_shape_manual(values = c(2, 1)) +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10),
                       labels = NULL, name = NULL,
                       sec.axis = sec_axis(
                         ~ ., name = NULL,
                         breaks = seq(from = 1900, to = 2100, by = 10),
                         labels = seq(from = 1900, to = 2100, by = 10)),
                       ) +
    geom_line(
      data = ssb,
      aes(x = year, y = median, group = survey),
      size = line_size,
      na.rm = TRUE
    ) +
    expand_limits(y = 0)
  if (!is.na(xlim[1])) {
    g <- g +
      coord_cartesian(xlim = xlim, expand = TRUE)
  }
  if (!is.na(annot)) {
    g <- g +
      annotate(
        geom = "text",
        x = -Inf,
        y = Inf,
        label = paste0("(", annot, ")"),
        vjust = 1.3,
        hjust = -0.1,
        size = 3
      )
  }
  if (!show_legend) {
    g <- g +
      guides(shape = FALSE, linetype = FALSE)
  }
  g <- modify_axes_labels(g,
    # x_label_text = newline_format(
    #   en2fr("Year", translate),
    #   x_axis_label_newline_length
    # ),
    y_label_text = newline_format(
      paste0(en2fr("Scaled abundance", translate),
             ifelse(translate, " (1 000 t)", " (1,000 t)")),
      y_axis_label_newline_length
    ),
    show_x_axis = show_x_axis,
    show_y_axis = show_y_axis,
    x_axis_label_size = x_axis_label_size,
    x_axis_tick_label_size = x_axis_tick_label_size,
    y_axis_label_size = y_axis_label_size,
    y_axis_tick_label_size = y_axis_tick_label_size
  )
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
                                   line_size = 0.75,
                                   ribbon_alpha = 0.5,
                                   xlim = NA,
                                   y_max = NA,
                                   show_x_axis = TRUE,
                                   show_y_axis = TRUE,
                                   x_axis_label_size = 8,
                                   x_axis_tick_label_size = 8,
                                   y_axis_label_size = 8,
                                   y_axis_tick_label_size = 8,
                                   x_axis_label_newline_length = 50,
                                   y_axis_label_newline_length = 40,
                                   annot = "b",
                                   translate = FALSE) {
  m <- model$mcmccalcs$nat.mort.quants %>%
    t() %>%
    as_tibble(rownames = "year") %>%
    mutate(year = as.numeric(year))
  names(m) <- c("year", "lower", "median", "upper")

  g <- ggplot(m, aes(x = year, y = median)) +
    geom_line(
      size = line_size,
      na.rm = TRUE
    ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10),
                       labels = NULL, name = NULL,
                       sec.axis = sec_axis(
                         ~ ., name = NULL,
                         breaks = seq(from = 1900, to = 2100, by = 10),
                         labels = seq(from = 1900, to = 2100, by = 10)),
    ) +
    expand_limits(y = c(0, y_max))
  if (!is.na(xlim[1])) {
    g <- g +
      coord_cartesian(xlim = xlim, expand = TRUE)
  }
  if (!is.na(annot)) {
    g <- g +
      annotate(
        geom = "text",
        x = -Inf,
        y = Inf,
        label = paste0("(", annot, ")"),
        vjust = 1.3,
        hjust = -0.1,
        size = 3
      )
  }
  g <- modify_axes_labels(g,
    # x_label_text = newline_format(
    #   en2fr("Year", translate),
    #   x_axis_label_newline_length
    # ),
    y_label_text = newline_format(
      paste0(
        en2fr(
          "Instantaneous natural mortality",
          translate
        ), " (/",
        en2fr("Year", translate, case = "lower"),
        ")"
      ),
      y_axis_label_newline_length
    ),
    show_x_axis = show_x_axis,
    show_y_axis = show_y_axis,
    x_axis_label_size = x_axis_label_size,
    x_axis_tick_label_size = x_axis_tick_label_size,
    y_axis_label_size = y_axis_label_size,
    y_axis_tick_label_size = y_axis_tick_label_size
  )
  g
}

#' Plot recruitment as points and errorbars
#'
#' @param model an iscam model object
#' @param point_size Size of points for median recruitment
#' @param line_size thickness of errorbars
#' @param xlim x-limits for the plot. Implemented with
#'   [ggplot2::coord_cartesian()]
#' @param show_x_axis see [modify_axes_labels()]
#' @param show_y_axis see [modify_axes_labels()]
#' @param x_axis_label_size see [modify_axes_labels()]
#' @param x_axis_tick_label_size see [modify_axes_labels()]
#' @param y_axis_label_size see [modify_axes_labels()]
#' @param y_axis_tick_label_size see [modify_axes_labels()]
#' @param x_axis_label_newline_length see [newline_format()]
#' @param y_axis_label_newline_length see [newline_format()]
#' @param show_r0 Add horizontal like for unfished recruitment R_0, and shaded
#'   rectangle for CI.
#' @param line_size_r0 Thickness for horizontal line
#' @param r0_ribbon_alpha Alpha for R_0 rectangle (uncertainty).
#' @param annot a character to place in parentheses in the top left of the plot.
#'   If NA, nothing will appear
#' @param translate Logical. If TRUE, translate to french
#'
#' @importFrom dplyr mutate as_tibble
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar ylab annotate xlab
#'   theme element_text element_blank
#' @export
#' @return A ggplot object
plot_recruitment <- function(model,
                             point_size = 1,
                             line_size = 1,
                             xlim = NA,
                             show_x_axis = FALSE,
                             show_y_axis = TRUE,
                             x_axis_label_size = 8,
                             x_axis_tick_label_size = 8,
                             y_axis_label_size = 8,
                             y_axis_tick_label_size = 8,
                             x_axis_label_newline_length = 50,
                             y_axis_label_newline_length = 40,
                             annot = "c",
                             show_r0 = TRUE,
                             line_size_r0 = 0.5,
                             r0_ribbon_alpha = 0.35,
                             translate = FALSE) {
  rec <- model$mcmccalcs$recr.quants %>%
    t() %>%
    as_tibble(rownames = "year") %>%
    mutate(year = as.numeric(year))
  names(rec) <- c("year", "lower", "median", "upper", "mpd")
  rec <- rec %>%
    mutate(
      lower = lower / 1000,
      median = median / 1000,
      upper = upper / 1000,
      mpd = mpd / 1000
    )

  r0 <- model$mcmccalcs$p.quants[, "ro"] / 1000
  names(r0) <- c("lower", "median", "upper")

  g <- ggplot(rec, aes(x = year, y = median)) +
    geom_point(size = point_size, na.rm = TRUE) +
    geom_line(size = 0.5, colour = "darkgrey") +
    geom_errorbar(aes(ymin = lower, ymax = upper),
      size = line_size / 2,
      width = 0
    ) +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10)) +
    expand_limits(y = 0)
  if (!is.na(xlim[1])) {
    g <- g +
      coord_cartesian(xlim = xlim, expand = TRUE)
  }
  if (!is.na(annot)) {
    g <- g +
      annotate(
        geom = "text",
        x = -Inf,
        y = Inf,
        label = paste0("(", annot, ")"),
        vjust = 1.3,
        hjust = -0.1,
        size = 3
      )
  }

  if (show_r0) {
    g <- g +
      geom_hline(yintercept = r0["median"], size = line_size_r0,
                 colour = "darkgrey") +
      annotate(
        geom = "rect", xmin = -Inf, xmax = Inf, ymin = r0["lower"],
        ymax = r0["upper"], alpha = r0_ribbon_alpha
      )
  }

  g <- modify_axes_labels(g,
    #x_label_text = newline_format(
    #  en2fr("Year", translate),
    #  x_axis_label_newline_length
    #),
    y_label_text = newline_format(
      paste(en2fr("Recruitment", translate),
            ifelse(translate, " (1 000 millions)", " (1,000 million)")),
      y_axis_label_newline_length
    ),
    show_x_axis = show_x_axis,
    show_y_axis = show_y_axis,
    x_axis_label_size = x_axis_label_size,
    x_axis_tick_label_size = x_axis_tick_label_size,
    y_axis_label_size = y_axis_label_size,
    y_axis_tick_label_size = y_axis_tick_label_size
  )
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
#' @param show_usr Logical. Show the upper stock reference. Default TRUE.
#' @param prod_yrs Numeric vector. Productive period to calculate the USR.
#'   Default 1990:1999.
#' @param prop_prod Numeric. Proportion of productive period for USR. Default
#'   1.0.
#' @param show_prod_yrs Logical. Show vertical band for productive period.
#'   Default TRUE.
#' @param show_sbo Logical. Show SB_0. Default TRUE.
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
                               point_size = 1,
                               errorbar_size = 0.5,
                               line_size = 0.75,
                               ribbon_alpha = 0.5,
                               lrp_ribbon_alpha = 0.35,
                               between_bars = 0.75,
                               refpt_show = "0.3sbo",
                               show_usr = TRUE,
                               prod_yrs = 1990:1999,
                               prop_prod = 1.0,
                               show_prod_yrs = TRUE,
                               show_sbo = TRUE,
                               xlim = NA,
                               show_x_axis = TRUE,
                               show_y_axis = TRUE,
                               x_axis_label_size = 8,
                               x_axis_tick_label_size = 8,
                               y_axis_label_size = 8,
                               y_axis_tick_label_size = 8,
                               x_axis_label_newline_length = 50,
                               y_axis_label_newline_length = 40,
                               annot = "d",
                               translate = FALSE) {
  if (length(unique(catch_df$region)) > 1) {
    stop("There is more than one region in the catch_df data frame", call. = FALSE)
  }
  proj <- model$mcmccalcs$proj.quants
  proj_yr <- as.numeric(gsub("B", "", colnames(proj)[2]))
  proj_sbt <- as.numeric(c(proj_yr, proj[, 2]))
  names(proj_sbt) <- c("year", "lower", "median", "upper")
  proj_sbt <- as_tibble(t(proj_sbt))

  sbt <- model$mcmccalcs$sbt.quants %>%
    t() %>%
    as_tibble(rownames = "year") %>%
    mutate(year = as.numeric(year)) %>%
    filter(year != proj_yr)
  names(sbt) <- c("year", "lower", "median", "upper", "mpd")

  prod_period <- sbt %>%
    filter(year %in% prod_yrs) %>%
    select(-year) %>%
    summarise(
      lower = mean(lower)*prop_prod,
      median = mean(median)*prop_prod,
      upper = mean(upper)* prop_prod
    )

  sbo <- as.numeric(model$mcmccalcs$r.quants["sbo", 2:4])

  ct <- catch_df %>%
    select(-c(area, group, sex, type, region)) %>%
    group_by(year, gear) %>%
    summarize(median = sum(value)) %>%
    ungroup()

  lrp <- model$mcmccalcs$r.quants
  lrp <- lrp[, -1] %>%
    as_tibble(rownames = "refpt") %>%
    filter(refpt == refpt_show)
  names(lrp) <- c("year", "lower", "median", "upper")
  lrp[1, 1] <- as.character(min(sbt$year) - 2)
  lrp[, 1] <- as.numeric(lrp[, 1])

  g <- ggplot(sbt, aes(x = year, y = median)) +
    geom_hline(
      yintercept = lrp$median,
      color = "red",
      size = line_size
    ) +
    geom_rect(
      data = lrp, aes(xmin = -Inf, xmax = Inf, ymin = lrp$lower, ymax = lrp$upper),
      alpha = lrp_ribbon_alpha,
      fill = "red"
    ) +
    geom_bar(
      data = ct,
      stat = "identity",
      width = between_bars,
      position = "stack",
      aes(fill = gear)
    ) +
    geom_line(
      size = line_size,
      na.rm = TRUE
    ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
    geom_point(
      data = proj_sbt,
      size = point_size,
      na.rm = TRUE
    ) +
    geom_errorbar(
      data = proj_sbt,
      aes(ymin = lower, ymax = upper),
      size = errorbar_size,
      width = 0
    ) +
    scale_fill_viridis_d() +
    guides(fill = FALSE) +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10),
                       labels = seq(from = 1900, to = 2100, by = 10),
                       name = NULL)

  if (show_sbo) {
    g <- g +
      geom_hline(yintercept = sbo[2]) +
      annotate(geom = "rect", fill="black", alpha = lrp_ribbon_alpha,
               xmin = -Inf, xmax = Inf, ymin = sbo[1], ymax = sbo[3])
  }

  if (show_usr) {
    g <- g +
      geom_hline(yintercept = prod_period$median, colour = "green") +
      annotate(geom = "rect", fill="green", alpha = lrp_ribbon_alpha,
               xmin = -Inf, xmax = Inf,
               ymin = prod_period$lower, ymax = prod_period$upper)
  }

  if (show_prod_yrs){
    g <- g +
      annotate(geom = "rect", fill = "purple", alpha = lrp_ribbon_alpha,
               xmin = min(prod_yrs) - 0.5, xmax = max(prod_yrs) + 0.5,
               ymin = -Inf, ymax = Inf)
  }

  if (!is.na(xlim[1])) {
    g <- g +
      coord_cartesian(xlim = xlim, expand = TRUE)
  }
  if (!is.na(annot)) {
    g <- g +
      annotate(
        geom = "text",
        x = -Inf,
        y = Inf,
        label = paste0("(", annot, ")"),
        vjust = 1.3,
        hjust = -0.1,
        size = 3
      )
  }
  g <- modify_axes_labels(g,
    # x_label_text = newline_format(
    #   en2fr("Year", translate),
    #   x_axis_label_newline_length
    # ),
    y_label_text = newline_format(
      paste0(en2fr("Spawning biomass", translate),
             ifelse(translate, " (1 000 t)", " (1,000 t)")),
      y_axis_label_newline_length
    ),
    show_x_axis = show_x_axis,
    show_y_axis = show_y_axis,
    x_axis_label_size = x_axis_label_size,
    x_axis_tick_label_size = x_axis_tick_label_size,
    y_axis_label_size = y_axis_label_size,
    y_axis_tick_label_size = y_axis_tick_label_size
  )
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
                                  point_size = 1,
                                  line_size = 0.5,
                                  errorbar_size = 0.5,
                                  zeroline_size = 0.5,
                                  zeroline_type = "dashed",
                                  xlim = NA,
                                  show_x_axis = TRUE,
                                  show_y_axis = TRUE,
                                  x_axis_label_size = 8,
                                  x_axis_tick_label_size = 8,
                                  y_axis_label_size = 8,
                                  y_axis_tick_label_size = 8,
                                  x_axis_label_newline_length = 50,
                                  y_axis_label_newline_length = 40,
                                  annot = "e",
                                  translate = FALSE) {
  recdev <- model$mcmccalcs$recr.devs.quants %>%
    t() %>%
    as_tibble(rownames = "year") %>%
    mutate(year = as.numeric(year))
  names(recdev) <- c("year", "lower", "median", "upper")

  recdev <- recdev %>%
    mutate(runmean = rollmean(
      x = median,
      k = run_mean_yrs,
      align = "right",
      na.pad = TRUE
    ))

  g <- ggplot(recdev, aes(x = year, y = median)) +
    geom_hline(
      yintercept = 0,
      size = zeroline_size,
      linetype = zeroline_type
    ) +
    geom_point(
      size = point_size,
      na.rm = TRUE
    ) +
    geom_line(size = 0.5, colour = "darkgrey") +
    geom_errorbar(aes(ymin = lower, ymax = upper),
      size = errorbar_size,
      width = 0
    ) +
    # geom_line(aes(y = runmean),
    #   color = "red",
    # size = line_size) +
    scale_x_continuous(breaks = seq(from = 1900, to = 2100, by = 10))
  if (!is.na(xlim[1])) {
    g <- g +
      coord_cartesian(xlim = xlim, expand = TRUE)
  }
  if (!is.na(annot)) {
    g <- g +
      annotate(
        geom = "text",
        x = -Inf,
        y = Inf,
        label = paste0("(", annot, ")"),
        vjust = 1.3,
        hjust = -0.1,
        size = 3
      )
  }
  g <- modify_axes_labels(g,
    x_label_text = newline_format(
      en2fr("Year", translate),
      x_axis_label_newline_length
    ),
    y_label_text = newline_format(
      en2fr("Log recruitment deviations", translate),
      y_axis_label_newline_length
    ),
    show_x_axis = show_x_axis,
    show_y_axis = show_y_axis,
    x_axis_label_size = x_axis_label_size,
    x_axis_tick_label_size = x_axis_tick_label_size,
    y_axis_label_size = y_axis_label_size,
    y_axis_tick_label_size = y_axis_tick_label_size
  )
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
                               point_size = 2.5,
                               line_size = 0.5,
                               path_line_size = 0.4,
                               text_size = 2,
                               zeroline_size = 0.5,
                               zeroline_type = "dashed",
                               lrp_ribbon_alpha = 0.35,
                               refpt_show = "0.3sbo",
                               show_x_axis = TRUE,
                               show_y_axis = TRUE,
                               x_axis_label_size = 8,
                               x_axis_tick_label_size = 8,
                               y_axis_label_size = 8,
                               y_axis_tick_label_size = 8,
                               x_axis_label_newline_length = 50,
                               y_axis_label_newline_length = 40,
                               annot = "f",
                               translate = FALSE) {
  stopifnot(
    !is.na(new_surv_yr),
    is.numeric(new_surv_yr),
    length(new_surv_yr) == 1
  )
  if (length(unique(catch_df$region)) > 1) {
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
    mutate(
      catch = ifelse(is.na(catch), 0, catch),
      mediannext = lead(median),
      catchnext = lead(catch),
      production = mediannext - median + catchnext,
      prodrate = production / median
    ) %>%
    na.omit() %>%
    filter(year >= new_surv_yr)

  dd <- dd[-nrow(dd), ] %>%
    mutate(shp = factor(0))

  lrp <- model$mcmccalcs$r.quants
  lrp <- lrp[, -1] %>%
    as_tibble(rownames = "refpt") %>%
    filter(refpt == refpt_show)
  names(lrp) <- c("year", "lower", "median", "upper")
  lrp[1, 1] <- as.character(min(sbt$year) - 2)
  lrp[, 1] <- as.numeric(lrp[, 1])
  # # Add 0.38SB_0 (for HG)
  # lrp2 <- lrp %>%
  #   mutate( lower=lower/0.3*0.38,
  #           median=median/0.3*0.38,
  #           upper=upper/0.3*0.38 )

  g <- ggplot(dd, aes(
    x = median,
    y = production
  )) +
    geom_hline(
      yintercept = 0,
      size = zeroline_size,
      linetype = zeroline_type
    ) +
    geom_vline(
      xintercept = lrp$median,
      color = "red",
      size = line_size
    ) +
    geom_rect(
      data = lrp, aes(
        xmin = lrp$lower,
        xmax = lrp$upper,
        ymin = -Inf,
        ymax = Inf
      ),
      alpha = lrp_ribbon_alpha,
      fill = "red",
      inherit.aes = FALSE
    ) +
    # geom_vline(xintercept = lrp2$median,
    #            color = "blue",
    #            size = line_size) +
    # geom_rect(data = lrp2, aes(xmin = lrp2$lower,
    #                           xmax = lrp2$upper,
    #                           ymin = -Inf,
    #                           ymax = Inf),
    #           alpha = lrp_ribbon_alpha,
    #           fill = "blue",
    #           inherit.aes = FALSE) +
    geom_point(
      data = filter(dd, year != max(year)),
      aes(
        color = year,
        shape = shp
      ),
      size = point_size,
      na.rm = TRUE
    ) +
    geom_point(
      data = filter(dd, year == max(year)),
      shape = 24,
      color = "black",
      fill = "white",
      size = point_size,
      na.rm = TRUE
    ) +
    scale_color_gradient(low = "lightgrey", high = "black") +
    geom_path(
      size = path_line_size,
      na.rm = TRUE
    ) +
    geom_text_repel(aes(label = year),
      segment.colour = "lightgrey",
      size = text_size
    ) +
    guides(color = FALSE, shape = FALSE) +
    expand_limits(x = 0)
  if (!is.na(annot)) {
    g <- g +
      annotate(
        geom = "text",
        x = -Inf,
        y = Inf,
        label = paste0("(", annot, ")"),
        vjust = 1.3,
        hjust = -0.1,
        size = 3
      )
  }
  g <- modify_axes_labels(g,
    x_label_text = newline_format(
      paste0(en2fr("Spawning biomass", translate),
             ifelse(translate, " (1 000 t)", " (1,000 t)")),
      x_axis_label_newline_length
    ),
    y_label_text = newline_format(
      paste0(en2fr("Spawning biomass production", translate),
             ifelse(translate, " (1 000 t)", " (1,000 t)")),
      y_axis_label_newline_length
    ),
    show_x_axis = show_x_axis,
    show_y_axis = show_y_axis,
    x_axis_label_size = x_axis_label_size,
    x_axis_tick_label_size = x_axis_tick_label_size,
    y_axis_label_size = y_axis_label_size,
    y_axis_tick_label_size = y_axis_tick_label_size
  )
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
                     panel.text.size = 3) {
  label <- paste0(region, "\n", gsub("_", "\n", mp))
  tac <- sapply(hcr.lst, "[[", 1)
  hr <- sapply(hcr.lst, "[[", 2)
  sbt <- sapply(sbt.lst, "[[", length(sbt.lst[[1]]))
  df <- tibble(
    tac = tac,
    hr = hr,
    sbt = sbt
  )
  if (all(is.na(df$hr)) || all(is.na(df$tac))) {
    df <- data.frame()
    g <- ggplot(df) +
      geom_point() +
      annotate("text", x = 100, y = 20, label = "No data", size = panel.text.size) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      xlab("") +
      ylab("")
    h <- ggplot(df) +
      geom_point() +
      annotate("text", x = 100, y = 20, label = "No data", size = panel.text.size) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      xlab("") +
      ylab("")
    i <- ggplot(df) +
      geom_point() +
      annotate("text", x = 100, y = 20, label = "No data", size = panel.text.size) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      xlab("") +
      ylab("")
  } else {
    g <- ggplot(df, aes(x = tac, y = hr)) +
      geom_point(
        size = point.size,
        na.rm = TRUE
      ) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = axis.text.size)
      ) +
      ylab("") +
      xlab("TAC")

    h <- ggplot(df, aes(x = sbt, y = tac)) +
      geom_point(
        size = point.size,
        na.rm = TRUE
      ) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = axis.text.size)
      ) +
      xlab("") +
      ylab("TAC")

    i <- ggplot(df, aes(x = sbt, y = hr)) +
      geom_point(
        size = point.size,
        na.rm = TRUE
      ) +
      theme(axis.title = element_text(size = axis.text.size)) +
      xlab("Projected SBt") +
      ylab("Harvest Rate")

    if (show.medians) {
      quants.tac <- as_tibble(t(as.data.frame(quantile(tac, probs = probs, na.rm = TRUE)))) %>%
        rename(lower = 1, upper = 2)
      quants.hr <- as_tibble(t(as.data.frame(quantile(hr, probs = probs, na.rm = TRUE)))) %>%
        rename(lower = 1, upper = 2)
      quants.sbt <- as_tibble(t(as.data.frame(quantile(sbt, probs = probs)))) %>%
        rename(lower = 1, upper = 2)
      g <- g +
        geom_vline(
          xintercept = median(tac),
          color = "red",
          size = line.width,
          linetype = "dashed"
        ) +
        geom_rect(
          data = quants.hr,
          aes(
            ymin = quants.hr$lower,
            ymax = quants.hr$upper,
            xmin = -Inf,
            xmax = Inf
          ),
          inherit.aes = FALSE,
          alpha = 0.2,
          color = "transparent",
          fill = "red"
        ) +
        geom_rect(
          data = quants.tac,
          aes(
            xmin = quants.tac$lower,
            xmax = quants.tac$upper,
            ymin = -Inf,
            ymax = Inf
          ),
          inherit.aes = FALSE,
          alpha = 0.2,
          color = "transparent",
          fill = "red"
        ) +
        geom_hline(
          yintercept = median(hr),
          color = "red",
          size = line.width,
          linetype = "dashed"
        )
      h <- h +
        geom_hline(
          yintercept = median(tac),
          color = "red",
          size = line.width,
          linetype = "dashed"
        ) +
        geom_vline(
          xintercept = median(sbt),
          color = "red",
          size = line.width,
          linetype = "dashed"
        ) +
        geom_rect(
          data = quants.sbt,
          aes(
            xmin = quants.sbt$lower,
            xmax = quants.sbt$upper,
            ymin = -Inf,
            ymax = Inf
          ),
          inherit.aes = FALSE,
          alpha = 0.2,
          color = "transparent",
          fill = "red"
        ) +
        geom_rect(
          data = quants.tac,
          aes(
            ymin = quants.tac$lower,
            ymax = quants.tac$upper,
            xmin = -Inf,
            xmax = Inf
          ),
          inherit.aes = FALSE,
          alpha = 0.2,
          color = "transparent",
          fill = "red"
        )
      i <- i +
        geom_hline(
          yintercept = median(hr),
          color = "red",
          size = line.width,
          linetype = "dashed"
        ) +
        geom_vline(
          xintercept = median(sbt),
          color = "red",
          size = line.width,
          linetype = "dashed"
        ) +
        geom_rect(
          data = quants.sbt,
          aes(
            xmin = quants.sbt$lower,
            xmax = quants.sbt$upper,
            ymin = -Inf,
            ymax = Inf
          ),
          inherit.aes = FALSE,
          alpha = 0.2,
          color = "transparent",
          fill = "red"
        ) +
        geom_rect(
          data = quants.hr,
          aes(
            ymin = quants.hr$lower,
            ymax = quants.hr$upper,
            xmin = -Inf,
            xmax = Inf
          ),
          inherit.aes = FALSE,
          alpha = 0.2,
          color = "transparent",
          fill = "red"
        )
    }
    if (show.means) {
      g <- g +
        geom_vline(
          xintercept = mean(tac),
          color = "green",
          size = line.width,
          linetype = "dashed"
        ) +
        geom_hline(
          yintercept = mean(hr),
          color = "green",
          size = line.width,
          linetype = "dashed"
        )
      h <- h +
        geom_hline(
          yintercept = mean(tac),
          color = "green",
          size = line.width,
          linetype = "dashed"
        )
      i <- i +
        geom_hline(
          yintercept = mean(hr),
          color = "green",
          size = line.width,
          linetype = "dashed"
        )
    }
  }
  if (!show.x.axes) {
    i <- i +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      xlab("")
    g <- g +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
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
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    xlab("") +
    ylab("") +
    theme(plot.margin = ff)

  plot_grid(h,
    j,
    i,
    g,
    nrow = 2,
    ncol = 2,
    align = "hv",
    axis = "tblr"
  )
}

#' Plot Beverton-holt
#'
#' @param models iscam models list
#' @param regions the regions to include
#' @param translate Logical; translate labels to french?
#'
#' @export
#' @importFrom tibble enframe
#' @importFrom scales comma
plot_bh <- function(models,
                    regions,
                    translate = FALSE) {
  # Note sbt goes from start year to end year + 1
  # rt goes from start year + start age to end year

  # lapply !!!
  model <- models

  sbtDat <- model$mcmccalcs$sbt.quants
  sbt <- tibble(
    sbt = sbtDat["50%", ],
    year = as.numeric(colnames(sbtDat))
  )

  rtDat <- model$mcmccalcs$recr.quants
  rt <- tibble(
    rt = rtDat["50%", ],
    year = as.numeric(colnames(rtDat))
  )

  d <- sbt %>%
    full_join(rt, by = "year") %>%
    na.omit()

  x <- model$mcmccalcs$p.quants
  h <- x["50%", "h"]
  r0 <- x["50%", "ro"]
  sb0 <- x["50%", "sbo"]
  alpha <- 4 * h * r0 / (sb0 * (1 - h))
  beta <- (5 * h - 1) / (sb0 * (1 - h))

  d0 <- tibble(sbt = sb0, rt = r0)

  p <- tibble(
    sbt = seq(from = 0, to = max(c(d$sbt, d0$sbt)), length.out = 100),
    rt = alpha * sbt / (1 + beta * sbt)
  )

  g <- ggplot(d, aes(x = sbt, y = rt)) +
    labs(
      x = paste(en2fr("Spawning biomass", translate),
                ifelse(translate, " (1 000 t)", " (1,000 t)")),
      y = paste(en2fr("Recruitment", translate),
                ifelse(translate, " (1 000 millions)", " (1,000 millions)"))
    ) +
    geom_line(data = p) +
    geom_point(
      data = filter(d, year != max(year)), aes(color = year),
      na.rm = TRUE
    ) +
    geom_point(
      data = filter(d, year == max(year)), na.rm = TRUE, shape = 24,
      color = "black", fill = "white"
    ) +
    geom_point(data = d0, shape = 8) +
    guides(color = FALSE, shape = FALSE) +
    scale_color_gradient(low = "lightgrey", high = "black") +
    scale_y_continuous(labels = function(x) x / 1000)
  g
}

#' Plot spawn index or scaled abundance by section
#'
#' @param model iscam model object (can be NA if scale is FALSE).
#' @param data_file Chracter. Path and name of data file (CSV) with spawn by
#' section and year.
#' @param sections Vector of sections to include, or NA to include all.
#' @param yr_range Start and end year, or NA to include all the data.
#' @param scale Logical. Scale the spawn index using q?
#' @param translate Logical. Translate to French?
#'
#' @export
#' @importFrom tibble tibble tribble
#' @importFrom readr read_csv
#' @importFrom dplyr mutate left_join filter
#' @importFrom ggplot2 ggplot geom_point geom_line labs scale_shape_manual
#' scale_x_continuous scale_y_continuous facet_wrap theme
#' @importFrom rosettafish en2fr
plot_spawn_section <- function(model,
                               data_file,
                               sections = NA,
                               yr_range = NA,
                               scale = TRUE,
                               ncol = 2,
                               translate = FALSE){
  yrBreaks <- seq(from = 1950, to = 2030, by = 10)
  if( !is.na(model) & scale ){
    qVals <- tibble(Survey = c("Surface", "Dive"),
                    q = model$mcmccalcs$q.quants[2,])
  }
  dat <- read_csv(file = data_file, col_types=cols())
  if(scale){
    dat <- dat %>%
      left_join( y=qVals, by="Survey") %>%
      mutate(Index = Index / q)
  }
  if( !is.na(sections) ) {
    dat <- dat %>%
      mutate(Section = as.numeric(Section)) %>%
      filter(Section %in% sections)
  }
  dat <- dat %>%
    mutate(Survey = factor(Survey, levels = c("Surface", "Dive")),
           Section = formatC(Section, width = 3, format = "d", flag = "0"))
  if(!is.na(yr_range)) {
    dat <- dat %>%
      filter(Year %in% c(min(yr_range):max(yr_range)))
  }

  # Custom Section names (i.e., local names)
  variable_names <- tribble(
    ~Section, ~Name,
    "067", "067 Kitasu Bay",
    "072", "072 Lower Spiller",
    "074", "074 Thompson/Styker",
    "078", "078 Upper Spiller")

  variable_names <- variable_names %>%
    mutate(Name  = en2fr(Name, translate, allow_missing = TRUE))

  dat <- dat %>%
    left_join(variable_names, by = "Section") %>%
    mutate(Name = ifelse(is.na(Name), Section, Name))

  g <- ggplot(data = dat, mapping = aes(x = Year, y = Index)) +
    geom_point(mapping = aes(shape = Survey), size = 1, na.rm = TRUE) +
    geom_line(mapping = aes(group = Survey), size = 0.5) +
    geom_vline(
      xintercept = new_surv_yr - 0.5, linetype = "dashed", size = 0.25
    ) +
    labs(x = en2fr("Year", translate),
         shape = en2fr("Survey period", translate)) +
    expand_limits(y = c(0, 100)) +
    scale_shape_manual(values = c(1, 2)) +
    scale_x_continuous(breaks = yrBreaks) +
    scale_y_continuous(labels = function(x) x / 1000) +
    facet_wrap(Name ~ ., ncol = ncol, scales = "free_y") +
    theme(legend.position = "top")
  if(!is.na(yr_range)){
    g <- g +
      expand_limits(x = yr_range)
  }
  if(scale){
    g <- g +
      labs(y = paste0(en2fr("Scaled abundance", translate),
                      ifelse(translate, " (1 000 t)", " (1,000 t)")))
  } else {
    g <- g +
      labs(y = paste0(en2fr("Spawn index", translate),
                      ifelse(translate, " (1 000 t)", " (1,000 t)")))
  }
  g
}
