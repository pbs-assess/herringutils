#' Summarize catch by region and gear
#'
#' @param models list of iscam models. Must be a [gfiscamutils::model.lst.class] type
#' @param models_names vector of model names that correspond to the input models
#' @param gear gear number as it appears in iscam data file
#' @param area area number as it appears in iscam data file
#' @param group group number as it appears in iscam data file
#' @param sex sex number as it appears in iscam data file
#' @param type type number as it appears in iscam data file
#'
#' @return a tibble
#' @importFrom rosettafish en2fr
#' @importFrom dplyr bind_rows mutate left_join select filter as_tibble
#' @importFrom forcats fct_relevel
#' @importFrom gfiscamutils verify_models
#' @export
get_catch <- function(models,
                      models_names,
                      gear,
                      area = 1,
                      group = 1,
                      sex = 0,
                      type = 1){
  verify_models(models, models_names)
  dfs <- lapply(seq_along(models), function(x){
    models[[x]]$dat$catch %>%
      as_tibble() %>%
      mutate(region = models_names[x])
  })
  df <- bind_rows(dfs) %>%
    filter(area %in% area,
           group %in% group,
           sex %in% sex,
           type %in% type) %>%
    left_join(gear, by="gear") %>%
    select(-gear) %>%
    rename(gear = gearname) %>%
    mutate(region = fct_relevel(region, models_names))
  df
}

#' Summarize weight-at-age data
#'
#' @rdname get_catch
#' @return a tibble
#' @importFrom rosettafish en2fr
#' @importFrom dplyr bind_rows mutate left_join select filter as_tibble
#' @importFrom forcats fct_relevel
#' @importFrom gfiscamutils verify_models
#' @export
get_wa <- function(models,
                   models_names,
                   gear,
                   area = 1,
                   group = 1,
                   sex = 0){
  verify_models(models, models_names)
  dfs <- lapply(seq_along(models), function(x){
    models[[x]]$dat$weight.at.age %>%
      as_tibble() %>%
      mutate(region = models_names[x])
  })
  df <- bind_rows(dfs) %>%
    filter(area %in% area,
           group %in% group,
           sex %in% sex) %>%
    left_join(gear, by="gear") %>%
    select(-gear) %>%
    rename(gear = gearname) %>%
    mutate(region = fct_relevel(region, models_names))
  df
}

#' Summarize proportion-at-age data
#'
#' @rdname get_catch
#' @return a tibble
#' @importFrom rosettafish en2fr
#' @importFrom dplyr bind_rows mutate left_join select filter as_tibble
#' @importFrom forcats fct_relevel
#' @importFrom gfiscamutils verify_models
#' @export
get_pa <- function(models,
                   models_names,
                   gear,
                   area = 1,
                   group = 1,
                   sex = 0){
  verify_models(models, models_names)
  dfs <- lapply(seq_along(models), function(x){
    lst <- models[[x]]$dat$age.comps
    lst <- lapply(lst, as_tibble)
    lst <- lst %>%
      bind_rows() %>%
      mutate(region = models_names[x])
  })
  df <- bind_rows(dfs) %>%
    filter(area %in% area,
           group %in% group,
           sex %in% sex) %>%
    left_join(gear, by="gear") %>%
    select(-gear) %>%
    rename(gear = gearname) %>%
    mutate(region = fct_relevel(region, models_names))
  df
}

#' Summarize survey index data
#'
#' @rdname get_catch
#' @return a tibble
#' @importFrom rosettafish en2fr
#' @importFrom dplyr bind_rows mutate left_join select filter as_tibble
#' @importFrom forcats fct_relevel
#' @importFrom gfiscamutils verify_models
#' @export
get_surv_ind <- function(models,
                         models_names,
                         gear,
                         area = 1,
                         group = 1,
                         sex = 0){
  verify_models(models, models_names)
  dfs <- lapply(seq_along(models), function(x){
    lst <- models[[x]]$dat$indices
    lst <- lapply(lst, as_tibble)
    lst <- lst %>%
      bind_rows() %>%
      mutate(region = models_names[x])
  })
  df <- bind_rows(dfs) %>%
    filter(area %in% area,
           group %in% group,
           sex %in% sex) %>%
    left_join(gear, by="gear") %>%
    select(-gear) %>%
    rename(gear = gearname,
           year = iyr,
           value = it) %>%
    mutate(region = fct_relevel(region, models_names))
  df
}

#' Get model output values for a given region.
#'
#' @param region Character. The short form for the region as defined in the
#'   `major_regions_short` vector`.
#' @param majors Logical. If TRUE use major models list (`major_models`), if
#'   FALSE use minor models list (`minor_models`). Default is TRUE.
#' @param n_hr_yrs Integer. Number of years to calculate mean HR.
#' @param french Logical. If TRUE, use French (default is FALSE).
#'
#' @importFrom tibble as_tibble
#'
#' @export
#' @return A list of values as required for the report.
#'
#' @examples
#' hg_vars <- get_vars("HG")
get_vars <- function(region, majors = TRUE, n_hr_yrs = 10, french = FALSE) {
  if (majors) {
    model_ind <- match(en2fr(region, french), major_regions_short)
    model <- major_models[[model_ind]]
  } else {
    model_ind <- match(en2fr(region, french), minor_regions_short)
    model <- minor_models[[model_ind]]
  }
  sbt <- model$mcmccalcs$sbt.quants
  sbt_yrs <- as.numeric(colnames(sbt))
  # Previous year spawning biomass - vector length 4:
  # 1 = lower, 2 = median, 3 = upper, 4 = mpd
  prev_yr_sbt <- sbt[, sbt_yrs == (assess_yr - 1)] * 1000
  # Final year spawning biomass - vector length 4:
  # 1 = lower, 2 = median, 3 = upper, 4 = mpd
  final_yr_sbt <- sbt[, sbt_yrs == assess_yr] * 1000
  refs <- model$mcmccalcs$r.quants
  sbo <- refs[rownames(refs) == "sbo", ][2:4] * 1000
  # Probability that final year biomass is less than 0.3B0 - vector length 3:
  # 1 = lower, 2 = median, 3 = upper
  prob_less_03sbo <-
    refs[rownames(refs) == paste0("psb", assess_yr, "/0.3sbo"), ][2]
  proj <- as_tibble(model$mcmccalcs$r.quants, rownames = "value")
  # Projected biomass for next year - vector length 3:
  # 1 = lower, 2 = median, 3 = upper
  proj_sbt <-
    as.numeric(proj[proj$value == paste0("sb", assess_yr + 1), -c(1, 2)]) * 1000
  # Probability that next year (projected) biomass is less than 0.3B0 -
  # vector length 3 - 1 = lower, 2 = median, 3 = upper
  prob_proj_less_03sbo <-
    refs[rownames(refs) == paste0("psb", assess_yr + 1, "/0.3sbo"), ][2]
  # Depletion
  dt <- model$mcmccalcs$depl.quants
  dt_yrs <- as.numeric(colnames(dt))
  # Depletion in final year
  final_yr_dt <- dt[, dt_yrs == assess_yr]
  # Catch
  ct <- model$dat$catch %>%
    as_tibble() %>%
    group_by(year) %>%
    summarise(catch = sum(value))
  # Rearrange sbt
  sbt_df <- t(sbt) %>%
    as_tibble(rownames = "year") %>%
    mutate(year = as.integer(year))
  # Harvest rate
  hr <- sbt_df %>%
    full_join(y = ct, by = "year") %>%
    mutate(median = catch / (catch + `50%`)) %>%
    na.omit() %>%
    tail(n = n_hr_yrs)
  # Years with HR data
  hr_yrs = hr %>%
    pull(year)
  # Mean harvest rate
  hr_mean <- hr %>%
    pull(median) %>%
    mean()
  # List to return
  list(
    prev_yr_sbt = prev_yr_sbt,
    final_yr_sbt = final_yr_sbt,
    sbo = sbo,
    prob_less_03sbo = prob_less_03sbo,
    proj_sbt = proj_sbt,
    prob_proj_less_03sbo = prob_proj_less_03sbo,
    final_yr_dt = final_yr_dt,
    n_hr_yrs = n_hr_yrs,
    hr_yrs = hr_yrs,
    hr_mean = hr_mean
  )
}
