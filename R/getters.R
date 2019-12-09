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
