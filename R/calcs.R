#Calculation functions

calc_harvest_rate <- function(models,
                              models_names,
                              gear,
                              area = 1,
                              group = 1,
                              type = 1,
                              start_year = 1951,
                              end_year = this_year){
  verify_models(models, models_names)
  df_catch <- lapply(seq_along(models), function(x){
    models[[x]]$dat$catch %>%
      as_tibble() %>%
      mutate(region = models_names[x])
  })

  df_sbt <- lapply(seq_along(models), function(x){
    models[[x]]$mcmccalcs$sbt.quants %>%
      as_tibble() %>%
      mutate(region = models_names[x])
  })

  sbt_df <- t(models[[x]]$mcmccalcs$sbt.quants) %>%
    as_tibble(rownames = "year") %>%
    mutate(year = as.integer(year))

  df <- bind_rows(dfs) %>%
    filter(area %in% area,
           group %in% group,
           type %in% type) %>%
    left_join(gear, by="gear") %>%
    select(-gear) %>%
    rename(gear = gearname) %>%
    mutate(region = fct_relevel(region, models_names))
  df
}
