#' Text for "Stock status update" section.
#'
#' @param SAR Character. SAR name (e.g., "HG").
#' @param this_yr Numeric. Usually `assess_yr`.
#' @param p_lrp Numeric. Proportion of SB_0 that is the LRP (default is 0.3).
#' @param translate Logical. Translate to french if TRUE.
#'
#' @note Requires variables from the `get_vars()` function named for the SAR
#'   (i.e., `hg_vars <- get_vars("HG", french = french)`).
#'
#' @export
#' @return Text.
stock_status_text <- function(SAR,
                              this_yr,
                              p_lrp = 0.3,
                              translate) {
  vars <- get(paste0(tolower(SAR), "_vars"))
  res <- paste0(
    ifelse(
      translate,
      "La biomasse reproductrice non pêchée estimée ",
      "Estimated unfished spawning biomass "
    ),
    "$\\SB_0$ ",
    ifelse(translate, "est de ", "is "),
    f(vars$sbo[2]), "$\\,\\text{t}$, ",
    ifelse(translate, "et le PRL de ", "and the LRP of "),
    "$", p_lrp, "\\SB_0$ ",
    ifelse(translate, "est de ", "is "),
    f(p_lrp * vars$sbo[2]), "$\\,\\text{t}$ ",
    ifelse(translate, "(médianes postérieures). ", "(posterior medians). "),
    ifelse(
      translate,
      "Par rapport à l'année dernière, la biomasse reproductrice estimée en ",
      "Compared to last year, estimated spawning biomass in "
    ),
    this_yr, " $\\SB_{", this_yr, "}$ ",
    if (translate) {
      ifelse(
        vars$final_yr_sbt[2] > vars$prev_yr_sbt[2],
        "a diminué à ",
        "a augmenté à "
      )
    },
    if (!translate) {
      ifelse(
        vars$final_yr_sbt[2] > vars$prev_yr_sbt[2],
        "increased to ",
        "decreased to "
      )
    },
    f(vars$final_yr_sbt[2]), "$\\,\\text{t}$ ",
    ifelse(
      translate,
      "(médiane postérieure), ce qui correspond à ",
      "(posterior median), which is equal to "
    ),
    f(vars$final_yr_dt[2] * 100, 1), "% ",
    ifelse(translate, "de ", "of "),
    "$\\SB_0$ (",
    ifelse(translate, "tableaux ", "Tables "),
    "\\@ref(tab:", tolower(SAR), "-spawning-biomass-depletion) & ",
    "\\@ref(tab:ref-points-", tolower(SAR), ")). ",
    ifelse(
      translate,
      "On estime que la biomasse de frai en ",
      "Spawning biomass in "
    ),
    this_yr,
    ifelse(
      translate,
      " sera supérieure au PLR avec une probabilité de ",
      " is estimated to be above the LRP with a "
    ),
    f((1 - vars$prob_less_03sbo) * 100, 1), "% ",
    ifelse(
      translate,
      "(tableau ",
      "probability (Table "
    ),
    "\\@ref(tab:ref-points-", tolower(SAR), "))."
  )
  # Encoding(res) <- "UTF-8"
  res
}

#' Text for "Application of MPs.." section.
#'
#' @param SAR Character. SAR name (e.g., "HG").
#' @param next_yr Numeric. Usually `assess_yr + 1`.
#' @param p_lrp Numeric. Proportion of SB_0 that is the LRP (default is 0.3).
#' @param translate Logical. Translate to french if TRUE.
#'
#' @note Requires variables from the `get_vars()` function named for the SAR
#'   (i.e., `hg_vars <- get_vars("HG", french = french)`).
#'
#' @export
#' @return Text.
proj_biomass_text <- function(SAR,
                              next_yr,
                              p_lrp = 0.3,
                              translate) {
  vars <- get(paste0(tolower(SAR), "_vars"))
  res <- paste0(
    ifelse(
      translate,
      "En l'absence de pêche, la biomasse féconde en ",
      "In the absence of fishing, spawning biomass in "
    ),
    next_yr, " $\\SB_{", next_yr, "}$ ",
    ifelse(translate, "est prévu pour être ", "is forecast to be "),
    f(vars$proj_sbt[2]), "$\\,\\text{t}$ ",
    ifelse(
      translate,
      "(médiane postérieure; tableau ",
      "(posterior median; Table "
    ),
    "\\@ref(tab:ref-points-", tolower(SAR), ")). ",
    ifelse(
      translate,
      "On prévoit que la biomasse de frai en ",
      "Spawning biomass in "
    ),
    next_yr,
    ifelse(
      translate,
      " sera inférieure au PRL de ",
      " is forecast to be below the LRP of "
    ),
    "$", p_lrp, "\\SB_0$ (", f(p_lrp * vars$sbo[2]), "$\\,\\text{t}$) ",
    ifelse(translate, "avec une probabilité de ", "with a "),
    f(vars$prob_proj_less_03sbo * 100, 1), "% ",
    ifelse(
      translate,
      "en l'absence de pêche (tableau ",
      "probability, in the absence of fishing (Table "
    ),
    "\\@ref(tab:ref-points-", tolower(SAR),
    ifelse(translate, ") et figure ", ") and Figure "),
    "\\@ref(fig:proj-biomass-density))."
  )
  # Encoding(res) <- "UTF-8"
  res
}
