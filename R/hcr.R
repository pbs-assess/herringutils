#' Calculate median of the TAC and HR from the hcr.lst
#'
#' @param hcr.lst A list of length-2 vectors, each of which is the TAC and the HR in that order
#'
#' @return a length-2 vector which includes the median TAC and HR for the hcr.lst given
#' @export
get_hcr_tac_hr <- function(hcr.lst){
  c(median(sapply(hcr.lst, "[[", 1)),
    median(sapply(hcr.lst, "[[", 2)))
}

#' Return the appropriate HCR values for the given 'row' as it appears in the MP data file
#'
#' @param row A row of one of the MP tables as it appears in `pbs-assess/herringsr/data/mp-*.csv` files
#' @param bt a list of vectors of timeseries biomass values (typically many posteriors)
#' @param bo a vector of numeric values (typically many posteriors) for the initial biomass to base
#'  relative biomasses on. This must be the same length as the bt list
#'
#' @return A list of length equal to the length of the bt and bo lists, of vectors of length two,
#'  containing the catch limit (tac) and associated harvest rate (hr)
#'
#' @export
hcr <- function(bt, bo, row){
  stopifnot(length(bt) == length(bo))
  hcr_helper <- function(bt, bo, row){
    val <- c(NA, NA)
    if(all(is.na(row$esc),
           is.na(row$abs_esc),
           is.na(row$cap),
           is.na(row$lrp),
           is.na(row$usr))){
      val <- c(NA, NA)
    }else if(is.na(row$lrp) &&
             is.na(row$usr) &&
             !is.na(row$esc) &&
             !is.na(row$abs_esc)){
      if(!is.na(row$num_end_yrs)){
        val <- hcr.min.esc(bt,
                           ref.hr = row$hr,
                           min.esc = row$esc,
                           abs.esc = row$abs_esc,
                           catch.cap = row$cap,
                           bo = bo,
                           num.end.yrs = row$num_end_yrs)
      }
    }else if(!is.na(row$lrp) &&
             !is.na(row$usr) &&
             is.na(row$esc)){
      if(!is.na(row$num_end_yrs)){
        val <- hcr.ref.pts(bt,
                           ref.hr = row$hr,
                           lrp = row$lrp,
                           usr = row$usr,
                           catch.cap = row$cap,
                           bo = bo,
                           num.end.yrs = row$num_end_yrs)
      }
    }else{
      stop("The row given does not have variables set correctly. Row:\n", paste(row, collapse = ", "))
    }
    val
  }

  lapply(seq_along(bt),
         function(x){
           hcr_helper(bt = bt[[x]],
                      bo = bo[x],
                      row = row)})
}

#' Calculate TAC and TAC-based harvest rate based on a minimum escapement and reference harvest rate
#'
#' @param bt biomass vector for years
#' @param ref.hr reference harvest rate
#' @param min.esc minimum escapement to base catch limit on
#' @param abs.esc if TRUE, `min.esc` is absolute. If FALSE, it is relative
#' @param catch.cap catch cap to use if calculated catch limit is higher
#' @param bo initial biomass used in reletive biomass calculation
#' @param num.end.yrs mean of these last N years at the end of the biomass vector to use
#'
#' @return catch limit
#' @export
hcr.min.esc <- function(bt,
                        ref.hr,
                        min.esc,
                        abs.esc = FALSE,
                        catch.cap = 0,
                        bo = 1,
                        num.end.yrs = 1){
  bt <- mean(tail(bt, num.end.yrs))
  if(abs.esc){
    min.esc <- min.esc / bo
  }
  min.esc.val <- min.esc * bo

  if(bt > min.esc.val){
    hr <- min((bt - min.esc.val) / bt, ref.hr)
  }else{
    hr <- 0
  }
  tac <- hr * bt
  if(is.na(catch.cap)){
    catch.cap <- 0
  }
  if(catch.cap > 0 && tac > catch.cap){
    tac <- catch.cap
    hr <- (bt - catch.cap) / bt
  }
  c(tac, hr)
}

#' Calculate TAC and TAC-based harvest rate based on biomass reference points and reference harvest rate
#'
#' @param bt biomass vector for years
#' @param ref.hr reference harvest rate
#' @param lrp limit reference point
#' @param usr upper stock reference point
#' @param catch.cap catch cap to use if calculated catch limit is higher
#' @param bo initial biomass used in reletive biomass calculation
#' @param num.end.yrs mean of these last N years at the end of the biomass vector to use
#'
#' @return a vector of length 2 made up of the catch limit and corresponding target harvest rate
#' @export
hcr.ref.pts <- function(bt,
                        ref.hr,
                        lrp,
                        usr,
                        catch.cap = 0,
                        bo = 1,
                        num.end.yrs = 1){
  bt <- mean(tail(bt, num.end.yrs))
  if(bt >= usr * bo){
    hr <- ref.hr
  }else if(lrp * bo <= bt && bt < usr * bo){
    hr <- ref.hr * ((bt - lrp * bo) / ((usr - lrp) * bo))
  }else{
    hr <- 0
  }
  tac <- hr * bt
  if(is.na(catch.cap)){
    catch.cap <- 0
  }
  if(catch.cap > 0 && tac > catch.cap){
    tac <- catch.cap
    hr <- catch.cap / bt

  }
  c(tac, hr)
}
