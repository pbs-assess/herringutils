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
      if(!is.na(row$slowyrs) && row$slowyrs > 1){
        val <- hcr.min.esc.slow(bt,
                                num.end.yrs = row$slowyrs,
                                ref.hr = row$hr,
                                min.esc = row$esc,
                                abs.esc = row$abs_esc,
                                catch.cap = row$cap,
                                bo = bo)
      }else{
        val <- hcr.min.esc(bt,
                           ref.hr = row$hr,
                           min.esc = row$esc,
                           abs.esc = row$abs_esc,
                           catch.cap = row$cap,
                           bo = bo)
      }
    }else if(!is.na(row$lrp) &&
             !is.na(row$usr) &&
             is.na(row$esc)){
      if(!is.na(row$slowyrs) && row$slowyrs > 1){
        val <- hcr.hs.slow(bt,
                           num.end.yrs = row$slowyrs,
                           ref.hr = row$hr,
                           lrp = row$lrp,
                           usr = row$usr,
                           catch.cap = row$cap,
                           bo = bo)
      }else{
        val <- hcr.hs(bt,
                      ref.hr = row$hr,
                      lrp = row$lrp,
                      usr = row$usr,
                      catch.cap = row$cap,
                      bo = bo)
      }
    }else{
      stop("The row given does not have variables set correctly. Row:\n", paste(row, collapse = ", "))
    }
  }

  lapply(seq_along(bt),
         function(x){
           hcr_helper(bt = bt[[x]],
                      bo = bo[x],
                      row = row)})
}

#' Calculate catch limit based on a minimum escapement and reference harvest rate
#'
#' @param bt biomass vector for years
#' @param ref.hr reference harvest rate
#' @param min.esc minimum escapement to base catch limit on
#' @param catch.cap catch cap to use if calculated catch limit is higher
#' @param bo initial biomass used in reletive biomass calculation
#'
#' @return catch limit
#' @export
hcr.min.esc <- function(bt,
                        ref.hr,
                        min.esc,
                        abs.esc = FALSE,
                        catch.cap = 0,
                        bo = 1){
  bt <- bt[length(bt)]
  catch.lim <- 0
  dep <- bt / bo

  if(abs.esc){
    min.esc <- min.esc / bo
  }
  min.esc.val <- min.esc * bo

  if(dep <= min.esc){
    return(c(0, 0))
  }
  if(dep > min.esc && dep <= min.esc / (1 - ref.hr)){
    catch.lim <- bt - min.esc.val
  }
  if(dep > min.esc / (1 - ref.hr)){
    catch.lim <- ref.hr * bt
  }
  if(is.na(catch.cap)){
    catch.cap <- 0
  }
  if(catch.cap > 0 && catch.lim > catch.cap){
    catch.lim <- catch.cap
  }
  targ.hr <- catch.lim / (catch.lim + bt)
  c(catch.lim, targ.hr)
}

#' Calculate catch limit based on a minimum escapement and reference harvest rate,
#'  using the last N years for the calculation
#'
#' @param bt biomass vector for years
#' @param num.end.yrs number of years at the end of the biomass vector to use
#' @param ref.hr reference harvest rate
#' @param min.esc minimum escapement to base catch limit on
#' @param catch.cap catch cap to use if calculated catch limit is higher
#' @param bo initial biomass used in reletive biomass calculation
#'
#' @return catch limit
#' @export
hcr.min.esc.slow <- function(bt,
                             num.end.yrs = 3,
                             ref.hr,
                             min.esc,
                             abs.esc = FALSE,
                             catch.cap = 0,
                             bo = 1){

  bt <- bt[(length(bt) - num.end.yrs):length(bt)]
  catch.lim <- 0
  if(abs.esc){
    min.esc <- min.esc / bo
  }
  min.esc.val <- min.esc * bo

  last.bt <- bt[length(bt)]
  dep <- last.bt / bo
  bt.diff <- bt - min.esc

  if(any(bt.diff <= 0)){
    return(c(0, 0))
  }
  if(dep > min.esc && dep <= min.esc / (1 - ref.hr)){
    catch.lim <- last.bt - min.esc.val
  }
  if(dep > min.esc / (1 - ref.hr)){
    catch.lim <- ref.hr * last.bt
  }
  if(is.na(catch.cap)){
    catch.cap <- 0
  }
  if(catch.cap > 0 && catch.lim > catch.cap){
    catch.lim <- catch.cap
  }
  targ.hr <- catch.lim / (catch.lim + bt)
  c(catch.lim, targ.hr)
}

#' Calculate catch limit based on biomass reference points and reference harvest rate
#'
#' @param bt biomass vector for years
#' @param ref.hr reference harvest rate
#' @param lrp limit reference point
#' @param usr upper stock reference point
#' @param catch.cap catch cap to use if calculated catch limit is higher
#' @param bo initial biomass used in reletive biomass calculation
#'
#' @return a vector of length 2 made up of the catch limit and corresponding target harvest rate
#' @export
hcr.hs <- function(bt,
                   ref.hr,
                   lrp,
                   usr,
                   catch.cap = 0,
                   bo = 1){

  bt <- bt[length(bt)]
  targ.hr <- 0
  dep <- bt / bo

  if(dep <= lrp){
    return(c(0, 0))
  }

  if(dep > lrp && dep <= usr){
    targ.hr <- (dep - lrp) * ref.hr / (usr - lrp)
  }
  if(dep > usr){
    targ.hr <- ref.hr
  }
  catch.lim <- targ.hr * bt
  if(is.na(catch.cap)){
    catch.cap <- 0
  }
  if(catch.cap > 0 && catch.lim > catch.cap){
    catch.lim <- catch.cap
  }
  c(catch.lim, targ.hr)
}

#' Calculate catch limit based on biomass reference points and reference harvest rate,
#'  using the last N years for the calculation
#'
#' @param bt biomass vector for years
#' @param num.end.yrs number of years at the end of the biomass vector to use
#' @param ref.hr reference harvest rate
#' @param lrp limit reference point
#' @param usr upper stock reference point
#' @param catch.cap catch cap to use if calculated catch limit is higher
#' @param bo initial biomass used in reletive biomass calculation
#'
#' @return a vector of length 2 made up of the catch limit and corresponding target harvest rate
#' @export
hcr.hs.slow <- function(bt,
                        num.end.yrs = 3,
                        ref.hr,
                        lrp,
                        usr,
                        catch.cap = 0,
                        bo = 1){

  bt <- bt[(length(bt) - num.end.yrs):length(bt)]
  targ.hr <- 0
  n.yrs <- length(bt)
  last.bt <- bt[n.yrs]
  dep <- last.bt / bo
  bt.diff <- bt - lrp

  if(any(bt.diff <= 0)){
    return(c(0, 0))
  }

  if(dep > lrp && dep <= usr){
    targ.hr <- (dep - lrp) * ref.hr / (usr - lrp)
  }
  if(dep > usr){
    targ.hr <- ref.hr
  }
  catch.lim <- targ.hr * last.bt
  if(is.na(catch.cap)){
    catch.cap <- 0
  }
  if(catch.cap > 0 && catch.lim > catch.cap){
    catch.lim <- catch.cap
  }
  c(catch.lim, targ.hr)
}
