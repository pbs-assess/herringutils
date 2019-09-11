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
                        catch.cap,
                        bo = 1){
  bt <- bt[length(bt)]
  catch.lim <- 0
  bt <- bt / bo
  min.esc <- min.esc * bo

  if(bt <= min.esc){
    return(0)
  }

  if(bt > min.esc & bt <= min.esc/(1 - ref.hr) )
    catch.lim = bt - min.esc
  if(bt > min.esc / (1 - ref.hr) )
    catch.lim = ref.hr * bt
  if(catch.cap > 0 & catch.lim > catch.cap){
    catch.lim <- catch.cap
  }
  catch.lim
}

#' Calculate catch limit based on a minimum escapement and reference harvest rate,
#'  using the last N years for the calculation
#'
#' @param bt biomass vector for years
#' @param num.end.yrs number of years at the end of the biomass vector to use
#' @param ref.hr reference harvest rate
#' @param min.esc minimum escapement to base cath limit on
#' @param catch.cap catch cap to use if calculated catch limit is higher
#' @param bo initial biomass used in reletive biomass calculation
#'
#' @return catch limit
#' @export
hcr.min.esc.slow <- function(bt,
                             num.end.yrs = 3,
                             ref.hr,
                             min.esc,
                             catch.cap,
                             bo = 1){

  bt <- bt[(length(bt) - num.end.yrs):length(bt)]
  catch.lim <- 0
  bt <- bt / bo
  min.esc <- min.esc * bo
  n.yrs <- length(bt)
  last.bt <- bt[n.yrs]
  bt.diff <- bt - min.esc

  if(any(bt.diff <= 0)){
    return(0)
  }

  if(last.bt > min.esc & last.bt <= min.esc/(1 - ref.hr)){
    catch.lim <- last.bt - min.esc
  }
  if(last.bt > min.esc / (1 - ref.hr)){
    catch.lim <- ref.hr * last.bt
  }
  if(catch.cap > 0 & catch.lim > catch.cap){
    catch.lim <- catch.cap
  }
  catch.lim
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
#' @return catch limit
#' @export
hcr.hs <- function(bt,
                   ref.hr,
                   lrp,
                   usr,
                   catch.cap,
                   bo = 1){
  bt <- bt[length(bt)]
  targ.hr <- 0
  dep <- bt / bo

  if(dep <= lrp){
    return(0)
  }

  if(dep > lrp & dep <= usr){
    targ.hr <- (dep - lrp) * ref.hr / (usr - lrp)
  }
  if(dep > usr){
    targ.hr <- ref.hr
  }
  catch.lim <- targ.hr * bt
  if(catch.cap > 0 & catch.lim > catch.cap){
    catch.lim <- catch.cap
  }
  catch.lim
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
#' @return catch limit
#' @export
hcr.hs.slow <- function(bt,
                        num.end.yrs = 3,
                        ref.hr,
                        lrp,
                        usr,
                        catch.cap,
                        bo = 1){

  bt <- bt[(length(bt) - num.end.yrs):length(bt)]
  targ.hr <- 0
  n.yrs <- length(bt)
  last.bt <- bt[n.yrs]
  dep <- last.bt / bo
  bt.diff <- bt - lrp

  if(any(bt.diff <= 0)){
    return(0)
  }

  if(dep > lrp & dep <= usr){
    targ.hr <- (dep - lrp) * ref.hr / (usr - lrp)
  }
  if(dep > usr){
    targ.hr <- ref.hr
  }
  catch.lim <- targ.hr * last.bt
  if(catch.cap > 0 & catch.lim > catch.cap){
    catch.lim <- catch.cap
  }
  catch.lim
}
