#' @title Health Related Quality of Life Module (HRQoL)
#'
#' @description Module to calculate the total HRQoL accumulated from
#'              person-time spent in each health state, with a final output of
#'              total quality-adjusted life years (QALYs)
#'
#' @return This function returns [insert] object with total HRQoL accumulated
#'         from person-time spent in each health state
#'
#' @export
#'

load("EconAnalysis/EpiModel SIMS/sim.n3001.rda")

source("EconAnalysis/R/params_cea.r")


hrqol.cea <- function(p, sim){

  # HRQoL for each HIV stage

  # Uninfected

  QALY.hivneg <- HIVneg.util*sim$epi$time.on.prep

  # Acute HIV, undiagnosed

  QALY.acute.undx <- param_cea$acute.undx.util *
                     (sim$stage.time.ar.ndx + sim$epi$stage.time.af.ndx)

  QALY.acute.dx <- param_cea$acute.dx.util *
                  (sim$stage.time.ar.dx + sim$stage.time.af.dx)

  # Output

  list(c(QALY.hivneg, QALY.acute.undx, QALY.acute.dx))
}














