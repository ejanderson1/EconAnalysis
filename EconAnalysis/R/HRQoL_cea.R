#' @title Health Related Quality of Life Module (HRQoL)
#'
#' @description Calculates the total HRQoL accumulated from
#'              person-time spent in each relevant health state,
#'              with a final output of total quality-adjusted
#'              life years (QALYs)
#'
#' @return This function returns an object with total QALYs accumulated
#'         from person-time spent in each health state over the duration of
#'         the model
#'
#' @details
#' HRQoL is calculated by multiplying the estimated utility for a
#' particular health state by the amount of person-time spent in that health
#' state. Total QALYs are then calcultated by summing accross the HRQoL for
#' all relevant health states.
#'
#' @keywords CEA
#'
#' @export
#'

load("EconAnalysis/EpiModel SIMS/sim.n3001.rda")

source("EconAnalysis/R/params_cea.r")

# General QALY calculation - Sum states separately (one at a time)

qaly_calc <- function(utility, pt_state) {
  qaly_state <- utility * pt_state
  return(qaly_state)
}

# Calculate QALYs for each state and sum to get total QALYs

total_qaly_calc <- function(util_vec, pt_vec) {
  qaly_vec <- util_vec * pt_vec
  total_qaly <- sum(qaly_vec[1:length(qaly_vec)])
  return(total_qaly)
}


# HIV specific

HIV_qaly_em <- function(HIVneg.util,
                     acute.undx.util,
                     acute.dx.util,
                     early.chron.undx.util,
                     early.chron.dx.yr1.util,
                     early.chron.dx.postyr1.util,
                     early.chron.art.util,
                     late.chron.undx.util,
                     late.chron.dx.util,
                     late.chron.art.util,
                     aids.undx.util,
                     aids.dx.util,
                     aids.art.util,
                     pt.neg,
                     pt.acute.undx,
                     pt.acute.dx,
                     pt.early.chron.undx,
                     pt.early.chron.dx.yr1,
                     pt.early.chron.dx.postyr1,
                     pt.early.chron.art,
                     pt.late.chron.undx,
                     pt.late.chron.dx,
                     pt.late.chron.art,
                     pt.aids.undx,
                     pt.aids.dx,
                     pt.aids.art,
                     ...) {

  # Calculate QALYs for each health state

  hivneg.qaly <- HIVneg.util * pt.neg
  acute.undx.qaly <- acute.undx.util * pt.acute.undx
  acute.dx.qaly <- acute.dx.util * pt.acute.dx
  early.chron.undx.qaly <- early.chron.undx.util * pt.early.chron.undx
  early.chron.dx.yr1.qaly <- early.chron.dx.yr1.util * pt.early.chron.dx.yr1
  early.chron.dx.postyr1.qaly <- early.chron.dx.postyr1.util *
                                 pt.early.chron.dx.postyr1
  early.chron.art.qaly <- early.chron.art.util * pt.late.chron.art
  late.chron.undx.qaly <- late.chron.undx.util * pt.late.chron.undx
  late.chron.dx.qaly <- late.chron.dx.util * pt.late.chron.dx
  late.chron.art.qaly <- late.chron.art.util * pt.late.chron.art
  aids.undx.qaly <- aids.undx.util * pt.aids.undx
  aids.dx.qaly <- aids.dx.util * pt.aids.dx
  aids.art.qaly <- aids.art.util * pt.aids.art

  # Calculate total QALYs

  total.qalys <- sum(hivneg.qaly, acute.undx.qaly, acute.dx.qaly,
                     early.chron.undx.qaly, early.chron.dx.yr1.qaly,
                     early.chron.dx.postyr1.qaly, early.chron.art.qaly,
                     late.chron.undx.qaly, late.chron.dx.qaly,
                     late.chron.art.qaly, aids.undx.qaly, aids.dx.qaly,
                     aids.art.qaly)

  return(list(hivneg.qaly, acute.undx.qaly, acute.dx.qaly,
              early.chron.undx.qaly, early.chron.dx.yr1.qaly,
              early.chron.dx.postyr1.qaly, early.chron.art.qaly,
              late.chron.undx.qaly, late.chron.dx.qaly, late.chron.art.qaly,
              aids.undx.qaly, aids.dx.qaly, aids.art.qaly, total.qalys))

}






