#' @title Person-time and HIV/STI Tests Module
#'
#' @description This module calculates the total person-time spent in each HIV-
#'              related health state and uninfected time, and total number of
#'              HIV and STI tests done over a 10-year simulation
#'
#' @param sim EpiModelHIV simulation R data file (.rda)
#'
#' @return This function returns a list of the total person-time spent in each
#'         in each HIV-related health state and time spent unifnected
#'
#' @details
#' An EpiModelHIV network simulation is read in to the function. Person-time
#' is summed accross each health state (HIV-related and uninfected) in each of
#' the simulations, and the median value stored. Person-time is discounted to
#' reflect a preference for the goods and services in the present over future
#' goods and services. The output from this function can be used to caluclate
#' costs and quality-adjusted life years (QALYs) for a CEA. Assumes a 10-year
#' simulation.
#'
#' @keywords CEA
#'
#' @export
#'

# Calculate person-time

pt_cea <- function(sim, disc.rate){

  #Incidence as a placeholder for time.hivneg

  time.hivneg <- (quantile(unname(colSums(sim$epi$incid[1:52, ])), probs = 0.5) * ((1 - disc.rate)^0)) +
                 (quantile(unname(colSums(sim$epi$incid[53:104, ])), probs = 0.5) * ((1 - disc.rate)^1)) +
                 (quantile(unname(colSums(sim$epi$incid[105:156, ])), probs = 0.5) * ((1 - disc.rate)^2)) +
                 (quantile(unname(colSums(sim$epi$incid[157:208])), probs = 0.5) * ((1 - disc.rate)^3)) +
                 (quantile(unname(colSums(sim$epi$incid[209:260,])), probs = 0.5) * ((1 - disc.rate)^4)) +
                 (quantile(unname(colSums(sim$epi$incid[261:312,])), probs = 0.5) * ((1 - disc.rate)^5)) +
                 (quantile(unname(colSums(sim$epi$incid[313:364,])), probs = 0.5) * ((1 - disc.rate)^6)) +
                 (quantile(unname(colSums(sim$epi$incid[365:416,])), probs = 0.5) * ((1 - disc.rate)^7)) +
                 (quantile(unname(colSums(sim$epi$incid[417:468,])), probs = 0.5) * ((1 - disc.rate)^8)) +
                 (quantile(unname(colSums(sim$epi$incid[469:520,])), probs = 0.5) * ((1 - disc.rate)^9))

  stage.time.ar.ndx <- (quantile(unname(colSums(sim$epi$stage.time.ar.ndx[1:52, ])), probs = 0.5) * ((1 - disc.rate)^0)) +
                       (quantile(unname(colSums(sim$epi$stage.time.ar.ndx[53:104, ])), probs = 0.5) * ((1 - disc.rate)^1)) +
                       (quantile(unname(colSums(sim$epi$stage.time.ar.ndx[105:156,])), probs = 0.5) * ((1 - disc.rate)^2)) +
                       (quantile(unname(colSums(sim$epi$stage.time.ar.ndx[157:208, ])), probs = 0.5) * ((1 - disc.rate)^3)) +
                       (quantile(unname(colSums(sim$epi$stage.time.ar.ndx[209:260, ])), probs = 0.5) * ((1 - disc.rate)^4)) +
                       (quantile(unname(colSums(sim$epi$stage.time.ar.ndx[261:312, ])), probs = 0.5) * ((1 - disc.rate)^5)) +
                       (quantile(unname(colSums(sim$epi$stage.time.ar.ndx[313:364, ])), probs = 0.5) * ((1 - disc.rate)^6)) +
                       (quantile(unname(colSums(sim$epi$stage.time.ar.ndx[365:416, ])), probs = 0.5) * ((1 - disc.rate)^7)) +
                       (quantile(unname(colSums(sim$epi$stage.time.ar.ndx[417:468, ])), probs = 0.5) * ((1 - disc.rate)^8)) +
                       (quantile(unname(colSums(sim$epi$stage.time.ar.ndx[469:520, ])), probs = 0.5) * ((1 - disc.rate)^9))

  stage.time.ar.dx <- (quantile(unname(colSums(sim$epi$stage.time.ar.dx[1:52, ])), probs = 0.5) * ((1 - disc.rate)^0)) +
                      (quantile(unname(colSums(sim$epi$stage.time.ar.dx[53:104, ])), probs = 0.5) * ((1 - disc.rate)^1)) +
                      (quantile(unname(colSums(sim$epi$stage.time.ar.dx[105:156, ])), probs = 0.5) * ((1 - disc.rate)^2)) +
                      (quantile(unname(colSums(sim$epi$stage.time.ar.dx[157:208, ])), probs = 0.5) * ((1 - disc.rate)^3)) +
                      (quantile(unname(colSums(sim$epi$stage.time.ar.dx[209:260, ])), probs = 0.5) * ((1 - disc.rate)^4)) +
                      (quantile(unname(colSums(sim$epi$stage.time.ar.dx[261:312, ])), probs = 0.5) * ((1 - disc.rate)^5)) +
                      (quantile(unname(colSums(sim$epi$stage.time.ar.dx[313:364, ])), probs = 0.5) * ((1 - disc.rate)^6)) +
                      (quantile(unname(colSums(sim$epi$stage.time.ar.dx[365:416, ])), probs = 0.5) * ((1 - disc.rate)^7)) +
                      (quantile(unname(colSums(sim$epi$stage.time.ar.dx[417:468, ])), probs = 0.5) * ((1 - disc.rate)^8)) +
                      (quantile(unname(colSums(sim$epi$stage.time.ar.dx[469:520, ])), probs = 0.5) * ((1 - disc.rate)^9))

  stage.time.af.ndx <- (quantile(unname(colSums(sim$epi$stage.time.af.ndx[1:52, ])), probs = 0.5) * ((1 - disc.rate)^0)) +
                       (quantile(unname(colSums(sim$epi$stage.time.af.ndx[53:104, ])), probs = 0.5) * ((1 - disc.rate)^1)) +
                       (quantile(unname(colSums(sim$epi$stage.time.af.ndx[105:156, ])), probs = 0.5) * ((1 - disc.rate)^2)) +
                       (quantile(unname(colSums(sim$epi$stage.time.af.ndx[157:208, ])), probs = 0.5) * ((1 - disc.rate)^3)) +
                       (quantile(unname(colSums(sim$epi$stage.time.af.ndx[209:260, ])), probs = 0.5) * ((1 - disc.rate)^4)) +
                       (quantile(unname(colSums(sim$epi$stage.time.af.ndx[261:312, ])), probs = 0.5) * ((1 - disc.rate)^5)) +
                       (quantile(unname(colSums(sim$epi$stage.time.af.ndx[313:364, ])), probs = 0.5) * ((1 - disc.rate)^6)) +
                       (quantile(unname(colSums(sim$epi$stage.time.af.ndx[365:416, ])), probs = 0.5) * ((1 - disc.rate)^7)) +
                       (quantile(unname(colSums(sim$epi$stage.time.af.ndx[417:468, ])), probs = 0.5) * ((1 - disc.rate)^8)) +
                       (quantile(unname(colSums(sim$epi$stage.time.af.ndx[469:520, ])), probs = 0.5) * ((1 - disc.rate)^9))

  stage.time.af.dx <- (quantile(unname(colSums(sim$epi$stage.time.af.dx[1:52, ])), probs = 0.5) * ((1 - disc.rate)^0)) +
                      (quantile(unname(colSums(sim$epi$stage.time.af.dx[53:104, ])), probs = 0.5) * ((1 - disc.rate)^1)) +
                      (quantile(unname(colSums(sim$epi$stage.time.af.dx[105:156, ])), probs = 0.5) * ((1 - disc.rate)^2)) +
                      (quantile(unname(colSums(sim$epi$stage.time.af.dx[157:208, ])), probs = 0.5) * ((1 - disc.rate)^3)) +
                      (quantile(unname(colSums(sim$epi$stage.time.af.dx[209:260, ])), probs = 0.5) * ((1 - disc.rate)^4)) +
                      (quantile(unname(colSums(sim$epi$stage.time.af.dx[261:312, ])), probs = 0.5) * ((1 - disc.rate)^5)) +
                      (quantile(unname(colSums(sim$epi$stage.time.af.dx[313:364, ])), probs = 0.5) * ((1 - disc.rate)^6)) +
                      (quantile(unname(colSums(sim$epi$stage.time.af.dx[365:416, ])), probs = 0.5) * ((1 - disc.rate)^7)) +
                      (quantile(unname(colSums(sim$epi$stage.time.af.dx[417:468, ])), probs = 0.5) * ((1 - disc.rate)^8)) +
                      (quantile(unname(colSums(sim$epi$stage.time.af.dx[469:520, ])), probs = 0.5) * ((1 - disc.rate)^9))

  stage.time.early.chronic.ndx <- (quantile(unname(colSums(sim$epi$stage.time.early.chronic.ndx[1:52, ])), probs = 0.5) * ((1 - disc.rate)^0)) +
                                  (quantile(unname(colSums(sim$epi$stage.time.early.chronic.ndx[53:104, ])), probs = 0.5) * ((1 - disc.rate)^1)) +
                                  (quantile(unname(colSums(sim$epi$stage.time.early.chronic.ndx[105:156, ])), probs = 0.5) * ((1 - disc.rate)^2)) +
                                  (quantile(unname(colSums(sim$epi$stage.time.early.chronic.ndx[157:208, ])), probs = 0.5) * ((1 - disc.rate)^3)) +
                                  (quantile(unname(colSums(sim$epi$stage.time.early.chronic.ndx[261:312, ])), probs = 0.5) * ((1 - disc.rate)^5)) +
                                  (quantile(unname(colSums(sim$epi$stage.time.early.chronic.ndx[313:364, ])), probs = 0.5) * ((1 - disc.rate)^6)) +
                                  (quantile(unname(colSums(sim$epi$stage.time.early.chronic.ndx[365:416, ])), probs = 0.5) * ((1 - disc.rate)^7)) +
                                  (quantile(unname(colSums(sim$epi$stage.time.early.chronic.ndx[417:468, ])), probs = 0.5) * ((1 - disc.rate)^8)) +
                                  (quantile(unname(colSums(sim$epi$stage.time.early.chronic.ndx[469:520, ])), probs = 0.5) * ((1 - disc.rate)^9))

  stage.time.early.chronic.dx.yrone <- (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrone[1:52, ])), probs = 0.5) * ((1 - disc.rate)^0)) +
                                       (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrone[53:104, ])), probs = 0.5) * ((1 - disc.rate)^1)) +
                                       (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrone[105:156, ])), probs = 0.5) * ((1 - disc.rate)^2)) +
                                       (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrone[157:208, ])), probs = 0.5) * ((1 - disc.rate)^3)) +
                                       (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrone[209:260, ])), probs = 0.5) * ((1 - disc.rate)^4)) +
                                       (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrone[261:312, ])), probs = 0.5) * ((1 - disc.rate)^5)) +
                                       (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrone[313:364, ])), probs = 0.5) * ((1 - disc.rate)^6)) +
                                       (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrone[365:416, ])), probs = 0.5) * ((1 - disc.rate)^7)) +
                                       (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrone[417:468, ])), probs = 0.5) * ((1 - disc.rate)^8)) +
                                       (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrone[469:520, ])), probs = 0.5) * ((1 - disc.rate)^9))

  stage.time.early.chronic.dx.yrstwotolate <- (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrstwotolate[1:52, ])), probs = 0.5) * ((1 - disc.rate)^0)) +
                                              (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrstwotolate[53:104, ])), probs = 0.5) * ((1 - disc.rate)^1)) +
                                              (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrstwotolate[105:156,])), probs = 0.5) * ((1 - disc.rate)^2)) +
                                              (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrstwotolate[157:208, ])), probs = 0.5) * ((1 - disc.rate)^3)) +
                                              (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrstwotolate[209:260, ])), probs = 0.5) * ((1 - disc.rate)^4)) +
                                              (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrstwotolate[261:312, ])), probs = 0.5) * ((1 - disc.rate)^5)) +
                                              (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrstwotolate[313:364, ])), probs = 0.5) * ((1 - disc.rate)^6)) +
                                              (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrstwotolate[365:416, ])), probs = 0.5) * ((1 - disc.rate)^7)) +
                                              (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrstwotolate[417:468, ])), probs = 0.5) * ((1 - disc.rate)^8)) +
                                              (quantile(unname(colSums(sim$epi$stage.time.early.chronic.dx.yrstwotolate[469:520, ])), probs = 0.5) * ((1 - disc.rate)^9))

  stage.time.early.chronic.art <- (quantile(unname(colSums(sim$epi$stage.time.early.chronic.art[1:52, ])), probs = 0.5) * ((1 - disc.rate)^0)) +
                                  (quantile(unname(colSums(sim$epi$stage.time.early.chronic.art[53:104, ])), probs = 0.5) * ((1 - disc.rate)^1)) +
                                  (quantile(unname(colSums(sim$epi$stage.time.early.chronic.art[105:156, ])), probs = 0.5) * ((1 - disc.rate)^2)) +
                                  (quantile(unname(colSums(sim$epi$stage.time.early.chronic.art[157:208, ])), probs = 0.5) * ((1 - disc.rate)^3)) +
                                  (quantile(unname(colSums(sim$epi$stage.time.early.chronic.art[209:260, ])), probs = 0.5) * ((1 - disc.rate)^4)) +
                                  (quantile(unname(colSums(sim$epi$stage.time.early.chronic.art[261:312, ])), probs = 0.5) * ((1 - disc.rate)^5)) +
                                  (quantile(unname(colSums(sim$epi$stage.time.early.chronic.art[313:364, ])), probs = 0.5) * ((1 - disc.rate)^6)) +
                                  (quantile(unname(colSums(sim$epi$stage.time.early.chronic.art[365:416, ])), probs = 0.5) * ((1 - disc.rate)^7)) +
                                  (quantile(unname(colSums(sim$epi$stage.time.early.chronic.art[417:468, ])), probs = 0.5) * ((1 - disc.rate)^8)) +
                                  (quantile(unname(colSums(sim$epi$stage.time.early.chronic.art[469:520, ])), probs = 0.5) * ((1 - disc.rate)^9))

  stage.time.late.chronic.ndx <- (quantile(unname(colSums(sim$epi$stage.time.late.chronic.ndx[1:52, ])), probs = 0.5) * ((1 - disc.rate)^0)) +
                                 (quantile(unname(colSums(sim$epi$stage.time.late.chronic.ndx[53:104,])), probs = 0.5) * ((1 - disc.rate)^1)) +
                                 (quantile(unname(colSums(sim$epi$stage.time.late.chronic.ndx[105:156,])), probs = 0.5) * ((1 - disc.rate)^2)) +
                                 (quantile(unname(colSums(sim$epi$stage.time.late.chronic.ndx[157:208])), probs = 0.5) * ((1 - disc.rate)^3)) +
                                 (quantile(unname(colSums(sim$epi$stage.time.late.chronic.ndx[209:260,])), probs = 0.5) * ((1 - disc.rate)^4)) +
                                 (quantile(unname(colSums(sim$epi$stage.time.late.chronic.ndx[261:312,])), probs = 0.5) * ((1 - disc.rate)^5)) +
                                 (quantile(unname(colSums(sim$epi$stage.time.late.chronic.ndx[313:364,])), probs = 0.5) * ((1 - disc.rate)^6)) +
                                 (quantile(unname(colSums(sim$epi$stage.time.late.chronic.ndx[365:416,])), probs = 0.5) * ((1 - disc.rate)^7)) +
                                 (quantile(unname(colSums(sim$epi$stage.time.late.chronic.ndx[417:468,])), probs = 0.5) * ((1 - disc.rate)^8)) +
                                 (quantile(unname(colSums(sim$epi$stage.time.late.chronic.ndx[469:520,])), probs = 0.5) * ((1 - disc.rate)^9))

  stage.time.late.chronic.dx <- (quantile(unname(colSums(sim$epi$stage.time.late.chronic.dx[1:52, ])), probs = 0.5) * ((1 - disc.rate)^0)) +
                                (quantile(unname(colSums(sim$epi$stage.time.late.chronic.dx[53:104, ])), probs = 0.5) * ((1 - disc.rate)^1)) +
                                (quantile(unname(colSums(sim$epi$stage.time.late.chronic.dx[105:156, ])), probs = 0.5) * ((1 - disc.rate)^2)) +
                                (quantile(unname(colSums(sim$epi$stage.time.late.chronic.dx[157:208, ])), probs = 0.5) * ((1 - disc.rate)^3)) +
                                (quantile(unname(colSums(sim$epi$stage.time.late.chronic.dx[209:260, ])), probs = 0.5) * ((1 - disc.rate)^4)) +
                                (quantile(unname(colSums(sim$epi$stage.time.late.chronic.dx[261:312, ])), probs = 0.5) * ((1 - disc.rate)^5)) +
                                (quantile(unname(colSums(sim$epi$stage.time.late.chronic.dx[313:364, ])), probs = 0.5) * ((1 - disc.rate)^6)) +
                                (quantile(unname(colSums(sim$epi$stage.time.late.chronic.dx[365:416, ])), probs = 0.5) * ((1 - disc.rate)^7)) +
                                (quantile(unname(colSums(sim$epi$stage.time.late.chronic.dx[417:468, ])), probs = 0.5) * ((1 - disc.rate)^8)) +
                                (quantile(unname(colSums(sim$epi$stage.time.late.chronic.dx[469:520, ])), probs = 0.5) * ((1 - disc.rate)^9))

  stage.time.late.chronic.art <- (quantile(unname(colSums(sim$epi$stage.time.late.chronic.art[1:52, ])), probs = 0.5) * ((1 - disc.rate)^0)) +
                                 (quantile(unname(colSums(sim$epi$stage.time.late.chronic.art[53:104, ])), probs = 0.5) * ((1 - disc.rate)^1)) +
                                 (quantile(unname(colSums(sim$epi$stage.time.late.chronic.art[105:156, ])), probs = 0.5) * ((1 - disc.rate)^2)) +
                                 (quantile(unname(colSums(sim$epi$stage.time.late.chronic.art[157:208, ])), probs = 0.5) * ((1 - disc.rate)^3)) +
                                 (quantile(unname(colSums(sim$epi$stage.time.late.chronic.art[209:260, ])), probs = 0.5) * ((1 - disc.rate)^4)) +
                                 (quantile(unname(colSums(sim$epi$stage.time.late.chronic.art[261:312, ])), probs = 0.5) * ((1 - disc.rate)^5)) +
                                 (quantile(unname(colSums(sim$epi$stage.time.late.chronic.art[313:364, ])), probs = 0.5) * ((1 - disc.rate)^6)) +
                                 (quantile(unname(colSums(sim$epi$stage.time.late.chronic.art[365:416, ])), probs = 0.5) * ((1 - disc.rate)^7)) +
                                 (quantile(unname(colSums(sim$epi$stage.time.late.chronic.art[417:468, ])), probs = 0.5) * ((1 - disc.rate)^8)) +
                                 (quantile(unname(colSums(sim$epi$stage.time.late.chronic.art[469:520, ])), probs = 0.5) * ((1 - disc.rate)^9))

  stage.time.aids.ndx <- (quantile(unname(colSums(sim$epi$stage.time.aids.ndx[1:52, ])), probs = 0.5) * ((1 - disc.rate)^0)) +
                         (quantile(unname(colSums(sim$epi$stage.time.aids.ndx[53:104, ])), probs = 0.5) * ((1 - disc.rate)^1)) +
                         (quantile(unname(colSums(sim$epi$stage.time.aids.ndx[105:156, ])), probs = 0.5) * ((1 - disc.rate)^2)) +
                         (quantile(unname(colSums(sim$epi$stage.time.aids.ndx[157:208, ])), probs = 0.5) * ((1 - disc.rate)^3)) +
                         (quantile(unname(colSums(sim$epi$stage.time.aids.ndx[209:260, ])), probs = 0.5) * ((1 - disc.rate)^4)) +
                         (quantile(unname(colSums(sim$epi$stage.time.aids.ndx[261:312, ])), probs = 0.5) * ((1 - disc.rate)^5)) +
                         (quantile(unname(colSums(sim$epi$stage.time.aids.ndx[313:364, ])), probs = 0.5) * ((1 - disc.rate)^6)) +
                         (quantile(unname(colSums(sim$epi$stage.time.aids.ndx[365:416, ])), probs = 0.5) * ((1 - disc.rate)^7)) +
                         (quantile(unname(colSums(sim$epi$stage.time.aids.ndx[417:468, ])), probs = 0.5) * ((1 - disc.rate)^8)) +
                         (quantile(unname(colSums(sim$epi$stage.time.aids.ndx[469:520, ])), probs = 0.5) * ((1 - disc.rate)^9))

  stage.time.aids.dx <- (quantile(unname(colSums(sim$epi$stage.time.aids.dx[1:52, ])), probs = 0.5) * ((1 - disc.rate)^0)) +
                        (quantile(unname(colSums(sim$epi$stage.time.aids.dx[53:104, ])), probs = 0.5) * ((1 - disc.rate)^1)) +
                        (quantile(unname(colSums(sim$epi$stage.time.aids.dx[105:156, ])), probs = 0.5) * ((1 - disc.rate)^2)) +
                        (quantile(unname(colSums(sim$epi$stage.time.aids.dx[157:208, ])), probs = 0.5) * ((1 - disc.rate)^3)) +
                        (quantile(unname(colSums(sim$epi$stage.time.aids.dx[209:260, ])), probs = 0.5) * ((1 - disc.rate)^4)) +
                        (quantile(unname(colSums(sim$epi$stage.time.aids.dx[261:312, ])), probs = 0.5) * ((1 - disc.rate)^5)) +
                        (quantile(unname(colSums(sim$epi$stage.time.aids.dx[313:364, ])), probs = 0.5) * ((1 - disc.rate)^6)) +
                        (quantile(unname(colSums(sim$epi$stage.time.aids.dx[365:416, ])), probs = 0.5) * ((1 - disc.rate)^7)) +
                        (quantile(unname(colSums(sim$epi$stage.time.aids.dx[417:468, ])), probs = 0.5) * ((1 - disc.rate)^8)) +
                        (quantile(unname(colSums(sim$epi$stage.time.aids.dx[469:520, ])), probs = 0.5) * ((1 - disc.rate)^9))

  stage.time.aids.art <- (quantile(unname(colSums(sim$epi$stage.time.aids.art[1:52, ])), probs = 0.5) * ((1 - disc.rate)^0)) +
                         (quantile(unname(colSums(sim$epi$stage.time.aids.art[53:104, ])), probs = 0.5) * ((1 - disc.rate)^1)) +
                         (quantile(unname(colSums(sim$epi$stage.time.aids.art[105:156, ])), probs = 0.5) * ((1 - disc.rate)^2)) +
                         (quantile(unname(colSums(sim$epi$stage.time.aids.art[157:208, ])), probs = 0.5) * ((1 - disc.rate)^3)) +
                         (quantile(unname(colSums(sim$epi$stage.time.aids.art[209:260, ])), probs = 0.5) * ((1 - disc.rate)^4)) +
                         (quantile(unname(colSums(sim$epi$stage.time.aids.art[261:312, ])), probs = 0.5) * ((1 - disc.rate)^5)) +
                         (quantile(unname(colSums(sim$epi$stage.time.aids.art[313:364, ])), probs = 0.5) * ((1 - disc.rate)^6)) +
                         (quantile(unname(colSums(sim$epi$stage.time.aids.art[365:416, ])), probs = 0.5) * ((1 - disc.rate)^7)) +
                         (quantile(unname(colSums(sim$epi$stage.time.aids.art[417:468, ])), probs = 0.5) * ((1 - disc.rate)^8)) +
                         (quantile(unname(colSums(sim$epi$stage.time.aids.art[469:520, ])), probs = 0.5) * ((1 - disc.rate)^9))

  return(list(time.hivneg = time.hivneg, stage.time.ar.ndx = stage.time.ar.ndx,
              stage.time.ar.dx = stage.time.ar.dx,
              stage.time.af.ndx = stage.time.af.ndx,
              stage.time.af.dx = stage.time.af.dx,
              stage.time.early.chronic.ndx = stage.time.early.chronic.ndx,
              stage.time.early.chronic.dx.yrone = stage.time.early.chronic.dx.yrone,
              stage.time.early.chronic.dx.yrstwotolate, stage.time.early.chronic.dx.yrstwotolate,
              stage.time.early.chronic.art = stage.time.early.chronic.art,
              stage.time.late.chronic.ndx = stage.time.late.chronic.ndx,
              stage.time.late.chronic.dx = stage.time.late.chronic.dx,
              stage.time.late.chronic.art, stage.time.late.chronic.art,
              stage.time.aids.ndx = stage.time.aids.ndx,
              stage.time.aids.dx = stage.time.aids.dx,
              stage.time.aids.art = stage.time.aids.art))
}
