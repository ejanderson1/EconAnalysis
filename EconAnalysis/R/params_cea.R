#' @title CEA parameters
#'
#' @description sets parameter values for the CEA
#'
#' @param HIVneg.util utility for health state: HIV negative
#' @param acute.undx.util utility for health state: actue HIV, undiagnosed
#' @param acute.dx.util utility for health state: actue HIV, diagnosed
#' @param early.chron.undx.util utility for health state: early chronic HIV,
#'        undiagnosed
#' @param early.chron.dx.yr1.util utility for health state: early chronic HIV,
#'        year 1 post-diagnosis
#' @param early.chron.dx.postyr1.util utility for health state: early chronic HIV,
#'        subsequent years post-diagnosis
#' @param early.chron.art.util utility for health state: early chronic HIV,
#'        on ART
#' @param late.chron.undx.util utility for health state: late chronic HIV,
#'        undiagnosed
#' @param late.chron.dx.util utility for health state: late chronic HIV,
#'        diagnosed
#' @param late.chron.art.util utility for health state: late chronic HIV,
#'        on ART
#' @param aids.undx.util utility for health state: AIDS, undiagnosed
#' @param aids.dx.util utility for health state: AIDS, diagnosed
#' @param aids.art.util utility for health state: AIDS, on ART
#' @param healthcare.cost annual healthcare costs not associated with HIV,
#'        applied to both those with and without HIV
#' @param acute.cost annual healthcare costs for those in the acute HIV
#'        stage, not including the cost of ART
#' @param early.chron.cost annual healthcare costs for those in the
#'        early chronic stage and not on tx
#' @param early.chron.tx.cost annual healthcare costs for those in the
#'        early chronic stage and on tx, not including ART costs
#' @param late.chron.cost annual healthcare costs for those in the
#'        late chronic stage and not on tx
#' @param late.chron.tx.cost annual healthcare costs for those in the
#'        late chronic stage and on tx, not including ART costs
#' @param aids.cost annual healthcare costs for those in the AIDS stage
#'        and not on tx
#' @param aids.tx.cost  annual healthcare costs for those in the AIDS stage
#'        and on tx, not including ART costs
#' @param ann.art.cost annual cost of ART
#' @param test.init cost of preliminary HIV test (e.g. ELISA antibody test)
#' @param test.confirm cost of confirmatory HIV test for preliminary
#'        positive tests (e.g. Western Blot)
#' @param total.test.cost.pos total testing costs for HIV-positive individuals
#' @param pre.test.couns HIV pre-test counseling cost
#' @param post.test.pos HIV post-test counseling and linkage for HIV-positive
#'        individuals
#' @param post.test.neg HIV post-test costs for HIV-negative individuals
#' @param total.couns.cost.neg total cost of counseling (pre- and post-test)
#'        for HIV negative individuals
#' @param total.couns.cost.pos total cost of counseling (pre- and post-test)
#'        for HIV positive individuals
#' @param syph.test.couns.cost office visit, testing, and counseling cost for
#'        syphilis
#' @param rCT.test.couns.cost office visit, testing, and counseling cost for
#'        rectal chlamydia
#' @param uCT.test.couns.cost office visit, testing, and counseling cost for
#'        urethral chlamydia
#' @param rNG.test.couns.cost office visit, testing, and counseling cost for
#'        rectal gonorrhea
#' @param uNG.test.couns.cost office visit, testing, and counseling cost for
#'        urethral gonorrhea
#' @param disc.rate discount rate
#'
#' @return a list object of class \code{params_cea},
#' to pass in to [insert function]
#'
#' @export
#'

param_cea <- list(c(

  # HRQoL

  HIVneg.util <- 1.0,
  acute.undx.util <- 0.92,
  acute.dx.util = 0.86,
  early.chron.undx.util <- 0.91,
  early.chron.dx.yr1.util <- 0.84,
  early.chron.dx.postyr1.util <- 0.89,
  early.chron.art.util <- 0.95,
  late.chron.undx.util <- 0.79,
  late.chron.dx.util <- 0.72,
  late.chron.art.util <- 0.83,
  aids.undx.util <- 0.72,
  aids.dx.util <- 0.72,
  aids.art.util <- 0.82,

  # Costs

  # Annual non-HIV-related healthcare costs

  healthcare.cost <- 4632.67,

  # Annual HIV-related healthcare costs (excludes ART)

  acute.cost <- 34.5,
  early.chron.cost <- 4744.23,
  early.chron.tx.cost <- 3880.49,
  late.chron.cost <- 7964.56,
  late.chron.tx.cost <- 7100.82,
  aids.cost <- 25116.24,
  aids.tx.cost <- 11429.86,

  # Annual ART costs

  ann.art.cost <- 17908.46,

  # HIV testing costs

  test.init <- 25.41,
  test.confirm <- 31.57,
  total.test.cost.pos <- 25.41 + 31.57,

  # HIV counseling costs

  pre.test.couns <- 13,
  post.test.pos <- 14,
  post.test.neg <- 7,
  total.couns.cost.neg <- 13 + 7,
  total.couns.cost.pos <- 13 + 14,

  # STI testing and counseling costs

  syph.test.couns.cost <- 168.7,
  rCT.test.couns.cost <- 180.64,
  uCT.test.couns.cost <- 180.64,
  rNG.test.couns.cost <- 180.64,
  uNG.test.couns.cost <- 180.64,

  # STI treatment costs

  syph.tx <- 101.74,
  rCT.tx <- 51.91,
  uCT.tx <- 51.91,
  rNG.tx <- 51.91,
  uNG.tx <- 51.91,
  CT.tx <- 51.91,
  NG.tx <- 51.91,

  # Discount rate

  disc.rate <- 0

))



