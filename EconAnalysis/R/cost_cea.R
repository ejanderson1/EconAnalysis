#' @title Cost Module
#'
#' @description Calculates the total cost of an intervention or epidemic
#'
#' @return This function returns an object with the total cost of an
#'         intervention or epidemic over the duration of the model
#'
#' @details
#' Costs are calculated by [insert]
#'
#' @keywords CEA
#'
#' @export
#'

cost_calc_em <- function(ann.hivneg.cost,
                      ann.acute.cost,
                      ann.early.chron.cost,
                      ann.late.chron.cost,
                      ann.aids.cost,
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
                      tot.acute.cost,
                      tot.early.chron.cost,
                      tot.late.chron.cost,
                      tot.aids.cost,
                      hiv.test.cost,
                      hivpos.test.cost,
                      syph.test.cost,
                      ct.test.cost,
                      ng.test.cost,
                      num.hiv.tests,
                      num.hivpos.test,
                      num.syph.test,
                      num.ct.test,
                      num.ng.cost,
                      ...){

  # Annual healthcare costs

  # HIV-negative
  tot.hivneg.cost <- ann.hivneg.cost * pt.neg

  # HIV-positive
  tot.acute.cost <- ann.acute.cost * (pt.acute.undx + pt.acute.dx)
  tot.early.chron.cost <- ann.early.chron.cost * (pt.early.chron.undx +
                                                    pt.early.chron.dx.yr1 +
                                                    pt.early.chron.dx.postyr1 +
                                                    pt.early.chron.art)
  tot.late.chron.cost <- ann.late.chron.cost * (pt.late.chron.undx +
                                                  pt.late.chron.dx +
                                                  pt.late.chron.art)
  tot.aids.cost <- ann.aids.cost * (pt.aids.undx + pt.aids.dx + pt.aids.art)

  # Total Annual HIV-related healthcare costs
  tot.hiv.care.cost <- tot.acute.cost + tot.early.chron.cost + tot.late.chron.cost
                  + tot.aids.cost

  # HIV testing costs
  tot.hiv.test.cost <- (num.hiv.tests * hiv.test.cost) +
                   (num.hivpos.test * hivpos.test.cost)

  # Total costs (excluding STI costs)
  tot.hiv.cost <- tot.hiv.care.cost + tot.hiv.test.cost + tot.hivneg.cost

  # STI testing costs
  tot.syph.test.cost <- num.syph.test * syph.test.cost
  tot.ct.test.cost <- num.ct.test * ct.test.cost
  tot.ng.test.cost <- num.ng.cost * ng.test.cost
  tot.sti.test.cost <- tot.syph.test.cost + tot.ct.test.cost + tot.ng.test.cost

  # STI treatment costs



  return(list(tot.hivneg.cost, tot.acute.cost, tot.early.chron.cost,
              tot.late.chron.cost, tot.aids.cost, tot.hiv.care.cost,
              tot.hiv.test.cost, tot.hiv.cost, tot.syph.test.cost,
              tot.ct.test.cost, tot.ng.test.cost, tot.sti.test.cost))
}










