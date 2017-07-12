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

# General function specific to EpiModel STI project

cost_calc_em <- function(ann.hlthcare.cost,
                      ann.acute.cost,
                      ann.early.chron.nart.cost,
                      ann.early.chron.art.cost,
                      ann.late.chron.nart.cost,
                      ann.late.chron.art.cost,
                      ann.aids.nart.cost,
                      ann.aids.art.cost,
                      ann.art.cost,
                      hiv.pretest.cost,
                      hiv.posttest.neg.cost,
                      hiv.posttest.pos.cost,
                      hiv.test1.cost,
                      confirm.hiv.test,
                      time.hivneg,
                      stage.time.ar.ndx,
                      stage.time.af.ndx,
                      stage.time.ar.dx,
                      stage.time.af.dx,
                      stage.time.early.chronic.ndx,
                      stage.time.early.chronic.dx.yrone,
                      stage.time.early.chronic.dx.yrstwotolate,
                      stage.time.early.chronic.art,
                      stage.time.late.chronic.ndx,
                      stage.time.late.chronic.dx,
                      stage.time.late.chronic.art,
                      stage.time.aids.ndx,
                      stage.time.aids.dx,
                      stage.time.aids.art,
                      hivtests.nprep,
                      hivtests.pos,
                      syph.test.cost,
                      ct.test.cost,
                      ng.test.cost,
                      ng.tx.cost,
                      ct.tx.cost,
                      earlysyph.tx.cost,
                      latesyph.tx.cost,
                      GCsympttests,
                      CTsympttests,
                      syphsympttests,
                      GCasympttests,
                      CTasympttests,
                      syphasympttests,
                      rGCasympttests.pos.pos,
                      uGCasympttests.pos,
                      GCasympttests.pos,
                      rCTasympttests.pos,
                      uCTasympttests.pos,
                      CTasympttests.pos,
                      syphasympttests.pos,
                      txCT,
                      txGC,
                      txearlysyph,
                      txlatesyph,
                      txasympt,
                      ...){



  # Annual cost for uninfected individuals
  hivneg.cost <- ann.hlthcare.cost *  time.hivneg

  # Annual cost for HIV-positive, acute stage
  acute.cost <- (ann.acute.cost + ann.hlthcare.cost) * (stage.time.ar.ndx +
                stage.time.af.ndx +stage.time.ar.dx + stage.time.af.dx)

  # Annual cost for HIV-positive, early chronic stage
  early.chron.nart.cost <- (ann.early.chron.nart.cost + ann.hlthcare.cost) *
                           (stage.time.early.chronic.ndx +
                            stage.time.early.chronic.dx.yrone +
                            stage.time.early.chronic.dx.yrstwotolate)

  # Annual cost for HIV-positive, early chronic stage on ART
  early.chron.art.cost <- (ann.early.chron.art.cost + ann.art.cost +
                           ann.hlthcare.cost) * stage.time.early.chronic.art

  # Annual cost for HIV-positive, late chronic stage
  late.chron.nart.cost <- (ann.late.chron.nart.cost + ann.hlthcare.cost) *
                          (stage.time.late.chronic.ndx +
                           stage.time.late.chronic.dx)

  # Annual cost for HIV-positive, late chronic stage on ART
  late.chron.art.cost <- (ann.late.chron.art.cost + ann.art.cost +
                          ann.hlthcare.cost) * stage.time.late.chronic.art

  # Annual cost for AIDS
  aids.nart.cost <- (ann.aids.nart.cost + ann.hlthcare.cost) *
                    (stage.time.aids.ndx + stage.time.aids.dx)

  # Annual cost for AIDS on ART
  aids.art.cost <- (ann.aids.art.cost + ann.hlthcare.cost + ann.art.cost) *
                   stage.time.aids.art

  # HIV testing cost - HIV negative
  hivneg.test.cost <- (hivtests.nprep - hivtests.pos) * (hiv.pretest.cost +
                      hiv.posttest.neg.cost + hiv.test1.cost)

  # HIV testing cost - HIV positive
  hivpos.test.cost <- hivtests.pos * (hiv.pretest.cost + hiv.posttest.pos.cost
                      + hiv.test1.cost + confirm.hiv.test)

  # Total HIV costs (negative and positive)
  tot.hiv.cost <- hivneg.cost + acute.cost + early.chron.nart.cost +
                  early.chron.art.cost + late.chron.nart.cost +
                  late.chron.art.cost + aids.nart.cost + aids.art.cost +
                  hivneg.test.cost + hivpos.test.cost

  # Gonorrhea testing & counseling costs
  tot.ng.test.cost <- ng.test.cost * (GCsympttests + GCasympttests)

  # Chlamdydia testing & counseling costs
  tot.ct.test.cost <- ct.test.cost * (CTsympttests + CTasympttests)

  # Syphilis testing & counseling costs
  tot.syph.test.cost <- syph.test.cost * (syphsympttests + syphasympttests)

  # Gonorrhea treatment costs
  tot.ng.tx.cost <- txGC * ng.tx.cost

  # Chalmydia treatment costs
  tot.ct.tx.cost <- txCT * ct.tx.cost

  # Syphilis treatment costs
  tot.syph.tx.cost <- (txearlysyph * earlysyph.tx.cost) +(txlatesyph *
                       latesyph.tx.cost)

  # Total NG costs
  tot.ng.cost <- tot.ng.test.cost + tot.ng.tx.cost

  # Total CT costs
  tot.ct.cost <- tot.ct.test.cost + tot.ct.tx.cost

  # Total syphilis costs
  tot.syph.cost <- tot.syph.test.cost + tot.syph.tx.cost

  # Total STI costs
  tot.sti.cost <- tot.ng.cost + tot.ct.cost + tot.syph.cost

  # Total cost (HIV and STIs)
  total.cost <- tot.hiv.cost + tot.sti.cost

  return(list(hivneg.cost, acute.cost, early.chron.nart.cost,
              early.chron.art.cost, late.chron.nart.cost, late.chron.art.cost,
              aids.nart.cost, aids.art.cost, hivneg.test.cost, hivpos.test.cost,
              tot.hiv.cost, tot.ng.test.cost, tot.ct.test.cost,
              tot.syph.test.cost, tot.ng.tx.cost, tot.ct.tx.cost,
              tot.syph.tx.cost, tot.ng.cost, tot.ct.cost, tot.syph.cost,
              tot.sti.cost, total.cost))
}


