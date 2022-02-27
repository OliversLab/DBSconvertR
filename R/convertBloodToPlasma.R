#' Estimates plasma concentration from whole blood or dried blood concentrations
#' @returns estimated plasma concentration
#' @examples
#' convertBloodToPlasma()
#' @export



convertBloodToPlasma <- function(conc_blood = NULL, conc_plasma=NULL, hct =NULL, rho = NULL, fu = NULL, K_bp = NULL, pb=NULL, dem=NULL, method="Method 1") {

  #TODO hct length either 1 or same as conc_blood

  if (method == "Method 1") {
    est_conc_plasma <- conc_blood / ( (1-hct) + hct * rho * fu)
  } else if (method == "Method 2") {
    est_conc_plasma <- conc_blood / ( (1-hct) + hct * K_bp)
  } else if (method == "Method 3") {
    bp_ratio <- conc_blood/conc_plasma
    est_conc_plasma <- conc_blood / mean(bp_ratio)
  } else if (method == "Method 4") {
    est_conc_plasma <- (conc_blood-pb@glob.coef[1]) / pb@glob.coef[2]
  } else if (method == "Method 5") {
    est_conc_plasma <- (conc_blood-dem@glob.coef[1]) / dem@glob.coef[2]
  }


  return(est_conc_plasma)

}
