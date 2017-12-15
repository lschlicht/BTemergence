rpt2 = function (formula, grname, data, datatype = c("Gaussian", "Binary",
                                              "Proportion", "count"), link = c("logit", "probit", "log",
                                                                               "sqrt"), CI = 0.95, nboot = 1000, npermut = 0, parallel = FALSE,
          ncores = NULL, ratio = TRUE, adjusted = TRUE, expect = "meanobs",
          rptObj = NULL, update = FALSE)
{
  if (datatype == "Gaussian") {
    out_gaussian <- rptGaussian2(formula, grname, data, CI,
                                nboot, npermut, parallel, ncores, ratio, adjusted,
                                rptObj, update)
    out_gaussian$call <- match.call()
    return(out_gaussian)
  }
  if (datatype == "Binary") {
    out_binary <- rptBinary(formula, grname, data, link,
                            CI, nboot, npermut, parallel, ncores, ratio, adjusted,
                            expect, rptObj, update)
    out_binary$call <- match.call()
    return(out_binary)
  }
  if (datatype == "Proportion") {
    out_proportion <- rptProportion(formula, grname, data,
                                    link, CI, nboot, npermut, parallel, ncores, ratio,
                                    adjusted, expect, rptObj, update)
    out_proportion$call <- match.call()
    return(out_proportion)
  }
  if (datatype == "Poisson") {
    out_poisson <- rptPoisson(formula, grname, data, link,
                              CI, nboot, npermut, parallel, ncores, ratio, adjusted,
                              expect, rptObj, update)
    out_poisson$call <- match.call()
    return(out_poisson)
  }
}
