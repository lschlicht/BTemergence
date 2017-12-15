rptGaussian2 = function (formula, grname, data, CI = 0.95, nboot = 1000, npermut = 0,
          parallel = FALSE, ncores = NULL, ratio = TRUE, adjusted = TRUE,
          rptObj = NULL, update = FALSE)
{
  no_NA_vals <- stats::complete.cases(as.data.frame(data)[all.vars(formula)])
  if (sum(!no_NA_vals) > 0) {
    warning(paste0(sum(!no_NA_vals), " rows containing missing values were removed"))
    data <- data[no_NA_vals, ]
  }
  if (!any((grname != "Residual") & (grname != "Overdispersion") &
           (grname != "Fixed")))
    stop("Specify at least one grouping factor in grname")
  mod <- lme4::lmer(formula, data = data)
  if (nboot == 1) {
    warning("nboot has to be greater than 1 to calculate a CI and has been set to 0")
    nboot <- 0
  }
  if (nboot < 0)
    nboot <- 0
  if (npermut < 1)
    npermut <- 1
  grname_org <- grname
  output_resid <- FALSE
  output_overdisp <- FALSE
  output_fixed <- FALSE
  for (component in c("Residual", "Overdispersion", "Fixed")) {
    if (any(grname == component)) {
      grname <- grname[-which(grname == component)]
      if (component == "Residual")
        output_resid <- TRUE
      if (component == "Overdispersion")
        output_overdisp <- TRUE
      if (component == "Fixed")
        output_fixed <- TRUE
    }
  }
  terms <- attr(terms(formula), "term.labels")
  randterms <- terms[which(regexpr(" //| ", terms, perl = TRUE) >
                             0)]
  check_modelspecs <- sapply(grname, function(x) sum(grepl(paste0("\\\\b",
                                                                  x, "\\\\b"), randterms)))
  if (any(check_modelspecs > 1)) {
    stop("Fitting the same grouping factor in more than one random \\n                      effect term is not possible at the moment")
  }
  R_pe <- function(formula, data, grname, mod = NULL, resp = NULL) {
    if (!is.null(mod)) {
      mod <- lme4::refit(mod, newresp = resp)
    }
    else {
      mod <- lme4::lmer(formula, data)
    }
    VarComps <- lme4::VarCorr(mod)
    var_e <- attr(VarComps, "sc")^2
    names(var_e) <- "Residual"
    var_o <- var_e
    names(var_o) <- "Overdispersion"
    var_f <- stats::var(stats::predict(mod, re.form = NA))
    names(var_f) <- "Fixed"
    var_a <- unlist(lapply(grname, rptR:::group_vars, VarComps,
                           mod))
    names(var_a) <- grname
    var_VarComps <- unlist(lapply(names(VarComps), rptR:::group_vars,
                                  VarComps, mod))
    var_p <- sum(as.numeric(var_VarComps)) + attr(VarComps,
                                                  "sc")^2
    if (!adjusted)
      var_p <- var_p + var_f
    if (ratio == FALSE) {
      R <- as.data.frame(t(var_a))
      names(R) <- grname
      if (output_resid) {
        R$Residual <- var_e
      }
      if (output_overdisp) {
        R$Overdispersion <- var_o
      }
      if (output_fixed) {
        R$Fixed <- var_f
      }
      return(R)
    }
    if (ratio == TRUE) {
      R <- var_a/var_p
      R <- as.data.frame(t(R))
      names(R) <- grname
      if (output_resid) {
        R$Residual <- var_e/var_p
      }
      if (output_overdisp) {
        R$Overdispersion <- var_e/var_p
      }
      if (output_fixed) {
        R$Fixed <- var_f/var_p
      }
      return(R)
    }
  }
  R <- R_pe(formula, data, grname)
  if (nboot > 0)
    Ysim <- as.matrix(stats::simulate(mod, nsim = nboot))
  bootstr <- function(y, mod, formula, data, grname) {
    resp <- as.vector(y)
    R_pe(formula, data, grname, mod = mod, resp = resp)
  }
  num_iter <- NULL
  warnings_boot <- rptR:::with_warnings({
    if (nboot > 0 & parallel == TRUE) {
      if (is.null(ncores)) {
        ncores <- parallel::detectCores() - 1
        warning("No core number specified: detectCores() is used to detect the number of \\n cores on the local machine")
      }
      cl <- parallel::makeCluster(ncores)
      parallel::clusterExport(cl, "R_pe", envir = environment())
      R_boot <- unname(parallel::parApply(cl = cl, Ysim,
                                          2, bootstr, mod = mod, formula = formula, data = data,
                                          grname = grname))
      parallel::stopCluster(cl)
    }
    if (nboot > 0 & parallel == FALSE) {
      cat("Bootstrap Progress:\\n")
      R_boot <- unname(pbapply::pbapply(Ysim, 2, bootstr,
                                        mod = mod, formula = formula, data = data, grname = grname))
    }
    if (nboot == 0) {
      R_boot <- NA
    }
  })
  boot <- as.list(rep(NA, length(grname)))
  names(boot) <- grname
  calc_CI <- function(x) {
    out <- stats::quantile(x, c((1 - CI)/2, 1 - (1 - CI)/2),
                           na.rm = TRUE)
  }
  if (length(R_boot) == 1) {
    if (is.na(R_boot)) {
      se <- NA
      CI_emp <- calc_CI(NA)
    }
  }
  else {
    boot <- do.call(rbind, R_boot)
    if (update) {
      if (is.null(rptObj))
        stop("provide rpt object for rptObj argument")
      boot <- rbind(boot, rptObj$R_boot)
    }
    CI_emp <- as.data.frame(t(apply(boot, 2, calc_CI)))
    se <- as.data.frame(t(as.data.frame(lapply(boot, stats::sd))))
    names(se) <- "se"
  }
  P_permut <- rep(NA, length(grname))
  if (npermut == 1) {
    R_permut <- NA
    P_permut <- NA
  }
  permut <- function(nperm, formula, data, mod_red, dep_var,
                     grname, i, mod) {
    y_perm <- stats::fitted(mod_red) + sample(stats::resid(mod_red))
    data_perm <- data
    data_perm[dep_var] <- y_perm
    out <- R_pe(formula, data_perm, grname[i], mod = mod,
                resp = y_perm)
    out
  }
  dep_var <- as.character(formula)[2]
  R_permut <- data.frame(matrix(rep(NA, length(grname) * npermut),
                                nrow = length(grname)))
  P_permut <- rep(NA, length(grname))
  if (update) {
    if (is.null(rptObj))
      stop("provide rpt object for rptObj argument")
    old_perm_length <- length(rptObj$R_permut[[1]])
    R_permut <- data.frame(matrix(rep(NA, length(grname) *
                                        (npermut + old_perm_length)), nrow = length(grname)))
    if (npermut > 0)
      npermut <- npermut + 1
  }
  mod_fun <- ifelse(length(randterms) == 1, stats::lm, lme4::lmer)
  warnings_permut <- rptR:::with_warnings({
    if (npermut > 1) {
      for (i in 1:length(grname)) {
        randterm <- randterms[grep(grname[i], randterms)]
        formula_red <- stats::update(formula, eval(paste(". ~ . ",
                                                         paste("- (", randterm, ")"))))
        mod_red <- mod_fun(formula_red, data = data)
        if (parallel == TRUE) {
          if (is.null(ncores)) {
            ncores <- parallel::detectCores()
            warning("No core number specified: detectCores() is used to detect the number of \\n cores on the local machine")
          }
          cl <- parallel::makeCluster(ncores)
          parallel::clusterExport(cl, "R_pe", envir = environment())
          R_permut[i, ] <- c(R[i], as.numeric(unlist(parallel::parSapply(cl,
                                                                         1:(npermut - 1), permut, formula, data, mod_red,
                                                                         dep_var, grname, i, mod))))
          parallel::stopCluster(cl)
        }
        else if (parallel == FALSE) {
          cat("Permutation Progress for", grname[i],
              ":\\n")
          R_permut[i, ] <- c(R[i], as.numeric(unlist(pbapply::pbreplicate(npermut -
                                                                            1, permut(formula = formula, data = data,
                                                                                      mod_red = mod_red, dep_var = dep_var, grname = grname,
                                                                                      i = i, mod = mod)))))
        }
        if (update) {
          if (is.null(rptObj))
            stop("provide rpt object for rptObj argument")
          R_permut[i, ] <- c(rptObj$R_permut[[i]], unlist(R_permut[i,
                                                                   ])[-1])
        }
        P_permut[i] <- sum(R_permut[i, ] >= unlist(R[i]))/npermut
      }
    }
  })
  row.names(R_permut) <- grname
  names(P_permut) <- grname
  LRT_mod <- ifelse(length(randterms) == 1, as.numeric(stats::logLik(stats::update(mod,
                                                                                   REML = FALSE))), as.numeric(stats::logLik(mod)))
  VarComps <- lme4::VarCorr(mod)
  mat_dims <- unlist(lapply(VarComps[grname], ncol))
  calc_df <- function(k, k_names) {
    if (k == 1)
      df <- 1
    if (k > 1) {
      terms <- attr(terms(formula), "term.labels")
      current_term <- terms[grep(k_names, terms)]
      df <- k * (k - 1)/2 + k
    }
    df
  }
  LRT_df <- mapply(calc_df, mat_dims, names(mat_dims))
  for (i in c("LRT_P", "LRT_D", "LRT_red")) assign(i, rep(NA,
                                                          length(grname)))
  for (i in 1:length(grname)) {
    randterm <- randterms[grep(grname[i], randterms)]
    formula_red <- stats::update(formula, eval(paste(". ~ . ",
                                                     paste("- (", randterm, ")"))))
    LRT_red[i] <- as.numeric(stats::logLik(mod_fun(formula = formula_red,
                                                   data = data)))
    LRT_D[i] <- as.numeric(-2 * (LRT_red[i] - LRT_mod))
    LRT_P[i] <- ifelse(LRT_D[i] <= 0, 1, stats::pchisq(LRT_D[i],
                                                       LRT_df[i], lower.tail = FALSE))
  }
  LRT_P <- LRT_P/ifelse(LRT_df == 1 & LRT_D > 0, 2, 1)
  LRT_table <- data.frame(logL_red = LRT_red, LR_D = LRT_D,
                          LRT_P = LRT_P, LRT_df = LRT_df, stringsAsFactors = FALSE)
  row.names(LRT_table) <- grname
  P <- as.data.frame(cbind(LRT_P, P_permut))
  row.names(P) <- grname
  for (component in c("Residual", "Overdispersion", "Fixed")) {
    if (any(grname_org == component)) {
      P <- rbind(P, as.numeric(NA))
      row.names(P)[nrow(P)] <- component
      R_permut <- rbind(R_permut, as.numeric(NA))
      row.names(R_permut)[nrow(R_permut)] <- component
      LRT_table <- rbind(LRT_table, as.numeric(NA))
      row.names(LRT_table)[nrow(LRT_table)] <- component
    }
  }
  res <- list(call = match.call(), datatype = "Gaussian", CI = CI,
              R = R, se = se, CI_emp = CI_emp, P = P, R_boot = boot,
              R_permut = lapply(as.data.frame(t(R_permut)), function(x) return(x)),
              LRT = list(LRT_mod = LRT_mod, LRT_table = LRT_table),
              ngroups = unlist(lapply(as.data.frame(data)[grname], function(x) length(unique(x)))),
              nobs = nrow(data), mod = mod, ratio = ratio, adjusted = adjusted,
              all_warnings = list(warnings_boot = warnings_boot, warnings_permut = warnings_permut))
  class(res) <- "rpt"
  return(res)
}
