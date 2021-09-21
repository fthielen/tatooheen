#' Fit parametric survival functions
#' @description A function to fit several parametric survival functions. This function is adapted from `daarthools::fit.fun()`
#' @param data The data set
#' @param formula The formula with or without co-variates (to be specified with `as.formula()`)
#' @export


fit_fun <- function(data,
                    formula) {

        # Survival
        # Fit Kaplan-Meier
        KM.fit <- survfit(
                as.formula(
                        paste(
                                c(as.character(surv_formula)[c(2, 1)],
                                  "1"),
                                collapse = " ")
                        ),
                data = data)

        # Fit parametric
        fit.gengamma  <- flexsurvreg(form,
                                     data = data,
                                     dist = "gengamma") # fit model with gamma distribution
        fit.genf  <- flexsurvreg(form,
                                 data = data,
                                 dist = "genf") # fit model with Generalized F (stable) distribution
        fit.weib   <- flexsurvreg(form,
                                  data = data,
                                  dist = "weibull")       # fit model with Weibull distribution
        fit.gamma  <- flexsurvreg(form,
                                  data = data,
                                  dist = "gamma")       # fit model with gamma distribution
        fit.exp    <- flexsurvreg(form,
                                  data = data,
                                  dist = "exp")       # fit model with exponential distribution
        fit.llogis <- flexsurvreg(form,
                                  data = data,
                                  dist = "llogis")       # fit model with loglogistic distribution
        fit.lnorm  <- flexsurvreg(form,
                                  data = data,
                                  dist = "lnorm")       # fit model with lognormal distribution
        fit.gompertz  <- flexsurvreg(form,
                                     data = data,
                                     dist = "gompertz") # fit model with gompertz distribution

        # compare AIC values
        AIC <- c(
                GenGamma    = AIC(fit.gengamma),
                GeneralizedF = AIC(fit.genf),
                Weibull     = AIC(fit.weib),
                Gamma       = AIC(fit.gamma),
                Exponential = AIC(fit.exp),
                Loglogistic = AIC(fit.llogis),
                Lognormal   = AIC(fit.lnorm),
                Gompertz    = AIC(fit.gompertz)
        )
        AIC = round(AIC)

        # compare BIC values
        BIC <- c(
                GenGamma    = BIC(fit.gengamma),
                GeneralizedF = BIC(fit.genf),
                Weibull     = BIC(fit.weib),
                Gamma       = BIC(fit.gamma),
                Exponential = BIC(fit.exp),
                Loglogistic = BIC(fit.llogis),
                Lognormal   = BIC(fit.lnorm),
                Gompertz    = BIC(fit.gompertz)
        )

        BIC <- round(BIC)

        res <- list(
                KM = KM.fit,
                GenGamma    = fit.gengamma,
                GeneralizedF = fit.genf,
                Weibull     = fit.weib,
                Gamma       = fit.gamma,
                Exponential = fit.exp,
                Loglogistic = fit.llogis,
                Lognormal   = fit.lnorm,
                Gompertz    = fit.gompertz,
                AIC         = AIC,
                BIC         = BIC
        )
        res
}
