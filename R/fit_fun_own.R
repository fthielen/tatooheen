#' Fit parametric survival functions own
#'
#'
#'

SurvExtraGroupFun <- function(time.event, event, grouping = 1,
                              data.source, wgts = NULL,
                              max.yr.plot = 1,
                              name.plot = ""){

        require(survival)
        require(flexsurv)
        # Max time to plot
        max_plot_x <- max.yr.plot * 365.25

        # Possible models
        std_mods_names <- c("Exponential", "Weibull", "Log normal", "Log logistic",
                            "Gompertz", "Generalized gamma")
        std_mods <- c("exponential", "weibull", "lnorm", "llogis", "gompertz", "gengamma")

        plot_cols <- c("black", "green", "red", "deeppink",
                       "blue", "gray40", "darkorange3", "bisque2",
                       "bisque3", "bisque4", "darkkhaki")


        # Kaplan-Meier
        # Translate function arguments into arguments that can be used
        params <- list(time.event = substitute(time.event),
                       event = substitute(event),
                       grouping = substitute(grouping),
                       data.source = substitute(data.source),
                       wgts = substitute(wgts))

        # Generate Kaplan-Meier ======================================================

        km.expr <- substitute(survfit(Surv(time = time.event, event = event) ~ grouping,
                                      data = data.source, weights = wgts),
                              params)
        km <- eval.parent(km.expr)


        fit <- eval.parent(
                substitute(
                        flexsurvreg(Surv(time = time.event, event = event) ~ grouping,
                                    data = data.source, dist = "exponential",
                                    weights = wgts), params))

        # Fit all models
        ls_mods_try <- lapply(std_mods, function(x) {
                tryCatch(update(fit, dist = x), error = function(e) NULL)})

        # Filter models that didnt parse
        names(ls_mods_try) <- std_mods_names
        ls_mods <- Filter(Negate(is.null), ls_mods_try)
        n_ls_mods <- length(ls_mods)
        failed_mods <- setdiff(std_mods_names, names(ls_mods))
        failed_mods_pos <- which(std_mods == failed_mods)


        # AIC/BIC table
        aucbic <- data.frame(Distribution = std_mods_names,
                             AIC = sapply(ls_mods,
                                          function(x) (x)$AIC),
                             BIC = sapply(ls_mods,
                                          function(x) BIC(x)),
                             stringsAsFactors = F) %>%
                dplyr::mutate(AIC_rank = rank(AIC),
                              BIC_rank = rank(BIC)) %>%
                dplyr::select(Distribution, AIC, AIC_rank, BIC, BIC_rank)


        # Plot
        p_col_use <- as.list(plot_cols[1:n_ls_mods])
        try_plot_line <- function(x, colour_n){
                tryCatch(
                        lines(x,
                              col = plot_cols[colour_n],
                              lwd = 1,
                              t = 0:max_plot_x,
                              B = 0),
                        error = function(e) NULL)
        }

        plot(x = 0:max_plot_x, xaxs = "i",
             ylim = 0:1, yaxs = "i",
             type = "n", xaxt = "n",
             main = name.plot, xlab = "Time", ylab = "Survival probability"
        )
        axis(1,
             at = seq(0, max_plot_x, by = (365.25 * 2)),
             labels = seq(0, max_plot_x / 365.25,
                          by = 2)
        )
        lines(km, lty = c(1:3),
              lwd = 2,
              conf.int = FALSE, mark.time = F)
        try_plot_line(ls_mods[[1]], colour_n = 2)
        try_plot_line(ls_mods[[2]], colour_n = 3)
        try_plot_line(ls_mods[[3]], colour_n = 4)
        try_plot_line(ls_mods[[4]], colour_n = 5)
        try_plot_line(ls_mods[[5]], colour_n = 6)
        try_plot_line(ls_mods[[6]], colour_n = 7)
        legend("topright", bg = "transparent",
               ncol = 1,
               legend = c(paste("KM:",
                                names(km$strata)[1]),
                          paste("KM:",
                                names(km$strata)[2]),
                          names(ls_mods)),
               col = plot_cols[c(1, 1 , 2:7)],
               lty = c(1, 2, 1, 1, 1, 1, 1, 1),
               lwd = c(2, 2, 1, 1, 1, 1, 1, 1))

        res <- list(aicbic = aucbic,
                    models = ls_mods)

}

#
# xts <- SurvExtraGroupFun(time.event = os.time, event = os.status,
#                          data.source = db.survival, grouping = TRT01P,
#                          max.yr.plot = 40)





SurvExtraProbFun <- function(time.event, event, grouping = 1,
                             data.source,
                             wgts = NULL,
                             draw.plot = "yes",
                             show.aic.bic = "yes",
                             draw.distribution = "all", max.year.plot = 20,
                             t.y.extrapol = 80,
                             export.extrapol.dist = "exponential",
                             name.plot = "no name",
                             break.time.at = 365.25,
                             xaxis.lab = "Time in years",
                             sim.ci = 0,
                             splines = FALSE) {

        # Check input
        poss.dist <- c("exponential", "weibull", "lognorm", "loglog", "gompertz",
                       "gen.gamma",
                       #"gen.f",
                       "spline1k", "spline2k"
        )

        full.names <- c("Exponential", "Weibull", "Log normal", "Log logistic",
                        "Gompertz", "Generalized gamma",#"Generalized F",
                        "Spline: 1 knot",
                        "Spline: 2 knot", "Spline: 3 knot", "Spline: 4 knot")

        if (!draw.distribution %in% c(poss.dist, "all")) stop(
                cat("In draw.distribution: possible distributions are:",
                    paste0(poss.dist, ","), "or 'all'.")
        )

        if (!export.extrapol.dist %in% c(poss.dist, "all")) stop(
                cat("In export.extrapol.dist: possible distributions are:",
                    paste0(poss.dist, ","), ".")
        )

        n_models <- ifelse(splines, 8, 6)

        # Load in required packages ==================================================
        require(survival)
        require(flexsurv)
        require(survminer)

        # General set-up =============================================================
        # Create vector exrtraploation time
        t.d.extrapol <- t.y.extrapol * 365.25 # translate years in days
        v.t.d.extrapol <- seq(0,t.d.extrapol) # create vector starting at 0

        # Translate function arguments into arguments that can be used
        params <- list(time.event = substitute(time.event),
                       event = substitute(event),
                       grouping = substitute(grouping),
                       data.source = substitute(data.source),
                       wgts = substitute(wgts))

        # Generate Kaplan-Meier ======================================================

        km.expr <- substitute(survfit(Surv(time = time.event, event = event) ~ grouping,
                                      data = data.source, weights = wgts),
                              params)
        km <- eval.parent(km.expr)

        # Fit parametric models =====================================================
        # Exponential
        fit1 <- eval.parent(
                substitute(
                        flexsurvreg(Surv(time = time.event, event = event) ~ grouping,
                                    data = data.source, dist = "exponential",
                                    weights = wgts), params))
        # Extract coefs
        coef_fit1   <- fit1$coefficients        # parameter mean values
        vcov_fit1   <- fit1$cov                 # parameter variance covariance
        fit1.cov2cor <- cov2cor(vcov_fit1)
        rate1 <- exp(coef_fit1)[1]

        st.exp <- 1 - pexp(v.t.d.extrapol, rate = rate1)

        # Weibull
        fit2 <- eval.parent(
                substitute(
                        flexsurvreg(Surv(time = time.event, event = event) ~ grouping,
                                    data = data.source, dist = "weibull",
                                    weights = wgts), params))

        coef_fit2   <- fit2$coefficients        # parameter mean values
        vcov_fit2   <- fit2$cov                 # parameter variance covariance
        fit2.cov2cor <- cov2cor(vcov_fit2)
        shape2 <- exp(coef_fit2)[1]
        scale2 <- exp(coef_fit2)[2]

        st.wei <- 1 - pweibull(v.t.d.extrapol, shape = shape2, scale = scale2)

        # Lognormal
        fit3 <- eval.parent(
                substitute(
                        flexsurvreg(Surv(time = time.event, event = event) ~ grouping,
                                    data = data.source, dist = "lnorm",
                                    weights = wgts), params))

        coef_fit3   <- fit3$coefficients        # parameter mean values
        vcov_fit3   <- fit3$cov                 # parameter variance covariance
        fit3.cov2cor <- cov2cor(vcov_fit3)
        mean.log3 <- exp(coef_fit3)[1]
        sd.log3 <- exp(coef_fit3)[2]

        st.lognorm <- 1 - plnorm(q = v.t.d.extrapol,
                                 meanlog = log(mean.log3),
                                 sdlog = sd.log3,
                                 lower.tail = T, log.p = F)

        # Loglog
        fit4 <- eval.parent(
                substitute(
                        flexsurvreg(Surv(time = time.event, event = event) ~ grouping,
                                    data = data.source, dist = "llogis",
                                    weights = wgts), params))

        coef_fit4   <- fit4$coefficients        # parameter mean values
        vcov_fit4   <- fit4$cov                 # parameter variance covariance
        fit4.cov2cor <- cov2cor(vcov_fit4)
        shape <- exp(coef_fit4)[1]
        scale <- exp(coef_fit4)[2]
        st.loglog <- 1 - pllogis(q = v.t.d.extrapol, shape = shape, scale = scale)

        # Gompertz
        fit5 <- eval.parent(
                substitute(
                        flexsurvreg(Surv(time = time.event, event = event) ~ grouping,
                                    data = data.source, dist = "gompertz",
                                    weights = wgts), params))

        coef_fit5   <- fit5$coefficients        # parameter mean values
        vcov_fit5   <- fit5$cov                 # parameter variance covariance
        fit5.cov2cor <- cov2cor(vcov_fit5)
        shape <- exp(coef_fit5)[1]
        rate <- exp(coef_fit5)[2]
        st.gomp.gr <- 1 - flexsurv::pgompertz(v.t.d.extrapol, shape = log(shape), rate = rate)


        # Gengamma
        fit6 <- eval.parent(
                substitute(
                        flexsurvreg(Surv(time = time.event, event = event) ~ grouping,
                                    data = data.source, dist = "gengamma",
                                    weights = wgts), params))

        coef_fit6   <- fit6$coefficients        # parameter mean values
        vcov_fit6   <- fit6$cov                 # parameter variance covariance
        fit6.cov2cor <- cov2cor(vcov_fit6)
        mu <- exp(coef_fit6)[1]
        sigma <- exp(coef_fit6)[2]
        q <- exp(coef_fit6)[3]

        st.gengam.gr <- 1 - pgengamma(v.t.d.extrapol, mu = log(mu), sigma = sigma, Q = log(q))
        # self generated line
        #line.gengam <- 1-pgengamma(v.t.d.extrapol, mu = mu.gengam, sigma = sig.gengam, Q = q.gengam)


        # GenF
        # fit7 <- eval.parent(
        #   substitute(
        #     flexsurvreg(Surv(time = time.event, event = event) ~ grouping,
        #                 data = data.source, dist = "genf",
        #                 weights = wgts), params))
        #
        # coef_fit7   <- fit7$coefficients        # parameter mean values
        # vcov_fit7   <- fit7$cov                 # parameter variance covariance
        # #fit7.cov2cor <- cov2cor(vcov_fit7)
        # mu <- exp(coef_fit7)[1]
        # sigma <- exp(coef_fit7)[2]
        # q <- exp(coef_fit7)[3]
        # p <- exp(coef_fit7)[4]
        #
        # st.genf.gr <- 1 - pgenf(v.t.d.extrapol, mu = log(mu), sigma = sigma,
        #                         Q = log(q), P = p)
        # # self generated line
        #line.gengam <- 1-pgengamma(v.t.d.extrapol, mu = mu.gengam, sigma = sig.gengam, Q = q.gengam)


        # Spline models ========================================================
        ## Intermediate
        st.spline.1k <- rep(0, length(v.t.d.extrapol))
        aic.spline.1k <- NA
        bic.spline.1k <- NA
        fit.spline.1k <- NA

        st.spline.2k <- rep(0, length(v.t.d.extrapol))
        aic.spline.2k <- NA
        bic.spline.2k <- NA
        fit.spline.2k <- NA


        if(splines){

                # 1 knot ---------------------------------------------------------------
                fit.spline.1k <- eval.parent(
                        substitute(
                                flexsurvspline(Surv(time = time.event, event = event) ~ grouping,
                                               data = data.source,
                                               weights = wgts, k = 1), params))

                #st.spline.1k <- data.frame(summary(fit.spline.1k, t = v.t.d.extrapol, ci = FALSE))$est
                st.spline.1k <- 1 - psurvspline(q = v.t.d.extrapol,
                                                gamma = coef(fit.spline.1k),
                                                knots = fit.spline.1k$knots,
                                                scale = fit.spline.1k$scale)

                aic.spline.1k <- fit.spline.1k$AIC
                bic.spline.1k <- BIC(fit.spline.1k)

                # 2 knots ---------------------------------------------------------------
                fit.spline.2k <- eval.parent(
                        substitute(
                                flexsurvspline(Surv(time = time.event, event = event) ~ grouping,
                                               data = data.source,
                                               weights = wgts, k = 2), params))

                st.spline.2k <- 1 - psurvspline(q = v.t.d.extrapol,
                                                gamma = coef(fit.spline.2k),
                                                knots = fit.spline.2k$knots,
                                                scale = fit.spline.2k$scale)

                aic.spline.2k <- fit.spline.2k$AIC
                bic.spline.2k <- BIC(fit.spline.2k)
        }

        # 3 knots ---------------------------------------------------------------
        #  fit.spline.3k <- eval.parent(
        #    substitute(
        #      flexsurvspline(Surv(time = time.event, event = event) ~ grouping,
        #                     data = data.source,
        #                     weights = wgts, k = 3), params))
        #
        # st.spline.3k <- 1 - psurvspline(q = v.t.d.extrapol,
        #                                  gamma = coef(fit.spline.3k),
        #                                  knots = fit.spline.3k$knots,
        #                                  scale = fit.spline.3k$scale)

        # 4 knots ---------------------------------------------------------------
        # fit.spline.4k <- eval.parent(
        #   substitute(
        #     flexsurvspline(Surv(time = time.event, event = event) ~ grouping,
        #                    data = data.source,
        #                    weights = wgts, k = 4), params))
        #
        # st.spline.4k <- 1 - psurvspline(q = v.t.d.extrapol,
        #                                 gamma = coef(fit.spline.4k),
        #                                 knots = fit.spline.4k$knots,
        #                                 scale = fit.spline.4k$scale)

        # Export model distribution ==================================================
        switch(export.extrapol.dist,
               exponential = st.model <- st.exp,
               weibull = st.model <- st.wei,
               lognorm = st.model <- st.lognorm,
               loglog = st.model <- st.loglog,
               gompertz = st.model <- st.gomp.gr,
               gen.gamma = st.model <- st.gengam.gr,
               #gen.f = st.model <<- st.genf.gr
               spline1k = st.model <- st.spline.1k,
               spline2k = st.model <- st.spline.2k
        )

        # Export model ==================================================
        switch(export.extrapol.dist,
               exponential = fit <- fit1,
               weibull = fit <- fit2,
               lognorm = fit <- fit3,
               loglog = fit <- fit4,
               gompertz = fit <- fit5,
               gen.gamma = fit <- fit6,
               #gen.f = fit <- fit7,
               spline1k = fit <- fit.spline.1k,
               spline2k = fit <- fit.spline.2k
        )

        # Export cov2cor ==================================================
        switch(export.extrapol.dist,
               exponential = vcov_fit <- vcov_fit1,
               weibull = vcov_fit <- vcov_fit2,
               lognorm = vcov_fit <- vcov_fit3,
               loglog = vcov_fit <- vcov_fit4,
               gompertz = vcov_fit <- vcov_fit5,
               gen.gamma = vcov_fit <- vcov_fit6,
               #gen.f = vcov_fit <- vcov_fit7,
               pline1k = vcov_fit <- NA,
               spline2k = vcov_fit <- NA
        )

        # Export coefit ==================================================
        switch(export.extrapol.dist,
               exponential = coef_fit <- coef_fit1,
               weibull = coef_fit <- coef_fit2,
               lognorm = coef_fit <- coef_fit3,
               loglog = coef_fit <- coef_fit4,
               gompertz = coef_fit <- coef_fit5,
               gen.gamma = coef_fit <- coef_fit6,
               #gen.f = coef_fit <- coef_fit7,
               pline1k = coef_fit <- NA,
               spline2k = coef_fit <- NA
        )

        ## AIC/BIC ===================================================================
        aicbic <- data.frame(distribution = full.names[1:n_models],
                             AIC = c(fit1$AIC, fit2$AIC, fit3$AIC,
                                     fit4$AIC, fit5$AIC, fit6$AIC
                                     #fit7$AIC,
                                     #aic.spline.1k, aic.spline.2k
                                     #fit.spline.3k$AIC, fit.spline.4k$AIC
                             ),
                             BIC = c(BIC(fit1), BIC(fit2), BIC(fit3),
                                     BIC(fit4), BIC(fit5), BIC(fit6)
                                     #BIC(fit7),
                                     #bic.spline.1k, bic.spline.2k
                                     #BIC(fit.spline.3k), BIC(fit.spline.4k)
                             ),
                             stringsAsFactors = F) %>%
                mutate(AIC_rank = rank(AIC),
                       BIC_rank = rank(BIC)) %>%
                select(distribution, AIC, AIC_rank, BIC, BIC_rank)


        ## Summary =================================================================
        # Median survival

        fit.medians <- rbind(
                summary(km)$table[7:9],
                unlist(summary(fit1, type = "median", B = sim.ci)),
                unlist(summary(fit2, type = "median", B = sim.ci)),
                unlist(summary(fit3, type = "median", B = sim.ci)),
                unlist(summary(fit4, type = "median", B = sim.ci)),
                unlist(summary(fit5, type = "median", B = sim.ci)),
                unlist(summary(fit6, type = "median", B = sim.ci))
                #unlist(summary(fit7, type = "median", B = sim.ci)),
                #unlist(summary(fit.spline.1k, type = "median", B = sim.ci)),
                #unlist(summary(fit.spline.2k, type = "median", B = sim.ci))
                #unlist(summary(fit.spline.3k, type = "median", B = sim.ci))
                #unlist(summary(fit.spline.4k, type = "median", B = sim.ci))
        ) %>%
                #round(2) %>%
                data.frame(stringsAsFactors = F) %>%
                #replace(is.na(.) | . == "Inf", "not reached") %>%
                rename(median = 1, LCL = 2, UCL = 3) %>%
                mutate(type = c("Observed", full.names[1:n_models])) %>%
                select(type, median, LCL, UCL)
        ## Draw plots ================================================================

        # ggplots ------------------------------------------------------------------
        all.st <- data.frame(probabilities = c(st.exp, st.wei, st.lognorm,
                                               st.loglog, st.gomp.gr, st.gengam.gr
                                               #st.genf.gr,
                                               #st.spline.1k, st.spline.2k
                                               #st.spline.3k #,st.spline.4k
        ),
        times = rep(v.t.d.extrapol, n_models),
        distribution = rep(full.names[1:n_models],
                           each = length(v.t.d.extrapol))) %>%
                mutate(distribution = factor(distribution, levels = full.names[1:n_models]))

        # Plot colours
        plot.cols <- c("black", "green", "red", "deeppink",
                       "blue", "gray40", "darkorange3", "bisque2",
                       #"bisque3",
                       "bisque4", "darkkhaki")

        # Max time to plot
        max.time.plot <- max.year.plot * 365.25

        # GGplot
        p1 <- survminer::ggsurvplot(km,
                                    #censor.shape = "|",
                                    size = 1,
                                    conf.int = FALSE,
                                    risk.table = FALSE,
                                    axes.offset = TRUE,
                                    #palette = plot.cols,
                                    #break.time.by = break.time.at,
                                    xlim = c(0, max.time.plot),
                                    pval = FALSE,
                                    legend.labs = "Kaplan-Meier",
                                    ggtheme = theme_light()
                                    #title = paste0("Kaplan-Meier curve", title.plot)
        )

        p1 <- p1$plot +
                geom_line(data = all.st,
                          aes(x = times, y = probabilities,
                              group = distribution, color = distribution)) +
                scale_x_continuous(expand = c(0, 0), labels = d2y,
                                   breaks = seq(0, max.time.plot,
                                                by = break.time.at)) +
                scale_y_continuous(expand = c(0, 0)) +
                scale_color_manual(name = "Legend",
                                   values = c("Kaplan-Meier" = plot.cols[1],
                                              "Exponential" = plot.cols[2],
                                              "Weibull" = plot.cols[3],
                                              "Log normal" = plot.cols[4],
                                              "Log logistic" = plot.cols[5],
                                              "Gompertz" = plot.cols[6],
                                              "Generalized gamma" = plot.cols[7]
                                              #"Generalized F" = plot.cols[8],
                                              #"Spline: 1 knot" = plot.cols[9],
                                              #"Spline: 2 knot" = plot.cols[10]
                                              #"Spline: 3 knot" = plot.cols[10]
                                              #"Spline: 4 knot" = plot.cols[11]
                                   ),
                                   breaks = c("Kaplan-Meier",
                                              "Exponential",
                                              "Weibull",
                                              "Log normal",
                                              "Log logistic",
                                              "Gompertz",
                                              "Generalized gamma"
                                              #"Generalized F",
                                              # "Spline: 1 knot",
                                              #  "Spline: 2 knot"
                                              #"Spline: 3 knot",
                                              #"Spline: 4 knot"
                                   )) +
                xlab(xaxis.lab) +
                guides(colour = guide_legend(nrow = 3))

        # base R plot -----------------------------------------------------------

        #  if(draw.plot == "yes"){
        #
        # #always plot KM curve
        # plot(km, conf.int = FALSE, mark.time = TRUE, log = FALSE,
        #      xscale =  365.25, xmax = 36525,  yaxs = "i", xlab = "Time in years",
        #      xlim = c(0, max.year.plot), ylim = c(0, 1),
        #      ylab = "Survival probability", lty = 1, lwd = 2,
        #      main = paste(name.plot, "Exported distribution:", export.extrapol.dist))
        #
        # #switch is a simplified and faster ifelse alternative
        # switch(draw.distribution,
        #        exponential = lines(st.exp, col = "green"),
        #        weibull = lines(st.wei, col = "red"),
        #        lognorm = lines(st.lognorm, col = "deeppink"),
        #        loglog = lines(st.loglog, col = "blue"),
        #        gompertz = lines(st.gomp.gr, col = "gray40"),
        #        gen.gamma = lines(st.gengam.gr, col = "darkorange3"),
        #        all = c(lines(st.exp, col = "green"),
        #                lines(st.wei, col = "red", lty = 1),
        #                lines(st.lognorm, col = "deeppink"),
        #                lines(st.loglog, col = "blue"),
        #                lines(st.gomp.gr, col = "gray40"),
        #                lines(st.gengam.gr, col = "darkorange3")
        #        ),
        #
        #        # add warning in case argument input doesn't match distributions
        #        stop("Please check function argument. Valid entries are: all, exponential, weibull, lognorm, loglog")
        # )
        # if(draw.distribution == "all"){
        #         legend(x = "topright",
        #                legend = c("Kaplan-Meier", "Exponential", "Weibull", "Log normal",
        #                           "Log-logistic", "Gompertz", "Generalized gamma"),
        #                lwd = 2, bty = "n",
        #                col = c("black", "green", "red", "deeppink", "blue", "gray40", "darkorange3"))
        # }
        # p <- recordPlot()
        # }
        #
        # if(draw.plot == "no"){
        # p <- "Plot was not drawn. Please make sure that function agrument `draw.plot` = 'yes' "
        # }

        # if(show.aic.bic == "yes") {
        #         print(aic.mat)
        # }

        mod.list <- list(plot = p1,
                         km.base = km,
                         aic.matrix = aicbic,
                         vcov_fit  = vcov_fit,
                         coef_fit = coef_fit,
                         chosen.mod = fit,
                         st.model = st.model,
                         chosen.dist = export.extrapol.dist,
                         exp.mod = fit1,
                         wei.mod = fit2,
                         logn.mod = fit3,
                         loglog.mod = fit4,
                         gomp.mod = fit5,
                         gengam.mod = fit6,
                         #genf.mod = fit7,
                         #  spline.1k = fit.spline.1k,
                         # spline.2k = fit.spline.2k,
                         # spline.3k = fit.spline.3k,
                         # spline.4k = fit.spline.4k,
                         fit.medians = fit.medians,
                         st.all = all.st)

}


# Test and run the function ====================================================
# x <- SurvExtraProbFun(time.event = time, event = status, data.source = lung,
#                  name.plot = "Foo", export.extrapol.dist = "exponential",
#                  draw.plot = "yes", show.aic.bic = "yes", max.year.plot = 5)
#
# ty <- SurvExtraProbFun(time.event = time, event = status, data.source = lung,
#                       name.plot = "rpob", export.extrapol.dist = "weibull",
#                       show.aic.bic = "no")
#SurvExtraFun(time.event = time, event = status, data.source = lung, name.plot = "Test")
#SurvExtraFun(time.event = time, event = status, data.source = lung, grouping = ph.ecog)
#SurvExtraFun(time.event = rectime, event = censrec, data.source = bc, grouping = group)

# Extract survival time from fitted models

SurvExtraTimeSummary <- function(model, times.yr, ...){
        times <- times.yr * 365.25

        t.km <- summary(model$km.base, t = times)
        t.exp.mod <- summary(model$exp.mod, t = times)
        t.wei.mod <- summary(model$wei.mod, t = times)
        t.logn.mod <- summary(model$logn.mod, t = times)
        t.loglog.mod <- summary(model$loglog.mod, t = times)
        t.gomp.mod <- summary(model$gomp.mod, t = times)
        t.gengam.mod <- summary(model$gengam.mod, t = times)

        res <- data.frame(Years = times.yr,
                          Observed = c(t.km$surv,
                                       rep(NA,
                                           length(times) - length(t.km$surv))),
                          Exponential = t.exp.mod[[1]][[2]],
                          Weibull = t.wei.mod[[1]][[2]],
                          Log_normal = t.logn.mod[[1]][[2]],
                          Log_logistic = t.loglog.mod[[1]][[2]],
                          Gompertz = t.gomp.mod[[1]][[2]],
                          Generalized_gamma = t.gengam.mod[[1]][[2]])
        res
}


#SurvExtraTimeSummary(xfit, times.yr = c(0, 0.5, 1, 2, 5, 10))

## NEW version 02-09-2019 ########################################################
# SurvExtraProbFun2 <- function(time.event, event, grouping = 1,
#                              data.source,
#                              wgts = NULL,
#                              draw.plot = "yes",
#                              show.aic.bic = "yes",
#                              draw.distribution = "all", max.year.plot = 20,
#                              t.y.extrapol = 80,
#                              export.extrapol.dist = "exponential",
#                              name.plot = "no name") {
#
#   # Check input
#   flexsurv.names <- c("exp", "weibull", "lnorm", "llogis", "gompertz",
#                       "gengamma")
#
#   full.names <- c("Exponential", "Weibull", "Log normal", "Log logistic",
#                   "Gompertz", "Genearlized gamma")
#
#
#   # Load in required packages ==================================================
#   require(survival)
#   require(flexsurv)
#   require(survminer)
#
#   # General set-up =============================================================
#   # Create vector exrtraploation time
#   t.d.extrapol <- t.y.extrapol * 365.25 # translate years in days
#   v.t.d.extrapol <- seq(0,t.d.extrapol) # create vector starting at 0
#
#   # Translate function arguments into arguments that can be used
#   params <- list(time.event = substitute(time.event),
#                  event = substitute(event),
#                  grouping = substitute(grouping),
#                  data.source = substitute(data.source),
#                  wgts = substitute(wgts))
#
#   # Generate Kaplan-Meier ======================================================
#
#   km.expr <- substitute(survfit(Surv(time = time.event, event = event) ~ grouping,
#                                 data = data.source, weights = wgts),
#                         params)
#   km <- eval.parent(km.expr)
#
#   # Fit parametric models =====================================================
#
#   # Exponential
#   fit1 <- eval.parent(
#     substitute(
#       flexsurvreg(Surv(time = time.event, event = event) ~ grouping,
#                   data = data.source, dist = "exponential",
#                   weights = wgts), params))
#   # Extract coefs
#   coef_fit1   <- fit1$coefficients        # parameter mean values
#   vcov_fit1   <- fit1$cov                 # parameter variance covariance
#   fit1.cov2cor <- cov2cor(vcov_fit1)
#
#   # Weibull
#   fit2 <- eval.parent(
#     substitute(
#       flexsurvreg(Surv(time = time.event, event = event) ~ grouping,
#                   data = data.source, dist = "weibull",
#                   weights = wgts), params))
#
#   coef_fit2   <- fit2$coefficients        # parameter mean values
#   vcov_fit2   <- fit2$cov                 # parameter variance covariance
#   fit2.cov2cor <- cov2cor(vcov_fit2)
#
#   # Lognormal
#   fit3 <- eval.parent(
#     substitute(
#       flexsurvreg(Surv(time = time.event, event = event) ~ grouping,
#                   data = data.source, dist = "lnorm",
#                   weights = wgts), params))
#
#   coef_fit3   <- fit3$coefficients        # parameter mean values
#   vcov_fit3   <- fit3$cov                 # parameter variance covariance
#   fit3.cov2cor <- cov2cor(vcov_fit3)
#
#   # Loglog
#   fit4 <- eval.parent(
#     substitute(
#       flexsurvreg(Surv(time = time.event, event = event) ~ grouping,
#                   data = data.source, dist = "llogis",
#                   weights = wgts), params))
#
#   coef_fit4   <- fit4$coefficients        # parameter mean values
#   vcov_fit4   <- fit4$cov                 # parameter variance covariance
#   fit4.cov2cor <- cov2cor(vcov_fit4)
#
#   # Gompertz
#   fit5 <- eval.parent(
#     substitute(
#       flexsurvreg(Surv(time = time.event, event = event) ~ grouping,
#                   data = data.source, dist = "gompertz",
#                   weights = wgts), params))
#
#   coef_fit5   <- fit5$coefficients        # parameter mean values
#   vcov_fit5   <- fit5$cov                 # parameter variance covariance
#   fit5.cov2cor <- cov2cor(vcov_fit5)
#
#   # Gengamma
#   fit6 <- eval.parent(
#     substitute(
#       flexsurvreg(Surv(time = time.event, event = event) ~ grouping,
#                   data = data.source, dist = "gengamma",
#                   weights = wgts), params))
#
#   coef_fit6   <- fit6$coefficients        # parameter mean values
#   vcov_fit6   <- fit6$cov                 # parameter variance covariance
#   fit6.cov2cor <- cov2cor(vcov_fit6)
#
#
#   # Export model distribution ==================================================
#   # switch(export.extrapol.dist,
#   #        exponential = st.model <<- st.exp,
#   #        weibull = st.model <<- st.wei,
#   #        lognorm = st.model <<- st.lognorm,
#   #        loglog = st.model <<- st.loglog,
#   #        gompertz = st.model <<- st.gomp.gr,
#   #        gen.gamma = st.model <<- st.gengam.gr)
#   #
#   # # Export model ==================================================
#   # switch(export.extrapol.dist,
#   #        exponential = fit <- fit1,
#   #        weibull = fit <- fit2,
#   #        lognorm = fit <- fit3,
#   #        loglog = fit <- fit4,
#   #        gompertz = fit <- fit5,
#   #        gen.gamma = fit <- fit6)
#   #
#   # # Export cov2cor ==================================================
#   # switch(export.extrapol.dist,
#   #        exponential = vcov_fit <- vcov_fit1,
#   #        weibull = vcov_fit <- vcov_fit2,
#   #        lognorm = vcov_fit <- vcov_fit3,
#   #        loglog = vcov_fit <- vcov_fit4,
#   #        gompertz = vcov_fit <- vcov_fit5,
#   #        gen.gamma = vcov_fit <- vcov_fit6)
#   #
#   # # Export coefit ==================================================
#   # switch(export.extrapol.dist,
#   #        exponential = coef_fit <- coef_fit1,
#   #        weibull = coef_fit <- coef_fit2,
#   #        lognorm = coef_fit <- coef_fit3,
#   #        loglog = coef_fit <- coef_fit4,
#   #        gompertz = coef_fit <- coef_fit5,
#   #        gen.gamma = coef_fit <- coef_fit6)
#
#   ## AIC/BIC ===================================================================
#
#   # AIC for parametric models
#   aicbic <- data.frame(distribution = full.names,
#                        AIC = c(fit1$AIC, fit2$AIC, fit3$AIC,
#                                fit4$AIC, fit5$AIC, fit6$AIC),
#                        BIC = c(BIC(fit1), BIC(fit2), BIC(fit3),
#                                BIC(fit4), BIC(fit5), BIC(fit6))) %>%
#     mutate(AIC_rank = rank(AIC),
#            BIC_rank = rank(BIC)) %>%
#     select(distribution, AIC, AIC_rank, BIC, BIC_rank)
#
#   ## General statistics of fitting =============================================
#
#
#
#   data.frame(distribution = c("Observed", full.names),
#              median_survival = c(summary(km.base)$table["median"][[1]],
#                                  )
#   )
#
#   ## Draw plots ================================================================
#
#   p <- ggsurvplot(km,
#                   censor.shape = "|",
#                   size = 1,
#                   conf.int = FALSE,
#                   risk.table = FALSE,
#                   axes.offset = TRUE,
#                   #break.time.by = break.time.at,
#                   #xlim = c(0, max.time.plot),
#                   pval = FALSE,
#                   #legend.labs = strata.name,
#                   ggtheme = theme_light(),
#                   #title = paste0("Kaplan-Meier curve", title.plot)
#                   )
#
#   p <- p$plot +
#     geom_line(data = data.frame(summary(fit1, t = 0:15000)),
#               aes(x = time, y = est))
#
#
#   mod.list <- list(mod.plot = p,
#                    km.base = km
#                    )
#
# }
#
#
# x <- SurvExtraProbFun2(time.event = time, event = status, data.source = lung,
#                  name.plot = "Tamara", export.extrapol.dist = "exponential",
#                  draw.plot = "yes", show.aic.bic = "yes", max.year.plot = 5)
# x$mod.plot
