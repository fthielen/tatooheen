#' @title Survival Analysis Plotting Function
#' @name f_flexsurv_plots
#' @description This function plots Kaplan-Meier and parametric survival curves.
#' @param fit A list of fitted survival models. Default is ls_obj.
#' @param show_dists A character vector specifying which distributions to show. Default is "all".
#' @param time_plot A numeric vector specifying the time points for the plot. Default is 0:6000.
#' @param conf_int_km A logical indicating whether to include confidence intervals for the Kaplan-Meier curve. Default is FALSE.
#' @param scale_time A numeric value to scale time from days to years. Default is 365.25.
#' @return A ggplot2 object.
#' @examples
#' f_flexsurv_plots()
#' @author F. Thielen

# Load required libraries
library(flexsurv)
library(survminer)
library(ggfortify)
library(tidyverse)

# Fit Kaplan-Meier and parametric survival models
km <- survival::survfit(formula = Surv(futime, fustat) ~ 1, data = ovarian)
fitw <- flexsurvreg(formula = Surv(futime, fustat) ~ 1, data = ovarian, dist="weibull")
fitlogn <- flexsurvreg(formula = Surv(futime, fustat) ~ 1, data = ovarian, dist="lognorm")
fitgamma <- flexsurvreg(formula = Surv(futime, fustat) ~ 1, data = ovarian, dist="gamma")

# Create a list of fitted models
ls_obj <- list(km, fitw, fitlogn, fitgamma)

# Define function to plot survival curves
f_flexsurv_plots <- function(fit = ls_obj,
                             show_dists = "all",
                             time_plot = 0:6000,
                             conf_int_km = F,
                             scale_time = 365.25) {
        # Written by F. Thielen

        # Initialize variables
        n_plots <- length(ls_obj)
        ls_names <- vector(length = n_plots)
        ls_names[1] <- "KM"

        # Loop to get distribution names
        for(i in 2:n_plots){
                ls_names[i] <- ls_obj[[i]]$call$dist
        }

        # Create the base plot
        ggplot2::autoplot(ls_obj[[1]],
                          conf.int = conf_int_km,
                          conf.int.alpha = 0.1,
                          censor.alpha = 0.4,
                          xlim = c(min(time_plot), max(time_plot)),
                          ylim = c(0, 1), col = ls_names[1]) +

                # Add axis scaling and labels
                scale_x_continuous(breaks = seq(from = 0,
                                                to = max(time_plot),
                                                by = scale_time),
                                   labels = function(x) round(x/scale_time, 0)) +

                # Add lines for each parametric model
                geom_line(data = data.frame(
                        summary(object = ls_obj[[2]],
                                type = "survival",
                                t = time_plot,
                                ci = FALSE)[[1]][,1:2]),
                        aes(x = time, y = est, col = ls_names[2])) +

                # Add more lines if more models are available
                {if (n_plots >= 3)
                        geom_line(data = data.frame(
                                summary(object = ls_obj[[3]],
                                        type = "survival",
                                        t = time_plot,
                                        ci = FALSE)[[1]][,1:2]),
                                aes(x = time, y = est, col = ls_names[3])) } +

                {if (n_plots >= 4)
                        geom_line(data = data.frame(
                                summary(object = ls_obj[[4]],
                                        type = "survival",
                                        t = time_plot,
                                        ci = FALSE)[[1]][,1:2]),
                                aes(x = time, y = est, col = ls_names[4])) } +

                # Add title and labels
                labs(title = paste0("Empirical and extrapolated survival"),
                     x = "",
                     y = "",
                     col = "Parametric distributions:") +

                # Apply themes
                theme_light() +
                theme(plot.title = element_text(size = 10),
                      legend.position = "right",
                      legend.title = element_text(size = 5),
                      legend.text = element_text(size = 5)
                )

}

# Call the function to generate the plot
f_flexsurv_plots()

