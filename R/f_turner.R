f_turner_live <- function(orig_val = 200000,
                          from_year = 2006,
                          to_year = 2016,
                          from_country = "Vietnam",
                          conv_type = "tradable",
                          output_currency = "US$",
                          exchange_period = "year_average"){

        library("priceR")
        library("wbstats")
        library("countrycode")
        library("dplyr")

        # Tests -------------------------------------------------------------------
        output_currency_opts <- c("I$", "US$")
        conv_type_opts <- c("tradable", "non-tradable", "cpi")
        exchange_period_opts <- c("start", "end", "year_average")

        # Currency conversion ------------------------------------------------------
        use_year <- switch (conv_type,
                            "tradable" = from_year,
                            "non-tradable" = to_year)

        # International dollar
        if(output_currency == output_currency_opts[1]){

                conv_rt <- 1 / wb_data(indicator = "PA.NUS.PPP",
                                       country = from_country,
                                       start_date = use_year,
                                       end_date = use_year)$PA.NUS.PPP[1]
                if (length(conv_rt) == 0) stop("foo")
        }

        df_to_id <- wb_data(indicator = "PA.NUS.PPP",
                            country = from_country,
                            start_date = from_year,
                            end_date = to_year)[, c("date", "PA.NUS.PPP")] %>%
                mutate("to_id" = 1 / PA.NUS.PPP)

        # US dollar
        if(output_currency == output_currency_opts[2]){

                # Translate country name to ISO currency name
                name_currency <- filter(
                        countrycode::codelist, country.name.en == from_country)$iso4217c


                # Set time range for historic exchange rates
                if (exchange_period == exchange_period_opts[1]) {
                        exchange_period_start <- exchange_period_end <- paste0(use_year, "-01-01")
                }

                if (exchange_period == exchange_period_opts[2]) {
                        exchange_period_start <- exchange_period_end <- paste0(use_year, "-12-31")
                }

                if (exchange_period == exchange_period_opts[3]) {
                        exchange_period_start <- paste0(use_year, "-01-01")
                        exchange_period_end <- paste0(use_year, "-12-31")
                }

                # Get historic exchange rates
                conv_rt <- priceR::historical_exchange_rates(
                        from = name_currency,
                        to = "USD",
                        start_date = exchange_period_start,
                        end_date = exchange_period_end)[, 2]

                if (length(conv_rt) > 1) {
                        conv_rt <- mean(conv_rt)
                }
        }




        df_to_usd <- priceR::historical_exchange_rates(
                from = name_currency,
                to = "USD",
                start_date = exchange_period_start,
                end_date = "2016-12-31") %>%
                mutate(yr = lubridate::year(date)) %>%
                group_by(yr) %>%
                rename("to_usd" = 2) %>%
                summarise(to_usd = mean(to_usd))


        # Inflation ---------------------------------------------------------------------
        use_country <- switch (conv_type,
                               "tradable" = "USA",
                               "non-tradable" = from_country,
                               "cpi" = from_country)

        df_cpi <- wb_data(indicator = "FP.CPI.TOTL",
                          country = use_country,
                          start_date = from_year,
                          end_date = to_year)


        cpi_rt <- (last(df_cpi$FP.CPI.TOTL) / first(df_cpi$FP.CPI.TOTL))[1]

        df_cpi <- wb_data(indicator = "FP.CPI.TOTL",
                          country = c(use_country, from_country),
                          start_date = from_year,
                          end_date = to_year) %>%
                select(country, date, FP.CPI.TOTL) %>%
                pivot_wider(names_from = country, values_from = FP.CPI.TOTL) %>%
                rename("yr" = 1, "cpi_us" = 2, "cpi_local" = 3)

        # Calculation -------------------------------------------------------------
        # Tradable formula
        if (conv_type == "tradable") {
                res <- orig_val * conv_rt * cpi_rt
        }

        # Non-tradable formula
        if (conv_type == "non-tradable") {
                res <- orig_val * cpi_rt * conv_rt
        }

        res
}
