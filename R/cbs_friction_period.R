#' A wrapper function to download CBS data for the friction period.
#' @param years Years for which friction data should be kept. Usually only one year, but several are possible (e.g. 2014:2020)
#' @param industry Industry for which friction period should be calculated. Usually "All economic activities" = "T001081" (default). Check downloaded data with cbsodataR::cbs_add_label_columns() for an other industries.
#' @param folderpath The path to were the data should be downloaded. Best specified with here::here().
#' @param type Whether data should be downloaded and stored ("download"), only shown ("show") or read from previously downloaded ("read"). Default is "download".
#' @param yeardays Number of days used to convert years to days. Default is 365.25 days for one year.
#' @keywords Generic, CBS
#' @export cbs_friction_period

cbs_friction_period <- function(years,
                                industry = "T001081",
                                folderpath = NULL,
                                type = "download",
                                yeardays = 365.25,
                                ...) {

        # Required libraries
        require("dplyr")
        require("cbsodataR")
        require("lubridate")

        possible_types <- c("download", "show", "read")

        # Set path
        path1 <- folderpath
        path2 <- paste0(folderpath, "/data_fric_per.RDS")

        # # Create directory if not existing
        # if (type == "download" & is.null(folderpath)){
        #         dir.create(path1, recursive = TRUE)
        # }

        # Checks
        if (type == possible_types[1] & is.null(folderpath)) {
                stop("Specify `folderpath` for downloaded CBS data.")
        }

        if (type == possible_types[3] & !file.exists(path2)) {
                stop("Data cannot be loaded because file does not exist. Try `type = 'download'` first.")
        }


        if (type == possible_types[3]) {
                res <- readr::read_rds(path2)
                return(res)
        }

        if (type %in% possible_types[1:2]) {

                dat <- cbsodataR::cbs_get_data("80472NED") %>%
                        # Add column labels and dates
                        cbsodataR::cbs_add_label_columns() %>%
                        cbsodataR::cbs_add_date_column()

                # Check for years input parameter
                if(any(!years %in% year(
                        dat[dat$Perioden_freq == "Y",
                            "Perioden_Date"][[1]])))
                        stop(
                                paste0(
                                        "No data available for selected year(s). Latest year
                                available is ",
                                        max(
                                                year(
                                                        dat[dat$Perioden_freq == "Y",
                                                            "Perioden_Date"][[1]])),
                                        ". Please change in years in FrictionPeriods()"
                                )
                        )

                # Calculate
                res <- dat %>%
                        filter(Perioden_freq == "Y",
                               Bedrijfskenmerken %in% c("T001081")) %>%
                        mutate(
                                Year = lubridate::year(Perioden_Date),
                                Friction_period_days = yeardays /
                                        (VervuldeVacatures_3 /
                                                 OpenstaandeVacatures_1) +
                                        4 * 7,
                                Friction_period_wks = Friction_period_days / 7
                        ) %>%
                        distinct(
                                Year,
                                VervuldeVacatures_3,
                                OpenstaandeVacatures_1,
                                Friction_period_days,
                                Friction_period_wks
                        ) %>%
                        filter(Year %in% years) %>%
                        rename(Filled_vacancies = "VervuldeVacatures_3",
                               Open_vacancies = "OpenstaandeVacatures_1") %>%
                        data.frame()
        }

        if (type == possible_types[1]) {

                if (!dir.exists(path1)) {
                        dir.create(path1, recursive = TRUE)
                }

                saveRDS(res, file = path2)
                res
        }

        if (type == possible_types[2]) {
                res
        }
}


#Test
# x <- cbs_friction_period(folderpath = here::here("data_test/friction_period/"),
#                                              download = T,
#                                              years = 2020)
# x


