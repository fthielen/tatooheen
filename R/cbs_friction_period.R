#' A wrapper function to download CBS data for the friction period.
#' @param years Years for which friction data should be kept. Usually only one year, but several are possible (e.g. 2014:2020)
#' @param industry Industry for which friction period should be calculated. Usually "All economic acvitivies" = "T001081" (default). Check downloaded data with cbsodataR::cbs_add_label_columns() for an other industries.
#' @param folderpath The path to were the data should be dowloaded. Best specified with here::here().
#' @param download Whether data should be downloaded and stored. Default is `TRUE`. If `FALSE`, data will not be stored but need to be fetched every time.
#' @param yeardays Time in days to convert years to days. Default is 365.25 days for one year.
#' @keywords Generic, CBS
#' @export cbs_friction_period

cbs_friction_period <- function(years,
                            industry = "T001081",
                            folderpath = NULL,
                            download = T,
                            yeardays = 365.25,
                            ...) {

        # Required libraroes
        require("dplyr")
        require("cbsodataR")
        require("lubridate")

        # Check path input parameter
        if (download & is.null(folderpath))
                stop("Specify folderpath for downloaded CBS data or set download to FALSE")

        # Set path
        path1 <- folderpath
        path2 <- paste0(folderpath, "/data_fric_per.Rds")

        # Create directory if not existing
        if (download & !dir.exists(path1))
                dir.create(path1, recursive = TRUE)

        # Download data
        if (download) {
                if (!file.exists(path2)) {
                        dat.cbs <- cbsodataR::cbs_get_data("80472NED")
                        saveRDS(dat.cbs, file = path2)
                } else {
                        dat.cbs <- readRDS(path2)
                }

        } else{
                dat.cbs <- cbsodataR::cbs_get_data("80472NED")
        }

        # Add column labels and dates
        dat <- dat.cbs %>%
                cbsodataR::cbs_add_label_columns() %>%
                cbsodataR::cbs_add_date_column()

        # Check for years input parameter
        if(any(!years %in% year(
                dat[dat$Perioden_freq == "Y", "Perioden_Date"][[1]])))
                stop(paste0("No data available for selected year(s). Latest year
                            available is ",
                            max(year(
                                    dat[dat$Perioden_freq == "Y", "Perioden_Date"][[1]])),
                            ". Please change in years in FrictionPeriods()"
                            ))


        # Calculate
        res <- dat %>%
                filter(Perioden_freq == "Y",
                       Bedrijfskenmerken %in% c("T001081")) %>%
                mutate(
                        Year = lubridate::year(Perioden_Date),
                        Friction_period_days = yeardays /
                                (VervuldeVacatures_3 / OpenstaandeVacatures_1) +
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
                rename(Filled_vacancies = 2,
                       Open_vacancies = 3) %>%
                data.frame()
        res
}


#Test
# x <- cbs_friction_period(folderpath = here::here("data_test/friction_period/"),
#                                              download = T,
#                                              years = 2020)
# x


