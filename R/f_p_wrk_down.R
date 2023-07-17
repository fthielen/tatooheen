#' A wrapper function to download CBS data for the CPI.
#' @param save_cbs_data Should CPI data be saved? Standard is TRUE
#' @param save_dir Directory where data should be saved. Standard is "data/cbs/"
#' @param file_name File name for the saved CPI data. Standard is "cbs_cpi.RDS
#' @keywords Generic, CBS, CPI
#' @export f_cbs_p_work_down

f_cbs_p_work_down <- function(
                save_cbs_data = TRUE,
                save_dir = "data/cbs/",
                file_name = "cbs_p_wrk.RDS"){

        require("cbsodataR")
        require("here")
        require("dplyr")

        df <- cbsodataR::cbs_get_data("84183NED") %>%
                cbsodataR::cbs_add_date_column() %>%
                cbsodataR::cbs_add_label_columns() %>%
                filter(Perioden_freq == "Y",
                       BedrijfstakkenBranchesSBI2008 %in% c("T001081")) %>%
                select(Perioden_label,
                       PrijsindexArbeid_1, LoonkostenPerGewerktUur_3)

        if (save_cbs_data) {
                if (!dir.exists(here::here(save_dir))) {
                        dir.create(here::here(save_dir), recursive = T)
                }

                readr::write_rds(x = df, file = here(paste0(save_dir, file_name)))
        }

        df

}
