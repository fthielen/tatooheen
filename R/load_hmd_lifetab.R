#' Load HMD data for background survival
#' @description A wrapper function for `HMDHFDplus::readHMDweb` to load HMD data (both male and female) for the first time. If a file for the year (based on `Sys.Date()`) already exists, the download is skipped. Updates can be forced with `force_update = TRUE`.
#' See https://towardsdatascience.com/using-dotenv-to-hide-sensitive-information-in-r-8b878fa72020 how to set and load environment data for username and password.
#' @param path_to_file A character for path to file. Default is `"data/hmd/"`. File name will be given automatically.
#' @param force_update If `TRUE` update will be forced (i.e. new download).
#' @param country Country for which HMD data should be downloaded. Default is "NL".
#' @param hmd_user HMD username.
#' @keywords HMD
#' @export

load_hmd_lifetab <- function(path_to_file = "data/hmd/",
                             force_update = FALSE,
                             country = "NLD",
                             hmd_user = Sys.getenv("HMD_USER"),
                             hmd_password = Sys.getenv("HMD_PW")){
        if(!file.exists(paste0(here(path_to_file),
                               substr(Sys.Date(), 1, 4),
                               "_hmd_male.csv")) | force_update){
                death.prop.male <- HMDHFDplus::readHMDweb(CNTRY = country,
                                                          item = "mltper_1x1",
                                                          username = hmd_user,
                                                          password = Shmd_password);
                write.csv(death.prop.male, file = paste0(here(path_to_file),
                                                         substr(Sys.Date(), 1, 4),
                                                         "_hmd_male.csv"))
        }

        if(!file.exists(paste0(here(path_to_file),
                               substr(Sys.Date(), 1, 4),
                               "_hmd_female.csv"))){
                death.prop.male <- HMDHFDplus::readHMDweb(CNTRY = country,
                                                          item = "fltper_1x1",
                                                          username = hmd_user,
                                                          password = hmd_password)
                write.csv(death.prop.male, paste0(here(path_to_file),
                                                  substr(Sys.Date(), 1, 4),
                                                  "_hmd_female.csv"))
        }


}

