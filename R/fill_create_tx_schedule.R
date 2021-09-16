#' A wrapper function for `tatooheen::create_tx_schedule()`
#' @description A wrapper function for `tatooheen::create_tx_schedule()` for pre-specified `tx_regimens.xlsx` tables that need to be provided.
#' @param regimen_name The regimen name for which to fill `tatooheen::create_tx_schedule()`
#' @param regimen_phase The regimen phase for which to fill `tatooheen::create_tx_schedule()`
#' @param df A data.frame in the form of `tx_regimens.xlsx`
#' @param cyc_max The maximum number of cycles when no cycle duration is given (i.e. until progression in some instances)
#' @export

fill_create_tx_schedule <- function(regimen_name,
                                    regimen_phase,
                                    df = df_tx_regimen,
                                    cyc_max = 100){

        # Preparation
        df <- df %>%
                filter(regimen %in% regimen_name,
                       phase == regimen_phase)

        tx_names <- df %>%
                select(starts_with("drug_")) %>%
                unite(
                        "tx_names_c",
                        everything(),
                        na.rm = T,
                        remove = T,
                        sep = ","
                ) %>%
                pull %>%
                strsplit(., ",") %>%
                unlist()

        tx1_days <-  df %>%
                select(days_1) %>%
                pull %>% as.character()

        tx2_days <-  df %>%
                select(days_2) %>%
                pull %>% as.character()

        tx3_days <-  df %>%
                select(days_3) %>%
                pull %>% as.character()

        tx4_days <-  df %>%
                select(days_4) %>%
                pull %>% as.character()

        tx5_days <-  df %>%
                select(days_5) %>%
                pull %>% as.character()

        create_tx_schedule(cyc_dur_d = df$cyc_dur,
                           cyc_n = ifelse(is.na(df$max_cyc), cyc_max, df$max_cyc),
                           tx_names = tx_names,
                           phase_n = paste("Phase", regimen_phase),
                           tx1_d = as.numeric(unlist(strsplit(tx1_days, ","))),
                           tx2_d = as.numeric(unlist(strsplit(tx2_days, ","))),
                           tx3_d = as.numeric(unlist(strsplit(tx3_days, ","))),
                           tx4_d = as.numeric(unlist(strsplit(tx4_days, ","))),
                           tx5_d = as.numeric(unlist(strsplit(tx5_days, ","))))
}
