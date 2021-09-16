#' A wrapper function for `tatooheen::create_tx_schedule()` for pre-specified `tx_regimens.xlsx` tables
#' @param regimen_name The regimen name for which to fill `tatooheen::create_tx_schedule()`
#' @param regimen_phase The regimen phase for which to fill `tatooheen::create_tx_schedule()`
#' @param df A data.frame in the form of `tx_regimens.xlsx`
#' @param cyc_max The maximum number of cycles when no cycle duration is given (i.e. until progression)
#' @keywords CADTH
#' @export fill_create_tx_schedule

fill_create_tx_schedule <- function(regimen_name = "CarDex",
                                    regimen_phase = "I",
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
                pull

        tx2_days <-  df %>%
                select(days_2) %>%
                pull

        tx3_days <-  df %>%
                select(days_3) %>%
                pull

        tx4_days <-  df %>%
                select(days_4) %>%
                pull

        tx5_days <-  df %>%
                select(days_5) %>%
                pull

        # Fill
        create_tx_schedule(cyc_dur_d = df$cyc_dur,
                           cyc_n = ifelse(is.na(df$max_cyc), cyc_max, df$max_cyc),
                           tx1_d = if(
                                   !is.na(tx1_days)) as.numeric(unlist(strsplit(tx1_days, ","))),
                           tx2_d = if(
                                   !is.na(tx2_days)) as.numeric(unlist(strsplit(tx2_days, ","))),
                           tx3_d = if(
                                   !is.na(tx3_days)) as.numeric(unlist(strsplit(tx3_days, ","))),
                           tx_names = tx_names,
                           tx4_d = if(
                                   !is.na(tx4_days)) as.numeric(unlist(strsplit(tx4_days, ","))),
                           tx5_d = if(
                                   !is.na(tx5_days)) as.numeric(unlist(strsplit(tx5_days, ","))),

        )
}
