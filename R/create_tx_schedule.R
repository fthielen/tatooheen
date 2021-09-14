#' Create a drug treatment schedule data.frame
#' @param cyc_dur_d The cycle duration in days
#' @param cyc_n The number of cycles
#' @param tx.._d Days of cycle at which drug is administered (up to 5 drugs can be added)
#' @param tx_names A character vector with the respective treatment names
#' @param phase_n The phase if treatment changes between different phases and more than one phase is needed.
#' @export create_tx_schedule

create_tx_schedule <- function(cyc_dur_d,
                               cyc_n,
                               tx1_d = 0,
                               # days of tx per cycle in phase
                               tx2_d = 0,
                               tx3_d = 0,
                               tx4_d = 0,
                               tx5_d = 0,
                               tx_names = c("foo", "bar"),
                               phase_n = "Phase I") {
        x <- data.frame(
                days = seq(from = 1, to = cyc_dur_d * cyc_n),
                cycle = rep(1:cyc_n, each = cyc_dur_d),
                phase = phase_n,
                tx1 = 0,
                tx2 = 0,
                tx3 = 0,
                tx4 = 0,
                tx5 = 0
        )

        keep_cols <- 3 + length(tx_names)

        x <- x %>%
                group_by(cycle) %>%
                mutate(
                        tx1 = ifelse(row_number() %in% tx1_d, 1, 0),
                        tx2 = ifelse(row_number() %in% tx2_d, 1, 0),
                        tx3 = ifelse(row_number() %in% tx3_d, 1, 0),
                        tx4 = ifelse(row_number() %in% tx4_d, 1, 0),
                        tx5 = ifelse(row_number() %in% tx5_d, 1, 0)
                ) %>%
                ungroup()

        x <- x[, 1:keep_cols]

        names(x) <- c("days", "cycle", "phase", tx_names)

        x

}
