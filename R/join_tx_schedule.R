#' Join phases created with `tatooheen::create_tx_schedulde` or `tatooheen::fill_create_tx_schedulde`
#' @param tx_schedule_ph1 A treatment schedule for phase I
#' @param tx_schedule_ph2 A treatment schedule for phase II
#' @param tx_schedule_ph3 A treatment schedule for phase III
#' @param tx_schedule_ph4 A treatment schedule for phase IV
#' @param tx_schedule_ph5 A treatment schedule for phase V
#' @keywords CADTH
#' @export join_tx_schedule

join_tx_schedule <- function(tx_schedule_ph1,
                             tx_schedule_ph2,
                             tx_schedule_ph3 = NULL,
                             tx_schedule_ph4 = NULL,
                             tx_schedule_ph5 = NULL){
        bind_rows(tx_schedule_ph1,
                  tx_schedule_ph2) %>%

                {if(!is.null(tx_schedule_ph3)) bind_rows(., tx_schedule_ph3) else . } %>%
                {if(!is.null(tx_schedule_ph4)) bind_rows(., tx_schedule_ph4) else . } %>%
                {if(!is.null(tx_schedule_ph5)) bind_rows(., tx_schedule_ph5) else . } %>%
                replace(is.na(.), 0) %>%
                group_by(phase, cycle) %>%
                mutate(cycle = cur_group_id()) %>%
                ungroup()
}
