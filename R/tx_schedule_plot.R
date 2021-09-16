#' A function to visualise treatment schedules created with for `tatooheen::create_tx_schedule()`
#' @param data The treatment schedule from `tatooheen::create_tx_schedule()`
#' @param cycle A selection of cycles to display
#' @keywords CADTH
#' @export tx_schedule_plot


tx_schedule_plot <- function(data, cycles){


        data %>%
                pivot_longer(
                        cols = -c(days:phase),
                        names_to = "drug",
                        values_to = "give"
                ) %>%
                mutate(give = ifelse(give == 0, NA, 1)) %>%
                filter(cycle %in% cycles) %>%
                ggplot(data = .,
                       aes(
                               x = days,
                               y = factor(drug),
                               fill = factor(give)
                       )) +
                geom_tile(color = "white", size = 1.1) +
                scale_fill_brewer(palette = "Dark2", na.value = "gray") +
                #coord_equal() +
                theme_bw() +
                theme(
                        #axis.text.x = element_blank(),
                        axis.line = element_blank(),
                        axis.title.y = element_blank(),
                        #axis.title.x = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        #panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_blank(),
                        panel.background = element_blank(),
                        legend.position = "none",
                        aspect.ratio = 1 / 13
                ) +
                scale_x_continuous(expand = c(0, 0),
                                   breaks = scales::pretty_breaks(10)) +
                facet_wrap(. ~ cycle,
                           scales = "free",
                           ncol = 1,
                           drop = TRUE)

}
