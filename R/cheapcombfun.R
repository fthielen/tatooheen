#' A function to calculate the cheapest drug combination given a data.frame with combination and prices
#' @param target The target dose
#' @param combi A complete dataframe with combinations and prices
#' @param total_max A maximum of possible combinations. Default = 10
#' @param only_total If `TRUE`, only display the total costs. If `FALSE`, return all information.
#' @param vial_share If `TRUE`, vial share is allowed.
#' @keywords CADTH
#' @export CheapCombFun
#' @examples
#' # Data frame of possible doses and prices per dose
#' df_combi <- data.frame(drug = rep(LETTERS[1], 3),
#'                        dose = c(10, 20, 50),
#'                        price_dose = c(11.39, 20, 56.96))
#'
#' # Return only total costs of cheapest option
#' tatooheen::CheapCombFun(target = 92.5, combi = df_combi, only_total = T)
#'
#' # Return more information as a list
#' res <- tatooheen::CheapCombFun(target = 92.5, combi = df_combi, only_total = F)

CheapCombFun <- function(target,
                         combi,
                         total_max = 10,
                         only_total = TRUE,
                         vial_share = FALSE){

        if(vial_share){
                p_dose <- combi %>%
                        mutate(p_dose = price_dose / dose) %>%
                        summarise(min = min(p_dose)) %>%
                        pull

                p_dose * target
        }else{
                # Silence dplyr summarise information
                options(dplyr.summarise.inform = FALSE)

                # Section I: Loading required packages ####
                require("RcppAlgos")
                require("tidyverse")

                # Section II: preparing function input
                # Define number from which to choose and order them if necessary
                possible_mg <- sort(combi$dose)

                # Define target value
                target_mg <- target

                # Define new target if needed

                new_target <- ifelse(target_mg %% min(possible_mg) == 0,
                                     target_mg,
                                     ceiling(target_mg / min(possible_mg)) * min(possible_mg))

                # Stop if target can never be reached with total_max
                if(new_target / max(possible_mg) > total_max) stop("Target cannot be reached with total_max.")

                # Define maximum of combinations based on the combining the smallest unit to reach target
                max_combs <- min(new_target / min(possible_mg), total_max)

                try(if(max_combs %% 1 !=0) stop("AllCombFunFast: Minimum combination is not multiple of target. Check if target can be reached without decimals."))

                try(if(max_combs > total_max) stop("AllCombFunFast: Combination may not be reachable with setting for total_max."))

                # Section III: find all possible combinations ####
                # Make matrix with all possible combinations of possible mgs
                all_combs <- comboGeneral(c(0, possible_mg), max_combs, TRUE)

                # Only keep colums that reach target
                reduced_combs <- all_combs[rowSums(all_combs) == new_target, , drop = FALSE] # to prevent from vectorising if only one combination is possible


                # Keep only unique combinations
                unique_combs <- t(reduced_combs)
                colnames(unique_combs) <- paste0("combi_", 1:ncol(unique_combs))

                # Convert to data frame for easy use
                df_combis <- unique_combs %>% as.data.frame() %>%
                        pivot_longer(everything(),
                                     names_to = "combi",
                                     values_to = "dose") %>%
                        filter(dose > 0) %>%
                        group_by(combi, dose) %>%
                        summarise(freq = n()) %>%
                        ungroup()

                # Create cost df
                df_cost <- left_join(df_combis, combi, by = "dose") %>%
                        mutate(cost_dosing = freq * price_dose) %>%
                        group_by(combi) %>%
                        mutate(cost_combi = sum(cost_dosing)) %>%
                        ungroup %>%
                        mutate(cost_rank = min_rank(cost_combi))

                # Select cheapest option for reference df
                df_cheapest <- df_cost %>%
                        filter(cost_rank == 1)

                # Store results
                ifelse(only_total,
                       res <- df_cheapest[[1, "cost_combi"]],
                       res <- list(unique_combis = unique_combs,
                                   df_combis = df_combis,
                                   df_cost = df_cost,
                                   df_cheapest = df_cheapest,
                                   waste = new_target - target))
                res
        }
}





#
#
# xx <- CheapCombFun(target = 15,
#              combi = df_tx_prices[df_tx_prices$inn == "bortezomib", ], only_total = T,
#              vial_share = F)
# xx
# xx$df_cost %>%
#   mutate(p_dose = price_dose / dose) %>%
#   arrange(p_dose)
#
#
# res_doxo$df_cost %>%
#   group_by(combi) %>%
#   summarise(d = sum(dose * freq))
# es_doxo$df_cheapest %>% mutate(dose * freq)
#
#
# p_dose <- combi %>%
#   mutate(p_dose = price_dose / dose) %>%
#   summarise(min = min(p_dose)) %>%
#   pull
