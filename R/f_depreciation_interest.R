#' A function to calculate the costs of medical equipment
#' @description
#' A function to calculate the costs of medical equipment based on Section 3.3 of the Dutch EE guideline; k = annual depreciation and interest expense [jaarlijkse afschrijvings- en rentekosten]
#' @param v_replace_val V: vervangingswaarde; replacement value
#' @param r_salvage_val R: restwaarde; salvage value
#' @param n_amortisation_period N: afschrijvingstermijn,; amortization period
#' @param i_interest_rt i: rentepercentage; interest rate
#' @keywords Generic, costs equipment

#' @export f_depreciation_interest

f_depreciation_interest <- function(
                v_replace_val,
                r_salvage_val,
                n_amortisation_period = 10,
                i_interest_rt = 0.042){

        # Annuity factor

        a_annuity_fct <- (1 / i_interest_rt) * (1 - (1 / (1 + i_interest_rt)^n_amortisation_period))
        a_annuity_fct

        k_annual_depr_int_exp <- (v_replace_val - (r_salvage_val /
                                                           (1 + i_interest_rt)^n_amortisation_period)
                                  ) / (a_annuity_fct)
        c(a_annuity_fct, k_annual_depr_int_exp)
}

f_depreciation_interest(v_replace_val = 100, r_salvage_val = 10, n_amortisation_period = 10, i_interest_rt = 0.042)
