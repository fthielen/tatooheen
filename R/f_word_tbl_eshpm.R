#' A wrapper function for flextable related funtctions
#' @param df A data.frame of tibble
#' @param cpt The table caption.
#' @param tb_lbl A table label used for bookdown documents. Default is the chunk label. If no chunk label provided add own here.
#' @param col_names New column names. If left empty (NULL) names will be taken from `df`.
#' @param f_format_dbl If TRUE (default) numbers in table will be formatted.
#' @param f_digi Number of digits when `f_format_dbl` is TRUE.
#' @param flex_layout Used for `flextable::set_table_properties`. 'autofit' or 'fixed' algorithm. Default to 'autofit'.
#' @export f_word_tbl_eshpm

f_word_tbl_eshpm <- function(df,
                      cpt = "",
                      tb_lbl = knitr::opts_current$get()$label, # Default is the chunk label. If no chunk label provided add own here.l
                      col_names = NULL,
                      f_format_dbl = TRUE,
                      f_digi = 2,
                      flex_layout = "autofit"){

        require("flextable")

        df %>%
                flextable::flextable() %>%
                {if (!is.null(col_names)) flextable::set_header_labels(values = col_names) else .} %>%
                {if (f_format_dbl) flextable::colformat_double(x = ., digits = f_digi) else .} %>%
                flextable::set_caption(caption = cpt,
                                       autonum = officer::run_autonum(seq_id = "tab",
                                                                      bkm = tb_lbl)) %>%
                flextable::set_table_properties(layout = flex_layout#, width = 1
                ) %>%
                bg(part = "header", bg = "#801A99") %>%
                color(part = "header", color = "white") %>%
                bold(part = "header", bold = T) %>%
                fontsize(part = "all", size = 9) %>%
                theme_box()
}
