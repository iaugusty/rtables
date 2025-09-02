# round_type utils get/set/add  ----

#' setter and getter tool for controlling the rounding method
#' 
#' setter and getter tool for controlling the rounding method ("iec" vs "sas") for rtables TableTree objects
#' @rdname round_type
#' 
#' @inheritParams gen_args
#' @param round_type Overall method of rounding for table object
#'
#'
set_round_type <- function(tt, round_type = c("iec", "sas")){
  round_type <- match.arg(round_type)
  # rather than working with attr, use similar approach as formatters::title_main
  attr(tt, "round_type") <- round_type
  tt
}


#' @rdname round_type
#' @inheritParams gen_args
#' @inheritParams set_round_type
#'
#'
add_round_type <- function(tt, round_type) {
  tt <- set_round_type(tt, round_type)
  tt
}


#' @rdname round_type
#' @inheritParams gen_args
#' @inheritParams set_round_type
#'
#' @returns rounding method assigned to the TableTree object
#' @export
#'
#' @examples
#' library(dplyr)
#' DM <- ex_adsl
#'
#' vals <- c(1.865, 2.985, 3.457)
#'
#' txtvals_iec <- mapply(format_value, x = vals, format = "xx.xx", round_type = "iec")
#' txtvals_sas <- mapply(format_value, x = vals, format = "xx.xx", round_type = "sas")
#'
#' DM <- DM %>% 
#'   mutate(new_var = case_when(ARMCD == "ARM A" ~ vals[1],
#'                              ARMCD == "ARM B" ~ vals[2],
#'                              ARMCD == "ARM C" ~ vals[3])) %>% 
#'   select(USUBJID, ARMCD, new_var)
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARMCD") %>% 
#'   analyze("new_var")
#'
#' tbl_iec <- build_table(lyt, DM, round_type = "iec")
#' tbl_sas <- build_table(lyt, DM, round_type = "sas")
#'
#' tbl_iec
#' tbl_sas 

get_round_type <- function(tt){
  round_type <- attr(tt, "round_type")
  round_type
}

