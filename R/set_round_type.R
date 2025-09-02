set_round_type <- function(tt, round_type = c("iec", "sas")){
  round_type <- match.arg(round_type)
  # rather than working with attr, use similar approach as formatters::title_main
  attr(tt, "round_type") <- round_type
  tt
}

get_round_type <- function(tt){
  round_type <- attr(tt, "round_type")
  round_type
}

add_round_type <- function(tt, round_type) {
  tt <- set_round_type(tt, round_type)
  tt
}