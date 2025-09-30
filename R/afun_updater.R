### look into match_extra_args in tt_dotabulation

# taken from tern
extra_afun_params <- list(
  .N_col = integer(),
  .N_total = integer(),
  .N_row = integer(),
  .df_row = data.frame(),
  .var = character(),
  .ref_group = character(),
  .ref_full = vector(mode = "numeric"),
  .in_ref_col = logical(),
  .spl_context = data.frame(),
  .all_col_exprs = vector(mode = "expression"),
  .all_col_counts = vector(mode = "integer")
)

# taken from tern - except from restricting to non-symbolics only
# this is to cover cases where .ref_group is not defined, then it is a symbolic
retrieve_extra_afun_params <- function(extra_afun_params) {
  envir <- parent.frame()
  symbolics <- sapply(extra_afun_params, function(x) {
    typeof(envir[[x]]) %in% c("language", "symbol")
  })
  extra_afun_params <- extra_afun_params[!symbolics]

  out <- list()
  for (extra_param in extra_afun_params) {
    out <- c(out, list(get(extra_param, envir = envir)))
  }

  setNames(out, extra_afun_params)
}

# taken from tern
afun_ext_add_fun_params <- function(original_func) {
  extended_func <- original_func
  formals(extended_func) <- c(formals(original_func), extra_afun_params)
  # return this function
  extended_func
}


upd_fmt_args <- function(args, .spl_context, .format_col) {
  #### this is the piece for getting format from .format_col
  splc <- .spl_context
  parent_df <- splc$full_parent_df[[NROW(splc)]]

  # get format_col from .spl_context and update .formats
  if (is.character(.format_col) && length(.format_col) == 1 && .format_col %in% names(parent_df)) {
    args[[".formats"]] <- unlist(unique(parent_df[[.format_col]]))
  } else {
    args[[".formats"]] <- .format_col
  }
  # now that .formats is updated, ensure to set formatting via afun to TRUE
  args[["fmt_afun"]] <- TRUE

  return(args)
}


update_afun <- function(.format_col, original_func) {
  updated_afun1 <- afun_ext_add_fun_params(original_func)
  # note that this function will be used in the call inside updated_afun


  # to avoid using the same code in 2 blocks
  corepartall <- quote({
    .additional_fun_parameters <- retrieve_extra_afun_params(names(extra_afun_params))

    # Get original arguments --- critical here is envir parent.frame(3)
    first_arg <- get("dat", envir = parent.frame(3))

    # .additional_fun_parameters is passed twice in order to work with tern functions
    # this is in order to properly execute following step in tern afuns
    # extra_afun_params <- retrieve_extra_afun_params(
    # names(dots_extra_args$.additional_fun_parameters)
    # )
    args <- c(
      list(first_arg, ..., ".additional_fun_parameters" = .additional_fun_parameters),
      .additional_fun_parameters
    )

    args <- upd_fmt_args(args, .spl_context, .format_col)
    # Call original function with updated args
    result <- do.call(updated_afun1, args)
    result
  })

  if (.takes_df(original_func)) {
    # first argument is df
    updated_afun <- function(df, ...,
                             .N_col,
                             .N_total,
                             .N_row,
                             .df_row,
                             .var,
                             .ref_group,
                             .ref_full,
                             .in_ref_col,
                             .spl_context,
                             .all_col_exprs,
                             .all_col_counts) {
      eval(corepartall)
    }
  } else {
    # first arg is x
    updated_afun <- function(x, ...,
                             .N_col,
                             .N_total,
                             .N_row,
                             .df_row,
                             .var,
                             .ref_group,
                             .ref_full,
                             .in_ref_col,
                             .spl_context,
                             .all_col_exprs,
                             .all_col_counts) {
      eval(corepartall)
    }
  }
  # return this function
  return(updated_afun)
}
