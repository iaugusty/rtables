#' toolkit for analyze with no auto formatting feature
#'
#' These are key functions for no auto formatting handling with the analyze function.\cr
#' End users can find more details on how to use the `no auto formatting feature` in vignette ....
#'
#' @name no_auto_fmt
#' @rdname no_auto_fmt
#' @return Various, but not described here.
NULL

#' @order 1
#' @rdname no_auto_fmt
#' @export
no_auto_fmt <- structure(list(), class = "no_auto_fmt")


#' @noRd

format_spec_type <- function(format) {
  if (is.null(format)) {
    type <- "null"
  } else if (length(format) == 1 && is.null(names(format))) {
    if (is_valid_format(format) || is.function(format)) {
      type <- "format spec"
    } else {
      type <- "format variable name"
    }
  } else if (!is.list(format) || is.list(format) && any(sapply(format, is.function))) {
    type <- "format spec"
  } else if (is.list(format)) {
    type <- "list analysis variable name"
  } else {
    stop("format_spec_type issue: inproper format input")
  }
  return(type)
}

#' @noRd
#'
get_formatvec <- function(format, datcol, dfpart, ncrows, nms) {
  formatvec <- NULL

  if (is.null(format)) {
    return(formatvec)
  }

  fmt_spec_type <- format_spec_type(format)
  if (fmt_spec_type == "list analysis variable name") {
    # check that format is a named list (names are the variable names from vars)
    if (is.null(names(format))) {
      stop("when format is a list it should be a named list")
    }

    if (!(datcol %in% names(format)) && !("default" %in% names(format))) {
      stop(paste("No format specification found for variable: ", datcol))
    }

    format_datcol <- datcol
    if (!(datcol %in% names(format)) && "default" %in% names(format)) {
      format_datcol <- "default"
    }
    format <- format[[format_datcol]]
  } else if (fmt_spec_type == "format variable name") {
    if (!format %in% names(dfpart)) {
      stop("Format specification issue: Variable ", format, " is not present in input dataset df.")
    }
    format <- unlist(unique(dfpart[[format]]))
  }

  if (is.function(format)) {
    format <- list(format)
  }
  if (is.null(names(format))) {
    formatvec <- rep(format, length.out = ncrows)
  } else {
    ### KEY assumption: names of the rows from verticalsection start with the name of the statistic, (possibly followed with ".")
    ### method will not work when not all columns are from the same statistics (eg relative risk column)
    ###
    if (!is.null(names(nms))) nms <- names(nms)
    nmf <- sub("\\..*", "", nms)
    xx <- match(nmf, names(format))
    if (any(is.na(xx))) {
      nf <- unique(which(is.na(xx)))
      stop(paste("No format provided for statistic(s): "), paste(nmf[nf], collapse = ","))
    }
    formatvec <- format[xx]
  }

  if (!is.null(formatvec)) {
    cond <- sapply(formatvec, is_valid_format)
    invalid <- unique(unlist(formatvec[!cond]))
    if (length(invalid) > 0) {
      stop(paste("Following format specifications are invalid: ", paste(invalid, collapse = "; ")))
    }
  }
  formatvec
}



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


#' @noRd
#' @param extra_afun_params (`list`)\cr list of additional parameters (`character`) to be
#'   retrieved from the environment. Curated list is present in [rtables::additional_fun_params].
#' #@rdname no_auto_fmt
#' #@order 14
#' #@keywords internal

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

#' @noRd
#' @inheritParams gen_args
#' @inheritParams lyt_args
#' @order 15
#' @keywords internal
#' #@rdname no_auto_fmt
#'
afun_ext_add_fun_params <- function(afun) {
  extended_func <- afun
  if (".spl_context" %in% names(formals(afun))) {
    extended_func <- afun
  } else {
    formals(extended_func) <- c(formals(afun), extra_afun_params)
  }
  # return this function
  extended_func
}




#' @inheritParams gen_args
#' @inheritParams lyt_args
#' @order 2
#' @rdname no_auto_fmt
#'
#' @export
update_afun_no_auto <- function(afun) {
  # update afun (only in some cases)
  updated_afun1 <- afun_ext_add_fun_params(afun)
  # note that function updated_afun1 will be used in the call inside corepartall

  # corepartall body code to avoid using the same code in 2 blocks
  # this part of code deals with updating .formats in each facet
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

    # Call afun (updated version)
    result <- do.call(updated_afun1, args)
    result
  })

  if (.takes_df(afun)) {
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
