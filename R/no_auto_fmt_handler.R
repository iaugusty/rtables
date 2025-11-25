#' analyze no auto formatting feature
#'
#' These are internal methods for no auto formatting handling with the analyze function.\cr
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

#' @inheritParams gen_args
#' @inheritParams lyt_args
#' @order 5
#' @keywords internal
#' @rdname no_auto_fmt
#'
#'
no_auto_fmt_handler <- function(extra_args,
                                format,
                                afun,
                                vars) {
  .stats <- extra_args[[".stats"]]
  fmt_spec_type <- format_spec_type(format)

  # perform some basic checks on format
  format_spec_check(format, .stats, vars)

  if ((fmt_spec_type %in% c("format spec", "format variable name"))) {
    if (fmt_spec_type == "format spec") {
      # restrict format to requested stats only
      format <- format[.stats]
    }
    # both calling afun and .formats will be updated -- in each split facet based upon spl_context
    afun <- update_afun_no_auto(format = format, afun = afun, method = "format_from_splcontext")
  } else if (fmt_spec_type == "list analysis variable name") {
    # both calling afun and .formats will be updated -- in each split facet based upon vars
    afun <- update_afun_no_auto(format = format, afun = afun, method = "format_from_var")
  }

  # updated afun and format are key for further processing
  return(
    list(
      afun = afun,
      format = format
    )
  )
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



#' @param extra_afun_params (`list`)\cr list of additional parameters (`character`) to be
#'   retrieved from the environment. Curated list is present in [rtables::additional_fun_params].
#' @rdname no_auto_fmt
#' @order 4
#' @keywords internal

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

#' @inheritParams gen_args
#' @inheritParams lyt_args
#' @order 1
#' @keywords internal
#' @rdname no_auto_fmt
#'
afun_ext_add_fun_params <- function(afun) {
  extended_func <- afun
  if (".spl_context" %in% names(formals(afun))) {
    # cat("no update to afun in step afun_ext_add_fun_params")
    extended_func <- afun
  } else {
    # cat("afun is updated in step afun_ext_add_fun_params")
    formals(extended_func) <- c(formals(afun), extra_afun_params)
  }
  # return this function
  extended_func
}



#' @inheritParams gen_args
#' @inheritParams lyt_args
#' @order 3
#' @rdname no_auto_fmt
#' @keywords internal
#'
upd_fmt_args <- function(args, .spl_context = NULL, .var = NULL, format) {
  if (is.null(.spl_context) && is.null(.var)) {
    stop("upd_fmt_args error: .spl_context and .var cannot both be NULL.")
  }
  if (!is.null(.spl_context)) {
    #### this is the piece for getting format from variable on .spl_context
    parent_df <- .spl_context$full_parent_df[[NROW(.spl_context)]]
  }

  fmt_spec_type <- format_spec_type(format)

  if (fmt_spec_type == "format spec") {
    # Method 1: take .formats from format
    args[[".formats"]] <- format
  } else if (fmt_spec_type == "format variable name") {
    # Method 2: take .formats from spl_context format spec -- variable
    # first check this variable is indeed present on input dataframe
    # this check could not yet be covered in format_spec_check - need to be done inside facet
    if (!(format %in% names(parent_df))) {
      stop(paste0("format variable (", format, ") not present in input dataframe"))
    }
    args[[".formats"]] <- unlist(unique(parent_df[[format]]))
  } else if (fmt_spec_type == "list analysis variable name") {
    if (!.var %in% names(format) && "default" %in% names(format)) .var <- "default"
    # Method 3: take .formats from format input
    args[[".formats"]] <- unlist(unname(format[.var]))
  }

  return(args)
}


#' @inheritParams gen_args
#' @inheritParams lyt_args
#' @order 2
#' @rdname no_auto_fmt
#' @param method (`character`)\cr method to be used for retrieving formatting specifications.
#'
#' Options are: `format_from_splcontext` and `format_from_var`.
#'
#' @export
update_afun_no_auto <- function(format = NULL,
                                afun,
                                method = c("format_from_splcontext", "format_from_var")) {
  method <- match.arg(method)

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

    # update of .formats : appropriate method passed to upd_fmt_args
    # the approriate method is passed in no_auto_fmt_handler
    if (method == "format_from_splcontext") {
      args <- upd_fmt_args(args, .spl_context = .spl_context, format = format)
    } else if (method == "format_from_var") {
      args <- upd_fmt_args(args, .var = .var, format = format)
    }

    # Call original function with updated args
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
      # cat("afun update for input df\n")
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
      # cat("afun update for input x\n")
      eval(corepartall)
    }
  }
  # return this function
  return(updated_afun)
}


format_spec_type <- function(format) {
  if (is.null(format)) {
    type <- "null"
  } else if (length(format) == 1 && is.null(names(format))) {
    type <- "format variable name"
  } else if (!is.list(format) || is.list(format) && any(sapply(format, is.function))) {
    type <- "format spec"
  } else if (is.list(format)) {
    type <- "list analysis variable name"
  } else {
    stop("format_spec_type issue: inproper format input")
  }
  return(type)
}


format_spec_check <- function(format, .stats, vars) {
  fmt_spec_type <- format_spec_type(format)

  # perform some basic checks on format
  if (fmt_spec_type == "null") {
    # no check needed
  } else if (fmt_spec_type == "format variable name") {
    # check will be done later in facet - variable name present on input df
  } else if (fmt_spec_type == "format spec") {
    # 1. check that all stats have a format
    misstats <- .stats[!(.stats %in% names(format))]
    if (length(misstats) > 0) {
      stop(paste("Following .stats have no format specification: ", paste(misstats, collapse = ", ")))
    }
    # 2. check that all formats are valid (or function)
    format <- format[.stats]
    cond <- sapply(format, is_valid_format)
    invalid <- unique(unlist(format[!cond]))
    if (length(invalid) > 0) {
      stop(paste("Following format specifications are invalid: ", paste(invalid, collapse = "; ")))
    }
  } else if (fmt_spec_type == "list analysis variable name") {
    # 0. format is a named list (names are the variable names from vars)
    if (is.null(names(format))) {
      stop("when format is a list it should be a named list")
    }
    # 1. check that all vars have a specification
    misvars <- vars[!(vars %in% names(format))]
    if (!("default" %in% names(format)) && length(misvars) > 0) {
      stop(paste("Following vars have no format specification: ", paste(misvars, collapse = ", ")))
    }
    # 2. check that for each var all stats have a format
    misstats2 <- sapply(names(format),
      function(x) {
        y <- format[[x]]
        misstats <- .stats[!(.stats %in% names(y))]
        misstats
      },
      simplify = FALSE, USE.NAMES = TRUE
    )
    misstats2 <- unique(unlist(misstats2))
    if (length(misstats2) > 0) {
      stop(paste("Following stats have no format specification for at least one variable: ", paste(misstats2, collapse = ", ")))
    }

    # 3. check that for each var all formats are valid (or function)
    invalid2 <- sapply(
      names(format),
      function(x) {
        y <- format[[x]]
        ret <- sapply(y, is_valid_format)
        invalid <- format[!ret]
        invalid
      }
    )

    invalid2 <- unique(unlist(invalid2))
    if (length(invalid2) > 0) {
      stop(paste("Following format specifications are invalid: ", paste(invalid2, collapse = "; ")))
    }
    return(NULL)
  }
}
