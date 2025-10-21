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
## Cases:
## format spec - normal format specification: function or valid format label
## null - NULL
## - format variable name - variable name on input dataset, eg format = "fmt_col"
## - list analysis variable name - list of variable names with their corresponding formatting specifications
## eg format = list(AGE = c(n = "xx", mean = "xx.x", mean_sd = "xx.x (xx.xx)"),
##                  WEIGHT = c(n = "xx", mean = "xx.xx", mean_sd = "xx.xx (xx.xxx)"),
##                  default = c(n = "xx", mean = "xx.x", mean_sd = "xx.x (xx.xx)", count_fraction = "xx. (xx.x%)"))
## - format spec - a standard format specification
##   named vector: format = c(n = "xx", mean = "xx.x", mean_sd = "xx.x (xx.xx)")
##   or unnamed character(1): format = "xx.x"
##   include option for unnamed vector length > 1????????

# examples
# fmt1 <- list(AGE = c(n = "xx", mean = "xx.x"),
#              default = c(n = "xx", mean = "xx.x", count_fraction = format_count_fraction_fixed_dp))
# format_spec_type(fmt1)
#
# fmt2 <- c(n = "xx", mean = "xx.x", count_fraction = format_count_fraction_fixed_dp)
# format_spec_type(fmt2)
#
# fmt3 <- c("fmt_col")
# format_spec_type(fmt3)
#
# fmt3 <- c("xx.xxxxxxx")
# format_spec_type(fmt3)
#
# fmt4 <- c("xx.x", "xx.xx")
# format_spec_type(fmt4)
#
# fmt5 <- format_count_fraction_fixed_dp
# format_spec_type(fmt5)
#
# fmt6 <- c("xx.x", "xx.xx", format_count_fraction_fixed_dp)
# format_spec_type(fmt6)


format_spec_type <- function(format) {
  if (is.null(format)) {
    type <- "null"
  } else if (length(format) == 1 && is.null(names(format))) {
    if (is_valid_format(format) || grepl("xx.", format, fixed = TRUE) || is.function(format)) {
      type <- "format spec"
    } else {
      type <- "format variable name"
    }
  } else if (!is.list(format) || (is.list(format) && any(sapply(format, is.function)))) {
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
    if (datcol %in% names(format)) {
      format_datcol <- datcol
    } else if ("default" %in% names(format)) {
      format_datcol <- "default"
    } else {
      stop(paste("No format specification found for variable: ", datcol))
    }
    format <- format[[format_datcol]]
  } else if (fmt_spec_type == "format variable name") {
    if (!format %in% names(dfpart)) {
      stop("Format specification issue: Variable ", format, " is not present in input dataset df.")
    }
    format_orig <- format
    format <- unlist(unique(dfpart[[format]]))
    if (any(duplicated(names(format)))) {
      stop("Format specification issue: Content of variable ", format_orig, " is not unique accross current facet.")
    }
  }

  if (is.function(format)) {
    format <- list(format)
  }
  if (is.null(names(format))) {
    ### original rtables behavior
    formatvec <- rep(format, length.out = ncrows)
  } else {
    ### new behavior: input format is named
    ### KEY assumption: names of the rows from verticalsection start with the name of the statistic, (possibly followed with ".")
    ### method will not work when not all columns are from the same statistics (eg relative risk column)
    ###
    if (!is.null(names(nms))) nms <- names(nms)
    nmf <- sub("\\..*", "", nms)

    not_in_fmt <- setdiff(nmf, names(format))
    if (length(not_in_fmt) > 0) {
      # proceed with unformatted presentation ("xx")
      not_in_fmt2 <- rep("xx", length(not_in_fmt))
      names(not_in_fmt2) <- not_in_fmt
      format_ext <- c(format, not_in_fmt2)
      # message(paste("No format provided for statistic(s): "), paste(not_in_fmt, collapse = ", "))
    } else {
      format_ext <- format
    }
    xx <- match(nmf, names(format_ext))
    formatvec <- format_ext[xx]
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
