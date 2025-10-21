context("no auto fmt formatting")

test_that("no auto format inheritance", {
  skip_if_not_installed("dplyr")
  require(dplyr, quietly = TRUE)

  # Test data
  DM2 <- DM %>%
    filter(ARM != levels(DM$ARM)[3]) %>%
    mutate(ARM = as.factor(as.character(ARM)))
  DM2$AGE[1] <- NA # Adding one NA

  # afun that deals with auto formatting and ability to turn off
  test_afun <- function(x, .stats = c("n", "mean", "mean_se", "median", "range"), .formats = NULL, ...) {
    assertthat::assert_that(is.numeric(x))
    x <- x[is.finite(x)]

    y <- list()
    y$n <- c("n" = length(x))
    y$sum <- c("sum" = ifelse(length(x) == 0, NA_real_, sum(x, na.rm = FALSE)))
    y$mean <- c("mean" = ifelse(length(x) == 0, NA_real_, mean(x, na.rm = FALSE)))
    y$sd <- c("sd" = stats::sd(x, na.rm = FALSE))
    y$se <- c("se" = stats::sd(x, na.rm = FALSE) / sqrt(length(stats::na.omit(x))))
    y$mean_sd <- c(y$mean, "sd" = stats::sd(x, na.rm = FALSE))
    y$mean_se <- c(y$mean, y$se)
    y$median <- c("median" = stats::median(x, na.rm = FALSE))
    y$range <- stats::setNames(c(min(x, na.rm = FALSE), max(x, na.rm = FALSE)), c("min", "max"))

    x_stats <- y[.stats]

    if (inherits(.formats, "no_auto_fmt")) {
      .formats <- NULL
    } else {
      .formats_defaults <- c(
        n = "xx",
        sum = "xx.x",
        mean = "xx.x",
        sd = "xx.x",
        se = "xx.x",
        mean_sd = "xx.x (xx.x)",
        mean_se = "xx.x (xx.x)",
        median = "xx.x",
        range = "xx.x - xx.x"
      )
      .formats_miss <- setdiff(names(.formats_defaults), names(.formats))
      .formats <- c(.formats, .formats_defaults[.formats_miss])
      .formats <- .formats[.stats]
    }

    rslt <- in_rows(
      .list = x_stats,
      .formats = .formats,
      .names = names(x_stats),
      .stat_names = names(x_stats)
    )
    rslt
  }

  # every default format xx.x increased to xx.xx
  new_format <- c(
    n = "xx",
    sum = "xx.xx",
    mean = "xx.xx",
    sd = "xx.xx",
    se = "xx.xx",
    mean_sd = "xx.xx (xx.xx)",
    mean_se = "xx.xx (xx.xx)",
    median = "xx.xx",
    range = "xx.xx - xx.xx"
  )

  # Expected formats
  one_col <- c("xx", "xx.x", "xx.x (xx.x)", "xx.x", "xx.x - xx.x")
  one_col2 <- rep("xx", length(one_col))
  one_col3 <- gsub("xx.x", "xx.xx", one_col)

  # table with .formats NULL -- afun takes care of using default formats
  tbl <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze("AGE", test_afun, extra_args = list(.formats = NULL)) %>%
    build_table(DM2)
  result <- get_formatted_cells(tbl, shell = TRUE) # Main function

  expected <- cbind(one_col, one_col)
  dimnames(expected) <- NULL # Fixing attributes
  # Check if it preserves the shell format
  expect_identical(result, expected)

  # table with .formats no auto fmt -- afun turns formats off
  tbl2 <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze("AGE", test_afun, extra_args = list(.formats = no_auto_fmt)) %>%
    build_table(DM2)
  result2 <- get_formatted_cells(tbl2, shell = TRUE) # Main function

  expected2 <- cbind(one_col2, one_col2)
  dimnames(expected2) <- NULL # Fixing attributes
  # Check if it preserves the shell format
  expect_identical(result2, expected2)

  # table with .formats no auto fmt and format provided to analyze
  tbl3 <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze("AGE", test_afun,
      extra_args = list(.formats = no_auto_fmt),
      format = new_format
    ) %>%
    build_table(DM2)
  result3 <- get_formatted_cells(tbl3, shell = TRUE) # Main function

  expected3 <- cbind(one_col3, one_col3)
  dimnames(expected3) <- NULL # Fixing attributes
  # Check if it preserves the shell format
  expect_identical(result3, expected3)


  # table with .formats no auto fmt and format provided to analyze
  tbl4 <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze(c("AGE", "BMRKR1"), test_afun,
      extra_args = list(.formats = no_auto_fmt),
      format = new_format
    ) %>%
    build_table(DM2)
  result4 <- get_formatted_cells(tbl4, shell = TRUE) # Main function

  one_col4_p1 <- c("-", gsub("xx.x", "xx.xx", one_col))
  one_col4 <- rep(one_col4_p1, 2)
  expected4 <- cbind(one_col4, one_col4)
  dimnames(expected4) <- NULL # Fixing attributes
  # Check if it preserves the shell format
  expect_identical(result4, expected4)

  # table with .formats no auto fmt and format provided to analyze
  # different format for different variable
  # every default format xx.x increased to xx.xxx
  # exception mean_sd mean_se range to prevent non-valid format
  new_format2 <- c(
    n = "xx",
    sum = "xx.xxx",
    mean = "xx.xxx",
    sd = "xx.xxx",
    se = "xx.xxx",
    mean_sd = "(xx.xxx, xx.xxx)",
    mean_se = "(xx.xxx, xx.xxx)",
    median = "xx.xxx",
    range = "(xx.xxx, xx.xxx)"
  )

  tbl5 <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze(c("AGE", "BMRKR1"), test_afun,
      extra_args = list(.formats = no_auto_fmt),
      format = list(
        AGE = new_format,
        BMRKR1 = new_format2
      )
    ) %>%
    build_table(DM2)
  result5 <- get_formatted_cells(tbl5, shell = TRUE) # Main function

  one_col5_p1 <- c("-", gsub("xx.x", "xx.xx", one_col, fixed = TRUE))
  one_col5_p2 <- c("-", gsub("xx.x", "xx.xxx", one_col, fixed = TRUE))
  one_col5_p2 <- gsub("xx.xxx (xx.xxx)", "(xx.xxx, xx.xxx)", one_col5_p2, fixed = TRUE)
  one_col5_p2 <- gsub("xx.xxx - xx.xxx", "(xx.xxx, xx.xxx)", one_col5_p2, fixed = TRUE)

  one_col5 <- c(one_col5_p1, one_col5_p2)
  expected5 <- cbind(one_col5, one_col5)
  dimnames(expected5) <- NULL # Fixing attributes
  # Check if it preserves the shell format
  expect_identical(result5, expected5)

  #### second situation: format as variable on input dataset
  # Test data
  def_format <- c(
    n = "xx",
    sum = "xx.x",
    mean = "xx.x",
    sd = "xx.x",
    se = "xx.x",
    mean_sd = "xx.x (xx.x)",
    mean_se = "xx.x (xx.x)",
    median = "xx.x",
    range = "xx.x - xx.x"
  )

  advs <- ex_advs %>%
    filter(PARAMCD %in% c("DIABP", "PULSE", "SYSBP")) %>%
    filter(AVISIT %in% c("WEEK 1 DAY 8", "WEEK 2 DAY 15")) %>%
    select(USUBJID, ARM, PARAMCD, AVISIT, AVAL)

  advs <- advs %>%
    mutate(
      fmt_col =
        case_when(
          PARAMCD == "DIABP" ~ list(new_format),
          PARAMCD == "SYSBP" ~ list(new_format2),
          TRUE ~ list(def_format)
        )
    )

  tbl6 <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("PARAMCD", split_fun = drop_split_levels) %>%
    split_rows_by("AVISIT", split_fun = drop_split_levels) %>%
    analyze(c("AVAL"), test_afun,
      extra_args = list(.stats = c("n", "mean_sd"), .formats = no_auto_fmt),
      format = "fmt_col"
    ) %>%
    build_table(advs)
  ncol <- ncol(tbl6)
  tbl6a <- get_formatted_cells(tbl6[c("PARAMCD", "DIABP"), seq_len(ncol(tbl6))], shell = TRUE)[4, 1]
  tbl6b <- get_formatted_cells(tbl6[c("PARAMCD", "SYSBP"), seq_len(ncol(tbl6))], shell = TRUE)[4, 1]
  tbl6c <- get_formatted_cells(tbl6[c("PARAMCD", "PULSE"), seq_len(ncol(tbl6))], shell = TRUE)[4, 1]

  expected6a <- unname(new_format["mean_sd"])
  expected6b <- unname(new_format2["mean_sd"])
  expected6c <- unname(def_format["mean_sd"])

  expect_identical(tbl6a, expected6a)
  expect_identical(tbl6b, expected6b)
  expect_identical(tbl6c, expected6c)


  # table with only one stat auto formatting disabled using "none" format
  new_mean_se_fmt <- "xx.x (xx.xx)"
  tbl7 <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze(c("AGE", "BMRKR1"), test_afun,
      extra_args = list(.formats = c(mean_se = "none")),
      format = c(mean_se = new_mean_se_fmt)
    ) %>%
    build_table(DM2)
  result7 <- unique(as.vector(get_formatted_cells(tbl7[c("mean_se"), seq_len(ncol(tbl7))], shell = TRUE)))
  expect_identical(result7, new_mean_se_fmt)
})



test_that("tests for format_spec_type", {
  fmt1 <- list(
    AGE = c(n = "xx", mean = "xx.x"),
    default = c(n = "xx", mean = "xx.x", count_fraction = tern::format_count_fraction_fixed_dp)
  )

  expect_identical(format_spec_type(fmt1), "list analysis variable name")

  fmt2 <- c(n = "xx", mean = "xx.x", count_fraction = tern::format_count_fraction_fixed_dp)
  expect_identical(format_spec_type(fmt2), "format spec")

  fmt3 <- c("fmt_col")
  expect_identical(format_spec_type(fmt3), "format variable name")

  fmt3 <- c("xx.xxxxxxx")
  expect_identical(format_spec_type(fmt3), "format spec")

  fmt4 <- c("xx.x", "xx.xx")
  expect_identical(format_spec_type(fmt4), "format spec")

  fmt5 <- tern::format_count_fraction_fixed_dp
  expect_identical(format_spec_type(fmt5), "format spec")

  fmt6 <- c("xx.x", "xx.xx", tern::format_count_fraction_fixed_dp)
  expect_identical(format_spec_type(fmt6), "format spec")

  expect_identical(format_spec_type(NULL), "null")
})


test_that("tests for get_formatvec", {
  nms <- c("n", "mean", "count_fraction", "bla")
  ncrows <- length(nms)

  fmt1 <- list(
    AGE = c(n = "xx", mean = "xx.x"),
    default = c(n = "xx", mean = "xx.x", count_fraction = tern::format_count_fraction_fixed_dp)
  )


  rslt1 <- get_formatvec(fmt1, datcol = "AGE", dfpart = NULL, ncrows, nms)

  expect_identical(rslt1, c(n = "xx", mean = "xx.x", count_fraction = "xx", bla = "xx"))


  fmt2 <- c(n = "xx", mean = "xx.x", count_fraction = tern::format_count_fraction_fixed_dp)
  rslt2 <- get_formatvec(fmt2, datcol = "AGE", dfpart = NULL, ncrows, nms)
  expect_identical(rslt2, c(n = "xx", mean = "xx.x", count_fraction = tern::format_count_fraction_fixed_dp, bla = "xx"))


  fmt3 <- c("fmt_col")
  expect_error(
    get_formatvec(fmt3, datcol = "AGE", dfpart = NULL, ncrows, nms),
    "Format specification issue: Variable fmt_col is not present in input dataset df."
  )

  def_format <- c(
    n = "xx",
    sum = "xx.x",
    mean = "xx.x",
    sd = "xx.x",
    se = "xx.x",
    mean_sd = "xx.x (xx.x)",
    mean_se = "xx.x (xx.x)",
    median = "xx.x",
    range = "xx.x - xx.x"
  )

  new_format <- c(
    n = "xx",
    sum = "xx.xx",
    mean = "xx.xx",
    sd = "xx.xx",
    se = "xx.xx",
    mean_sd = "(xx.xx, xx.xx)",
    mean_se = "(xx.xx, xx.xx)",
    median = "xx.xx",
    range = "(xx.xx, xx.xx)"
  )
  new_format2 <- c(
    n = "xx",
    sum = "xx.xxx",
    mean = "xx.xxx",
    sd = "xx.xxx",
    se = "xx.xxx",
    mean_sd = "(xx.xxx, xx.xxx)",
    mean_se = "(xx.xxx, xx.xxx)",
    median = "xx.xxx",
    range = "(xx.xxx, xx.xxx)"
  )

  advs <- ex_advs %>%
    filter(PARAMCD %in% c("DIABP", "PULSE", "SYSBP")) %>%
    filter(AVISIT %in% c("WEEK 1 DAY 8", "WEEK 2 DAY 15")) %>%
    select(USUBJID, ARM, PARAMCD, AVISIT, AVAL)

  advs <- advs %>%
    mutate(
      fmt_col =
        case_when(
          PARAMCD == "DIABP" ~ list(new_format),
          PARAMCD == "SYSBP" ~ list(new_format2),
          TRUE ~ list(def_format)
        )
    )

  expect_error(
    get_formatvec(fmt3, datcol = "AGE", dfpart = advs, ncrows, nms),
    "Format specification issue: Content of variable"
  )

  rslt3 <- get_formatvec(fmt3, datcol = "AGE", dfpart = advs %>% filter(PARAMCD == "SYSBP"), ncrows, nms)
  expect_identical(rslt3, c(n = "xx", mean = "xx.xxx", count_fraction = "xx", bla = "xx"))
})
