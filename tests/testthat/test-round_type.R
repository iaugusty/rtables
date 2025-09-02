context("Printing tables with proper round_type")


test_that("toString method works correclty with user defined round_type", {
  skip_if_not_installed("dplyr")
  require(dplyr, quietly = TRUE)
  
  vals <- c(1.865, 2.985, 3.457)
  
  txtvals_iec <- mapply(format_value, x = vals, format = "xx.xx", round_type = "iec")
  txtvals_sas <- mapply(format_value, x = vals, format = "xx.xx", round_type = "sas")
 
  # adjust vals if following is not TRUE
  expect_true(any(txtvals_iec != txtvals_sas))
 
  adsl <- ex_adsl %>% select(USUBJID, ARMCD, SEX)
  
  adsl <- adsl %>% 
    mutate(new_var = case_when(ARMCD == "ARM A" ~ vals[1],
                               ARMCD == "ARM B" ~ vals[2],
                               ARMCD == "ARM C" ~ vals[3]))
  
  lyt <- basic_table(show_colcounts = FALSE) %>%
    split_cols_by("ARMCD") %>%
    analyze(c("new_var"), function(x) {
      in_rows(
        mean = mean(x),
        .formats = c("xx.xx"),
        .labels = c("Mean")
      )
    }) 
  
  tbl_iec <- lyt %>%
    build_table(adsl, round_type = "iec")
  
  tbl_sas <- lyt %>%
    build_table(adsl, round_type = "sas")  


  colheader <- c("ARM A", "ARM B", "ARM C")
  names(txtvals_iec) <- colheader
  names(txtvals_sas) <- colheader
  
  totl <- 28
  colheader2 <- substr(paste(strrep(" ", 6), paste(sprintf("%-7s", colheader), collapse = " ")), 1, totl)
  txtvals_sas2 <- paste(substr(paste("Mean  ", paste(sprintf("%-7s", txtvals_sas), collapse = " ")), 1, totl - 1), "\n")
  txtvals_iec2 <- paste(substr(paste("Mean  ", paste(sprintf("%-7s", txtvals_iec), collapse = " ")), 1, totl - 1), "\n")
 
  
  expstr_lns_iec <- c(
    colheader2,
    strrep("—", totl),
    txtvals_iec2
  )
  
  expstr_lns_sas <- c(
    colheader2,
    strrep("—", totl),
    txtvals_sas2
  )

  exp_str_iec <- paste(expstr_lns_iec, collapse = "\n")
  exp_str_sas <- paste(expstr_lns_sas, collapse = "\n")

  expect_identical(
    toString(tbl_iec),
    exp_str_iec
  )
  
  expect_identical(
    toString(tbl_sas),
    exp_str_sas
  )

  expect_identical(
    toString(tbl_iec, round_type = "sas"),
    exp_str_sas
  )  
  
  expect_identical(
    get_round_type(tbl_iec),
    "iec"
  )
  
  expect_identical(
    get_round_type(tbl_sas),
    "sas"
  )
  
  
  # round_type is maintained when subtable from ElementaryTable (select single column)
  sub_tbl_sas <- tbl_sas[, c(1, 2)]
  expect_identical(
    get_round_type(sub_tbl_sas),
    "sas"
  )  
  
  # testing as_result_df
  df_iec <- as_result_df(tbl_iec, data_format = "strings")
  df_sas <- as_result_df(tbl_sas, data_format = "strings")
  df_iec2 <- as_result_df(tbl_sas, data_format = "strings", round_type = "iec")
  
  df_x <- df_iec[, 1: 6]
  tdf_iec <- cbind(df_x, as.data.frame(t(txtvals_iec)))
  tdf_sas <- cbind(df_x, as.data.frame(t(txtvals_sas)))
  
  expect_identical(
    df_iec,
    tdf_iec
  )    
  
  expect_identical(
    df_sas,
    tdf_sas
  )    
  
  expect_identical(
    df_iec,
    df_iec2
  )    
  

  # testing as_html
  skip_if_not_installed("xml2")
  require(xml2, quietly = TRUE)
  
  html_tbl_iec <- as_html(tbl_iec)
  html_tbl_sas <- as_html(tbl_sas)
  html_tbl_iec2 <- as_html(tbl_sas, round_type = "iec")
  
  expect_identical(
    html_tbl_iec,
    html_tbl_iec2
  )    
  
  html_parts_iec <- html_tbl_iec$children[[1]][[3]]$children[[2]]$children[[1]]
  html_parts_sas <- html_tbl_sas$children[[1]][[3]]$children[[2]]$children[[1]]
  
  get_value_from_html <- function(html) {
    doc <- xml2::read_html(as.character(html)) 
    val <- xml2::xml_text(doc) 
  }
  
  from_html_iec <- sapply(html_parts_iec, get_value_from_html)[2:4]
  from_html_sas <- sapply(html_parts_sas, get_value_from_html)[2:4]
  
  expect_identical(
    from_html_iec,
    unname(txtvals_iec)
  )    
  
  expect_identical(
    from_html_sas,
    unname(txtvals_sas)
  )      
  
  
})

test_that("round_type still available on subtable", {
  adsl <- ex_adsl %>% 
    filter(SEX %in% c("F", "M")) %>% 
    select(USUBJID, ARMCD, SEX, AGE)
  
  
  lyt <- basic_table(show_colcounts = FALSE) %>%
    split_cols_by("ARMCD") %>%
    split_rows_by("SEX", split_fun = drop_split_levels) %>% 
    analyze(c("AGE"), function(x) {
      in_rows(
        mean = mean(x),
        .formats = c("xx.xx"),
        .labels = c("Mean")
      )
    }) 
  
  tbl_iec <- lyt %>%
    build_table(adsl, round_type = "iec")
  
  tbl_sas <- lyt %>%
    build_table(adsl, round_type = "sas")    
  
  sub_tbl <- tbl_sas[c("SEX", "F"), ]
  expect_identical(
    get_round_type(sub_tbl),
    "sas"
  )    
  
})
