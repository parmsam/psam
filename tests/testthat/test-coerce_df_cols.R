test_that("coerce_df_cols() works", {
  df1 <- tibble::tibble(
    a = sample(1:10, 5),
    b = letters[1:5],
    c = sample(c(T,F), 5, replace = T),
    d = 6:10
  )
  df2 <- tibble::tibble(
    a = as.character(sample(1:10, 5)),
    foo = 1:5,
    b = letters[1:5],
    bar = runif(5),
    c = as.character(sample(c(TRUE,FALSE), 5, replace = T)),
    stringsAsFactors = FALSE
  )
  result <- coerce_df_cols(truth = df1, source = df2)
  expect_true(
    class(df1$a) == class(result$a) &&
      class(df1$b) == class(result$b) &&
      class(df1$c) == class(result$c)
  )
})
