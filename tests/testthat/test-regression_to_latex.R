test_that("basic lm model works", {
  model <- lm(mpg ~ wt + hp, mtcars)
  result <- regression_to_latex(model, R2 = TRUE)
  expect_type(result, "character")
  expect_true(grepl("\\$\\$", result))
})

test_that("probit model works", {
  skip_if_not_installed("fixest")
  library(fixest)
  
  mtcars$vs_binary <- as.numeric(mtcars$vs > 0)
  model <- feglm(vs_binary ~ wt + hp, 
                 family = binomial(link = "probit"), 
                 data = mtcars)
  
  result <- regression_to_latex(model)
  expect_true(grepl("\\Phi", result))
})
