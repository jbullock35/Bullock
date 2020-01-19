# test_regTable.R
# Created on 2019-12-14
# Created by John Bullock

context("Check regTable() output")


test_that("regTable() checks appropriately for existence of clusterVar", {
  data(iris)
  lm1 <- lm(Sepal.Length ~ Petal.Length,               data = iris)
  lm2 <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
  expect_is(regTable(list(lm1)), "regTable")
  expect_is(regTable(list(lm1),      clusterVar = NULL), "regTable")
  expect_error(regTable(list(lm1),   clusterVar = iris$species), "clusterVar variable doesn't exist")
  expect_error(regTable(list(lm1),   clusterVar = iris$Species), "clusterVar must be an object of class 'list'")
  expect_is(regTable(list(lm1),      clusterVar = list(iris$Species)), "regTable")
  expect_is(regTable(list(lm1, lm2), clusterVar = list(iris$Species)), "regTable")  
})


test_that("regTable() produces correct output with IV regressons", {
  if (!require(AER)) skip("AER package not available")
  iv1 <- ivreg(Sepal.Length ~ Petal.Length | Petal.Width,  data = iris)
  iv2 <- ivreg(Sepal.Length ~ Petal.Width  | Petal.Length, data = iris)
  expect_is(regTable(list(iv1)),      "regTable")
  expect_is(regTable(list(iv1, iv2)), "regTable")
  expect_identical(nrow(regTable(list(iv1))),      2L)
  expect_identical(nrow(regTable(list(iv1, iv2))), 3L)  # Is regTable() eliminating the duplicate rowname?
})
