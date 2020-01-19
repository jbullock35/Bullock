# test_regTable.R
# Created on 2019-12-14
# Created by John Bullock

context("Check regTable() output")

data(iris)
lm1 <- lm(Sepal.Length ~ Petal.Length,               data = iris)
lm2 <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
rT1_cluster <- regTable(list(lm1),      clusterVar = list(iris$Species))
rT2_cluster <- regTable(list(lm1, lm2), clusterVar = list(iris$Species))


test_that("regTable() checks appropriately for existence of clusterVar", {
  expect_is(regTable(list(lm1)), "regTable")
  expect_is(regTable(list(lm1),      clusterVar = NULL), "regTable")
  expect_error(regTable(list(lm1),   clusterVar = iris$species), "clusterVar variable doesn't exist")
  expect_error(regTable(list(lm1),   clusterVar = iris$Species), "clusterVar must be an object of class 'list'")
  expect_is(regTable(list(lm1),      clusterVar = list(iris$Species)), "regTable")
  expect_is(regTable(list(lm1, lm2), clusterVar = list(iris$Species)), "regTable")  
})


test_that("regTable() produces correct output with IV regressions", {
  if (!require(AER)) skip("AER package not available")
  iv1 <- ivreg(Sepal.Length ~ Petal.Length | Petal.Width,  data = iris)
  iv2 <- ivreg(Sepal.Length ~ Petal.Width  | Petal.Length, data = iris)
  expect_is(regTable(list(iv1)),      "regTable")
  expect_is(regTable(list(iv1, iv2)), "regTable")
  expect_identical(nrow(regTable(list(iv1))),      2L)
  expect_identical(nrow(regTable(list(iv1, iv2))), 3L)  # Is regTable() eliminating the duplicate rowname?
})


test_that("regTable() produces correct output with lm() objects and clustered SEs", {
  # We take this as the reference output:  [2020 01 19]
  #    
  # multiwayvcov::cluster.vcov(lm1, iris$Species)
  #                (Intercept) Petal.Length
  # (Intercept)      0.03674     -0.00739
  # Petal.Length    -0.00739      0.00161  
  # 
  # multiwayvcov::cluster.vcov(lm2, iris$Species)
  #             (Intercept) Petal.Length Petal.Width
  # (Intercept)       0.0597      -0.0249      0.0355
  # Petal.Length     -0.0249       0.0124     -0.0189
  # Petal.Width       0.0355      -0.0189      0.0291   
  expect_equal(rT1_cluster[1, 2], sqrt(0.03674), tol = 0.0001)
  expect_equal(rT1_cluster[2, 2], sqrt(0.00161), tol = 0.0001)
  
  expect_equal(rT2_cluster[1, 2], sqrt(0.03674), tol = 0.0001)
  expect_equal(rT2_cluster[2, 2], sqrt(0.00161), tol = 0.0001)
  expect_equal(rT2_cluster[1, 4], sqrt(0.0597),  tol = 0.0001)
  expect_equal(rT2_cluster[2, 4], sqrt(0.0124),  tol = 0.0005)
  expect_equal(rT2_cluster[3, 4], sqrt(0.0291),  tol = 0.0001)
})
