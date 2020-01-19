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


test_that("regTable() produces matrices of right class and dimensions with IV regressions", {
  if (!require(AER)) skip("AER package not available")
  iv1 <- ivreg(Sepal.Length ~ Petal.Length | Petal.Width,  data = iris)
  iv2 <- ivreg(Sepal.Length ~ Petal.Width  | Petal.Length, data = iris)
  expect_is(regTable(list(iv1)),      "regTable")
  expect_is(regTable(list(iv1, iv2)), "regTable")
  expect_identical(nrow(regTable(list(iv1))),      2L)
  expect_identical(nrow(regTable(list(iv1, iv2))), 3L)  # Is regTable() eliminating the duplicate rowname?
})


test_that("regTable() produces correct estimates and SEs with \"lm\" objects and clustering", {
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



test_that("regTable() produces correct estimates and SEs with \"ivreg\" objects and clustering", {
  if (!require(AER)) skip("AER package not available")
  if (!require(AER)) skip("ivpack package not available")
  iv1 <- ivreg(Sepal.Length ~ Petal.Length | Petal.Width,  data = iris)
  iv2 <- ivreg(Sepal.Length ~ Petal.Width  | Petal.Length, data = iris)
  rT1_iv_cluster <- regTable(list(iv1),      clusterVar = list(iris$Species))
  rT2_iv_cluster <- regTable(list(iv1, iv2), clusterVar = list(iris$Species))
    
  # We take this as the reference output:  [2020 01 19]
  #    
  # ivpack::cluster.robust.se(iv1, iris$Species)
  #             Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)    4.3459     0.1360    32.0   <2e-16 ***
  # Petal.Length   0.3985     0.0274    14.5   <2e-16 ***  
  #
  # ivpack::cluster.robust.se(iv2, iris$Species)
  #           Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)   4.6637     0.1546    30.2   <2e-16 ***
  # Petal.Width   0.9836     0.0862    11.4   <2e-16 ***    
  expect_equal(rT1_iv_cluster[1, 2], 0.1360, tol = 0.0001)
  expect_equal(rT1_iv_cluster[2, 2], 0.0274, tol = 0.0001)
  
  expect_equal(rT2_iv_cluster[1, 2], 0.1360, tol = 0.0001)
  expect_equal(rT2_iv_cluster[2, 2], 0.0274, tol = 0.0001)
  expect_equal(rT2_iv_cluster[1, 4], 0.1546, tol = 0.0001)
  expect_equal(rT2_iv_cluster[3, 4], 0.0862, tol = 0.0005)
})


test_that("regTable() produces correct estimates and SEs with both \"lm\" and \"ivreg\" objects and clustering", {
  if (!require(AER)) skip("AER package not available")
  if (!require(AER)) skip("ivpack package not available")
  iv1 <- ivreg(Sepal.Length ~ Petal.Length | Petal.Width,  data = iris)
  iv2 <- ivreg(Sepal.Length ~ Petal.Width  | Petal.Length, data = iris)
  rT_mixed <- regTable(list(lm1, lm2, iv1, iv2), clusterVar = list(iris$Species))
  
  expect_equal(rT_mixed[1, 2], sqrt(0.03674), tol = 0.0001)
  expect_equal(rT_mixed[2, 2], sqrt(0.00161), tol = 0.0001)
  expect_equal(rT_mixed[1, 4], sqrt(0.0597),  tol = 0.0001)
  expect_equal(rT_mixed[2, 4], sqrt(0.0124),  tol = 0.0005)
  expect_equal(rT_mixed[3, 4], sqrt(0.0291),  tol = 0.0001)
   
  expect_equal(rT_mixed[1, 6], 0.1360, tol = 0.0001)
  expect_equal(rT_mixed[2, 6], 0.0274, tol = 0.0001)
  expect_equal(rT_mixed[1, 8], 0.1546, tol = 0.0001)
  expect_equal(rT_mixed[3, 8], 0.0862, tol = 0.0005)

})