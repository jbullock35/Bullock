# test_regTable.RT1_robust
# Created on 2019-12-14
# Created by John Bullock

context("Check regTable() output")

data(iris)
lm1 <- lm(Sepal.Length ~ Petal.Length,               data = iris)
lm2 <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
if (require(multiwayvcov)) {
  rT1_cluster <- regTable(list(lm1),      clusterVar = list(iris$Species))
  rT2_cluster <- regTable(list(lm1, lm2), clusterVar = list(iris$Species))
}
iris$SepalMed <- iris$Sepal.Length > median(iris$Sepal.Length)
glm1 <- glm(SepalMed ~ Petal.Length, data = iris, family = binomial(link = logit))
glm2 <- glm(SepalMed ~ Petal.Length + Petal.Width, data = iris, family = binomial(link = logit))


test_that("regTable() checks appropriately for existence of clusterVar", {
  expect_is(regTable(list(lm1)),       "regTable")
  expect_is(regTable(list(lm1, glm2)), "regTable")
  expect_is(regTable(list(lm1),          clusterVar = NULL), "regTable")
  expect_is(regTable(list(lm1),          clusterVar = list(iris$Species)), "regTable")
  expect_is(regTable(list(lm1, lm2),     clusterVar = list(iris$Species)), "regTable")  
  expect_error(regTable(list(lm1),       clusterVar = iris$species),       "clusterVar variable doesn't exist")
  expect_error(regTable(list(lm1),       clusterVar = iris$Species),       "clusterVar must be an object of class 'list'")
  expect_error(regTable(list(lm1, glm2), clusterVar = list(iris$Species)), "cannot cluster SEs for glm objects.")  
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
  if (!require(multiwayvcov)) skip("multiwayvcov package not available")    
    
  expect_equal(rT1_cluster[1, 2], sqrt(0.03674), tol = 0.0001)
  expect_equal(rT1_cluster[2, 2], sqrt(0.00161), tol = 0.0001)
  
  expect_equal(rT2_cluster[1, 2], sqrt(0.03674), tol = 0.0001)
  expect_equal(rT2_cluster[2, 2], sqrt(0.00161), tol = 0.0001)
  expect_equal(rT2_cluster[1, 4], sqrt(0.0597),  tol = 0.0001)
  expect_equal(rT2_cluster[2, 4], sqrt(0.0124),  tol = 0.0005)
  expect_equal(rT2_cluster[3, 4], sqrt(0.0291),  tol = 0.0001)
})



test_that("regTable() produces correct estimates and SEs with \"ivreg\" objects and clustering", {
  if (!require(AER))    skip("AER package not available")
  if (!require(ivpack)) skip("ivpack package not available")
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
  if (!require(AER))          skip("AER package not available")
  if (!require(ivpack))       skip("ivpack package not available")
  if (!require(multiwayvcov)) skip("multiwayvcov package not available")
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


test_that("regTable() objects have correct class and attributes after subsetting", {
  if (!require(multiwayvcov)) skip("multiwayvcov package not available")
  
  expect_s3_class(rT2_cluster[1:2,    ], qw("regTable matrix"), exact = TRUE)    
  expect_s3_class(rT2_cluster[   , 1:2], qw("regTable matrix"), exact = TRUE)
  expect_s3_class(rT2_cluster[   , 3:4], qw("regTable matrix"), exact = TRUE)
  expect_s3_class(rT2_cluster[1:2, 3:4], qw("regTable matrix"), exact = TRUE)
  expect_s3_class(rT2_cluster[  2,    ], qw("regTable matrix"), exact = TRUE)  # only one row
  expect_s3_class(rT2_cluster[  2, 3:4], qw("regTable matrix"), exact = TRUE)  # only one row
  expect_equal(class(rT2_cluster[   , 2:3]), "matrix")
  expect_equal(class(rT2_cluster[   ,   2]), "matrix")
  expect_equal(class(rT2_cluster[1:2,   2]), "matrix")
  expect_equal(class(rT2_cluster[  1,   2]), "numeric")  

  rT2_cluster_attrNames <- names(attributes(rT2_cluster))
  expect_true(all(qw("N r.squared SER") %in% rT2_cluster_attrNames))
  
  rT2_cluster_attrNames_b <- names(attributes(rT2_cluster[1,]))
  expect_true(all(qw("N r.squared SER") %in% rT2_cluster_attrNames_b))

  rT2_cluster_attrNames_c <- names(attributes(rT2_cluster[2:3,]))
  expect_true(all(qw("N r.squared SER") %in% rT2_cluster_attrNames_c))
  
  rT2_cluster_attrNames_d <- names(attributes(rT2_cluster[, 3:4]))
  expect_true(all(qw("N r.squared SER") %in% rT2_cluster_attrNames_d))
  
  rT2_cluster_attrNames_e <- names(attributes(rT2_cluster[, 2:3]))
  expect_false(all(qw("N r.squared SER") %in% rT2_cluster_attrNames_e))

  rT2_cluster_attrNames_f <- names(attributes(rT2_cluster[, 3]))
  expect_false(all(qw("N r.squared SER") %in% rT2_cluster_attrNames_f))

  expect_message(
    object = rT2_cluster[1:2, ], 
    regexp = "Rows removed, but.*unchanged\\.")
})


test_that("regTable subsetting works even when it occurs within another function", {
  if (!require(multiwayvcov)) skip("multiwayvcov package not available")
    
  rows <- 1
  cols <- 1
  myFunc <- function (rT, rows, cols) {
    rT[rows, cols]
  }
  rT2_cluster_subset <- myFunc(rT2_cluster, 2:3, 2:4)
  
  expect_equal(class(rT2_cluster_subset), "matrix")
  expect_equal(dim(rT2_cluster_subset), c(2, 3))
})



test_that("print.regTable() output is formatted correctly", {
  if (!require(multiwayvcov)) skip("multiwayvcov package not available")
  
  rT1_cluster_output     <- capture.output(print(rT1_cluster, 2))
  rT2_cluster_output     <- capture.output(print(rT2_cluster, 2))
  rT2_dp0_cluster_output <- capture.output(print(rT2_cluster, 0))
  rT2_dp9_cluster_output <- capture.output(print(rT2_cluster, 9))
  rT_oneRow_output       <- capture.output(
    print.regTable(
      regTable(list(lm1, lm2), rowsToKeep = 'Length'),
      decimalPlaces = 2)
  )

  expect_output(print(rT1_cluster_output[1]), "             Sepal.Length")  
  expect_output(print(rT1_cluster_output[2]), "                 Est   SE")
  expect_output(print(rT1_cluster_output[3]), " \\(Intercept\\)    4.31 0.19")
  expect_output(print(rT1_cluster_output[4]), "Petal.Length    0.41 0.04")
  
  expect_output(print(rT2_cluster_output[1]), "             Sepal.Length  Sepal.Length")  
  expect_output(print(rT2_cluster_output[2]), "                 Est   SE      Est   SE")
  expect_output(print(rT2_cluster_output[3]), " \\(Intercept\\)    4.31 0.19     4.19 0.24")
  expect_output(print(rT2_cluster_output[4]), "Petal.Length    0.41 0.04     0.54 0.11")
  expect_output(print(rT2_cluster_output[5]), " Petal.Width                 -0.32 0.17")
  
  expect_output(print(rT2_dp0_cluster_output[1]), "             Sepal.Length  Sepal.Length")  
  expect_output(print(rT2_dp0_cluster_output[2]), "                   Est SE        Est SE")
  expect_output(print(rT2_dp0_cluster_output[3]), " \\(Intercept\\)         4  0          4  0")
  expect_output(print(rT2_dp0_cluster_output[4]), "Petal.Length         0  0          1  0")
  expect_output(print(rT2_dp0_cluster_output[5]), " Petal.Width                       0  0")
  
  expect_output(print(rT2_dp9_cluster_output[1]), "                         Sepal.Length              Sepal.Length")  
  expect_output(print(rT2_dp9_cluster_output[2]), "                      Est          SE           Est          SE")
  expect_output(print(rT2_dp9_cluster_output[3]), " \\(Intercept\\)  4.306603415 0.191679472   4.190582429 0.244328907")
  expect_output(print(rT2_dp9_cluster_output[4]), "Petal.Length  0.408922277 0.040080702   0.541777154 0.111515660")
  expect_output(print(rT2_dp9_cluster_output[5]), " Petal.Width                           -0.319550561 0.170671950")
  
  expect_output(print(rT_oneRow_output[1]), "             Sepal.Length  Sepal.Length")  
  expect_output(print(rT_oneRow_output[2]), "                 Est   SE      Est   SE")
  expect_output(print(rT_oneRow_output[3]), "Petal.Length    0.41 0.02     0.54 0.07")
})


test_that("print.regTable() output is formatted correctly when using lm_robust objects", {
  if (!require(estimatr)) skip("estimatr package not available")
  lm1_robust <- estimatr::lm_robust(Petal.Width ~ Petal.Length,               data = iris)
  lm2_robust <- estimatr::lm_robust(Petal.Width ~ Petal.Length * Sepal.Width, data = iris)
  rT1_robust <- regTable(list(lm1_robust, lm2_robust))
  rT1_robust_output <- capture.output(print(rT1_robust, 2))
  
  # We take this as the reference output:
  #  
  # > rT1_robust 
  #                          Petal.Width  Petal.Width
  #                             Est   SE     Est   SE
  #              (Intercept)  -0.36 0.03   -0.20 0.30
  #             Petal.Length   0.42 0.01    0.28 0.11
  #              Sepal.Width               -0.05 0.09
  # Petal.Length:Sepal.Width                0.05 0.04
  
  expect_output(print(rT1_robust_output[1]), "                         Petal.Width  Petal.Width")  
  expect_output(print(rT1_robust_output[2]), "                            Est   SE     Est   SE")
  expect_output(print(rT1_robust_output[3]), "             \\(Intercept\\)  -0.36 0.03   -0.20 0.30")
  expect_output(print(rT1_robust_output[4]), "            Petal.Length   0.42 0.01    0.28 0.11")
  expect_output(print(rT1_robust_output[5]), "             Sepal.Width               -0.05 0.09")
  expect_output(print(rT1_robust_output[6]), "Petal.Length:Sepal.Width                0.05 0.04")
})



