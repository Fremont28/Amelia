### pass tests, converting current test files to the test that paradigm

test_that("Test bounds",{
  data(freetrade)
  
  bds <- matrix(c(3, 30, 32), nrow = 1, ncol = 3)
  
  set.seed(12345)
  a.out.bds <- amelia(freetrade, ts = "year", cs = "country", bounds = bds,
                      max.resample = 10, p2s = 0)
  
  out <- range(a.out.bds$imputations$imp1[is.na(freetrade[,3]),3])
  
  expect_gte(out[1],30)
  
  expect_lte(out[2], 32)
})

test_that('check moPrep',{
  x <- rnorm(100)
  s <- x + rnorm(100, 0, 0.1)
  vv <- rep(0.1^2/var(s), 100)
  
  df <- data.frame(x, s)
  mop <- moPrep(df,s ~ s,error.proportion = vv)
  ##a.out <- amelia(mop, parallel = 'no')
  
  
})

test_that('check overimp', {
  data(africa)
  af2 <- na.omit(africa)
  
  oi <- matrix(c(1:10,rep(4, 10)), nrow = 10)
  
  a.out <- amelia(af2, cs = 2, overimp = oi, p2s = 0)
  
  expect_false(a.out$imputations[[1]][1,4] == a.out$imputations[[2]][1,4])
})

