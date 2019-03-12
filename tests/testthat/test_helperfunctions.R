context("Helper functions")

dfile <- system.file("testdata","testdata.rda", package = "xnet")

load(dfile)

test_that("create_grid works correctly", {
  expect_equal(create_grid(c(1e-2, 1e2), 5),
               c(0.01,0.1,1,10,100))
  expect_error(create_grid(4,1))
  expect_error(create_grid(1:2, 1:2))
})

test_that("valid labels are recognized", {
  data("drugtarget")
  data("proteinInteraction")
  expect_true(xnet:::valid_labels(drugTargetInteraction,
                                  targetSim,
                                  drugSim))
  expect_true(xnet:::valid_labels(Y,K,G))
  expect_true(xnet:::valid_labels(proteinInteraction,
                                  Kmat_y2h_sc))
})

test_that("valid_labels returns errors when needed",{
  # Test with wrong dimensions
  expect_error(xnet:::valid_labels(Y,K,targetSim),
               "Dimensions are incompatible")
  # Construct non-matching labels
  Yw <- matrix(0,nrow = 4, ncol = 4)
  rownames(Yw) <-  c("F","E","Q","R")
  Kw <- matrix(0, nrow=4, ncol=4)
  rownames(Kw) <- colnames(Kw) <- c("E","Q","R","G")
  # No colnames Yw
  expect_error(xnet:::valid_labels(Yw,Kw),
               "Not all row labels and col labels")
  colnames(Yw) <- rownames(Yw)
  expect_error(xnet:::valid_labels(Yw,Kw),
               "rownames of y and k are not matching")

  rownames(Kw) <- colnames(Yw)
  expect_error(xnet:::valid_labels(Yw, Kw),
               "Different row- and colnames found for k")


})

test_that("Input is checked correctly", {
  expect_equal(.test_input(Y,K,G, lambda = 1e-4,
                           testdim = TRUE, testlabels = TRUE),
               list(lambda.k = 1e-4,
                    lambda.g = 1e-4,
                    homogenous = FALSE)
  )
  expect_error(.test_input(1:10,K,G), "y should be a matrix")
  expect_error(.test_input(Y,1:10,G), "k should be a matrix")
  expect_error(.test_input(Y,K,1:10), "g should be a matrix")
  expect_error(.test_input(Y,K,G, lambda = 1:10),
               "lambda should contain one or two values")
  expect_error(.test_input(Y,K,g = NULL, lambda = c(1,2)),
               "lambda should be a single value")
  # Test NA passage
  Yna <- Y
  Yna[c(3,7)] <- NA
  expect_error(.test_input(Yna,K,G))
  expect_equal(.test_input(Yna,K,G, checkna = FALSE),
               list(lambda.k = 1e-4,
                    lambda.g = 1e-4,
                    homogenous = FALSE))

  # Test dimensions
  expect_error(.test_input(Y,K, g=NULL),
               "dimensions of the matrices don't match.")
  Kwrong <- matrix(1:12,nrow=4)
  expect_error(.test_input(Y,Kwrong,G),
               "k should be a symmetric matrix")
  Gwrong <- matrix(1:15, ncol = 5)
  expect_error(.test_input(Y,K,Gwrong),
               "g should be a symmetric matrix")
  YL <- Y
  KL <- K
  GL <- G
  rownames(YL) <- letters[1:4]
  colnames(YL) <- letters[11:15]
  rownames(KL) <- colnames(KL) <- letters[5:8]
  rownames(GL) <- colnames(GL) <- letters[11:15]
  expect_error(.test_input(YL,KL,GL),
               "rownames of y and k are not matching.")



})

test_that("Dimension checks work correctly",{
  expect_true(valid_dimensions(matrix(ncol = 5, nrow = 5),
                               matrix(ncol = 5, nrow = 5)))
  expect_false(valid_dimensions(Y,G,K))
})