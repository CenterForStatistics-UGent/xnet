context("Helper functions")

dfile <- system.file("testdata","testdata.rda", package = "xnet")

load(dfile)

test_that("Dimension checks work correctly",{
  expect_true(valid_dimensions(Y,K,G))
  expect_false(valid_dimensions(Y,K))
  expect_true(valid_dimensions(matrix(ncol = 5, nrow = 5),
                               matrix(ncol = 5, nrow = 5)))
  expect_false(valid_dimensions(Y,G,K))
})

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
})