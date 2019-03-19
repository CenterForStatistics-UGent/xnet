context("Helper functions")

dfile <- system.file("testdata","testdata.rda", package = "xnet")

load(dfile)

test_that("create_grid works correctly", {
  expect_equal(create_grid(c(1e-2, 1e2), 5),
               c(0.01,0.1,1,10,100))
  expect_error(create_grid(4,1))
  expect_error(create_grid(1:2, 1:2))
})

# Valid labels ------------------------------------------------

test_that("valid_labels returns errors when needed",{
  # Test with wrong dimensions
  expect_error(valid_labels(Y,K,matrix(0,ncol=3,nrow=3)),
               "Dimensions are incompatible")
  # Construct non-matching labels
  Yw <- matrix(0,nrow = 4, ncol = 4)
  rownames(Yw) <-  c("F","E","Q","R")
  Kw <- matrix(0, nrow=4, ncol=4)
  rownames(Kw) <- colnames(Kw) <- c("E","Q","R","G")
  # No colnames Yw
  expect_error(valid_labels(Yw,Kw),
               "Not all row labels and col labels")
  colnames(Yw) <- rownames(Yw)
  expect_error(valid_labels(Yw,Kw),
               "rownames of y and k are not matching")

  rownames(Kw) <- rownames(Kw)[c(2,1,3,4)]
  expect_error(valid_labels(Yw, Kw),
               "Different row- and colnames found for k")

  # Create G and set everything back to OK for Y and K
  Gw <- matrix(0,nrow = 4, ncol = 4)
  rownames(Gw) <- colnames(Gw) <- c("D","A","V","B")
  rownames(Yw) <- colnames(Kw)
  colnames(Yw) <- c("D","A","W","B")
  rownames(Kw) <- colnames(Kw)

  expect_error(valid_labels(Yw, Kw, Gw),
               "colnames of y and g are not matching")
  colnames(Yw) <- colnames(Gw)
  rownames(Gw) <- rownames(Gw)[c(2,1,3,4)]
  expect_error(valid_labels(Yw, Kw, Gw),
               "Different row- and colnames found for g")
  colnames(Gw) <- NULL
  expect_error(valid_labels(Yw, Kw, Gw),
               "Not all row labels and col labels could be found")
  expect_error(valid_labels(Yw, Kw),
               "colnames of y and k are not matching")

})

# Check input ------------------------------------------------
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
  expect_error(.test_input(Y,K, lambda = "a"))
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


# Match labels -----------------------------------------------
mat <- matrix(1:12, ncol = 4,
              dimnames = list(c("b", "a", "d"),
                              c("ca", "cb","cd","cc")))
rmatch <- c("a","b", "d")
cmatch <- c("ca","cb","cc","cd")

rmat <- matrix(1:9, ncol = 3, dimnames = list(rmatch,rmatch))
cmat <- matrix(1:16, ncol = 4, dimnames = list(cmatch,cmatch))
res <- mat[c(2,1,3), c(1,2,4,3)]

mat2 <- mat
rownames(mat2) <- c("x","a","d")
mat3 <- mat
colnames(mat3) <- c("cx","cb","cc","cd")


test_that("Label matching produces correct errors",{
  expect_error(match_labels(1:10,rmatch, cmatch),
               "be a matrix")
  expect_error(match_labels(mat, matrix(1:9,nrow=3)),
               "no rownames")
  expect_error(match_labels(mat, 1:3),
               "with rownames or a character vector")
  expect_error(match_labels(mat, rmat, matrix(1:16, ncol = 4)),
               "no colnames")
  expect_error(match_labels(mat, rmat, 1:16),
               "colnames or a character vector")
  expect_error(match_labels(mat, cmat,rmat),
               "row labels not of the correct length")
  expect_error(match_labels(mat, rmat, rmat),
               "col labels not of the correct length")
  expect_error(match_labels(mat2, rmat, cmat),
               "row labels not compatible with rownames y")
  expect_error(match_labels(mat3, rmat, cmat),
               "col labels not compatible with colnames y")

})

nomatres <- nomat <- mat
dimnames(nomat) <- NULL
dimnames(nomatres) <- list(rmatch,cmatch)

rmatshift <- rmat[c(2,1,3),c(3,1,2)]

test_that("Label matching gives correct results",{
  expect_equal(match_labels(mat,rmatch,cmatch), res)
  expect_equal(match_labels(mat,rmat,cmatch), res)
  expect_equal(match_labels(mat,rmatch,cmat), res)
  expect_equal(match_labels(mat,rmat,cmat), res)
  expect_equal(match_labels(nomat,rmatch,cmatch), nomatres)
  expect_equal(match_labels(rmatshift,rmatch),rmat)
})

# Test symmetry ----------------------------------
test_that("test_symmetry returns correct error",{
  expect_error(test_symmetry(1),
               "x should be a matrix")
})

# Test is_symmetric ------------------------------
test_that("is_symmetric returns correct values/errors",{
  expect_error(is_symmetric(matrix(c("a","b"))),
               "x should be a numeric matrix")
  expect_error(is_symmetric(c(1,4)),
               "x should be a numeric matrix")
  expect_true(is_symmetric(matrix(1)))
  expect_false(is_symmetric(matrix(0,nrow=2,ncol=3)))
  expect_false(is_symmetric(matrix(rnorm(16), ncol = 4)))
})