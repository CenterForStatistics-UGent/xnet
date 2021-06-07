context("Construction of data classes")
library(kernlab)
library(igraph)
# Preparation gramDataFrame ----------------------------
adf <- data.frame(
  a = c(4,1,2,4,5),
  b = c(5,4,3,2,1),
  c = c(1,5,2,4,3)
)
# Empty numeric matrix
mnought <- matrix(numeric(0), nrow = 0, ncol = 0)
# kernelmatrix
gm <- kernelMatrix(rbfdot(),
                   model.matrix(~ .-1, adf))
# Convert to matrix, as that happens inside
# the code as well
eig <- eigen(gm)

conv <- function(x){
  tcrossprod(as.matrix(x))
}

gramadf <- gramData(adf)
gramadf2 <- gramData(adf, conv)
gramadf3 <- gramData(adf, keep.gram = TRUE)

test_that("gramData works correctly with a data frame",{
  # Origin is kept correctly
  expect_equal(adf, get_orig(gramadf))
  # conversion works
  expect_equal(eig, gramadf@eigen)
  expect_equal(conv, gramadf2@conversion)
  expect_equal(gm, gramadf3@gram)
  # gram matrix is stored correctly
  expect_equal(mnought,
               gramadf@gram)
  expect_false(gramadf@hasgram)
})

gramgm <- gramData(gm, keep.gram = TRUE)
amat <- as.matrix(adf)
gramamat <- gramData(amat, conv)
gramamat2 <- gramData(amat, conv, keep.gram = TRUE)

test_that("gramData works correctly on a matrix", {
  expect_false(has_orig(gramgm))
  expect_warning(get_orig(gramgm),
                 "No original data")
  expect_true(gramgm@hasgram)
  expect_equal(gramgm@eigen, eig)
  expect_equal(gramgm@gram, gm)
  # another matrix
  expect_true(has_orig(gramamat))
  expect_equal(get_orig(gramamat), amat)
  # keep gram
  expect_true(gramamat2@hasgram)
  expect_equal(gramamat2@gram, conv(amat))
})

#testlabs
gramadflabs <- gramData(adf, conv,
                        labels = letters[1:5])
amatlabs <- tcrossprod(amat)
rownames(amatlabs) <- colnames(amatlabs) <-  letters[1:5]
gramamatlabs <- gramData(amatlabs)
amatlabs2 <- amatlabs
rownames(amatlabs2) <- letters[6:10]

test_that("labels are constructed correctly for gramDataFrame",{
  expect_equal(labels(gramadf),
               as.character(1:5))
  expect_null(labels(gramadf, do.NULL = TRUE))
  expect_equal(labels(gramadf, prefix = "row"),
                paste0("row",1:5))
  expect_equal(labels(gramadflabs),
               letters[1:5])
  expect_equal(labels(gramamatlabs),
               letters[1:5])
  expect_error(gramData(amatlabs2),
               "names differ")
  expect_equal(gramData(amatlabs2, labels = letters[1:5]),
               gramamatlabs)

})