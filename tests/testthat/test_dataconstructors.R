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

# Preparation adjacencyData ----------------------------

# Homogeneous networks
data("proteinInteraction")

protgraph <- graph_from_adjacency_matrix(proteinInteraction)

adjM <- adjacencyData(proteinInteraction, identity)
adjM2 <- adjacencyData(proteinInteraction)

test_that("Homogeneous matrices give correct adjacency.",{
  expect_equal(adjM@conversion, identity)
  expect_true(has_orig(adjM))
  expect_false(has_orig(adjM2))
  expect_equal(get_orig(adjM), proteinInteraction)
  expect_warning(get_orig(adjM2),
                 "No original data")
})

adjigraph <- adjacencyData(protgraph)
test_that("igraphs give correct adjacency",{
  expect_true(has_orig(adjigraph))
  expect_equal(get_orig(adjigraph),protgraph)
  expect_equal(as.matrix(adjigraph), proteinInteraction)
  expect_equal(igraph::as_adjacency_matrix,
               adjigraph@conversion)
  expect_equal(proteinInteraction,
               convert(protgraph, adjigraph))
})
