context("Plot functions")

dfile <- system.file("testdata","testdata.rda", package = "xnet")

load(dfile)
lambdak <- 0.01
lambdag <- 1.5
mod <- tskrr(Y, K, G, lambda = c(lambdak, lambdag))
tunedmod <- tune(mod)

dfile <- system.file("testdata","testdataH.rda", package = "xnet")
load(dfile)
modh <- tskrr(Yh, Kh, lambda = lambdak)
mods <- tskrr(Ys, Kh, lambda = lambdak)
tunedmodh <- tune(modh)

test_that("Errors are generated correctly for plot",{
  expect_error(plot(mod, legend = 1),
               "should be a logical value")
  expect_error(plot(mod, rows = c(0.1,2,4)),
               "contains non-integer values")
  expect_error(plot(mod, cols = c(2,1,0.4,3)),
               "contains non-integer values")
  expect_error(plot(mod, rows = c("row1","row6")),
               "Not all row labels")
  expect_error(plot(mod, cols = c("row1","row6")),
               "Not all column labels")
  expect_error(plot(mod, breaks = 30, col = rainbow(25)),
               "Not enough colors for 30 breaks")
  expect_error(plot(mod, col = rainbow(5),
                    breaks = seq(0,1,by=0.1)),
               "breaks should be 1 value longer than colors")
  expect_warning(plot(mod, col = rainbow(10), breaks = 5),
                 "The last 6 colors are ignored")
  expect_error(plot(mod, breaks = 1),
                "at least 2 breaks")
})

plotres <- plot(mod, which = "residuals",
                dendro = "none")

plotfit <- plot(mod, dendro = "col", col = rainbow(5))

plotloo <- plot(mod, dendro = "both", which = "loo",
                exclusion = "row")

test_that("Values are correctly processed",{
  # Check output residuals
  expect_equal(residuals(mod),
               plotres$val)
  expect_null(plotres$ddK)
  expect_null(plotres$ddG)
  # Check output fitted and ordering col dendro
  expect_identical(labels(mod)$g[labels(plotfit$ddG)],
                   colnames(plotfit$val))
  expect_equal(fitted(mod)[, labels(plotfit$ddG)],
               plotfit$val)
  # Check output loo and ordering row dendro
  idr <- labels(plotloo$ddK)
  idc <- labels(plotloo$ddG)
  expect_identical(loo(mod, exclusion = "row")[idr,idc],
                   unname(plotloo$val))
})

plotbr <- plot(mod, breaks = 6)
plotbrm <- plot(mod, breaks = seq(0,1,by = 0.1))

test_that("Breaks and colors are correctly processed",{
  expect_identical(plotfit$col,
                   rainbow(5))
  expect_equal(length(plotfit$breaks), 6)
  expect_equal(plotbr$col,
               rev(heat.colors(5)))
  expect_equal(plotbrm$col,
               rev(heat.colors(10)))
  expect_identical(plotbrm$breaks,
                   seq(0,1,by=0.1))

})