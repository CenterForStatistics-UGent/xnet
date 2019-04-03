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
  expect_error(plot(mod, breaks = "hello", main = "Nah"),
               "breaks should be numeric")
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
  expect_identical(labels(mod)$g,
                   colnames(plotfit$val)[order(labels(plotfit$ddG))])
  expect_equal(fitted(mod)[, labels(plotfit$ddG)],
               plotfit$val)
  # Check output loo and ordering row dendro
  idr <- labels(plotloo$ddK)
  idc <- labels(plotloo$ddG)
  expect_identical(loo(mod, exclusion = "row")[idr,idc],
                   plotloo$val)
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

plotr <- plot(mod, rows = c("row4","row1","row3"))
plotrn <- plot(mod, rows = c(4,1,3))

plotc <- plot(mod, cols = c("col4","col1","col2","col3"))
plotcn <- plot(mod, cols = c(4,1,2,3))


test_that("Selection of values works",{
  expect_equal(sort(rownames(plotr$val)),
               c("row1","row3","row4"))
  expect_equal(plotr,
               plotrn)
  expect_equal(plotc,
               plotcn)
})

plotbest <- plot(mod, nbest = 4, dendro = "none")
thefits <- fitted(mod)
bestpos <- find_best_pos(thefits, 4)
test_that("find_best_pos works",{
  expect_equal(sort(thefits, decreasing = TRUE)[1:5],
               thefits[find_best_pos(thefits,5)])
  expect_equal(plotbest$val,
               thefits[unique(bestpos[,1]),
                       unique(bestpos[,2])])

})

# plot_grid ----------------------------------------
# Currently not tested as there's no output there.
tuned <- tune(mod)
test_that("plot_grid returns sensible errors",{
  expect_error(plot_grid(mod,
                         "x has to be a tskrrTune object"))
  expect_error(plot_grid(tuned, addlambda = 2),
               "single logical value")
  expect_error(plot_grid(tuned, addlambda = c(TRUE,TRUE)),
               "single logical value")
  expect_error(plot_grid(tuned, lambdapars = c(col = "red")))
})