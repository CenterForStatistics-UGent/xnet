context("Imputation of values")


# Test loss functions --------------------------------

x <- matrix(c(1,0,0,1,0,0), ncol = 2)
y <- matrix(c(0.9,0.1,0.2,0.4,0.1,0.6), ncol = 2)
yna <- y
yna[3] <- NA

# Manually calculation of the AUC
tab <- table(x,y)
truepos <- rev(cumsum(rev(tab[2,])))
falsepos <- rev(cumsum(rev(tab[1,])))
sens = c(truepos / sum(tab[2,]), 0)
omspec = c(falsepos / sum(tab[1,]), 0)
height = (sens[-1]+sens[-length(sens)]) / 2
width = -diff(omspec)
auc <- sum(height*width)

test_that("loss functions give correct result",{
  expect_equal(mean((y - x)^2), loss_mse(x,y))
  expect_equal(loss_auc(x,y), 1 - auc)
  expect_equal(loss_mse(x,yna), NA_real_)
  expect_equal(mean((yna - x)^2, na.rm = TRUE),
               loss_mse(x,yna, na.rm = TRUE))

})


