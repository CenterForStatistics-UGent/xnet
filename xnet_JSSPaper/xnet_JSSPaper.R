# RScript containing the code mentioned in the paper
# Network Prediction with Two-Step Kernel Ridge Regression: The R package xnet
# The package xnet can be installed from CRAN.
# -------------------------------------------------------------------------------

## ----setup----------------------------------------------------------
library(xnet)
data(drugtarget)


## ----split data------------------------------------------------------
data(drugtarget)
interaction <- drugTargetInteraction[, -c(1:3)]
traindrugSim <- drugSim[-c(1:3), -c(1:3)]
testdrugSim <- drugSim[1:3, -c(1:3)]


## ----fit a heterogeneous model-----------------------------------------------------
trained <- tune(interaction, targetSim, traindrugSim,
                lim = list(k = c(1e-4,10), g = c(1e-3,10)),
                ngrid = list(k = 20, g = 10),
                fun = loss_auc,
                exclusion = "interaction", replaceby0 = TRUE)
lambda(trained)


## Look at the output----------------------------------------------------------------------------------
trained


## ----modelplot, fig.cap="Heatmap of the residuals for the training model on interactions between drugs and neural receptors. Negative values indicate possible interactions not present in the original data."----
plot(trained, dendro = "none",
     which = "residuals", cols = 10:30, rows = 10:25,
     main = "Residuals for drug target interaction.")


## ----lossplot, fig.cap = "Contour plot of the loss values for different combinations of lambda values for the training model on drug-target interaction. The red cross indicates the lowest loss value."----
plot_grid(trained, main = "Loss surface for drug-target interaction")


## Looking at the similarity matrix-------------------------------------------------------------------------
testdrugSim[, 1:6]


## ---- Calculate the AUC-------------------------------------------------
library(pROC)
newpreds <- predict(trained, g = testdrugSim)
pvec <- as.vector(newpreds)
rvec <- as.vector(drugTargetInteraction[, 1:3])
curve <- roc(rvec, pvec)
auc(curve)


## Illustration of imputation--------------------------------------
data("proteinInteraction")
idrow <- seq(10,150, by = 10)
idcol <- seq(5, 145, by = 10)
proteiny <- proteinInteraction
proteiny[idrow,idcol] <- proteiny[idcol,idrow] <- NA

imputed <- impute_tskrr(y = proteiny, k = Kmat_y2h_sc,
                        start = 0.5)


## ---- plotimpute, fig.cap="Visualization of the imputed values for the missing values in the protein-protein interaction dataset."----
id <- is_imputed(imputed)
rowid <- rowSums(id) > 0
colid <- colSums(id) > 0
plot(imputed, rows = rowid, cols = colid,
     dendro = "none", which = "response")


## ---- Calculating AUC for imputation--------------------------------
id <- which_imputed(imputed)
orig <- proteinInteraction[id]
imp <- response(imputed)[id]
curve <- roc(orig, imp)
auc(curve)