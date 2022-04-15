# Test to see whether it all works
#---------------------------------
library(dplyr)
library(kernlab)
# Uncomment what you need
# datadir <- "C:/users/jfmeys/STACK/Work/Packages/2018_xnet/data/Pollinatie"
 datadir <- "D:/STACK/Stack_files/Work/Packages/2018_xnet/data/Pollinatie"

# Collect data. We need to take care about not introducing mismatches
# between the traits and adjacency matrix
Y <- as.matrix(read.csv(
  file.path(datadir, "network.csv"),
  row.names = 1
))
colnames(Y) <- gsub("\\."," ",colnames(Y))

# T
traits_plants <- read.csv(file.path(datadir,"traits_plants.csv"))

traits_poll <- read.csv(file.path(datadir,"traits_pollinators.csv")) %>%
  na.omit()

plants <- intersect(colnames(Y), traits_plants$Plant)
pollinators <- intersect(rownames(Y),
                         traits_poll$Pollinator)

# Select Y
Y <- Y[pollinators, plants]

traits_plants <- dplyr::filter(traits_plants,
                               Plant %in% plants)
sel_plants <- select(traits_plants,
                     -c(Species, Plant, Number, Genus))

traits_poll <- dplyr::filter(traits_poll,
                             Pollinator %in% pollinators)
sel_poll <- select(traits_poll,
                   -c(Species, Nummer, Pollinator, Nov, Dec, Jan))

# Create the gram data
# It needs a formula, and also a set of labels
# If not, tskrr() will get cranky.
# Also remove columns with identical values, as SD = 0 for those.
g_plants <- gramData(sel_plants,
                    labels = traits_plants$Plant,
                    kernel = polydot,
                    keep.gram = TRUE,
                    scale = TRUE)

g_poll <- gramData(sel_poll,
                   keep.gram = TRUE,
                   kernel = polydot,
                   labels = traits_poll$Pollinator)

mod <- tskrr(Y, g_poll, g_plants)

imp <- feature_importance(mod)

imp
