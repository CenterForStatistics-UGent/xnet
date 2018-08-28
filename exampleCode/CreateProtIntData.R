ddir <- "E:/STACK/Work/Packages/2018_xnet/DataHomogenous/Data"

fn <- dir(ddir)
set.seed(100)
sel <- sample(769,150)

for(i in fn){
  obj <- gsub("\\.txt","",i)
  tmp <- as.matrix(read.table(file.path(ddir,i),
                              header = TRUE,
                              row.names = 1))[sel,sel]
  colnames(tmp) <- gsub(".","-",colnames(tmp),fixed = TRUE)
  assign(paste0(obj,"_sc"),tmp)
}
proteinInteraction <- admat_sc

save(proteinInteraction, Kmat_y2h_sc, file = "pkg/xnet/data/proteinInteraction.rda")