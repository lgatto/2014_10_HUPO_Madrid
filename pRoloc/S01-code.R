library("pRoloc")
library("pRolocdata")

fn <- dir(system.file("extdata", package = "pRoloc"), 
          full.names = TRUE, pattern = "pdres.rda")
load(fn)
fn <- dir(system.file("extdata", package = "pRoloc"), 
          full.names = TRUE, pattern = "params.rda")
load(fn)

fn <- dir(system.file("extdata", package = "pRoloc"), 
          full.names = TRUE, pattern = "params2.rda")
load(fn)
rm(fn)

data("tan2009r1")

spat <- tan2009r1

pdf("./figures/pca1.pdf")
plot2D(spat, fcol = NULL)
dev.off()

pdf("./figures/pca2.pdf")
plot2D(spat)
addLegend(tan2009r1, where = "bottomright", bty = "n")
dev.off()

setStockcol(paste0(getStockcol(), 80))

pdf("./figures/pca3.pdf")
plot2D(spat, fcol = "pd.2013")
addLegend(spat, fcol = "pd.2013", ncol = 2,
          where = "bottomright", bty = "n")
dev.off()
pdf("./figures/pca4.pdf")
plot2D(spat, fcol = "pd.markers")
addLegend(spat, fcol = "pd.markers", ncol = 2,
          where = "bottomright", bty = "n")
dev.off()

pdf("./figures/opt1.pdf")
levelPlot(params)
dev.off()
pdf("./figures/opt2.pdf")
plot(params)
dev.off()

w <- table(fData(tan2009r1)[, "pd.markers"])
w <- 1/w[names(w) != "unknown"]

spat <- svmClassification(spat, params2, 
                          class.weights = w,
                          fcol = "pd.markers")

ptsze <- exp(fData(spat)$svm.scores) - 1

pdf("./figures/svm.pdf")
plot2D(spat, fcol = "svm", cex = ptsze)
addLegend(spat, fcol = "svm", where = "bottomright",
          ncol = 2, bty = "n")
dev.off()
