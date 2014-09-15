source("bioc-functions.R")
source("biocView-functions.R")
source("plot-functions.R")

# example call:
# createBiocViewFigure(views=c("Proteomics", "MassSpectrometry", "MassSpectrometryData"),
#                      rep=c("BioCsoft", "BioCsoft", "BioCexp"),
#                      biocVersions=c("2.6", "2.7", "2.8", "2.9", "2.10",
#                                     "2.11", "2.12", "2.13"))
createBiocViewFigure <- function(views, biocVersions,
                                 rep = "BioCsoft",
                                 labels = biocVersions,
                                 cols = rainbow(length(views))) {
  rep <- rep_len(rep, length(views))
  counts <- vector(mode="list", length=length(views))

  for (i in seq(along=views)) {
    message(views[i])
    counts[[i]] <- getBiocViewPackagesNumber(views[i], rep[i],
                                             biocVersions=biocVersions)
  }

  invisible(plotCountsVsVersions(counts, views=views, labels=biocVersions,
                                 main="Development of BiocViews of time",
                                 ylab="Number of Packages", cols=cols))
}

## run the following
versions <- paste(2, 6:14, sep=".")
labels <- paste(versions, biocDates[match(versions, biocVersions)], sep="\n")
views <- c("Proteomics", "MassSpectrometry", "MassSpectrometryData")

png(file.path("..", "figures", "development_biocviews_sep2014.png"), width=640, height=640)
counts <- createBiocViewFigure(views=views,
                               rep=c("BioCsoft", "BioCsoft", "BioCexp"),
                               biocVersions=versions, labels=labels)
dev.off()

counts <- do.call(cbind, counts)
colnames(counts) <- views

dir.create(file.path("..", "output"), showWarnings = FALSE)
write.csv(counts,
          file=file.path("..", "output", "development_biocviews_sep2014.csv"))
