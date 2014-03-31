## taken from https://gist.github.com/lgatto/4493747
getPackagesInBiocView <- function(view,
                                  rep = c("BioCsoft", "BioCann",
                                    "BioCexp", "BioCextra"),
                                  biocVersion = "2.12") {
  require("biocViews")
  data(biocViewsVocab)
  rep <- match.arg(rep)
  biocMirror <- getOption("BioC_mirror", "http://bioconductor.org")
  biocPaths <- switch(rep,         
                      BioCsoft = "bioc",
                      BioCann = "data/annotation", 
                      BioCexp = "data/experiment",
                      BioCextra = "extra")   
  rep <- paste(biocMirror,
               "packages",
               biocVersion,
               biocPaths, 
               sep = "/")

  bv <- getBiocViews(rep, biocViewsVocab, "NoViewProvided")
  
  if (!view %in% names(bv)) {
    warning("BiocView ", view, " not found.")
    return(NULL)
  }  
  return(bv[[view]])
}

getBiocViewPackagesNumber <- function(view, 
                                      rep = c("BioCsoft", "BioCann",
                                              "BioCexp", "BioCextra"),
                                      biocVersions = c("2.12", "2.13")) {

  counts <- sapply(biocVersions, function(x) {
                   ## ignore warnings about missing views
                   p <- suppressWarnings(
                          getPackagesInBiocView(view=view, rep=rep, biocVersion=x))
                   p <- sapply(p@packageList, function(x)x@Package)
                   return(length(p))
  })

  return(counts)
}

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

  maxCount <- max(unlist(lapply(counts, max)))

  xlim <- c(1, length(biocVersions))
  ylim <- c(0, maxCount)

  plot(NA, xlim=xlim, ylim=ylim,
       main="Development of BiocViews over time",
       xlab="", ylab="Number of Packages",
       xaxt="n")
  axis(side=1, at=seq_along(biocVersions), labels=NA)
  mtext(text=labels, at=seq_along(biocVersions), line=2, side=1)
  title(xlab="Bioconductor Versions", line=4)

  for (i in seq(along=views)) {
    lines(counts[[i]], col=cols[i], type="b", pch=15)
  }

  legend("topleft", legend=views, col=cols, pch=15)

  invisible(counts)
}

## run the following
biocVersions <- paste(2, 6:13, sep=".")
dates <- c("Apr 2010", "Oct 2010", # 2.6, 2.7
           "Apr 2011", "Nov 2011", # 2.8, 2.9
           "Apr 2012", "Oct 2012", # 2.10, 2.11
           "Apr 2013", "Oct 2013") # 2.12, 2.13
labels <- paste(biocVersions, dates, sep="\n")

png(file.path("figures", "development_biocviews.png"), width=640, height=640)
createBiocViewFigure(views=c("Proteomics", "MassSpectrometry", "MassSpectrometryData"),
                     rep=c("BioCsoft", "BioCsoft", "BioCexp"),
                     biocVersions=biocVersions, labels=labels)
dev.off()
