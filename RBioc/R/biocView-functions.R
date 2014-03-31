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
                                      biocVersions = "2.12") {

  counts <- sapply(biocVersions, function(x) {
                   ## ignore warnings about missing views
                   p <- suppressWarnings(
                          getPackagesInBiocView(view=view, rep=rep, biocVersion=x))
                   p <- sapply(p@packageList, function(x)x@Package)
                   return(length(p))
  })

  return(counts)
}

getBiocViewPackagesNames <- function(view,
                                     rep = c("BioCsoft", "BioCann",
                                             "BioCexp", "BioCextra"),
                                     biocVersion = "2.12") {

  ## ignore warnings about missing views
  p <- suppressWarnings(getPackagesInBiocView(view=view, rep=rep,
                                              biocVersion=biocVersion))
  p <- sapply(p@packageList, function(x)x@Package)

  return(p)
}

getDownloadsByBiocView <- function(downloadDf, view, biocVersions,
                                rep = "BioCsoft") {
  counts <- sapply(biocVersions, function(x) {
                   ## ignore warnings about missing views
                   p <- getBiocViewPackagesNames(view=view, rep=rep,
                                                 biocVersion=x)
                   isCurrentBiocView <- downloadDf$biocversion == x
                   isInView <- downloadDf$package %in% p
                   return(sum(isCurrentBiocView & isInView))
  })

  return(counts)
}

