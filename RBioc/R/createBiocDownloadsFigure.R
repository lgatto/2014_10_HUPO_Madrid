source("bioc-functions.R")
source("biocView-functions.R")
source("plot-functions.R")

## load bioc downloads
df <- read.csv(gzfile(file.path("..", "data", "biocdownloads.csv.gz")),
               stringsAsFactors=FALSE, header=FALSE)

colnames(df) <- c("pkgversion", "biocversion", "utc_offset", "protocol",
                  "biocrepo", "day_month_year", "url", "package", "bytes",
                  "errorcode", "ips", "pkgtype", "referer", "user_agent", "time",
                  "month_year", "method", "biocrepo_relurl")
## reduce df
df <- df[, c("package", "pkgversion", "biocversion", "day_month_year", "biocrepo_relurl")]

## clean up
df$day_month_year <- as.Date(df$day_month_year, format="%d/%b/%Y")
df$biocversion <- gsub("^/+packages/+([0-9.]+|release|devel)/+(bioc|extra).*", "\\1", df$biocrepo_relurl)
df$biocversion <- gsub("/packages/bioc/?|/scratch-repos/", "", df$biocversion)
df$biocversion <- gsub("stable", "release", df$biocversion)

## "impute" empty/devel/release biocversions
isRelease <- df$biocversion == "" | df$biocversion == "release"
isDevel <- df$biocversion == "devel"

df$biocversion[isRelease] <- closestBiocRelease(df$day_month_year[isRelease],
                                                biocDates=biocDates,
                                                biocVersions=biocVersions)
df$biocversion[isDevel] <- closestBiocRelease(df$day_month_year[isDevel],
                                              biocDates=biocDates,
                                              biocVersions=biocVersions,
                                              devel=TRUE)

# example call:
# createBiocViewDownloadFigure(df, views=c("Proteomics", "MassSpectrometry")
#                              biocVersions=c("2.6", "2.7", "2.8", "2.9",
#                                             "2.10", "2.11", "2.12", "2.13"))
createBiocViewDownloadFigure <- function(downloadDf, views, biocVersions,
                                 labels = biocVersions,
                                 cols = rainbow(length(views))) {
  counts <- vector(mode="list", length=length(views))

  for (i in seq(along=views)) {
    message(views[i])
    counts[[i]] <- getDownloadsByBiocView(downloadDf=downloadDf,
                                          view=views[i],
                                          biocVersions=biocVersions)
  }

  ## 2.12 has some really strange download counts (more than 10fold above the
  ## 2.13). We interpolate the download number of 2.12 using the mean of 2.11
  ## and 2.13
  bc212Idx <- match("2.12", biocVersions, nomatch=0)

  if (bc212Idx > 1 && bc212Idx < length(biocVersions)) {
    counts <- lapply(counts, function(x) {
      x[bc212Idx] <- mean(x[bc212Idx+c(-1, 1)])
      x
    })
  }

  invisible(plotCountsVsVersions(counts, views, labels=biocVersions,
                                 main="Development of BiocViews of time",
                                 ylab="Number of Downloads", cols=cols))
}

## run the following
versions <- paste(2, 6:13, sep=".")
labels <- paste(versions, biocDates[match(versions, biocVersions)], sep="\n")

png(file.path("..", "figures", "downloads_biocviews.png"), width=640, height=640)
createBiocViewDownloadFigure(df, views=c("Proteomics", "MassSpectrometry"),
                             biocVersions=versions, labels=labels, cols=2:3)
dev.off()

