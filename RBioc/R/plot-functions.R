plotCountsVsVersions <- function(counts, views, labels,
                                 cols = rainbow(length(views)), ...) {

  maxCount <- max(unlist(lapply(counts, max)))

  xlim <- c(1, length(labels))
  ylim <- c(0, maxCount)

  plot(NA, xlim=xlim, ylim=ylim, xlab="", xaxt="n", ...)
  axis(side=1, at=seq_along(labels), labels=NA)
  mtext(text=labels, at=seq_along(labels), line=2, side=1)
  title(xlab="Bioconductor Versions", line=4)

  for (i in seq(along=views)) {
    lines(counts[[i]], col=cols[i], type="b", pch=15)
  }

  legend("topleft", legend=views, col=cols, pch=15)

  invisible(counts)
}
