## "constants"
biocVersions <- c(paste(1, 6:9, sep="."),  # 1
                  paste(2, 0:15, sep=".")) # 2

biocDates <- paste(c("Apr", "Oct"), rep(2005:2014, each=2), sep="/")
names(biocDates) <- biocVersions

## 1.6 => May/2005
## 2.2 => May/2008
## 2.9 => Nov/2011
biocDates[c("1.6", "2.2", "2.9")] <- c("May/2005", "May/2008", "Nov/2011")

biocDates <- as.Date(paste0("1/", biocDates), format="%d/%b/%Y")


## functions
closestBiocRelease <- function(date, biocDates, biocVersions, devel=FALSE) {
  return(biocVersions[findInterval(as.Date(date), biocDates)+devel])
}
