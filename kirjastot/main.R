


GetLibraryStats <- function (year, verbose = TRUE) {

  # Suomen kirjastojen vuositilastot 1999-2011
  # http://tilastot.kirjastot.fi/fi-FI/vuositilastot.aspx

  library(gdata)

  # Excel (XML also available)
  url <- paste("http://tilastot.kirjastot.fi/LibraryStatistics/File/YearlyReport_Y", year, "N2.xls?AreaTypeKey=Y", year, "N2&CultureId=fi-FI", sep = "")

  if (verbose) {message(paste("Downloading data from", url))}

  df <- read.xls("http://tilastot.kirjastot.fi/LibraryStatistics/File/YearlyReport_Y2011N2.xls?AreaTypeKey=Y2011N2&CultureId=fi-FI")

  colnames(df) <- as.character(unlist(df[1,]))
  df <- df[-1,]

  df
}


df <- GetLibraryStats(year)