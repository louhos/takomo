.get.format <- Vectorize(function(x) {
  if (nchar(x) == 4) {
    return("%H%M")
  } else if(nchar(x) == 6) {
    return("%H%M%S")
  } else {
    warning(paste("Time String should be either HHMM or HHMMSS:", x))
    return(NA)
  }
}, SIMPLIFY=TRUE, USE.NAMES=FALSE)

diffschedule <- function(t1, t2, ...) {

  # Pad the time Strings of needed
  #t1 <- padd.time(t1)
  #t2 <- padd.time(t2)

  t1 <- as.POSIXlt(t1, format="%d/%M/%Y %H%M%S")
  t2 <- as.POSIXlt(t2, format="%d/%M/%Y %H%M%S")
  
  return(as.vector(difftime(t2, t1, ...)))
}

padd.time <- Vectorize(function(x) {
  # Bounce NAs back
  if (is.na(x)) {
    return(NA)
  }
  # If x is not a string, try to coerce it into one
  if (class(x) != "character") {
    x <- as.character(x)
  }
  # Get the length of the time string
  x.length <- nchar(x)
  if (x.length == 3 | x.length == 5) {
    x <- paste0("0", x)
  } else if (x.length != 4 & x.length != 6) {
    warning(paste("Time String should be either HHMM or HHMMSS:", x))
    return(NA)
  }
  return(x)
}, SIMPLIFY=TRUE, USE.NAMES=FALSE)

trim.str <- function(str) {
  
  # Substitute several white spaces with a single whitespace
  str <- gsub("\\s+", " ", str)
  # returns string w/o leading or trailing whitespace
  str <- gsub("^\\s+|\\s+$", "", str)
  return(str)
}