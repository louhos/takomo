GetMML <- function (url, tmp.dir) {

  require(maptools)

  # Temporary file name
  items <- unlist(strsplit(gsub("/", "--", url), "--")); 
  local.zip <- items[[length(items)]]
  local.zip <- paste(tmp.dir, local.zip, sep = "/")

  # Create temporary directory and zip file destination
  if (length(dir(tmp.dir)) == 0) {
    system(paste("mkdir ", tmp.dir))  
  }

  # Download the zip file:
  download.file(url, destfile = local.zip)

  # Unzip the downloaded zip file
  unzip(local.zip, exdir = file.path(tmp.dir))

  # List the unzipped shape files
  shape.files <- dir(tmp.dir, pattern = ".shp$")

  shape.list <- list()
  for (f in shape.files) {
 
    # Read and preprocess shape file
    message(f)
    fnam <- paste(tmp.dir, "/", f, sep = "")
    sp <- maptools::readShapeSpatial(fnam)
    shape.list[[f]] <- PreprocessShapeMML(sp)

  }

  list(shape.list = shape.list, zipfile = local.zip, tmp.dir = tmp.dir)

}



#' Convert MML shape objects into RData format. For detailed example, see https://github.com/louhos/sorvi/wiki/Maanmittauslaitos
#'
#' Arguments:
#'   @param MML output from GetShapeMML(input.data.dir = ".")
#'   @param output.dir output data directory
#'
#' Returns:
#'   @return output data directory name
#'
#' @export
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

ConvertMMLToRData <- function (MML, output.dir) {

  if (length(dir(output.dir)) == 0) {
    nams <- unlist(strsplit(gsub("/", "--", output.dir), "--"))
    dir.orig <- getwd()
    k <- 1
    while (k <= length(nams)) {
      system(paste("mkdir", nams[[k]]))
      setwd(nams[[k]])
      k <- k+1
    }
    setwd(dir.orig)
  }

  for (item in names(MML)) {
    message(item)

    sp <- MML[[item]]    
      
    fnam <- paste(output.dir, item, ".RData", sep = "")
    fnam <- gsub(".shp", "", fnam)

    # Save the data
    message(paste("Saving data to ", fnam))
    save(sp, file = fnam)

  }

  output.dir

}





#' Preprocessing function for MML data 
#'
#' This script can be used to preprocess shape data 
#' obtained from Finnish geographical agency (Maanmittauslaitos, MML)
#' The data copyright is on (C) MML 2011.
#'
#' @aliases preprocess.shape.mml
#'
#' Arguments:
#'   @param sp Shape object (SpatialPolygonsDataFrame)
#'
#' Returns:
#'   @return Shape object (from SpatialPolygonsDataFrame class)
#'
#' @details The various Finland shape data files obtained from http://www.maanmittauslaitos.fi/aineistot-palvelut/digitaaliset-tuotteet/ilmaiset-aineistot/hankinta have been preprocessed with this function, and the preprocessed versions are included in soRvi package. You can also download shape files and apply this function.
#'
#' @export
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # Not run:
#' # load(url(paste(sorvi.data.url, "MML.rda", sep = ""))); 
#' # sp <- MML[[1]][[1]]; 
#' # sp2 <- PreprocessShapeMML(sp)
#'
#' @keywords utilities

PreprocessShapeMML <- function (sp) {

  # TODO: parseri, joka poimii vain oleelliset tiedot data.frameen
  # ja tekee tarpeelliset merkistomuunnokset.
  # names(sp)
  # "Suuralue"  "Suural_ni1" "Suural_ni2" 
  # "AVI"        "AVI_ni1"    "AVI_ni2"    
  # "Maakunta"   "Maaku_ni1" "Maaku_ni2"  
  # "Seutukunta" "Seutuk_ni1" "Seutuk_ni2" 
  # "Kunta"     "Kunta_ni1"  "Kunta_ni2" 
  # "Kieli_ni1"  "Kieli_ni2"  # Ruotsi/Suomi
  # "Kaupunki"   
  # "SHAPE_Leng" "SHAPE_Area"

  # Specify fields that need to converted into UTF-8
  nams <- colnames(sp@data)
  inds <- which(nams %in% c("AVI_ni1", "AVI_ni2", "Kieli_ni1", "Kieli_ni2", "TEXT1", "TEXT2", "TEXT3", "Suural_ni1", "Suural_ni2", "Maaku_ni1",  "Maaku_ni2", "Seutuk_ni1", "Seutuk_ni2", "Kunta_ni1", "Kunta_ni2"))
  dat <- sp@data

  # Convert encoding to UTF-8 for the text fields
  dat[, inds] <- apply(sp@data[, inds], 2, function (x) {iconv(x, from = "latin1", to = "UTF-8")})

  # Convert text fields back into factors as in the original data
  for (k in inds) { dat[, k] <- factor(dat[,k]) }

  ###################################

  # The name (ni1) is always given with the main language (Kieli_ni1)
  # For compatibility with other data sources, add fields where all
  # names are systematically listed in Finnish, no matter what is the
  # main language

  if (!is.null(sp$AVI_ni1)) {
    # All ni1 already in Finnish
    dat$AVI.FI <- iconv(sp$AVI_ni1, from = "latin1", to = "UTF-8")
  }

  if (!is.null(sp$Kieli_ni1)) {
    # All ni1 already in Finnish
    dat$Kieli.FI <- dat$Kieli_ni1
  }

  if (!is.null(sp$Suural_ni1)) {
    # All ni1 already in Finnish
    dat$Suuralue.FI   <- iconv(dat$Suural_ni1, from = "latin1", to = "UTF-8") 
  }

  if (!is.null(sp$Maaku_ni1)) {
    # All ni1 already in Finnish
    dat$Maakunta.FI   <- iconv(dat$Maaku_ni1, from = "latin1", to = "UTF-8")  
  }

  if (!is.null(sp$Seutuk_ni1)) { 

    # Combine ni1, ni2 to use systematically Finnish names
    kunta <- as.character(sp$Seutuk_ni1)
    inds <- sp$Kieli_ni1 == "Ruotsi" & !sp$Seutuk_ni2 == "N_A"
    kunta[inds] <- as.character(sp$Seutuk_ni2[inds])
    dat$Seutukunta.FI <- factor(iconv(kunta, from = "latin1", to = "UTF-8"))

  }

  if (!is.null(sp$Kunta_ni1)) {
    # Combine ni1, ni2 to use systematically Finnish names
    kunta <- as.character(sp$Kunta_ni1)
    inds <- sp$Kieli_ni1 == "Ruotsi" & !sp$Kunta_ni2 == "N_A"
    kunta[inds] <- as.character(sp$Kunta_ni2[inds])
    dat$Kunta.FI <- iconv(kunta, from = "latin1", to = "UTF-8")
    
    # Update municipality names
    dat$Kunta.FI <- ConvertMunicipalityNames(dat$Kunta.FI)
    dat$Kunta.FI <- factor(dat$Kunta.FI)

  }

  sp@data <- dat

  sp
}


