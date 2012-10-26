#################################################################
# 
# File:         read.px.R
# Purpose:      reads a PC-Axis file into R
#
# Created:      20110618
# Authors:      fvf, cjgb, opl
#
# Modifications: 
#		20111210, cjgb: in the data string, "-" may represent the value 0
#		20111210, cjgb: fixing the strsplit when the split character is contained in the data part
#
#################################################################

read.px <- function(filename, encoding = "latin1", 
                    na.strings = c('"."', '".."', '"..."', '"...."')) {

#filename <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/610_kvaa_2008_2009-10-30_tau_137_fi.px" # OK
#filename <- "http://pxweb2.stat.fi/Database/StatFin/vaa/kvaa/2008_04/410_kvaa_2008_2009-11-02_tau_123_fi.px" # CRaSH
#encoding = "latin1";
#na.strings = c('"."', '".."', '"..."', '"...."')

    ## auxiliary functions ##

    unquote <- function(x){
        gsub('\\"', "", x)
    }

    clean.spaces <- function(x){
        gsub("^[[:space:]]+|[[:space:]]+$", "", x) # elimina blancos por delante|detrás
    }

    get.attributes <- function(x){
        x <- gsub( "([A-Z-]*)\\((.*)\\).*", "\\1;\\2", x ) ## parte etiqueta y atributo con ";"
        x <- strsplit( x, ";" )
        x <- lapply( x, function( x ) c( x, rep( "value", 2 - length( x ) ) ) )
        x <- do.call( rbind, x )
        x[,2] <- unquote( x[,2] )
        clean.spaces( x )
    }

    break.clean <- function( x, sep = '\\"' ) {
        x <- strsplit( x, sep )[[1]]
        if (sep != " ") x <- clean.spaces( x )
        x <- x[ x != "" ]
        x <- x[ x != "," ]
        x
    }

    make.list <- function( dat, my.label ){
        dat <- subset( dat, label == my.label, select = c( attribute, value ) )

        my.list <- as.list( dat$value )
        names( my.list ) <- dat$attribute
        my.list
    }

    ## end: auxiliary functions ##
    a <- scan(filename, what = "character", sep = "\n", quiet = TRUE, fileEncoding = encoding)
    a <- paste(a, collapse = " ")	## " " necesario para que no junte lineas en DATA
    a <- unlist(strsplit(a, ";"))	## ; is the logical line end in px files

    a <- sub( "=", "//=//", a )

    # ------------------------------------------

    # CRASH:
    # a <- do.call(rbind, strsplit(a, "//=//" ))
    #
    # FIX by antagomir 23.9.2012
    a <- a[!a == " "]
    a2 <- lapply(a, function (x) {strsplit(x, "//=//")})
    attribs <- get.attributes(sapply(a2, function (x) {x[[1]][[1]]}))
    vals <- sapply(a2, function (x) {x[[1]][[2]]})
    #a3 <- unlist(strsplit(a2[[32]][[2]], " "))
    #a4 <- as.numeric(gsub("\"-\"", "0", a3))
    a <- data.frame(cbind(attribs, vals))

    # ------------------------------------------

    colnames(a) <- c("label", "attribute", "value")

    a$label     <- make.names(a$label)
    a$attribute <- make.names(a$attribute)
    a$value     <- as.character(a$value)

    ## build a px object: list with px class attribute ##

    px <- sapply(unique( a$label ), function(label) make.list(a, label), simplify = FALSE)

    # turns data values into an R vector
    px$STUB$value    <- make.names(break.clean(px$STUB$value))
    px$HEADING$value <- make.names(break.clean(px$HEADING$value))

    px$VALUES <- lapply(px$VALUES, break.clean )
    px$CODES  <- lapply(px$CODES,  break.clean )

    tmp <- gsub('"-"', 0, px$DATA$value)        # 0 can be encoded as "-"
    dat <- textConnection(tmp)                  #much faster than with cleanDat (strsplit)
    # dat <- textConnection(px$DATA$value) #much faster than with cleanDat (strsplit)
    px$DATA$value <- scan(dat, na.strings = na.strings, quiet = TRUE)
    close(dat)
    
    class(px) <- "px"
    px

}


