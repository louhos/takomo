# This file is a part of the soRvi program (http://louhos.github.com/sorvi/)

# Copyright (C) 2010-2012 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Build a WMS service description for the GDAL WMS driver.
#'
#' WMS service description file is a XML file that describes required and
#' optional information on how to retrieve an exisiting WMS raster over the
#' web. The extent of the raster tile from the data source is defined by the
#' extent of a SpatialPolygonsDataFrame object (no other ways of
#' providing extent are implemented yet). Raster resolution (pixel size is
#' also provided as a parameter (there seems to be no way to query the original
#' resolution from the service.
#'
#' @param WMS WMS-object containing the necessary service information
#' @param layer the name of the layer to be fetched from the data source
#' @param extent SpatialPolygonsDataFrame object to be used to define the extent
#' @param resolution integer value of the resolution (CRS dependent)
#'
#' @return character XML string
#'
#' @note meant for package internal use only
#'
#' @references
#' \url{http://www.gdal.org/frmt_wms.html}
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.org}

BuildService <- function(WMS, layer, extent, resolution) {
  
  .InstallMarginal("XML")
  
  if (class(WMS) != 'WMS') {
    stop(paste('WMS unsupported type: ', class(WMS)))
  }
  
  # Extent is defined by the bounding box of the SpatialPolygonsObject provided
  # as extent parameter.
  # TODO: implement other ways of providing the extent
  if (class(extent) == 'SpatialPolygonsDataFrame') {
    bbox.extent <- bbox(extent)
    # Number of columns and rows (i.e. resolution) is defined by the real
    # width and height of the raster divided by the resolution parameter
    # (all this depends on the CRS, not very tested)
    ncols <- round((bbox.extent[1, 2] - bbox.extent[1, 1]) / resolution, 0)
    nrows <- round((bbox.extent[2, 2] - bbox.extent[2, 1]) / resolution, 0)
    # Set the extent corners
    ul.x <- bbox.extent[1, 1]
    ul.y <- bbox.extent[2, 2]
    lr.x <- bbox.extent[1, 2]
    lr.y <- bbox.extent[2, 1]
  } else {
    stop('Function only supports SpatialPolygonDataFrames')
  }
  
  # Create the XML structure based on the GDAL specification at
  # http://www.gdal.org/frmt_wms.html
  
  # Root level
  root <- XML::newXMLNode('GDAL_WMS')
  # Service level
  service <- XML::newXMLNode('Service', attrs=c(name='WMS'), parent=root)
  version <- XML::newXMLNode('Version', text='1.1.1', parent=service)
  server.url <- XML::newXMLNode('ServerUrl', text=WMS@base.url, parent=service)
  # TODO: CRS should not be hard coded
  srs <- XML::newXMLNode('SRS', text='EPSG:3067', parent=service)
  # Not sure if really needed
  image.format <- XML::newXMLNode('ImageFormat', text='image/tiff', parent=service)
  layers <- XML::newXMLNode('Layers', text=layer, parent=service)
  # Style is needed even if empty
  style <- XML::newXMLNode('Style', parent=service)
  
  # dw.node level
  dw.node <- XML::newXMLNode('DataWindow', parent=root)
  # Note that the following notation is minX, maxY, maxX, minY
  ulx.node <- XML::newXMLNode('UpperLeftX', text=ul.x, parent=dw.node)
  uly.node <- XML::newXMLNode('UpperLeftY', text=ul.y, parent=dw.node)
  lrx.node <- XML::newXMLNode('LowerRightX', text=lr.x, parent=dw.node)
  lry.node <- XML::newXMLNode('LowerRightY', text=lr.y, parent=dw.node)
  # TODO: although size is set here, it is not completely clear how the
  # native raster resolution on the WMS server is related to resolution
  # requested
  sizex.node <- XML::newXMLNode('SizeX', text=ncols, parent=dw.node)
  sizey.node <- XML::newXMLNode('Sizey', text=nrows, parent=dw.node)
  originy.node <- XML::newXMLNode('YOrigin', text='top', parent=dw.node)
  
  # Back to the root level
  projection.node <- XML::newXMLNode('Projection', text='EPSG:3067', parent=root)
  # Optional, this is also the default. Seems to be required in case where the
  # the raster requested is 3-band RGB raster.
  bands.count.node <- XML::newXMLNode('BandsCount', text='3', parent=root)
  # Optional, probably not needed here
  cache <- XML::newXMLNode('Cache', parent=root)
  
  # Save the created XML object, not providing a file path converts the object
  # into string.
  return(XML::saveXML(root))
}

#' Create a WMS object.
#'
#' WMS object fetches the relevant meta data from the WMS server (without
#' actually getting the data). This helper function is a thin wrapper to
#' create and return the appropriate WMS object.
#'
#' @param url string describing the URL of the WMS
#' @param cache string file system path controlling where the WMS cache is created (not implemented yet!)
#'
#' @return WMS object corresponding to the provided URL
#'
#' @seealso listWMSurls
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.org}
#' @export

PreprocessWMS <- function(url, cache='~') {
  
  .InstallMarginal("XML")
  
  wms <- new("WMS", base.url=url)
}

#' Get WMS capabilities (meta data)
#'
#' Function intiates a GetCapabilities query to a specified URL and returns
#' the response. Function will trigger an error if the response cannot be parsed
#' and/or is not valid.
#'
#' @param url a string url to target WMS
#'
#' @return XMLRootNode
#'
#' @note Meant for package internal use only.
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.org}

GetCapabilities <- function(url) {
  
  .InstallMarginal("XML")
  
  errormsg <- "Could not get capabilities for WMS"
  
  xmlroot <- tryCatch(XML::xmlRoot(XML::xmlTreeParse(url, isURL=TRUE)),
                      error=function(err) stop(paste(errormsg,err)))
  return(xmlroot)
}

#' Get WMS layers
#'
#' All WMS have a set of layers which correspond to the actual rasters. getLayers
#' list all layers associated with a specific WSM object.
#'
#' @param WMS object
#'
#' @note NOT IMPLEMENTED YET.
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.org}
#' @export

GetWMSlayers <- function(WMS) {
  # input.json <- GetURL("http://paikkatieto.ymparisto.fi/ArcGIS/rest/services/INSPIRE/SYKE_Maanpeite/MapServer/layers?f=json")
  # json <- fromJSON(input.json)
  stop("getLayers is not implemented yet!")
}

#' Get the URL associated with one of the standard WMSs.
#'
#' Standard WMSs are a group of well-known WMSs provided as part of soRvi. All
#' URLs have a provider (organization etc.) and a wmsname.
#'
#' @param provider string describing the provider
#' @param service string describing the WMS service requested
#'
#' @return string URL
#'
#' @seealso listWMSurls
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.org}
#' @export

LoadWMSurl <- function(provider, service) {
  
  .InstallMarginal("XML")
  
  errormsg <- "Could not load WMS definition XML, have you loaded soRvi?"
  
  xml.urls <- tryCatch(XML::xmlRoot(XML::xmlTreeParse(system.file("extdata/wms-urls.xml",
                                                        package="sorvi"))),
                       error = function(err) stop(paste(errormsg, err)))
  
  xpath.string <- paste("/services//provider[@name='", provider,
                        "']//service[@name='", service, "']//url[@type='WMS']",
                        sep="")
  url <- unlist(XML::getNodeSet(xml.urls, xpath.string))
  if (length(url) > 0){
    # FIXME: this is just waiting to get broken...
    return(url[4][[1]])
  } else {
    stop(paste("Could not find provider", provider, "with wmsname", service))
  }
}

#' Get WMS raster.
#'
#' After the WMS object is set up, it can be queried in order the get the
#' actual data (raster map). Function uses GDAL to read the remote WMS by first
#' creating the service description.
#'
#' @param WMS a WMS object containing the necessary service informations
#' @param layer string name of the layer to be fetched from the data source
#' @param extent SpatialPolygonsDataFrame object to be used to define the extent
#' @param resolution integer value of the resolution (CRS dependent)
#'
#' @seealso buildServiceDesc
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.org}
#' @export

GetWMSraster <- function(WMS, layer, extent, resolution) {
  
  .InstallMarginal("rgdal")
  
  # Use GDAL to read in the value, rgdal::readGDAL returns a SpatialObject
  wms.description <- BuildService(WMS, layer, extent, resolution)
  wms.raster <- rgdal::readGDAL(wms.description)
  return(wms.raster)
}

#' List all the URLs associated with one of the standard WMSs.
#'
#' Standard WMSs are a group of well-known WMSs provided as part of soRvi. All
#' URLs have a provider (organization etc.) and a WMS name and an URL.
#'
#' @return SpatialObject from the target WMS raster
#'
#' @seealso buildServiceDesc listWMSurls
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.org}
#' @export

ListWMSurls <- function() {
  
  .InstallMarginal("XML")
  
  errormsg <- "Could not load WMS definition XML, have you loaded soRvi?"
  
  xml.urls <- tryCatch(XML::xmlRoot(XML::xmlTreeParse(system.file("extdata/wms-urls.xml",
                                                        package="sorvi"))),
                       error = function(err) stop(paste(errormsg, err)))
  
  # FIXME: url node should not be hard coded
  # Loop through the XML structure and print out the provider name and WMS names
  # and URLs
  for (i in 1:length(xml.urls[[1]])) {
    provider <- ""
    if (XML::xmlName(xml.urls[[1]]) == "provider") {
      provider  <- XML::xmlAttrs(xml.urls[[1]])[[1]]
    }
    if (XML::xmlName(xml.urls[[1]][[i]]) == "service") {
      cat("Provider: ", provider, "\n")
      cat("Service: ", XML::xmlAttrs(xml.urls[[1]][[i]]), "\n")
      for (item in XML::xmlChildren(xml.urls[[1]][[i]])) {
        if (length(XML::xmlAttrs(item)) > 0 ) {
          cat("\t", XML::xmlName(item), " (", XML::xmlAttrs(item)[[1]], ")",
              ": ", XML::xmlValue(item), "\n", sep="")
        } else {
          cat("\t", XML::xmlName(item), ": ", XML::xmlValue(item), "\n", sep="")
        }
      }
    }
  }
}