# This file is a part of the sorvi program (http://louhos.github.com/sorvi/)

# Copyright (C) 2010-2013 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# Datavaalit::council -----------------------------------------------------

#' council class
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{url}:}{Object of class \code{"character"}, specifying the URL.}
#'    \item{\code{members}:}{Object of class \code{"list"}.}
#'  }
#'
#' @name council
#' @rdname council
#' @aliases council-class
#' @exportClass council
#' @author Leo Lahti and Joona Lehtomaki

setClass("council", representation(url = "character",
                                   members = "list"))

setMethod("initialize", "council", function(.Object, url) {
  
  .Object@url <- url
  
  # TODO: Wrap in trycatch
  # TODO: implement limits
  members <- fromJSON(paste(readLines(url), collapse = ""))
  
  .Object@members <- data.frame()
  
  for (member in members$objects) {
    # Parse the municipality info
    municipality <- unlist(strsplit(member$municipality, "/"))
    municipality <- as.numeric(municipality[length(municipality)])
    # Parse first and last name
    # TODO: can't handle unusual names
    names <- unlist(strsplit(member$name, " "))
    #browser()
    .Object@members[member$name] <- new("candidate", id=member$id, 
                                        municipality=municipality, 
                                        first.name=names[1],
                                        last.name=names[2],
                                        party=member$party)
  }
  .Object
})

setMethod("names", "council", function(x) {
  return(names(x@members))  
})


#' GetMember
#' Get members of a particular council
#'
#' @param x object
#' @param name name of a person
#'
#' @return candidate object
#' @docType methods
#' 
#' @author Joona Lehtomaki \email{louhos@@googlegroups.com}
#' @export


setMethod("GetMember", "council", function(x, name) {
  
  if (class(x) != "council") {
    stop("Object must be an instance of class council")
  }
  
  if (!name %in% names(x)) {
    # TODO: should this return NULL instead?
    stop(paste("Person", name, "is not a member of given council"))
  } else {
    return(x@members[name][[1]]) 
  }  
})


setMethod("GetParties", "council", function(x) {
  
  if (class(x) != "council") {
    stop("Object must be an instance of class council")
  }
  
  return(table(sapply(x@members, function(x) {return(x@party)})))
})


# Datavaalit::candidate ---------------------------------------------------

# TODO: should the party name be checked for consistency?

setClass("candidate", representation(id = "numeric",
                                     municipality = "numeric",
                                     first.name = "character",
                                     last.name = "character",
                                     party = "character"))

setMethod("show", "candidate", function(object) {
  cat("First name: ", object@first.name, "\n", "Last name: ",  object@last.name, 
      "\n", "Municipality: ", object@municipality, "\n", "Id: ", object@id, "\n", 
      "Party: ", object@party)
})
