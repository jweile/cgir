


#' Redirect messages to a log file
#' 
#' Redirects any messages (e.g. errors) to a log file. Doing so at the start of your script is
#' absolutely vital! Otherwise any potential info messages, warnings or errors can corrupt your
#' HTTP response
#' @param filename the log file to which messages will be redirected. Must be writable 
#'    for apache user!
#' @export
setMessageSink <- function(filename="msg.log") {
	sink(file(filename, open="w"),type="message")
}


#' Create universally unique ID (UUIDv4)
#' 
#' Creates a universally unique identifier compatible with the UUID v4.0 standard.
#' See \url{https://en.wikipedia.org/wiki/Universally_unique_identifier#Version_4_(random)}
#' 
#' @return the UUID as a character string
#' @export
makeUUID <- function() {
	baseuuid <- paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse="")
	paste(
	    substr(baseuuid,1,8),"-",
	    substr(baseuuid,9,12),"-","4",
	    substr(baseuuid,13,15),"-",
	    sample(c("8","9","a","b"),1),
	    substr(baseuuid,16,18),"-",
	    substr(baseuuid,19,30),
	    sep="", collapse=""
	)
}

#' Read GET data from HTTP request
#' 
#' Reads the GET data transmitted via an HTTP request and returns it as a named character vector.
#' 
#' @return a named list containing the GET data
#' @export
#' @examples
#' getData <- readGET()
#' if ("foo" %in% names(getData)) {
#'    foo <- getData[["foo"]]
#' }
readGET <- function() {
	getTable <- do.call(rbind,strsplit(strsplit(Sys.getenv("QUERY_STRING"),"&")[[1]],"="))
	getData <- lapply(getTable[,2],URLdecode)
	names(getData) <- getTable[,1]
	getData
}

#' Read POST data from HTTP request
#' 
#' Reads the POST data transmitted via an HTTP request and returns it as a named character vector.
#' 
#' @return a named list containing the POST data or NULL if none exists
#' @export
#' @examples
#' postData <- readPOST()
#' if ("foobar" %in% names(postData)) {
#'    foobar <- postData[["foobar"]]
#' }
readPOST <- function() {
	f <- file("stdin",open="r")
	postRaw <- readLines(f)
	close(f)
	if (!is.null(postRaw) && length(postRaw) > 0 && nchar(postRaw) > 0) {
	  postTable <- do.call(rbind,strsplit(strsplit(postRaw,"&")[[1]],"="))
	  postData <- lapply(postTable[,2],URLdecode)
	  names(postData) <- postTable[,1]
	} else {
	  postData <- NULL
	}
	postData
}


#' Interpolate variables in a file
#' 
#' This function reads the contents of a file and performs a search-and-replace
#' on a set of given variable names with given values. In the context of a CGI script,
#' this function is useful for interpolating HTML templates and using them as the HTTP response.
#' variable occurrences are marked with a sentinel character, which defaults to "\%". 
#' 
#' For example, given the text "Hello \%foo" and the variable foo="World", the interpolation
#' will produce the text "Hello World"
#' 
#' @param filename the name of the file containing the text to be interpolated
#' @param values a named vector, in which the names are used as the variable names, and the 
#'     values as the variable values.
#' @param sentinel a sentinel character indicating a variable in the text. Defaults to "\%".
#' @return the interpolated text.
#' @export
#' @examples
#' error <- "Insufficient amount of coffee!"
#' errorHTML <- interpolate("../../html/app/error.html",c(message=error))
#' respondHTML(errorHTML)
interpolate <- function(filename, values,sentinel="%") {
	con <- file(filename,open="r")
	string <- paste(readLines(con),collapse="\n")
	close(con)
	for (name in names(values)) {
		string <- gsub(paste(sentinel,name,sep=""),values[[name]],string)
	}
	string
}


#' Respond to HTTP request using a given MIME type
#' 
#' Responds to an HTTP request using a given MIME type
#' 
#' @param content a character string containing the content
#' @param mime a character string containing the MIME type (e.g. "text/html" or "image/gif")
#' @export
#' @examples
#' error <- "Insufficient amount of coffee!"
#' errorHTML <- interpolate("../../html/app/error.html",c(message=error))
#' respondHTML(errorHTML)
respond <- function(content,mime) {
	#Content type MUST be followed by two newline characters!!
	cat(paste0("Content-type: ",mime,"\n\n"))
	cat(content)
	return(invisible(NULL))
}

#' Respond to HTTP request using HTML
#' 
#' Responds to an HTTP request using a given HTML content
#' 
#' @param html a character string containing the HTML content
#' @export
#' @examples
#' error <- "Insufficient amount of coffee!"
#' errorHTML <- interpolate("../../html/app/error.html",c(message=error))
#' respondHTML(errorHTML)
respondHTML <- function(html) {
	respond(html,"text/html")
	# cat("Content-type: text/html\n\n")
	# cat(html)
	# return(invisible(NULL))
}


#' Respond to HTTP request using HTML interpolated from a template file
#' 
#' Responds to an HTTP request using a given HTML file as well as interpolated content
#' 
#' @param templateFile the name of the file containing the HTML content
#' @param values a named vector, in which the names are used as the variable names, and the 
#'     values as the variable values.
#' @export
#' @examples
#' error <- "Insufficient amount of coffee!"
#' respondTemplateHTML("../../html/app/error.html",c(message=error))
respondTemplateHTML <- function(templateFile,values) {
	respondHTML(interpolate(templateFile,values))
}

#' Respond to HTTP request using JSON
#' 
#' Responds to an HTTP request using JSON formatted data
#' 
#' @param content a named list, which will be serialized to JSON using RJSONIO
#' @export
#' @examples
#' data <- list(foo="bar",baz=1:5)
#' respondJSON(data)
respondJSON <- function(content) {
	library(RJSONIO)
	json <- toJSON(content)
	respond(json,"application/json")
	# cat("Content-type: application/json\n\n")
	# cat(json)
	# return(invisible(NULL))
}

#' Respond to HTTP request using plain text
#' 
#' Responds to an HTTP request using plain text
#' 
#' @param content a character string to respond
#' @export
#' @examples
#' respondTEXT("Hello World!")
respondTEXT <- function(text) {
	respond(text,"text/plain")
	# cat("Content-type: text/plain\n\n")
	# cat(text)
	# return(invisible(NULL))
}

#' Respond to HTTP request using binary data in a given MIME type
#' 
#' Responds to an HTTP request using binary data a given MIME type
#' 
#' @param binFile the name of a file containing the binary data
#' @param mime a character string containing the MIME type (e.g. "image/gif")
#' @param filename the filename to be used in the HTTP header
#' @param download logical. whether or not to mark this as a download
#' @export
#' @examples
#' respondBinary(imageFile,"image/png","image.png",download=TRUE)
respondBinary <- function(binFile,mime,filename,download=FALSE) {
	dispo <- if (download) "attachment" else "inline"
	cat(paste0("Content-Disposition: ",dispo,"; filename=\"",filename,"\"\n"))
	cat(paste0("Content-type: ",mime,"\n\n"))
	system(paste("cat",binFile))
}

#' Respond to HTTP request with PNG image data
#' 
#' Responds to an HTTP request with PNG image data from a given PNG file
#' 
#' @param pngFile the name of a file containing the binary data
#' @param filename the filename to be used in the HTTP header. Defaults to image.png
#' @param download whether or not to treat this as a download. Defaults to FALSE
#' @export
respondPNG <- function(pngFile,filename="image.png",download=FALSE) {
	respondBinary(pngFile,"image/png",filename,download)
}

#' Respond to HTTP request with PDF data
#' 
#' Responds to an HTTP request with PDF data from a given PDF file
#' 
#' @param pdfFile the name of a file containing the binary datav
#' @param filename the filename to be used in the HTTP header. Defaults to image.pdf
#' @param download whether or not to treat this as a download. Defaults to FALSE.
#' @export
respondPDF <- function(pdfFile,filename="image.pdf",download=FALSE) {
	respondBinary(pdfFile,"application/pdf",filename,download)
}

#' Respond to HTTP request with SVG data
#' 
#' Responds to an HTTP request with SVG data from a given SVG file
#' 
#' @param svgFile the name of a file containing the binary data
#' @param filename the filename to be used in the HTTP header. Defaults to image.svg
#' @param download whether or not to treat this as a download. Defaults to FALSE.
#' @export
respondSVG <- function(svgFile,filename="image.svg",download=FALSE) {
	respondBinary(svgFile,"image/svg+xml",filename,download)
}


#' Respond to HTTP request with a 400:BadRequest error
#' 
#' Responds to an HTTP request with 400:BadRequest error
#' 
#' @param message the error message
#' @export
respond400 <- function(message) {
	cat("Status: 400 Bad Request\nContent-Type: text/plain\n\nERROR: ",message)
}


#' Respond to HTTP request with a 404:NotFound error
#' 
#' Responds to an HTTP request with 404:NotFound error
#' 
#' @param message the error message
#' @export
respond404 <- function(message) {
	cat("Status: 404 Not Found\nContent-Type: text/plain\n\nERROR: ",message)
}


#' Respond to HTTP request with a 500 Internal Server Error
#' 
#' Responds to an HTTP request with 500 Internal Server Error
#' 
#' @param message the error message
#' @export
respond500 <- function(message) {
	cat("Status: 500 Internal Server Error\nContent-Type: text/plain\n\nERROR: ",message)
}
