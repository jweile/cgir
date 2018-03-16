


#' Redirect messages to a log file
#' 
#' Redirects any messages (e.g. errors) to a log file. Doing so at the start of your script is
#' absolutely vital! Otherwise any potential info messages, warnings or errors can corrupt your
#' HTTP response
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
#' @returns the interpolated text.
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
	#Content type MUST be followed by two newline characters!!
	cat("Content-type: text/html\n\n")
	cat(html)
	return(invisible(NULL))
}