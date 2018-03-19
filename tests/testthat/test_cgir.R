library("cgir")

context("cgir")

test_that("interpolation works",{

	text <- "Hello %foo!!"
	testfile <- tempfile()
	con <- file(testfile,open="wt")
	writeLines(text,con)
	close(con)

	result <- interpolate(testfile,c(foo="World"))
	expect_equal("Hello World!!",result)
})

test_that("respondHTML works", {

	html <- "<html><head><title>test</title></head><body><h1>Test</h1></body></html>"
	expected <- "Content-type: text/html\n\n<html><head><title>test</title></head><body><h1>Test</h1></body></html>"

	expect_output(respondHTML(html),expected)
}) 

test_that("readGET works",{

	Sys.setenv(QUERY_STRING="foo=bar&foobar=baz")

	getdata <- readGET()
	expect_equal(getdata$foo,"bar")
	expect_equal(getdata$foobar,"baz")

	Sys.unsetenv("QUERY_STRING")

})

test_that("makeUUID works", {
	id <- makeUUID()
	expect_match(id,"^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab]{1}[0-9a-f]{3}-[0-9a-f]{12}$")

})
