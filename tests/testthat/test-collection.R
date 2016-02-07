#
context("collection")

## TODO: Rename context
## TODO: Add more tests

test_that("single item", {

  url <- "http://www.w3c.org"
  u <- URL(url,classes=c('root','base'))

  us <- as.URLS(u)

  #print(us)

  expect_equal(u$url, url)
  expect_identical(us@urls[[1]], u)

})


test_that("list items", {

  l <- list(
    org.w3c='http://www.w3c.org',
    org.ietf='http://ietf.org',
    org.net.iana='http://www.iana.org',
    loc.hosts='hosts',
    'R env'='file://etc/R/Renviron'
  )

  uz <- URL(l, src=URL('https://cran.r-project.org/'), base='/etc')
  us <- as.URLS(uz)

  #print(us)

  expect_equal(length(l), length(uz))
  expect_identical(names(l), names(uz))

  expect_equal(length(uz), length(us@urls))
  expect_identical(names(uz), names(us@urls))


})


test_that("vectors items", {

  l <- c(
    org.w3c='http://www.w3c.org',
    org.ietf='http://ietf.org',
    org.net.iana='http://www.iana.org',
    loc.hosts='hosts',
    'R env'='file://etc/R/Renviron'
  )

  uz <- URL(l, src=URL('https://cran.r-project.org/'), base='/etc')
  us <- as.URLS(uz)

  #show(us)

  expect_equal(length(l), length(uz))
  expect_identical(names(l), names(uz))

  expect_equal(length(uz), length(us@urls))
  expect_identical(names(uz), names(us@urls))

})


