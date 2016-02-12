#
context("execute")

## TODO: Rename context
## TODO: Add more tests

test_that("single item", {

  url <- "http://www.w3c.org"

  xs <- url %>%
    us %>%
    ux(URL_config) %>%
    ux(URL_fetch) %>%
    ux(URL_parse)


  expect_true(TRUE)

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
  vs <- as.URLS(uz)

  xs <- vs %>%
    us %>%
    ux(URL_config) %>%
    ux(URL_expand) %>%
    ux(URL_fetch) %>%
    ux(URL_parse)

  expect_true(TRUE)


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
  vs <- as.URLS(uz)

  xs <- vs %>%
    us %>%
    ux(URL_config) %>%
    ux(URL_expand) %>%
    ux(URL_fetch) %>%
    ux(URL_parse)

  expect_true(TRUE)


})


