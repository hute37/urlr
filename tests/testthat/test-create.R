context("create")

## TODO: Rename context
## TODO: Add more tests

test_that("absolute scalar", {

  url <- "http://www.w3c.org"
  u <- URL(url)

  expect_equal(u$url, url)
  expect_equal(as.character(u), url)
  expect_false(u$relative)
  expect_true(u$origin)

})


test_that("relative, base $(pwd)", {

  x <- URL("README.md")

  expect_equal(x$url, paste0("file://",getwd(),"/","README.md"))
  expect_match(as.character(x), '^file:///.*/README.md')
  expect_true(x$relative)
  expect_true(x$origin)

})

test_that("relative, base $(HOME)", {

  x <- URL("~/.Rprofile")
  y <- URL(".Rprofile", base="~")

  expect_equal(x$url, paste0("file://",path.expand('~'),"/",".Rprofile"))
  expect_false(x$relative)
  expect_equal(as.character(y$url), as.character(x$url))
  expect_true(y$relative)

})


test_that("absolute, no schema", {

  x <- URL("/etc/hosts")

  expect_equal(x$url, "file:///etc/hosts")
  expect_false(x$relative)
  expect_true(x$origin)

})

test_that("relative, local path", {

  x <- URL("hosts", base="/etc")

  expect_equal(x$url, "file:///etc/hosts")
  expect_true(x$relative)
  expect_true(x$origin)

})


test_that("relative, remote path", {

  u <- URL("index.html", base="http://www.w3c.org")

  expect_equal(as.character(u), "http://www.w3c.org/index.html")
  expect_true(u$relative)
  expect_true(u$origin)

})


test_that("absolute scalar with source", {

  u.url <- "http://www.w3c.org"
  u <- URL(u.url)

  v.url <- "http://ietf.org"
  v <- URL(v.url, src=u)

  expect_equal(v$url, v.url)
  expect_equivalent(v$src, u)
  expect_false(v$origin)
  expect_true(v$src$origin)

})


test_that("absolute scalar with source string", {

  u.url <- "http://www.w3c.org"
  v.url <- "http://ietf.org"

  v <- URL(v.url, src=u.url)

  expect_equal(v$url, v.url)
  expect_equal(v$src$url, u.url)
  expect_false(v$origin)
  expect_true(v$src$origin)

})

test_that("absolute scalar with 3 source levels", {

  u.url <- "http://www.w3c.org"
  v.url <- "http://ietf.org"
  w.url <- "http://iana.org"

  v <- URL(v.url, src=u.url)
  w <- URL(w.url, src=v)

  expect_equal(w$url, w.url)
  expect_equal(w$src$url, v.url)
  expect_equal(w$src$src$url, u.url)
  expect_false(w$origin)
  expect_false(w$src$origin)
  expect_true(w$src$src$origin)

})

test_that("list constructon", {

  l <- list(
    w3c='http://www.w3c.org',
    ietf='http://ietf.org',
    iana='http://www.iana.org',
    hosts='hosts',
    'R env'='file://etc/R/Renviron'
  )

  uz <- URL(l, src=URL('https://cran.r-project.org/'), base='/etc')

  expect_equal(length(l), length(uz))
  expect_identical(names(l), names(uz))

})


test_that("vector constructon", {

  l <- c(
    org.w3c='http://www.w3c.org',
    org.ietf='http://ietf.org',
    org.iana='http://www.iana.org',
    loc.hosts='hosts',
    'R env'='file://etc/R/Renviron'
  )

  uz <- URL(l, src=URL('https://cran.r-project.org/'), base='/etc')

  expect_equal(length(l), length(uz))
  expect_identical(names(l), names(uz))

})


test_that("list constructon as.data.frame", {

  l <- list(
    org.w3c='http://www.w3c.org',
    org.ietf='http://ietf.org',
    org.net.iana='http://www.iana.org',
    loc.hosts='hosts',
    'R env'='file://etc/R/Renviron'
  )

  uz <- URL(l, src=URL('https://cran.r-project.org/'), base='/etc')

  df <- as.data.frame.URL(uz)

  expect_equal(length(l), nrow(df))
  expect_identical(names(l), rownames(df))

})


test_that("vector constructon as.data.frame", {

  l <- c(
    org.w3c='http://www.w3c.org',
    org.ietf='http://ietf.org',
    org.net.iana='http://www.iana.org',
    loc.hosts='hosts',
    'R env'='file://etc/R/Renviron'
  )

  uz <- URL(l, src=URL('https://cran.r-project.org/'), base='/etc')

  df <- as.data.frame.URL(uz)

  expect_equal(length(l), nrow(df))
  expect_identical(names(l), rownames(df))

})

