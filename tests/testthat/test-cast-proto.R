context("cast.proto")


test_that("cast protocol", {

    ob_local <- function() {

      ob <- function(a,b) structure(list(a=a,b=b), class='ob')
      is.ob <- function(x) inherits(x,'ob')

      as.ob_list <- function(x, ...) UseMethod('as.ob_list')
      as.ob_list.list <- function(x, ...) {
        stopifnot(all(sapply(x,is.ob)))
        class(x) <- append(class(x),'ob_list',after=0)
        x
      }

      as.data.frame.ob <- function(x, ...) data.frame(t(as.matrix(unlist(x))))
      as.data.frame.ob_list <- function(x, ...) do.call('rbind', lapply(x,as.data.frame))


      o1 <- ob(1,10)
      o2 <- ob(2,20)
      o3 <- ob(3,30)

      ol <- list(o1,o2,o3)

      df <- as.data.frame(as.ob_list(ol))

      print(df)

      expect_equal(length(ol), 3)
      expect_equal(length(ol), nrow(df))
      expect_equal(df$a[1], 1)
      expect_equal(df$a[2], 2)
      expect_equal(df$a[3], 3)
      expect_equal(df$b[1], 10)
      expect_equal(df$b[2], 20)
      expect_equal(df$b[3], 30)
    }

    expect_true(TRUE)

})


