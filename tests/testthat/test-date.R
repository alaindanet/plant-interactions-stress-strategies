context("Date conversion to number of month")


test_that("return a number of month", {
    duration <- yearmon2month(date = zoo::as.yearmon("janv. 2016"))

    expect_length(duration,1)
    expect_is(duration, "integer")
    expect_warning(yearmon2month(date = zoo::as.yearmon("janv. 2012")),
	"The duration is negative.")
})

test_that("correct input", {
    expect_error(yearmon2month(date = NA))
    expect_error(yearmon2month(date = zoo::as.yearmon("bad format 2012")))
    expect_error(yearmon2month(starting = NA))

})

test_that("make a good calculation", {
    expect_equal(duration,2)
    expect_equal(yearmon2month(date = zoo::as.yearmon("nov. 2016")), 13)
})
