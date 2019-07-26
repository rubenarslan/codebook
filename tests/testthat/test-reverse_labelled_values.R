test_that("reverse labelled values", {
  x <- haven::labelled(rep(1:3, each = 3), c(Bad = 1, Good = 5))
  expect_silent(reversed <- reverse_labelled_values(x))
  expect_identical(attributes(reversed)$labels, c(Bad = 5, Good = 1))
  expect_equal(reversed[[1]], 5)
  expect_equal(reversed[[4]], 4)
  expect_equal(reversed[[9]], 3)

  x <- haven::labelled(rep(1:3, each = 3), c(Missing = NA_real_))
  expect_warning(reversed <- reverse_labelled_values(x), "values outside")
  expect_identical(attributes(reversed)$labels, c(Missing = NA_real_))
  expect_failure(expect_equal(reversed[[1]], 5))
  expect_failure(expect_equal(reversed[[4]], 4))
  expect_failure(expect_equal(reversed[[9]], 3))

  x <- haven::labelled(rep(1:3, each = 3), c(Bad = 1, Good = 5))
  expect_silent(reversed <- reverse_labelled_values(x))
  expect_identical(attributes(reversed)$labels, c(Bad = 5, Good = 1))
  expect_equal(reversed[[1]], 5)
  expect_equal(reversed[[4]], 4)
  expect_equal(reversed[[9]], 3)

  x <- haven::labelled(rep(1:3, each = 3), c(Bad = 1, Medium = 2, Good = 3))
  expect_silent(reversed <- reverse_labelled_values(x))
  expect_identical(attributes(reversed)$labels,
                   c(Bad = 3, Medium = 2, Good = 1))
  expect_equal(reversed[[1]], 3)
  expect_equal(reversed[[4]], 2)
  expect_equal(reversed[[9]], 1)

  x <- haven::labelled(rep(1:3, each = 3), c(Missing = NA_real_))
  expect_warning(reversed <- reverse_labelled_values(x), "values outside")
  expect_identical(attributes(reversed)$labels, c(Missing = NA_real_))
  expect_equal(reversed[[1]], 3)
  expect_equal(reversed[[4]], 2)
  expect_equal(reversed[[9]], 1)

  x <- haven::labelled(c("herp", "derp", "jerk"), c("Herpetologist" = "herp"))
  expect_warning(reversed <- reverse_labelled_values(x), "not numeric")
  expect_identical(x, reversed)

  x <- haven::labelled(rep(1:3, each = 3), c(Missing = NA_real_))
  expect_warning(reversed <- reverse_labelled_values(x), "values outside")
  expect_identical(attributes(reversed)$labels, c(Missing = NA_real_))
  expect_equal(reversed[[1]], 3)
  expect_equal(reversed[[4]], 2)
  expect_equal(reversed[[9]], 1)

  x <- rep(1:3, each = 3)
  expect_warning(reversed <- reverse_labelled_values(x), "values outside")
  expect_null(attributes(reversed)$labels)
  expect_equal(reversed[[1]], 3)
  expect_equal(reversed[[4]], 2)
  expect_equal(reversed[[9]], 1)

  x <- factor(rep(1:3, each = 3), levels = 1:5, labels = letters[1:5])
  expect_warning(reversed <- reverse_labelled_values(x), "factor")
  expect_identical(names(attributes(reversed)$labels), letters[1:5])
  expect_equivalent(attributes(reversed)$labels, 5:1)
  expect_equal(reversed[[1]], 5)
  expect_equal(reversed[[4]], 4)
  expect_equal(reversed[[9]], 3)


  x <- haven::labelled(c(rep(1:3, each = 3), NA_real_), c(Bad = 1, Good = 5,
                                                          Missing = NA_real_))
  expect_silent(reversed <- reverse_labelled_values(x))
  expect_identical(attributes(reversed)$labels, c(Bad = 5, Good = 1,
                                                  Missing = NA_real_))
  expect_equal(reversed[[1]], 5)
  expect_equal(reversed[[4]], 4)
  expect_equal(reversed[[9]], 3)

  x <- haven::labelled(c(rep(1:3, each = 3), NA_real_), c(Bad = 1, Ugh = 2,
                                                          Meh = 3, Mhh = 4,
                                                          Good = 5,
                                                          Missing = NA_real_))
  expect_silent(reversed <- reverse_labelled_values(x))
  expect_identical(attributes(reversed)$labels, c(Bad = 5, Ugh = 4,
                                                  Meh = 3, Mhh = 2,
                                                  Good = 1,
                                                  Missing = NA_real_))
  expect_equal(reversed[[1]], 5)
  expect_equal(reversed[[4]], 4)
  expect_equal(reversed[[9]], 3)

})
