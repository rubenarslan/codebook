context("Metadata creation")


test_that("test attribute creators", {
  data("bfi")
  data_name(bfi) <- "MOCK Big Five Inventory dataset (German metadata demo)"
  data_description(bfi) <- "a small mock Big Five Inventory dataset"
  data_identifier(bfi) <- "doi:10.5281/zenodo.1326520"
  data_datePublished(bfi) <- "2016-06-01"
  data_creator(bfi) <- list(
    "@type" = "Person",
    givenName = "Ruben", familyName = "Arslan",
    email = "ruben.arslan@gmail.com",
    affiliation = list("@type" = "Organization",
                       name = "MPI Human Development, Berlin"))
  data_citation(bfi) <- "Arslan (2016). Mock BFI data."
  data_url(bfi) <-
    "https://rubenarslan.github.io/codebook/articles/codebook.html"
  data_temporalCoverage(bfi) <- "2016"
  data_spatialCoverage(bfi) <- "Goettingen, Germany"
  data_keywords(bfi) <- "Personality"


  expect_equal(names(attributes(bfi)),
               c("class", "row.names", "names", "name", "description",
                 "identifier", "datePublished", "creator", "citation",
                 "url", "temporalCoverage",
                   "spatialCoverage", "keywords"))
})

