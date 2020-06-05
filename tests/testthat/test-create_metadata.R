context("Metadata creation")


test_that("test attribute creators", {
  data("bfi")
  metadata(bfi)$name <- "MOCK Big Five Inventory dataset (German metadata demo)"
  metadata(bfi)$description <- "a small mock Big Five Inventory dataset"
  metadata(bfi)$identifier <- "doi:10.5281/zenodo.1326520"
  metadata(bfi)$datePublished <- "2016-06-01"
  metadata(bfi)$creator <- list(
    "@type" = "Person",
    givenName = "Ruben", familyName = "Arslan",
    email = "ruben.arslan@gmail.com",
    affiliation = list("@type" = "Organization",
                       name = "MPI Human Development, Berlin"))
  metadata(bfi)$citation <- "Arslan (2016). Mock BFI data."
  metadata(bfi)$url <-
    "https://rubenarslan.github.io/codebook/articles/codebook.html"
  metadata(bfi)$temporalCoverage <- "2016"
  metadata(bfi)$spatialCoverage <- "Goettingen, Germany"
  metadata(bfi)$keywords <- "Personality"


  expect_equal(names(attributes(bfi)$metadata),
               c("name", "description",
                 "identifier", "datePublished", "creator", "citation",
                 "url", "temporalCoverage",
                   "spatialCoverage", "keywords"))
})



test_that("list to dict and back", {
  data('bfi')
  labels <- var_label(bfi)
  head(labels, 2)
  dict <- list_to_dict(labels)
  head(dict, 2)
  expect_equal(dict_to_list(list_to_dict(labels)), labels)
})

