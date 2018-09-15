
library(DDIwR)
library(codebook)
data(bfi)
library(rio)
attributes(rio::gather_attrs(bfi))
attributes(bfi)


ddi_attr
for(i in seq_along(names(attributes(bfi)))) {

}
test <- list()

test$ID <- list(
  label = "Questionnaire ID",
  type = "num",
  measurement = "interval",
  uniqueid = TRUE
)

test$V1 <- list(
  label = "Label for the first variable",
  values = c(
    "No"             =  0,
    "Yes"            =  1,
    "Not applicable" = -7,
    "Not answered"   = -9),
  missing = c(-7, -9),
  type = "cat",
  measurement = "nominal"
)

test$V2 <- list(
  label = "Label for the second variable",
  values = c(
    "Very little"    =  1,
    "Little"         =  2,
    "So, so"         =  3,
    "Much"           =  4,
    "Very much"      =  5,
    "Don't know"     = -8),
  missing = c(-8),
  type = "cat",
  measurement = "ordinal"
)

test$V3 <- list(
  label = "Label for the third variable",
  values = c(
    "First answer"   = "A",
    "Second answer"  = "B",
    "Don't know"     = -8),
  missing = c(-8),
  type = "cat",
  measurement = "nominal"
)

test$V4 <- list(
  label = "Number of children",
  values = c(
    "Don't know"     = -8,
    "Not answered"   = -9),
  missing = c(-8, -9),
  type = "numcat",
  measurement = "ratio"
)

test$V5 <- list(
  label = "Political party reference",
  type = "char",
  txt = "When the respondent indicated his political party reference, his/her open response
was recoded on a scale of 1-99 with parties with a left-wing orientation coded on the low end
of the scale and parties with a right-wing orientation coded on the high end of the scale.
Categories 90-99 were reserved miscellaneous responses."
)



exportDDI(test)
