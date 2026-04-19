
rm(list = ls())
library("data.table")
library("dplyr")
library("tidyr")
library("readr")
library("ggplot2")
library("igraph")
library("tidygraph")
library("ggraph")
library("readxl")
library("stringr")
library("patchwork")
library("lubridate")

# Extract years for all publications...Which is going to be annoying as hell. 
# Output: sidecar file to use for a chisquare type test
# Load data ---------------------------------------------------------------

dat <- read_excel("data/systematic_review/Cat_Database.xlsx",
                  sheet = "Citations") |> setDT()

dat

years <- melt(dat[, .(article_node_name, cited_by_node_name, scientificName)],
              id.vars = "scientificName")


# Split out evidence versus core sources ----------------------------------
years[grepl("core source", value)]
years[grepl("Web of Science", value)]
years[grepl("Wallach", value)]
years[grepl("Opportunistic", value)]

sources <- years[grepl("core source", value) | 
                  value == "Web of Science" |
                   value == "Opportunistic" |
                    value == "(Wallach & Lundgren 2025)", ]
sources

unique(sources$value)

evidence <- years[!value %in% sources$value, ]


# IUCN has to be dealt with separately than the other sources
years[grepl("IUCN", value)]

iucn <- sources[value == "(IUCN 2025) core source"]
other_sources <- sources[value != "(IUCN 2025) core source"]

remove(sources)


# Extract date for evidence -----------------------------------------------

unique(evidence$value)
evidence[, scratch := gsub("\\(", "", value)]
evidence[, scratch := gsub(")", "", scratch)]
evidence[, scratch := gsub(" core source", "", scratch)]
evidence

# evidence[, pub_year := parse_number(scratch)]
# evidence
# Well that didn't work great now did it.

evidence[, pub_year := word(scratch, -1, sep = " ")]
evidence[, pub_year := gsub("[a-zA-Z]", "", pub_year) ]
evidence
unique(evidence$pub_year)

unique(evidence[pub_year == ""]$scratch)
unique(evidence[pub_year == "."]$scratch)
unique(evidence[pub_year == ".."]$scratch)
evidence[grepl("IUCN", value)]

# This one is going to be more complex.
evidence[grepl("Dickman", value)]

evidence

# Get IUCN assessment dates -----------------------------------------------

claims <- read_excel("data/systematic_review/Cat_Database.xlsx",
                  sheet = "Species Claims") |>
  setDT()
claims

claims <- claims[, .(scientificName, assessmentDate)]
claims
claims[, pub_year := ymd_hms(assessmentDate) |> year()]
claims
claims[, value := "(IUCN 2025) core source"]
claims <- claims[, .(scientificName, value, pub_year)]
claims[duplicated(scientificName), ]

claims[scientificName == "Coenocorypha pusilla"]
claims <- unique(claims)

#  >>> Merge species specific IUCN dates into sidecar ------------------------------------------
claims[, key := paste(scientificName, value)]
iucn[, key := paste(scientificName, value)]

iucn_years <- merge(iucn,
                    claims[, .(key, pub_year)],
                    by = "key",
                    all.x = FALSE,
                    all.y = FALSE)
iucn_years[, ]

# Load core source literature search dates --------------------------------

core_source_dates <- read_excel("data/systematic_review/Source review dates.xlsx") |> 
  setDT()
# core_source_dates[, pub_year := year(review_ended)]
core_source_dates[, review_ended_conservative := review_ended - months(6)]

core_source_dates[, pub_year := year(review_ended_conservative)]
core_source_dates

setnames(core_source_dates, c("source"), c("value"))
# core_source_dates[, pub_year := year(review_ended)]
core_source_dates

claims
core_source_dates


# >>> Merge into core claims (to get species) ----------------------------------------------

setdiff(core_source_dates$value, other_sources$value)
setdiff(other_sources$value, core_source_dates$value)

other_sources.final <- merge(other_sources,
                             core_source_dates[, .(value, Note, pub_year)],
                             by = "value")


# Bind finished year dataset ----------------------------------------------
other_sources.final
iucn_years[, pub_year := pub_year-1]
# evidence[, review_ended := pub_year-1]


claims.bind <- rbind(evidence[, .(scientificName, value, pub_year)],
                     iucn_years[, .(scientificName, value, pub_year)],
                     other_sources.final[, .(scientificName, value, pub_year, Note)],
                     fill = TRUE)

claims.bind


# Save --------------------------------------------------------------------


fwrite(claims.bind, "data/systematic_review/all publication years.csv")

#  Test analysis ----------------------------------------------------------
# Chi square tests are devilishly confusing...

# So from a pool of 10 population papers on species X available, source A cited 2. Hmmm maybe it's just a % that we report?

# Could do probability of being cited ~ in_support + study type + claim_no_claim + (1|Review ID) + (1|species)
# To test the hypothesis of citation bias (which I don't think there is) or research quality bias (based on claim/no-claim)




