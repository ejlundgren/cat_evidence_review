#
#
# Summarize for text
#
#

rm(list = ls())
library("data.table")
library("gt")
library("dplyr")

# Claims -------------------------------------------------------
claims <- fread("builds/species_claims_tidy_populated.csv")

claims
claims <- claims[exclude_species == "included_species"]
claims

length(unique(claims$spp_name_corrected))

claims[, .(n = uniqueN(spp_name_corrected)),
       by = .(class)]

# Systematic review -------------------------------------------------------

sys_rev <- fread("builds/systematic_review_tidy.csv")
sys_rev

unique(sys_rev[exclude_species == "excluded_species"]$scientificName)

sys_rev <- sys_rev[exclude_species == "included_species"]

sys_rev[, synth_fill := evidence_type]
sys_rev[, synth_fill := fcase(grepl("Population", evidence_type) & has_data == "yes", paste(evidence_type, "with data"),
                                grepl("Population", evidence_type) & has_data == "no", paste(evidence_type, "without data"),
                              !grepl("Population", evidence_type), evidence_type)]
unique(sys_rev$synth_fill)

# >>> Overview ------------------------------------------------------------

sys_rev[,  .(n_studies = uniqueN(study_id),
                       n_articles = uniqueN(Article_simple),
                       n_species = uniqueN(scientificName))]


x <- sys_rev[,
        .(n_studies = uniqueN(study_id)),
        by = .(synth_fill, of_quality)]
x[of_quality == "no"]
x[of_quality == "yes"]


# >>> By class ------------------------------------------------------------

sys_rev[,
        .(n_studies = uniqueN(study_id),
          n_articles = uniqueN(Article_simple),
          n_species = uniqueN(scientificName)),
        by = .(class)]


x <- sys_rev[exclude_species == "included_species" & 
               class == "Birds", 
             .(n_studies = uniqueN(study_id)),
             by = .(synth_fill, of_quality)]
x[of_quality == "no"]
x[of_quality == "yes"]


x <- sys_rev[exclude_species == "included_species" & 
               class == "Mammals", 
             .(n_studies = uniqueN(study_id)),
             by = .(synth_fill, of_quality)]
x[of_quality == "no"]
x[of_quality == "yes"]


x <- sys_rev[exclude_species == "included_species" & 
               class == "Reptiles", 
             .(n_studies = uniqueN(study_id)),
             by = .(synth_fill, of_quality)]
x[of_quality == "no"]
x[of_quality == "yes"]

x <- sys_rev[exclude_species == "included_species" & 
               class == "Amphibians", 
             .(n_studies = uniqueN(study_id)),
             by = .(synth_fill, of_quality)]
x[of_quality == "no"]
x[of_quality == "yes"]


# Best evidence -----------------------------------------------------------

claims <- fread("builds/species_claims_tidy_populated.csv")
claims

# claims[duplicated(scientificName)]
# claims[scientificName == "Coenocorypha pusilla", Synonyms_or_previous_lump := "Coenocorypha aucklandica"]
# claims <- unique(claims)
# claims
# claims[duplicated(scientificName)]
unique(claims$class)

claims <- claims[exclude_species == "included_species", .(spp_name_corrected, scientificName, 
                                                          realm, systems, redlistCategory,
                                                          populationTrend, class)] |> unique()
length(unique(claims$scientificName))
length(unique(claims$spp_name_corrected))
claims[duplicated(spp_name_corrected)]

# We have class for this one so let's use the other name:
claims[spp_name_corrected == "Prosobonia parvirostris", ]
claims[spp_name_corrected == "Prosobonia parvirostris", spp_name_corrected := scientificName]
length(unique(claims$spp_name_corrected))

# claims[spp_name_corrected == "Coenocorypha pusilla", Synonyms_or_previous_lump := "Coenocorypha aucklandica"]
claims[duplicated(spp_name_corrected)]

#
ranking <- copy(sys_rev)
ranking <- ranking[, .(spp_name_corrected,scientificName,
                       of_quality, evidence_type, has_data)]

# ranking[spp_name_corrected == "Prosobonia parvirostris", ]
# ranking[spp_name_corrected == "Prosobonia parvirostris", spp_name_corrected := scientificName]

unique(ranking$has_data)

sort(unique(ranking$evidence_type))

ranking[of_quality == "yes" &
          evidence_type == "Population in support", rank := 10]
ranking[of_quality == "no" &
          has_data == "yes" &
          evidence_type == "Population in support", rank := 9]
ranking[of_quality == "no" &
          has_data == "no" &
          evidence_type == "Population in support", rank := 8]
ranking[evidence_type == "Predation in support", rank := 7]

ranking

unique(ranking[is.na(rank)]$evidence_type)

ranking[is.na(rank), rank := 0]
ranking[rank == 0, evidence_type := "No evidence in support"]
ranking[rank == 0, has_data := NA]
ranking[rank == 0, of_quality := NA]

ranking <- ranking[, max_rank := max(rank),
                   by = .(spp_name_corrected)]
ranking[, no_evidence := .N, by = .(spp_name_corrected)]
ranking[no_evidence > 1, ]
ranking <- ranking[rank == max_rank, ] |> unique()
ranking[duplicated(spp_name_corrected), ]

ranking[spp_name_corrected == "Procellaria cinerea"]
# Must be 0 rows....

ranking

best_evidence <- merge(claims,
                       ranking[, .(spp_name_corrected, evidence_type, of_quality,
                                   has_data)],
                       by = "spp_name_corrected",
                       all.x = T,
                       all.y = T)
nrow(best_evidence)
nrow(claims)
best_evidence[!spp_name_corrected %in% claims$spp_name_corrected]

#
best_evidence[duplicated(spp_name_corrected)]


best_evidence[is.na(redlistCategory)] # Must be 0 rows
unique(best_evidence$class)
best_evidence[is.na(redlistCategory), class := "Mammals"]

best_evidence[is.na(evidence_type), evidence_type := "No evidence found"]

# >>> Summarize -----------------------------------------------------------
best_evidence

length(unique(best_evidence$spp_name_corrected))
length(unique(best_evidence$scientificName))

best_evidence[, .(n_species = uniqueN(scientificName)),
              by = .(evidence_type, has_data, of_quality)]


best_evidence[class == "Birds", .(n_species = uniqueN(scientificName)),
              by = .(evidence_type, has_data, of_quality)]


best_evidence[class == "Mammals", .(n_species = uniqueN(scientificName)),
              by = .(evidence_type, has_data, of_quality)]


best_evidence[class == "Reptiles", .(n_species = uniqueN(scientificName)),
              by = .(evidence_type, has_data, of_quality)]


best_evidence[class == "Amphibians", .(n_species = uniqueN(scientificName)),
              by = .(evidence_type, has_data, of_quality)]

# % of Studies not in support --------------------------------------------------

sys_rev

unique(sys_rev[, .(Evidence_category, evidence_type, has_data, of_quality)])
sys_rev[Evidence_category == "Control program", Evidence_category := "Population"]
sys_rev[, Evidence_category := paste(Evidence_category, 
                                     ifelse(has_data == "yes", "with data", "without data"), 
                                     ifelse(of_quality == "yes", "of quality", ""))]
sys_rev[, Evidence_category := trimws(Evidence_category)]
unique(sys_rev[, .(Evidence_category, evidence_type, has_data, of_quality)])

sys_rev

# Calculate number per species
sys_tally <- sys_rev[, .(n_studies = uniqueN(study_id)),
                     by = .(class, Evidence_category, scientificName,
                            Hypothesis_supported)]
sys_tally

# Cast wide
sys_tally[, Hypothesis_supported := ifelse(Hypothesis_supported == 0, "no", "yes")]
sys_tally

sys_tally.wide <- dcast(sys_tally, 
                        class + scientificName + Evidence_category ~ Hypothesis_supported,
                        value.var = "n_studies",
                        fill = 0)
sys_tally.wide[, total := no + yes]
sys_tally.wide[, percent_not_in_support := no / total * 100]
sys_tally.wide
unique(sys_tally.wide$Evidence_category)

sys_tally.wide

fwrite(sys_tally.wide, "builds/temp/perc_not_in_support_for_checking.csv")

# >>> Overview: -----------------------------------------------------------------
sys_tally.wide[, .(avg_not_support = mean(percent_not_in_support), 
                   sd = sd(percent_not_in_support)),
               by = .(Evidence_category)]

sys_tally.wide %>% 
  group_by(Evidence_category) %>% 
  summarize(mean_not = mean(percent_not_in_support))

# >>> By class ------------------------------------------------------------

sys_tally.wide[class == "Birds", .(avg_not_support = mean(percent_not_in_support), 
                                   sd = sd(percent_not_in_support)),
               by = .(Evidence_category)]

sys_tally.wide[class == "Mammals", .(avg_not_support = mean(percent_not_in_support), sd = sd(percent_not_in_support)),
               by = .(Evidence_category)]

sys_tally.wide[class == "Reptiles", .(avg_not_support = mean(percent_not_in_support), sd = sd(percent_not_in_support)),
               by = .(Evidence_category)]

sys_tally.wide[class == "Amphibians", .(avg_not_support = mean(percent_not_in_support), sd = sd(percent_not_in_support)),
               by = .(Evidence_category)]


# Citations ---------------------------------------------------------------

terminus_edges <- fread("builds/citation_network/terminal_citation_chains.csv")
terminus_edges
unique(terminus_edges$cited_by_id)

# >>> Merge in class ------------------------------------------------------
setdiff(terminus_edges$scientificName, claims$scientificName)
terminus_edges.mrg <- merge(terminus_edges,
                            claims[, .(scientificName, class)],
                            by = "scientificName",
                            all.x = T)
nrow(terminus_edges.mrg) == nrow(terminus_edges)


unique(terminus_edges.mrg$evidence_type)

# >>> Add an overall category ---------------------------------------------

terminus_edges.mrg2 <- copy(terminus_edges.mrg)
terminus_edges.mrg2[, class := "overall"]

terminus_edges.bnd <- rbind(terminus_edges.mrg,
                              terminus_edges.mrg2)

terminus_edges.bnd

# Going to save a table for Arian because the template is HUGE.
terminus_edges.bnd



# >>> Add claimant, non-claimant, etc categories --------------------------
unique(terminus_edges$cited_by_id)

terminus_edges.bnd[, network_type := "entire network"]

sub_net <- terminus_edges.bnd[grepl("core source_claim", cited_by_id)]
sub_net[, network_type := "claimant network"]
unique(sub_net$cited_by_id)


sub_net2 <- terminus_edges.bnd[grepl("Web of Science", cited_by_id)]
sub_net2[, network_type := "Web of Science network"]
unique(sub_net2$cited_by_id)

sub_net3 <- terminus_edges.bnd[grepl("Web of Science", cited_by_id) | grepl("without claim", cited_by_id)]
sub_net3[, network_type := "Non-claimant network"]
unique(sub_net3$cited_by_id)

terminus_edges.final <- rbind(terminus_edges.bnd,
                              sub_net,
                              sub_net2,
                              sub_net3)

terminus_edges.final

# >>> First, need # and proportion that do not end with data --------------
unique(terminus_edges.final$evidence_type_synthetic)

terminus_edges.final[, is_primary_data := ifelse(grepl("Population", evidence_type_synthetic) | grepl("Predation", evidence_type_synthetic),
                                                 "yes", "no")]
terminus_edges.final[, total_citation_chains := .N,
                     by = .(class, network_type)]

sum1 <- terminus_edges.final[, .(n_citations = .N,
                                 mean_distance = mean(distance_to_terminus),
                                 sd_distance = sd(distance_to_terminus)),
                             by = .(class, network_type, is_primary_data, total_citation_chains)]
sum1

sum1[, percent := n_citations / total_citation_chains * 100]
sum1[, evidence_type := ifelse(is_primary_data == "no", "Not primary data", "Primary data")]
sum1$is_primary_data <- NULL
sum1

unique(terminus_edges.final$evidence_type_synthetic)

sum2 <-  terminus_edges.final[, .(n_citations = .N,
                                  mean_distance = mean(distance_to_terminus),
                                  sd_distance = sd(distance_to_terminus)),
                              by = .(class, network_type, evidence_type_synthetic, total_citation_chains)]
sum2[, percent := n_citations / total_citation_chains * 100]
setnames(sum2, "evidence_type_synthetic", "evidence_type")
sum2

range(sum2$mean_distance)

final_table <- rbind(sum1,
                     sum2)
final_table

final_table <- final_table[, !c("mean_distance", "sd_distance")]
final_table
setorder(final_table, class, network_type, evidence_type)
final_table

unique(final_table$evidence_type)

setdiff(final_table$evidence_type, c("Not primary data", "Primary data",
                                     "No citation given", "Opinion claim", "No claim",
                                     "Inaccessible", "Core claim",
                                     "Does not test claim",
                                     "Predation in support without data", "Predation not in support without data",
                                     "Population in support without data", "Population not in support without data",
                                     "Population in support with data", "Population not in support with data",
                                     "Population in support with data of quality", "Population not in support with data of quality"))

# Ugh I don't feel like doing that...
final_table[, evidence_type := factor(evidence_type,
                                      levels = c("Not primary data", "Primary data",
                                                 "No citation given", "Opinion claim", "No claim",
                                                 "Inaccessible", "Not in English or Spanish",
                                                 "Core claim",
                                                 "Does not test claim",
                                                 "Predation in support without data", "Predation not in support without data",
                                                 "Population in support without data", "Population not in support without data",
                                                 "Population in support with data", "Population not in support with data",
                                                 "Population in support with data of quality", "Population not in support with data of quality"))]
unique(final_table$evidence_type)
final_table[, sort := as.numeric(evidence_type)]

setorder(final_table, -class, network_type,sort)

final_table$sort <- NULL
final_table

fwrite(final_table, "builds/citation_network/terminus_stats_for_text.csv")

# >>> By individual source ------------------------------------------------

terminus_edges_source <- terminus_edges.final[network_type %in% c("claimant network", "Non-claimant network"), ]
terminus_edges_source$network_type <- NULL

unique(terminus_edges_source$evidence_type_synthetic)

terminus_edges_source[, is_primary_data := ifelse(grepl("Population", evidence_type_synthetic) | grepl("Predation", evidence_type_synthetic),
                                                 "yes", "no")]

#
terminus_edges_source[, total_citation_chains := .N,
                     by = .(class, cited_by_id)]
unique(terminus_edges_source$cited_by_id)

terminus_edges_source[cited_by_id == "(Medina et al. 2011) core source_claim"]

unique(terminus_edges_source$cited_by_id)

# unique(terminus_edges_source[network_type == "Non-claimant network"]$cited_by_id)


sum1 <- terminus_edges_source[, .(n_citations = .N,
                                 mean_distance = mean(distance_to_terminus),
                                 sd_distance = sd(distance_to_terminus)),
                             by = .(cited_by_id,
                                    class, is_primary_data, total_citation_chains)] |> unique()
sum1[duplicated(paste(cited_by_id, class, is_primary_data))]
# Must be 0 rows

sum1[, percent := n_citations / total_citation_chains * 100]
sum1[, evidence_type := ifelse(is_primary_data == "no", "Not primary data", "Primary data")]
sum1$is_primary_data <- NULL
sum1

unique(terminus_edges_source$evidence_type_synthetic)

sum2 <-  terminus_edges_source[,.(n_citations = .N,
                                  mean_distance = mean(distance_to_terminus),
                                  sd_distance = sd(distance_to_terminus)),
                              by = .(cited_by_id,
                                     class, evidence_type_synthetic, 
                                     total_citation_chains)]
sum2[, percent := n_citations / total_citation_chains * 100]
setnames(sum2, "evidence_type_synthetic", "evidence_type")
sum2
sum2[duplicated(paste(cited_by_id, class, evidence_type))]
# Must be 0 rows

range(sum2$mean_distance)

final_table <- rbind(sum1,
                     sum2)
final_table

final_table <- final_table[, !c("mean_distance", "sd_distance")]
final_table
setorder(final_table, class, evidence_type)
final_table

unique(final_table$evidence_type)

# Ugh I don't feel like doing that...
final_table[, evidence_type := factor(evidence_type,
                                      levels = c("Not primary data", "Primary data",
                                                 "No citation given", "Opinion claim", "No claim",
                                                 "Inaccessible", "Not in English or Spanish",
                                                 "Core claim",
                                                 "Does not test claim",
                                                 "Predation in support without data", "Predation not in support without data",
                                                 "Population in support without data", "Population not in support without data",
                                                 "Population in support with data", "Population not in support with data",
                                                 "Population in support with data of quality", "Population not in support with data of quality"))]
unique(final_table$evidence_type)
final_table[, sort := as.numeric(evidence_type)]

setorder(final_table, -class,sort)

final_table

unique(final_table$cited_by_id)
final_table[, claim := ifelse(grepl("core source_claim", cited_by_id),
                              "Making claim", "No claim")]
unique(final_table[, .(cited_by_id, claim)])
final_table[, cited_by_id := gsub("_Systematic external review", "", cited_by_id)]
final_table[, cited_by_id := gsub("_external review without claim", "", cited_by_id)]
final_table[, cited_by_id := gsub(" core source_claim", "", cited_by_id)]
final_table


# unique(final_table$network_type)
# final_table <- 


setorder(final_table, claim, cited_by_id, -class, sort)
final_table$sort <- NULL

final_table


gt_tbl <- final_table %>%
  mutate(cited_by_id = gsub("(", "", cited_by_id, fixed = T),
         cited_by_id = gsub(")", "", cited_by_id, fixed = T)) %>%
  group_by(cited_by_id, claim) %>%
  gt() %>%
  fmt_number(columns = percent, decimals = 2) %>%
  cols_move(columns = c("evidence_type"),
            after = "class")
gt_tbl
gtsave(data = gt_tbl, filename = "figures/SI/citation_terminus_by_source.rtf")

  
# fwrite(final_table, "builds/citation_network/terminus_stats_by_source.csv")


