# Create node and edge file
#
#
#
#
#

rm(list = ls())
library("data.table")
library("dplyr")
library("tidyr")
library("ggplot2")
library("igraph")
library("tidygraph")
library("ggraph")
library("readxl")
library("stringr")
library("patchwork")
library("broom")
library("broom.mixed")

# Load data ---------------------------------------------------------------

dat <- read_excel("data/systematic_review/Cat_Database.xlsx",
                  sheet = "Citations")

dat
setDT(dat)
dat

unique(dat$encounter_method)
dat <- dat[exclude_species == "included_species"]

dat[article_node_name == "(Bradley & Gerber 2006)"]
dat[article_node_name == "(Bradley & Gerber 2005)"]

# Sarr is cited by Banko (snowball) for Chasiempis sandwichensis
# How does Sarr become associated with Loxiodes? Ah by citing Pletschet, which is included
dat[grepl("Sarr", Article)]
dat[grepl("Sarr", Article_cited_by)]
dat[scientificName == "Loxioides bailleui"]
dat[scientificName == "Loxioides bailleui" & grepl("Sarr", Article)]
dat[scientificName == "Loxioides bailleui" & grepl("Sarr", Article_cited_by)]
# OK, so the reason that Sarr isn't finding an attribute below is because it was encountered via Banko for one species and was excluded
# but Sarr also cited Pletschet for a different species. The key (article species) therefore does not match. Hmmmm

# Do we merge the non-matches just by article, ignoring the species? I think so.
unique(dat$evidence_type)
dat[evidence_type == "Lethal program in support", evidence_type := "Control program in support"]

dat[evidence_inclusion == "EXCLUDE" & Hypothesis_supported == 1, ]

dat[evidence_inclusion == "EXCLUDE", Hypothesis_supported := NA]
dat[evidence_inclusion == "EXCLUDE", of_quality := NA]
dat[evidence_inclusion == "EXCLUDE", has_data := NA]

unique(dat$evidence_type)
dat[evidence_type == "Control program in support", evidence_type := "Population in support"]
dat[evidence_type == "Control program not in support", evidence_type := "Population not in support"]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Format ------------------------------------------------------------
#' [Let's think about scientificName as an edge attribute instead of a node attribute]
#' [The problem is that without species, core sources end up with terminal attributes...]
#' [Need to create node IDs while dataset is long....]

dat[grepl("core source", cited_by_node_name), 
    cited_by_id := paste(cited_by_node_name, "claim", sep = "_")]
dat
dat[cited_by_node_name == "(Wallach & Lundgren 2025)", 
    cited_by_id :=  "(Wallach & Lundgren 2025)_external review without claim"]
unique(dat[is.na(cited_by_id)]$cited_by_node_name)
dat[cited_by_node_name %in% c("Web of Science", "Opportunistic"), ]
dat[cited_by_node_name %in% c("Web of Science", "Opportunistic"), 
    cited_by_id := paste(cited_by_node_name, "Systematic external review", sep = "_")]
dat[!is.na(cited_by_id), ]

dat[grepl("core source", cited_by_node_name)]

dat[is.na(cited_by_node_name), ]
# Should be 0 rows

# >>> Core sources that are cited should have same ID ---------------------
dat[grepl("core source", article_node_name), ]
unique(dat[grepl("core source", article_node_name), ]$article_node_name)
unique(dat[grepl("core source", article_node_name), ]$evidence_inclusion)

dat[!grepl("core source", article_node_name), 
    article_id := paste(article_node_name, evidence_inclusion, Peer_reviewed_source,
                           evidence_type, 
                           has_data, of_quality, sep = "_")]

dat[grepl("core source", article_node_name) & evidence_inclusion == "INCLUDE",]
dat[grepl("core source", article_node_name) & evidence_inclusion == "INCLUDE", 
    article_id := paste(article_node_name, 
                        evidence_inclusion, Peer_reviewed_source,
                        evidence_type, 
                        has_data, of_quality, sep = "_")]

dat[grepl("core source", article_node_name) & evidence_inclusion == "EXCLUDE",]
dat[grepl("core source", article_node_name) & evidence_inclusion == "EXCLUDE", 
    article_id := paste(article_node_name, "claim", sep = "_")]

unique(c(dat[grepl("core source", cited_by_id)]$cited_by_id,
       dat[grepl("core source", article_id)]$cited_by_id))
dat[is.na(article_id), ]


# [Now, how do get the article_id attributes cited_by_id column for snowballs (e.g., excluded but then citation trail was followed]
# [Maybe need to do this by species? No because then we lose the cross-species citation linkages]

# dat[is.na(cited_by_id), key := paste(scientificName, cited_by_node_name)]

temp <- unique(dat[, .(article_node_name, article_id)])
setnames(temp, "article_id", "cited_by_id_new")
temp

# This will expand dataset...
dat.m1 <- merge(dat,
                temp, 
                by.x = "cited_by_node_name",
                by.y = "article_node_name",
                all.x = T,
                allow.cartesian = T)
dat.m1[, .(cited_by_node_name, cited_by_id, cited_by_id_new,
           article_node_name, article_id)]
dat.m1[is.na(cited_by_id), ]

dat.m1[grepl("Alberts", cited_by_node_name)]

dat.m1[!is.na(cited_by_id), cited_by_id_new := NA]
dat.m1[is.na(cited_by_id), cited_by_id := cited_by_id_new]
dat.m1[, cited_by_id_new := NULL]
dat.m1

dat.m1[, .(cited_by_node_name, cited_by_id,
           article_node_name, article_id)]

dat.m1[is.na(cited_by_id), ]
# Holy shit, 0 rows???
dat.m1[is.na(article_id), ]

dat.m1[grepl("core source_claim", article_id)]

dat.m1[grepl("core source_claim", article_id)]

# >>> Claims without citations should be concatenated w species names --------
dat.m1[grepl("NOTHING", article_id), article_id := paste(article_id, scientificName)]

dat.m1[grepl("NOTHING", article_id),]

nrow(dat.m1)
dat.m1 <- unique(dat.m1)
dat.m1

# >>> Make sure article_id of core sources matches their cited_by_id... --------
dat.m1[cited_by_id == "(Doherty et al. 2016) core source_claim" &
         grepl("IUCN", article_id)]

unique(dat.m1[evidence_type == "Cites different core source", ]$article_id)
dat.m1[evidence_type == "Cites different core source", 
       article_id := gsub("_EXCLUDE_Cites different core source_NA_NA",
                          "",
                          article_id)]
dat.m1[evidence_type == "Cites different core source", article_id := paste0(article_id, "_claim")]
unique(dat.m1[evidence_type == "Cites different core source",]$article_id)
unique(dat.m1[grepl("core source", cited_by_id),]$cited_by_id)

#
unique(dat.m1[evidence_type == "Cites different core source", ]$article_id) %in% 
  unique(dat.m1[grepl("core source", cited_by_id),]$cited_by_id)
# Must all be TRUE


dat.m1[grepl("core source", article_id) & !grepl("INCLUDE", article_id), ]
unique(dat.m1[grepl("core source", article_id) & !grepl("INCLUDE", article_id), ]$article_id)
# gsub("source_*",
#      "",
#      "(Alberts 2000) core source_EXCLUDE_Opinion claim_NA_NA")

dat.m1[is.na(cited_by_node_name)]

#' [Must be 0 rows]
# >>> Create vertex file ----------------------------------------------------------------
nodes <- rbind(dat.m1[, .(cited_by_id)] |> rename(node_id = cited_by_id), 
               dat.m1[, .(article_id)]  |> rename(node_id = article_id)) |>
  unique()
nodes

# # >>> Assign attributes for cited core sources ----------------------------
# 
# nodes[grepl("core source", node_id)]

# >>> Tidy the node attributes --------------------------------------------
nodes[grepl("English", node_id), node_id := gsub("Not_in_English_or_Spanish", "Not in English or Spanish", node_id)]

nodes <- nodes %>%
  separate(col = "node_id", remove = F,
           sep = "_", into=c("article_node_name", "evidence_inclusion", "Peer_reviewed_source",
                             "evidence_type", 
                             "has_data", "of_quality")) |>
  setDT()

nodes[grepl("NOTHING", node_id), article_node_name := paste(article_node_name, of_quality)]
nodes[grepl("NOTHING", node_id), ]
nodes[grepl("NOTHING", node_id), `:=` (of_quality = NA, has_data = NA)]
nodes[grepl("NOTHING", node_id), ]
nodes[evidence_inclusion == "claim", evidence_type_synthetic := "Core claim"]

nodes[has_data == "NA", has_data := NA]
nodes[of_quality == "NA", of_quality := NA]
unique(nodes$of_quality)

nodes[is.na(of_quality) & !is.na(has_data)]
nodes[!is.na(has_data)]

nodes[is.na(evidence_type_synthetic) & !is.na(has_data) & evidence_inclusion == "INCLUDE", 
      evidence_type_synthetic := paste(evidence_type, 
                                       ifelse(has_data == "yes", "with data", "without data"),
                                       ifelse(of_quality == "yes", "of quality", ""))]
nodes[!is.na(evidence_type_synthetic)]
nodes[evidence_inclusion == "EXCLUDE", evidence_type_synthetic := evidence_type]

nodes

nodes[, evidence_type_synthetic := trimws(evidence_type_synthetic)]
unique(nodes$evidence_type_synthetic)

unique(nodes$evidence_type_synthetic)
nodes[is.na(evidence_type_synthetic)]
nodes[is.na(evidence_type_synthetic), evidence_type_synthetic := str_to_sentence(evidence_inclusion)]

nodes[evidence_type_synthetic == "No citation given", evidence_type_synthetic := "Opinion claim"]
nodes[evidence_type_synthetic %in% c("External review without claim", "Systematic external review"),
      evidence_type_synthetic := "External review without claim"]
nodes[evidence_type %in% c("Not in English"), evidence_type_synthetic := "Not in English or Spanish"]

nodes[is.na(evidence_type)]
nodes[is.na(evidence_type), evidence_type := evidence_type_synthetic]

unique(nodes$evidence_type_synthetic)
nodes[is.na(evidence_type_synthetic)]

unique(nodes$evidence_type)

nodes[evidence_type == "Model no pop data", evidence_type := "Modelling without data"]
nodes[evidence_type_synthetic == "Model no pop data", evidence_type_synthetic := "Modelling without data"]

nodes[, evidence_type_fine := evidence_type_synthetic]

# Coarsen the evidence categories for primary plots
sort(unique(nodes$evidence_type_synthetic))

nodes[evidence_type_synthetic %in% c("Failure to access or locate online full citation",
                                     "Personal communication", "Not in English or Spanish",
                                     "Unpublished data or article",
                                     "Missing reference"),
      evidence_type_synthetic := "Inaccessible"]


nodes[evidence_type %in% c("Failure to access or locate online full citation",
                                     "Personal communication", "Not in English or Spanish",
                           "Unpublished data or article",
                                     "Missing reference"),
      evidence_type := "Inaccessible"]



nodes[evidence_type %in% c("Expert opinion",
                           "Modelling without data"),
      evidence_type := "Does not test claim"]


nodes[evidence_type_synthetic %in% c("Expert opinion",
                           "Modelling without data"),
      evidence_type_synthetic := "Does not test claim"]

# >>> Format some factor values ------------------------------------------------------
edges <- dat.m1[, .(cited_by_id, article_id, edge_type,
                    scientificName, encounter_method)]

# Set factor levels of node types
sort(unique(nodes$evidence_type_synthetic))
lvls <- c("Core claim", "External review without claim", 
          "Opinion claim", "Inaccessible", 
          "Does not test claim", "Cites different core source",
          "No claim",
          "Predation not in support without data", "Predation in support without data",
          "Control program not in support without data", "Control program in support without data",
          "Population not in support without data", "Population in support without data",
          "Population not in support with data", "Population in support with data",
          "Population not in support with data of quality", "Population in support with data of quality")

nodes[is.na(evidence_type_synthetic)]
setdiff(unique(nodes$evidence_type_synthetic), lvls)
nodes$evidence_type_synthetic <- factor(nodes$evidence_type_synthetic,
                                        levels = lvls)
sort(unique(nodes$evidence_type_synthetic))

# >>> Further collapsing --------------------------------------------------
nodes[, evidence_type_simple := ifelse(evidence_inclusion == "EXCLUDE", 
                                       "Excluded", 
                                       as.character(evidence_type_synthetic))]
unique(nodes$evidence_type_simple)
nodes[, evidence_type_simple := gsub("of quality", "", evidence_type_simple)]
nodes[, evidence_type_simple := trimws(evidence_type_simple)]
nodes
unique(nodes$evidence_type_simple)
unique(nodes$of_quality)
nodes[is.na(of_quality), of_quality := "no"]

unique(nodes$evidence_type_simple)
nodes[, in_support := NA_character_]
nodes[grepl("not in support", evidence_type_simple), in_support := "No"]
nodes[grepl("in support", evidence_type_simple) & is.na(in_support), in_support := "Yes"]
nodes
nodes[!is.na(in_support)]
nodes[is.na(in_support), in_support := "Not relevant"]
nodes
nodes[, evidence_type_simple := gsub("not in support", "", evidence_type_simple)]
nodes[, evidence_type_simple := gsub("in support", "", evidence_type_simple)]

unique(nodes$evidence_type_simple)
nodes[, evidence_type_simple := gsub("  ", " ", evidence_type_simple)]
nodes

unique(nodes$evidence_type_simple)

nodes$evidence_type_simple <- factor(nodes$evidence_type_simple,
                                     levels = c("Core claim", "External review without claim",
                                                "Excluded", "Predation without data",
                                                "Control program without data", "Population without data", 
                                                "Population with data"))
unique(nodes$evidence_type_simple)

edges[scientificName == "Pseudobulweria becki"]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Save nodes and edges ----------------------------------------------------
unique(edges$edge_type)

edges[edge_type == "core_source_that_provided_primary_evidence"]
edges[edge_type == "core_source_that_provided_primary_evidence", edge_type := "citation"]

edges.sub <- edges[edge_type %in% c("citation"), ]

setdiff(edges.sub$cited_by_id, nodes$node_id)
setdiff(edges.sub$article_id, nodes$node_id)
edges.sub[, article_id := gsub("Not_in_English_or_Spanish", "Not in English or Spanish", article_id)]
setdiff(edges.sub$article_id, nodes$node_id)

nodes.sub <- nodes[node_id %in% c(edges.sub$cited_by_id, edges.sub$article_id)]

setdiff(edges.sub$article_id, nodes.sub$node_id)
setdiff(edges.sub$cited_by_id, nodes.sub$node_id)

nodes.sub[grepl("English", node_id)]

# >>> Fix an error -----------------------------------------

nodes.sub[grepl("_claim_claim", node_id), node_id := gsub("_claim_claim", "_claim", node_id)]

edges.sub[grepl("_claim_claim", article_id), article_id := gsub("_claim_claim", "_claim", article_id)]

edges.sub[grepl("_claim_claim", cited_by_id)]

nodes.sub <- unique(nodes.sub)
edges.sub <- unique(edges.sub)

nodes.sub[duplicated(node_id)]
nodes.sub[node_id == "(IUCN 2025) core source_claim"]


unique(nodes.sub$evidence_type)
nodes.sub[evidence_type != "Inaccessible" | is.na(evidence_type), Peer_reviewed_source := NA]
nodes.sub <- unique(nodes.sub)

# >>> No claim core sources ---------------------------------------------
# We missed this. Citations that terminate in another core source shouldn't happen because those core
# sources should either cite Nothing or something.

#' [This apparently didn't work...]
core_claims <- edges.sub[grepl("core source_claim", cited_by_id), ] |> unique()
core_claims[, key := paste(cited_by_id, scientificName)]

# I think all we need is this:
core_claims <- core_claims$key


edges.sub[grepl("No claim", cited_by_id)]

edges.sub[, temp_key := paste(article_id, scientificName)]
temp <- edges.sub[grepl("core source_claim", temp_key) &
            !temp_key %in% core_claims, ]
edges.sub2 <- edges.sub[!(grepl("core source_claim", temp_key) &
                          !temp_key %in% core_claims), ]

temp
temp[grepl("core source_claim", temp_key) &
            !temp_key %in% core_claims, 
          article_id := gsub("_claim", "_EXCLUDE_NA_No claim", article_id)]
temp

temp$temp_key <- NULL
edges.sub2$temp_key <- NULL

# Going to have to add the missing nodes
unique(nodes.sub$evidence_type)
unique(nodes.sub[evidence_type == "No claim"]$node_id)
nodes.new <- temp[, .(article_id)]
setnames(nodes.new, "article_id", "node_id")
nodes.new <- nodes.new %>%
  separate(col = node_id, into = c("article_node_name", "evidence_inclusion",
                                   "Peer_reviewed_source", "evidence_type"),
           sep = "_",
           remove = F) |> setDT()
nodes.new 

setdiff(temp$article_id, nodes.new$node_id)
setdiff(nodes.new$node_id, temp$article_id)
unique(nodes.sub[evidence_type == "No claim"]$evidence_type_fine)

nodes.new[, evidence_type_fine := "No claim"]
nodes.new[, evidence_type_synthetic := "No claim"]
nodes.new[, evidence_type_simple := "Excluded"]
nodes.new[, in_support := "Not relevant"]
nodes.new[, of_quality := "no"]
nodes.new[, has_data := NA]
#

temp[scientificName == "Mundia elpenor", ]
edges.sub[scientificName == "Mundia elpenor", ]
edges[scientificName == "Mundia elpenor", ]

temp[scientificName == "Pezophaps solitaria", ]
edges.sub[scientificName == "Pezophaps solitaria", ]


#
edges.final <- rbind(temp, edges.sub2)
nodes.final <- rbind(nodes.sub, nodes.new)
nodes.final <- unique(nodes.final)

# >>> Test that network works ---------------------------------------------

igraph.gr <- igraph::graph_from_data_frame(d = edges.final, 
                                           vertices = nodes.final,
                                           directed = T)
igraph.gr

# >>> Save --------------------------------------------------
fwrite(nodes.final, "builds/citation_network/nodes.csv")
fwrite(edges.final, "builds/citation_network/edges.csv")



