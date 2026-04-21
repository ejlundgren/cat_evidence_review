# 
#
#
# Create a data frame of sources -> potential evidence available before publication
#
#
#

rm(list = ls())
library("data.table")
library("igraph")
library("tidygraph")
library("ggplot2")
library("readxl")

raw_dat <- read_excel("data/systematic_review/Cat_Database.xlsx",
                      sheet = "Citations") |> setDT()
# unique(raw_dat[Article_cited_by == "IUCN"]$Article)

raw_dat[scientificName == "Nannoscincus hanchisteus"]

years <- fread("data/systematic_review/all publication years.csv")
years <- unique(years)

nodes <- fread("builds/citation_network/nodes.csv")
edges <- fread("builds/citation_network/edges.csv")

unique(nodes$evidence_type)

years[grepl("core source", value)]
years[grepl("Web of Science", value)]
years[grepl("Wallach", value)]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------
# Test that network citations are feasible based on years -----------------
years[, pub_year := as.numeric(pub_year)]

test_edges <- copy(edges)#merge(edges, nodes[, .()])

test_edges[, from_key := paste(word(cited_by_id, 1, sep = "_"), 
                               scientificName, sep = " | ")]

years[, from_key := paste(value, scientificName, sep = " | ")]
setdiff(test_edges$from_key, years$from_key)
years[duplicated(from_key), ]
# Must be length 0 for both

unique(years[from_key == "(Barbraud et al. 2021) | Diomedea exulans"])

test_edges.mrg <- merge(test_edges, 
                        years[, .(from_key, pub_year)],
                        by = "from_key",
                        all.x = T)
test_edges.mrg
setnames(test_edges.mrg, "pub_year", "source_year")

# Now merge in to article/evidence
test_edges.mrg[, to_key := paste(word(article_id, 1, sep = "_"), 
                                 scientificName, sep = " | ")]

years[, to_key := paste(value, scientificName, sep = " | ")]
setdiff(test_edges$to_key, test_edges.mrg$to_key)

test_edges.mrg2 <- merge(test_edges.mrg, 
                         years[, .(to_key, pub_year)],
                         by = "to_key",
                         all.x = T)
test_edges.mrg2
setnames(test_edges.mrg2, "pub_year", "article_year")

# >>> Check direct citations ----------------------------------------------
test_edges.mrg2[source_year < article_year]

unique(test_edges.mrg2[source_year < article_year]$cited_by_id)

test_edges.mrg2[source_year < article_year &
                  cited_by_id == "(Wallach & Lundgren 2025)_external review without claim"]
# We must have data that we added opportunistically too.

# Save for Arian to check and fix in main dataset.

test_edges.mrg2[source_year < article_year, ]

setcolorder(test_edges.mrg2, c("cited_by_id", "from_key", "article_id",
                               "to_key", "edge_type", "scientificName", "encounter_method",
                               "source_year", "article_year"))

fwrite(test_edges.mrg2[source_year < article_year, ], "builds/temp/check year inconsistencies.csv")

test_edges.mrg2

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Identify terminal evidence types ----------------------------------------------------------

# >>> Identify terminus of citation chain, by species.---------------------------------
# Instead of starting with graph we should just start with the dataset.
# source <- "(Hess 2014) core source_claim"
#' [The problem is that this drops the duplicate citations per species, though it doesn't actually seem to...]
#
#grepl("Opportunistic", article_node_name) | grepl("Web of Science", article_node_name)
sources <- c(nodes[grepl("core source_claim", node_id)]$node_id,
             "Web of Science_Systematic external review",
             "Opportunistic_Systematic external review",
             "(Wallach & Lundgren 2025)_external review without claim")
sources

setdiff(sources,  nodes$node_id)
nodes[grepl("Opportunistic", node_id)]

# This is per claim. So if a study cites 2 sources for a species, it'll track all of them to their terminuses, leading to 2 chains.
guide <- unique(edges[cited_by_id %in% sources, .(cited_by_id, scientificName)])
guide

guide[cited_by_id == "Opportunistic_Systematic external review"]

# This will create a list of something...
graph_list <- list()
i <- 1

grep("Pseudobulweria becki", guide$scientificName)
guide[337, ]
i <- 467

edges[grepl("nothing", article_id, ignore.case = T)]

for(i in 1:nrow(guide)){
  
  # Create a new graph based on species
  edges2 <- edges[scientificName == guide$scientificName[i], ]
  nodes2 <- nodes[node_id %in% c(edges2$cited_by_id, edges2$article_id)]
  
  if("No citation given" %in% nodes2$evidence_type){
    print(paste("Nothing at", i))
  }
  
  g <- igraph::graph_from_data_frame(d = edges2,
                                     vertices = nodes2,
                                     directed = T)
  g
  
  # Identify nodes that are connected somehow to source
  reachable_nodes <- subcomponent(g, guide$cited_by_id[i], mode = "out")
  
  # Find the node in the subcomponent with out-degree 0 (which means a terminus of a chain)
  ultimate_nodes <- V(g)[reachable_nodes[degree(g, reachable_nodes, mode = "out") == 0]]
  # Get any other nodes that are population or predation studies
  ultimate_nodes <- union(ultimate_nodes,
                          reachable_nodes[(grepl("Predation", name) | grepl("Population", name) | 
                                             grepl("Control", name) | grepl("Lethal", name)) &
                                            !reachable_nodes$name %in% ultimate_nodes$name])
  
  ultimate_nodes$name
  
  # Now the source node
  starts <- V(g)[name %in% guide$cited_by_id[i]]
  
  # Get distances
  dist <- distances(g, 
                    v = starts,
                    to = ultimate_nodes,
                    mode = "out") |>
    as.data.frame() |>
    t() |>
    as.data.frame()
  
  colnames(dist) <- "distance_to_terminus"
  dist$article_id <- row.names(dist)
  setDT(dist)
  
  graph_list[[i]] <- data.table(guide[i, ],
                                dist)
  
  graph_list[[i]]
  edges2[cited_by_id == guide$cited_by_id[i]]
  # graph_list[[i]] should be an equal number of rows or larger, right?
  
  # if(nrow(graph_list[[i]]) < nrow(edges2[cited_by_id == guide$cited_by_id[i]])) cat("FUCK at", i, "\r")
  # Subset igraph:
  
  cat(i, "/", nrow(guide), "\r")
  
}

#
head(graph_list)

# plot(graph_list[[i]])

# edges(g_filtered)

terminus_edges <- rbindlist(graph_list)
terminus_edges[distance_to_terminus > 2, ]
# OK. I think this worked.


# There shouldn't be a core source_claim in article id (instead those would be Nothing cited)
terminus_edges[grepl("core source", article_id)]

which(guide$scientificName == "Perameles eremiana")
guide[467]
guide[1031]

terminus_edges[grepl("nothing", article_id, ignore.case = T)]

# Save this for double checking
fwrite(terminus_edges[grepl("core source", article_id)],
       "builds/citation_network/check_core_source_terminus.csv")

# >>> Extract data from simplified network -------------------------------------------------------
terminus_edges
unique(nodes$evidence_type)
nodes[evidence_type_synthetic %in% c("Core claim",
                                     "External review without claim"), `:=`
      (evidence_type = evidence_type_synthetic)]

#
unique(nodes$evidence_type_simple)
unique(nodes$evidence_type_synthetic)
nodes[evidence_type == "Not in English", 
      evidence_type_synthetic := "Not in English"]

nodes[evidence_type == "No citation given", evidence_type_synthetic := "No citation given"]
nodes[evidence_type == "No citation given", evidence_type_simple := "No citation given"]

terminus_edges[grepl("Opportunistic", cited_by_id)]

unique(nodes$evidence_type_fine)

terminus_edges.mrg <- merge(terminus_edges,
                            nodes[, .(node_id, article_node_name, Peer_reviewed_source,  in_support,
                                      evidence_type_fine,
                                      evidence_type_synthetic, has_data, of_quality)],
                            by.x = "article_id",
                            by.y = "node_id")
terminus_edges.mrg[grepl("Hess", cited_by_id)]

terminus_edges <- copy(terminus_edges.mrg)
remove(terminus_edges.mrg)
unique(terminus_edges$evidence_type_synthetic)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------

# Format data ------------------------------------------------------------------

# >>> Merge year of review/source into cited_by  --------------------------------------------------
unique(years$value)
claim_years <- years[grepl("core source", value) |
                       value %in% c("Web of Science", "Opportunistic", "(Wallach & Lundgren 2025)")]
unique(claim_years$value)
claim_years[grepl("core source", value), value := paste0(value, "_claim")]
claim_years[value == "(Wallach & Lundgren 2025)", value := paste0(value, "_external review without claim")]
claim_years[value == "Web of Science", value := paste0(value, "_Systematic external review")]
claim_years[value == "Opportunistic", value := paste0(value, "_Systematic external review")]

claim_years
setdiff(terminus_edges$cited_by_id, claim_years$value)

claim_years[, key := paste(scientificName, value)]
terminus_edges[, key := paste(scientificName, cited_by_id)]

terminus_edges.mrg <- merge(terminus_edges,
                            unique(claim_years[, .(key, pub_year)]),
                            by = "key",
                            all.x = T)

nrow(terminus_edges.mrg) == nrow(terminus_edges)
#' [Must be TRUE]

setnames(terminus_edges.mrg, "pub_year", "source_year")
terminus_edges.mrg

unique(terminus_edges.mrg$evidence_type_synthetic)

# >>> Save ----------------------------------------------------------------

fwrite(terminus_edges.mrg, "builds/citation_network/terminal_citation_chains.csv")

# >>> Merge node attributes into evidence years ----------------------------

# Now we don't actually want the evidence types in this. We just want source -> article and source year
terminus_simple <- terminus_edges.mrg[, .(scientificName, cited_by_id, source_year,
                                          article_node_name, article_id)]

terminus_simple
evidence_years <- years[!(grepl("core source", value) | value %in% c("Web of Science", 
                                                                     "Opportunistic", 
                                                                     "(Wallach & Lundgren 2025)"))]
evidence_years


# This may increase number of rows because there are multiple evidence types per article
evidence_years.mrg <- merge(evidence_years,
                            unique(nodes[, .(article_node_name, node_id, evidence_inclusion, Peer_reviewed_source,
                                             evidence_type_fine,
                                             evidence_type_synthetic, in_support, has_data, of_quality)]),
                            by.x = "value",
                            by.y = "article_node_name",
                            all.x = F) # because of excluded species
evidence_years.mrg

nrow(evidence_years.mrg)
nrow(evidence_years)
# Makes sense.

evidence_years.mrg
setnames(evidence_years.mrg, "value", "potential_node")
setnames(evidence_years.mrg, "node_id", "potential_evidence")

evidence_years.mrg

# >>> Then do a explosion merge by species, so all combinations of articles are present --------
terminus_simple

#' [OR do we do something like key %in% key, cited := "yes"? After filtering to evidence-species combinations that exist?]

evidence_years.mrg
unique(evidence_years.mrg[potential_node == "(Gillies et al. 2003)", .(scientificName, evidence_type_synthetic)])
nodes[article_node_name == "(Gillies et al. 2003)"]

#
x <- setdiff(evidence_years.mrg$scientificName, terminus_simple$scientificName)
x

# Ahh excluded species
# Ok so what are these??
raw_dat[scientificName %in% x & exclude_species == "included_species"]
# OK, so this is a loop. No terminus since it goes back to original source. Hilarious.
evidence_years.mrg <- evidence_years.mrg[scientificName %in% raw_dat[exclude_species == "included_species"]$scientificName, ]
evidence_years.mrg

terminus_simple <- terminus_simple[scientificName %in% raw_dat[exclude_species == "included_species"]$scientificName, ]
#
terminus_simple[grepl("Pseudobulweria", scientificName)]
# Pseudobulweria becki

x <- setdiff(terminus_simple$scientificName, evidence_years.mrg$scientificName)
unique(terminus_simple[scientificName %in% x, ]$article_id)
# OK, so loops or NOTHING citations. Can we keep those in?

terminus_simple[article_node_name == "(Gillies et al. 2003)"]
# Right these are NOTHING citations
#
terminus_exploded <- merge(evidence_years.mrg[, .(scientificName, potential_node, potential_evidence, pub_year, Peer_reviewed_source,
                                                  evidence_inclusion, evidence_type_fine,
                                                  evidence_type_synthetic, in_support, has_data, of_quality)],
                           terminus_simple[, .(scientificName, cited_by_id, source_year, article_id)],
                           by = "scientificName",
                           all.x = T,
                           all.y = T,
                           allow.cartesian = TRUE)
nrow(terminus_simple)
nrow(terminus_exploded)
terminus_exploded$evidence_type_fine
terminus_exploded

terminus_exploded[potential_node == "(Gillies et al. 2003)"]

#' [So this is every combination of cited papers and all papers. Now we tag whether article_node == potential_article, which means that the article was cited by source]
#' [We then drop potential_evidence and make unique, so that the dataset will be source -> all potential studies, with whether they were cited or not]
terminus_exploded

# >>> Tag whether article was cited or not ------------------------------------
terminus_exploded[article_id == potential_evidence, ]
terminus_exploded[potential_node == "(Gillies et al. 2003)" & article_id == potential_evidence, ]

# This needs to be a summary operation. Not what I originally did.
terminus_exploded[, cited := ifelse(article_id == potential_evidence, 1, 0)]
terminus_exploded[potential_node == "(Gillies et al. 2003)" & cited == 0, ]

# Now summarize by selecting the largest cited of each group. Drop 'article_id' (from terminus)
terminus_exploded
terminus_exploded <- terminus_exploded[, .SD[which.max(cited), !c("article_id")],
                                       by = .(scientificName, cited_by_id, source_year, Peer_reviewed_source,
                                              potential_node, potential_evidence, pub_year,
                                              evidence_inclusion, evidence_type_fine,
                                              evidence_type_synthetic,
                                              in_support, has_data, of_quality)] |> unique()
names(terminus_exploded)
# terminus_exploded <- unique(terminus_exploded[, .(scientificName, cited_by_id, source_year, potential_node, potential_evidence,
#                                                   pub_year, evidence_inclusion, evidence_type_synthetic, in_support, has_data, of_quality,
#                                                   cited)])
terminus_exploded[, .(n = .N), by = .(scientificName, cited_by_id, potential_evidence, evidence_type_synthetic)][n > 1]
#' [Must be 0 rows]

terminus_exploded[, .(n = .N), by = .(scientificName, cited_by_id, potential_evidence)][n > 1]

# Well that's weird.
unique(terminus_exploded[potential_node == "(Gillies et al. 2003)", ])


#
evidence_years.mrg[potential_node == "(Gillies et al. 2003)"]

terminus_exploded

# >>> Publication rate of popuation-with data studies --------------------

unique(evidence_years.mrg$evidence_type_synthetic)

# evidence_years.mrg[, pub_year := as.numeric(pub_year)]
terminus_exploded[, making_claim := ifelse(grepl("claim", cited_by_id), "CLAIM", "NO_CLAIM")]
evidence_years.mrg[, pub_year := as.numeric(pub_year)]
terminus_exploded[, source_year := as.numeric(source_year)]

# We want all studies, not filtered to <= review date
ggplot()+
  geom_density(data = evidence_years.mrg[grepl("Population", evidence_type_synthetic) & has_data == "yes"],
               aes(x = pub_year, fill = in_support),
               alpha = .5)+
  geom_jitter(data = unique(terminus_exploded[, .(cited_by_id, source_year, making_claim)]), 
              height = 0,
              aes(x = source_year, y = 0, color = making_claim), size = 5, shape = "|", stroke = 5)+
  theme_bw()+
  theme(panel.grid = element_blank())


# >>> Filter to only articles available <= review date ------------------------
terminus_exploded[, `:=` (source_year = as.numeric(source_year),
                          pub_year = as.numeric(pub_year))]
terminus_exploded_filtered <- terminus_exploded[complete.cases(source_year, pub_year), ]
terminus_exploded_filtered <- terminus_exploded_filtered[pub_year <= source_year, ]
terminus_exploded_filtered

# Save exploded data frame  --------------------------------------------------------------------
fwrite(terminus_exploded_filtered, "builds/citation_network/citation_probability.csv")

