#
#
# Figures:
# full citation network
# alluvial plot
# probability of evidence being cited
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
library("glmmTMB")
library("broom")
library("broom.mixed")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Full network ------------------------------------------------------------

# >>> Load data -----------------------------------------------------------
nodes <- fread("builds/citation_network/nodes.csv")
edges <- fread("builds/citation_network/edges.csv")

# >>> Collapse opportunistic and web of science -------------------------------

nodes[grepl("Opportunistic", node_id),]
nodes[grepl("Web of Science", node_id),]

nodes[grepl("Opportunistic", node_id) | grepl("Web of Science", node_id), node_id := "Systematic (WoS+Opportunistic)"]
nodes[grepl("Opportunistic", node_id) , article_node_name := "Systematic (WoS+Opportunistic)"]
nodes <- unique(nodes)

edges[grepl("Opportunistic", cited_by_id),]
edges[grepl("Web of Science", cited_by_id),]

edges[grepl("Opportunistic", cited_by_id) | grepl("Web of Science", cited_by_id), cited_by_id := "Systematic (WoS+Opportunistic)"]

# >>> Check that core sources in evidence is treated as the same node as cited_by unless it presents novel data --------
edges[grepl("core source", article_id), ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------

# Plot ----------------------------------------------------

nodes[duplicated(node_id)]
# Must be length 0

# nodes[node_id == "Systematic (WoS + Opportunistic)", article_node_name := "Systematic (WoS + Opportunistic)"]
nodes <- unique(nodes)

# edges[grepl("Opportunistic", cited_by_id) | grepl("Web of Science", cited_by_id), 
#       cited_by_id := "Systematic (WoS + Opportunistic)"]
setdiff(edges$cited_by_id, nodes$node_id)
# Must be length 0

#
unique(nodes$evidence_type_simple)
unique(nodes$evidence_type_synthetic)
nodes[evidence_type == "Not in English", evidence_type_synthetic := "Not in English" ]

#
nodes[grepl("NOTHING", article_node_name), ]
nodes[grepl("NOTHING", article_node_name), evidence_type_synthetic := "No citation"]

nodes[evidence_type_synthetic %in% c("Opinion claim", "Inaccessible", "No claim",
                                     "Does not test claim", "Cites different core source"),
      evidence_type_synthetic_simple := "Excluded"]
nodes[!evidence_type_synthetic %in% c("Opinion claim", "Inaccessible", "No claim", 
                                      "Does not test claim", "Cites different core source"), 
      evidence_type_synthetic_simple := evidence_type_synthetic]

#
unique(nodes$evidence_type_synthetic)
unique(nodes$evidence_type_synthetic_simple)
nodes[evidence_type_synthetic == "Not in English", evidence_type_synthetic_simple := "Excluded"]

# unique(nodes$evidence_type_synthetic_simple)
nodes[grepl("not in support", evidence_type_synthetic_simple), ]
unique(nodes$evidence_type_synthetic_simple)

# nodes[grepl("not in support", evidence_type_synthetic_simple), 
#       evidence_type_synthetic_simple := "Not in support"]
nodes[evidence_type_synthetic_simple == "Population in support with data of quality", 
      evidence_type_synthetic_simple := "Population in support with data"]
nodes[evidence_type_synthetic_simple == "Population not in support with data of quality", 
      evidence_type_synthetic_simple := "Population not in support with data"]
unique(nodes$evidence_type_synthetic_simple)
# nodes[grepl("Opportunistic", article_node_name), evidence_type_synthetic_simple := "Opportunistic"]

unique(nodes$evidence_type_synthetic_simple)

# Create color palettes
unique(nodes$evidence_type_synthetic_simple)

nodes[evidence_type == "No citation given", evidence_type_synthetic_simple := "Nothing cited"]

nodes[is.na(evidence_type_synthetic_simple)]
nodes[evidence_type_synthetic_simple == "Control program in support without data",
      evidence_type_synthetic_simple := "Population in support without data"]

#
lvls <- c("Core claim", "External review without claim",
          # "Opportunistic",
          "Excluded", "Nothing cited",
          # "Not in English",
          # "Not in support",
          "Predation in support without data", 
          "Predation not in support without data",
          # "Control program in support without data",
          "Population not in support without data",
          "Population not in support with data",
          "Population in support without data",
          "Population in support with data")
setdiff(nodes$evidence_type_synthetic_simple, lvls)

nodes[evidence_type_synthetic_simple == "Not in English or Spanish", evidence_type_synthetic_simple := "Excluded"]

nodes$evidence_type_synthetic_simple <- factor(nodes$evidence_type_synthetic_simple,
                                               levels = lvls)
template <- rep("XXXX", length(levels(nodes$evidence_type_synthetic_simple)))
names(template) <- (levels(nodes$evidence_type_synthetic_simple))

dput(template)
node_fill <- c(`Core claim` = "hotpink", `External review without claim` = "pink", 
               Excluded = "grey", `Nothing cited` = "black", 
               
               `Predation in support without data` = "#a6d3a0", 
               `Predation not in support without data` = "#40531b", 
               
               `Population not in support without data` = "indianred4", `Population not in support with data` = "indianred", 
               `Population in support without data` = "dodgerblue4", `Population in support with data` = "dodgerblue"
)


# Let's do node size based on evidence type, regardless of support.
nodes[evidence_type_synthetic_simple == "Opportunistic", ]
nodes[evidence_type_synthetic_simple == "Opportunistic", evidence_type_simple := "Opportunistic"]

template <- rep(1, length(levels(nodes$evidence_type_simple)))
names(template) <- (levels(nodes$evidence_type_simple))
dput(template)
node_size <- c(`Core claim` = 5, `External review without claim` = 5, `Opportunistic` = 1,
               Excluded = 1, 
               `Predation without data` = 2, #`Control program without data` = 2, 
               `Population without data` = 3, `Population with data` = 4)


unique(nodes$of_quality)
stroke_color <- c("no" = "black", 
                  "yes" = "gold")

stroke_width <- c("no" = .01, 
                  "yes" = 2)

unique(nodes$in_support)
# support_shape <- c("Not relevant" = 21,
#                    "Yes" = 24,
#                    "No" = 25)

# >>> Network -------------------------------------------------------------
edges[grepl("NOTHING", article_id)]

# Create network
edges.sub <- edges[edge_type == "citation", ]
nodes.sub <- nodes[node_id %in% c(edges.sub$cited_by_id, edges.sub$article_id)]

edges.sub[scientificName == "Nannoscincus hanchisteus"]

igraph.gr <- igraph::graph_from_data_frame(d = edges.sub, 
                                           vertices = nodes.sub,
                                           directed = T)
igraph.gr


graph <- tidygraph::as_tbl_graph(igraph.gr)
#

unique(edges$edge_type)
#
unique(nodes$evidence_type_synthetic)
#

# >>> Plot ----------------------------------------------------------------
unique(nodes$evidence_type_synthetic_simple)
labels <- c(`Core claim` = "Core claim", 
            `External review without claim` = "External source without claim", #`Opportunistic` = "Opportunistic",
            `Nothing cited` = "Nothing cited",
            `Excluded` = "No primary data found", #`Not in support` = "Not in support", 
            `Predation in support without data` = "Predation record without data", 
            `Predation not in support without data` = "Predation record without data", 
            
            `Population in support without data` = "Population - description", 
            `Population in support with data` = "Population - study",
            `Population not in support without data` = "Population - description", 
            `Population not in support with data` = "Population - study")

setdiff(unique(nodes$evidence_type_synthetic_simple),
        names(labels))
setdiff(names(labels),
        unique(nodes$evidence_type_synthetic_simple))

p1 <- ggraph(graph, layout = "kk")+
  geom_edge_fan(arrow = arrow(length = unit(1, 'mm')), 
                start_cap = circle(1, 'mm'),
                end_cap = circle(1, 'mm'),
                color = "grey50",
                alpha = .5,
                linewidth = 0.25) + 
  # geom_edge_loop()+ #aes(color = edge_type)
  geom_node_point(aes(fill = evidence_type_synthetic_simple,
                      color = of_quality,
                      stroke = of_quality,
                      size = evidence_type_simple), 
                  shape = 21, #stroke = 2,
                  alpha = .75)+
  scale_fill_manual("Article type",
                    values = node_fill,
                    # breaks = levels(nodes$evidence_type_synthetic_simple),
                    labels = labels)+
  scale_size_manual("Article type",
                    values = node_size *2,
                    breaks = levels(nodes$evidence_type_simple))+ 
  guides(size = "none",
         fill = guide_legend(nrow = 4, byrow = TRUE))+
  # scale_linewidth_manual(values = stroke_width) + 
  scale_discrete_manual("Quality research",
                        aesthetics = "stroke", 
                        values = c(1, 2))+
  # scale_shape_manual(values = support_shape) +
  scale_color_manual("Quality research",
                     values = stroke_color )+
  # scale_fill_manual("trophic level / guild", values = TL_palette)+
  # scale_fill_brewer("trophic level", palette = "Spectral")+
  # geom_node_text(aes(label = name), #label.size = NA, 
  #                repel = TRUE)+
  theme_graph()+
  theme(legend.position = 'bottom')
p1

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------

# Possible to structure this at all? --------------------------------------
graph <- create_notable('meredith')

# Add x and y columns to the node data, fixing the first node at (0, 0)
# and leaving others as NA
graph <- graph |>
  activate(nodes) |>
  #   mutate(x = c(0, rep(NA, graph_order() - 1)),
  #          y = c(0, rep(NA, graph_order() - 1))) |>
  mutate(col = c("central", rep("non-central", graph_order()-1)))
#
# graph
# The stress layout automatically uses existing x/y columns if present
ggraph(graph, layout = 'stress',
       x = c(0, rep(NA, 69)),
       y = c(0, rep(NA, 69))) +
  geom_edge_link() +
  geom_node_point(aes(color = col, size = col)) #+
# geom_node_label(aes(label = seq_len(graph_order(graph))), repel = TRUE)

ggraph(graph, layout = 'stress',
       x = c(10, rep(NA, 69)),
       y = c(0, rep(NA, 69))) +
  geom_edge_link() +
  geom_node_point(aes(color = col, size = col)) #+

# Unfortunately that didn't work for some reason with full network
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -----------------------------------------
# Probability of a population study being cited by support/not support ---------------------------

#' [Amplification: the process of citing papers that do not contain primary data]
#' [Bias: Citing only papers in support]
#' [Invention: Misciting papers not in support as if they were in support]
# 
# Reanalyze with just claims, on in support / not support
# Sensitivity analysis with just Doherty, Medina, and Hess (all from tables no potentially text-misinterpretations)
# 

# >>> Load and format data ----------------------------------------------------
terminus_exploded_filtered <- fread("builds/citation_network/citation_probability.csv")

# >>> Model -----------------------------------------------------------------
#' [Only look at the sources making a claim]
# Create some predictors.
terminus_exploded_filtered[, making_claim := ifelse(grepl("_claim", cited_by_id), "CLAIM", "NO_CLAIM")]
terminus_exploded_filtered <- terminus_exploded_filtered[making_claim == "CLAIM", ]

unique(terminus_exploded_filtered$in_support)

unique(terminus_exploded_filtered$cited_by_id)
# terminus_exploded_filtered[grepl("O")]

m <- glmmTMB(cited ~ in_support + (1|cited_by_id) + (1|scientificName),
             family = binomial(link = "logit"),
             data = terminus_exploded_filtered[grepl("Population", evidence_type_synthetic) &
                                                 has_data == "yes", ])
summary(m)
tidy(m)

confint(m)
pred <- predict(m, data.table(in_support = c("Yes", "No")),
                re.form = NA, se = TRUE)

# predict_response(m)
# summary(m)
pred <- as.data.frame(pred) |> setDT()
crit <- qnorm(0.975)
pred[, lower_ci := fit - (se.fit * 1.96)]
pred[, upper_ci := fit + (se.fit * 1.96)]
pred[, fit := plogis(fit)]
pred[, lower_ci := plogis(lower_ci)]
pred[, upper_ci := plogis(upper_ci)]

#
pred$in_support <- c("Yes", "No")
pred

# Hmmm. 
citation.p <- ggplot(data = pred, 
                     aes(x = in_support, 
                         fill = in_support, y = (fit), 
                         ymin = (lower_ci), ymax = (upper_ci)))+
  geom_errorbar(position = position_dodge(width = .75), width = .25)+
  geom_point(position = position_dodge(width = .75),
                  shape = 21, size = 4)+
  ylab("Probability of a claimant citing\nan available population study with data")+
  scale_fill_manual("In support",
                    values = c("No" = "indianred", "Yes" = "dodgerblue"))+
  xlab("Negative association found")+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")
citation.p

#
#
# Just datasets with lists:
unique(terminus_exploded_filtered$cited_by_id)

unique(terminus_exploded_filtered$cited_by_id)
m <- glmmTMB(cited ~ in_support + (1|scientificName),
             family = binomial(link = "logit"),
             data = terminus_exploded_filtered[grepl("Population", evidence_type_synthetic) &
                                                 has_data == "yes" &
                                                 cited_by_id %in% c("(Doherty et al. 2016) core source_claim",
                                                                    "(Medina et al. 2011) core source_claim",
                                                                    "(Hess 2014) core source_claim")])
summary(m)
pred <- predict(m, data.table(in_support = c("Yes", "No")),
                re.form = NA, se = TRUE)

# predict_response(m)
# summary(m)
pred <- as.data.frame(pred) |> setDT()
crit <- qnorm(0.975)
pred[, lower_ci := fit - (se.fit * 1.96)]
pred[, upper_ci := fit + (se.fit * 1.96)]
pred[, fit := plogis(fit)]
pred[, lower_ci := plogis(lower_ci)]
pred[, upper_ci := plogis(upper_ci)]

#
pred$in_support <- c("Yes", "No")
pred

# Hmmm. 
ggplot(data = pred, aes(x = in_support, y = plogis(fit), 
                        ymin = plogis(lower_ci), ymax = plogis(upper_ci)))+
  geom_pointrange()+
  ylab("Probability of an available population study with data being cited")+
  xlab(NULL)+
  theme_bw()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------
# Citation terminus -----------------------------------------------------------

terminus_edges <- fread("builds/citation_network/terminal_citation_chains.csv")

terminus_edges <- terminus_edges[cited_by_id != "Opportunistic_Systematic external review"]
unique(terminus_edges$evidence_type_fine)

# Number of citations:
terminus_freq <- terminus_edges[, .(n = .N),
                                by = .(cited_by_id, evidence_type_synthetic, 
                                       has_data, of_quality, in_support)]

sort(unique(terminus_freq$evidence_type_synthetic))

# >>> Simple bar chart of citation terminus types ----------------------------------------------------

unique(terminus_freq$cited_by_id)
terminus_freq[grepl("Opportunistic", cited_by_id)]

unique(terminus_edges$evidence_type_synthetic)

unique(terminus_edges[grepl("EXCLUDE", article_id)]$evidence_type_fine)

terminus_edges[evidence_type_synthetic %in% c("Does not test claim", "Inaccessible",
                                              "Not in English or Spanish",
                                              "No claim", "Opinion claim"),
                     evidence_type_synthetic := "Excluded"]

unique(terminus_edges$cited_by_id)
terminus_edges[, making_claim := ifelse(grepl("core source_claim", cited_by_id), 
                                        "making claim", "no claim")]

terminus_freq.simple <- terminus_edges[, .(n = .N),
                                       by = .(evidence_type_synthetic, making_claim,
                                              has_data, of_quality, in_support)]

terminus_freq.simple[grepl("Population", evidence_type_synthetic), evidence_simple := "Population"]
terminus_freq.simple[grepl("Predation", evidence_type_synthetic), evidence_simple := "Predation"]
unique(terminus_freq.simple[is.na(evidence_simple)]$evidence_type_synthetic)
terminus_freq.simple[evidence_type_synthetic %in% c("Core claim", "No citation given",
                                                    "No claim", "Inaccessible", "Opinion claim",
                                                    "Excluded", "Not in English or Spanish",
                                                    "Does not test claim"), evidence_simple := "Excluded"]

terminus_freq.simple$evidence_simple <- factor(terminus_freq.simple$evidence_simple,
                                               levels = c("Excluded", "Predation", "Population"))

unique(terminus_freq.simple$evidence_type_synthetic)

unique(terminus_freq.simple$of_quality)

terminus_freq.simple$evidence_type_synthetic <- factor(terminus_freq.simple$evidence_type_synthetic,
                                                       levels = rev(c( "Core claim", "No citation given", 
                                                                       "Excluded", 
                                                                  "Predation in support without data", 
                                                                  "Predation not in support without data", 
                                                                  "Population not in support without data",
                                                                  "Population not in support with data",
                                                                  "Population not in support with data of quality",
                                                                  "Population in support without data", 
                                                                  "Population in support with data", 
                                                                  "Population in support with data of quality")))
labs <- c(`Core claim` = "Core claim", `No citation given` = "No citation given", 
          Excluded = "No primary data found", `Predation in support without data` = "Predation in support without data", 
          `Predation not in support without data` = "Predation not in support without data", 
          `Population not in support without data` = "Population not in support without data", 
          `Population not in support with data` = "Population not in support with data", 
          `Population not in support with data of quality` = "Population not in support with data of quality", 
          `Population in support without data` = "Population in support without data", 
          `Population in support with data` = "Population in support with data", 
          `Population in support with data of quality` = "Population in support with data of quality"
)

unique(terminus_freq.simple$evidence_type_synthetic)

fill_pal <- c(`Core claim` = "hotpink", `Excluded` = "grey", `External review without claim` = "pink", 
              Inaccessible = "grey", `Does not test claim` = "grey", `Opinion claim` = "grey", 
              `No claim` = "grey", `No citation given` = "black", 
              `Predation in support without data` = "#a6d3a0", 
              `Predation not in support without data` = "#40531b", 
              `Population not in support without data` = "indianred4", 
              `Population not in support with data` = "indianred", 
              `Population not in support with data of quality` = "indianred", 
              `Population in support without data` = "dodgerblue4", 
              `Population in support with data` = "dodgerblue",
              `Population in support with data of quality` = "dodgerblue")
setdiff(terminus_freq.simple$evidence_type_synthetic, names(fill_pal))

terminus_freq.simple[in_support == "Not relevant", in_support := "No"]


#
cites.p.included <- ggplot(data = terminus_freq.simple[evidence_simple %in% c("Population", "Predation") &
                                                         making_claim == "making claim"], #[making_claim == "making claim"],
       aes(y = evidence_simple, fill = evidence_type_synthetic, 
           color = of_quality, #group = in_support,# label = value, 
           x = n
       ))+
  geom_col(lwd = 1) +
  scale_y_discrete(breaks = c("Population", "Predation", "Excluded"),
                   labels = c("Population", "Predation", "No primary data found"))+
  scale_color_manual(name = "Of quality",
                     values = c("no" = "transparent",
                                "yes" = "gold"),
                     na.value = "black")+
  scale_fill_manual(values = fill_pal,
                    labels = labs)+
  xlab("Terminus of citation chains\n(total number of citations)")+
  ylab(NULL)+
  # geom_text(stat = "stratum", size = 3, color = "black") +
  # coord_flip()+
  coord_cartesian(xlim = c(0, 527))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside")
cites.p.included

# >>> evidence type fine just by sources making a claim--------------------------------------------------
# This is really tricky...Getting the levels right...Ugh
# I hate making plots like this...

terminus_edges
terminus_edges[evidence_type_synthetic == "No citation given", evidence_type_fine := "No citation given"]

unique(terminus_edges$evidence_type_synthetic)

unique(terminus_edges[grepl("EXCLUDE", article_id)]$evidence_type_fine)

unique(terminus_edges[, .(evidence_type_fine, evidence_type_synthetic)])

terminus_edges[, evidence_inclusion := ifelse(grepl("INCLUDE", article_id), "Evidence", "Excluded")]

terminus_freq.simple <- terminus_edges[, .(n = .N),
                                       by = .(evidence_type_fine, making_claim,
                                              evidence_inclusion,
                                              has_data, of_quality, in_support)]

terminus_freq.simple[grepl("Population", evidence_type_fine), evidence_simple := "Population"]
terminus_freq.simple[grepl("Predation", evidence_type_fine), evidence_simple := "Predation"]
unique(terminus_freq.simple[is.na(evidence_simple)]$evidence_type_fine)
terminus_freq.simple[evidence_type_fine == "No citation given", evidence_simple := "No citation given"]

unique(terminus_freq.simple[is.na(evidence_simple)]$evidence_type_fine)
terminus_freq.simple[is.na(evidence_simple) , evidence_simple := "Excluded"]

unique(terminus_freq.simple$evidence_simple)
terminus_freq.simple$evidence_simple <- factor(terminus_freq.simple$evidence_simple,
                                               levels = c("No citation given", "Excluded", "Predation", "Population"))

unique(terminus_freq.simple$of_quality)
unique(terminus_freq.simple$evidence_type_fine)
terminus_freq.simple[, evidence_type_fine := gsub(" of quality", "", evidence_type_fine)]

#

lvls <- c( "Core claim", 
           "No citation given", 
           "Failure to access or locate online full citation",
           "No claim",
           "Expert opinion",
           "Unpublished data or article",
           "Personal communication",
           
           "Missing reference",
           "Does not test claim", 
           "Not in English or Spanish", 
           "Modelling without data",
          
           "Predation in support without data", 
           "Predation not in support without data", 
           "Population not in support without data",
           "Population not in support with data",
           "Population in support without data", 
           "Population in support with data")

setdiff(terminus_freq.simple$evidence_type_fine, lvls)

#
terminus_freq.simple$evidence_type_fine <- factor(terminus_freq.simple$evidence_type_fine,
                                                       levels = rev(lvls))

labs <- c(`Core claim` = "Core claim", `No citation given` = "No citation given", 
          `Modelling without data` = "Modelling without data", `Does not test claim` = "Does not test claim", 
          `Personal communication` = "Personal communication", `Missing reference` = "Missing reference", 
          `Unpublished data or article` = "Unpublished", 
          `Not in English or Spanish` = "Not in English or Spanish", `Expert opinion` = "Expert opinion", 
          `Failure to access or locate online full citation` = "Unavailable", 
          `No claim` = "No claim", `Predation in support without data` = "Predation in support without data", 
          `Predation not in support without data` = "Predation not in support without data", 
          `Population not in support without data` = "Population not in support without data", 
          `Population not in support with data` = "Population not in support with data",  
          `Population in support without data` = "Population in support without data", 
          `Population in support with data` = "Population in support with data"
)

unique(terminus_freq.simple$evidence_simple)
terminus_freq.simple[is.na(evidence_simple), evidence_simple := "No citation given"]
# terminus_freq.simple[evidence_simple %in% c("Population", "Predation"),
#                      evidence_simple := evidence_type_fine]

unique(terminus_freq.simple$evidence_simple)
fill_pal <- c(`Excluded` = "grey", `No citation given` = "black", 
              `Predation in support without data` = "#a6d3a0", 
              `Predation not in support without data` = "#40531b", 
              `Population not in support without data` = "indianred4", 
              `Population not in support with data` = "indianred", 
              # `Population not in support with data of quality` = "indianred", 
              `Population in support without data` = "dodgerblue4", 
              `Population in support with data` = "dodgerblue"#,
              # `Population in support with data of quality` = "dodgerblue"
              )
setdiff(terminus_freq.simple$evidence_simple, names(fill_pal))

terminus_freq.simple[in_support == "Not relevant", in_support := "No"]

#
max(terminus_freq.simple[making_claim == "making claim" &
                           evidence_inclusion == "Excluded"]$n)

terminus_freq.simple
cites.p.excluded <- ggplot(data = terminus_freq.simple[making_claim == "making claim" &
                                                         evidence_inclusion == "Excluded"], 
                  aes(y = evidence_type_fine, fill = evidence_simple, 
                      color = of_quality, 
                      x = n
                  ))+
  geom_col(lwd = 1) +
  # facet_wrap(~evidence_inclusion, scales = "free_y")+
  scale_color_manual(name = "Of quality",
                     values = c("no" = "transparent",
                                "yes" = "gold"),
                     na.value = "black")+
  scale_fill_manual(values = fill_pal,
                    labels = labs)+
  scale_y_discrete(breaks = names(labs),
                   labels = labs)+
  xlab("Terminus of citation chains\n(total number of citations)")+
  ylab(NULL)+
  # geom_text(stat = "stratum", size = 3, color = "black") +
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "none")
cites.p.excluded


# terminus_freq.simple[]
# 
# terminus_freq.simple[making_claim == "making claim" &
#                        evidence_inclusion == "Evidence"]
#


#
# cites.p.included <- ggplot(data = terminus_freq.simple[making_claim == "making claim" &
#                                                          evidence_inclusion == "Evidence"], 
#                            aes(y = evidence_simple, fill = evidence_type_fine, 
#                                color = of_quality, group = in_support,
#                                x = n
#                            ))+
#   geom_col(lwd = 1, position = position_dodge()) +
#   facet_wrap(~evidence_inclusion, scales = "free_y")+
#   scale_color_manual(name = "Of quality",
#                      values = c("no" = "transparent",
#                                 "yes" = "gold"),
#                      na.value = "black")+
#   scale_fill_manual(values = fill_pal,
#                     labels = labs)+
#   xlab("Terminus of citation chains\n(total number of citations)")+
#   ylab(NULL)+
#   # geom_text(stat = "stratum", size = 3, color = "black") +
#   theme_bw()+
#   theme(panel.grid = element_blank(),
#         strip.background = element_blank(),
#         panel.border = element_blank(),
#         legend.position = "none")
# cites.p.included

# >>> By class ------------------------------------------------------------
spp <- fread("builds/claims/species_claims_tidy_populated.csv")
spp <- unique(spp[, .(scientificName, class)])

setdiff(terminus_edges$scientificName, spp$scientificName)

terminus_edges.mrg <- merge(terminus_edges,
                            spp,
                            by = "scientificName")
unique(terminus_edges.mrg$class)


terminus_edges.mrg[evidence_type_synthetic %in% c("Does not test claim", "Inaccessible",
                                              "No claim", "Opinion claim"),
               evidence_type_synthetic := "Excluded"]

unique(terminus_edges.mrg$cited_by_id)
terminus_edges.mrg[, making_claim := ifelse(grepl("core source_claim", cited_by_id), 
                                        "making claim", "no claim")]

terminus_freq.simple <- terminus_edges.mrg[, .(n = .N),
                                       by = .(evidence_type_synthetic, making_claim,
                                              has_data, of_quality, in_support,
                                              class)]

terminus_freq.simple[grepl("Population", evidence_type_synthetic), evidence_simple := "Population"]
terminus_freq.simple[grepl("Predation", evidence_type_synthetic), evidence_simple := "Predation"]
unique(terminus_freq.simple[is.na(evidence_simple)]$evidence_type_synthetic)
terminus_freq.simple[evidence_type_synthetic %in% c("Core claim", "No citation given",
                                                    "No claim", "Inaccessible", "Opinion claim",
                                                    "Excluded", "Not in English or Spanish",
                                                    "Does not test claim"), evidence_simple := "Excluded"]

terminus_freq.simple$evidence_simple <- factor(terminus_freq.simple$evidence_simple,
                                               levels = c("Excluded", "Predation", "Population"))

terminus_freq.simple[evidence_type_synthetic == "Not in English or Spanish", evidence_type_synthetic := "Excluded"]
setdiff(unique(terminus_freq.simple$evidence_type_synthetic), c( "Core claim", "No citation given", 
                                                                 "Excluded", 
                                                                 "Predation in support without data", 
                                                                 "Predation not in support without data", 
                                                                 "Population not in support without data",
                                                                 "Population not in support with data",
                                                                 "Population not in support with data of quality",
                                                                 "Population in support without data", 
                                                                 "Population in support with data", 
                                                                 "Population in support with data of quality"))


unique(terminus_freq.simple$of_quality)

terminus_freq.simple$evidence_type_synthetic <- factor(terminus_freq.simple$evidence_type_synthetic,
                                                       levels = rev(c( "Core claim", "No citation given", 
                                                                       "Excluded", 
                                                                       "Predation in support without data", 
                                                                       "Predation not in support without data", 
                                                                       "Population not in support without data",
                                                                       "Population not in support with data",
                                                                       "Population not in support with data of quality",
                                                                       "Population in support without data", 
                                                                       "Population in support with data", 
                                                                       "Population in support with data of quality")))
labs <- c(`Core claim` = "Core claim", `No citation given` = "No citation given", 
          Excluded = "No primary data found", `Predation in support without data` = "Predation in support without data", 
          `Predation not in support without data` = "Predation not in support without data", 
          `Population not in support without data` = "Population not in support without data", 
          `Population not in support with data` = "Population not in support with data", 
          `Population not in support with data of quality` = "Population not in support with data of quality", 
          `Population in support without data` = "Population in support without data", 
          `Population in support with data` = "Population in support with data", 
          `Population in support with data of quality` = "Population in support with data of quality"
)

unique(terminus_freq.simple$evidence_type_synthetic)

fill_pal <- c(`Core claim` = "hotpink", `Excluded` = "grey", `External review without claim` = "pink", 
              Inaccessible = "grey", `Does not test claim` = "grey", `Opinion claim` = "grey", 
              `No claim` = "grey", `No citation given` = "black", 
              `Predation in support without data` = "#a6d3a0", 
              `Predation not in support without data` = "#40531b", 
              `Population not in support without data` = "indianred4", 
              `Population not in support with data` = "indianred", 
              `Population not in support with data of quality` = "indianred", 
              `Population in support without data` = "dodgerblue4", 
              `Population in support with data` = "dodgerblue",
              `Population in support with data of quality` = "dodgerblue")
setdiff(terminus_freq.simple$evidence_type_synthetic, names(fill_pal))

terminus_freq.simple[in_support == "Not relevant", in_support := "No"]

#
unique(terminus_freq.simple$class)
# terminus_freq.simple[class %in% c("Reptiles", "Amphibians"), class := "Reptiles & Amphibians"]
terminus_freq.simple$class <- factor(terminus_freq.simple$class ,
                                     levels = c("Birds", "Reptiles", "Mammals", "Amphibians"))
#
cites.p.class <- ggplot(data = terminus_freq.simple, #[making_claim == "making claim"],
                  aes(x = evidence_simple, fill = evidence_type_synthetic, 
                      color = of_quality, #group = in_support,# label = value, 
                      y = n
                  ))+
  facet_grid(class~making_claim, #ncol = 1,
             # strip.position = "left",
             labeller = as_labeller(c("making claim" = "Making claim",
                                      "no claim" = "No claim",
                                      "Mammals" = "Mammals",
                                      "Birds" = "Birds", 
                                      "Reptiles" = "Reptiles",
                                      "Amphibians" = "Amphibians")))+
  geom_col(lwd = 1) +
  # geom_stratum(alpha = 1, aes(fill = type_simple, color = of_quality
  # ),
  # reverse = TRUE)+
  scale_x_discrete(breaks = c("Population", "Predation", "Excluded"),
                   labels = c("Population", "Predation", "No primary data found"))+
  scale_color_manual(name = "Of quality",
                     values = c("no" = "transparent",
                                "yes" = "gold"),
                     na.value = "black")+
  scale_fill_manual(NULL,
                    values = fill_pal,
                    labels = labs)+
  ylab("Terminus of citation chains\n(total number of citations)")+
  xlab(NULL)+
  # geom_text(stat = "stratum", size = 3, color = "black") +
  coord_flip()+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside")
cites.p.class

ggsave("figures/SI/citation terminus by class.pdf", width = 12, height = 8)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------

# Patchwork ---------------------------------------------------------------
# right_bar <- (alluv.p + citation.p) + plot_layout(heights = c(.66, .33))
# (p1 + theme(legend.position = "bottom")) + right_bar + plot_layout(widths = c(.75, .25))
# 
# (p1 + theme(legend.position = "none")) / 
#   (cites.p + theme(legend.position = "none") + citation.p + plot_layout(widths = c(.66, .33)))
# right_bar <- ((cites.p + theme(legend.position = "none")) / citation.p) 
# (p1 + theme(legend.position = "bottom")) + right_bar + plot_layout(widths = c(.66, .33))

# ggsave("figures/main_text/citations_raw.pdf", width = 8.5, height = 11,
#        device = cairo_pdf)


# Alternative:
citation.p.flip <- citation.p + coord_flip()
citation.p.flip

cites.p.included <- cites.p.included + theme(legend.position = "none")

(p1 + theme(legend.position = "none")) / 
  (cites.p.excluded + theme(legend.position = "none") + 
     cites.p.included / citation.p.flip)

ggsave("figures/main_text/citations_raw.pdf", width = 8.5, height = 11,
       device = cairo_pdf)
# cites.p.excluded + cites.p.included


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------

# Proportion excluded by peer-reviewed ------------------------------------
nodes

excluded <- nodes[evidence_inclusion == "EXCLUDE" & evidence_type_synthetic != "No citation", ]
unique(excluded[is.na(Peer_reviewed_source)]$article_node_name)

excluded[, .(n = uniqueN(article_node_name)), by = .(Peer_reviewed_source)]



