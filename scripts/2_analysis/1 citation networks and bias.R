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
# Number of citations:
terminus_freq <- terminus_edges[, .(n = .N),
                                by = .(cited_by_id, evidence_type_synthetic, 
                                       has_data, of_quality, in_support)]

sort(unique(terminus_freq$evidence_type_synthetic))

# >>> Simple bar chart of citation terminus types ----------------------------------------------------

unique(terminus_freq$cited_by_id)
terminus_freq[grepl("Opportunistic", cited_by_id)]


terminus_edges[evidence_type_synthetic %in% c("Does not test claim", "Inaccessible",
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
                                                    "Excluded",
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
cites.p <- ggplot(data = terminus_freq.simple, #[making_claim == "making claim"],
       aes(x = evidence_simple, fill = evidence_type_synthetic, 
           color = of_quality, #group = in_support,# label = value, 
           y = n
       ))+
  facet_wrap(~making_claim, ncol = 1,
             strip.position = "left",
             labeller = as_labeller(c("making claim" = "Making claim",
                                      "no claim" = "No claim")))+
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
  scale_fill_manual(values = fill_pal,
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
cites.p


# >>> By class ------------------------------------------------------------
spp <- fread("builds/species_claims_tidy_populated.csv")
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

(p1 + theme(legend.position = "none")) / 
  (cites.p + theme(legend.position = "none") + citation.p + plot_layout(widths = c(.66, .33)))
# right_bar <- ((cites.p + theme(legend.position = "none")) / citation.p) 
# (p1 + theme(legend.position = "bottom")) + right_bar + plot_layout(widths = c(.66, .33))

ggsave("figures/main_text/citations_raw.pdf", width = 8.5, height = 11,
       device = cairo_pdf)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------

# Proportion excluded by peer-reviewed ------------------------------------
nodes

excluded <- nodes[evidence_inclusion == "EXCLUDE" & evidence_type_synthetic != "No citation", ]
unique(excluded[is.na(Peer_reviewed_source)]$article_node_name)

excluded[, .(n = uniqueN(article_node_name)), by = .(Peer_reviewed_source)]



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# OLD ---------------------------------------------------------------------

# >>> Test ggalluvial -----------------------------------------------------
#' 
#' test <- alluvial::Refugees
#' setDT(test)
#' 
#' test[, year := c(rep("<2000", 50),
#'                  rep(">2000", 60))]
#' test[1:16, outcome := c(rep("A", 8),
#'                         rep("B", 8))]
#' 
#' ggplot(data = test,
#'        aes(y = refugees, axis1 = country, axis2 = year, axis3 = outcome))+
#'   geom_alluvium(width = 1/12) +
#'   geom_stratum(width = 1/12, fill = "black", color = "grey")
#' 
#' # >>> Test plot 1 ---------------------------------------------------------
#' 
#' terminus_freq[, evidence_type_synthetic := gsub(" of quality", "", evidence_type_synthetic)]
#' unique(terminus_freq$evidence_type_synthetic)
#' unique(terminus_freq$in_support)
#' 
#' terminus_freq[grepl("Predation", evidence_type_synthetic), 
#'               evidence_type_synthetic := gsub(" with data", "", evidence_type_synthetic)]
#' terminus_freq[grepl("Predation", evidence_type_synthetic), 
#'               evidence_type_synthetic := gsub(" without data", "", evidence_type_synthetic)]
#' unique(terminus_freq$evidence_type_synthetic)
#' 
#' fill_pal <- c(`Core claim` = "hotpink", `External review without claim` = "pink", 
#'                Inaccessible = "grey", `Does not test claim` = "grey",
#'                `Opinion claim` = "grey", `No claim` = "grey",
#'                `No citation given` = "black", 
#'                `Predation in support` = "#749C75", `Predation not in support` = "#F8FA90", 
#'                `Population not in support without data` = "indianred4", `Population not in support with data` = "indianred", 
#'                `Population in support without data` = "dodgerblue4", `Population in support with data` = "dodgerblue"
#' )
#' 
#' setdiff(unique(terminus_freq$evidence_type_synthetic), names(node_fill))
#' 
#' # setdiff(names(node_fill), unique(terminus_freq$evidence_type_synthetic))
#' # I think we'll have to color those in inkscape. Might look stupid anyways.
#' 
#' #
#' ggplot(data = terminus_freq,
#'        aes(y = n, axis1 = cited_by_id, axis2 = evidence_type_synthetic))+
#'   geom_alluvium(width = 1/12, aes(fill = evidence_type_synthetic, color = of_quality)) +
#'   geom_stratum(width = 1/12, fill = "white", color = "grey")+
#'   geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
#'   scale_color_manual(values = c("no" = "transparent",
#'                                 "yes" = "gold"))+
#'   scale_fill_manual(values = fill_pal)+
#'   ylab("Number of citations")+
#'   theme_bw()+
#'   theme(panel.grid = element_blank())
#' 
#' # >>> Long format plot ----------------------------------------------------
#' #' [This is really difficult lol]
#' terminus_freq[, id := seq(1:.N)]
#' terminus_freq[, type_simple := evidence_type_synthetic]
#' terminus_freq[in_support == "Yes", in_support := "In support"]
#' terminus_freq[in_support == "No", in_support := "Not in support"]
#' unique(terminus_freq$in_support)
#' 
#' terminus_freq[evidence_type_synthetic == "Predation in support"]
#' 
#' 
#' #
#' unique(terminus_freq$in_support)
#' terminus_freq.mlt <- melt(terminus_freq,
#'                           measure.vars = c("cited_by_id", "evidence_type_synthetic",
#'                                            "in_support"))
#' # dput(levels(terminus_freq$evidence_type_synthetic))
#' 
#' terminus_freq.mlt[value %in% c("In support", "Not in support"),]
#' 
#' terminus_freq.mlt[!value %in% c("In support", "Not in support"),]
#' terminus_freq.mlt[!value %in% c("In support", "Not in support"),
#'                   of_quality := "not relevant"]
#' 
#' # terminus_freq.mlt <- terminus_freq.mlt[!is.na(value), ]
#' unique(terminus_freq.mlt$value)
#' 
#' dput(unique(terminus_freq$cited_by_id))
#' dput(unique(terminus_freq$evidence_type_synthetic))
#' dput(unique(terminus_freq$in_support))
#' 
#' unique(terminus_freq.mlt$value)
#' terminus_freq.mlt[value == "In support" & of_quality == "yes", 
#'                   value := "In support with qualities"]
#' terminus_freq.mlt[value == "Not in support" & of_quality == "yes", 
#'                   value := "Not in support with qualities"]
#' 
#' terminus_freq.mlt$value <- factor(terminus_freq.mlt$value ,
#'                                   levels = c("(Doherty et al. 2016) core source_claim", "(IUCN 2025) core source_claim", 
#'                                              "Web of Science_Systematic external review", "(Hume 2017) core source_claim", 
#'                                              "(Medina et al. 2011) core source_claim", "(Garnett & Baker 2021) core source_claim", 
#'                                              "(Wallach & Lundgren 2025)_external review without claim", "(Woinarski et al. 2014) core source_claim", 
#'                                              "(Garnett et al. 2011) core source_claim", "(Radford et al. 2018) core source_claim", 
#'                                              "(Dickman 1996b) core source_claim", "(Campolina et al. 2024) core source_claim", 
#'                                              "(Oedin et al. 2021) core source_claim", "(Welch & Leppanen 2017) core source_claim", 
#'                                              "(Alberts 2000) core source_claim", "(Hess 2014) core source_claim",
#'                                              "No citation given", "No claim", "Inaccessible", 
#'                                              "Opinion claim",  "Does not test claim", "Core claim", 
#'                                              "Predation not in support", "Predation in support", 
#'                                              "Population not in support without data", "Population in support without data", 
#'                                              "Population not in support with data", "Population in support with data",
#'                                              "Not relevant",
#'                                              "In support", "Not in support",
#'                                              "In support with qualities", "Not in support with qualities"#,
#'                                              
#'                                              
#'                                   ))
#' levels(terminus_freq.mlt$value)
#' terminus_freq.mlt[is.na(value)]
#' #
#' terminus_freq.mlt$variable <- factor(terminus_freq.mlt$variable,
#'                                      levels = c("cited_by_id", "evidence_type_synthetic", "in_support"))
#' 
#' unique(terminus_freq.mlt$value)
#' 
#' # terminus_freq.mlt[!value %in% c("In support", "Not in support"),
#' #                   of_quality := NA]
#' 
#' # terminus_freq.mlt[value %in% c("In support", "Not in support")]
#' 
#' # # terminus_freq.mlt[variable == "in_support", ]
#' # unique(terminus_freq.mlt[variable == "in_support", ]$type_simple)
#' 
#' unique(terminus_freq.mlt$of_quality)
#' # type_pal <- unique(terminus_freq.mlt$type_simple)
#' # type_pal
#' # Hmmm
#' 
#' ggplot(data = terminus_freq.mlt,
#'        aes(x = variable, fill = type_simple, 
#'            stratum = value,
#'            alluvium = id, label = value, #color = of_quality,
#'            y = n
#'        ))+
#'   geom_flow(stat = "alluvium", lode.guidance = "frontback",
#'             alpha = .75) +
#'   geom_stratum(alpha = 1, aes(fill = type_simple, color = of_quality
#'                               ),
#'                reverse = TRUE)+
#'   scale_color_manual(values = c("no" = "black",
#'                                 "not relevant" = "black",
#'                                 "yes" = "gold"),
#'                      na.value = "black")+
#'   scale_fill_manual(values = fill_pal)+
#'   ylab("Number of citations")+
#'   xlab(NULL)+
#'   geom_text(stat = "stratum", size = 3, color = "black") +
#'   theme_bw()+
#'   theme(panel.grid = element_blank())
#' 
#' 
#' # >>> Alt -----------------------------------------------------------------
#' # Let's just do 2 columns.
#' terminus_freq.alt <- copy(terminus_freq)
#' 
#' unique(terminus_freq.alt$evidence_type_synthetic)
#' unique(terminus_freq.alt$of_quality)
#' terminus_freq.alt[of_quality == "yes" & grepl("Population", evidence_type_synthetic),
#'                   evidence_type_synthetic := paste(evidence_type_synthetic, "and qualities")]
#' terminus_freq.mlt <- melt(terminus_freq.alt,
#'                           measure.vars = c("cited_by_id", "evidence_type_synthetic"))
#' unique(terminus_freq.mlt$value)
#' 
#' #
#' terminus_freq.mlt$value <- factor(terminus_freq.mlt$value ,
#'                                   levels = c("(Doherty et al. 2016) core source_claim", "(IUCN 2025) core source_claim", 
#'                                              "Web of Science_Systematic external review", "(Hume 2017) core source_claim", 
#'                                              "(Medina et al. 2011) core source_claim", "(Garnett & Baker 2021) core source_claim", 
#'                                              "(Wallach & Lundgren 2025)_external review without claim", "(Woinarski et al. 2014) core source_claim", 
#'                                              "(Garnett et al. 2011) core source_claim", "(Radford et al. 2018) core source_claim", 
#'                                              "(Dickman 1996b) core source_claim", "(Campolina et al. 2024) core source_claim", 
#'                                              "(Oedin et al. 2021) core source_claim", "(Welch & Leppanen 2017) core source_claim", 
#'                                              "(Alberts 2000) core source_claim", "(Hess 2014) core source_claim",
#'                                              "No citation given", "No claim", "Inaccessible", 
#'                                              "Opinion claim",  "Does not test claim", "Core claim", 
#'                                              "Predation not in support", "Predation in support", 
#'                                              "Population not in support without data", "Population in support without data", 
#'                                              "Population not in support with data", "Population in support with data",
#'                                              "Population not in support with data and qualities", "Population in support with data and qualities"
#'                                              
#'                                   ))
#' levels(terminus_freq.mlt$value)
#' terminus_freq.mlt[is.na(value)]
#' #
#' terminus_freq.mlt$variable <- factor(terminus_freq.mlt$variable,
#'                                      levels = c("cited_by_id", "evidence_type_synthetic", "in_support"))
#' 
#' unique(terminus_freq.mlt$value)
#' 
#' # terminus_freq.mlt[!value %in% c("In support", "Not in support"),
#' #                   of_quality := NA]
#' 
#' # terminus_freq.mlt[value %in% c("In support", "Not in support")]
#' 
#' # # terminus_freq.mlt[variable == "in_support", ]
#' # unique(terminus_freq.mlt[variable == "in_support", ]$type_simple)
#' 
#' unique(terminus_freq.mlt$of_quality)
#' # type_pal <- unique(terminus_freq.mlt$type_simple)
#' # type_pal
#' # Hmmm
#' 
#' alluv.p <- ggplot(data = terminus_freq.mlt,
#'        aes(x = variable, fill = type_simple, 
#'            stratum = value,
#'            alluvium = id, label = value, #color = of_quality,
#'            y = n
#'        ))+
#'   geom_flow(stat = "alluvium", lode.guidance = "frontback",
#'             alpha = .75) +
#'   geom_stratum(alpha = 1, aes(fill = type_simple, color = of_quality
#'   ),
#'   reverse = TRUE)+
#'   scale_color_manual(values = c("no" = "black",
#'                                 "not relevant" = "black",
#'                                 "yes" = "gold"),
#'                      na.value = "black")+
#'   scale_fill_manual(values = fill_pal)+
#'   ylab("Number of citations")+
#'   xlab(NULL)+
#'   geom_text(stat = "stratum", size = 3, color = "black") +
#'   theme_bw()+
#'   theme(panel.grid = element_blank(),
#'         panel.border = element_blank())
#' alluv.p
#' 

# >>> Calculate frequencies --------------------------------------
# We want support / not support as the final column, quality as an aesthetic attribute
unique(terminus_edges$evidence_type_synthetic)
terminus_edges[grepl("not in support", evidence_type_synthetic), in_support := "Not in support"]
terminus_edges[grepl("not in support", evidence_type_synthetic), 
               evidence_type_synthetic := gsub(" not in support", "", evidence_type_synthetic)]

terminus_edges[grepl("in support", evidence_type_synthetic), in_support := "In support"]
terminus_edges[grepl("in support", evidence_type_synthetic), 
               evidence_type_synthetic := gsub(" in support", "", evidence_type_synthetic)]
unique(terminus_edges$in_support)

terminus_edges[, evidence_type_synthetic := gsub(" of quality", "", evidence_type_synthetic)]
unique(terminus_edges$evidence_type_synthetic)

terminus_edges


unique(terminus_freq$evidence_type_synthetic)
terminus_freq[, type_simple := ifelse(evidence_type_synthetic %in% c("Predation without data",
                                                                     "Population without data",
                                                                     "Population with data"), 
                                      evidence_type_synthetic,
                                      "Excluded")]

# terminus_freq[is.na(in_support), in_support := "N/A"]


# Colors based on type_simple
#' [Where'd NOTHING go?????]
unique(terminus_freq$type_simple)

unique(terminus_freq$evidence_type_synthetic)

# I think just two columns now. Let's 



fill_pal <- c("Excluded" = "grey50", "Predation without data" = "grey20",
              "Population with data" = "dodgerblue2",   "Population without data" = "dodgerblue4")

unique(terminus_freq$cited_by_id)

unique(terminus_freq$of_quality)
terminus_freq$of_quality <- factor(terminus_freq$of_quality,
                                   levels = c("no", "yes"))
setorder(terminus_freq, 
         cited_by_id, evidence_type_synthetic,of_quality, in_support)

unique(terminus_freq$evidence_type_synthetic)
terminus_freq$evidence_type_synthetic <- factor(terminus_freq$evidence_type_synthetic,
                                                levels = c("No citation", "No claim", "Opinion claim", 
                                                           "Core claim", "Inaccessible", "Does not test claim",
                                                           "Predation without data", "Population without data",
                                                           "Population with data"))
terminus_freq[, cited_by_id := gsub(" core source_claim", "", cited_by_id)]
terminus_freq[, cited_by_id := gsub("_external review without claim", "", cited_by_id)]


ggplot(data = terminus_freq,
       aes(y = n, axis1 = cited_by_id, axis2 = evidence_type_synthetic, axis3 = in_support))+
  geom_alluvium(width = 1/12, aes(fill = type_simple, color = of_quality)) +
  geom_stratum(width = 1/12, fill = "white", color = "grey")+
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_color_manual(values = c("no" = "transparent",
                                "yes" = "gold"))+
  scale_fill_manual(values = fill_pal)+
  ylab("Number of citations")+
  theme_bw()+
  theme(panel.grid = element_blank())


# >>> Long format plot ----------------------------------------------------

terminus_freq[, id := seq(1:.N)]

terminus_freq.mlt <- melt(terminus_freq,
                          measure.vars = c("cited_by_id", "in_support", "evidence_type_synthetic"))
dput(levels(terminus_freq$evidence_type_synthetic))
terminus_freq.mlt <- terminus_freq.mlt[!is.na(value), ]
unique(terminus_freq.mlt$value)

dput(unique(terminus_freq$cited_by_id))
unique(terminus_freq.mlt$value)
terminus_freq.mlt$value <- factor(terminus_freq.mlt$value ,
                                  levels = c("(IUCN 2025)", "(Alberts 2000)", "(Campolina et al. 2024)", "(Dickman 1996b)", 
                                             "(Doherty et al. 2016)", "(Garnett & Baker 2021)", "(Garnett et al. 2011)", 
                                             "(Hess 2014)", "(Hume 2017)", "(Medina et al. 2011)", 
                                             "(Oedin et al. 2021)", "(Radford et al. 2018)", 
                                             "(Welch & Leppanen 2017)", "(Woinarski et al. 2014)", "(Wallach & Lundgren 2025)", 
                                             "Web of Science_Systematic external review","Opportunistic_Systematic external review",
                                             "Population with data", 
                                             "Population without data", 
                                             "Predation without data", 
                                             "No citation", "No claim", "Opinion claim", "Core claim", "Inaccessible", 
                                             "Does not test claim",
                                             "In support", "Not in support"))
unique(terminus_freq.mlt$value)
#
terminus_freq.mlt$variable <- factor(terminus_freq.mlt$variable,
                                     levels = c("cited_by_id", "evidence_type_synthetic", "in_support"))

unique(terminus_freq.mlt$value)
# terminus_freq.mlt[!value %in% c("In support", "Not in support"),
#                   of_quality := NA]

terminus_freq.mlt[value %in% c("In support", "Not in support")]


type_pal <- unique(terminus_freq.mlt$type_simple)
type_pal

# Hmmm
ggplot(data = terminus_freq.mlt,
       aes(x = variable, fill = type_simple, stratum = value,
           alluvium = id, label = value, color = of_quality,
           y = n
       ))+
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            alpha = .75) +
  geom_stratum(alpha = 1, aes(fill = type_simple))+
  scale_color_manual(values = c("no" = NA,
                                "yes" = "gold"))+
  scale_fill_manual(values = fill_pal)+
  ylab("Number of citations")+
  xlab(NULL)+
  geom_text(stat = "stratum", size = 3, color = "black") +
  theme_bw()+
  theme(panel.grid = element_blank())


# OK, i think last shot is to have of quality in there....gahh
terminus_freq.mlt[of_quality == "yes", ]

terminus_freq.mlt[of_quality == "yes", value2 := paste(value, "of quality")]
terminus_freq.mlt[of_quality == "no", value2 := value]
terminus_freq.mlt


terminus_freq.mlt$value2 <- factor(terminus_freq.mlt$value2 ,
                                   levels = c("(IUCN 2025)", "(Alberts 2000)", "(Campolina et al. 2024)", "(Dickman 1996b)", 
                                              "(Doherty et al. 2016)", "(Garnett & Baker 2021)", "(Garnett et al. 2011)", 
                                              "(Hess 2014)", "(Hume 2017)", "(Medina et al. 2011)", 
                                              "(Oedin et al. 2021)", "(Radford et al. 2018)", 
                                              "(Welch & Leppanen 2017)", "(Woinarski et al. 2014)", "(Wallach & Lundgren 2025)", 
                                              "Web of Science_Systematic external review","Opportunistic_Systematic external review",
                                              
                                              "Predation without data", "Population without data", 
                                              "Population with data", "Population with data of quality", 
                                              
                                              "No citation", "No claim", "Opinion claim", "Core claim", "Inaccessible", 
                                              "Does not test claim", 
                                              "In support", "Not in support"))

#
#
alluv <- ggplot(data = terminus_freq.mlt,
                aes(x = variable, fill = type_simple, stratum = value2,
                    alluvium = id, label = value2, color = of_quality,
                    y = n
                ))+
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            alpha = .75) +
  geom_stratum(alpha = 1, color = "black")+
  scale_color_manual(values = c("no" = NA,
                                "yes" = "gold"))+
  scale_fill_manual(values = fill_pal)+
  ylab("Number of citations")+
  xlab(NULL)+
  geom_text(stat = "stratum", size = 3, color = "black") +
  theme_bw()+
  theme(panel.grid = element_blank(), panel.border = element_blank(),
        legend.position = "bottom")
alluv 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------

# OLD ---------------------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Frequency plot ----------------------------------------------------------

# >>> By # of citations ---------------------------------------------------

citation_freq <- dat.m1[edge_type == "citation" & original_source_makes_claim == "Yes", 
                        .(n_citations = .N),
                        by = .(evidence_type, evidence_inclusion, Hypothesis_supported,
                               has_data, of_quality)]

citation_freq[, evidence_type_simple := gsub("not in support", "", evidence_type)]
citation_freq[, evidence_type_simple := gsub("in support", "", evidence_type_simple)]
citation_freq[, evidence_type_simple := trimws(evidence_type_simple)]

citation_freq[Hypothesis_supported == 0, n_citations := -n_citations]
citation_freq[is.na(Hypothesis_supported),]
# citation_freq[is.na(Hypothesis_supported), n_citations := -n_citations]

citation_freq[evidence_inclusion == "EXCLUDE", has_data := NA]
citation_freq[evidence_inclusion == "EXCLUDE", of_quality := NA]

citation_freq[evidence_inclusion == "INCLUDE", evidence_type_v2 := ifelse(has_data == "yes",
                                                                          paste(evidence_type_simple, "with data"),
                                                                          paste(evidence_type_simple, "without data"))]
citation_freq[is.na(evidence_type_v2), evidence_type_v2 := evidence_type_simple]
citation_freq

dput(node_fill)
unique(citation_freq$evidence_type_v2)
bar_fill <- c(`No claim` = "grey90", `Opinion claim` = "grey90", `No citation given` = "grey90",
              Inaccessible = "grey90", `Cites different core source` = "grey90",
              Inaccessible = "grey90", `Does not test claim` = "grey90", 
              `Predation without data` = "grey50", `Control program without data` = "indianred4", 
              `Population without data` = "dodgerblue4", `Population with data` = "dodgerblue")

unique(citation_freq$evidence_type_simple)
setdiff(citation_freq$evidence_type_simple, c("Inaccessible", "No citation given", "No claim", "Opinion claim",
                                              "Cites different core source", "Does not test claim", "Predation",
                                              "Control program", "Population"))
setdiff(c("Inaccessible", "No citation given", "No claim", "Opinion claim",
          "Cites different core source", "Does not test claim", "Predation",
          "Control program", "Population"), citation_freq$evidence_type_simple)
citation_freq$evidence_type_simple <- factor(citation_freq$evidence_type_simple,
                                             levels = c("Inaccessible", "No citation given", "No claim", "Opinion claim",
                                                        "Cites different core source", "Does not test claim", "Predation",
                                                        "Control program", "Population"))
unique(citation_freq$evidence_type_simple)

unique(citation_freq$evidence_type_v2)
citation_freq$evidence_type_v2 <- factor(citation_freq$evidence_type_v2,
                                         levels = c("Inaccessible", "No citation given", "No claim", "Opinion claim",
                                                    "Cites different core source", "Does not test claim", "Predation without data",
                                                    "Control program without data", "Population with data", 
                                                    "Population without data"))

citation_freq$of_quality <- factor(citation_freq$of_quality,
                                   levels = c("yes", "no"))

# citation_freq[evidence_inclusion == "EXCLUDE", n_citations := -n_citations]
cit_freq <- ggplot(data = citation_freq,
                   aes(x = n_citations, y = evidence_type_simple,
                       fill = evidence_type_v2, color = of_quality))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_col(linewidth = 1)+
  scale_fill_manual("Evidence type",
                    # breaks = levels(citation_freq$evidence_type_v2),
                    values = bar_fill)+
  scale_color_manual("Of quality",,
                     values = c("yes" = "gold",
                                "no" = "transparent"),
                     na.value = "transparent")+
  xlab("Number citations by core claims (not in support / in support)")+
  ylab(NULL)+
  # facet_wrap(~evidence_inclusion, ncol = 1,
  #            scales = "free_y",
  #            labeller = as_labeller(c("EXCLUDE" = "Excluded articles",
  #                                   "INCLUDE" = "Included articles")))+
  theme_bw()+
  theme(strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank())
cit_freq
# Not sure if that makes sense to do...


p1 + cit_freq + plot_layout(ncol = 2, widths = c(.75, .25))


# >>> By # of articles/species combinations ----------------------------------------------
evidence <- dat.m1[, .(scientificName, article_node_name,
                       evidence_type, evidence_inclusion, has_data, of_quality, 
                       Hypothesis_supported)] |> unique()
evidence[, article_species := paste(article_node_name, scientificName)]
evidence[evidence_type == "Does not test claim" & Hypothesis_supported == 1]

evidence_freq <- evidence[, .(n_citations = uniqueN(article_species)),
                          by = .(evidence_type, evidence_inclusion, Hypothesis_supported,
                                 has_data, of_quality)]

evidence_freq[, evidence_type_simple := gsub("not in support", "", evidence_type)]
evidence_freq[, evidence_type_simple := gsub("in support", "", evidence_type_simple)]
evidence_freq[, evidence_type_simple := trimws(evidence_type_simple)]

evidence_freq[Hypothesis_supported == 0, n_citations := -n_citations]
# evidence_freq[is.na(Hypothesis_supported), n_citations := -n_citations]
evidence_freq[evidence_inclusion == "EXCLUDE", has_data := NA]
evidence_freq[evidence_inclusion == "EXCLUDE", of_quality := NA]

evidence_freq[evidence_inclusion == "INCLUDE", evidence_type_v2 := ifelse(has_data == "yes",
                                                                          paste(evidence_type_simple, "with data"),
                                                                          paste(evidence_type_simple, "without data"))]
evidence_freq[is.na(evidence_type_v2), evidence_type_v2 := evidence_type_simple]
evidence_freq

dput(node_fill)
unique(evidence_freq$evidence_type_v2)
bar_fill <- c(`No claim` = "grey90", `Opinion claim` = "grey90", `No citation given` = "grey90",
              Inaccessible = "grey90", `Cites different core source` = "grey90",
              Inaccessible = "grey90", `Does not test claim` = "grey90", `Predation without data` = "grey50", `Control program without data` = "indianred4", 
              `Population without data` = "dodgerblue4", `Population with data` = "dodgerblue")

evidence_freq$evidence_type_simple <- factor(evidence_freq$evidence_type_simple,
                                             levels = c("Inaccessible", #"No citation given", 
                                                        "No claim", "Opinion claim",
                                                        "Cites different core source", "Does not test claim", "Predation",
                                                        "Control program", "Population"))

evidence_freq$of_quality <- factor(evidence_freq$of_quality,
                                   levels = c("yes", "no"))

ggplot(data = evidence_freq,
       aes(x = n_citations, y = evidence_type_simple,
           fill = evidence_type_v2, color = of_quality))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_col(linewidth = 1)+
  scale_fill_manual(values = bar_fill)+
  scale_color_manual(values = c("yes" = "gold",
                                "no" = "transparent"),
                     na.value = "transparent")+
  xlab("Number of studies (not in support / in support)")+
  facet_wrap(~evidence_inclusion, ncol = 1,
             scales = "free_y",
             labeller = as_labeller(c("EXCLUDE" = "Excluded articles",
                                      "INCLUDE" = "Included articles")))+
  theme_bw()

# Not sure if that makes sense to do...





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------

# >>> Alternative colors --------------------------------------------------

# Let's do a continuous color palette...
nodes

# This is probably too complicated. Fuck...
# nodes[, type_quant := fcase(evidence_type_simple == "Excluded", NA,
#                             )]
library("scico")
scico(n = 8, palette = "roma")
# "#7E1700" "#A05F1B" "#BD9C3D" "#D1DE98" "#9BE2D4" "#45ABCB" "#2471B4" "#023198"

nodes[, evidence_type_simple_v2 := evidence_type_simple]
unique(nodes$in_support)
nodes[in_support != "Not relevant", evidence_type_simple_v2 := ifelse(in_support == "Yes",
                                                                      paste(evidence_type_simple, "in support"),
                                                                      paste(evidence_type_simple, "not in support"))]
nodes[, evidence_type_simple_v2 := gsub("Predation without data", "Predation", evidence_type_simple_v2)]
nodes
dput(as.character(unique(nodes$evidence_type_simple_v2)))
unique(nodes$evidence_type_simple_v2)
# Going to be funny...
nodes$evidence_type_simple_v2 <- factor(nodes$evidence_type_simple_v2,
                                        levels = c("Core claim", "External review without claim", 
                                                   "Population with data not in support", 
                                                   "Population without data not in support", 
                                                   "Control program without data not in support",
                                                   "Predation not in support", 
                                                   "Excluded", 
                                                   "Predation in support", 
                                                   "Control program without data in support", 
                                                   "Population without data in support", 
                                                   "Population with data in support"
                                        ))
node_fill <- scico(n = 8, palette = "roma")
names(node_fill) <- c("Population with data not in support", 
                      "Population without data not in support", 
                      "Control program without data not in support",
                      "Predation not in support", 
                      "Predation in support", 
                      "Control program without data in support", 
                      "Population without data in support", 
                      "Population with data in support")
node_fill <- c(node_fill,
               "Core claim" = "hotpink", "External review without claim" = "pink", 
               "Excluded" = "grey80")


# template <- rep("XXXX", length(unique(nodes$evidence_type_simple_v2)))
# names(template) <- (unique(nodes$evidence_type_simple_v2))
# dput(template)
# node_fill <- c(Excluded = "XXXX", `Core claim` = "XXXX", `Predation without data in support` = "XXXX", 
#                `Population with data not in support` = "XXXX", `Predation without data not in support` = "XXXX", 
#                `Population without data in support` = "XXXX", `Population with data in support` = "XXXX", 
#                `Population without data not in support` = "XXXX", `External review without claim` = "XXXX", 
#                `Control program without data in support` = "XXXX", `Control program without data not in support` = "XXXX"
# )
# 
# node_fill <- c(`Core claim` = "hotpink", `External review without claim` = "pink",
#                Excluded = "grey90", 
#                `Predation without data` = "grey50", `Control program without data` = "indianred4",
#                `Population without data` = "dodgerblue4", `Population with data` = "dodgerblue"
# )
template <- rep(3, length(unique(nodes$evidence_type_simple_v2)))
names(template) <- (unique(nodes$evidence_type_simple_v2))
dput(template)
node_size <- c(Excluded = 1, `Core claim` = 5, `Predation in support` = 3, 
               `Population with data not in support` = 3, `Predation not in support` = 3, 
               `Population without data in support` = 3, `Population with data in support` = 3, 
               `Population without data not in support` = 3, `External review without claim` = 5, 
               `Control program without data in support` = 3, `Control program without data not in support` = 3)


unique(nodes$of_quality)
stroke_color <- c("no" = "black", 
                  "yes" = "gold")

stroke_width <- c("no" = .1, 
                  "yes" = 2)

unique(nodes$in_support)
# support_shape <- c("Not relevant" = 21,
#                    "Yes" = 24,
#                    "No" = 25)

# Create network
graph <- igraph::graph_from_data_frame(edges, 
                                       vertices = nodes,
                                       directed = T)
graph


graph <- tidygraph::as_tbl_graph(graph)
#

unique(edges$edge_type)
#
unique(nodes$evidence_type_simple_v2)
#

# >>> Plot ----------------------------------------------------------------
# labels <- c(`Core claim` = "Core claim", `External review without claim` = "External review without claim", 
#             Excluded = "Excluded", `Predation without data` = "Predation", `Control program without data` = "Control program without data", 
#             `Population without data` = "Population without data", `Population with data` = "Population with data"
# )


#
p1 <- ggraph(graph, layout = "kk")+
  geom_edge_fan(arrow = arrow(length = unit(1, 'mm')), 
                start_cap = circle(1, 'mm'),
                end_cap = circle(1, 'mm'),
                color = "grey50",
                alpha = .5,
                linewidth = 0.25) + 
  # geom_edge_loop()+ #aes(color = edge_type)
  geom_node_point(aes(fill = evidence_type_simple_v2,
                      color = of_quality,
                      stroke = of_quality,
                      size = evidence_type_simple_v2), 
                  shape = 21, #stroke = 2,
                  alpha = .75)+
  scale_fill_manual("Article type",
                    values = node_fill,
                    breaks = levels(nodes$evidence_type_simple_v2))+
  scale_size_manual("Article type",
                    values = node_size *2,
                    breaks = levels(nodes$evidence_type_simple_v2))+ 
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
  theme_graph()#+
p1





# p1 + alluv

# >>> Alternative long format ----------------------------------------------------
# This is tricky....
# terminus_freq[, id := .GRP, by = .(cited_by_id, evidence_type_synthetic)]
# 
# terminus_freq.mlt <- melt(terminus_freq[, .(cited_by_id, evidence_type_synthetic, n, type_simple,
#                                             id)],
#                           measure.vars = c("cited_by_id", "evidence_type_synthetic"))
# terminus_freq.mlt
# 
# # Now a separate ID just linking population/predation studies with support...
# terminus.2 <- terminus_freq[!is.na(in_support), .(evidence_type_synthetic,
#                                                   in_support, type_simple, of_quality, 
#                                                   n, id)]
# terminus.2
# terminus.2 <- terminus.2[, .(n = sum(n),
#                              id = min(id)),
#                          by = .(evidence_type_synthetic, in_support, type_simple, of_quality)]
# # .(n = sum(n)),
# # terminus.2[, id := paste0("3rd_axis", seq(1:.N))]
# terminus.2.mlt <- melt(terminus.2,
#                         measure.vars = c("evidence_type_synthetic", "in_support"))
# 
# 
# terminus_bind <- rbind(terminus_freq.mlt, terminus.2.mlt, fill = TRUE)
# terminus_bind
# 
# dput(unique(terminus_freq$cited_by_id))
# 
# terminus_bind$value <- factor(terminus_bind$value ,
#                                   levels = c("(IUCN 2025)", "(Alberts 2000)", "(Campolina et al. 2024)", "(Dickman 1996b)", 
#                                              "(Doherty et al. 2016)", "(Garnett & Baker 2021)", "(Garnett et al. 2011)", 
#                                              "(Hess 2014)", "(Hume 2017)", "(Medina et al. 2011)", 
#                                              "(Oedin et al. 2021)", "(Radford et al. 2018)", 
#                                              "(Welch & Leppanen 2017)", "(Woinarski et al. 2014)", "(Wallach & Lundgren 2025)", 
#                                              "Systematic (WoS + Opportunistic)",
#                                              
#                                              "No citation", "No claim", "Opinion claim", "Core claim", "Inaccessible", 
#                                              "Does not test claim", "Predation without data", "Population without data", 
#                                              "Population with data", "In support", "Not in support"))
# unique(terminus_bind$variable)
# #
# terminus_bind$variable <- factor(terminus_bind$variable,
#                                      levels = c("cited_by_id", "evidence_type_synthetic", "in_support"))
# 
# unique(terminus_bind$value)
# # terminus_bind[!value %in% c("In support", "Not in support"),
# #                   of_quality := NA]
# 
# terminus_bind[value %in% c("In support", "Not in support")]
# 
# # is_lodes_form(terminus_bind)
# # Hmmm
# ggplot(data = terminus_bind,
#        aes(x = variable, stratum = value, alluvium = id, 
#            fill = type_simple,  y = n,
#            label = value, color = of_quality,
#        ))+
#   geom_flow(stat = "alluvium", lode.guidance = "frontback",
#             alpha = .75) +
#   geom_stratum(alpha = 1, color = "grey70")+
#   scale_color_manual(values = c("no" = "transparent",
#                                 "yes" = "gold"))+
#   scale_fill_manual(values = fill_pal)+
#   geom_text(stat = "stratum", size = 3, color = "black") +
#   theme_bw()
# 

