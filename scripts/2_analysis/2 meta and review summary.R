# Conduct meta-analysis and tabulate systematic review figures
#
#
#
#

rm(list = ls())

library("data.table")
library("metafor")
library("ggplot2")
library("orchaRd")
library("patchwork")
library("readxl")
library("stringr")
library("ggstance")
library("sf")
library("mapview")
library("tidyr")
library("broom")
library("broom.mixed")
library("dplyr")
library("rotl")
library("ape")
library("gt")
library("ggtext")

rma_predictions <- function(m, 
                            newgrid,
                            has_intercept = T){
  
  
  if(!is.data.frame(newgrid)){errorCondition("ERROR newgrid must be a data frame")}
  #create the new model matrix. 
  
  if(!all(unlist(lapply(names(newgrid), # lapply through names of newgrid to check that they're in formula
                        function(x) grepl(pattern=x,
                                          x = as.character(m$formula.mods)[-1]))))){
    errorCondition("ERROR: variables in newgrid are not in model formula")
  }
  
  
  # Drop levels that might be missing from the model...
  cols <- names(newgrid)
  coef_nms <- names(coef(m))
  temp <- c()
  
  if(has_intercept == F){
    
    for(i in 1:length(cols)){
      if(class(unlist(newgrid[, cols[i], with = F])) %in% c("factor", "character")){
        temp <- paste0(names(newgrid[, cols[i], with = F]),
                       unlist(newgrid[, cols[i], with = F]))
        newgrid <- newgrid[temp %in% coef_nms, ]
      }
    }
    
  }
  
  newgrid
  
  # Create prediction matrix
  predgrid <- (model.matrix(m$formula.mods, data=newgrid))
  predgrid
  
  if(any(grepl("intercept", colnames(predgrid), 
               ignore.case = TRUE))){
    #if intercept is present, remove it?
    predgrid <- predgrid[, -1]
  }
  
  # predict onto the new model matrix
  pred.out <- as.data.frame(predict(m, newmods=predgrid))
  
  #attach predictions to variables for plotting
  final.pred <- cbind(newgrid, pred.out)
  
  return(final.pred)
}

# Load meta-analysis data ---------------------------------------------------------------

dat <- fread("builds/meta_analysis/analysis_ready_dataset.csv")
dat <- unique(dat)

dat[, log_mass := log10(Mass_g_final)]

dat[, .(n = .N), by = .(Order_final)]
# Not enough data.

dat[, .(n = .N), by = .(class)]
# dat[class == "Reptiles", class := NA]

unique(dat$log_mass)

dat[, article_id := paste(word(Article, 1, sep = "[[:space:][:punct:]]"), .GRP), by = .(Article)]
unique(dat[, .(Article, article_id)])
length(unique(dat$Article)) == length(unique(dat$article_id))

nrow(dat[duplicated(article_id)])
nrow(dat[duplicated(Article)])
# Must be equal. Good

unique(dat[, .(analysis_group, analysis_effect_size)])

dat.long <- copy(dat)#melt(dat, measure.vars = c("class", "log_mass", "continent_island"),
#   value.name = "predictor")

# dat.long[analysis_group == "Before-after eradication abundance", .(analysis_effect_size)]

unique(dat.long[, .(analysis_effect_size, analysis_group)])
unique(dat.long$Effect_size_ID)
dat.long[Effect_size_ID %in% c("ES_47a", "ES_47b")]
#
dat.long[Effect_size_ID %in% c("ES_38a")]$analysis_group
dat.long

#
dat.long[Effect_size_ID == "ES_43"]$analysis_group
dat.long[Effect_size_ID == "ES_37"]$analysis_group

dat.long[, .(n = .N), by = .(Effect_size_ID, analysis_group)]
dat.long[Effect_size_ID == "ES_13", ]

dat.long[, .(n = .N), by = .(Effect_size_ID, analysis_group)][n > 1] # must be 0 rows

# >>> Add column for whether effect size type is dominant within analysis group --------
dat.long[, total_n_articles := uniqueN(article_id), by = .(analysis_group)]
dat.long[, n_articles_per_original_es := uniqueN(article_id), by = .(analysis_group,
                                                                     original_effect_size)]
dat.long[, dominant_effect_size := ifelse((n_articles_per_original_es / total_n_articles) > .5,
                                          "yes", "no")]
dat.long[dominant_effect_size == "no", ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -----------------------------------------
# Set up model guide -----------------------------------------------------------------

dat.long[prey_range_primary == "outside"]$scientificName
dat.long[prey_range_secondary == "outside"]$scientificName
dat.long[prey_range_tertiary == "outside"]$scientificName
# Same ones. Not enough data to fit a model. Going to use this as a subset analysis instead.

# Also, we should do a sensitivity analysis
x <- dat.long[, .(n = uniqueN(article_id)),
    by = .(analysis_group, original_effect_size)]
# 
setorder(x, analysis_group, n)
x

unique(dat.long$original_effect_size)
unique(dat.long$analysis_group)

#
unique(dat$analysis_group)
unique(dat$class)

names(dat.long)

# Do the guide in 2 steps. We don't have enough data for class * habitat sub-analyses.
guide <- CJ(analysis_group = unique(dat.long$analysis_group),
            moderator = c("1"),
            class = c("Birds", "Mammals", "Reptiles", "All"),
            prey_range = c("All", "inside"),
            non_phylo_species = c("yes", "no"),
            phylo_species = c("yes", "no"),
            only_dominant_effect_size = c("yes", "no")
)

guide2 <- CJ(analysis_group = unique(dat.long$analysis_group),
              moderator = c("log_mass", 
                            "locomotion_volant",
                           "ground_or_burrow_resting_or_nesting",
                           "Foraging_habitat_ground", "continent_island"),
             class = c("All"),
             prey_range = c("All"),
             non_phylo_species = c("yes", "no"),
             phylo_species = c("yes", "no"),
             only_dominant_effect_size = c("no")
             )
guide <- rbind(guide, guide2)

guide[, model_comparison_id := paste0("model_comp_", .GRP), 
      by = .(analysis_group, class, prey_range,
             moderator, only_dominant_effect_size)]
guide
guide[, random_effect := "list(~1|article_id/Effect_size_ID"]

guide[, random_effect := ifelse(non_phylo_species == "yes",
                                paste0(random_effect, ", ~1|scientificName"), 
                                paste0(random_effect))]
guide[, random_effect := ifelse(phylo_species == "yes",
                                paste0(random_effect, ", ~1|phylo_species)"),
                                paste0(random_effect, ")"))]
guide


# Formulate exclusion:
guide[, exclusion := paste0("analysis_group == '", analysis_group, "'")]
# guide[moderator != "1", ]
guide[moderator != "1", exclusion := paste0(exclusion, " & !is.na(", moderator, ")")]
guide[class != "All", exclusion := paste0(exclusion, " & class %in% '", class, "'")]
guide[class != "All",]
guide[prey_range == "inside", exclusion := paste0(exclusion, " & prey_range_primary == 'inside'")]
guide[prey_range == "All"]

#
guide <- merge(guide,
               unique(dat.long[, .(analysis_group, analysis_effect_size)]),
               by = 'analysis_group',
               all.x = T)
guide[is.na(analysis_effect_size)]
guide

#
guide[only_dominant_effect_size == "yes", exclusion := paste0(exclusion, " & dominant_effect_size == 'yes'")]

#
guide[, formula := paste("~", moderator)]

guide
guide

unique(guide$formula)


# >>> Add model ID --------------------------------------------------------

guide[, model_id := paste0("model_", seq(1:.N))]

dat.long$class

guide[class == "All"]$exclusion
guide[prey_range == "inside"]$exclusion
guide <- guide[!(prey_range == "inside" & only_dominant_effect_size == "yes"), ]


guide[analysis_group == "Abundance with/without cats" &
        moderator == "1" &
        only_dominant_effect_size == "no" &
        prey_range == "All" &
        phylo_species == "no" &
        non_phylo_species == "no" &
        class == "All"]
# This used to fit successfully but now it doesn't.
# model_1321
# model_comp_331

# >>> Get overall sample sizes ----------------------------------------------------
Ns <- list()
sub.dat <- c()
i <- 49

for(i in 1:nrow(guide)){
  sub.dat <- dat.long[eval(parse(text = guide[i, ]$exclusion))]
  
  Ns[[i]] <- sub.dat[, .(n_species = uniqueN(scientificName),
                    n_articles = uniqueN(Article),
                    n_obs = .N,
                    model_id = guide[i, ]$model_id,
                    analysis_group = guide[i, ]$analysis_group,
                    class = guide[i, ]$class)]
  Ns[[i]]$model_id <- guide[i, ]$model_id
  
}
Ns <- rbindlist(Ns)
Ns

guide <- merge(guide, Ns[, .(n_species, n_articles, n_obs, model_id)], by = "model_id")
guide[n_articles == 0, ]

# at least 3 articles for intercept only models
guide <- guide[n_articles > 0, ]
guide # Keep all analysis groups for which there are data for intercept-only models

#
# More than 2 observations:
guide <- guide[n_obs > 2, ]
guide

# and at least 5 for continuous
guide <- guide[!(moderator == "log_mass" & n_articles < 5), ]
guide

# If only 1 article, drop Article from random effects
guide[n_articles == 1, random_effect := gsub("article_id/", "", random_effect)]
guide

guide[analysis_group == "Abundance with/without cats" &
        moderator == "1" &
        only_dominant_effect_size == "no" &
        prey_range == "All" &
        phylo_species == "no" &
        non_phylo_species == "no" &
        class == "All"]
# This used to fit successfully but now it doesn't.
# model_1321
# model_comp_331

# >>> Download phylogeny ---------------------------------------------
dat.long[, spp_name_corrected := scientificName]
dat.long[scientificName == "Pampusana erythroptera",
         spp_name_corrected := "Gallicolumba erythroptera"]
nms <- unique(dat.long$spp_name_corrected)

(nms_res <- tnrs_match_names(nms))
nms_res[]
tnrs_match_names("Prosobonia cancellata")
#
#
tree <- tol_induced_subtree(ott_ids = nms_res$ott_id)
tree
plot(tree)
sort(tree$tip.label)
tree$tip.label[grepl("Prosobonia", tree$tip.label)]

# Need to match phylo_Species with those names...

setDT(nms_res)
nms_res[, label := paste0(gsub(" ", "_", unique_name),
                          "_ott", ott_id)]
nms_res

sort(tree$tip.label)
setdiff(nms_res$label, tree$tip.label)
setdiff(tree$tip.label, nms_res$label)

nms_res[, search_string := str_to_sentence(search_string)]
unique(nms_res$search_string)
setdiff(nms_res$search_string, dat.long$spp_name_corrected)
setdiff(dat.long$spp_name_corrected, nms_res$search_string)

dat.long.m <- merge(dat.long,
                 nms_res[, .(search_string, label)],
                 by.x = "spp_name_corrected",
                 by.y = "search_string",
                 all.x = T)
dat.long.m
setnames(dat.long.m, "label", "phylo_species")

createPhyloCorr <- function(spp_list, tree){
  
  tree.filt <- keep.tip(tree, spp_list)
  tree.br <- compute.brlen(tree.filt)
  tree.corr <- vcv(tree.br, corr=T)
  return(tree.corr)
}

# createPhyloCorr(spp_list = dat.long.m$label,
#                 tree)

dat.long <- copy(dat.long.m)
dat.long[is.na(class)]

dat.long

guide[analysis_group == "Abundance with/without cats" &
        moderator == "1" &
        only_dominant_effect_size == "no" &
        prey_range == "All" &
        phylo_species == "no" &
        non_phylo_species == "no" &
        class == "All"]
# This used to fit successfully but now it doesn't.
# model_1321
# model_comp_241
nrow(dat.long[eval(parse(text = "analysis_group == 'Abundance with/without cats'"))])


# >>> Add categorical models without intercept ----------------------------
#' [For the overall difference from 0 comparisons]
guide[, model_type := "primary_models"]
guide2 <- guide[moderator %in% c("Foraging_habitat_ground",
                                 "ground_or_burrow_resting_or_nesting",
                                 "locomotion_volant", "continent_island")]

guide2[, model_type := "intercept_removed"]
guide2[, formula := paste(formula, "- 1")]
guide2

guide <- rbind(guide, guide2)

# >>> Set up prediction grids ---------------------------------------------

unique(guide$moderator)
grids <- list()
grids[["log_mass"]] <- data.table(log_mass = seq(from = min(dat.long$log_mass, na.rm = T),
                                                 to = max(dat.long$log_mass, na.rm = T),
                                                 by = .1))
grids[["Foraging_habitat_ground"]] <- data.table(Foraging_habitat_ground = unique(dat.long$Foraging_habitat_ground))
grids[["ground_or_burrow_resting_or_nesting"]] <- data.table(ground_or_burrow_resting_or_nesting = unique(dat.long$ground_or_burrow_resting_or_nesting))
grids[["locomotion_volant"]] <- data.table(locomotion_volant = unique(dat.long$locomotion_volant))
grids[["continent_island"]] <- data.table(continent_island = unique(dat.long$continent_island))

grids

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Run models --------------------------------------------------------------

models <- list()
predictions <- list()
tidy_models <- list()
i <- 149
which(guide$moderator == "locomotion_volant" & guide$n_articles > 5)

for(i in 1:nrow(guide)){
  
  sub_guide <- guide[i, ]
  sub_guide
  dat.sub <- dat.long[eval(parse(text = sub_guide$exclusion))]
  dat.sub
  #
  #
  models[[i]] <- try({
      if(sub_guide$phylo_species == "yes"){
        
         rma.mv(yi_analysis, 
                V = vi_analysis,
                mods = as.formula(sub_guide$formula),
                random = eval(parse(text = sub_guide$random_effect)),
                dfs = "contain",
                test = "t",
                method = "REML",
                R =  list(phylo_species = createPhyloCorr(dat.sub$phylo_species, tree)),
                data = dat.sub) 
        
      } else{
        rma.mv(yi_analysis, 
                              V = vi_analysis,
                              mods = as.formula(sub_guide$formula),
                              random = eval(parse(text = sub_guide$random_effect)),
                              dfs = "contain",
                              test = "t",
                              method = "REML",
                              data = dat.sub) 
      }
  })

  if(inherits(models[[i]], "try-error")){
    models[[i]] <- as.character(models[[i]])
  }else{
    
    if(sub_guide$moderator == "1"){
      predictions[[i]] <- predict(models[[i]]) |>
        as.data.frame() |>
        bind_cols(sub_guide) |>
        rename(lower_ci = ci.lb,
               upper_ci = ci.ub,
               lower_pi = pi.lb,
               upper_pi = pi.ub) |>
        setDT()
    }else{
      predictions[[i]] <- rma_predictions(models[[i]], grids[[sub_guide$moderator]]) |>
        rename("term" = sub_guide$moderator) |>
        bind_cols(sub_guide) |>
        rename(lower_ci = ci.lb,
               upper_ci = ci.ub,
               lower_pi = pi.lb,
               upper_pi = pi.ub) |>
        setDT()
    }
    
    tidy_models[[i]] <- tidy(models[[i]]) |>
      mutate(lower_ci = models[[i]]$ci.lb, 
             upper_ci = models[[i]]$ci.ub,
             df = unique(models[[i]]$ddf)) |>
      mutate(overfit = ifelse(any(models[[i]]$sigma2 == 0), "yes", "no"),
             aic = AIC(models[[i]])) |>
      bind_cols(as.data.frame(i2_ml(models[[i]])) |> t() ) |>
      bind_cols(sub_guide) |>
      setDT()
    
    cat(i, "/", nrow(guide), "\r")
    
    names(predictions)[i] <- sub_guide$model_id
    names(tidy_models)[i] <- sub_guide$model_id
    # names(models[i]) <- tidy_models[i]$model_id
    
  } 
}

# The 'V' is not a square numeric matrix is for single species phylogenetic models.
# Many won't fit because they only have 1 level of the moderator. 

names(models) <- guide$model_id

setdiff(names(tidy_models[[2]]), names(tidy_models[[1]]))
tidy_models <- rbindlist(tidy_models, fill = TRUE)
tidy_models[overfit == "yes", ]
tidy_models <- tidy_models[overfit != "yes", ]

length(predictions)
predictions <- rbindlist(predictions, fill = TRUE)
predictions
names(models)

# >>> Select best model ---------------------------------------------------
tidy_models[, min_aic := min(aic),
            by = .(model_comparison_id)]

tidy_models <- tidy_models[min_aic == aic, ]
tidy_models[overfit == "yes", ]

predictions <- predictions[model_id %in% tidy_models$model_id]
predictions

length(unique(predictions$model_id))
length(unique(tidy_models$model_id))

predictions

predictions[, string := paste0(analysis_effect_size, ": ", n_articles, "(", n_species, ", ", n_obs, ")")]
predictions

dat.long[, analysis_group_lab := gsub("Abundance ", "", analysis_group)]
dat.long[, analysis_group_lab := gsub("Reproduction ", "", analysis_group_lab)]
dat.long[, analysis_group_lab := str_to_sentence(analysis_group_lab)]


predictions[, analysis_group_lab := gsub("Abundance ", "", analysis_group)]
predictions[, analysis_group_lab := gsub("Reproduction ", "", analysis_group_lab)]
predictions[, analysis_group_lab := str_to_sentence(analysis_group_lab)]
predictions

dput(sort(unique(dat.long$analysis_group_lab)))
lvls <- c("With/without cats", 
          "Before-after eradication", "Inside/outside exclosure", "On islands with/without cats", 
          "Spatial association", "Temporal association")
dat.long$analysis_group_lab <- factor(dat.long$analysis_group_lab,
                                      levels = rev(lvls))
predictions$analysis_group_lab <- factor(predictions$analysis_group_lab,
                                        levels = rev(lvls))


# >>> Get sample sizes for moderator models -------------------------------
#' [Just do this here instead of messing around in SI tables...]
tidy_models
unique(tidy_models$moderator)

models <- unique(predictions[moderator %in% c("Foraging_habitat_ground",
                                               "continent_island", 
                                               "ground_or_burrow_resting_or_nesting", 
                                               "locomotion_volant")]$model_id)
models

out <- list()
i <- 1

for(i in 1:length(models)){
  dat.sub <- dat.long[eval(parse(text = unique(predictions[model_id == models[i], ]$exclusion)))]
  dat.sub <- melt(dat.sub,
                  measure.vars = c("Foraging_habitat_ground",
                                   "continent_island",
                                   "ground_or_burrow_resting_or_nesting",
                                   "locomotion_volant"),
                  variable.name = "moderator",
                  value.name = "term")
  Ns <- dat.sub[moderator %in% predictions[model_id == models[i], ]$moderator, 
                .(n_fac_obs = uniqueN(Effect_size_ID),
                    n_fac_articles = uniqueN(article_id),
                    n_fac_species = uniqueN(scientificName),
                    model_id = models[i]),
                by = .(moderator, term)]
  Ns
  out[[i]] <- Ns

  
}
fac_Ns <- rbindlist(out)
fac_Ns

fac_Ns[, key := paste(model_id, moderator, term)]
fac_Ns

predictions[, key := paste(model_id, moderator, term)]
setdiff(fac_Ns$key, predictions$key)

predictions <- merge(predictions,
                     fac_Ns[, .(key, n_fac_obs, n_fac_articles, n_fac_species)],
                     by = "key",
                     all.x = T)
predictions

unique(tidy_models$moderator)

# Need to change this based on intercept only vs non-intercept only...
tidy_models[moderator == "Foraging_habitat_ground"]$term
fac_Ns[moderator == "Foraging_habitat_ground"]$term

tidy_models[moderator == "continent_island"]$term
fac_Ns[moderator == "continent_island"]$term

tidy_models[moderator == "ground_or_burrow_resting_or_nesting"]$term
fac_Ns[moderator == "ground_or_burrow_resting_or_nesting"]$term

tidy_models[moderator == "locomotion_volant"]$term
fac_Ns[moderator == "locomotion_volant"]$term

#
tidy_models[moderator == "Foraging_habitat_ground",
            term_name := fcase(term == "Foraging_habitat_groundother", "other", 
                           term == "intercept", "ground",
                           term == "Foraging_habitat_groundground", "ground")]

tidy_models[moderator == "continent_island",
            term_name := fcase(term == "continent_islandmainland", "mainland",
                           term == "intercept", "mainland",
                           term == "continent_islandisland", "island")]

tidy_models[moderator == "ground_or_burrow_resting_or_nesting",
            term_name := fcase(term == "ground_or_burrow_resting_or_nestingground", "ground",
                           term == "intercept", "ground",
                           term == "ground_or_burrow_resting_or_nestingother", "other")]


tidy_models[moderator == "locomotion_volant",
            term_name := fcase(term == "locomotion_volantvolant", "volant",
                           term == "intercept", "non_volant",
                           term == "locomotion_volantnon_volant", "non_volant")]


fac_Ns

fac_Ns[, key := paste(model_id, moderator, term)]
fac_Ns

tidy_models[, key := paste(model_id, moderator, term_name)]
# setdiff(tidy_models[!moderator %in% c("1")]$key, fac_Ns$key)
fac_Ns$key %in% tidy_models$key

#
tidy_models <- merge(tidy_models,
                     fac_Ns[, .(key, n_fac_obs, n_fac_articles, n_fac_species)],
                     by = "key",
                     all.x = T)
tidy_models

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Plot -----------------------------------------------------------------
tidy_models[moderator == "1" & only_dominant_effect_size == "no", ]

# >>> Main text figures----------------------
predictions[analysis_group == "Abundance with/without cats" &
              moderator == "1" &
              class == "All"]

intercepts <- predictions[moderator == "1" & 
                            only_dominant_effect_size == "no" &
                            class == "All" &
                            prey_range == "All"]
intercepts
unique(intercepts$analysis_group)

dat.plot <- dat.long[analysis_group %in% unique(intercepts$analysis_group), ] # Drop analysis groups for which models didn't converge.
intercepts

intercepts$analysis_effect_size

p.abund <- ggplot()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = intercepts[grepl("abundance", analysis_group, ignore.case = T)],
            aes(y = analysis_group_lab,
                x = -3, vjust = 2,
               label = string),
            color = "grey50",
            size = 3)+
  geom_jitter(data = dat.plot[grepl("abundance", analysis_group, ignore.case = T), ], 
             aes(x = yi, y = analysis_group_lab, 
                 size = 1/vi_analysis, fill = yi_analysis),
             shape = 21, #fill = "grey90",
             height = 0.25, width = 0,
             alpha = .5)+
  scale_fill_gradient2(low = "dodgerblue", high = "indianred",
                       midpoint = 0, mid = "white")+
  guides(size = "none", fill = "none")+
  geom_errorbar(data = intercepts[grepl("abundance", analysis_group, ignore.case = T)], 
                aes(y = analysis_group_lab, 
                    xmin = lower_ci, xmax = upper_ci),
                width = .25)+
  geom_pointrange(data = intercepts[grepl("abundance", analysis_group, ignore.case = T)], 
                  aes(x = pred, y = analysis_group_lab,
                      xmin = lower_pi, xmax = upper_pi),
                  shape = 21, fill = "grey50",
                  size = 1)+
  ggtitle("Abundance")+
  scale_y_discrete(breaks = c("With/without cats", 
                              "Before-after eradication", "Inside/outside exclosure", "On islands with/without cats", 
                              "Spatial association", "Temporal association"),
                   labels = c("With-without cats",
                              "Before-after eradication", "Inside-outside exclosure", 
                              "Islands with-without cats", 
                              "Spatial association", "Temporal association"))+
  ylab(NULL)+
  xlab("Association between cats and threatened species")+
  # coord_cartesian(ylim = c(-4, 4))+
  # xlab(NULL)+
  # ylab("Short term abundance correlation (Zr)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
p.abund


p.reprod <- ggplot()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = intercepts[grepl("reproduction", analysis_group, ignore.case = T)],
            aes(y = analysis_group_lab,
                x = -3, vjust = 2,
                label = string),
            color = "grey50",
            size = 3)+
  geom_jitter(data = dat.plot[grepl("reproduction", analysis_group, ignore.case = T), ], 
              aes(x = yi, y = analysis_group_lab, 
                  size = 1/vi_analysis, fill = yi_analysis),
              shape = 21, #fill = "grey90",
              height = 0.25, width = 0,
              alpha = .5)+
  scale_fill_gradient2(low = "dodgerblue", high = "indianred",
                       midpoint = 0, mid = "white")+
  guides(size = "none", fill = "none")+
  geom_errorbar(data = intercepts[grepl("reproduction", analysis_group, ignore.case = T)], 
                aes(y = analysis_group_lab, 
                    xmin = lower_ci, xmax = upper_ci),
                width = .25)+
  geom_pointrange(data = intercepts[grepl("reproduction", analysis_group, ignore.case = T)], 
                  aes(x = pred, y = analysis_group_lab,
                      xmin = lower_pi, xmax = upper_pi),
                  shape = 21, fill = "grey50",
                  size = 1)+
  ggtitle("Bird reproduction")+
  ylab(NULL)+
  xlab("Association between cats and threatened bird reproduction")+
  scale_y_discrete(breaks = c("With/without cats", 
                              "Before-after eradication", "Inside/outside exclosure", "On islands with/without cats", 
                              "Spatial association", "Temporal association"),
                   labels = c("With-without cats",
                              "Before-after eradication", "Inside-outside exclosure", 
                              "Islands with-without cats", 
                              "Spatial association", "Temporal association"))+
  # coord_cartesian(ylim = c(-4, 4))+
  # xlab(NULL)+
  # ylab("Short term abundance correlation (Zr)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
p.reprod


# > Supplementary figures: ------------------------------------------------
# 3 observations per factor level will be our threshold:

predictions[is.na(n_fac_obs), ]
predictions[, min_obs := min(n_fac_obs), by = model_id]

# >>> Inside prey range ---------------------------------------------------

dat.prey <- dat.plot[prey_range_primary == "inside"]
intercept_inside <- predictions[moderator == "1" & 
                                  only_dominant_effect_size == "no" &
                                   class == "All" &
                                  prey_range == "inside", ]

intercept_inside$analysis_effect_size

p.abund.inside <- ggplot()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = intercept_inside[grepl("abundance", analysis_group, ignore.case = T)],
            aes(y = analysis_group_lab,
                x = -3, vjust = 2,
                label = string),
            color = "grey50",
            size = 3)+
  geom_jitter(data = dat.prey[grepl("abundance", analysis_group, ignore.case = T), ], 
              aes(x = yi, y = analysis_group_lab, 
                  size = 1/vi_analysis, fill = yi_analysis),
              shape = 21, #fill = "grey90",
              height = 0.25, width = 0,
              alpha = .5)+
  scale_fill_gradient2(low = "dodgerblue", high = "indianred",
                       midpoint = 0, mid = "white")+
  guides(size = "none", fill = "none")+
  geom_errorbar(data = intercept_inside[grepl("abundance", analysis_group, ignore.case = T)], 
                aes(y = analysis_group_lab, 
                    xmin = lower_ci, xmax = upper_ci),
                width = .25)+
  geom_pointrange(data = intercept_inside[grepl("abundance", analysis_group, ignore.case = T)], 
                  aes(x = pred, y = analysis_group_lab,
                      xmin = lower_pi, xmax = upper_pi),
                  shape = 21, fill = "grey50",
                  size = 1)+
  ggtitle("Abundance")+
  scale_y_discrete(breaks = c("With/without cats", 
                              "Before-after eradication", "Inside/outside exclosure", "On islands with/without cats", 
                              "Spatial association", "Temporal association"),
                   labels = c("With-without cats",
                              "Before-after eradication", "Inside-outside exclosure", 
                              "Islands with-without cats", 
                              "Spatial association", "Temporal association"))+
  ylab(NULL)+
  xlab("Association between cats and threatened species")+
  coord_cartesian(xlim = c(-8, 8))+
  # xlab(NULL)+
  # ylab("Short term abundance correlation (Zr)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
p.abund.inside


p.reprod.inside <- ggplot()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = intercept_inside[grepl("reproduction", analysis_group, ignore.case = T)],
            aes(y = analysis_group_lab,
                x = -5, vjust = 2.5,
                label = string),
            color = "grey50",
            size = 3)+
  geom_jitter(data = dat.prey[grepl("reproduction", analysis_group, ignore.case = T), ], 
              aes(x = yi, y = analysis_group_lab, 
                  size = 1/vi_analysis, fill = yi_analysis),
              shape = 21, #fill = "grey90",
              height = 0.25, width = 0,
              alpha = .5)+
  scale_fill_gradient2(low = "dodgerblue", high = "indianred",
                       midpoint = 0, mid = "white")+
  guides(size = "none", fill = "none")+
  geom_errorbar(data = intercept_inside[grepl("reproduction", analysis_group, ignore.case = T)], 
                aes(y = analysis_group_lab, 
                    xmin = lower_ci, xmax = upper_ci),
                width = .25)+
  geom_pointrange(data = intercept_inside[grepl("reproduction", analysis_group, ignore.case = T)], 
                  aes(x = pred, y = analysis_group_lab,
                      xmin = lower_pi, xmax = upper_pi),
                  shape = 21, fill = "grey50",
                  size = 1)+
  ggtitle("Bird reproduction")+
  ylab(NULL)+
  xlab("Association between cats and threatened bird reproduction")+
  scale_y_discrete(breaks = c("With/without cats", 
                              "Before-after eradication", "Inside/outside exclosure", "On islands with/without cats", 
                              "Spatial association", "Temporal association"),
                   labels = c("With-without cats",
                              "Before-after eradication", "Inside-outside exclosure", 
                              "Islands with-without cats", 
                              "Spatial association", "Temporal association"))+
  coord_cartesian(xlim = c(-6, 6))+
  # xlab(NULL)+
  # ylab("Short term abundance correlation (Zr)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
p.reprod.inside

p.inside <- p.abund.inside + p.reprod.inside + plot_layout(ncol = 1,
                                                           heights = c(5/8, 3/8)) +
  plot_annotation(tag_levels = "A")
p.inside


# >>> Class ----------------------------------------------------------------
predictions

#
class_pred <- predictions[moderator == "1" & class != "All" &
                       only_dominant_effect_size == "no" &
                         prey_range == "All"]
unique(class_pred$class)

class.abundance <- ggplot()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = class_pred[grepl("abundance", analysis_group, ignore.case = T)],
            aes(y = analysis_group_lab,
                x = -8, vjust = 2,
                label = string),
            color = "grey50",
            size = 3)+
  geom_jitter(data = dat.plot[grepl("abundance", analysis_group, ignore.case = T) &
                                analysis_group %in% class_pred$analysis_group & 
                                class %in% class_pred$class, ], 
              aes(x = yi, y = analysis_group_lab, 
                  size = 1/vi_analysis, fill = yi_analysis),
              shape = 21, #fill = "grey90",
              height = 0.25, width = 0,
              alpha = .5)+
  scale_fill_gradient2(low = "dodgerblue", high = "indianred",
                       midpoint = 0, mid = "white")+
  guides(size = "none", fill = "none")+
  geom_errorbar(data = class_pred[grepl("abundance", analysis_group, ignore.case = T)], 
                aes(y = analysis_group_lab, 
                    xmin = lower_ci, xmax = upper_ci),
                width = .25)+
  geom_pointrange(data = class_pred[grepl("abundance", analysis_group, ignore.case = T)], 
                  aes(x = pred, y = analysis_group_lab,
                      xmin = lower_pi, xmax = upper_pi),
                  shape = 21, fill = "grey50",
                  size = 1)+
  scale_y_discrete(breaks = c("With/without cats", 
                              "Before-after eradication", "Inside/outside exclosure", "On islands with/without cats", 
                              "Spatial association", "Temporal association"),
                   labels = c("With-without cats",
                              "Before-after eradication", "Inside-outside exclosure", 
                              "Islands with-without cats", 
                              "Spatial association", "Temporal association"))+
  facet_wrap(~class, scales = "free_x")+
  ggtitle("Abundance")+
  ylab(NULL)+
  xlab("Association between cats and threatened species")+
  # coord_cartesian(ylim = c(-4, 4))+
  # xlab(NULL)+
  # ylab("Short term abundance correlation (Zr)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
class.abundance

# Only birds:
# class.reproduction <- ggplot()+
#   geom_vline(xintercept = 0, linetype = "dashed")+
#   geom_text(data = class_pred[grepl("reproduction", analysis_group, ignore.case = T)],
#             aes(y = analysis_group_lab,
#                 x = -8, vjust = 2,
#                 label = string),
#             color = "grey50",
#             size = 3)+
#   geom_jitter(data = dat.plot[grepl("reproduction", analysis_group, ignore.case = T), ], 
#               aes(x = yi, y = analysis_group_lab, 
#                   size = 1/vi_analysis, fill = yi_analysis),
#               shape = 21, #fill = "grey90",
#               height = 0.25, width = 0,
#               alpha = .5)+
#   scale_fill_gradient2(low = "dodgerblue", high = "indianred",
#                        midpoint = 0, mid = "white")+
#   guides(size = "none", fill = "none")+
#   geom_errorbar(data = class_pred[grepl("reproduction", analysis_group, ignore.case = T)], 
#                 aes(y = analysis_group_lab, 
#                     xmin = lower_ci, xmax = upper_ci),
#                 width = .25)+
#   geom_pointrange(data = class_pred[grepl("reproduction", analysis_group, ignore.case = T)], 
#                   aes(x = pred, y = analysis_group_lab,
#                       xmin = lower_pi, xmax = upper_pi),
#                   shape = 21, fill = "grey50",
#                   size = 1)+
#   facet_wrap(~class, scales = "free_x")+
#   ggtitle("Reproduction")+
#   ylab(NULL)+
#   xlab("Association between cats and threatened species")+
#   # coord_cartesian(ylim = c(-4, 4))+
#   # xlab(NULL)+
#   # ylab("Short term abundance correlation (Zr)")+
#   guides(size = "none")+
#   theme_bw()+
#   theme(panel.grid = element_blank(),
#         plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom",
#         strip.placement = "outside",        
#         strip.background = element_blank(),
#         panel.border = element_blank())
# class.reproduction

# >>> log_mass ------------------------------------------------------------
tidy_models[moderator == "log_mass"  & only_dominant_effect_size == "no", ]

mass <- predictions[moderator == "log_mass"  & only_dominant_effect_size == "no" &
                      class == "All" & prey_range == "All", ]
unique(mass$analysis_group_lab)
#
# Back-transform mass.
mass[, mass_g := 10^as.numeric(term)]

mass$analysis_group_lab <- factor(mass$analysis_group_lab ,
                                  levels = (c("With/without cats",
                                              "On islands with/without cats",
                                              "Temporal association")))

dput(unique(dat.long$analysis_group_lab))
# dat.long$analysis_group <- factor(dat.long$analysis_group ,
#                               levels = (c("Abundance with/without cats", 
#                                              "Abundance on islands with/without cats", 
#                                              "Abundance temporal association", 
#                                              "Reproduction spatial association", 
#                                              "Abundance spatial association", 
#                                              "Abundance before-after eradication", 
#                                              "Reproduction inside/outside exclosure", 
#                                              "Reproduction with/without cats", 
#                                              "Reproduction before-after eradication", 
#                                              "Abundance inside/outside exclosure", 
#                                              "Reproduction temporal association")))
# #
p.mass.abund <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_ribbon(data = mass[grepl("abundance", analysis_group, ignore.case = T)], 
              aes(x = mass_g, 
                  ymin = lower_pi, ymax = upper_pi),
              alpha = .5,
              fill = "transparent",
              color = "black", linetype = "dotted")+
  geom_ribbon(data = mass[grepl("abundance", analysis_group, ignore.case = T)], 
              aes(x = mass_g, 
                  ymin = lower_ci, ymax = upper_ci),
              alpha = .5,
              fill = "grey")+
  geom_line(data = mass[grepl("abundance", analysis_group, ignore.case = T)], 
            aes(x = mass_g, 
                y = pred),
            color = "black")+
  geom_jitter(data = dat.long[grepl("abundance", analysis_group, ignore.case = T) &
                                analysis_group %in% mass$analysis_group], 
              aes(x = Mass_g_final, y = yi_analysis, 
                  fill = yi_analysis,
                  size = 1/vi_analysis),
              shape = 21,
              # fill = "grey50",
              alpha = .5)+
  scale_fill_gradient2(low = "dodgerblue", high = "indianred",
                       midpoint = 0, mid = "white")+
  ggtitle("Abundance")+
  facet_wrap(~(analysis_group_lab), scales = "free_y",
             ncol = 1,
             labeller = as_labeller(
               c("With/without cats" = "With-without cats", 
                 "On islands with/without cats" = "Islands with-without cats", 
                 "Temporal association"="Temporal association")))+
  # coord_cartesian(ylim = c(-2, 2))+
  labs(x = expression(atop("Body mass", "(grams,"~log[10]~"scale)")),#"Body mass (log<sub>10</sub>)",
       y = "Association between cats and threatened species")+
  guides(size = "none")+
  scale_x_log10()+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        # axis.text.x = ggtext::element_markdown(),
        strip.background = element_blank(),
        panel.border = element_blank())
p.mass.abund

# >>> Locomotion/habitat ----------------------------------------------------------------

tidy_models[moderator == "Foraging_habitat_ground" &
              analysis_group == "Abundance on islands with/without cats", 
            .(term, estimate, p.value, model_type)]






predictions
#
unique(predictions$moderator)
unique(predictions$prey_range)

unique(predictions$model_type)
habitat_pred <- predictions[moderator %in% c("Foraging_habitat_ground", "ground_or_burrow_resting_or_nesting",
                                           "locomotion_volant") & 
                            model_type == "primary_models" &
                            min_obs >= 3,
                            ]
unique(habitat_pred$moderator)
unique(habitat_pred$analysis_group)

unique(habitat_pred$term)

habitat_pred
#
unique(habitat_pred$analysis_group)

# Melt data:
dat.plot
dat.habitat <- melt(dat.plot,
                 measure.vars = c("locomotion_volant", "ground_or_burrow_resting_or_nesting",
                                  "Foraging_habitat_ground"),
                 variable.name = "moderator",
                 value.name = "term")
dat.habitat <- dat.habitat[analysis_group %in% habitat_pred$analysis_group]
dat.habitat[, key := paste(analysis_group, moderator)]
habitat_pred[, key := paste(analysis_group, moderator)]

dat.habitat <- dat.habitat[key %in% habitat_pred$key]
# unique(dat.habitat[, .(dat.habitat, )])
#
#
habitat.abund.1 <- ggplot()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = habitat_pred[grepl("abundance", analysis_group, ignore.case = T) &
                                  !moderator == "locomotion_volant"],
            aes(y = analysis_group_lab,
                x = -2, vjust = 2.5,
                label = string,
                color = term),
            position = position_dodgev(height = .5),
            size = 2.5)+
  geom_jitter(data = dat.habitat[grepl("abundance", analysis_group, ignore.case = T) &
                                   !moderator == "locomotion_volant", ],
              aes(x = yi, y = analysis_group_lab,
                  group = term, fill = term,
                  size = 1/vi_analysis), #fill = yi_analysis),
              # position = position_dodgev(height = .25),
              position = position_jitterdodgev(jitter.height = .1,
                                               jitter.width = 0,
                                               dodge.height = .5
                                               ),
              shape = 21, #fill = "grey90",
              # height = 0.15, width = 0,
              alpha = .6)+
  # scale_fill_gradient2(low = "dodgerblue", high = "indianred",
  #                      midpoint = 0, mid = "white")+
  geom_errorbar(data = habitat_pred[grepl("abundance", analysis_group, ignore.case = T) &
                                      !moderator == "locomotion_volant"], 
                aes(y = analysis_group_lab, group = term,
                    # color = term,
                    xmin = lower_ci, xmax = upper_ci),
                position = position_dodgev(height = .5),
                width = .25)+
  geom_pointrange(data = habitat_pred[grepl("abundance", analysis_group, ignore.case = T) &
                                        !moderator == "locomotion_volant"], 
                  aes(x = pred, y = analysis_group_lab,
                      group = term, fill = term,
                      xmin = lower_pi, xmax = upper_pi),
                  position = position_dodgev(height = .5),
                  shape = 21,
                  # fill = "grey50",
                  size = 1)+
  scale_y_discrete(breaks = c("With/without cats", 
                              "Before-after eradication", "Inside/outside exclosure", "On islands with/without cats", 
                              "Spatial association", "Temporal association"),
                   labels = c("With-without cats",
                              "Before-after eradication", "Inside-outside exclosure", 
                              "Islands with-without cats", 
                              "Spatial association", "Temporal association"))+
  scale_color_manual(values = c("ground" = "#6D1A36",
                               "non_volant" = "#63535B",
                               "other" = "#FCD0A1",
                               "volant" = "#53917E"),
                    labels = c("ground" = "Ground",
                               "non_volant" = "Non-volant",
                               "other" = "Aerial/arboreal",
                               "volant" = "Volant"))+
  scale_fill_manual(values = c("ground" = "#6D1A36",
                               "non_volant" = "#63535B",
                               "other" = "#FCD0A1",
                               "volant" = "#53917E"),
                    labels = c("ground" = "Ground",
                               "non_volant" = "Non-volant",
                               "other" = "Aerial/arboreal",
                               "volant" = "Volant"))+
  facet_wrap(~moderator, scales = "free_x",
             labeller = as_labeller(c("Foraging_habitat_ground" = "Foraging habitat",
                                      "ground_or_burrow_resting_or_nesting" = "Nesting/resting habitat",
                                      "locomotion_volant" = "Locomotion")))+
  ylab(NULL)+
  xlab("Association between cats and\nthreatened species")+
  # coord_cartesian(ylim = c(-4, 4))+
  # xlab(NULL)+
  # ylab("Short term abundance correlation (Zr)")+
  ggtitle("Abundance")+
  guides(size = "none", color = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 1),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
habitat.abund.1

habitat.abund.2 <- ggplot()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = habitat_pred[grepl("abundance", analysis_group, ignore.case = T) &
                                  moderator == "locomotion_volant"],
            aes(y = analysis_group_lab,
                x = -3, vjust = 2.5,
                label = string,
                color = term),
            position = position_dodgev(height = .5),
            size = 2.5)+
  geom_jitter(data = dat.habitat[grepl("abundance", analysis_group, ignore.case = T) &
                                   moderator == "locomotion_volant", ],
              aes(x = yi, y = analysis_group_lab,
                  group = term, fill = term,
                  size = 1/vi_analysis), #fill = yi_analysis),
              # position = position_dodgev(height = .25),
              position = position_jitterdodgev(jitter.height = .1,
                                               jitter.width = 0,
                                               dodge.height = .5
              ),
              shape = 21, #fill = "grey90",
              # height = 0.15, width = 0,
              alpha = .6)+
  # scale_fill_gradient2(low = "dodgerblue", high = "indianred",
  #                      midpoint = 0, mid = "white")+
  geom_errorbar(data = habitat_pred[grepl("abundance", analysis_group, ignore.case = T) &
                                      moderator == "locomotion_volant"], 
                aes(y = analysis_group_lab, group = term,
                    # color = term,
                    xmin = lower_ci, xmax = upper_ci),
                position = position_dodgev(height = .5),
                width = .25)+
  geom_pointrange(data = habitat_pred[grepl("abundance", analysis_group, ignore.case = T) &
                                        moderator == "locomotion_volant"], 
                  aes(x = pred, y = analysis_group_lab,
                      group = term, fill = term,
                      xmin = lower_pi, xmax = upper_pi),
                  position = position_dodgev(height = .5),
                  shape = 21,
                  # fill = "grey50",
                  size = 1)+
  scale_color_manual(values = c("ground" = "#6D1A36",
                                "non_volant" = "#63535B",
                                "other" = "#FCD0A1",
                                "volant" = "#53917E"),
                     labels = c("ground" = "Ground",
                                "non_volant" = "Non-volant",
                                "other" = "Aerial/arboreal",
                                "volant" = "Volant"))+
  scale_fill_manual(values = c("ground" = "#6D1A36",
                               "non_volant" = "#63535B",
                               "other" = "#FCD0A1",
                               "volant" = "#53917E"),
                    labels = c("ground" = "Ground",
                               "non_volant" = "Non-volant",
                               "other" = "Aerial/arboreal",
                               "volant" = "Volant"))+
  scale_y_discrete(breaks = c("With/without cats", 
                              "Before-after eradication", "Inside/outside exclosure", "On islands with/without cats", 
                              "Spatial association", "Temporal association"),
                   labels = c("With-without cats",
                              "Before-after eradication", "Inside-outside exclosure", 
                              "Islands with-without cats", 
                              "Spatial association", "Temporal association"))+
  facet_wrap(~moderator, scales = "free_x",
             labeller = as_labeller(c("Foraging_habitat_ground" = "Foraging habitat",
                                      "ground_or_burrow_resting_or_nesting" = "Nesting/resting habitat",
                                      "locomotion_volant" = "Locomotion")))+
  ylab(NULL)+
  xlab("Association between cats and\nthreatened species")+
  # coord_cartesian(ylim = c(-4, 4))+
  # xlab(NULL)+
  # ylab("Short term abundance correlation (Zr)")+
  # ggtitle("Abundance")+
  guides(size = "none", color = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 1),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
habitat.abund.2


# Add empty space for the missing temporal:
right <-  habitat.abund.2 + theme(axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank()) +
  plot_spacer() + plot_layout(heights = c(10/12, 2/12)) #

habitat.final <- habitat.abund.1 + 
  right +
  plot_layout(nrow = 1, 
               widths = c(2/3, 1/3))
habitat.final

# >>> Continent vs island ----------------------------------------------------------
predictions
#
tidy_models[moderator %in% c("continent_island") & 
              class == "All" &
              prey_range == "All" & 
              only_dominant_effect_size == "no"  &
              p.value <= 0.05]
#
unique(predictions$moderator)
unique(predictions$model_type)

island_pred <- predictions[moderator %in% c("continent_island") & 
                           model_type == "primary_models", ]
unique(island_pred$moderator)
unique(island_pred$term)
unique(island_pred$analysis_group)
# again, no reproduction

island_pred
#
unique(island_pred$analysis_group)

# Melt data:
dat.plot
dat.island <- melt(dat.plot,
                 measure.vars = c("continent_island"),
                 variable.name = "moderator",
                 value.name = "term")
dat.island <- dat.island[analysis_group %in% island_pred$analysis_group]

#
Ns <- dat.island[, .(n_articles = uniqueN(article_id),
                   n_obs = uniqueN(Effect_size_ID),
                   n_species = uniqueN(scientificName)),
               by = .(analysis_group_lab, analysis_group,
                      moderator, term, analysis_effect_size)]
Ns[, N_string :=  paste0(n_articles, "(", n_species, ", ", n_obs, ")")]
Ns

#
Ns[n_obs < 3, ]$analysis_group
island_pred <- island_pred[!analysis_group %in% Ns[n_obs < 3, ]$analysis_group]
dat.island <- dat.island[!analysis_group %in% Ns[n_obs < 3, ]$analysis_group]
Ns <- Ns[analysis_group %in% island_pred$analysis_group, ]
Ns

#
unique(dat.island$term)
unique(dat.island$moderator)

continent.abund <- ggplot()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = Ns[grepl("abundance", analysis_group, ignore.case = T)],
            aes(y = analysis_group_lab,
                x = -1, vjust = 2.5,
                label = N_string,
                color = term),
            position = position_dodgev(height = .5),
            size = 2.5)+
  geom_jitter(data = dat.island[grepl("abundance", analysis_group, ignore.case = T), ],
              aes(x = yi, y = analysis_group_lab,
                  group = term, fill = term,
                  size = 1/vi_analysis), #fill = yi_analysis),
              # position = position_dodgev(height = .25),
              position = position_jitterdodgev(jitter.height = .1,
                                               jitter.width = 0,
                                               dodge.height = .5
              ),
              shape = 21, 
              alpha = .6)+
  geom_errorbar(data = island_pred[grepl("abundance", analysis_group, ignore.case = T)], 
                aes(y = analysis_group_lab, group = term,
                    # color = term,
                    xmin = lower_ci, xmax = upper_ci),
                position = position_dodgev(height = .5),
                width = .25)+
  geom_pointrange(data = island_pred[grepl("abundance", analysis_group, ignore.case = T) ], 
                  aes(x = pred, y = analysis_group_lab,
                      group = term, fill = term,
                      xmin = lower_pi, xmax = upper_pi),
                  position = position_dodgev(height = .5),
                  shape = 21,
                  # fill = "grey50",
                  size = 1)+
  scale_fill_manual(values = c("mainland" = "#6D1A36",
                               "island" = "#53917E"),
                    labels = c("mainland" = "Continent (Australia)",
                               "island" = "Island"))+
  scale_color_manual(values = c("mainland" = "#6D1A36",
                               "island" = "#53917E"),
                    labels = c("mainland" = "Continent (Australia)",
                               "island" = "Island"))+
  facet_wrap(~moderator, scales = "free_x",
             labeller = as_labeller(c("continent_island" = "Island or Continent (Australia)")))+
  ylab(NULL)+
  xlab("Association between cats and\nthreatened species")+
  # coord_cartesian(ylim = c(-4, 4))+
  # xlab(NULL)+
  # ylab("Short term abundance correlation (Zr)")+
  ggtitle("Abundance")+
  guides(size = "none", color = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())

continent.abund

# >>> Only dominant effect sizes --------------------------------------------------------------------

tidy_models[moderator == "1" & only_dominant_effect_size == "yes", ]

intercepts <- predictions[moderator == "1" & 
                            only_dominant_effect_size == "yes" &
                            class == "All" &
                            prey_range == "All"]

unique(intercepts$analysis_group)

intercepts[, key := paste(analysis_effect_size, analysis_group)]
dat.plot[, key := paste(analysis_effect_size, analysis_group)]

#
p1.si <- ggplot()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = intercepts[grepl("abundance", analysis_group, ignore.case = T)],
            aes(y = analysis_group_lab,
                x = -3, vjust = 2,
                label = string),
            color = "grey50",
            size = 3)+
  geom_jitter(data = dat.plot[grepl("abundance", analysis_group, ignore.case = T) &
                                key %in% intercepts$key, ], 
              aes(x = yi, y = analysis_group_lab, 
                  size = 1/vi_analysis, fill = yi_analysis),
              shape = 21, #fill = "grey90",
              height = 0.25, width = 0,
              alpha = .5)+
  scale_fill_gradient2(low = "dodgerblue", high = "indianred",
                       midpoint = 0, mid = "white")+
  guides(size = "none", fill = "none")+
  geom_errorbar(data = intercepts[grepl("abundance", analysis_group, ignore.case = T)], 
                aes(y = analysis_group_lab, 
                    xmin = lower_ci, xmax = upper_ci),
                width = .25)+
  geom_pointrange(data = intercepts[grepl("abundance", analysis_group, ignore.case = T)], 
                  aes(x = pred, y = analysis_group_lab,
                      xmin = lower_pi, xmax = upper_pi),
                  shape = 21, fill = "grey50",
                  size = 1)+
  scale_y_discrete(breaks = c("With/without cats", 
                              "Before-after eradication", "Inside/outside exclosure", "On islands with/without cats", 
                              "Spatial association", "Temporal association"),
                   labels = c("With-without cats",
                              "Before-after eradication", "Inside-outside exclosure", 
                              "Islands with-without cats", 
                              "Spatial association", "Temporal association"))+
  ggtitle("Abundance")+
  ylab(NULL)+
  xlab("Association between cats and threatened species")+
  coord_cartesian(xlim = c(-4, 4))+
  # xlab(NULL)+
  # ylab("Short term abundance correlation (Zr)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
p1.si

p2.si <- ggplot()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = intercepts[grepl("reproduction", analysis_group, ignore.case = T)],
            aes(y = analysis_group_lab,
                x = -15, vjust = 2,
                label = string),
            color = "grey50",
            size = 3)+
  geom_jitter(data = dat.plot[grepl("reproduction", analysis_group, ignore.case = T) &
                                key %in% intercepts$key, ], 
              aes(x = yi, y = analysis_group_lab, 
                  size = 1/vi_analysis, fill = yi_analysis),
              shape = 21, #fill = "grey90",
              height = 0.25, width = 0,
              alpha = .5)+
  scale_fill_gradient2(low = "dodgerblue", high = "indianred",
                       midpoint = 0, mid = "white")+
  guides(size = "none", fill = "none")+
  geom_errorbar(data = intercepts[grepl("reproduction", analysis_group, ignore.case = T)], 
                aes(y = analysis_group_lab, 
                    xmin = lower_ci, xmax = upper_ci),
                width = .25)+
  geom_pointrange(data = intercepts[grepl("reproduction", analysis_group, ignore.case = T)], 
                  aes(x = pred, y = analysis_group_lab,
                      xmin = lower_pi, xmax = upper_pi),
                  shape = 21, fill = "grey50",
                  size = 1)+
  scale_y_discrete(breaks = c("With/without cats", 
                              "Before-after eradication", "Inside/outside exclosure", "On islands with/without cats", 
                              "Spatial association", "Temporal association"),
                   labels = c("With-without cats",
                              "Before-after eradication", "Inside-outside exclosure", 
                              "Islands with-without cats", 
                              "Spatial association", "Temporal association"))+
  ggtitle("Reproduction")+
  ylab(NULL)+
  xlab("Association between cats and bird reproduction")+
  coord_cartesian(xlim = c(-30, 30))+
  # xlab(NULL)+
  # ylab("Short term abundance correlation (Zr)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
p2.si

dominant.only <- p1.si + p2.si + plot_layout(guides = "collect", nrow = 2,
                            heights = c(3/5, 2/5)) +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "none")
dominant.only

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Save  SI plots ----------------------------------------------------------

dominant.only
ggsave("figures/SI/meta dominant effect size.pdf",
       width = 7, height = 8)
ggsave("figures/SI/meta dominant effect size.png",
       width = 7, height = 8)

class.abundance
ggsave("figures/SI/meta effects by class.pdf",
       width = 9, height = 9)
ggsave("figures/SI/meta effects by class.png",
       width = 9, height = 9)

p.mass.abund
ggsave("figures/SI/meta effects by mass.pdf",
       width = 6, height = 7.5)
ggsave("figures/SI/meta effects by mass.png",
       width = 6, height = 7.5)

habitat.final
ggsave("figures/SI/meta effects by habitat.pdf",
       width = 11, height = 7)
ggsave("figures/SI/meta effects by habitat.png",
       width = 11, height = 7)

p.inside # Only species inside prey range
ggsave("figures/SI/meta effects inside prey range only.pdf",
       width = 9, height = 9)
ggsave("figures/SI/meta effects inside prey range only.png",
       width = 9, height = 9)

continent.abund
ggsave("figures/SI/meta effects continent vs island.pdf",
       width = 7, height = 7)
ggsave("figures/SI/meta effects continent vs island.png",
       width = 7, height = 7)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# SI Tables ---------------------------------------------------------------

si_models <- copy(tidy_models)
si_models$term
#
# >>> All data models ----------------------------------------------------

si_models <- si_models[, !c("random_effect", "model_comparison_id", 
                            "non_phylo_species", "phylo_species", "I2_scientificName",
                            "I2_phylo_species", "min_aic", #"moderator", 
                            "model_id", "aic", "overfit")]
si_models[, `Residual heterogeneity` := paste0("$I^2_{total}=", round(I2_Total, 1), "$<br>",
                                                "$I^2_{article}=", round(I2_article_id, 1), "$<br>",
                                                "$I^2_{obs}=", round(`I2_article_id/Effect_size_ID`), "$")]

si_models[, `Test statistics` := paste0("$t_{", df, "}=", round(statistic, 2), ", p=", round(p.value, 2), "$")]
si_models

si_models[, Estimate := paste0(round(estimate, 2), " ±[", round(lower_ci, 2), ", ", round(upper_ci, 2), "]")]

si_models[is.na(n_fac_obs), `Sample size` := paste0("$N_{articles}=", n_articles, "$<br>",
                                     "$N_{species}=", n_species, "$<br>",
                                     "$N_{observations}=", n_obs, "$")]

si_models[!is.na(n_fac_obs), `Sample size` := paste0("$N_{articles}=", n_fac_articles, "$<br>",
                                                    "$N_{species}=", n_fac_species, "$<br>",
                                                    "$N_{observations}=", n_fac_obs, "$")]


si_models$analysis_group

si_models[class == "All", class := "All species"]
si_models$class <- factor(si_models$class,
                          levels = rev(c("All species", "Mammals", "Birds")))
si_models

unique(si_models$moderator)

si_models[moderator %in% c("Foraging_habitat_ground", 
                           "ground_or_burrow_resting_or_nesting",
                           "locomotion_volant", "continent_island"),
          term := gsub(unique(moderator), "", term),
          by = .(moderator)]

si_models[moderator %in% c("Foraging_habitat_ground", 
                           "ground_or_burrow_resting_or_nesting",
                           "locomotion_volant"),]$term

unique(dat.long$Foraging_habitat_ground)
unique(si_models[moderator %in% c("Foraging_habitat_ground")]$term)
unique(dat.long$ground_or_burrow_resting_or_nesting)
unique(si_models[moderator %in% c("ground_or_burrow_resting_or_nesting")]$term)
unique(dat.long$locomotion_volant)
unique(si_models[moderator %in% c("locomotion_volant")]$term)

si_models[moderator %in% c("Foraging_habitat_ground") &
            term == "intercept", term := "ground (Intercept)"]
si_models[moderator %in% c("ground_or_burrow_resting_or_nesting") &
            term == "intercept", term := "ground (Intercept)"]
si_models[moderator %in% c("locomotion_volant") &
            term == "intercept", term := "non-volant (Intercept)"]

unique(si_models$moderator)
unique(si_models[moderator %in% c("continent_island")]$term)
si_models[moderator %in% c("continent_island") &
            term == "intercept", term := "island (Intercept)"]


si_models[, moderator := fcase(moderator == "Foraging_habitat_ground", "Ground foraging",
                               moderator == "ground_or_burrow_resting_or_nesting", "Ground nesting/resting",
                               moderator == "locomotion_volant", "Volant",
                               moderator == "continent_island", "Continent vs island",
                               default = moderator)]
si_models$term

si_models[, min_obs := ifelse(is.na(n_fac_obs), 
                              n_obs,
                              min(n_fac_obs)), 
          by = .(analysis_group, moderator, class, prey_range, only_dominant_effect_size,
                 analysis_effect_size)]
si_models
#

#
unique(si_models$analysis_group)
unique(si_models$term)

si_models[,moderator := ifelse(moderator == "1", "Overall estimate", moderator)]
si_models[, term := ifelse(term == "log_mass",  "Mass (g, log10)", term)]
si_models[, moderator := ifelse(moderator == "log_mass",  "Mass", moderator)]

# Sort factor levels...This is going to be very annoying.
si_models[, group := paste(analysis_group, class, moderator, analysis_effect_size, sep = " | ")]
dput(unique(si_models[class == "All species" & moderator == "Overall estimate"]$group))
unique(si_models$moderator)
dput(unique(si_models[class == "All species" & moderator == "Mass"]$group))


lvls <- c("Abundance with/without cats | All species | Overall estimate | SMD", 
          "Abundance before-after eradication | All species | Overall estimate | SMD", 
          "Abundance on islands with/without cats | All species | Overall estimate | lnOR", 
          "Abundance spatial association | All species | Overall estimate | Zr", 
          "Abundance temporal association | All species | Overall estimate | Zr", 
          "Reproduction with/without cats | All species | Overall estimate | lnOR", 
          "Reproduction before-after eradication | All species | Overall estimate | SMD", 
          "Reproduction temporal association | All species | Overall estimate | Zr", 
          "Abundance with/without cats | Mammals | Overall estimate | SMD", 
          "Abundance on islands with/without cats | Mammals | Overall estimate | lnOR", 
          "Abundance spatial association | Mammals | Overall estimate | Zr", 
          "Abundance temporal association | Mammals | Overall estimate | Zr", 
          "Abundance with/without cats | Birds | Overall estimate | SMD", 
          "Abundance on islands with/without cats | Birds | Overall estimate | lnOR", 
          "Reproduction with/without cats | Birds | Overall estimate | lnOR", 
          "Reproduction before-after eradication | Birds | Overall estimate | SMD", 
          "Reproduction temporal association | Birds | Overall estimate | Zr", 
          
          "Abundance with/without cats | All species | Mass | SMD", 
          "Abundance on islands with/without cats | All species | Mass | lnOR",
          "Abundance temporal association | All species | Mass | Zr", 
          "Abundance temporal association | Mammals | Mass | Zr",
          
          "Abundance with/without cats | All species | Continent vs island | SMD", 
          "Abundance spatial association | All species | Continent vs island | Zr", 
          "Abundance temporal association | All species | Continent vs island | Zr", 
          "Abundance with/without cats | Mammals | Continent vs island | SMD", 
          "Abundance spatial association | Mammals | Continent vs island | Zr", 
          "Abundance temporal association | Mammals | Continent vs island | Zr", 
          
          "Abundance with/without cats | All species | Ground foraging | SMD", 
          "Abundance on islands with/without cats | All species | Ground foraging | lnOR", 
          "Abundance temporal association | All species | Ground foraging | Zr", 
          "Abundance with/without cats | Mammals | Ground foraging | SMD", 
          "Abundance on islands with/without cats | Mammals | Ground foraging | lnOR", 
          "Abundance temporal association | Mammals | Ground foraging | Zr", 
          "Abundance with/without cats | Birds | Ground foraging | SMD", 
          "Abundance on islands with/without cats | Birds | Ground foraging | lnOR", 
          
          "Abundance with/without cats | All species | Ground nesting/resting | SMD", 
          "Abundance on islands with/without cats | All species | Ground nesting/resting | lnOR", 
          "Abundance temporal association | All species | Ground nesting/resting | Zr", 
          "Abundance with/without cats | Mammals | Ground nesting/resting | SMD", 
          "Abundance on islands with/without cats | Mammals | Ground nesting/resting | lnOR", 
          "Abundance temporal association | Mammals | Ground nesting/resting | Zr", 
          "Abundance with/without cats | Birds | Ground nesting/resting | SMD", 
          "Abundance on islands with/without cats | Birds | Ground nesting/resting | lnOR", 
          
          "Abundance with/without cats | All species | Volant | SMD", "Abundance before-after eradication | All species | Volant | SMD", 
          "Abundance on islands with/without cats | All species | Volant | lnOR", 
          "Abundance temporal association | All species | Volant | Zr", 
          "Abundance with/without cats | Mammals | Volant | SMD", "Abundance on islands with/without cats | Mammals | Volant | lnOR", 
          "Abundance with/without cats | Birds | Volant | SMD", "Abundance on islands with/without cats | Birds | Volant | lnOR"
)
lvls <- lvls[lvls %in% si_models$group]
setdiff(si_models$group, lvls)

si_models$group <- factor(si_models$group,
                          levels = lvls)

# Also should sort by Intercept vs moderator
unique(si_models$term)
si_models$term <- factor(si_models$term,
                         levels = c("overall", "ground (Intercept)",
                                    "island (Intercept)", "intercept", "non-volant (Intercept)",
                                    "other", "mainland", "volant", "Mass (g, log10)"))
si_models[, num1 := as.numeric(group)]
si_models[, num2 := as.numeric(term)]

setorder(si_models, num1, num2)
si_models

#
main_text <- si_models %>%
  filter(only_dominant_effect_size == "no" &
           prey_range == "All" &
           model_type == "primary_models" &
           # !(class != "All species" & !moderator %in% c("Overall estimate") ) &
           min_obs >= 3) %>%
  select(group, term, Estimate, 
         moderator,
         `Test statistics`, `Sample size`, 
         `Residual heterogeneity`) %>%
  rename("Term" = "term",
         "Moderator" = "moderator") %>%
  group_by(group) %>%
  # arrange(group) %>%
  gt() %>%
  fmt_markdown(columns = `Residual heterogeneity`) %>%
  fmt_markdown(columns = `Moderator`) %>%
  fmt_markdown(columns = `Sample size`) %>%
  fmt_markdown(columns = `Test statistics`) %>%
  tab_header(#title = ,
    md("**Table S6**. Model summaries for intercept-only (main text) models as well as models evaluating influence of habitat, body mass, and continents versus islands. Model estimates ± 95% CIs and test statistics are reported along with sample sizes and residual unexplained heterogeneity ($I^2$), decomposed by hierarchical model levels (article and observation). The effect size used is given in the heading of each model (SMD=standardized mean difference or Hedges' g; Zr=correlation coefficient; lnOR=log odds ratio).")) |>
  opt_align_table_header(align = c("left")) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()) %>%
  opt_table_font(
    size = 12
  ) |> 
  tab_options(
    latex.use_longtable = TRUE,
    latex.header_repeat = FALSE
  )
main_text
gtsave(main_text, filename = "figures/SI/main text model table.pdf")

# fmt_markdown(columns = `Sample size`) %>%


# >>> Sub table of non-intercept models ------------------------------------
si_models %>%
  filter(only_dominant_effect_size == "no" &
           prey_range == "All" &
           model_type == "intercept_removed" &
           # !(class != "All species" & !moderator %in% c("Overall estimate") ) &
           min_obs >= 3) %>% 
  pull(p.value) |> range()

unique(si_models$model_type)

no_intercept <- si_models %>%
  filter(only_dominant_effect_size == "no" &
           prey_range == "All" &
           model_type == "intercept_removed" &
           # !(class != "All species" & !moderator %in% c("Overall estimate") ) &
           min_obs >= 3) %>%
  select(group, term_name, Estimate, 
         moderator,
         `Test statistics`, `Sample size`, 
         `Residual heterogeneity`) %>%
  rename("Term" = "term_name",
         "Moderator" = "moderator") %>%
  group_by(group) %>%
  # arrange(group) %>%
  gt() %>%
  fmt_markdown(columns = `Residual heterogeneity`) %>%
  fmt_markdown(columns = `Moderator`) %>%
  fmt_markdown(columns = `Sample size`) %>%
  fmt_markdown(columns = `Test statistics`) %>%
  tab_header(#title = ,
    md("**Table SX**. Model summaries for intercept-only (main text) models as well as models evaluating influence of habitat, body mass, and continents versus islands. Model estimates ± 95% CIs and test statistics are reported along with sample sizes and residual unexplained heterogeneity ($I^2$), decomposed by hierarchical model levels (article and observation). The effect size used is given in the heading of each model (SMD=standardized mean difference or Hedges' g; Zr=correlation coefficient; lnOR=log odds ratio).")) |>
  opt_align_table_header(align = c("left")) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()) %>%
  opt_table_font(
    size = 12
  )
no_intercept
gtsave(no_intercept, filename = "figures/SI/difference from 0 tests.pdf")


# >>> Inside prey range -------------------------------------------------------
unique(dat.long[prey_range_tertiary == "outside"]$scientificName)
dat.long[prey_range_tertiary == "outside", .(n = uniqueN(Effect_size_ID),
                                             articles = uniqueN(article_id))]


inside_prey_range <- si_models %>% 
  filter(only_dominant_effect_size == "no" &
           prey_range == "inside" &
           min_obs >= 3) %>%
  select(group, term, Estimate, 
         moderator,
         `Test statistics`, `Sample size`, 
         `Residual heterogeneity`) %>%
  rename("Term" = "term",
         "Moderator" = "moderator") %>%
  group_by(group) %>%
  arrange(group) %>%  
  gt() %>%
  fmt_markdown(columns = `Residual heterogeneity`) %>%
  fmt_markdown(columns = `Moderator`) %>%
  fmt_markdown(columns = `Sample size`) %>%
  fmt_markdown(columns = `Test statistics`) %>%
  tab_header(#title = ,
    md("**Table S8**. Model summaries for models filtered to only include threatened species that are within the prey range of cats, thus excluding 2 observations from 2 articles on *Eupleres goudotii* and *Megadyptes antipodes.* Model estimates ± 95% CIs and test statistics are reported along with sample sizes and residual unexplained heterogeneity ($I^2$), decomposed by hierarchical model structure (article and observation). The effect size used is given in the heading of each model (SMD=standardized mean difference or Hedges' g; Zr=correlation coefficient; lnOR=log odds ratio).")) |>
  opt_align_table_header(align = c("left")) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()) %>%
  opt_table_font(
    size = 12
  )
inside_prey_range
gtsave(inside_prey_range, filename = "figures/SI/inside prey range table.pdf")

# fmt_markdown(columns = `Sample size`) %>%

# >>> Only dominant -------------------------------------------------------
#
only_dominant <- si_models %>%
  filter(only_dominant_effect_size == "yes" &
           prey_range == "All" &
           min_obs >= 3) %>%
  select(group, term, Estimate, 
         moderator,
         `Test statistics`, `Sample size`, 
         `Residual heterogeneity`) %>%
  rename("Term" = "term",
         "Moderator" = "moderator") %>%
  group_by(group) %>%
  arrange(group) %>%  
  gt() %>%
  fmt_markdown(columns = `Residual heterogeneity`) %>%
  fmt_markdown(columns = `Moderator`) %>%
  fmt_markdown(columns = `Sample size`) %>%
  fmt_markdown(columns = `Test statistics`) %>%
  tab_header(#title = ,
    md("**Table S7**. Model summaries of models filtered to only include the dominant effect size type (e.g., Zr, SMD or lnOR) without converted effect sizes. Model estimates ± 95% CIs and test statistics are reported along with sample sizes and residual unexplained heterogeneity ($I^2$), decomposed by hierarchical model structure (article and observation). The effect size used is given in the heading of each model (SMD=standardized mean difference or Hedges' g; Zr=correlation coefficient; lnOR=log odds ratio).")) |>
  opt_align_table_header(align = c("left")) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()) %>%
  opt_table_font(
    size = 12
  )
only_dominant
gtsave(only_dominant, filename = "figures/SI/only dominant effect size models.pdf")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Systematic review panel -------------------------------------------------
sys_rev <- fread("builds/systematic_review/systematic_review_tidy.csv")
sys_rev

sys_rev[spp_name_corrected == "Dasyurus maculatus"]$exclude_species
sys_rev <- sys_rev[exclude_species == "included_species",]
n_studies <- sys_rev[, .(n_studies = uniqueN(study_id)),
                     by = .(evidence_type, has_data, of_quality, Hypothesis_supported,
                            class)]
n_studies

n_studies[, synth_fill := fcase(grepl("Population", evidence_type) & has_data == "yes", paste(evidence_type, "with data"),
                                grepl("Population", evidence_type) & has_data == "no", paste(evidence_type, "without data"))]

n_studies[is.na(synth_fill), synth_fill := evidence_type]
n_studies[of_quality == "yes",]
n_studies[of_quality == "yes", synth_fill := paste(synth_fill, "and qualities")]

# >>> Plot ----------------------------------------------------------------
# `Predation in support without data` = "#a6d3a0", 
# `Predation not in support without data` = "#40531b", 
fill_pal <- c(`Predation in support` = "#a6d3a0", `Predation not in support` = "#40531b", 
              `Population not in support without data` = "indianred4", `Population not in support with data` = "indianred", 
              `Population not in support with data and qualities` = "indianred", 
              `Population in support without data` = "dodgerblue4", `Population in support with data` = "dodgerblue",
              `Population in support with data and qualities` = "dodgerblue"
)
#
dput(unique(n_studies$synth_fill))

n_studies$synth_fill <- factor(n_studies$synth_fill,
                               levels = (c("Predation not in support", "Predation in support", 
                                          "Population not in support without data",
                                          "Population not in support with data", "Population not in support with data and qualities",
                                          "Population in support without data", "Population in support with data", 
                                          "Population in support with data and qualities" 
                                          )))

#
n_studies[, evidence_simple := fcase(grepl("Predation", evidence_type), "Predation",
                                     grepl("Population", evidence_type), "Population")]


# n_studies[, x_axis := paste(evidence_simple, Hypothesis_supported)]

p.sys <- ggplot(data = n_studies, 
                aes(x = class, y = n_studies, fill = synth_fill, color = of_quality))+
  geom_col()+ #position = position_dodge()
  ylab("Number of studies")+
  # facet_wrap(~class)+
  xlab(NULL)+
  scale_fill_manual(values = fill_pal)+
  scale_color_manual(values = c("no" = "transparent",
                                "yes" = "gold"))+
  guides(fill = guide_legend(nrow = 4))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_blank())
# I hate this kind of figure
p.sys

#
n_studies.alt <- copy(n_studies)
n_studies.alt[Hypothesis_supported == 0, n_studies := -n_studies]
n_studies.alt$synth_fill <- factor(n_studies.alt$synth_fill,
                               levels = (c("Predation not in support", "Predation in support", 
                                              "Population not in support without data",
                                              "Population not in support with data", "Population not in support with data and qualities",
                                              "Population in support without data", "Population in support with data", 
                                              "Population in support with data and qualities" 
                               )))

#
p.sys <- ggplot(data = n_studies.alt, 
                aes(y = class, x = n_studies, fill = synth_fill, color = of_quality))+
  geom_col(lwd = 1)+ #position = position_dodge()
  geom_vline(xintercept = 0)+
  # coord_flip()+
  ylab("Number of studies")+
  # facet_wrap(~class)+
  coord_cartesian(xlim = c(-50, 50))+
  xlab(NULL)+
  scale_fill_manual(values = fill_pal)+
  scale_color_manual(values = c("no" = "transparent",
                                "yes" = "gold"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_blank())
# I hate this kind of figure
p.sys

n_studies.alt[evidence_simple %in% "Population" & class == "Birds"]
# n_studies.alt[class %in% c("Reptiles", "Amphibians"), class := "Reptiles & Amphibians"]
n_studies.alt$class <- factor(n_studies.alt$class,
                              levels = c("Birds", "Reptiles", "Mammals", "Amphibians"))
n_studies.alt$Hypothesis_supported <- factor(n_studies.alt$Hypothesis_supported ,
                                             levels = c(1, 0))
p.sys <- ggplot(data = n_studies.alt, 
                aes(x = (Hypothesis_supported), 
                    y = abs(n_studies), fill = synth_fill, color = of_quality))+
  geom_col(lwd = 1)+ #
  # geom_hline(yintercept = 0)+
  # coord_flip()+
  ylab("Number of studies")+
  facet_wrap(~class, scales = "free_x",
             strip.position = "bottom", nrow = 1)+
  xlab(NULL)+
  scale_fill_manual(name = NULL,
                    values = fill_pal)+
  scale_color_manual(name = "With qualities",
                     values = c("no" = "transparent",
                                "yes" = "gold"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_blank())
# I hate this kind of figure
p.sys

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Best evidence per species -----------------------------------------------------------
claims <- fread("builds/claims/species_claims_tidy_populated.csv")
library("beepr")
beep(7)
#' [Use scientificName instead of spp_name_corrected!]
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
claims[duplicated(spp_name_corrected)]

claims[spp_name_corrected == "Dasyurus maculatus", ]

# We have class for this one so let's use the other name:
claims[spp_name_corrected == "Prosobonia parvirostris", spp_name_corrected := scientificName]

# claims[spp_name_corrected == "Coenocorypha pusilla", Synonyms_or_previous_lump := "Coenocorypha aucklandica"]
claims[duplicated(spp_name_corrected)]

#
ranking <- copy(sys_rev)
ranking <- ranking[, .(spp_name_corrected,
                       of_quality, evidence_type, has_data)]
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
best_evidence[duplicated(spp_name_corrected)]
best_evidence[spp_name_corrected == "Procellaria cinerea"]

#
best_evidence[is.na(redlistCategory)] # Must be 0 rows
unique(best_evidence$class)
best_evidence[is.na(redlistCategory), class := "Mammals"]

# Hmmm. Well let's let Arian figure that out.
best_evidence[is.na(evidence_type), evidence_type := "No evidence found"]

best_evidence

unique(best_evidence$evidence_type)

unique(best_evidence$has_data)
unique(best_evidence$of_quality)
best_evidence[is.na(of_quality), of_quality := "no"]
best_evidence[is.na(has_data), has_data := "no"]

names(fill_pal)

best_evidence[grepl("Population", evidence_type), 
              synth_fill := paste(evidence_type, 
                                    ifelse(has_data == "yes", 
                                           "with data", "without data")) |> trimws()]
best_evidence[grepl("Population", evidence_type), 
              synth_fill := paste(synth_fill, 
                                    ifelse(of_quality == "yes",
                                           "and qualities", "")) |> trimws()]
setdiff(best_evidence$synth_fill, names(fill_pal))
best_evidence[is.na(synth_fill), synth_fill := evidence_type]
setdiff(best_evidence$synth_fill, names(fill_pal))

fill_pal2 <- c(`Predation in support` = "#a6d3a0", #`Predation not in support` = "grey40", 
               # `Population not in support without data` = "indianred4", `Population not in support with data` = "indianred", 
               # `Population not in support with data and qualities` = "indianred", 
               `Population in support without data` = "dodgerblue4", `Population in support with data` = "dodgerblue", 
               `Population in support with data and qualities` = "dodgerblue",
               `No evidence in support` = "grey40",
               `No evidence found` = "black")

unique(best_evidence$class)

# 
# best_evidence[class %in% c("Reptiles", "Amphibians"),
#                    class := "Reptiles & Amphibians"]
best_evidence.freq <- best_evidence[, .(n_species = uniqueN(scientificName)),
                                    by = .(synth_fill,
                                           of_quality, class)]
#
best_evidence.freq[duplicated(paste(synth_fill, class)),]

unique(best_evidence.freq$synth_fill)
best_evidence.freq$synth_fill <- factor(best_evidence.freq$synth_fill,
                                        levels = rev(c("No evidence found", "No evidence in support", 
                                                       "Predation in support",
                                                       "Population in support without data",
                                                       "Population in support with data",
                                                       "Population in support with data and qualities")))

unique(best_evidence.freq$class)
# best_evidence.freq[is.na(class) | class == "", class := "TBD"]

best_evidence.freq$class <- factor(best_evidence.freq$class,
                                   levels = (c("Birds", "Reptiles", "Mammals",  "Amphibians")))

best.p <- ggplot(data = best_evidence.freq,
       aes(x = class, y = n_species, fill = synth_fill,
           color = of_quality))+
  geom_col(position = position_stack(), width = .75)+
  scale_fill_manual("Best evidence",
                    values = fill_pal2)+
  scale_color_manual(values = c("no" = "transparent",
                                "yes" = "gold"))+
  ylab("Number of species")+
  xlab(NULL)+
  # coord_flip()+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid = element_blank())
best.p


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Final figure ------------------------------------------------------------
clean_lab <- theme(axis.text.x = element_blank(),
                   axis.ticks.x = element_blank())

(best.p + theme(legend.position = "none") | p.sys + theme(legend.position = "none"))  / 
  (p.abund | p.reprod) + plot_annotation(tag_levels = "A")

# (p.sys / p.class ) | (p1 + p2 + p3 + plot_layout(ncol = 1))

#
left <- (best.p + theme(legend.position = "none")) + 
           (p.sys + theme(legend.position = "none")) + 
           plot_spacer() + plot_layout(ncol = 1, heights = c(3/7, 3/7, 1/7))
right <- p.abund + p.reprod + plot_layout(ncol = 1, heights = c(5/8, 3/8))

left | (right) + plot_annotation(tag_levels = "A")

#
ggsave("figures/main_text/meta_review_raw.pdf", width = 8, height = 8)
# p1 + p2 + p3 + plot_layout(ncol = 1)

# p.class + p.mass.1 + p.mass.2 + plot_layout(ncol = 2, nrow = 2)

top <- best.p + theme(legend.position = "none") + 
        p.sys + theme(legend.position = "none") +
        plot_spacer() +
        plot_layout(nrow = 1, widths = c(3/7, 3/7, 1/7))


top / (p.abund | p.reprod) + plot_annotation(tag_levels = "A")
ggsave("figures/main_text/meta_review_raw.pdf", width = 9, height = 7)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------

# Maps ------------------------------------------------------
library("stringr")
# sys_rev$Study_lat
sys_rev <- fread("builds/systematic_review/systematic_review_tidy.csv")
sys_rev[Article_simple %in% dat$Article]
setdiff(dat$Article, sys_rev$Article_simple)
sys_rev[, in_meta := ifelse(Article_simple %in% dat$Article, "yes", "no")]
sys_rev[grepl("Does aerial baiting for controlling feral cats in a heterogeneous landscape confer benefits",
              Article_simple), in_meta := "yes"]
sys_rev[grepl("Austral Ecology 35.2 (2010)", Article_simple, ignore.case = T),]

sys_rev[grepl("Are dingoes a trophic regulator in arid Australia?", Article_simple, ignore.case = T),]
sys_rev[grepl("Are dingoes a trophic regulator in arid Australia?",
              Article_simple), in_meta := "yes"]
dat

#
sys_rev[grepl("Does a top‐predator provide an endangered rodent with refuge from an invasive mesopredator?", Article_simple, ignore.case = T),]
sys_rev[grepl("Does a top‐predator provide an endangered rodent with refuge from an invasive mesopredator?",
              Article_simple), in_meta := "yes"]

# 
# sys_rev[grepl("Gerber", Article_simple, ignore.case = T),]
# sys_rev[grepl("Does a top‐predator provide an endangered rodent with refuge from an invasive mesopredator?",
#               Article_simple), in_meta := "yes"]

#
sys_rev[is.na(as.numeric(Study_lat))]
sys_rev[, Study_lat := gsub("−", "-", Study_lat)]
sys_rev[, Study_long := gsub("−", "-", Study_long)]
sys_rev[is.na(as.numeric(Study_lat))]
sys_rev[is.na(as.numeric(Study_long))]$Study_long
sys_rev[, Study_long := str_trim(Study_long)]
grepl(" ", "137.164﻿")
sys_rev[, Study_long := gsub("﻿", "", Study_long)]
sys_rev[is.na(as.numeric(Study_long))]$Study_long

sys_rev[, `:=` (Study_lat = as.numeric(Study_lat),
                Study_long = as.numeric(Study_long))]
sys_rev

# >>> Plot ----------------------------------------------------

continents <- st_read("data/spatial/4a7d27e1-84a3-4d6a-b4c2-6b6919f3cf4b202034-1-2zg7ul.ht5ut.shp")

unique(sys_rev$Evidence_category)
sys_rev[Evidence_category == "Control program", Evidence_category := "Population"]
#
sys_rev
sys_rev[, synth_fill := fcase(grepl("Population", evidence_type) & has_data == "yes", paste(evidence_type, "with data"),
                                grepl("Population", evidence_type) & has_data == "no", paste(evidence_type, "without data"))]

sys_rev[is.na(synth_fill), synth_fill := evidence_type]
sys_rev[of_quality == "yes",]
sys_rev[of_quality == "yes", synth_fill := paste(synth_fill, "and qualities")]

unique(sys_rev$synth_fill)

fill_pal <- c(`Predation in support` = "#a6d3a0", `Predation not in support` = "#40531b", 
              `Population not in support without data` = "indianred4", `Population not in support with data` = "indianred", 
              `Population not in support with data and qualities` = "indianred", 
              `Population in support without data` = "dodgerblue4", `Population in support with data` = "dodgerblue",
              `Population in support with data and qualities` = "dodgerblue"
)
setdiff(unique(sys_rev$synth_fill), names(fill_pal))
setdiff(names(fill_pal), unique(sys_rev$synth_fill))

#
sys.map <- ggplot()+
  geom_sf(data = continents, aes(geometry = geometry))+
  geom_jitter(data = sys_rev[!is.na(Study_long)], 
             aes(x = Study_long, y = Study_lat,
                 fill = synth_fill, color = of_quality,
                 size = in_meta),
             shape = 21, stroke = 1,
             inherit.aes = F)+
  scale_size_manual(name = "In meta-analysis",
                    values = c("yes" = 4,
                               "no" = 1))+
  scale_color_manual(name = "With qualities",
                     values = c("no" = "transparent",
                                "yes" = "gold"))+
  scale_fill_manual(name = NULL,
                    values = fill_pal)+
  guides(fill = "none", color = "none")+
  theme_bw()+
  theme(panel.border = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())
sys.map

