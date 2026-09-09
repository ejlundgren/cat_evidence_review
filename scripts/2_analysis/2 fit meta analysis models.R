# Last reviewed on July 31st, 2026
# 
#
# Conduct meta-analysis and tabulate systematic review figures
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


createPhyloCorr <- function(spp_list, tree){
  
  tree.filt <- keep.tip(tree, spp_list)
  tree.br <- compute.brlen(tree.filt)
  tree.corr <- vcv(tree.br, corr=T)
  return(tree.corr)
}

# Load meta-analysis data ---------------------------------------------------------------

dat <- fread("builds/meta_analysis/analysis_ready_dataset.csv")

dat[, .(n = .N), by = .(Order_final)]
# Not enough data.

dat[, .(n = .N), by = .(class)]

unique(dat$log_mass)

dat[, article_id := paste(word(Article, 1, sep = "[[:space:][:punct:]]"), .GRP), by = .(Article)]
unique(dat[, .(Article, article_id)])
length(unique(dat$Article)) == length(unique(dat$article_id))

nrow(dat[duplicated(article_id)])
nrow(dat[duplicated(Article)])
# Must be equal. Good

unique(dat[, .(analysis_group, analysis_effect_size)])

unique(dat[, .(analysis_effect_size, analysis_group)])
unique(dat$Effect_size_ID)
dat[Effect_size_ID %in% c("ES_47a", "ES_47b")]
#
dat[Effect_size_ID %in% c("ES_38a")]$analysis_group
dat

#
dat[Effect_size_ID == "ES_43"]$analysis_group
dat[Effect_size_ID == "ES_37"]$analysis_group

dat[, .(n = .N), by = .(Effect_size_ID, analysis_group)]
dat[Effect_size_ID == "ES_13", ]

dat[, .(n = .N), by = .(Effect_size_ID, analysis_group)][n > 1] # must be 0 rows

# >>> Add column for whether effect size type is dominant within analysis group --------
dat[, total_n_articles := uniqueN(article_id), by = .(analysis_group)]
dat[, n_articles_per_original_es := uniqueN(article_id), by = .(analysis_group,
                                                                original_effect_size)]
dat[, dominant_effect_size := ifelse((n_articles_per_original_es / total_n_articles) > .5,
                                     "yes", "no")]
dat[dominant_effect_size == "no", ]

# >>> Make data long by habitat traits ------------------------------------
# Instead of having these as factors, let's do univariate subgroup models
dat.long <- melt(dat,
                 measure.vars = c("locomotion_volant",
                                  "ground_or_burrow_resting_or_nesting",
                                  "Foraging_habitat_ground", "continent_island"),
                 variable.name = "habitat_category",
                 value.name = "habitat_trait")
dat.long

dat[, habitat_trait := "All"]
dat[, habitat_category := "All"]

dat.final <- rbind(dat[, !c("locomotion_volant",
                            "ground_or_burrow_resting_or_nesting",
                            "Foraging_habitat_ground", "continent_island"),
                       with = F], 
                   dat.long)
dat.final

dat.final

# Now let's format these names and concatenate with variable type.
unique(dat.final[, .(habitat_category, habitat_trait)])
dat.final[habitat_trait != "All", habitat_trait := paste0(habitat_category, ".", habitat_trait)]
dput(unique(dat.final$habitat_trait))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -----------------------------------------
# Set up model guide -----------------------------------------------------------------


# >>> Cross join guide ----------------------------------------------------
unique(dat.final[analysis_group == "Abundance before-after eradication"]$Effect_size_ID)

# 
# # Do the guide in 2 steps. We don't have enough data for class * habitat sub-analyses.
guide <- CJ(analysis_group = unique(dat.final$analysis_group),
            moderator = c("1", "log_mass"),
            class = c("Birds", "Mammals", "Reptiles", "All"),
            habitat_trait = "All",
            prey_range = c("All", "inside"),
            non_phylo_species = c("yes", "no"),
            phylo_species = c("yes", "no"),
            only_dominant_effect_size = c("yes", "no")
)

guide <- guide[!(moderator == "log_mass" & class != "All"), ]
unique(guide[class != "All", ]$moderator)
#' [Can't do log_mass for class subgroups]
unique(guide[moderator == "log_mass", ]$class)

guide2 <- CJ(analysis_group = unique(dat.final$analysis_group),
             moderator = "1",
             habitat_trait = c("locomotion_volant.non_volant", "locomotion_volant.volant", 
                               "ground_or_burrow_resting_or_nesting.other", "ground_or_burrow_resting_or_nesting.ground", 
                               "Foraging_habitat_ground.other", "Foraging_habitat_ground.ground", 
                               "continent_island.island", "continent_island.mainland"),
             class = c("All"),
             prey_range = c("All"),
             non_phylo_species = c("yes", "no"),
             phylo_species = c("yes", "no"),
             only_dominant_effect_size = c("no")
)

guide <- rbind(guide, guide2)

# Add a model comparison ID
guide[, model_comparison_id := paste0("model_comp_", .GRP), 
      by = .(analysis_group, class, moderator, prey_range,
             habitat_trait, only_dominant_effect_size)]
guide
guide[, random_effect := "list(~1|article_id/Effect_size_ID"]

guide[, random_effect := ifelse(non_phylo_species == "yes",
                                paste0(random_effect, ", ~1|scientificName"), 
                                paste0(random_effect))]
guide[, random_effect := ifelse(phylo_species == "yes",
                                paste0(random_effect, ", ~1|phylo_species)"),
                                paste0(random_effect, ")"))]
guide


# Formulate exclusion formula:
guide[, exclusion := paste0("analysis_group == '", analysis_group, "'",
                            " & habitat_trait == '", habitat_trait, "'")]
guide[class != "All", exclusion := paste0(exclusion, " & class %in% '", class, "'")]
guide[class != "All",]
guide[prey_range == "inside", exclusion := paste0(exclusion, " & prey_range_primary == 'inside'")]
guide[prey_range == "All"]

#
guide.m1 <- merge(guide,
                  unique(dat[, .(analysis_group, analysis_effect_size)]),
                  by = 'analysis_group',
                  all.x = T)
guide.m1[is.na(analysis_effect_size)]
guide.m1

#
guide.m1[only_dominant_effect_size == "yes", exclusion := paste0(exclusion, " & dominant_effect_size == 'yes'")]

#
guide.m1[, formula := ifelse(moderator == "1",
                             "~ 1",
                             paste("~", moderator))]

guide.m1

unique(guide.m1$formula)

guide.m1[habitat_trait != "All", ]

# >>> Add model ID --------------------------------------------------------

guide.m1[, model_id := paste0("model_", seq(1:.N))]

dat$class

guide.m1[class == "All"]$exclusion
guide.m1[prey_range == "inside"]$exclusion
guide.m1 <- guide.m1[!(prey_range == "inside" & only_dominant_effect_size == "yes"), ]
#' [Insufficient data for these models]

# >>> Get overall sample sizes ----------------------------------------------------
Ns <- list()
sub.dat <- c()
i <- 1

for(i in 1:nrow(guide.m1)){
  sub.dat <- dat.final[eval(parse(text = guide.m1[i, ]$exclusion))]
  
  Ns[[i]] <- sub.dat[, .(n_species = uniqueN(scientificName),
                         n_articles = uniqueN(Article),
                         n_obs = .N,
                         model_id = guide[i, ]$model_id,
                         analysis_group = guide[i, ]$analysis_group,
                         class = guide[i, ]$class)]
  Ns[[i]]$model_id <- guide.m1[i, ]$model_id
  
}
Ns <- rbindlist(Ns)
Ns

guide.m2 <- merge(guide.m1, 
                  Ns[, .(n_species, n_articles, n_obs, model_id)], 
                  by = "model_id")
guide.m2[n_articles == 0, ]

# at least 3 articles for intercept only models
guide.m2 <- guide.m2[n_articles > 0, ]
guide.m2 # Keep all analysis groups for which there are data for intercept-only models

# More than 2 observations:
guide.m2 <- guide.m2[n_obs >= 2, ]
guide.m2

# and at least 5 for continuous
guide.m2 <- guide.m2[!(moderator == "log_mass" & n_articles < 5), ]
guide.m2

# If only 1 article, drop Article from random effects
guide.m2[n_articles == 1, random_effect := gsub("article_id/", "", random_effect)]
guide.m2

# >>> Download phylogeny ---------------------------------------------
dat.final[, spp_name_corrected := scientificName]
dat.final[scientificName == "Pampusana erythroptera",
          spp_name_corrected := "Gallicolumba erythroptera"]
nms <- unique(dat.final$spp_name_corrected)

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


setDT(nms_res)
nms_res[, label := paste0(gsub(" ", "_", unique_name),
                          "_ott", ott_id)]
nms_res

sort(tree$tip.label)
setdiff(nms_res$label, tree$tip.label)
setdiff(tree$tip.label, nms_res$label)

nms_res[, search_string := str_to_sentence(search_string)]
unique(nms_res$search_string)
setdiff(nms_res$search_string, dat$spp_name_corrected)
setdiff(dat.final$spp_name_corrected, nms_res$search_string)

dat.final.m <- merge(dat.final,
                     nms_res[, .(search_string, label)],
                     by.x = "spp_name_corrected",
                     by.y = "search_string",
                     all.x = T)
nrow(dat.final.m) == nrow(dat.final)
setnames(dat.final.m, "label", "phylo_species")

dat.final.m[is.na(class)]

dat.final.m

# >>> Set up prediction grids ---------------------------------------------

unique(guide.m2$moderator)
grids <- list()
grids[["log_mass"]] <- data.table(log_mass = seq(from = min(dat.final.m$log_mass, na.rm = T),
                                                 to = max(dat.final.m$log_mass, na.rm = T),
                                                 by = .1))
grids

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Run models --------------------------------------------------------------

guide <- copy(guide.m2)
dat <- copy(dat.final.m)
rm(guide.m2, guide.m1, dat.final.m)

models <- list()
predictions <- list()
tidy_models <- list()

for(i in 1:nrow(guide)){
  
  sub_guide <- guide[i, ]
  sub_guide
  dat.sub <- dat[eval(parse(text = sub_guide$exclusion))]
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
      predictions[[i]] <- rma_predictions(models[[i]], 
                                          grids[[sub_guide$moderator]]) |>
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

# >>> Save model lists ----------------------------------------------------

saveRDS(tidy_models, "builds/meta_analysis/models/tidy_models.Rds")
saveRDS(predictions, "builds/meta_analysis/models/predictions.Rds")
saveRDS(dat, "builds/meta_analysis/models/analysis_data.Rds")
