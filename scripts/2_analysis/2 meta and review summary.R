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
guide.m2 <- guide.m2[n_obs > 2, ]
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

length(unique(predictions$model_id))
length(unique(tidy_models$model_id))

predictions

predictions[, string := paste0(analysis_effect_size, ": ", n_articles, "(", n_species, ", ", n_obs, ")")]
predictions

dat[, analysis_group_lab := gsub("Abundance ", "", analysis_group)]
dat[, analysis_group_lab := gsub("Reproduction ", "", analysis_group_lab)]
dat[, analysis_group_lab := str_to_sentence(analysis_group_lab)]


predictions[, analysis_group_lab := gsub("Abundance ", "", analysis_group)]
predictions[, analysis_group_lab := gsub("Reproduction ", "", analysis_group_lab)]
predictions[, analysis_group_lab := str_to_sentence(analysis_group_lab)]
predictions

dput(sort(unique(dat$analysis_group_lab)))
lvls <- c("With/without cats", 
          "Before-after eradication", "Inside/outside exclosure", "On islands with/without cats", 
          "Spatial association", "Temporal association")
dat$analysis_group_lab <- factor(dat$analysis_group_lab,
                                      levels = rev(lvls))
predictions$analysis_group_lab <- factor(predictions$analysis_group_lab,
                                        levels = rev(lvls))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Plot -----------------------------------------------------------------


# > Overall effects across all subgroup -----------------------------------------------------

intercepts <- predictions[moderator == "1" & 
                            only_dominant_effect_size == "no" &
                            class == "All" &
                            habitat_trait == "All" &
                            prey_range == "All"]
intercepts
unique(intercepts$analysis_group)

dat.plot <- dat[analysis_group %in% unique(intercepts$analysis_group) &
                  habitat_trait == "All", ] # Drop analysis groups for which models didn't converge.
intercepts

# >>> Effect size conversion table ----------------------------------------

intercepts
original_es <- dat.plot[, .(original_effect_size = paste(unique(original_effect_size),
                                                         collapse = "; ")),
                        by = .(analysis_group, analysis_effect_size)]
original_es

tab <- merge(intercepts[, .(analysis_group, n_articles, n_obs)],
             original_es, 
             by = "analysis_group")
tab
fwrite(tab, "builds/meta_analysis/sample_size_table.csv")

dat.plot[Effect_size_ID == "ES_49"]
unique(dat.plot$analysis_group)

dat[!Effect_size_ID %in% dat.plot$Effect_size_ID]$Effect_size_ID

# >>> Main text figures----------------------

intercepts$analysis_effect_size

p.abund <- ggplot()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = intercepts[grepl("abundance", analysis_group, ignore.case = T)],
            aes(y = analysis_group_lab,
                x = -6, vjust = 2,
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
  xlab("Association between cats and\nthreatened species abundance")+
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
  xlab("Association between cats and\nthreatened bird reproduction")+
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

# >>> Inside prey range ---------------------------------------------------

intercept_inside <- predictions[moderator == "1" & 
                                  habitat_trait == "All" &
                                  only_dominant_effect_size == "no" &
                                   class == "All" &
                                  prey_range == "inside", ]

dat.prey <- dat[analysis_group %in% unique(intercept_inside$analysis_group) &
                  habitat_trait == "All", ] 
dat.prey <- dat.plot[prey_range_primary == "inside"]

intercept_inside$analysis_effect_size

p.abund.inside <- ggplot()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = intercept_inside[grepl("abundance", analysis_group, ignore.case = T)],
            aes(y = analysis_group_lab,
                x = -6, vjust = 2,
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
  # coord_cartesian(xlim = c(-8, 8))+
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
  # coord_cartesian(xlim = c(-6, 6))+
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
                            habitat_trait == "All" &
                            only_dominant_effect_size == "no" &
                            prey_range == "All"]
unique(class_pred$class)
dat.plot <- dat[grepl("abundance", analysis_group, ignore.case = T) &
                  habitat_trait == "All" &
                   analysis_group %in% class_pred$analysis_group & 
                   class %in% class_pred$class, ]

class.abundance <- ggplot()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = class_pred[grepl("abundance", analysis_group, ignore.case = T)],
            aes(y = analysis_group_lab,
                x = -15, vjust = 2,
                label = string),
            color = "grey50",
            size = 3)+
  geom_jitter(data = dat.plot, 
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
  facet_wrap(~class)+
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

# >>> log_mass ------------------------------------------------------------
tidy_models[moderator == "log_mass"  & only_dominant_effect_size == "no", ]

mass <- predictions[moderator == "log_mass"  & 
                      only_dominant_effect_size == "no" &
                      habitat_trait == "All" &
                      class == "All" & 
                      prey_range == "All", ]
unique(mass$analysis_group_lab)
#
# Back-transform mass.
mass[, mass_g := 10^as.numeric(term)]

mass$analysis_group_lab <- factor(mass$analysis_group_lab ,
                                  levels = (c("With/without cats",
                                              "On islands with/without cats",
                                              "Temporal association")))

dat.plot <- dat[grepl("abundance", analysis_group, ignore.case = T) &
                    analysis_group %in% mass$analysis_group &
                  habitat_trait == "All"]

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
  geom_jitter(data = dat.plot, 
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
predictions[habitat_trait != "All", ]

habitat_pred <- predictions[habitat_trait != "All" & 
                              class == "All" &
                              prey_range == "All" &
                              only_dominant_effect_size == "no"
                            ]
unique(habitat_pred$analysis_group)

dat.plot <- dat[habitat_trait != "All" &
                  analysis_group %in% habitat_pred$analysis_group]

# Drop levels for which models didn't converge:
habitat_pred[, key := paste(analysis_group, habitat_trait)]
dat.plot[, key := paste(analysis_group, habitat_trait)]
dat.plot <- dat.plot[key %in% habitat_pred$key]

# Need to format habitat trait and category in dat and predictions
unique(dat.plot$habitat_trait)
unique(dat.plot$habitat_category)
dat.plot[, habitat_category := fcase(habitat_category == "locomotion_volant", "Locomotion",
                                     habitat_category == "ground_or_burrow_resting_or_nesting", "Ground-nesting",
                                     habitat_category == "Foraging_habitat_ground", "Ground-foraging",
                                     habitat_category == "continent_island", "Landform")]
dat.plot[, habitat_fac_lvl := tstrsplit(habitat_trait, "[.]")[2]]
dat.plot[, habitat_fac_lvl := gsub("_", "-", habitat_fac_lvl)]
dat.plot[, habitat_fac_lvl := str_to_sentence(habitat_fac_lvl)]
dat.plot$habitat_category <- factor(dat.plot$habitat_category,
                                    levels = c("Ground-nesting", "Ground-foraging",
                                               "Locomotion", "Landform"))

habitat_pred.mrg <- merge(habitat_pred,
                      unique(dat.plot[, .(habitat_trait, 
                                          habitat_category,
                                          habitat_fac_lvl)]),
                      by = "habitat_trait",
                      all.x = T,
                      all.y = F)
nrow(habitat_pred.mrg) == nrow(habitat_pred) # Must be TRUE
unique(habitat_pred.mrg[, .(habitat_trait, habitat_category, habitat_fac_lvl)])
unique(habitat_pred.mrg$analysis_group_lab)

# Tricky plotting. Let's do ground-foraging, ground nesting together (same factor levels)

habitat.abund.1 <- ggplot()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = habitat_pred.mrg[grepl("abundance", 
                                      analysis_group, ignore.case = T) &
                                      habitat_category %in% c("Ground-foraging", "Ground-nesting")],
            aes(y = analysis_group_lab,
                x = -10, vjust = 1.5,
                label = string, group = habitat_fac_lvl,
                color = habitat_fac_lvl),
            position = position_dodgev(height = .5),
            size = 2.5)+
  geom_jitter(data = dat.plot[grepl("abundance", analysis_group, ignore.case = T) &
                                habitat_category %in% c("Ground-foraging", "Ground-nesting"), ],
              aes(x = yi, 
                  y = analysis_group_lab,
                  group = habitat_fac_lvl, fill = habitat_fac_lvl,
                  size = 1/vi_analysis), 
              position = position_jitterdodgev(jitter.height = .1,
                                               jitter.width = 0,
                                               dodge.height = .5
                                               ),
              shape = 21, 
              alpha = .6)+
  geom_errorbar(data = habitat_pred.mrg[grepl("abundance", analysis_group, ignore.case = T) &
                                          habitat_category %in% c("Ground-foraging", "Ground-nesting")], 
                aes(y = analysis_group_lab, group = habitat_fac_lvl,
                    # color = term,
                    xmin = lower_ci, xmax = upper_ci),
                position = position_dodgev(height = .5),
                width = .25)+
  geom_pointrange(data = habitat_pred.mrg[grepl("abundance", analysis_group, ignore.case = T) &
                                            habitat_category %in% c("Ground-foraging", "Ground-nesting")], 
                  aes(x = pred, y = analysis_group_lab,
                      group = habitat_fac_lvl, fill = habitat_fac_lvl,
                      xmin = lower_pi, xmax = upper_pi),
                  position = position_dodgev(height = .5),
                  shape = 21,
                  # fill = "grey50",
                  size = 1)+
  facet_wrap(~habitat_category)+
  scale_y_discrete(breaks = c("With/without cats", 
                              "Before-after eradication", "Inside/outside exclosure", 
                              "On islands with/without cats", 
                              "Spatial association", "Temporal association"),
                   labels = c("With-without cats",
                              "Before-after eradication", "Inside-outside exclosure", 
                              "Islands with-without cats", 
                              "Spatial association", "Temporal association"))+
  scale_color_manual(name = NULL,
                     values = c("Ground" = "#565554",
                                 "Other" = "#2E86AB"))+
  scale_fill_manual(name = NULL,
                    values = c("Ground" = "#565554",
                               "Other" = "#2E86AB"))+
  ylab(NULL)+
  xlab("Association between cats and\nthreatened species")+
  ggtitle("Abundance")+
  guides(size = "none", color = "none",
         fill = guide_legend(reverse = TRUE))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
habitat.abund.1

unique(habitat_pred.mrg[grepl("abundance", 
                              analysis_group, ignore.case = T) &
                          !habitat_category %in% c("Ground-foraging", "Ground-nesting")]$habitat_fac_lvl)

habitat.abund.2 <- ggplot()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = habitat_pred.mrg[grepl("abundance", 
                                          analysis_group, ignore.case = T) &
                                      !habitat_category %in% c("Ground-foraging", "Ground-nesting")],
            aes(y = analysis_group_lab,
                x = -7, vjust = 2.5,
                label = string, group = habitat_fac_lvl,
                color = habitat_fac_lvl),
            position = position_dodgev(height = .5),
            size = 2.5)+
  geom_jitter(data = dat.plot[grepl("abundance", analysis_group, ignore.case = T) &
                                !habitat_category %in% c("Ground-foraging", "Ground-nesting"), ],
              aes(x = yi, 
                  y = analysis_group_lab,
                  group = habitat_fac_lvl, fill = habitat_fac_lvl,
                  size = 1/vi_analysis), 
              position = position_jitterdodgev(jitter.height = .1,
                                               jitter.width = 0,
                                               dodge.height = .5
              ),
              shape = 21, 
              alpha = .6)+
  geom_errorbar(data = habitat_pred.mrg[grepl("abundance", analysis_group, ignore.case = T) &
                                          !habitat_category %in% c("Ground-foraging", "Ground-nesting")], 
                aes(y = analysis_group_lab, group = habitat_fac_lvl,
                    # color = term,
                    xmin = lower_ci, xmax = upper_ci),
                position = position_dodgev(height = .5),
                width = .25)+
  geom_pointrange(data = habitat_pred.mrg[grepl("abundance", analysis_group, ignore.case = T) &
                                            !habitat_category %in% c("Ground-foraging", "Ground-nesting")], 
                  aes(x = pred, y = analysis_group_lab,
                      group = habitat_fac_lvl, fill = habitat_fac_lvl,
                      xmin = lower_pi, xmax = upper_pi),
                  position = position_dodgev(height = .5),
                  shape = 21,
                  # fill = "grey50",
                  size = 1)+
  facet_wrap(~habitat_category)+
  scale_y_discrete(breaks = c("With/without cats", 
                              "Before-after eradication", "Inside/outside exclosure", 
                              "On islands with/without cats", 
                              "Spatial association", "Temporal association"),
                   labels = c("With-without cats",
                              "Before-after eradication", "Inside-outside exclosure", 
                              "Islands with-without cats", 
                              "Spatial association", "Temporal association"))+
  scale_color_manual(name = NULL,
                     values = c("Non-volant" = "#565554",
                                "Volant" = "#2E86AB",
                                "Island" = "#F7B538",
                                "Mainland" = "#F24236"),
                     labels = c("Non-volant" = "Flightless",
                                "Volant" = "Volant",
                                "Island" = "Island",
                                "Mainland" = "Mainland (Australia)"))+
  scale_fill_manual(name = NULL,
                    values = c("Non-volant" = "#565554",
                               "Volant" = "#2E86AB",
                               "Island" = "#F7B538",
                               "Mainland" = "#F24236"),
                    labels = c("Non-volant" = "Flightless",
                               "Volant" = "Volant",
                               "Island" = "Island",
                               "Mainland" = "Mainland (Australia)"))+
  ylab(NULL)+
  xlab("Association between cats and\nthreatened species")+
  guides(size = "none", color = "none",
         fill = guide_legend(reverse = TRUE))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
habitat.abund.2

habitat.final <- habitat.abund.1 + xlab(NULL) +
  habitat.abund.2 +
  plot_layout(nrow = 2)
habitat.final


# >>> Only dominant effect sizes --------------------------------------------------------------------

tidy_models[moderator == "1" & only_dominant_effect_size == "yes", ]

intercepts <- predictions[moderator == "1" & 
                            only_dominant_effect_size == "yes" &
                            habitat_trait == "All" &
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
       width = 9, height = 9)
ggsave("figures/SI/meta effects by habitat.png",
       width = 10, height = 10)

p.inside # Only species inside prey range
ggsave("figures/SI/meta effects inside prey range only.pdf",
       width = 9, height = 9)
ggsave("figures/SI/meta effects inside prey range only.png",
       width = 9, height = 9)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# SI Tables ---------------------------------------------------------------

si_models <- copy(tidy_models)
si_models$term
unique(si_models$analysis_group)

# General formatting
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

si_models[, `Sample size` := paste0("$N_{articles}=", n_articles, "$<br>",
                                     "$N_{species}=", n_species, "$<br>",
                                     "$N_{observations}=", n_obs, "$")]

si_models$analysis_group

si_models[class == "All", class := "All classes"]
si_models$class <- factor(si_models$class,
                          levels = rev(c("All classes", "Mammals", "Birds")))
si_models

unique(si_models$moderator)

#
unique(si_models$analysis_group)

si_models[,moderator := ifelse(moderator == "1", "Overall estimate", moderator)]
si_models[, term := ifelse(term == "log_mass",  "Mass (g, log10)", term)]
si_models[, moderator := ifelse(moderator == "log_mass",  "Mass", moderator)]

unique(si_models$term)
unique(si_models$habitat_trait)
si_models[, habitat_trait := gsub("[.]", " ", habitat_trait)]
si_models[, habitat_trait := gsub("Foraging_habitat_ground", "Foraging habitat:", habitat_trait)]
si_models[, habitat_trait := gsub("locomotion_volant", "Locomotion:", habitat_trait)]
si_models[, habitat_trait := gsub("continent_island", "Landform:", habitat_trait)]
si_models[, habitat_trait := gsub("ground_or_burrow_resting_or_nesting", "Ground nesting:", habitat_trait)]
unique(si_models$habitat_trait)
si_models[, habitat_trait := gsub("non_volant", "Non-volant", habitat_trait)]

unique(si_models$analysis_group)
unique(si_models$habitat_trait)

si_models[habitat_trait == "All", habitat_trait := "All habitats"]
si_models[, group := paste(analysis_group, class, habitat_trait, analysis_effect_size, sep = " | ")]
unique(si_models$group)

# >>> Main text models ----------------------------------------------------
unique(si_models$moderator)
sub_models <- si_models[class == "All classes" &
                          moderator == "Overall estimate" &
                          only_dominant_effect_size == "no" &
                          prey_range == "All" &
                          habitat_trait == "All habitats"]

sub_models[, group := paste(analysis_group, analysis_effect_size, sep = " | ")]
dput(unique(sub_models$group))

lvls <- c("Abundance with/without cats | SMD", 
         "Abundance before-after eradication | SMD", 
          "Abundance on islands with/without cats | lnOR", 
          "Abundance spatial association | Zr", 
          "Abundance temporal association | Zr", 
          "Reproduction with/without cats | lnOR", 
          "Reproduction before-after eradication | SMD",
          "Reproduction temporal association | Zr"
)

setdiff(sub_models$group, lvls)

sub_models$group <- factor(sub_models$group,
                          levels = lvls)

# Also should sort by Intercept vs moderator
unique(sub_models$term)
sub_models$term <- factor(sub_models$term,
                         levels = c("overall", "intercept", "Mass (g, log10)"))
sub_models[, num1 := as.numeric(group)]
sub_models[, num2 := as.numeric(term)]

setorder(sub_models, num1, num2)
sub_models

#
main_text <- sub_models %>%
  select(group, term, Estimate, 
         `Test statistics`, `Sample size`, 
         `Residual heterogeneity`) %>%
  rename("Term" = "term") %>%
  group_by(group) %>%
  # arrange(group) %>%
  gt() %>%
  fmt_markdown(columns = `Residual heterogeneity`) %>%
  fmt_markdown(columns = `Sample size`) %>%
  fmt_markdown(columns = `Test statistics`) %>%
  tab_header(#title = ,
    md("**Table S6**. Model summaries for intercept-only main-text models across all species. Model estimates ± 95% CIs and test statistics are reported along with sample sizes and residual unexplained heterogeneity ($I^2$), decomposed by hierarchical model levels (article and observation). The effect size used is given in the heading of each model (SMD=standardized mean difference or Hedges' g; Zr=correlation coefficient; lnOR=log odds ratio).")) |>
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
gtsave(main_text, filename = "figures/SI_Tables/main text model table.pdf")

# fmt_markdown(columns = `Sample size`) %>%

# >>> Body mass ----------------------------------------------------
unique(si_models$moderator)
sub_models <- si_models[class == "All classes" &
                          moderator == "Mass" &
                          only_dominant_effect_size == "no" &
                          prey_range == "All" &
                          habitat_trait == "All habitats"]
nrow(sub_models)
sub_models

# Need to do the group differently here:
sub_models[, `Sample size` := gsub("<br>", "; ", `Sample size`)]
sub_models[, `Residual heterogeneity` := gsub("<br>", "; ", `Residual heterogeneity`)]

sub_models[, group := paste0(analysis_group," | ", analysis_effect_size, "<br>",
                            `Sample size`, "<br>",
                            `Residual heterogeneity`)]

unique(sub_models$group)
dput(unique(sub_models$group))

lvls <- c("Abundance with/without cats | SMD<br>$N_{articles}=10$; $N_{species}=13$; $N_{observations}=15$<br>$I^2_{total}=54.2$; $I^2_{article}=54.2$; $I^2_{obs}=0$",
          "Abundance on islands with/without cats | lnOR<br>$N_{articles}=7$; $N_{species}=11$; $N_{observations}=11$<br>$I^2_{total}=42.1$; $I^2_{article}=42.1$; $I^2_{obs}=0$", 
          "Abundance temporal association | Zr<br>$N_{articles}=11$; $N_{species}=13$; $N_{observations}=19$<br>$I^2_{total}=86.4$; $I^2_{article}=62.3$; $I^2_{obs}=24$"
)
setdiff(sub_models$group, lvls)

sub_models$group <- factor(sub_models$group,
                           levels = lvls)

# Also should sort by Intercept vs moderator
unique(sub_models$term)
sub_models$term <- factor(sub_models$term,
                          levels = c("overall", "intercept", "Mass (g, log10)"))
sub_models[, num1 := as.numeric(group)]
sub_models[, num2 := as.numeric(term)]

setorder(sub_models, num1, num2)
sub_models
#
#
body_mass <- sub_models %>%
  select(group, term, Estimate, 
         `Test statistics`) %>%
  rename("Term" = "term") %>%
  group_by(group) %>%
  # arrange(group) %>%
  gt() %>%
  # fmt_markdown(columns = `Residual heterogeneity`) %>%
  # fmt_markdown(columns = `Sample size`) %>%
  fmt_markdown(columns = `Test statistics`) %>%
  tab_header(#title = ,
    md("**Table S9**. Model summaries for the relationship between body mass and cat effects on prey abundance. Model estimates ± 95% CIs and test statistics are reported along with sample sizes and residual unexplained heterogeneity ($I^2$), decomposed by hierarchical model levels (article and observation). The effect size used is given in the heading of each model (SMD=standardized mean difference or Hedges' g; Zr=correlation coefficient; lnOR=log odds ratio).")) |>
  opt_align_table_header(align = c("left")) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()) %>%
  text_transform(
    locations = cells_row_groups(),
    fn = function(x) {
      # Loop through each group name and wrap it in your desired Markdown structure
      lapply(x, function(group_name) {
        md( group_name )
      })
    }
  ) |>
  opt_table_font(
    size = 12
  ) |> 
  tab_options(
    latex.use_longtable = TRUE,
    latex.header_repeat = FALSE
  )
body_mass

gtsave(body_mass, filename = "figures/SI_Tables/body mass model table.pdf")

# fmt_markdown(columns = `Sample size`) %>%
# >>> Inside prey range -------------------------------------------------------
unique(si_models$moderator)
sub_models <- si_models[class == "All classes" &
                          moderator == "Overall estimate" &
                          only_dominant_effect_size == "no" &
                          prey_range == "inside" &
                          habitat_trait == "All habitats"]

sub_models[, group := paste(analysis_group, analysis_effect_size, sep = " | ")]

dput(unique(sub_models$group))

lvls <- c("Abundance with/without cats | SMD", 
          "Abundance before-after eradication | SMD",
          "Abundance on islands with/without cats | lnOR", 
          "Abundance spatial association | Zr", 
          "Abundance temporal association | Zr", 
          "Reproduction with/without cats | lnOR", 
          "Reproduction before-after eradication | SMD", 
          "Reproduction temporal association | Zr"
)

setdiff(sub_models$group, lvls)

sub_models$group <- factor(sub_models$group,
                           levels = lvls)

# Also should sort by Intercept vs moderator
unique(sub_models$term)
sub_models$term <- factor(sub_models$term,
                          levels = c("overall", "intercept", "Mass (g, log10)"))
sub_models[, num1 := as.numeric(group)]
sub_models[, num2 := as.numeric(term)]

setorder(sub_models, num1, num2)
sub_models

#
inside_prey_range <- sub_models %>%
  select(group, term, Estimate, 
         `Test statistics`, `Sample size`, 
         `Residual heterogeneity`) %>%
  rename("Term" = "term") %>%
  group_by(group) %>%
  gt() %>%
  fmt_markdown(columns = `Residual heterogeneity`) %>%
  fmt_markdown(columns = `Sample size`) %>%
  fmt_markdown(columns = `Test statistics`) %>%
  tab_header(#title = ,
    md("**Table S8**. Model summaries for intercept-only models for species within cat prey range. Model estimates ± 95% CIs and test statistics are reported along with sample sizes and residual unexplained heterogeneity ($I^2$), decomposed by hierarchical model levels (article and observation). The effect size used is given in the heading of each model (SMD=standardized mean difference or Hedges' g; Zr=correlation coefficient; lnOR=log odds ratio).")) |>
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
inside_prey_range
gtsave(inside_prey_range, filename = "figures/SI_Tables/inside prey range.pdf")

# fmt_markdown(columns = `Sample size`) %>%

# >>> Only dominant -------------------------------------------------------
unique(si_models$only_dominant_effect_size)
unique(si_models$moderator)
unique(si_models$prey_range)

sub_models <- si_models[class == "All classes" &
                          moderator == "Overall estimate" &
                          only_dominant_effect_size == "yes" &
                          prey_range == "All" &
                          habitat_trait == "All habitats"
                          ]
sub_models[, group := paste(analysis_group, analysis_effect_size, sep = " | ")]
dput(unique(sub_models$group))

lvls <- c("Abundance on islands with/without cats | lnOR", 
          "Abundance spatial association | Zr", 
          "Abundance temporal association | Zr",
          "Reproduction temporal association | Zr", 
          "Reproduction with/without cats | lnOR")

setdiff(sub_models$group, lvls)

sub_models$group <- factor(sub_models$group,
                           levels = lvls)

# Also should sort by Intercept vs moderator
unique(sub_models$term)
sub_models$term <- factor(sub_models$term,
                          levels = c("overall", "intercept", "Mass (g, log10)"))
sub_models[, num1 := as.numeric(group)]
sub_models[, num2 := as.numeric(term)]

setorder(sub_models, num1, num2)
sub_models

#
only_dominant <- sub_models %>%
  select(group, term, Estimate, 
         moderator,
         `Test statistics`, `Sample size`, 
         `Residual heterogeneity`) %>%
  rename("Term" = "term",
         "Moderator" = "moderator") %>%
  group_by(group) %>%
  gt() %>%
  fmt_markdown(columns = `Residual heterogeneity`) %>%
  fmt_markdown(columns = `Moderator`) %>%
  fmt_markdown(columns = `Sample size`) %>%
  fmt_markdown(columns = `Test statistics`) %>%
  tab_header(#title = ,
    md("**Table S7**. Model summaries for intercept-only models using only the dominant effect size (e.g., excluding minority converted effect sizes). Model estimates ± 95% CIs and test statistics are reported along with sample sizes and residual unexplained heterogeneity ($I^2$), decomposed by hierarchical model levels (article and observation). The effect size used is given in the heading of each model (SMD=standardized mean difference or Hedges' g; Zr=correlation coefficient; lnOR=log odds ratio).")) |>
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
only_dominant
gtsave(only_dominant, filename = "figures/SI_Tables/only dominant effect sizes.pdf")


# >>> Habitat -------------------------------------------------------
unique(si_models$only_dominant_effect_size)
unique(si_models$moderator)
unique(si_models$prey_range)
unique(si_models$habitat_trait)

sub_models <- si_models[class == "All classes" &
                          moderator == "Overall estimate" &
                          only_dominant_effect_size == "no" &
                          prey_range == "All" &
                          habitat_trait != "All habitats"]
sub_models[, group := paste(analysis_group, habitat_trait, analysis_effect_size, sep = " | ")]

dput(unique(sub_models$group))

lvls <- c("Abundance with/without cats | Foraging habitat: ground | SMD", 
        "Abundance with/without cats | Foraging habitat: other | SMD", 
        "Abundance with/without cats | Ground nesting: ground | SMD", 
        "Abundance with/without cats | Ground nesting: other | SMD", 
        "Abundance with/without cats | Locomotion: Non-volant | SMD", 
        "Abundance with/without cats | Locomotion: volant | SMD", 
        "Abundance with/without cats | Landform: island | SMD", 
        
        "Abundance before-after eradication | Foraging habitat: other | SMD", 
        "Abundance before-after eradication | Ground nesting: other | SMD", 
        "Abundance before-after eradication | Landform: island | SMD",
        
        "Abundance on islands with/without cats | Foraging habitat: ground | lnOR", 
         "Abundance on islands with/without cats | Ground nesting: ground | lnOR", 
         "Abundance on islands with/without cats | Ground nesting: other | lnOR", 
         "Abundance on islands with/without cats | Locomotion: Non-volant | lnOR", 
         "Abundance on islands with/without cats | Locomotion: volant | lnOR", 
        "Abundance on islands with/without cats | Landform: island | lnOR", 
        
           "Abundance spatial association | Foraging habitat: other | Zr", 
           "Abundance spatial association | Ground nesting: other | Zr", 
           "Abundance spatial association | Locomotion: Non-volant | Zr", 
        "Abundance spatial association | Landform: mainland | Zr", 
        
           "Abundance temporal association | Foraging habitat: ground | Zr", 
           "Abundance temporal association | Foraging habitat: other | Zr", 
           "Abundance temporal association | Ground nesting: ground | Zr", 
           "Abundance temporal association | Ground nesting: other | Zr", 
           "Abundance temporal association | Locomotion: Non-volant | Zr", 
        "Abundance temporal association | Landform: island | Zr", 
        "Abundance temporal association | Landform: mainland | Zr", 
          
          "Reproduction with/without cats | Foraging habitat: ground | lnOR", 
          "Reproduction with/without cats | Ground nesting: other | lnOR", 
          "Reproduction with/without cats | Locomotion: volant | lnOR", 
        "Reproduction with/without cats | Landform: island | lnOR", 
        
         
          "Reproduction before-after eradication | Foraging habitat: ground | SMD", 
          "Reproduction before-after eradication | Ground nesting: other | SMD", 
          "Reproduction before-after eradication | Locomotion: volant | SMD", 
        "Reproduction before-after eradication | Landform: island | SMD", 
        
        "Reproduction temporal association | Foraging habitat: ground | Zr", 
        "Reproduction temporal association | Ground nesting: other | Zr", 
        "Reproduction temporal association | Locomotion: volant | Zr", 
        "Reproduction temporal association | Landform: island | Zr"
          
)

setdiff(sub_models$group, lvls)

sub_models$group <- factor(sub_models$group,
                           levels = lvls)

# Also should sort by Intercept vs moderator
unique(sub_models$term)
sub_models$term <- factor(sub_models$term,
                          levels = c("overall", "intercept", "Mass (g, log10)"))
sub_models[, num1 := as.numeric(group)]
sub_models[, num2 := as.numeric(term)]

setorder(sub_models, num1, num2)
sub_models

#
habitat <- sub_models %>%
  select(group, term, Estimate, 
         moderator,
         `Test statistics`, `Sample size`, 
         `Residual heterogeneity`) %>%
  rename("Term" = "term",
         "Moderator" = "moderator") %>%
  group_by(group) %>%
  gt() %>%
  fmt_markdown(columns = `Residual heterogeneity`) %>%
  fmt_markdown(columns = `Moderator`) %>%
  fmt_markdown(columns = `Sample size`) %>%
  fmt_markdown(columns = `Test statistics`) %>%
  tab_header(#title = ,
    md("**Table S10**. Model summaries for habitat specific groupings, including foraging habitat (ground versus other), nesting habitat (ground versus other), locomotion (volant versus non-volant) and landform (continent vs island). Each habitat factor level was run indepenendently in a subgroup intercept-only model. Model estimates ± 95% CIs and test statistics are reported along with sample sizes and residual unexplained heterogeneity ($I^2$), decomposed by hierarchical model levels (article and observation). The effect size used is given in the heading of each model (SMD=standardized mean difference or Hedges' g; Zr=correlation coefficient; lnOR=log odds ratio).")) |>
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
habitat
gtsave(habitat, filename = "figures/SI_Tables/habitat.pdf")

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

claims

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

# top <- best.p + theme(legend.position = "none") + 
#         p.sys + theme(legend.position = "none") +
#         plot_spacer() +
#         plot_layout(nrow = 1, widths = c(3/7, 3/7, 1/7))

# 
# top / (p.abund | p.reprod) + plot_annotation(tag_levels = "A")
# ggsave("figures/main_text/meta_review_raw.pdf", width = 8.5, height = 7)

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Best evidence per class + maps -----------------------------------------------------------
# >>> build df ----
spp_evidence_claims <- merge(claims[, .(scientificName)],
                             sys_rev[, .(class, scientificName, evidence_type, has_data, of_quality, Study_long, Study_lat)],
                             all.x = TRUE)

spp_evidence_claims[is.na(evidence_type), evidence_type := "No evidence found"]

spp_evidence_claims[, synth_fill := fcase(
  grepl("Population", evidence_type) & has_data == "yes", paste(evidence_type, "with data"),
  grepl("Population", evidence_type) & has_data == "no",  paste(evidence_type, "without data")
)]
spp_evidence_claims[is.na(synth_fill), synth_fill := evidence_type]
spp_evidence_claims[of_quality == "yes", synth_fill := paste(synth_fill, "and qualities")]

spp_evidence_claims <- merge(spp_evidence_claims[, class := NULL],
                             best_evidence[, .(class, scientificName)])

# >>> Define levels and rankings ----
not_support <- c("Predation not in support",
                 "Population not in support without data",
                 "Population not in support with data",
                 "Population not in support with data and qualities")

quality_rank <- c(
  "Population in support with data and qualities"     = 1,
  "Population not in support with data and qualities" = 2,
  "Population in support with data"                   = 3,
  "Population in support without data"                = 4,
  "Predation in support"                              = 5,
  "Population not in support with data"               = 6,
  "Population not in support without data"            = 7,
  "Predation not in support"                          = 8,
  "No evidence found"                                 = 9
)

synth_levels <- c(
  "Population in support with data and qualities",
  "Population in support with data",
  "Population in support without data",
  "Predation in support",
  "Population not in support with data and qualities",
  "Population not in support with data",
  "Population not in support without data",
  "Predation not in support",
  "No evidence found"
)

spp_evidence_claims[, n := fcase(
  synth_fill %in% not_support,  -1,
  !synth_fill %in% not_support,  1
)]
spp_evidence_claims[synth_fill == "No evidence found", n := NA]
spp_evidence_claims[, quality_key := quality_rank[synth_fill]]
spp_evidence_claims$synth_fill <- factor(spp_evidence_claims$synth_fill, levels = rev(synth_levels))

#  >>> order + column split functions ----
make_order <- function(dt) {
  o <- dt[, .(
    has_support_qual   = any(as.character(synth_fill) == "Population in support with data and qualities",     na.rm = TRUE),
    has_nosupport_qual = any(as.character(synth_fill) == "Population not in support with data and qualities", na.rm = TRUE),
    has_support        = any(n == 1,  na.rm = TRUE),
    has_no_support     = any(n == -1, na.rm = TRUE),
    best_key           = min(quality_key, na.rm = TRUE),
    n_studies          = sum(!is.na(n))
  ), by = scientificName]
  
  o[, group := fcase(
    has_support_qual,                     1L,
    has_nosupport_qual,                   2L,
    has_support & !has_support_qual,      3L,
    has_no_support & !has_nosupport_qual, 4L,
    default =                             5L
  )]
  
  o[order(group, -n_studies, best_key, scientificName)]
}

make_cols <- function(dt, n_breaks = 3) {
  ord <- make_order(dt)
  ordered_spp <- data.table(scientificName = rev(ord$scientificName))
  
  if (n_breaks == 1) {
    ordered_spp[, col := 1L]
  } else {
    ordered_spp[, col := cut(seq_len(.N), breaks = n_breaks, labels = 1:n_breaks) |> as.integer()]
  }
  
  result <- merge(dt, ordered_spp, by = "scientificName")
  result$scientificName <- factor(result$scientificName, levels = rev(ord$scientificName))
  result
}

# >>> Apply function per class ----
rep_N_reptiles   <- make_cols(spp_evidence_claims[class == "Reptiles"])
rep_N_mammals    <- make_cols(spp_evidence_claims[class == "Mammals"])
rep_N_birds      <- make_cols(spp_evidence_claims[class == "Birds"])
rep_N_amphibians <- make_cols(spp_evidence_claims[class == "Amphibians"], n_breaks = 1)

# >>> Plots ----
#Amphibians (single panel)
study_amphi <- ggplot(data = rep_N_amphibians,
                      aes(x = n, y = scientificName, fill = synth_fill, color = of_quality)) +
  geom_col(lwd = 1, position = "stack") +
  geom_vline(xintercept = 0) +
  xlab("Number of studies") + 
  labs(title = "Amphibians") +
  ylab(NULL) +
  coord_cartesian(xlim = c(-2, 2), ylim = c(0, 17)) +
  scale_fill_manual(values = fill_pal) +
  scale_color_manual(values = c("no" = "transparent", "yes" = "gold")) +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.ticks.x = element_blank(),
        legend.position = "none", strip.background = element_blank(),
        panel.border = element_blank(), title = element_text(size = 15),
        axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  annotate("text", x = -1.5, y = 16.5, label = "No",  size = 4) +
  annotate("text", x =  2,   y = 16.5, label = "Yes", size = 4)

#multi-panel plot function
make_panel <- function(dt, col_id, title = " ", xlim_val = c(-3, 3), ylim_val = c(0, 87), show_annot = FALSE) {
  p <- ggplot(data = dt[col == col_id],
              aes(x = n, y = scientificName, fill = synth_fill, color = of_quality)) +
    geom_col(lwd = 1, position = "stack") +
    geom_vline(xintercept = 0) +
    xlab(if (col_id == 2) "Number of studies" else " ") +
    labs(title = title) + ylab(NULL) +
    coord_cartesian(xlim = xlim_val, ylim = ylim_val, clip = if (col_id == 1) "off" else "on") +
    scale_fill_manual(values = fill_pal) +
    scale_color_manual(values = c("no" = "transparent", "yes" = "gold")) +
    theme_bw() +
    theme(panel.grid = element_blank(), axis.ticks.x = element_blank(),
          legend.position = "none", strip.background = element_blank(),
          panel.border = element_blank(), title = element_text(size = 15),
          axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 13))
  if (show_annot) {
    p <- p +
      annotate("text", x = xlim_val[1] / 2, y = ylim_val[2] - 0.5, label = "No",  size = 4) +
      annotate("text", x = xlim_val[2],      y = ylim_val[2] - 0.5, label = "Yes", size = 4)
  }
  p
}

#Reptiles
study_rept <- cowplot::plot_grid(
  make_panel(rep_N_reptiles, 3, title = "Reptiles", xlim_val = c(-3,3), ylim_val = c(0,87), show_annot = TRUE),
  make_panel(rep_N_reptiles, 2, xlim_val = c(-3,3), ylim_val = c(0,87)),
  make_panel(rep_N_reptiles, 1, xlim_val = c(-3,3), ylim_val = c(0,87)),
  nrow = 1)

#Mammals
study_mamm <- cowplot::plot_grid(
  make_panel(rep_N_mammals, 3, title = "Mammals", xlim_val = c(-11,11), ylim_val = c(0,51), show_annot = TRUE),
  make_panel(rep_N_mammals, 2, xlim_val = c(-11,11), ylim_val = c(0,51)),
  make_panel(rep_N_mammals, 1, xlim_val = c(-11,11), ylim_val = c(0,51)),
  nrow = 1)

#Birds
study_bird <- cowplot::plot_grid(
  make_panel(rep_N_birds, 3, title = "Birds", xlim_val = c(-6,6), ylim_val = c(0,106), show_annot = TRUE),
  make_panel(rep_N_birds, 2, xlim_val = c(-6,6), ylim_val = c(0,106)),
  make_panel(rep_N_birds, 1, xlim_val = c(-6,6), ylim_val = c(0,106)),
  nrow = 1)

#World map
spp_evidence_claims_map <- spp_evidence_claims[!is.na(Study_long), ]
str(spp_evidence_claims_map)
spp_evidence_claims_map <-st_as_sf(spp_evidence_claims_map, coords = c("Study_long", "Study_lat"), crs = 4326)
unique(spp_evidence_claims_map$scientificName) #223 species
setdiff(spp_evidence_claims$scientificName, spp_evidence_claims_map$scientificName) #505 species

library(rnaturalearth)
world_sf <- st_as_sf(ne_countries(scale = "medium", returnclass = "sf"))
world_sf <- st_as_sf(world_sf, crs = 4326)

#Overall
ggplot() +
  geom_sf(data = world_sf, fill = "gray90", color = "black", size = 0.1) +
  geom_sf(data = spp_evidence_claims_map,
          aes(fill = synth_fill),
          shape = 21, size = 3, stroke = 0, color = "transparent") +
  geom_sf(data = spp_evidence_claims_map[spp_evidence_claims_map$of_quality == "yes", ],
          aes(fill = synth_fill),
          shape = 21, size = 3, stroke = 1, color = "gold") +
  scale_fill_manual(values = fill_pal) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = "+proj=moll")

#Amphibians
map_amphi <-ggplot() +
  geom_sf(data = world_sf, fill = "gray90", color = "black", size = 0.1) +
  geom_sf(data = spp_evidence_claims_map[which(spp_evidence_claims_map$class == "Amphibians"), ],
          aes(fill = synth_fill),
          shape = 21, size = 3, stroke = 0, color = "transparent") +
  # geom_sf(data = spp_evidence_claims_map[which(spp_evidence_claims_map$class == "Amphibians" & 
  #                                                spp_evidence_claims_map$of_quality == "yes"), ],
  #         aes(fill = synth_fill),
  #         shape = 21, size = 3, stroke = 1, color = "gold") +
  scale_fill_manual(values = fill_pal) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        panel.spacing = unit(0, "lines")
  )+
  coord_sf(crs = "+proj=moll")+ #, expand = FALSE
  labs(#title="Amphibians",
    fill=" ")

#Reptiles
map_repti <-ggplot() +
  geom_sf(data = world_sf, fill = "gray90", color = "black", size = 0.1) +
  geom_sf(data = spp_evidence_claims_map[which(spp_evidence_claims_map$class == "Reptiles"), ],
          aes(fill = synth_fill),
          shape = 21, size = 3, stroke = 0, color = "transparent") +
  geom_sf(data = spp_evidence_claims_map[which(spp_evidence_claims_map$class == "Reptiles" & 
                                                 spp_evidence_claims_map$of_quality == "yes"), ],
          aes(fill = synth_fill),
          shape = 21, size = 3, stroke = 1, color = "gold") +
  scale_fill_manual(values = fill_pal) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        panel.spacing = unit(0, "lines")
  ) +
  coord_sf(crs = "+proj=moll")+#, expand = FALSE
  labs(#title="Reptiles",
    fill=" ")

#Birds
map_birds <-ggplot() +
  geom_sf(data = world_sf, fill = "gray90", color = "black", size = 0.1) +
  geom_sf(data = spp_evidence_claims_map[which(spp_evidence_claims_map$class == "Birds"), ],
          aes(fill = synth_fill),
          shape = 21, size = 3, stroke = 0, color = "transparent") +
  geom_sf(data = spp_evidence_claims_map[which(spp_evidence_claims_map$class == "Birds" & 
                                                 spp_evidence_claims_map$of_quality == "yes"), ],
          aes(fill = synth_fill),
          shape = 21, size = 3, stroke = 1, color = "gold") +
  scale_fill_manual(values = fill_pal) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        panel.spacing = unit(0, "lines")) +
  coord_sf(crs = "+proj=moll")+#, expand = FALSE
  labs(#title="Birds",
    fill=" ")

#Mammals
map_mammals <-ggplot() +
  geom_sf(data = world_sf, fill = "gray90", color = "black", size = 0.1) +
  geom_sf(data = spp_evidence_claims_map[which(spp_evidence_claims_map$class == "Mammals"), ],
          aes(fill = synth_fill),
          shape = 21, size = 3, stroke = 0, color = "transparent") +
  geom_sf(data = spp_evidence_claims_map[which(spp_evidence_claims_map$class == "Mammals" & 
                                                 spp_evidence_claims_map$of_quality == "yes"), ],
          aes(fill = synth_fill),
          shape = 21, size = 3, stroke = 1, color = "gold") +
  scale_fill_manual(values = fill_pal) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        panel.spacing = unit(0, "lines")) +
  coord_sf(crs = "+proj=moll")+#, expand = FALSE
  labs(#title="Mammals",
    fill=" ")

# >>> final figures ----
#Save and add evidence legend on Inkscape
cowplot::plot_grid(study_amphi, map_amphi, ncol=1, labels = "AUTO")
ggsave("figures/SI/best_evidence_amphi.pdf", width = 16, height = 14)

cowplot::plot_grid(study_rept, map_repti, ncol=1, labels = "AUTO", rel_heights = c(1,.5))
ggsave("figures/SI/best_evidence_rept.pdf", width = 16, height = 20)

cowplot::plot_grid(study_mamm, map_mammals, ncol=1, labels = "AUTO", rel_heights = c(1,.5))
ggsave("figures/SI/best_evidence_mamm.pdf", width = 16, height = 20)

cowplot::plot_grid(study_bird, map_birds, ncol=1, labels = "AUTO", rel_heights = c(1,.5))
ggsave("figures/SI/best_evidence_bird.pdf", width = 16, height = 22)
