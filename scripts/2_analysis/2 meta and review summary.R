

rm(list = ls())

library("data.table")
library("metafor")
library("ggplot2")
library("orchaRd")
library("patchwork")
library("readxl")
library("stringr")
library("sf")
library("mapview")
library("tidyr")
library("broom")
library("broom.mixed")
library("dplyr")
library("rotl")
library("ape")
library("gt")

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

# Load data ---------------------------------------------------------------

dat <- fread("builds/meta_analysis/analysis_ready_dataset.csv")

dat[, log_mass := log10(Mass_g_final)]

dat[, .(n = .N), by = .(Order_final)]
# Not enough data.

dat[, .(n = .N), by = .(class)]
dat[class == "Reptiles", class := NA]

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

dat.long[analysis_group_collapsed %in% "reproduction_SMD"]

#
# dat.long[analysis_group == "Before-after eradication abundance", .(analysis_effect_size)]

unique(dat.long[, .(analysis_effect_size, analysis_group, analysis_group_collapsed)])
unique(dat.long$Effect_size_ID)
dat.long[Effect_size_ID %in% c("ES_47a", "ES_47b")]


#
dat.long[Effect_size_ID == "ES_43"]$analysis_group
dat.long[Effect_size_ID == "ES_37"]$analysis_group

# Set up model guide -----------------------------------------------------------------

# Also, we should do a sensitivity analysis
x <- dat.long[, .(n = uniqueN(article_id)),
    by = .(analysis_group_collapsed, original_effect_size)]
# 
setorder(x, analysis_group_collapsed, n)
x

unique(dat.long$original_effect_size)
unique(dat.long$analysis_group_collapsed)

# Let's also do some simpler analysis groups. Nah, that requires converting ZCOR. Let's not bother yet.

unique(dat$analysis_group)
unique(dat$class)

guide <- CJ(analysis_group_collapsed = unique(dat.long$analysis_group_collapsed),
            moderator = c("1", "log_mass"),
            class = c("Birds", "Mammals", "All"),
            non_phylo_species = c("yes", "no"),
            phylo_species = c("yes", "no"),
            only_dominant_effect_size = c("yes", "no")
)

guide[, model_comparison_id := paste0("model_comp_", .GRP), 
      by = .(analysis_group_collapsed, class,
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
guide[, exclusion := paste0("analysis_group_collapsed == '", analysis_group_collapsed, "'")]
# guide[moderator != "1", ]
guide[moderator != "1", exclusion := paste0(exclusion, " & !is.na(", moderator, ")")]
guide[class != "All", exclusion := paste0(exclusion, " & class %in% '", class, "'")]
guide[class != "All",]

#
#
guide[, analysis_effect_size := word(analysis_group_collapsed, -1, sep = "_")]
guide[only_dominant_effect_size == "yes", exclusion := paste0(exclusion, " & original_effect_size == '",
                                                              analysis_effect_size, "'")]
#
guide[, formula := paste("~", moderator)]

guide
guide

# >>> Add model ID --------------------------------------------------------

guide[, model_id := paste0("model_", seq(1:.N))]

# >>> Get sample sizes ----------------------------------------------------
Ns <- list()
sub.dat <- c()
i <- 1

for(i in 1:nrow(guide)){
  sub.dat <- dat.long[eval(parse(text = guide[i, ]$exclusion))]
  
  Ns[[i]] <- sub.dat[, .(n_species = uniqueN(scientificName),
                    n_articles = uniqueN(Article),
                    n_obs = .N,
                    model_id = guide[i, ]$model_id,
                    analysis_group_collapsed = guide[i, ]$analysis_group_collapsed,
                    class = guide[i, ]$class)]
  Ns[[i]]$model_id <- guide[i, ]$model_id
  
}
Ns <- rbindlist(Ns)
Ns

guide <- merge(guide, Ns[, .(n_species, n_articles, n_obs, model_id)], by = "model_id")
guide[n_articles == 0, ]

# at least 3 articles for intercept only models
guide <- guide[n_articles >= 3, ]
guide

# and at least 5 for continuous
guide <- guide[!(moderator == "log_mass" & n_articles < 5), ]
guide

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


# >>> Set up prediction grids ---------------------------------------------

grids <- list()
grids[["log_mass"]] <- data.table(log_mass = seq(from = min(dat.long$log_mass, na.rm = T),
                                                 to = max(dat.long$log_mass, na.rm = T),
                                                 by = .1))
grids

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Run models --------------------------------------------------------------

models <- list()
predictions <- list()
tidy_models <- list()
i <- 46

for(i in 1:nrow(guide)){
  
  sub_guide <- guide[i, ]
  sub_guide
  dat.sub <- dat.long[eval(parse(text = sub_guide$exclusion))]
  dat.sub
  
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
    
    if(sub_guide$moderator != "log_mass"){
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
names(models) <- guide$model_id

setdiff(names(tidy_models[[2]]), names(tidy_models[[1]]))
tidy_models <- rbindlist(tidy_models, fill = TRUE)
tidy_models[overfit == "yes", ]
tidy_models <- tidy_models[overfit != "yes", ]

length(predictions)
predictions <- rbindlist(predictions, fill = TRUE)
predictions

models[["model_9"]]

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

predictions[, string := paste0(n_articles, "(", n_species, ", ", n_obs, ")")]
predictions

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Main text plots --------------------------------------------------------------------
# >>> Intercept-only models -----------------------------------------------------------------
tidy_models[moderator == "1" & only_dominant_effect_size == "no", ]

intercepts <- predictions[moderator == "1" & only_dominant_effect_size == "no" &
                            class == "All"]

unique(intercepts$analysis_group_collapsed)

intercepts$analysis_group_collapsed <- factor(intercepts$analysis_group_collapsed,
                                              levels = c("reproduction_SMD", "abundance_Zr", "abundance_lnOR"))
dat.long$analysis_group_collapsed <- factor(dat.long$analysis_group_collapsed,
                                            levels = c("reproduction_SMD", "abundance_Zr", "abundance_lnOR"))

# intercepts[, effect_type_analyzed := ifelse(analysis_group == "Short-term abundance",
#                                             "Zr", "SMD")]
labels <- as_labeller(c("abundance_Zr" = "Short-term abundance",
                        "abundance_lnOR" = "Long-term abundance",
                        "reproduction_SMD" = "Reproductive success"))

#
p1 <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_text(data = intercepts[analysis_group_collapsed %in% "abundance_Zr"], 
            aes(x = "All species", y = -4, #hjust = 2,
               label = string),
            color = "grey50",
            size = 3)+
  geom_jitter(data = dat.long[analysis_group_collapsed %in% "abundance_Zr"], 
             aes(x = "All species", y = yi_analysis, 
                 size = 1/vi_analysis, fill = yi_analysis),
             shape = 21, #fill = "grey90",
             height = 0, width = 0.25,
             alpha = .5)+
  scale_fill_gradient2(low = "dodgerblue", high = "indianred",
                       midpoint = 0, mid = "white")+
  guides(size = "none", fill = "none")+
  geom_errorbar(data = intercepts[analysis_group_collapsed %in% "abundance_Zr"], 
                aes(x = "All species", 
                    ymin = lower_ci, ymax = upper_ci),
                width = .25)+
  geom_pointrange(data = intercepts[analysis_group_collapsed %in% "abundance_Zr"], 
                  aes(x = "All species", y = pred,
                      ymin = lower_pi, ymax = upper_pi),
                  shape = 21, fill = "grey50",
                  size = 1)+
  # facet_wrap(~analysis_group_collapsed, scales = "free_y",
  #            labeller = labels,
  #            strip.position = "bottom")+
  coord_cartesian(ylim = c(-4, 4))+
  xlab(NULL)+
  ylab("Short term abundance correlation (Zr)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
p1

p2 <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_text(data = intercepts[analysis_group_collapsed %in% "abundance_lnOR"], 
            aes(x = "All species", y = -4, #hjust = 2,
                label = string),
            color = "grey50",
            size = 3)+
  geom_jitter(data = dat.long[analysis_group_collapsed %in% "abundance_lnOR"], 
              aes(x = "All species", y = yi_analysis, 
                  size = 1/vi_analysis, fill = yi_analysis),
              shape = 21, #fill = "grey90",
              height = 0, width = 0.25,
              alpha = .5)+
  scale_fill_gradient2(low = "dodgerblue", high = "indianred",
                       midpoint = 0, mid = "white")+
  guides(size = "none", fill = "none")+
  geom_errorbar(data = intercepts[analysis_group_collapsed %in% "abundance_lnOR"], 
                aes(x = "All species", 
                    ymin = lower_ci, ymax = upper_ci),
                width = .25)+
  geom_pointrange(data = intercepts[analysis_group_collapsed %in% "abundance_lnOR"], 
                  aes(x = "All species", y = pred,
                      ymin = lower_pi, ymax = upper_pi),
                  shape = 21, fill = "grey50",
                  size = 1)+
  # facet_wrap(~analysis_group_collapsed, scales = "free_y",
  #            labeller = labels,
  #            strip.position = "bottom")+
  coord_cartesian(ylim = c(-4, 4))+
  xlab(NULL)+
  ylab("Long-term abundance (ln Odds Ratio)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
p2

p3 <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_text(data = intercepts[analysis_group_collapsed %in% "reproduction_SMD"], 
            aes(x = "All species", y = -2, #hjust = 2,
                label = string),
            color = "grey50",
            size = 3)+
  geom_jitter(data = dat.long[analysis_group_collapsed %in% "reproduction_SMD"], 
              aes(x = "All species", y = yi_analysis, 
                  size = 1/vi_analysis, fill = yi_analysis),
              shape = 21, #fill = "grey90",
              height = 0, width = 0.25,
              alpha = .5)+
  scale_fill_gradient2(low = "dodgerblue", high = "indianred",
                       midpoint = 0, mid = "white")+
  guides(size = "none", fill = "none")+
  geom_errorbar(data = intercepts[analysis_group_collapsed %in% "reproduction_SMD"], 
                aes(x = "All species", 
                    ymin = lower_ci, ymax = upper_ci),
                width = .25)+
  geom_pointrange(data = intercepts[analysis_group_collapsed %in% "reproduction_SMD"], 
                  aes(x = "All species", y = pred,
                      ymin = lower_pi, ymax = upper_pi),
                  shape = 21, fill = "grey50",
                  size = 1)+
  # facet_wrap(~analysis_group_collapsed, scales = "free_y",
  #            labeller = labels,
  #            strip.position = "bottom")+
  coord_cartesian(ylim = c(-2, 2))+
  xlab(NULL)+
  ylab("Reproductive success (Hedges' g)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
p3

# >>> Class ----------------------------------------------------------------
predictions

#
class <- predictions[moderator == "1" & class != "All" &
                       only_dominant_effect_size == "no"]
unique(class$analysis_group_collapsed)
dat.long[analysis_group_collapsed %in% "abundance_Zr", .(n = uniqueN(Article)),
         by = .(class, original_effect_size)]


class.abundance <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_text(data = class[analysis_group_collapsed %in% "abundance_Zr", ],
            aes(x = class, y = -3, hjust = 1,
                label = string),
            size = 3)+
  geom_jitter(data = dat.long[analysis_group_collapsed %in% "abundance_Zr"], 
              aes(x = class, y = yi_analysis, 
                  size = 1/vi_analysis, fill = yi_analysis),
              shape = 21, #fill = "grey90",
              height = 0, width = 0.25,
              alpha = .5)+
  scale_fill_gradient2(low = "dodgerblue", high = "indianred",
                       midpoint = 0, mid = "white")+
  guides(size = "none", fill = "none")+
  geom_errorbar(data = class[analysis_group_collapsed %in% "abundance_Zr"], 
                aes(x = class, 
                    ymin = lower_ci, ymax = upper_ci),
                width = .25)+
  geom_pointrange(data = class[analysis_group_collapsed %in% "abundance_Zr"], 
                  aes(x = class, y = pred,
                      ymin = lower_pi, ymax = upper_pi),
                  shape = 21,
                  fill = "grey50",
                  size = 1)+
  coord_cartesian(ylim = c(-4, 4))+
  xlab(NULL)+
  ylab("Short-term abundance correlation (Zr)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
class.abundance


class.long.abundance <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_text(data = class[analysis_group_collapsed %in% "abundance_lnOR", ],
            aes(x = class, y = -10, hjust = 1,
                label = string),
            size = 3)+
  geom_jitter(data = dat.long[analysis_group_collapsed %in% "abundance_lnOR" & !is.na(class)], 
              aes(x = class, y = yi_analysis, 
                  size = 1/vi_analysis, fill = yi_analysis),
              shape = 21, #fill = "grey90",
              height = 0, width = 0.25,
              alpha = .5)+
  scale_fill_gradient2(low = "dodgerblue", high = "indianred",
                       midpoint = 0, mid = "white")+
  guides(size = "none", fill = "none")+
  geom_errorbar(data = class[analysis_group_collapsed %in% "abundance_lnOR"], 
                aes(x = class, 
                    ymin = lower_ci, ymax = upper_ci),
                width = .25)+
  geom_pointrange(data = class[analysis_group_collapsed %in% "abundance_lnOR"], 
                  aes(x = class, y = pred,
                      ymin = lower_pi, ymax = upper_pi),
                  shape = 21,
                  fill = "grey50",
                  size = 1)+
  # coord_cartesian(ylim = c(-4, 4))+
  xlab(NULL)+
  ylab("Long-term abundance (log Odds Ratio)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
class.long.abundance

class.reproduction <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_text(data = class[analysis_group_collapsed %in% "reproduction_SMD", ],
            aes(x = class, y = -2, hjust = 1,
                label = string),
            size = 3)+
  geom_jitter(data = dat.long[analysis_group_collapsed %in% "reproduction_SMD" & !is.na(class)], 
              aes(x = class, y = yi_analysis, 
                  size = 1/vi_analysis, fill = yi_analysis),
              shape = 21, #fill = "grey90",
              height = 0, width = 0.25,
              alpha = .5)+
  scale_fill_gradient2(low = "dodgerblue", high = "indianred",
                       midpoint = 0, mid = "white")+
  guides(size = "none", fill = "none")+
  geom_errorbar(data = class[analysis_group_collapsed %in% "reproduction_SMD"], 
                aes(x = class, 
                    ymin = lower_ci, ymax = upper_ci),
                width = .25)+
  geom_pointrange(data = class[analysis_group_collapsed %in% "reproduction_SMD"], 
                  aes(x = class, y = pred,
                      ymin = lower_pi, ymax = upper_pi),
                  shape = 21,
                  fill = "grey50",
                  size = 1)+
  coord_cartesian(ylim = c(-3, 3))+
  xlab(NULL)+
  ylab("Reproductive success (Hedges' g)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
class.reproduction


# >>> log_mass ------------------------------------------------------------
tidy_models[moderator == "log_mass"  & only_dominant_effect_size == "no", ]
mass <- predictions[moderator == "log_mass"  & only_dominant_effect_size == "no" &
                      class == "All"]
unique(mass$analysis_group_collapsed)
#
# Back-transform mass.
mass[, mass_g := 10^log_mass]

#
p.mass.1 <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_ribbon(data = mass[analysis_group_collapsed %in% "abundance_Zr"], 
              aes(x = mass_g, 
                  ymin = lower_pi, ymax = upper_pi),
              alpha = .5,
              fill = "transparent",
              color = "black", linetype = "dotted")+
  geom_ribbon(data = mass[analysis_group_collapsed %in% "abundance_Zr"], 
              aes(x = mass_g, 
                  ymin = lower_ci, ymax = upper_ci),
              alpha = .5,
              fill = "grey")+
  geom_line(data = mass[analysis_group_collapsed %in% "abundance_Zr"], 
            aes(x = mass_g, 
                y = pred),
            color = "black")+
  geom_jitter(data = dat.long[analysis_group_collapsed %in% "abundance_Zr"], 
             aes(x = Mass_g_final, y = yi_analysis, 
                 size = 1/vi_analysis),
             shape = 21,
             fill = "grey50",
             alpha = .5)+
  facet_wrap(~analysis_group_collapsed, scales = "free_y",
             labeller = labels)+
  coord_cartesian(ylim = c(-2, 2))+
  xlab("Body mass (log10)")+
  ylab("Abundance correlation\n(Zr, correlation coefficient)")+
  guides(size = "none")+
  scale_x_log10()+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_blank())
p.mass.1


p.mass.2 <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_ribbon(data = mass[analysis_group_collapsed %in% "abundance_lnOR"], 
              aes(x = mass_g, 
                  ymin = lower_pi, ymax = upper_pi),
              alpha = .5,
              fill = "transparent",
              color = "black", linetype = "dotted")+
  geom_ribbon(data = mass[analysis_group_collapsed %in% "abundance_lnOR"], 
              aes(x = mass_g, 
                  ymin = lower_ci, ymax = upper_ci),
              alpha = .5,
              fill = "grey")+
  geom_line(data = mass[analysis_group_collapsed %in% "abundance_lnOR"], 
            aes(x = mass_g, 
                y = pred),
            color = "black")+
  geom_jitter(data = dat.long[analysis_group_collapsed %in% "abundance_lnOR"], 
              aes(x = Mass_g_final, y = yi_analysis, 
                  size = 1/vi_analysis),
              shape = 21,
              fill = "grey50",
              alpha = .5)+
  facet_wrap(~analysis_group_collapsed, scales = "free_y",
             labeller = labels)+
  coord_cartesian(ylim = c(-5, 5))+
  scale_x_log10()+
  xlab("Body mass (log10)")+
  ylab("Effect on abundance\n(log odds ratio)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_blank())
p.mass.2



p.mass.3 <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_ribbon(data = mass[analysis_group_collapsed %in% "reproduction_SMD"], 
              aes(x = mass_g, 
                  ymin = lower_pi, ymax = upper_pi),
              alpha = .5,
              fill = "transparent",
              color = "black", linetype = "dotted")+
  geom_ribbon(data = mass[analysis_group_collapsed %in% "reproduction_SMD"], 
              aes(x = mass_g, 
                  ymin = lower_ci, ymax = upper_ci),
              alpha = .5,
              fill = "grey")+
  geom_line(data = mass[analysis_group_collapsed %in% "reproduction_SMD"], 
            aes(x = mass_g, 
                y = pred),
            color = "black")+
  geom_jitter(data = dat.long[analysis_group_collapsed %in% "reproduction_SMD"], 
              aes(x = Mass_g_final, y = yi_analysis, 
                  size = 1/vi_analysis),
              shape = 21,
              fill = "grey50",
              alpha = .5)+
  facet_wrap(~analysis_group_collapsed, scales = "free_y",
             labeller = labels)+
  coord_cartesian(ylim = c(-5, 5))+
  scale_x_log10()+
  xlab("Body mass (log10)")+
  ylab("Effect on abundance\n(log odds ratio)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_blank())
p.mass.3


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# SI: Only dominant effect sizes --------------------------------------------------------------------

tidy_models[moderator == "1" & only_dominant_effect_size == "yes", ]

intercepts <- predictions[moderator == "1" & only_dominant_effect_size == "yes" &
                            class == "All"]

unique(intercepts$analysis_group_collapsed)

intercepts$analysis_group_collapsed <- factor(intercepts$analysis_group_collapsed,
                                              levels = c("reproduction_SMD", "abundance_Zr", "abundance_lnOR"))
dat.long$analysis_group_collapsed <- factor(dat.long$analysis_group_collapsed,
                                            levels = c("reproduction_SMD", "abundance_Zr", "abundance_lnOR"))

# intercepts[, effect_type_analyzed := ifelse(analysis_group == "Short-term abundance",
#                                             "Zr", "SMD")]
labels <- as_labeller(c("abundance_Zr" = "Short-term abundance",
                        "abundance_lnOR" = "Long-term abundance",
                        "reproduction_SMD" = "Reproductive success"))

#
p1.si <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_text(data = intercepts[analysis_group_collapsed %in% "abundance_Zr"], 
            aes(x = "All species", y = -3, #hjust = 2,
                label = string),
            size = 3)+
  geom_jitter(data = dat.long[analysis_group_collapsed %in% "abundance_Zr" &
                                original_effect_size == "Zr"], 
              aes(x = "All species", y = yi_analysis, 
                  size = 1/vi_analysis, fill = yi_analysis),
              shape = 21, #fill = "grey90",
              height = 0, width = 0.25,
              alpha = .5)+
  scale_fill_gradient2(low = "dodgerblue", high = "indianred",
                       midpoint = 0, mid = "white")+
  guides(size = "none", fill = "none")+
  geom_errorbar(data = intercepts[analysis_group_collapsed %in% "abundance_Zr"], 
                aes(x = "All species", 
                    ymin = lower_ci, ymax = upper_ci),
                width = .25)+
  geom_pointrange(data = intercepts[analysis_group_collapsed %in% "abundance_Zr"], 
                  aes(x = "All species", y = pred,
                      ymin = lower_pi, ymax = upper_pi),
                  shape = 21, fill = "grey50",
                  size = 1)+
  # facet_wrap(~analysis_group_collapsed, scales = "free_y",
  #            labeller = labels,
  #            strip.position = "bottom")+
  coord_cartesian(ylim = c(-4, 4))+
  xlab(NULL)+
  ylab("Short term abundance correlation (Zr)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
p1.si

p2.si <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_text(data = intercepts[analysis_group_collapsed %in% "abundance_lnOR"], 
            aes(x = "All species", y = -3, hjust = 2,
                label = string),
            size = 3)+
  geom_jitter(data = dat.long[analysis_group_collapsed %in% "abundance_lnOR" &
                                original_effect_size == "lnOR"], 
              aes(x = "All species", y = yi_analysis, 
                  size = 1/vi_analysis, fill = yi_analysis),
              shape = 21, #fill = "grey90",
              height = 0, width = 0.25,
              alpha = .5)+
  scale_fill_gradient2(low = "dodgerblue", high = "indianred",
                       midpoint = 0, mid = "white")+
  guides(size = "none", fill = "none")+
  geom_errorbar(data = intercepts[analysis_group_collapsed %in% "abundance_lnOR"], 
                aes(x = "All species", 
                    ymin = lower_ci, ymax = upper_ci),
                width = .25)+
  geom_pointrange(data = intercepts[analysis_group_collapsed %in% "abundance_lnOR"], 
                  aes(x = "All species", y = pred,
                      ymin = lower_pi, ymax = upper_pi),
                  shape = 21, fill = "grey50",
                  size = 1)+
  coord_cartesian(ylim = c(-4, 4))+
  # facet_wrap(~analysis_group_collapsed, scales = "free_y",
  #            labeller = labels,
  #            strip.position = "bottom")+
  # coord_cartesian(ylim = c(-3, 3))+
  xlab(NULL)+
  ylab("Long-term abundance (ln Odds Ratio)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",        
        strip.background = element_blank(),
        panel.border = element_blank())
p2.si

# >>> Class ----------------------------------------------------------------
#' [This is the same as the main text (no conversions)]
# predictions
# 
# #
# class <- predictions[moderator == "1" & class != "All" &
#                        only_dominant_effect_size == "yes"]
# unique(class$analysis_group_collapsed)
# dat.long[analysis_group_collapsed %in% "abundance_Zr", .(n = uniqueN(Article)),
#          by = .(class, original_effect_size)]
# 
# 
# class.abundance.si <- ggplot()+
#   geom_hline(yintercept = 0, linetype = "dashed")+
#   geom_text(data = class[analysis_group_collapsed %in% "abundance_Zr", ],
#             aes(x = class, y = -2, hjust = 1,
#                 label = string),
#             size = 3)+
#   geom_jitter(data = dat.long[analysis_group_collapsed %in% "abundance_Zr"], 
#               aes(x = class, y = yi_analysis, 
#                   size = 1/vi_analysis, fill = yi_analysis),
#               shape = 21, #fill = "grey90",
#               height = 0, width = 0.25,
#               alpha = .5)+
#   scale_fill_gradient2(low = "dodgerblue", high = "indianred",
#                        midpoint = 0, mid = "white")+
#   guides(size = "none", fill = "none")+
#   geom_errorbar(data = class[analysis_group_collapsed %in% "abundance_Zr"], 
#                 aes(x = class, 
#                     ymin = lower_ci, ymax = upper_ci),
#                 width = .25)+
#   geom_pointrange(data = class[analysis_group_collapsed %in% "abundance_Zr"], 
#                   aes(x = class, y = pred,
#                       ymin = lower_pi, ymax = upper_pi),
#                   shape = 21,
#                   fill = "grey50",
#                   size = 1)+
#   coord_cartesian(ylim = c(-4, 4))+
#   xlab(NULL)+
#   ylab("Short-term abundance correlation (Zr)")+
#   guides(size = "none")+
#   theme_bw()+
#   theme(panel.grid = element_blank(),
#         legend.position = "bottom",
#         strip.placement = "outside",        
#         strip.background = element_blank(),
#         panel.border = element_blank())
# class.abundance.si

# >>> Patchwork -----------------------------------------------------------

p1.si + p2.si + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "none")

ggsave("figures/SI/meta dominant effect size.pdf",
       width = 9, height = 6)
ggsave("figures/SI/meta dominant effect size.png",
       width = 9, height = 6)

# p.class + # p.conts + 
#   p.mass.1 + p.mass.2 + plot_layout(ncol = 2, nrow = 2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Other SI plots ----------------------------------------------------------
class.abundance + class.long.abundance + plot_annotation(tag_levels = "A")

ggsave("figures/SI/meta effects by class.pdf",
       width = 7, height = 6)
ggsave("figures/SI/meta effects by class.png",
       width = 7, height = 6)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# SI Tables ---------------------------------------------------------------

si_models <- tidy_models[moderator != "log_mass"]
si_models <- si_models[, !c("random_effect", "model_comparison_id", "non_phylo_species", "phylo_species", "I2_scientificName",
                            "I2_phylo_species", "min_aic", "moderator", "model_id", "aic", "overfit")]
si_models[, `Residual heterogeneity` := paste0("$I^2_{total}=", round(I2_Total, 1), "$<br>",
                                                "$I^2_{article}=", round(I2_article_id, 1), "$<br>",
                                                "$I^2_{obs}=", round(`I2_article_id/Effect_size_ID`), "$")]

si_models[, `Test statistics` := paste0("$t_{", df, "}=", round(statistic, 2), ", p=", round(p.value, 2), "$")]
si_models

si_models[, Estimate := paste0(round(estimate, 2), " ±[", round(lower_ci, 2), ", ", round(upper_ci, 2), "]")]

si_models[, `Sample size` := paste0("$N_{articles}=", n_articles, "$<br>",
                                     "$N_{species}=", n_species, "$<br>",
                                     "$N_{observations}=", n_obs, "$")]

si_models[, Model := fcase(analysis_group_collapsed == "abundance_Zr", "Short-term abundance (Zr)",
                           analysis_group_collapsed == "abundance_lnOR", "Long-term abundance (lnOR)",
                           analysis_group_collapsed == "reproduction_SMD", "Reproductive success (Hedges' g)")]
si_models[class == "All", class := "All species"]
si_models$class <- factor(si_models$class,
                          levels = rev(c("All species", "Mammals", "Birds")))
si_models$Model <- factor(si_models$Model,
                          levels = rev(c("Short-term abundance (Zr)",
                                     "Long-term abundance (lnOR)",
                                     "Reproductive success (Hedges' g)")))

main_text <- si_models %>%
  filter(only_dominant_effect_size == "no") %>%
  select(Model, class, Estimate, `Test statistics`, `Sample size`, `Residual heterogeneity`) %>%
  group_by(Model, class) %>%
  gt() %>%
  fmt_markdown(columns = `Residual heterogeneity`) %>%
  fmt_markdown(columns = `Sample size`) %>%
  fmt_markdown(columns = `Test statistics`) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()) %>%
  opt_table_font(
    size = 12
  )
main_text
gtsave(main_text, filename = "figures/SI/main text model table.pdf")

# fmt_markdown(columns = `Sample size`) %>%
  
only_dominant <- si_models %>%
  filter(only_dominant_effect_size == "yes") %>%
  select(Model, class, Estimate, `Test statistics`, `Sample size`, `Residual heterogeneity`) %>%
  group_by(Model, class) %>%
  gt() %>%
  fmt_markdown(columns = `Residual heterogeneity`) %>%
  fmt_markdown(columns = `Sample size`) %>%
  fmt_markdown(columns = `Test statistics`) %>%
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
sys_rev <- fread("builds/systematic_review_tidy.csv")
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
  (p1 + clean_lab | p2 + clean_lab | p3 + clean_lab)

# (p.sys / p.class ) | (p1 + p2 + p3 + plot_layout(ncol = 1))

ggsave("figures/main_text/meta_review_raw.pdf", width = 8, height = 8)
# p1 + p2 + p3 + plot_layout(ncol = 1)

# p.class + p.mass.1 + p.mass.2 + plot_layout(ncol = 2, nrow = 2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------

# Maps ------------------------------------------------------
library("stringr")
# sys_rev$Study_lat
sys_rev <- fread("builds/systematic_review_tidy.csv")
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

continents <- st_read("../../../Resources/Spatial/ESRI_continents/")
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

