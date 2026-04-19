

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
nrow(dat[duplicated(article_id)])
nrow(dat[duplicated(Article)])
# Must be equal. Good

dat[is.na(yi_smd), ]

unique(dat[, .(analysis_group, analysis_effect_size)])

# Let's lump abundance and reproduction by effect size type

dat.long <- copy(dat)#melt(dat, measure.vars = c("class", "log_mass", "continent_island"),
              #   value.name = "predictor")
dat.long[, .(n = uniqueN(article_id)), by = .(analysis_group_collapsed)]


dat.long[, .(n_articles = uniqueN(article_id)),
         by = .(class, analysis_group_collapsed)]#[, .(min(n_articles), nlvls = uniqueN(class)), by = .(analysis_group)]

dat.long$continent_island
dat.long[, .(n_articles = uniqueN(article_id)),
               by = .(continent_island, analysis_group)]


Ns <- dat.long[, .(n_articles = uniqueN(article_id)),
               by = .(analysis_group_collapsed)]
Ns


dat.long[!is.na(yi_smd), .(yi_smd, yi_analysis)]

# Set up model guide -----------------------------------------------------------------

# Let's also do some simpler analysis groups. Nah, that requires converting ZCOR. Let's not bother yet.

unique(dat$analysis_group)
guide <- CJ(analysis_group_collapsed = unique(dat.long$analysis_group_collapsed),
            moderator = c("1", "class", "log_mass", "continent_island")
)

guide[, random_effect := "~1 | article_id/Effect_size_ID"]

guide[, exclusion := paste0("analysis_group_collapsed == '", analysis_group_collapsed, "'")]
guide

guide[moderator != "1", exclusion := paste0(exclusion, " & !is.na(", moderator, ")")]

guide[, formula := paste("~", moderator)]

guide
# guide <- guide[!(moderator %in% c("class", "log_mass") & analysis_group %in% c("Long-term abundance",
#                                                               "Before-after eradication reproduction",
#                                                               "Short-term reproduction",
#                                                               "Before-after eradication abundance",
#                                                               "abundance", "reproduction"))]
# 
# guide <- guide[!(moderator %in% c("continent_island") & analysis_group %in% c("Before-after eradication reproduction",
#                                                                               "Before-after eradication abundance",
#                                                                               "reproduction"))]
guide <- merge(guide,
               Ns,
               by = "analysis_group_collapsed")
guide
guide <- guide[n_articles >= 3, ]
guide[, model_id := paste0("model_", seq(1:.N))]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Intercept only models  --------------------------------------------------------------------
sub_guide <- guide[moderator == "1", ]

models <- list()
tidy_models <- list()
i <- 1

for(i in 1:nrow(sub_guide)){
  
  sub_guide[i, ]
  dat.sub <- dat.long[eval(parse(text = sub_guide$exclusion[i]))]
  dat.sub
  
  tryCatch(
    expr={
   models[[i]] <- rma.mv(yi_analysis, 
                        V = vi_analysis,
                        mods = as.formula(sub_guide$formula[i]),
                        random = eval(parse(text = sub_guide$random_effect[i])),
                        data = dat.sub) 
  
  pred <- predict(models[[i]])
  
  tidy_models[[i]] <- tidy(models[[i]]) |>
    mutate(lower_ci = pred$ci.lb, 
           upper_ci = pred$ci.ub,
           lower_pi = pred$pi.lb,
           upper_pi = pred$pi.ub) |>
    mutate(overfit = ifelse(any(models[[i]]$sigma2 == 0), "yes", "no")) |>
    mutate(n_studies = length(unique(dat.sub$article_id)),
           n_observations = length(unique(dat.sub$Effect_size_ID))) |>
    bind_cols(as.data.frame(i2_ml(models[[i]])) |> t() ) |>
    bind_cols(sub_guide[i, ]) |>
    setDT()
  
  cat(i, "/", nrow(sub_guide), "\r")
  },
  error = {print("ERROR")})
}

tidy_models <- rbindlist(tidy_models)

tidy_models

# >>> Plot -----------------------------------------------------------------

intercepts <- copy(tidy_models)
unique(intercepts$analysis_group_collapsed)
intercepts$analysis_group_collapsed <- factor(intercepts$analysis_group_collapsed,
                                    levels = c("reproduction_SMD", "abundance_Zr", "abundance_lnOR"))
dat.long$analysis_group_collapsed <- factor(dat.long$analysis_group_collapsed,
                                  levels = c("reproduction_SMD", "abundance_Zr", "abundance_lnOR"))
#
# intercepts[, effect_type_analyzed := ifelse(analysis_group == "Short-term abundance",
#                                             "Zr", "SMD")]
labels <- as_labeller(c("abundance_Zr" = "Abundance (across surveys)",
                        "abundance_lnOR" = "Abundance (across sites)",
                        "reproduction_SMD" = "Reproductive success (across surveys)"))
#
p1 <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(data = dat.long[analysis_group_collapsed %in% "abundance_Zr"], 
              aes(x = analysis_group_collapsed, y = yi_analysis, 
                  fill = original_effect_size, size = 1/vi_analysis),
              shape = 21,
              position = position_jitterdodge(jitter.width = .15,
                                              dodge.width = .75),
              alpha = .5)+
  geom_errorbar(data = intercepts[analysis_group_collapsed %in% "abundance_Zr"], 
                aes(x = analysis_group_collapsed, 
                    ymin = lower_ci, ymax = upper_ci),
                width = .25)+
  geom_pointrange(data = intercepts[analysis_group_collapsed %in% "abundance_Zr"], 
                  aes(x = analysis_group_collapsed, y = estimate,
                                         ymin = lower_pi, ymax = upper_pi),
                  size = 1)+
  scale_fill_manual(values = c("SMD" = "hotpink",
                               "Zr" = "goldenrod",
                               "lnOR" = "dodgerblue"))+
  facet_wrap(~analysis_group_collapsed, scales = "free_y",
             labeller = labels)+
  # scale_x_discrete(labels = c("abundance" = "All abundance measures",
  #                             "Long-term abundance" = "Long-term abundance",
  #                             "Short-term abundance" = "Short-term abundance",
  #                             "reproduction" = "Reproduction"))+
  coord_cartesian(ylim = c(-2, 2))+
  xlab(NULL)+
  ylab("Effect on abundance (Zr, correlation coefficient)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_blank())
p1

p2 <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(data = dat.long[analysis_group_collapsed %in% "abundance_lnOR"], 
             aes(x = analysis_group_collapsed, y = yi_analysis, 
                 fill = original_effect_size, size = 1/vi_analysis),
             shape = 21,
             position = position_jitterdodge(jitter.width = .15,
                                             dodge.width = .75),
             alpha = .5)+
  geom_errorbar(data = intercepts[analysis_group_collapsed %in% "abundance_lnOR"], 
                aes(x = analysis_group_collapsed, 
                    ymin = lower_ci, ymax = upper_ci),
                width = .25)+
  geom_pointrange(data = intercepts[analysis_group_collapsed %in% "abundance_lnOR"], 
                  aes(x = analysis_group_collapsed, y = estimate,
                      ymin = lower_pi, ymax = upper_pi),
                  size = 1)+
  scale_fill_manual(values = c("SMD" = "hotpink",
                               "Zr" = "goldenrod",
                               "lnOR" = "dodgerblue"))+
  facet_wrap(~analysis_group_collapsed, scales = "free_y",
             labeller = labels)+
  # scale_x_discrete(labels = c("abundance" = "All abundance measures",
  #                             "Long-term abundance" = "Long-term abundance",
  #                             "Short-term abundance" = "Short-term abundance",
  #                             "reproduction" = "Reproduction"))+
  coord_cartesian(ylim = c(-3, 3))+
  xlab(NULL)+
  ylab("Effect on abundance (log odds ratio)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_blank())
p2

p3 <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(data = dat.long[analysis_group_collapsed %in% "reproduction_SMD"], 
             aes(x = analysis_group_collapsed, y = yi_analysis, 
                 fill = original_effect_size, size = 1/vi_analysis),
             shape = 21,
             position = position_jitterdodge(jitter.width = .15,
                                             dodge.width = .75),
             alpha = .5)+
  geom_errorbar(data = intercepts[analysis_group_collapsed %in% "reproduction_SMD"], 
                aes(x = analysis_group_collapsed, 
                    ymin = lower_ci, ymax = upper_ci),
                width = .25)+
  geom_pointrange(data = intercepts[analysis_group_collapsed %in% "reproduction_SMD"], 
                  aes(x = analysis_group_collapsed, y = estimate,
                      ymin = lower_pi, ymax = upper_pi),
                  size = 1)+
  scale_fill_manual(values = c("SMD" = "hotpink",
                               "Zr" = "goldenrod",
                               "lnOR" = "dodgerblue"))+
  facet_wrap(~analysis_group_collapsed, scales = "free_y",
             labeller = labels)+
  # scale_x_discrete(labels = c("abundance" = "All abundance measures",
  #                             "Long-term abundance" = "Long-term abundance",
  #                             "Short-term abundance" = "Short-term abundance",
  #                             "reproduction" = "Reproduction"))+
  coord_cartesian(ylim = c(-2, 2))+
  xlab(NULL)+
  ylab("Effect on reproduction (Hedges' g)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_blank())
p3

p1 + p2 + p3


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------

# Island vs continent -----------------------------------------------------
guide$moderator
sub_guide <- guide[moderator == "continent_island", ]

models <- list()
tidy_models <- list()
i <- 1
newgrid <- data.table(continent_island = unique(dat.long$continent_island))

for(i in 1:nrow(sub_guide)){
  
  sub_guide[i, ]
  dat.sub <- dat.long[eval(parse(text = sub_guide$exclusion[i]))]
  dat.sub
  
  tryCatch(
    expr={
      models[[i]] <- rma.mv(yi_analysis, 
                            V = vi_analysis,
                            mods = as.formula(sub_guide$formula[i]),
                            random = eval(parse(text = sub_guide$random_effect[i])),
                            data = dat.sub) 
      
      pred <- rma_predictions(models[[i]], newgrid) # Get prediction intervals for intercept only models
      tidy_models[[i]] <- pred |>
        rename(lower_ci = ci.lb,
               upper_ci = ci.ub,
               lower_pi = pi.lb,
               upper_pi = pi.ub) |>
        mutate(overfit = ifelse(any(models[[i]]$sigma2 == 0), "yes", "no")) |>
        mutate(n_studies = length(unique(dat.sub$article_id)),
               n_observations = length(unique(dat.sub$Effect_size_ID))) |>
        bind_cols(as.data.frame(i2_ml(models[[i]])) |> t() ) |>
        bind_cols(sub_guide[i, ]) |>
        setDT()
      
      cat(i, "/", nrow(sub_guide), "\r")
    },
    error = {print("ERROR")})
}

tidy_models <- rbindlist(tidy_models)

tidy_models


# >>> Plot ----------------------------------------------------------------

p1 <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(data = dat.long[analysis_group_collapsed %in% "abundance_Zr"], 
             aes(x = continent_island, y = yi_analysis, 
                 fill = original_effect_size, size = 1/vi_analysis),
             shape = 21,
             position = position_jitterdodge(jitter.width = .15,
                                             dodge.width = .75),
             alpha = .5)+
  geom_errorbar(data = tidy_models[analysis_group_collapsed %in% "abundance_Zr"], 
                aes(x = continent_island, 
                    ymin = lower_ci, ymax = upper_ci),
                width = .25)+
  geom_pointrange(data = tidy_models[analysis_group_collapsed %in% "abundance_Zr"], 
                  aes(x = continent_island, y = pred,
                      ymin = lower_pi, ymax = upper_pi),
                  size = 1)+
  scale_fill_manual(values = c("SMD" = "hotpink",
                               "Zr" = "goldenrod",
                               "lnOR" = "dodgerblue"))+
  facet_wrap(~analysis_group_collapsed, scales = "free_y",
             labeller = labels)+
  # scale_x_discrete(labels = c("abundance" = "All abundance measures",
  #                             "Long-term abundance" = "Long-term abundance",
  #                             "Short-term abundance" = "Short-term abundance",
  #                             "reproduction" = "Reproduction"))+
  coord_cartesian(ylim = c(-2, 2))+
  xlab(NULL)+
  ylab("Effect on abundance (Zr, correlation coefficient)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_blank())
p1

dat.long[analysis_group_collapsed %in% "abundance_lnOR"]$yi_analysis
length(unique(dat.long[analysis_group_collapsed %in% "abundance_lnOR" & continent_island == "Continent"]$article_id))
# OK only 1 for continents.
p2 <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(data = dat.long[analysis_group_collapsed %in% "abundance_lnOR"], 
             aes(x = continent_island, y = yi_analysis, 
                 fill = original_effect_size, size = 1/vi_analysis),
             shape = 21,
             position = position_jitterdodge(jitter.width = .15,
                                             dodge.width = .75),
             alpha = .5)+
  # geom_errorbar(data = tidy_models[analysis_group_collapsed %in% "abundance_lnOR"], 
  #               aes(x = continent_island, 
  #                   ymin = lower_ci, ymax = upper_ci),
  #               width = .25)+
  # geom_pointrange(data = tidy_models[analysis_group_collapsed %in% "abundance_lnOR"], 
  #                 aes(x = continent_island, y = pred,
  #                     ymin = lower_pi, ymax = upper_pi),
  #                 size = 1)+
  scale_fill_manual(values = c("SMD" = "hotpink",
                               "Zr" = "goldenrod",
                               "lnOR" = "dodgerblue"))+
  facet_wrap(~analysis_group_collapsed, scales = "free_y",
             labeller = labels)+
  # scale_x_discrete(labels = c("abundance" = "All abundance measures",
  #                             "Long-term abundance" = "Long-term abundance",
  #                             "Short-term abundance" = "Short-term abundance",
  #                             "reproduction" = "Reproduction"))+
  # coord_cartesian(ylim = c(-5, 5))+
  xlab(NULL)+
  ylab("Effect on abundance (log odds ratio)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_blank())
p2
p1 + p2


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------

# Class -----------------------------------------------------
guide$moderator
sub_guide <- guide[moderator == "class", ]
sub_guide

models <- list()
tidy_models <- list()
i <- 1
newgrid <- data.table(class = unique(dat.long$class))
newgrid <- newgrid[!is.na(class), ]

for(i in 1:nrow(sub_guide)){
  
  sub_guide[i, ]
  dat.sub <- dat.long[eval(parse(text = sub_guide$exclusion[i]))]
  dat.sub
  
  tryCatch(
    expr={
      models[[i]] <- rma.mv(yi_analysis, 
                            V = vi_analysis,
                            mods = as.formula(sub_guide$formula[i]),
                            random = eval(parse(text = sub_guide$random_effect[i])),
                            data = dat.sub) 
      
      pred <- rma_predictions(models[[i]], newgrid) # Get prediction intervals for intercept only models
      tidy_models[[i]] <- pred |>
        rename(lower_ci = ci.lb,
               upper_ci = ci.ub,
               lower_pi = pi.lb,
               upper_pi = pi.ub) |>
        mutate(overfit = ifelse(any(models[[i]]$sigma2 == 0), "yes", "no")) |>
        mutate(n_studies = length(unique(dat.sub$article_id)),
               n_observations = length(unique(dat.sub$Effect_size_ID))) |>
        bind_cols(as.data.frame(i2_ml(models[[i]])) |> t() ) |>
        bind_cols(sub_guide[i, ]) |>
        setDT()
      
      cat(i, "/", nrow(sub_guide), "\r")
    },
    error = {print("ERROR")})
}

tidy_models <- rbindlist(tidy_models)

tidy_models

# >>> Plot ----------------------------------------------------------------

p1 <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(data = dat.long[analysis_group_collapsed %in% "abundance_Zr" &
                               !is.na(class)], 
             aes(x = class, y = yi_analysis, 
                 fill = original_effect_size, size = 1/vi_analysis),
             shape = 21,
             position = position_jitterdodge(jitter.width = .15,
                                             dodge.width = .75),
             alpha = .5)+
  geom_errorbar(data = tidy_models[analysis_group_collapsed %in% "abundance_Zr"], 
                aes(x = class, 
                    ymin = lower_ci, ymax = upper_ci),
                width = .25)+
  geom_pointrange(data = tidy_models[analysis_group_collapsed %in% "abundance_Zr"], 
                  aes(x = class, y = pred,
                      ymin = lower_pi, ymax = upper_pi),
                  size = 1)+
  scale_fill_manual(values = c("SMD" = "hotpink",
                               "Zr" = "goldenrod",
                               "lnOR" = "dodgerblue"))+
  facet_wrap(~analysis_group_collapsed, scales = "free_y",
             labeller = labels)+
  # scale_x_discrete(labels = c("abundance" = "All abundance measures",
  #                             "Long-term abundance" = "Long-term abundance",
  #                             "Short-term abundance" = "Short-term abundance",
  #                             "reproduction" = "Reproduction"))+
  coord_cartesian(ylim = c(-2, 2))+
  xlab(NULL)+
  ylab("Effect on abundance (Zr, correlation coefficient)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_blank())
p1


p2 <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(data = dat.long[analysis_group_collapsed %in% "abundance_lnOR"&
                               !is.na(class)], 
             aes(x = class, y = yi_analysis, 
                 fill = original_effect_size, size = 1/vi_analysis),
             shape = 21,
             position = position_jitterdodge(jitter.width = .15,
                                             dodge.width = .75),
             alpha = .5)+
  geom_errorbar(data = tidy_models[analysis_group_collapsed %in% "abundance_lnOR"], 
                aes(x = class, 
                    ymin = lower_ci, ymax = upper_ci),
                width = .25)+
  geom_pointrange(data = tidy_models[analysis_group_collapsed %in% "abundance_lnOR"], 
                  aes(x = class, y = pred,
                      ymin = lower_pi, ymax = upper_pi),
                  size = 1)+
  scale_fill_manual(values = c("SMD" = "hotpink",
                               "Zr" = "goldenrod",
                               "lnOR" = "dodgerblue"))+
  facet_wrap(~analysis_group_collapsed, scales = "free_y",
             labeller = labels)+
  # scale_x_discrete(labels = c("abundance" = "All abundance measures",
  #                             "Long-term abundance" = "Long-term abundance",
  #                             "Short-term abundance" = "Short-term abundance",
  #                             "reproduction" = "Reproduction"))+
  coord_cartesian(ylim = c(-5, 5))+
  xlab(NULL)+
  ylab("Effect on abundance (log odds ratio)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_blank())
p2
p1 + p2

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------

# Body mass ---------------------------------------------------------------
guide$moderator
sub_guide <- guide[moderator == "log_mass", ]

models <- list()
tidy_models <- list()
i <- 1
newgrid <- data.table(log_mass = seq(from = min(dat.long$log_mass, na.rm = T), 
                                     to = max(dat.long$log_mass, na.rm = T),
                                     by = .1))

for(i in 1:nrow(sub_guide)){
  
  sub_guide[i, ]
  dat.sub <- dat.long[eval(parse(text = sub_guide$exclusion[i]))]
  dat.sub
  
  tryCatch(
    expr={
      models[[i]] <- rma.mv(yi_analysis, 
                            V = vi_analysis,
                            mods = as.formula(sub_guide$formula[i]),
                            random = eval(parse(text = sub_guide$random_effect[i])),
                            data = dat.sub) 
      
      pred <- rma_predictions(models[[i]], newgrid) # Get prediction intervals for intercept only models
      tidy_models[[i]] <- pred |>
        rename(lower_ci = ci.lb,
               upper_ci = ci.ub,
               lower_pi = pi.lb,
               upper_pi = pi.ub) |>
        mutate(overfit = ifelse(any(models[[i]]$sigma2 == 0), "yes", "no")) |>
        mutate(n_studies = length(unique(dat.sub$article_id)),
               n_observations = length(unique(dat.sub$Effect_size_ID))) |>
        bind_cols(as.data.frame(i2_ml(models[[i]])) |> t() ) |>
        bind_cols(sub_guide[i, ]) |>
        setDT()
      
      cat(i, "/", nrow(sub_guide), "\r")
    },
    error = {print("ERROR")})
}

tidy_models <- rbindlist(tidy_models)

tidy_models

unique(tidy_models$analysis_group_collapsed)
# >>> Plot ----------------------------------------------------------------

p1 <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_ribbon(data = tidy_models[analysis_group_collapsed %in% "abundance_Zr"], 
              aes(x = log_mass, 
                  ymin = lower_pi, ymax = upper_pi),
              alpha = .5,
              fill = "transparent",
              color = "black", linetype = "dotted")+
  geom_ribbon(data = tidy_models[analysis_group_collapsed %in% "abundance_Zr"], 
              aes(x = log_mass, 
                  ymin = lower_ci, ymax = upper_ci),
              alpha = .5,
              fill = "grey")+
  geom_line(data = tidy_models[analysis_group_collapsed %in% "abundance_Zr"], 
              aes(x = log_mass, 
                  y = pred),
              fill = "grey")+
  geom_point(data = dat.long[analysis_group_collapsed %in% "abundance_Zr"], 
             aes(x = log_mass, y = yi_analysis, 
                 fill = original_effect_size, size = 1/vi_analysis),
             shape = 21,
             position = position_jitterdodge(jitter.width = .15,
                                             dodge.width = .75),
             alpha = .5)+
  scale_fill_manual(values = c("SMD" = "hotpink",
                               "Zr" = "goldenrod",
                               "lnOR" = "dodgerblue"))+
  facet_wrap(~analysis_group_collapsed, scales = "free_y",
             labeller = labels)+
  # scale_x_discrete(labels = c("abundance" = "All abundance measures",
  #                             "Long-term abundance" = "Long-term abundance",
  #                             "Short-term abundance" = "Short-term abundance",
  #                             "reproduction" = "Reproduction"))+
  coord_cartesian(ylim = c(-2, 2))+
  xlab("Body mass (log10)")+
  ylab("Effect on abundance (Zr, correlation coefficient)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_blank())
p1


p2 <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_ribbon(data = tidy_models[analysis_group_collapsed %in% "abundance_lnOR"], 
              aes(x = log_mass, 
                  ymin = lower_pi, ymax = upper_pi),
              alpha = .5,
              fill = "transparent",
              color = "black", linetype = "dotted")+
  geom_ribbon(data = tidy_models[analysis_group_collapsed %in% "abundance_lnOR"], 
              aes(x = log_mass, 
                  ymin = lower_ci, ymax = upper_ci),
              alpha = .5,
              fill = "grey")+
  geom_line(data = tidy_models[analysis_group_collapsed %in% "abundance_lnOR"], 
            aes(x = log_mass, 
                y = pred),
            fill = "grey")+
  geom_point(data = dat.long[analysis_group_collapsed %in% "abundance_lnOR"], 
             aes(x = log_mass, y = yi_analysis, 
                 fill = original_effect_size, size = 1/vi_analysis),
             shape = 21,
             position = position_jitterdodge(jitter.width = .15,
                                             dodge.width = .75),
             alpha = .5)+
  scale_fill_manual(values = c("SMD" = "hotpink",
                               "Zr" = "goldenrod",
                               "lnOR" = "dodgerblue"))+
  facet_wrap(~analysis_group_collapsed, scales = "free_y",
             labeller = labels)+
  # scale_x_discrete(labels = c("abundance" = "All abundance measures",
  #                             "Long-term abundance" = "Long-term abundance",
  #                             "Short-term abundance" = "Short-term abundance",
  #                             "reproduction" = "Reproduction"))+
  coord_cartesian(ylim = c(-5, 5))+
  xlab("Body mass (log10)")+
  ylab("Effect on abundance (log odds ratio)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_blank())
p2

length(unique(dat.long[analysis_group_collapsed %in% "reproduction_SMD"]$scientificName))
tidy_models[analysis_group_collapsed %in% "reproduction_SMD"]
models[[3]]

p3 <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_ribbon(data = tidy_models[analysis_group_collapsed %in% "reproduction_SMD"], 
              aes(x = log_mass, 
                  ymin = lower_pi, ymax = upper_pi),
              alpha = .5,
              fill = "transparent",
              color = "black", linetype = "dotted")+
  geom_ribbon(data = tidy_models[analysis_group_collapsed %in% "reproduction_SMD"], 
              aes(x = log_mass, 
                  ymin = lower_ci, ymax = upper_ci),
              alpha = .5,
              fill = "grey")+
  geom_line(data = tidy_models[analysis_group_collapsed %in% "reproduction_SMD"], 
            aes(x = log_mass, 
                y = pred),
            fill = "grey")+
  geom_point(data = dat.long[analysis_group_collapsed %in% "reproduction_SMD"], 
             aes(x = log_mass, y = yi_analysis, 
                 fill = original_effect_size, size = 1/vi_analysis),
             shape = 21,
             position = position_jitterdodge(jitter.width = .15,
                                             dodge.width = .75),
             alpha = .5)+
  scale_fill_manual(values = c("SMD" = "hotpink",
                               "Zr" = "goldenrod",
                               "lnOR" = "dodgerblue"))+
  facet_wrap(~analysis_group_collapsed, scales = "free_y",
             labeller = labels)+
  # scale_x_discrete(labels = c("abundance" = "All abundance measures",
  #                             "Long-term abundance" = "Long-term abundance",
  #                             "Short-term abundance" = "Short-term abundance",
  #                             "reproduction" = "Reproduction"))+
  coord_cartesian(ylim = c(-2, 2), xlim = c(2, 3.5))+
  xlab("Body mass (log10)")+
  ylab("Effect on reproduction (Hedges' g)")+
  guides(size = "none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_blank())
p3

p1 + p2 + p3

# dat.long[analysis_group %in% intercepts$analysis_group,
#          .(articles = uniqueN(article_id)),
#          by = .(effect_size_type, analysis_group)]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------------

# Publication bias --------------------------------------------------------
#' [What's effective N for Zr -> SMD? Shin:]
# A good question we probably use the same formula as paired SMD or just use the half and use the formula if it makes sense 

unique(dat.long$effect_size_type)
dat.long[effect_size_type == "SMD", `:=` (n2 = Sample_size_overall_cats_Absent,
                                          n1 = Sample_size_overall_cats_Present)]

dat.long[effect_size_type %in% c("SMD", "Odds ratio converted to Hedges g"), effective_N := (4*n1*n2)/(n1+n2)]
#' [I think this is what he means:]
dat.long[effect_size_type %in% c("Correlation"), effective_N := (2*n)]

# 
dat.long[!analysis_group %in% "Short-term abundance", correction := 1/effective_N]
dat.long[!analysis_group %in% "Short-term abundance", bias_test := sqrt(1/effective_N)]
#
dat.long[analysis_group %in% "Short-term abundance", correction := vi]
dat.long[analysis_group %in% "Short-term abundance", bias_test := sqrt(vi)]

#
bias_guide <- CJ(analysis_group = unique(intercepts$analysis_group),
                 predictor = c("correction", "bias_test"))
bias_guide[, formula := paste("~", predictor)]
bias_guide[, model_id := paste0("bias_model_", seq(1:.N))]

bias_models <- list()
bias_tidy <- list()

# Rewrite to use yi_Zr and appropriate N for Short-term abundance...

for(i in 1:nrow(bias_guide)){
  sub.dat <- dat.long[analysis_group == bias_guide$analysis_group[i]]

  bias_models[[i]] <- rma.mv(yi,
                             V = vi,
                             mods = as.formula(bias_guide$formula[i]),
                             random = ~1 | article_id/Effect_size_ID,
                             data = sub.dat)
  
  bias_tidy[[i]] <- bias_models[[i]] |> 
    tidy() |>
    mutate(sigma = min(bias_models[[i]]$sigma2),
           lower_ci = bias_models[[i]]$ci.lb,
           upper_ci = bias_models[[i]]$ci.ub) |>
    bind_cols(bias_guide[i, ]) |>
    setDT()
  
}

bias_tidy <- rbindlist(bias_tidy)
bias_tidy
bias_tidy[p.value < 0.05 &
            term == "bias_test"]

bias_tidy[analysis_group == "Short-term abundance" &
            predictor == "correction"]
# Well that doesn't make any sense at all. Huh.

# Let's plot the bias-corrected estimates:
# corrected_estimates <- bias_tidy[predictor == "correction" &
#                                    term == "correction", ]

# corrected_estimates

# intercepts$model_type <- "Model estimates"
# corrected_estimates$model_type <- "Bias-corrected estimate"
# corrected_estimates$effect_type_analyzed <- "Zr"
# 
# # Well we can ignore those that aren't significant...
# ggplot()+
#   geom_hline(yintercept = 0, linetype = "dashed")+
#   geom_point(data = dat.long[analysis_group %in% intercepts$analysis_group], 
#              aes(x = analysis_group, y = yi, 
#                  fill = effect_size_type, size = 1/vi),
#              shape = 21,
#              position = position_jitterdodge(jitter.width = .15,
#                                              dodge.width = .75),
#              alpha = .5)+
#   geom_pointrange(data = intercepts, 
#                   aes(x = analysis_group, y = estimate,
#                        ymin = lower_ci, ymax = upper_ci,
#                       fill = model_type),
#                   shape = 21,
#                   size = 1)+
#   # geom_pointrange(data = corrected_estimates[analysis_group == "Short-term abundance"],
#   #                 aes(x = analysis_group, y = estimate,
#   #                     fill = model_type,
#   #                     ymin = lower_ci, ymax = upper_ci),
#   #                 shape = 21,
#   #                 size = 1,
#   #                 alpha = .5)+
#   facet_wrap(~effect_type_analyzed, scales = "free")+
#   scale_x_discrete(labels = c("abundance" = "All abundance measures",
#                               "Long-term abundance" = "Long-term abundance",
#                               "Short-term abundance" = "Short-term abundance",
#                               "reproduction" = "Reproduction"))+
#   guides(size = "none")+
#   theme_bw()+
#   theme(panel.grid = element_blank(),
#         panel.border = element_blank())

