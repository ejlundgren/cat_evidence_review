

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

dat[, article_id := paste(word(Article, 1, sep = "[[:space:][:punct:]]"), .GRP), by = .(Article)]
unique(dat[, .(Article, article_id)])
nrow(dat[duplicated(article_id)])
nrow(dat[duplicated(Article)])
# Must be equal. Good

dat[is.na(yi_smd), ]

unique(dat[, .(analysis_group, analysis_effect_size)])

# Let's lump abundance and reproduction by effect size type

dat.long <- copy(dat)#melt(dat, measure.vars = c("class", "log_mass", "continent_island"),#   value.name = "predictor")

# >>> Extract publicaiton years ------------------------------------------
library("readr")
# parse_number(x = "Gerber, G.P. and Iverson, J.B., 2000. Turks and Caicos iguana, Cyclura carinata carinata. West Indian Iguanas: Status Survey and Conservation Action Plan. IUCN the World Conservation Union, Gland, Switzerland, pp.15-18.")
str_extract("Gerber, G.P. and Iverson, J.B., 2000. Turks and Caicos iguana, Cyclura carinata carinata. West Indian Iguanas: Status Survey and Conservation Action Plan. IUCN the World Conservation Union, Gland, Switzerland, pp.15-18.", 
            "\\d+")

dat.long[, year := str_extract(Article,
                               "\\d+")]
unique(dat.long$year)
dat.long[year == "6", ]$Article
dat.long[year == "6", year := 1974]

dat.long[year == "13", ]$Article
dat.long[year == "13", year := 2010]

dat.long[year == "16", ]$Article
dat.long[year == "16", year := 2021]

dat.long[year == "35", ]$Article
dat.long[year == "35", year := 2010]

dat.long[year == "104", ]$Article
dat.long[year == "104", year := 2007]

dat.long[year == "12", ]$Article
dat.long[year == "12", year := 2009]

dat.long[, year := as.numeric(year)]

# >>> Calculate effective sample size ---------------------

unique(dat.long$analysis_effect_size)
dat.long[analysis_effect_size == "SMD", ]$original_effect_size
# I think we should use vi for this one

dat.long[analysis_effect_size == "lnOR", ]$original_effect_size
# I think we should use effective n for this one

dat.long[analysis_effect_size == "Zr", ]$original_effect_size
# I think we should use vi for this one

#
# n2i = Sample_size_overall_cats_Absent, n1i = Sample_size_overall_cats_Present,

dat.long[analysis_effect_size == "lnOR", .(Sample_size_overall_cats_Absent)]
dat.long[analysis_effect_size == "lnOR" &
           original_effect_size == "SMD",]
dat.long[analysis_effect_size == "lnOR" &
           original_effect_size == "SMD", `:=` (n2 = Sample_size_overall_cats_Absent,
                                                n1 = Sample_size_overall_cats_Present)]

dat.long[analysis_effect_size %in% c("lnOR"), 
         effective_N := (4*n1*n2)/(n1+n2)]
dat.long[analysis_effect_size %in% c("lnOR"), ]


dat.long[analysis_effect_size %in% c("lnOR"), correction := 1/effective_N]
dat.long[analysis_effect_size %in% c("lnOR"), bias_test := sqrt(1/effective_N)]

#
dat.long[analysis_effect_size %in% c("SMD","Zr"), correction := vi_analysis]
dat.long[analysis_effect_size %in% c("SMD","Zr"), bias_test := sqrt(vi_analysis)]


dat.long[, year_scaled := scale(year)]

# Set up model guide -----------------------------------------------------------------

unique(dat.long$analysis_group)
unique(dat.long$class)

guide <- CJ(analysis_group_collapsed = unique(dat.long$analysis_group_collapsed),
            moderator = c("year_scaled", "correction", "bias_test"),
            class = c("All", "Mammals", "Birds")
)

guide[, random_effect := "list(~1|article_id/Effect_size_ID"]

guide[, exclusion := paste0("analysis_group_collapsed == '", analysis_group_collapsed, "'")]
guide[class != "All", exclusion := paste(exclusion, "& class == '", class, "'")]

guide[, formula := paste("~", moderator)]

guide

guide[, model_id := paste(analysis_group_collapsed, moderator)]

# Run models --------------------------------------------------------------

bias_models <- list()
bias_tidy <- list()
i <- 1

for(i in 1:nrow(guide)){
  sub.dat <- dat.long[eval(parse(text = guide$exclusion[i]))]
  
  try({
    bias_models[[i]] <- rma.mv(yi_analysis,
                             V = vi_analysis,
                             method = "REML",
                             test = "t",
                             dfs = "contain",
                             mods = as.formula(guide$formula[i]),
                             random = ~1 | article_id/Effect_size_ID,
                             data = sub.dat)
  
  bias_tidy[[i]] <- bias_models[[i]] |> 
    tidy() |>
    mutate(sigma = min(bias_models[[i]]$sigma2),
           df = paste(bias_models[[i]]$ddf, collapse = ", "),
           lower_ci = bias_models[[i]]$ci.lb,
           upper_ci = bias_models[[i]]$ci.ub) |>
    bind_cols(guide[i, ]) |>
    setDT()
  })
  
}
# names(bias_models) <- guide$model_id
bias_models

# >>> Interpret -----------------------------------------------------------
# Bias:
bias_tidy <- rbindlist(bias_tidy)
bias_tidy
bias_tidy[p.value < 0.05 &
            term == "bias_test" &
            moderator == "bias_test"]

bias_tidy[analysis_group_collapsed == "reproduction_SMD" &
            moderator == "correction"]
#' [This suggests that without publication bias, cats would have a positive effect on reproduction]


# Decline effects:
bias_tidy[p.value < 0.05 &
            moderator == "year_scaled"]
# So increasingly positive relationship between abundance and cats with time. 

# Table -------------------------------------------------------------------

bias_tidy[, `Test statistics` := paste0("$t_{", df, "}=", round(statistic, 2), ", p=", round(p.value, 2), "$")]
bias_tidy[, Estimate := paste0(round(estimate, 2), " ±[", round(lower_ci, 2), ", ", round(upper_ci, 2), "]")]
bias_tidy[p.value < 0.05, `Test statistics` := paste(`Test statistics`, "*")]

bias_tidy

model.gt <- bias_tidy %>%
  mutate(response = case_when(analysis_group_collapsed == "abundance_Zr" ~ "Short-term abundance (Zr)",
                              analysis_group_collapsed == "abundance_lnOR" ~ "Long-term abundance (lnOR)",
                              analysis_group_collapsed == "reproduction_SMD" ~ "Reproductive success (SMD)"),
         moderator = case_when(moderator == "bias_test" ~ "Testing for bias",
                               moderator == "correction" ~ "Bias-corrected estimate",
                               moderator == "year_scaled" ~ "Year"),
         term = case_when(term == "bias_test" ~ "Bias test",
                          term == "correction" ~ "Correction",
                          term == "year_scaled" ~ "Publication year",
                          term == "intercept" ~ "intercept")) %>%
  select(moderator, term, `Test statistics`, Estimate,
         response, class) %>%
  group_by(response, class) %>%
  gt() %>%
  fmt_markdown(columns = `Test statistics`) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()) %>%
  opt_table_font(
    size = 12
  )
class(model.gt)

print(model.gt)

gtsave(model.gt, "figures/SI/publication bias models.pdf")

# gt()
