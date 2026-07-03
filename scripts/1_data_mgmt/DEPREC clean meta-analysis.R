

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
library("dplyr")
# Functions

get_r <- function(x, y){
  out <- cor.test(x, y)
  return(out$estimate)
}

# Copying from separate directory:
run <- FALSE
if(run){
  file.copy("../../Meta_Methods_UAlberta/package_development/metatools_dev/scripts/1_functions/convert_effect_sizes.R",
            "scripts/functions/convert_effect_sizes.R")
  file.copy("../../Meta_Methods_UAlberta/package_development/metatools_dev/data/conversion_formulas.csv",
            "scripts/functions/conversion_formulas.csv")
}

source("scripts/functions/convert_effect_sizes.R")

#0. Load data ---------------------------------------------------------------

files <- list.files("data/meta_analysis", full.names = T, pattern = ".xlsx")
files <- files[!grepl("~$", files, fixed = T)]
dat_list <- lapply(files, read_excel)
names(dat_list) <- word(files, -1, sep = " ")
dat_list

# >>> Number of articles by analysis group and effect size -------------------------------------

n.cor <- dat_list[["correlations.xlsx"]] %>%
  group_by(analysis_group, Description) %>%
  summarize(n_articles = n_distinct(Article)) %>%
  mutate(native_effect_type = "Zr")

n.smd <- dat_list[["SMDs.xlsx"]] %>%
  group_by(analysis_group, Description) %>%
  summarize(n_articles = n_distinct(Article)) %>%
  mutate(native_effect_type = "SMD")

n.or <- dat_list[["ORs.xlsx"]] %>%
  group_by(analysis_group, Description) %>%
  summarize(n_articles = n_distinct(Article)) %>%
  mutate(native_effect_type = "OR")
n.or

ns <- rbind(n.cor, n.smd, n.or)
ns
setDT(ns)

# Gah this is very challenging...
# Groups (split by reproduction vs abundance)
# All Long-term-abundance 
# Spatial correlations
# Temporal correlations
# 

# Going to determine in an excel file and merge in:
fwrite(ns, "builds/temp/determine analysis groups.csv")

# >>> Assign analysis groups & effect size types ----------------------------------------
#' [I deleted 'options' that were duplicated (e.g., same keys in that option)]
analysis_groups <- read_excel("data/meta_analysis/analysis groups.xlsx",
                              skip = 0) |> setDT()
analysis_groups

unique(analysis_groups[, .(Description, analysis_group_main_text,
                           analysis_group_supp)])


analysis_groups <- melt(analysis_groups,
                        id.vars = c("Description", "Abundance_reproduction"),
                        measure.vars = c("analysis_group_main_text", "analysis_group_supp"), 
                        variable.name = "analysis_grouping_option",
                        value.name = c("analysis_group")
)
analysis_groups
analysis_groups <- analysis_groups[!is.na(analysis_group), ]

analysis_groups

# Wow. Hahahaha
#' [We'll convert after calculating 'native' effect sizes]
# Calculate native effect size --------------------------------------------

# 1. Zr ---------------------------------------------------------

cor <- dat_list[["correlations.xlsx"]] |> setDT()
cor

sort(unique(cor$Effect_size_ID))

# >>> Clean -------------------------------------------------------------------
unique(cor$Description)

# This actually looks pretty good. Are there columns that need to be dropped in order to make it one row per ES_ID?
cor[Description == "Spatial correlation" & Effect_size_ID == "ES_3", ]

# OK good.

cor[is.na(as.numeric(Prey_correlation_data))]$Prey_correlation_data
cor[, Prey_correlation_data := str_trim(Prey_correlation_data)]
cor[is.na(as.numeric(Prey_correlation_data))]$Prey_correlation_data
# Some special character space...
cor[, Prey_correlation_data := as.numeric(Prey_correlation_data)]

cor[is.na(as.numeric(Cat_correlation_data))]$Cat_correlation_data
cor[, Cat_correlation_data := as.numeric(Cat_correlation_data)]

class(cor$cat_presence)
cor[is.na(as.numeric(cat_presence))]$cat_presence
# So these are binary predictor. Going to calculate point-biserial correlation for those.

point_biserial <- cor[is.na(Cat_correlation_data), ]
point_biserial$cat_presence
point_biserial[, .(n_points = .N), by = .(cat_presence, Effect_size_ID)]


# >>> Calculate ZCOR --------------------------------------------------
(cor)

cor.sum <- cor[!is.na(Cat_correlation_data), 
               .(r = get_r(x = Cat_correlation_data,
                           y = Prey_correlation_data),
                   n = .N),
               by = .(Effect_size_ID, scientificName,  Description,
                      Article, Article_secondary_same_data, 
                      analysis_group, effect_size_type, Abundance_reproduction,
                      Prey_units, Cat_units, Source, Analysis_comment, Latitude, Longitude, Study_location)]
cor.sum[duplicated(Effect_size_ID)]
cor.sum[Effect_size_ID == "ES_32", Effect_size_ID := paste0(Effect_size_ID, "_", .GRP), by = .(scientificName)]
cor.sum
# OK, so these should be separate effect sizes.

cor.sum <- escalc(measure = "ZCOR",
                     ri = r, ni = n,
                     data = cor.sum) |> setDT()

length(unique(cor.sum$Effect_size_ID))
length(unique(cor.sum$Article))


names(cor.sum)

unique(cor.sum$analysis_group)
ns

cor.sum[, original_effect_size := "Zr"]
cor.sum


# > Convert point-biserial Zr groups --------------------------------------------
unique(point_biserial$analysis_group)
ns
#' [Short-term abundance gets converted to Zr using point-biserial]
#' [Short-term reproduction gets converted to SMD directly ]
point_biserial[analysis_group == "Short-term reproduction", .(n = .N), by = .(cat_presence)]

# OK. 

point_biserial.summary <- point_biserial[, .(mean_prey = mean(Prey_correlation_data),
                                             sd_prey = sd(Prey_correlation_data),
                                             n = .N),
                                         by = .(Effect_size_ID, scientificName, Article, Article_secondary_same_data,
                                                analysis_group,effect_size_type, Description,
                                                Abundance_reproduction, Prey_units, Cat_units, Source,
                                                # Analysis_comment, 
                                                Latitude, Longitude, Study_location,
                                                cat_presence
                                                )]

point_biserial.summary.wide <- dcast(point_biserial.summary,
                                     ... ~ cat_presence,
                                     value.var = c("mean_prey", "sd_prey", "n"))
point_biserial.summary.wide
setnames(point_biserial.summary.wide, c("n_1", "n_0"), c("n1", "n2"))
zpb_out <- escalc(measure = "ZPB", 
                 m1i = mean_prey_1, m2i = mean_prey_0,
                 sd1i = sd_prey_1, sd2i = sd_prey_1,
                 n1i = n1, n2i = n2,
                 data = point_biserial.summary.wide) |> 
  setDT()
zpb_out[, n := n1 + n2] # for conversions, later

zpb_out
zpb_out[, original_effect_size := "Zr"]

point_biserial.summary.wide[n1 >= 3 & n2 >= 3, ]$Effect_size_ID

# >>> Bind ----------------------------------------------------------------

cor.sum
zpb_out
cor.final <- rbind(zpb_out, cor.sum, fill = T)

cor.final[duplicated(Effect_size_ID)]
# Must be 0 rows

# 2. SMD ---------------------------------------------------------
names(dat_list)
smd <- dat_list[["SMDs.xlsx"]] |> setDT()
smd

smd[is.na(analysis_group), ]


# >>> Clean -------------------------------------------------------------------
smd
# setnames(smd, "...30", "analysis_group_notes")
smd[is.na(as.numeric(Prey_mean_cats_Absent))]
smd[is.na(as.numeric(Prey_mean_cats_Present))]
smd[is.na(as.numeric(Prey_error_cats_Absent))]
smd[is.na(as.numeric(Prey_error_cats_Present))]
smd[is.na(as.numeric(Sample_size_overall_cats_Absent))]
smd[is.na(as.numeric(Sample_size_overall_cats_Present))]
smd[, `:=` (Prey_mean_cats_Absent = as.numeric(Prey_mean_cats_Absent),
            Prey_mean_cats_Present = as.numeric(Prey_mean_cats_Present),
            Prey_error_cats_Absent = as.numeric(Prey_error_cats_Absent),
            Prey_error_cats_Present = as.numeric(Prey_error_cats_Present),
            Sample_size_overall_cats_Absent = as.numeric(Sample_size_overall_cats_Absent),
            Sample_size_overall_cats_Present = as.numeric(Sample_size_overall_cats_Present))]

unique(smd$Effect_size_ID)
smd[Effect_size_ID == "ES_45", ]

# >>> Calculate effect sizes --------------------------------------------------
setnames(smd, c("Sample_size_overall_cats_Absent", "Sample_size_overall_cats_Present"),
                c("n2", "n1")) # This is to prevent later confusion while converting
smd <- escalc(measure = "SMD",
              m2i = Prey_mean_cats_Absent, m1i = Prey_mean_cats_Present,
              sd2i = Prey_error_cats_Absent, sd1i = Prey_error_cats_Present,
              n2i = n2, n1i = n1,
              data = smd) |> setDT()
smd[, .(Prey_mean_cats_Absent, Prey_mean_cats_Present, yi)]

smd[, original_effect_size := "SMD"]

# smd <- rbind(smd)
smd$analysis_group
ns
# So we convert this to OR

# smd[converted_to == "Zr"]
smd.final <- copy(smd)

cor.final

# 3. OR ---------------------------------------------------------
names(dat_list)

or <- dat_list[["ORs.xlsx"]] |> setDT()
or

ns

# >>> Clean -------------------------------------------------------------------

# This looks good.

# >>> Calculate effect sizes --------------------------------------------------

or <- escalc(measure = "OR",
             ai = Prey_and_cat_positive,
             bi = Prey_negative_cat_positive,
             ci = Prey_positive_cat_negative,
             di = Prey_and_cat_negative,
             data = or) |> setDT()
or

or[, n1 := Prey_and_cat_positive + Prey_negative_cat_positive]
or[, n2 := Prey_positive_cat_negative + Prey_and_cat_negative]
or[, original_effect_size := "lnOR"]
or.final <- copy(or)

# 4. Bind effect sizes ----------------------------------------------------

cor.final
smd.final
or.final

meta_combined <- rbind(cor.final, smd.final, or.final, fill = T)

meta_combined$original_effect_size

meta_combined[Effect_size_ID == "ES_ARM"]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------
# Explosion merge and convert effect sizes ------------------------------
meta_combined[, key := paste(Abundance_reproduction, Description)]
analysis_groups[, key := paste(Abundance_reproduction, Description)]

setdiff(meta_combined$key, analysis_groups$key)
setdiff(analysis_groups$key, meta_combined$key)

analysis_groups

meta_combined.mrg <- merge(meta_combined[, !c("analysis_group")],
                           analysis_groups[, !c("Abundance_reproduction", "Description")],
                           by = "key",
                           all.x = T,
                           allow.cartesian = TRUE)

meta_combined.mrg[is.na(analysis_group)]

meta_combined.mrg[, n_effect_types := uniqueN(original_effect_size),
                  by = .(analysis_group)]
meta_combined.mrg[n_effect_types == 1, ]

# >>> Deal with identical analysis groups (e.g., same number of obs/articles) --------
meta_combined.mrg[, n_articles := uniqueN(Article), by = .(analysis_group)]
meta_combined.mrg[, n_obs := uniqueN(Effect_size_ID), by = .(analysis_group)]
meta_combined.mrg

# >>> Single-type analysis groups: ----------------------------------
one_type <- meta_combined.mrg[n_effect_types == 1, ]

one_type[n_effect_types == 1, `:=` (yi_analysis = yi,
                                    vi_analysis = vi,
                                    analysis_effect_size = original_effect_size)]

# > Convert -------------------------------------------------------------
mixed_types <- meta_combined.mrg[n_effect_types > 1, ]
mixed_types

mixed_types[, .(paste(sort(unique(original_effect_size)), collapse = "; ")),
                by = .(analysis_group)]
#
# For lnOR and Zr, instead of 2 conversions, just convert to SMD. OTherwise, go by most populous entity
#

mixed_types[analysis_group %in% c("Reproduction before-after eradication",
                                  "Reproduction temporal association",
                                  "Reproduction association",
                                  "Reproduction spatial association"),
            analysis_effect_size := "SMD"]

x <- mixed_types[, .(n = .N),
                 by = .(analysis_group, original_effect_size)]
setorder(x, analysis_group)
x

mixed_types[original_effect_size == "SMD" &
              analysis_group == "Abundance temporal association",
            .(Effect_size_ID, Description, key)]

unique(mixed_types$analysis_group)
mixed_types[is.na(analysis_effect_size), 
             analysis_effect_size := fcase(analysis_group %in% c("Abundance association"), "Zr",
                                           analysis_group %in% c("Abundance before-after eradication"), "Zr",
                                           analysis_group %in% c("Abundance on islands with/without cats"), "lnOR",
                                           analysis_group %in% c("Abundance spatial association"), "Zr",
                                           analysis_group %in% c("Abundance temporal association"), "Zr",
                                           analysis_group %in% c("Reproduction association"), "Zr",
                                           analysis_group %in% c("Reproduction before-after eradication"), "Zr",
                                           analysis_group %in% c("Reproduction spatial association"), "lnOR",
                                           analysis_group %in% c("Reproduction temporal association"), "Zr")]
mixed_types[is.na(analysis_effect_size), ]

already_converted <- mixed_types[analysis_effect_size == original_effect_size, ]
to_convert <- mixed_types[analysis_effect_size != original_effect_size, ]

#
to_convert[, key := paste(original_effect_size, analysis_effect_size)]

combos <- unique(to_convert$key)
combos

out <- list()

to_convert

# Fortunately the function won't mind if you provide things that are NA, if they aren't in formula. I dont think.
convert_effect_sizes(formula_path = "scripts/functions/conversion_formulas.csv")
i <- 1

for(i in 1:length(combos)){
  sub_dat <- to_convert[key == combos[i]]
  
  out[[i]] <- convert_effect_sizes(n = n, 
                                   n1 = n1, n2 = n2,
                                   yi = yi, vi = vi, 
                                   r = r,
                                   from = ifelse(unique(sub_dat$original_effect_size) == "Zr", "r", unique(sub_dat$original_effect_size)),
                                   to = unique(sub_dat$analysis_effect_size),
                                   data = sub_dat,
                                   bind = TRUE,
                                   formula_path = "scripts/functions/conversion_formulas.csv")
}

converted <- rbindlist(out)
setnames(converted, c("yi_trans", "vi_trans"), c("yi_analysis", "vi_analysis"))
already_converted[, `:=` (yi_analysis = yi, vi_analysis = vi)]

mixed_type <- rbind(converted, already_converted)

# >>> Recombine -----------------------------------------------------------
meta.final <- rbind(one_type, mixed_type)
meta.final

length(unique(meta.final$Effect_size_ID))

# >>> Make sure there are no duplicate effect sizes per group -------------
# Since some groups are duplicated between the different options, there are duplicate effect size IDs
# Need to add the option to the 'analysis group'

meta.final[, analysis_group := paste(analysis_group, "option", analysis_grouping_option)]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------
# Merge in species body mass / class ------------------------------
# Copy phylacine over:
#
species <- data.table(scientificName = unique(c(meta.final$scientificName)))

species[, spp_name_corrected := scientificName]
species[scientificName == "Pampusana erythroptera", spp_name_corrected := "Gallicolumba erythroptera"]
# species[scientificName == "Prosobonia cancellata", spp_name_corrected := "Prosobonia parvirostris"]

# Mammals:
phyl <- fread("data/trait_databases/phylacine_traits.csv")

phyl[, Binomial.1.2 := gsub("_", " ", Binomial.1.2)]
phyl

species.m1 <- merge(species, phyl[, .(Binomial.1.2, Order.1.2, 
                                      Mass.g)],
                    by.x = "spp_name_corrected", by.y = "Binomial.1.2",
                    all.x = T, all.y = F)
species.m1

# Birds:

avonet <- read_excel("data/trait_databases/AVONET Supplementary dataset 1.xlsx",
                     "AVONET1_BirdLife") |> setDT()
avonet
unique(avonet$Species1)

species.m1[scientificName %in% avonet$Species1]

species.m2 <- merge(species.m1,
                    avonet[, .(Species1, Order1, Mass)],
                    by.x = "spp_name_corrected", by.y = "Species1",
                    all.x = T, all.y = F)

species.m2

species.m2[, Mass_g_final := ifelse(is.na(Mass.g), Mass, Mass.g)]
species.m2[, Order_final := ifelse(is.na(Order.1.2), Order1, Order.1.2)]
species.m2[is.na(Mass_g_final)]

species.m2[, source := fcase(!is.na(Order.1.2), "Phylacine 1.2",
                             !is.na(Order1), "Avonet",
                             is.na(Order.1.2) & is.na(Order1), NA)]

species.m2[, class := fcase(!is.na(Order.1.2), "Mammals",
                             !is.na(Order1), "Birds",
                             is.na(Order.1.2) & is.na(Order1), NA)]


species.m2[is.na(Mass_g_final)]
species.m2[spp_name_corrected == "Chelonoidis niger", `:=` (Order_final = "Testudines",
                                                            Mass_g_final = 80,# weight at birth
                                                            class = "Reptiles",
                                                            source = "IUCN Red List; https://ielc.libguides.com/sdzg/factsheets/galapagostortoises/summary")]
species.m2[spp_name_corrected == "Cyclura carinata", `:=` (Order_final = "Squamata",
                                                            Mass_g_final = 355,
                                                           class = "Reptiles",
                                                            source = "IUCN Red List;  ⁠Alberts, A. (1999) West Indian Iguanas: Status Survey and Conservation Action Plan. IUCN/SSC West Indian Iguana Specialist Group. IUCN, Gland, Switzerland and Cambridge ")]

species.m2[spp_name_corrected == "Gallicolumba erythroptera", `:=` (Order_final = "Columbiformes",
                                                           Mass_g_final = 113.5,
                                                           class = "Birds",
                                                           source = "Gibbs, David; Barnes, Eustace; Cox, John (2001). Pigeons and Doves: A Guide to the Pigeons and Doves of the World. Sussex: Pica Press. ISBN 1-873403-60-7.")]

species.m2[is.na(class), ]

# 8. Merge traits and locations into dataset ---------------------------------
meta.final.mrg <- merge(meta.final,
                           species.m2[, .(scientificName, Order_final, Mass_g_final, class)],
                           by = "scientificName",
                           all.x = T)

nrow(meta.final.mrg) == nrow(meta.final)
# Must be TRUE
meta.final.mrg[duplicated(Effect_size_ID)]
# Must be 0 rows

meta.final.mrg[is.na(class), ]
# 
# # 9. Analysis groups ---------------------------------------------------------
# 
# # Let's create some collapsed categories.
# meta.final.mrg[, analysis_group_collapsed := ifelse(grepl("abundance", analysis_group), "abundance", "reproduction")]
# meta.final.mrg[, analysis_group_collapsed := paste(analysis_group_collapsed, analysis_effect_size, sep = "_")]
# meta.final.mrg[, .(n = uniqueN(Article)), by = .(analysis_group, analysis_effect_size)]
# 
# meta.final.mrg
# # meta.final.mrg[!is.na(yi_smd), .(yi_smd, yi_analysis)]
# 

meta.final.mrg[, .(n_effects = uniqueN(analysis_effect_size)), by = .(analysis_group)]
# All should be 1

# >>> Filter out excluded_species -----------------------------------------
sort(unique(meta.final.mrg$scientificName))
meta.final.mrg <- meta.final.mrg[!scientificName %in% c("Dasyurus maculatus", 
                                                              "Fossa fossana",
                                                              "Cryptoprocta ferox")]



# >>> Check for duplicates by ID ------------------------------------------

meta.final.mrg[duplicated(Effect_size_ID)]
meta.final.mrg[Effect_size_ID == "ES_38a", ]
meta.final.mrg[Effect_size_ID == "ES_38b", ]
meta.final.mrg[Effect_size_ID == "ES_43", ]

# 10 Save --------------------------------------------------------------------
# Let's have arian check the labels:
# 
# meta.final.mrg[, analysis_group_label := fcase(analysis_group_collapsed == "abundance_Zr", "Abundance (across surveys)",
#                                                   analysis_group_collapsed == "abundance_lnOR", "Abundance (across sites)",
#                                                   analysis_group_collapsed == "reproduction_SMD", "Reproductive success (across surveys)")]

fwrite(meta.final.mrg, "builds/meta_analysis/analysis_ready_dataset.csv")

# saveRDS(meta.final.mrg, "builds/meta_analysis/analysis_ready_dataset.Rds")


meta.final.mrg[is.na(yi_analysis)]
