

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
  group_by(analysis_group) %>%
  summarize(n_articles = n_distinct(Article)) %>%
  mutate(native_effect_type = "Zr")

n.smd <- dat_list[["SMDs.xlsx"]] %>%
  group_by(analysis_group) %>%
  summarize(n_articles = n_distinct(Article)) %>%
  mutate(native_effect_type = "SMD")

n.or <- dat_list[["ORs.xlsx"]] %>%
  group_by(analysis_group) %>%
  summarize(n_articles = n_distinct(Article)) %>%
  mutate(native_effect_type = "OR")
n.or

ns <- rbind(n.cor, n.smd, n.or)
ns
setDT(ns)

# >>> Strategy: -----------------------------------------------------------
# Converting based on dominant effect size

ns[, analysis_effect_size := fcase(analysis_group == "Short-term abundance", "Zr",
                                   analysis_group %in% c("Short-term reproduction",
                                                         "Before-after eradication reproduction"), "SMD",
                                   analysis_group %in% c("Long-term abundance"), "OR"
                         )]

# 1. Zr ---------------------------------------------------------

cor <- dat_list[["correlations.xlsx"]] |> setDT()
cor

sort(unique(cor$Effect_size_ID))

# >>> Clean -------------------------------------------------------------------
unique(cor$Description)

# This actually looks pretty good. Are there columns that need to be dropped in order to make it one row per ES_ID?
cor[Description == "Spatial correlation" & Effect_size_ID == "ES_3", ]

unique(cor[, !c("Cat_correlation_data", "Prey_correlation_data", 
                "Time", "Time_details", "Site", "Site_details", "scientificName")])[, 
              .(n = .N), by = .(Effect_size_ID)][n > 1]

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
               by = .(Effect_size_ID, scientificName, 
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

setnames(cor.sum, c("yi", "vi"), c("yi_Zr", "vi_Zr"))

length(unique(cor.sum$Effect_size_ID))
length(unique(cor.sum$Article))


names(cor.sum)

unique(cor.sum$analysis_group)
ns

cor.sum[, original_effect_size := "Zr"]

cor.zr <- cor.sum[analysis_group == "Short-term abundance", ]
cor.zr[analysis_group == "Short-term abundance", `:=` (yi_analysis = yi_Zr,
                                                        vi_analysis = vi_Zr,
                                                       analysis_effect_size = "Zr")]

# >>> Convert Zr reproduction to SMD --------------------------------------
cor.smd <- cor.sum[analysis_group == "Short-term reproduction", ]

cor.smd <- convert_effect_sizes(from = "r",
                     to = "SMD",
                     r = r, n = n,
                     bind = TRUE,
                     formula_path = "scripts/functions/conversion_formulas.csv",
                     data = cor.smd) |> setDT()
setnames(cor.smd, c("yi_trans", "vi_trans"), c("yi_analysis", "vi_analysis"))
cor.smd[, analysis_effect_size := "SMD"]
cor.smd$analysis_group

# > Convert binary Zr groups --------------------------------------------
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
                                                analysis_group,effect_size_type,
                                                Abundance_reproduction, Prey_units, Cat_units, Source,
                                                # Analysis_comment, 
                                                Latitude, Longitude, Study_location,
                                                cat_presence
                                                )]

point_biserial.summary.wide <- dcast(point_biserial.summary,
                                     ... ~ cat_presence,
                                     value.var = c("mean_prey", "sd_prey", "n"))
point_biserial.summary.wide

# "Short-term reproduction are actually going to be analyzed as SMD. So let's just calculate SMD from them.
zpb_out <- escalc(measure = "ZPB", 
                 m1i = mean_prey_1, m2i = mean_prey_0,
                 sd1i = sd_prey_1, sd2i = sd_prey_1,
                 n1i = n_1, n2i = n_0,
                 data = point_biserial.summary.wide[Abundance_reproduction != "Reproduction"]) |> 
  setDT()
setnames(zpb_out,
         c("yi", "vi"), c("yi_Zr", "vi_Zr"))
zpb_out[, `:=` (yi_analysis = yi_Zr, vi_analysis = vi_Zr, analysis_effect_size = "Zr")]
zpb_out
zpb_out[, original_effect_size := "Zr"]

# Now SMD on reproduction data:

smd_reproduction <- escalc(measure = "SMD", 
                            m1i = mean_prey_1, m2i = mean_prey_0,
                            sd1i = sd_prey_1, sd2i = sd_prey_1,
                            n1i = n_1, n2i = n_0,
                            data = point_biserial.summary.wide[Abundance_reproduction == "Reproduction"]) |> 
  setDT()
setnames(smd_reproduction,
         c("yi", "vi"), c("yi_smd", "vi_smd"))
smd_reproduction

smd_reproduction[, `:=` (yi_analysis = yi_smd, vi_analysis = vi_smd, analysis_effect_size = "SMD",
                         original_effect_size = "SMD")]
#
smd_reproduction

# >>> Bind ----------------------------------------------------------------

cor.final <- rbind(cor.zr, cor.smd, zpb_out, smd_reproduction, fill = T)

cor.final[is.na(yi_analysis)]

cor.final[, .(n = .N), by = .(analysis_group, analysis_effect_size, original_effect_size)]

# 2. SMD ---------------------------------------------------------
names(dat_list)
smd <- dat_list[["SMDs.xlsx"]] |> setDT()

# >>> Clean -------------------------------------------------------------------
smd
setnames(smd, "...30", "analysis_group_notes")
smd

# what's the error type? Assume SD for now.
smd

unique(smd$Effect_size_ID)
smd[Effect_size_ID == "ES_45", ]
# smd[Effect_size_ID == "ES_50", `:=` (Sample_size_overall_cats_present = 19,
#                                      Sample_size_overall_cats_Absent = )]
# ES44, 45, 46, ES_50 <- convert these to OR

# smd[Effect_size_ID %in% c("ES_44", "ES_45", "ES_46", "ES_50"), converted_to := "lnOR"]
# smd[!Effect_size_ID %in% c("ES_44", "ES_45", "ES_46", "ES_50"), converted_to := "Zr"]
# smd

# >>> Calculate effect sizes --------------------------------------------------

smd <- escalc(measure = "SMD",
              m2i = Prey_mean_cats_Absent, m1i = Prey_mean_cats_Present,
              sd2i = Prey_error_cats_Absent, sd1i = Prey_error_cats_Present,
              n2i = Sample_size_overall_cats_Absent, n1i = Sample_size_overall_cats_Present,
              data = smd) |> setDT()
smd[, .(Prey_mean_cats_Absent, Prey_mean_cats_Present, yi)]
setnames(smd, c("yi", 'vi'), c("yi_smd", "vi_smd"))

smd[, original_effect_size := "SMD"]

# smd <- rbind(smd)
smd$analysis_group
ns
# So we convert this to OR

# smd[converted_to == "Zr"]

# >>> Convert -------------------------------------------------------------
convert_effect_sizes(formula_path = "scripts/functions/conversion_formulas.csv")
# 

smd_final <- convert_effect_sizes(from = "SMD",
                                     to = "lnOR",
                                     yi = yi_smd, vi = vi_smd,
                                     n1 = Sample_size_overall_cats_Absent,
                                     n2 = Sample_size_overall_cats_Present,
                                     data = smd,
                                     bind = TRUE,
                                  formula_path = "scripts/functions/conversion_formulas.csv")
setnames(smd_final, c("yi_trans", "vi_trans"), c("yi_analysis", "vi_analysis"))


smd_final[, analysis_effect_size := "lnOR"]
smd_final


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
setnames(or, c("yi", "vi"), c("yi_OR", "vi_OR"))
or[, n1 := Prey_and_cat_positive + Prey_negative_cat_positive]
or[, n2 := Prey_positive_cat_negative + Prey_and_cat_negative]
or[, original_effect_size := "lnOR"]

or.or <- or[analysis_group != "Before-after eradication reproduction", ]

or.or[, `:=` (yi_analysis = yi_OR, vi_analysis = vi_OR, analysis_effect_size = "lnOR")]

# >>> Convert reproduction to SMD -----------------------------------------

or.smd <- or[analysis_group == "Before-after eradication reproduction", ]
or.smd
convert_effect_sizes(formula_path = "scripts/functions/conversion_formulas.csv")

or.smd <- convert_effect_sizes(from = "lnOR", to = "SMD", 
                               yi = yi_OR, vi = vi_OR, n1 = n1, n2 = n2,
                               data = or.smd, bind = TRUE,
                               formula_path = "scripts/functions/conversion_formulas.csv")
setnames(or.smd, c("yi_trans", "vi_trans"), c("yi_analysis", "vi_analysis"))
or.smd[, analysis_effect_size := "SMD"]

or.final <- rbind(or.or,
                  or.smd)
or.final

# 5. Bind effect sizes ----------------------------------------------------

cor.final
smd_final
or.final


ns

meta_combined <- rbind(cor.final, smd_final, or.final, fill = T)
meta_combined[is.na(yi_analysis), ]

meta_combined$analysis_effect_size
meta_combined$original_effect_size

# 6. Species body mass / class ------------------------------
# Copy phylacine over:
#
species <- data.table(scientificName = unique(c(meta_combined$scientificName)))

species[, spp_name_corrected := scientificName]
species[scientificName == "Pampusana erythroptera", spp_name_corrected := "Gallicolumba erythroptera"]
species[scientificName == "Prosobonia cancellata", spp_name_corrected := "Prosobonia parvirostris"]

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
meta_combined.mrg <- merge(meta_combined,
                           species.m2[, .(scientificName, Order_final, Mass_g_final, class)],
                           by = "scientificName",
                           all.x = T)

nrow(meta_combined.mrg) == nrow(meta_combined)
# Must be TRUE
meta_combined.mrg[duplicated(Effect_size_ID)]
# Must be 0 rows

meta_combined.mrg[is.na(class), ]

# 9. Analysis groups ---------------------------------------------------------

# Let's create some collapsed categories.
meta_combined.mrg[, analysis_group_collapsed := ifelse(grepl("abundance", analysis_group), "abundance", "reproduction")]
meta_combined.mrg[, analysis_group_collapsed := paste(analysis_group_collapsed, analysis_effect_size, sep = "_")]
meta_combined.mrg[, .(n = uniqueN(Article)), by = .(analysis_group, analysis_effect_size)]

meta_combined.mrg
# meta_combined.mrg[!is.na(yi_smd), .(yi_smd, yi_analysis)]


# >>> Filter out excluded_species -----------------------------------------
sort(unique(meta_combined.mrg$scientificName))
meta_combined.mrg <- meta_combined.mrg[!scientificName %in% c("Dasyurus maculatus", 
                                                              "Fossa fossana",
                                                              "Cryptoprocta ferox")]



# >>> Check for duplicates by ID ------------------------------------------

meta_combined.mrg[duplicated(Effect_size_ID)]
meta_combined.mrg[Effect_size_ID == "ES_38a", ]
meta_combined.mrg[Effect_size_ID == "ES_38b", ]
meta_combined.mrg[Effect_size_ID == "ES_43", ]

# 10 Save --------------------------------------------------------------------
# Let's have arian check the labels:

meta_combined.mrg[, analysis_group_label := fcase(analysis_group_collapsed == "abundance_Zr", "Abundance (across surveys)",
                                                  analysis_group_collapsed == "abundance_lnOR", "Abundance (across sites)",
                                                  analysis_group_collapsed == "reproduction_SMD", "Reproductive success (across surveys)")]

fwrite(meta_combined.mrg, "builds/meta_analysis/analysis_ready_dataset.csv")

# saveRDS(meta_combined.mrg, "builds/meta_analysis/analysis_ready_dataset.Rds")


meta_combined.mrg[is.na(yi_analysis)]
