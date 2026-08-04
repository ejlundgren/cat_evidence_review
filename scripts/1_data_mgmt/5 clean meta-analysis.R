# Reviewed July 31st, 2026


# Prepare environment -----------------------------------------------------

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

# Copy convert_effect_sizes() from separate directory:
run <- FALSE
if(run){
  file.copy("../../Meta_Methods_UAlberta/package_development/metatools_dev/scripts/1_functions/convert_effect_sizes.R",
            "scripts/functions/convert_effect_sizes.R", overwrite = T)
  file.copy("../../Meta_Methods_UAlberta/package_development/metatools_dev/data/conversion_formulas.csv",
            "scripts/functions/conversion_formulas.csv", overwrite = T)
}

source("scripts/functions/convert_effect_sizes.R")

# 0. Load data ---------------------------------------------------------------


files <- c("data/meta_analysis/Cat meta-analysis - SMDs.xlsx",
           "data/meta_analysis/Cat meta-analysis ORs.xlsx",
           "data/meta_analysis/Cat meta-analysis - correlations.xlsx")
dat_list <- lapply(files, read_excel)
names(dat_list) <- c("SMDs", "ORs", "Zrs")

# >>> Number of articles by analysis group and effect size -------------------------------------

n.cor <- dat_list[["Zrs"]] %>%
  group_by(analysis_group, Description) %>%
  summarize(n_articles = n_distinct(Article)) %>%
  mutate(native_effect_type = "Zr")

n.smd <- dat_list[["SMDs"]] %>%
  group_by(analysis_group, Description) %>%
  summarize(n_articles = n_distinct(Article)) %>%
  mutate(native_effect_type = "SMD")

n.or <- dat_list[["ORs"]] %>%
  group_by(analysis_group, Description) %>%
  summarize(n_articles = n_distinct(Article)) %>%
  mutate(native_effect_type = "OR")
n.or

ns <- rbind(n.cor, n.smd, n.or)
ns
setDT(ns)

# Going to determine in an excel file and merge in:
fwrite(ns, "builds/temp/determine analysis groups.csv")

# >>> Assign analysis groups & effect size types ----------------------------------------
analysis_groups <- read_excel("data/meta_analysis/analysis groups.xlsx",
                              skip = 0) |> setDT()
analysis_groups

unique(analysis_groups[, .(Description, final_analysis_group1, final_analysis_group2)])

# Calculate native effect size --------------------------------------------

# 1. Zr ---------------------------------------------------------

cor <- dat_list[["Zrs"]] |> setDT()
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
# So these are binary predictor. ALl of these except one can be SMD
# The other can be Zr with 0/1 predictor.
cor[Effect_size_ID == "ES_47b", Cat_correlation_data := cat_presence]
cor[Effect_size_ID == "ES_47b"]

cor.smd <- cor[is.na(Cat_correlation_data), ]
cor.smd$cat_presence
cor.smd[, .(n_points = .N), by = .(cat_presence, Effect_size_ID)]
#' [Might actually do SMD with these.]


# >>> Calculate ZCOR --------------------------------------------------
(cor)

cor.sum <- cor[!is.na(Cat_correlation_data), 
               .(r = get_r(x = Cat_correlation_data,
                           y = Prey_correlation_data),
                   n = .N),
               by = .(Effect_size_ID, scientificName,  Description,
                      unit_of_replication,
                      Article, Article_secondary_same_data, 
                      analysis_group, effect_size_type, Abundance_reproduction,
                      Prey_units, Cat_units, Source, Analysis_comment, Latitude, Longitude, Study_location)]
cor.sum[duplicated(Effect_size_ID)]
cor.sum[Effect_size_ID == "ES_32", Effect_size_ID := paste0(Effect_size_ID, "_", .GRP), by = .(scientificName)]
cor.sum[Effect_size_ID == "ES_47b"]
cor[Effect_size_ID == "ES_47b"]

cor.sum <- escalc(measure = "ZCOR",
                     ri = r, ni = n,
                     data = cor.sum) |> setDT()

length(unique(cor.sum$Effect_size_ID))
length(unique(cor.sum$Article))

names(cor.sum)

unique(cor.sum$analysis_group)
ns

cor.sum[, original_effect_size := "Zr"]
cor.sum$unit_of_replication

# > Convert groups that can actually be SMD --------------------------------------------
unique(cor.smd$analysis_group)
ns
#' [Short-term abundance gets converted to Zr using point-biserial]
#' [Short-term reproduction gets converted to SMD directly ]
cor.smd[analysis_group == "Short-term reproduction", .(n = .N), by = .(cat_presence)]

# OK. 

cor.smd.sum <- cor.smd[, .(mean_prey = mean(Prey_correlation_data),
                                             sd_prey = sd(Prey_correlation_data),
                                             n = .N),
                                         by = .(Effect_size_ID, scientificName, Article, Article_secondary_same_data,
                                                analysis_group,effect_size_type, Description,
                                                unit_of_replication,
                                                Abundance_reproduction, Prey_units, Cat_units, Source,
                                                # Analysis_comment, 
                                                Latitude, Longitude, Study_location,
                                                cat_presence
                                                )]

#' [Transform percent data]
#' 
unique(cor.smd.sum$Effect_size_ID)
cor.smd.sum[Effect_size_ID %in% c("ES_43", "ES_37"),]

cor.smd.sum[Effect_size_ID %in% c("ES_43"), `:=` (sd_prey = sd_prey/100,
                                              mean_prey = mean_prey/100)]

cor.smd.sum[Effect_size_ID %in% c("ES_43", "ES_37"), `:=` 
        (sd_prey = sqrt( sd_prey^2 / 
         ((4*mean_prey) * (1-mean_prey)))
                )]
cor.smd.sum[Effect_size_ID %in% c("ES_43", "ES_37"), mean_prey := asin(sqrt(mean_prey))]
cor.smd.sum

#
cor.smd.wide <- dcast(cor.smd.sum,
                     ... ~ cat_presence,
                     value.var = c("mean_prey", "sd_prey", "n"))

cor.smd.wide
setnames(cor.smd.wide, c("n_1", "n_0"), c("n1", "n2"))

cor.smd.wide
cor.smd.out <- escalc(measure = "SMD", 
                 m1i = mean_prey_1, m2i = mean_prey_0,
                 sd1i = sd_prey_1, sd2i = sd_prey_1, # Shinichi sd2i = sd_prey_0????
                 n1i = n1, n2i = n2,
                 data = cor.smd.wide) |> 
  setDT()
cor.smd.out[, n := n1 + n2] # for conversions, later

cor.smd.out
cor.smd.out[, original_effect_size := "SMD"]

cor.smd.out

# >>> Bind ----------------------------------------------------------------

cor.sum
cor.final <- rbind(cor.sum, 
                   cor.smd.out,
                   fill = T)

cor.final[duplicated(Effect_size_ID)]

# Must be 0 rows
cor.final[Effect_size_ID == "ES_37", ]
cor.final[Effect_size_ID == "ES_37", ]

# 2. SMD ---------------------------------------------------------
names(dat_list)
smd <- dat_list[["SMDs"]] |> setDT()
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

smd.final[Effect_size_ID == "ES_37", ]
smd.final[Effect_size_ID == "ES_ARM", ]

# Drop this effect size that is in SMD dataset
cor.final <- cor.final[Effect_size_ID != "ES_ARM", ]
cor.final

# 3. OR ---------------------------------------------------------
names(dat_list)

or <- dat_list[["ORs"]] |> setDT()
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
meta_combined[Effect_size_ID == "ES_37"]

meta_combined <- meta_combined[!(Effect_size_ID == "ES_ARM" & original_effect_size == "Zr")]
meta_combined

meta_combined[duplicated(Effect_size_ID)]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------
# Explosion merge and convert effect sizes ------------------------------
meta_combined[, key := paste(Abundance_reproduction, Description)]
analysis_groups[, key := paste(Abundance_reproduction, Description)]

analysis_groups.mlt <- melt(analysis_groups,
                        id.vars = c("key"),
                        measure.vars = c("final_analysis_group1", "final_analysis_group2"),
                        variable.name = "analysis_group_level",
                        value.name = "final_analysis_group")
analysis_groups.mlt <- analysis_groups.mlt[!is.na(final_analysis_group)]
analysis_groups.mlt[, analysis_group_level := ifelse(analysis_group_level == "final_analysis_group1",
                                                 "fine_scale", "lumped")]

setdiff(meta_combined$key, analysis_groups.mlt$key)
setdiff(analysis_groups.mlt$key, meta_combined$key)

analysis_groups.mlt[, .(n = uniqueN(final_analysis_group)), by = .(key)]
# 

meta_combined.mrg <- merge(meta_combined[, !c("analysis_group")],
                           unique(analysis_groups.mlt[, .(key, analysis_group_level, final_analysis_group)]),
                           by = "key",
                           allow.cartesian = TRUE,
                           all.x = T)
nrow(meta_combined.mrg)
nrow(meta_combined)

meta_combined.mrg[is.na(final_analysis_group)]

meta_combined.mrg[, n_effect_types := uniqueN(original_effect_size),
                  by = .(final_analysis_group)]
meta_combined.mrg[n_effect_types == 1, ]

setnames(meta_combined.mrg, "final_analysis_group", "analysis_group")

meta_combined.mrg[duplicated(Effect_size_ID)]

# >>> Check unit of replication -------------------------------------------
meta_combined.mrg[, .(n_units = uniqueN(unit_of_replication),
                      units = paste(unique(unit_of_replication), collapse = "; ")), by = .(analysis_group)]
# OK, I think this is fair...

# >>> Single-type analysis groups: ----------------------------------
one_type <- meta_combined.mrg[n_effect_types == 1, ]

one_type[n_effect_types == 1, `:=` (yi_analysis = yi,
                                    vi_analysis = vi,
                                    analysis_effect_size = original_effect_size)]

# > Convert -------------------------------------------------------------
mixed_types <- meta_combined.mrg[n_effect_types > 1, ]
mixed_types

# Determine dominant effect size type per analysis group
mixed_types[, .(paste(sort(unique(original_effect_size)), collapse = "; ")),
                by = .(analysis_group)]

x <- mixed_types[, .(n = .N),
                 by = .(analysis_group, original_effect_size)]
setorder(x, analysis_group)
x

x <- x[, .SD[which.max(n)], by = .(analysis_group)]
x
setnames(x, "original_effect_size", "analysis_effect_size")
x

mixed_types <- merge(mixed_types,
                     x[, .(analysis_group, analysis_effect_size)], 
                     by = "analysis_group",
                     all.x = T)
mixed_types

mixed_types

#
# For lnOR and Zr groups, instead of 2 conversions, just convert to SMD. OTherwise, go by most populous entity
#
mixed_types[analysis_group %in% c( "Reproduction spatial association"),
            analysis_effect_size := "SMD"]


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
                                   from = unique(sub_dat$original_effect_size),
                                   # from = ifelse(unique(sub_dat$original_effect_size) == "Zr" &
                                   #                 !any(is.na(sub_dat$r)), "r", unique(sub_dat$original_effect_size)),
                                   to = unique(sub_dat$analysis_effect_size),
                                   data = sub_dat,
                                   bind = TRUE,
                                   formula_path = "scripts/functions/conversion_formulas.csv")
}
i
sub_dat


converted <- rbindlist(out)
setnames(converted, c("yi_trans", "vi_trans"), c("yi_analysis", "vi_analysis"))
already_converted[, `:=` (yi_analysis = yi, vi_analysis = vi)]

mixed_type <- rbind(converted, already_converted)
mixed_type[is.na(yi_analysis), ]

# >>> Recombine -----------------------------------------------------------
meta.final <- rbind(one_type, mixed_type)
meta.final

length(unique(meta.final$Effect_size_ID))

meta.final

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------
# Merge in covariates ------------------------------
# Copy phylacine over:
#


# >>> Body mass -----------------------------------------------------------


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

meta.final.mrg <- merge(meta.final,
                           species.m2[, .(scientificName, Order_final, Mass_g_final, class)],
                           by = "scientificName",
                           all.x = T)

nrow(meta.final.mrg) == nrow(meta.final)
# Must be TRUE

meta.final.mrg[is.na(class), ]
meta.final.mrg[scientificName == "Prosobonia cancellata",
               `:=` (class = "Birds",
                     Mass_g_final = 33)]
meta.final.mrg[class == "" | is.na(class), ]
meta.final.mrg
unique(meta.final.mrg$class)


meta.final.mrg[, log_mass := log10(Mass_g_final)]

# >>> Filter out excluded_species -----------------------------------------
sort(unique(meta.final.mrg$scientificName))
meta.final.mrg <- meta.final.mrg[!scientificName %in% c("Dasyurus maculatus", 
                                                        "Fossa fossana",
                                                        "Cryptoprocta ferox")]

# >>> Merge in habitat ----------------------------------------------------
habitat <- read_excel("data/meta_analysis/Habitat_use.xlsx") |> setDT()
habitat

names(habitat)
habitat <- habitat[, .(scientificName, locomotion_volant,
                       ground_or_burrow_resting_or_nesting, Foraging_habitat_ground)]
habitat[, `:=` (locomotion_volant = ifelse(locomotion_volant == 0, 
                                           "non_volant", "volant"))]
habitat[, `:=` (ground_or_burrow_resting_or_nesting = ifelse(ground_or_burrow_resting_or_nesting == 0, 
                                                             "ground", "other"))]
habitat[, `:=` (Foraging_habitat_ground = ifelse(Foraging_habitat_ground == 0, 
                                                             "ground", "other"))]
habitat

setdiff(meta.final.mrg$scientificName, habitat$scientificName)

meta.final.mrg2 <- merge(meta.final.mrg,
                        habitat,
                        by = "scientificName")

nrow(meta.final.mrg2) == nrow(meta.final.mrg)
# MUST BE TRUE

# >>> Inside prey range ---------------------------------------------------

meta.final.mrg2[, prey_range_primary := ifelse(Mass_g_final >= 17 & Mass_g_final <= 2000, "inside", "outside")]
meta.final.mrg2[, prey_range_secondary := ifelse(Mass_g_final >= 8 & Mass_g_final <= 2000, "inside", "outside")]
meta.final.mrg2[, prey_range_tertiary := ifelse(Mass_g_final >= 6 & Mass_g_final <= 2650, "inside", "outside")]
meta.final.mrg2

# >>> Continents vs islands -----------------------------------------------
continents <- st_read("data/spatial/4a7d27e1-84a3-4d6a-b4c2-6b6919f3cf4b202034-1-2zg7ul.ht5ut.shp")
continents
# The largest of each is the continent and the rest are islands...

continents <- continents %>%
  st_cast("POLYGON") %>%
  mutate(area = st_area(.)) %>%
  arrange(CONTINENT, -area) %>%
  group_by(CONTINENT) %>%
  mutate(continent_island = c("mainland", rep("island", (n()-1))))
continents

continents %>%
  filter(continent_island == "mainland") %>%
  mapview()

continents %>%
  filter(continent_island != "mainland") %>%
  mapview()

continents %>% filter(CONTINENT == "Oceania")
continents <- continents %>%
  mutate(continent_island = ifelse(CONTINENT == "Oceania", "island", continent_island))

# A few other places aren't really islands...the UK for instance...
continents <- continents %>%
  mutate(poly_id = seq(1:n()))

continents %>%
  filter(continent_island != "mainland") %>%
  mapview()

continents <- continents %>%
  mutate(continent_island = ifelse(poly_id %in% c(),
                                   "mainland", continent_island))

# Well we'll manually adjust studies that are on big islands...
meta.final.mrg2$Longitude
meta.final.mrg2[is.na(as.numeric(Longitude))]
meta.final.mrg2[is.na(as.numeric(Latitude))]

meta.spat <- meta.final.mrg2 %>%
  select(Effect_size_ID, Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = 4326)
meta.spat

meta.spat.jnd <- st_join(meta.spat,
                         continents %>% select(continent_island))
nrow(meta.spat.jnd) == nrow(meta.spat)

meta.spat.jnd
meta.spat.jnd %>%
  filter(is.na(continent_island)) %>% 
  mapview(zcol = "continent_island")
# ALl of tehse are islands

meta.spat.jnd <- meta.spat.jnd %>%
  mutate(continent_island = ifelse(is.na(continent_island), "island", continent_island))

meta.spat.jnd

meta.spat.jnd %>%
  filter(continent_island == "island") %>% 
  mapview(zcol = "continent_island")


meta.spat.jnd %>%
  filter(continent_island == "mainland") %>% 
  mapview(zcol = "continent_island")
# Only australia

meta.spat.jnd <- meta.spat.jnd %>%
  as.data.frame() %>%
  mutate(geometry = NULL) %>% 
  setDT() %>%
  unique()
meta.spat.jnd

meta.final.mrg3 <- merge(meta.final.mrg2,
                        meta.spat.jnd,
                        by = "Effect_size_ID")

nrow(meta.final.mrg3) == nrow(meta.final.mrg2)
# MUST BE TRUE

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------------

# Analysis groups & final prep ---------------------------------------------------------

meta.final.mrg3[, .(n_effects = uniqueN(analysis_effect_size)), by = .(analysis_group)]
# All should be 1

# >>> Check for duplicates by ID ------------------------------------------
meta.final.mrg3 <- unique(meta.final.mrg3)
meta.final.mrg3
meta.final.mrg3[duplicated(paste(Effect_size_ID, analysis_group))]
# Must be 0 rows

nrow(meta.final.mrg3) == nrow(meta.final.mrg)
# Must be TRUE

# 10 Save --------------------------------------------------------------------

fwrite(meta.final.mrg3, "builds/meta_analysis/analysis_ready_dataset.csv")

