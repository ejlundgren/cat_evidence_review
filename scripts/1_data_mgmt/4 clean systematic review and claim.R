#
# Tidy up the systematic review tallies and the claims
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
library("sf")
library("mapview")
library("dplyr")
# Functions

# Systematic review -------------------------------------------------------


dat <- read_excel("data/systematic_review/Cat_Database.xlsx",
                  sheet = "Evidence") |> setDT()
dat


# >>> Fix some taxonomy -------------------------------------------------------

dat[, spp_name_corrected := scientificName]
dat[scientificName == "Pampusana erythroptera", spp_name_corrected := "Gallicolumba erythroptera"]
dat[scientificName == "Prosobonia cancellata", spp_name_corrected := "Prosobonia parvirostris"]


# >>> Get class ---------------------------------------------------------------


# Mammals:
phyl <- fread("data/trait_databases/phylacine_traits.csv")
phyl[, Binomial.1.2 := gsub("_", " ", Binomial.1.2)]
phyl

dat.m1 <- merge(dat, phyl[, .(Binomial.1.2, Order.1.2, 
                                      Mass.g)],
                    by.x = "spp_name_corrected", by.y = "Binomial.1.2",
                    all.x = T, all.y = F)
dat.m1

# Birds
avonet <- read_excel("data/trait_databases//AVONET Supplementary dataset 1.xlsx",
                     "AVONET1_BirdLife") |> setDT() #AVONET1_BirdLife
avonet
unique(avonet$Species1)
dat.m1[scientificName %in% avonet$Species1]

dat.m2 <- merge(dat.m1,
                    avonet[, .(Species1, Order1, Mass)],
                    by.x = "spp_name_corrected", by.y = "Species1",
                    all.x = T, all.y = F)

dat.m2

dat.m2[, Mass_g_final := ifelse(is.na(Mass.g), Mass, Mass.g)]
dat.m2[, Order_final := ifelse(is.na(Order.1.2), Order1, Order.1.2)]
dat.m2[is.na(Mass_g_final)]

dat.m2[, source := fcase(!is.na(Order.1.2), "Phylacine 1.2",
                             !is.na(Order1), "Avonet",
                             is.na(Order.1.2) & is.na(Order1), NA)]

dat.m2[, class := fcase(!is.na(Order.1.2), "Mammals",
                            !is.na(Order1), "Birds",
                            is.na(Order.1.2) & is.na(Order1), NA)]


dat.m2[is.na(Mass_g_final)]
dat.m2[spp_name_corrected == "Chelonoidis niger", `:=` (Order_final = "Testudines",
                                                            Mass_g_final = 80,# weight at birth
                                                            class = "Reptiles")]
dat.m2[spp_name_corrected == "Cyclura carinata", `:=` (Order_final = "Squamata",
                                                           Mass_g_final = 355,
                                                           class = "Reptiles")]

dat.m2[spp_name_corrected == "Gallicolumba erythroptera", `:=` (Order_final = "Columbiformes",
                                                                    Mass_g_final = 113.5,
                                                                    class = "Birds")]

unique(dat.m2[is.na(class), ]$spp_name_corrected)

dat.m2[spp_name_corrected == "Amblyrhynchus cristatus", `:=` (Order_final = "Squamata",
                                                                Mass_g_final = NA,
                                                                class = "Reptilia")]

dat.m2[spp_name_corrected == "Ameiva provitaae", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Antillotyphlops monastus", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Atelopus guanujo", `:=` (Order_final = "Anura",
                                              Mass_g_final = NA,
                                              class = "Amphibia")]

dat.m2[spp_name_corrected == "Brachylophus vitiensis", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Caledoniscincus aquilonius", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Caledoniscincus auratus", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Caledoniscincus orestes", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Celatiscincus similis", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Chilabothrus chrysogaster", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Chilabothrus subflavus", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Cnemaspis thachanaensis", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Coenocorypha barrierensis", `:=` (Order_final = "Charadriiformes",
                                              Mass_g_final = NA,
                                              class = "Birds")]

dat.m2[spp_name_corrected == "Conolophus subcristatus", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Crotalus catalinensis", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Ctenosaura oedirhina", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Cyclura collei", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Cyclura lewisi", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Cyclura pinguis", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Cyclura stejnegeri", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Cyrtodactylus soba", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Emoia loyaltiensis", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Gallotia intermedia", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Gallotia simonyi", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Kanakysaurus viviparus", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Kanakysaurus zebratus", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Leiocephalus psammodromus", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Lerista puncticauda", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Liolaemus martorii", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Liolaemus paulinae", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Liopholis kintorei", `:=` (Order_final = "Squamata",
                                              Mass_g_final = NA,
                                              class = "Reptilia")]


dat.m2[spp_name_corrected == "Lioscincus steindachneri", `:=` (Order_final = "Squamata",
                                                         Mass_g_final = NA,
                                                         class = "Reptilia")]


dat.m2[spp_name_corrected == "Lioscincus vivae", `:=` (Order_final = "Squamata",
                                             Mass_g_final = NA,
                                             class = "Reptilia")]

dat.m2[spp_name_corrected == "Litoria raniformis", `:=` (Order_final = "Anura",
                                             Mass_g_final = NA,
                                             class = "Amphibia")]
dat.m2[spp_name_corrected == "Lygodactylus insularis", `:=` (Order_final = "Squamata",
                                             Mass_g_final = NA,
                                             class = "Reptilia")]
dat.m2[spp_name_corrected == "Marmorosphax taom", `:=` (Order_final = "Squamata",
                                             Mass_g_final = NA,
                                             class = "Reptilia")]
dat.m2[spp_name_corrected == "Microgoura meeki", `:=` (Order_final = "Columbiformes",
                                             Mass_g_final = NA,
                                             class = "Birds")]
dat.m2[spp_name_corrected == "Mundia elpenor", `:=` (Order_final = "Gruiformes",
                                             Mass_g_final = NA,
                                             class = "Birds")]
dat.m2[spp_name_corrected == "Nannoscincus hanchisteus", `:=` (Order_final = "Squamata",
                                             Mass_g_final = NA,
                                             class = "Reptilia")]
dat.m2[spp_name_corrected == "Nesoenas duboisi", `:=` (Order_final = "Columbiformes",
                                             Mass_g_final = NA,
                                             class = "Birds")]
dat.m2[spp_name_corrected == "Oligosoma grande", `:=` (Order_final = "Squamata",
                                             Mass_g_final = NA,
                                             class = "Reptilia")]
dat.m2[spp_name_corrected == "Oligosoma otagense", `:=` (Order_final = "Squamata",
                                             Mass_g_final = NA,
                                             class = "Reptilia")]
dat.m2[spp_name_corrected == "Pezophaps solitaria", `:=` (Order_final = "Columbiformes",
                                             Mass_g_final = NA,
                                             class = "Birds")]
dat.m2[spp_name_corrected == "Phasmasaurus maruia", `:=` (Order_final = "Squamata",
                                             Mass_g_final = NA,
                                             class = "Reptilia")]
dat.m2[spp_name_corrected == "Phasmasaurus tillieri", `:=` (Order_final = "Squamata",
                                             Mass_g_final = NA,
                                             class = "Reptilia")]
dat.m2[spp_name_corrected == "Plestiodon longirostris", `:=` (Order_final = "Squamata",
                                             Mass_g_final = NA,
                                             class = "Reptilia")]
dat.m2[spp_name_corrected == "Podarcis lilfordi", `:=` (Order_final = "Squamata",
                                             Mass_g_final = NA,
                                             class = "Reptilia")]
dat.m2[spp_name_corrected == "Pterodroma rupinarum", `:=` (Order_final = "Procellariiformes",
                                             Mass_g_final = NA,
                                             class = "Birds")]
dat.m2[spp_name_corrected == "Pteropus vetula", `:=` (Order_final = "Chiroptera",
                                             Mass_g_final = NA,
                                             class = "Mammal")]
dat.m2[spp_name_corrected == "Raorchestes primarrumpfi", `:=` (Order_final = "Anura",
                                             Mass_g_final = NA,
                                             class = "Amphibia")]
dat.m2[spp_name_corrected == "Raorchestes tinniens", `:=` (Order_final = "Anura",
                                             Mass_g_final = NA,
                                             class = "Amphibia")]
dat.m2[spp_name_corrected == "Sauromalus klauberi", `:=` (Order_final = "Squamata",
                                             Mass_g_final = NA,
                                             class = "Reptilia")]
dat.m2[spp_name_corrected == "Strigops habroptilus", `:=` (Order_final = "Psittaciformes",
                                             Mass_g_final = NA,
                                             class = "Birds")]
dat.m2[spp_name_corrected == "Traversia lyalli", `:=` (Order_final = "Passeriformes",
                                             Mass_g_final = NA,
                                             class = "Birds")]
dat.m2[spp_name_corrected == "Tropidophis schwartzi", `:=` (Order_final = "Squamata",
                                             Mass_g_final = NA,
                                             class = "Reptilia")]
dat.m2[spp_name_corrected == "Urosaurus auriculatus", `:=` (Order_final = "Squamata",
                                             Mass_g_final = NA,
                                             class = "Reptilia")]

# 
unique(dat.m2$class)

dat.m2[class %in% c("Reptilia", "Reptiles"), class := "Reptiles"]
dat.m2[class == "Amphibia", class := "Amphibians"]
dat.m2[class %in% c("Mammal", "Mammals"), class := "Mammals"]

dat.m2[class == "Amphibians"]
setdiff(dat.m2$scientificName, dat$scientificName)
setdiff(dat$scientificName, dat.m2$scientificName)


# >>> Filter and save ---------------------------------------------------------

unique(dat.m2$evidence_type)

dat.m2[evidence_type == "Control program in support", evidence_type := "Population in support"]
dat.m2[evidence_type == "Control program not in support", evidence_type := "Population not in support"]

dat.m2 <- dat.m2[!grepl("Overlap", evidence_type)]
dat.m2 <- dat.m2[!grepl("Disease", evidence_type)]


dat.m2 <- dat.m2[!grepl("Lethal program", evidence_type)]
dat.m2 <- dat.m2[!grepl("Lethal program", evidence_type)]
unique(dat.m2$evidence_type)

dat.m2 <- dat.m2[!grepl("Naivety", evidence_type)]
dat.m2 <- dat.m2[!grepl("Naivety", evidence_type)]

dat.m2
dat.m2 <- dat.m2[, .(scientificName, spp_name_corrected, Article_simple,
                     study_id,
                     Study_location, Study_lat, Study_long,
                     Evidence_category, Evidence_effect, Evidence_method,
                     Hypothesis_supported, has_data, of_quality,
                     evidence_type, exclude_species, Mass_g_final, Order_final,
                     source, class)]
fwrite(dat.m2, "builds/systematic_review/systematic_review_tidy.csv")


# Claims -------------------------------------------------------


dat <- read_excel("data/systematic_review/Cat_Database.xlsx",
                  sheet = "Species Claims") |> setDT()
dat


# >>> Fix some taxonomy -------------------------------------------------------

dat[, spp_name_corrected := scientificName]
dat[scientificName == "Pampusana erythroptera", spp_name_corrected := "Gallicolumba erythroptera"]
dat[scientificName == "Prosobonia cancellata", spp_name_corrected := "Prosobonia parvirostris"]


# >>> Get class ---------------------------------------------------------------

# Mammals:
phyl <- fread("data/trait_databases/phylacine_traits.csv")
phyl[, Binomial.1.2 := gsub("_", " ", Binomial.1.2)]
phyl

dat.m1 <- merge(dat, phyl[, .(Binomial.1.2, Order.1.2, 
                              Mass.g)],
                by.x = "spp_name_corrected", by.y = "Binomial.1.2",
                all.x = T, all.y = F)
dat.m1

# Birds
avonet <- read_excel("data/trait_databases/AVONET Supplementary dataset 1.xlsx",
                     "AVONET1_BirdLife") |> setDT() #AVONET1_BirdLife
avonet
unique(avonet$Species1)
dat.m1[scientificName %in% avonet$Species1]

dat.m2 <- merge(dat.m1,
                avonet[, .(Species1, Order1, Mass)],
                by.x = "spp_name_corrected", by.y = "Species1",
                all.x = T, all.y = F)

dat.m2

dat.m2[, Mass_g_final := ifelse(is.na(Mass.g), Mass, Mass.g)]
dat.m2[, Order_final := ifelse(is.na(Order.1.2), Order1, Order.1.2)]
dat.m2[is.na(Mass_g_final)]

dat.m2[, source := fcase(!is.na(Order.1.2), "Phylacine 1.2",
                         !is.na(Order1), "Avonet",
                         is.na(Order.1.2) & is.na(Order1), NA)]

dat.m2[, class := fcase(!is.na(Order.1.2), "Mammals",
                        !is.na(Order1), "Birds",
                        is.na(Order.1.2) & is.na(Order1), NA)]


dat.m2[is.na(class)]$spp_name_corrected

dat.m2[spp_name_corrected == "Chelonoidis niger", `:=` (Order_final = "Testudines",
                                                        Mass_g_final = 80,# weight at birth
                                                        class = "Reptiles")]
dat.m2[spp_name_corrected == "Cyclura carinata", `:=` (Order_final = "Squamata",
                                                       Mass_g_final = 355,
                                                       class = "Reptiles")]

dat.m2[spp_name_corrected == "Gallicolumba erythroptera", `:=` (Order_final = "Columbiformes",
                                                                Mass_g_final = 113.5,
                                                                class = "Birds")]

unique(dat.m2[is.na(class), ]$spp_name_corrected)

dat.m2[spp_name_corrected == "Amblyrhynchus cristatus", `:=` (Order_final = "Squamata",
                                                              Mass_g_final = NA,
                                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Ameiva provitaae", `:=` (Order_final = "Squamata",
                                                       Mass_g_final = NA,
                                                       class = "Reptilia")]

dat.m2[spp_name_corrected == "Antillotyphlops monastus", `:=` (Order_final = "Squamata",
                                                               Mass_g_final = NA,
                                                               class = "Reptilia")]

dat.m2[spp_name_corrected == "Atelopus guanujo", `:=` (Order_final = "Anura",
                                                       Mass_g_final = NA,
                                                       class = "Amphibia")]

dat.m2[spp_name_corrected == "Brachylophus vitiensis", `:=` (Order_final = "Squamata",
                                                             Mass_g_final = NA,
                                                             class = "Reptilia")]

dat.m2[spp_name_corrected == "Caledoniscincus aquilonius", `:=` (Order_final = "Squamata",
                                                                 Mass_g_final = NA,
                                                                 class = "Reptilia")]

dat.m2[spp_name_corrected == "Caledoniscincus auratus", `:=` (Order_final = "Squamata",
                                                              Mass_g_final = NA,
                                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Caledoniscincus orestes", `:=` (Order_final = "Squamata",
                                                              Mass_g_final = NA,
                                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Celatiscincus similis", `:=` (Order_final = "Squamata",
                                                            Mass_g_final = NA,
                                                            class = "Reptilia")]

dat.m2[spp_name_corrected == "Chilabothrus chrysogaster", `:=` (Order_final = "Squamata",
                                                                Mass_g_final = NA,
                                                                class = "Reptilia")]

dat.m2[spp_name_corrected == "Chilabothrus subflavus", `:=` (Order_final = "Squamata",
                                                             Mass_g_final = NA,
                                                             class = "Reptilia")]

dat.m2[spp_name_corrected == "Cnemaspis thachanaensis", `:=` (Order_final = "Squamata",
                                                              Mass_g_final = NA,
                                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Coenocorypha barrierensis", `:=` (Order_final = "Charadriiformes",
                                                                Mass_g_final = NA,
                                                                class = "Birds")]

dat.m2[spp_name_corrected == "Conolophus subcristatus", `:=` (Order_final = "Squamata",
                                                              Mass_g_final = NA,
                                                              class = "Reptilia")]

dat.m2[spp_name_corrected == "Crotalus catalinensis", `:=` (Order_final = "Squamata",
                                                            Mass_g_final = NA,
                                                            class = "Reptilia")]

dat.m2[spp_name_corrected == "Ctenosaura oedirhina", `:=` (Order_final = "Squamata",
                                                           Mass_g_final = NA,
                                                           class = "Reptilia")]

dat.m2[spp_name_corrected == "Cyclura collei", `:=` (Order_final = "Squamata",
                                                     Mass_g_final = NA,
                                                     class = "Reptilia")]

dat.m2[spp_name_corrected == "Cyclura lewisi", `:=` (Order_final = "Squamata",
                                                     Mass_g_final = NA,
                                                     class = "Reptilia")]

dat.m2[spp_name_corrected == "Cyclura pinguis", `:=` (Order_final = "Squamata",
                                                      Mass_g_final = NA,
                                                      class = "Reptilia")]

dat.m2[spp_name_corrected == "Cyclura stejnegeri", `:=` (Order_final = "Squamata",
                                                         Mass_g_final = NA,
                                                         class = "Reptilia")]

dat.m2[spp_name_corrected == "Cyrtodactylus soba", `:=` (Order_final = "Squamata",
                                                         Mass_g_final = NA,
                                                         class = "Reptilia")]

dat.m2[spp_name_corrected == "Emoia loyaltiensis", `:=` (Order_final = "Squamata",
                                                         Mass_g_final = NA,
                                                         class = "Reptilia")]

dat.m2[spp_name_corrected == "Gallotia intermedia", `:=` (Order_final = "Squamata",
                                                          Mass_g_final = NA,
                                                          class = "Reptilia")]

dat.m2[spp_name_corrected == "Gallotia simonyi", `:=` (Order_final = "Squamata",
                                                       Mass_g_final = NA,
                                                       class = "Reptilia")]

dat.m2[spp_name_corrected == "Kanakysaurus viviparus", `:=` (Order_final = "Squamata",
                                                             Mass_g_final = NA,
                                                             class = "Reptilia")]

dat.m2[spp_name_corrected == "Kanakysaurus zebratus", `:=` (Order_final = "Squamata",
                                                            Mass_g_final = NA,
                                                            class = "Reptilia")]

dat.m2[spp_name_corrected == "Leiocephalus psammodromus", `:=` (Order_final = "Squamata",
                                                                Mass_g_final = NA,
                                                                class = "Reptilia")]

dat.m2[spp_name_corrected == "Lerista puncticauda", `:=` (Order_final = "Squamata",
                                                          Mass_g_final = NA,
                                                          class = "Reptilia")]

dat.m2[spp_name_corrected == "Liolaemus martorii", `:=` (Order_final = "Squamata",
                                                         Mass_g_final = NA,
                                                         class = "Reptilia")]

dat.m2[spp_name_corrected == "Liolaemus paulinae", `:=` (Order_final = "Squamata",
                                                         Mass_g_final = NA,
                                                         class = "Reptilia")]

dat.m2[spp_name_corrected == "Liopholis kintorei", `:=` (Order_final = "Squamata",
                                                         Mass_g_final = NA,
                                                         class = "Reptilia")]


dat.m2[spp_name_corrected == "Lioscincus steindachneri", `:=` (Order_final = "Squamata",
                                                               Mass_g_final = NA,
                                                               class = "Reptilia")]


dat.m2[spp_name_corrected == "Lioscincus vivae", `:=` (Order_final = "Squamata",
                                                       Mass_g_final = NA,
                                                       class = "Reptilia")]

dat.m2[spp_name_corrected == "Litoria raniformis", `:=` (Order_final = "Anura",
                                                         Mass_g_final = NA,
                                                         class = "Amphibia")]
dat.m2[spp_name_corrected == "Lygodactylus insularis", `:=` (Order_final = "Squamata",
                                                             Mass_g_final = NA,
                                                             class = "Reptilia")]
dat.m2[spp_name_corrected == "Marmorosphax taom", `:=` (Order_final = "Squamata",
                                                        Mass_g_final = NA,
                                                        class = "Reptilia")]
dat.m2[spp_name_corrected == "Microgoura meeki", `:=` (Order_final = "Columbiformes",
                                                       Mass_g_final = NA,
                                                       class = "Birds")]
dat.m2[spp_name_corrected == "Mundia elpenor", `:=` (Order_final = "Gruiformes",
                                                     Mass_g_final = NA,
                                                     class = "Birds")]
dat.m2[spp_name_corrected == "Nannoscincus hanchisteus", `:=` (Order_final = "Squamata",
                                                               Mass_g_final = NA,
                                                               class = "Reptilia")]
dat.m2[spp_name_corrected == "Nesoenas duboisi", `:=` (Order_final = "Columbiformes",
                                                       Mass_g_final = NA,
                                                       class = "Birds")]
dat.m2[spp_name_corrected == "Oligosoma grande", `:=` (Order_final = "Squamata",
                                                       Mass_g_final = NA,
                                                       class = "Reptilia")]
dat.m2[spp_name_corrected == "Oligosoma otagense", `:=` (Order_final = "Squamata",
                                                         Mass_g_final = NA,
                                                         class = "Reptilia")]
dat.m2[spp_name_corrected == "Pezophaps solitaria", `:=` (Order_final = "Columbiformes",
                                                          Mass_g_final = NA,
                                                          class = "Birds")]
dat.m2[spp_name_corrected == "Phasmasaurus maruia", `:=` (Order_final = "Squamata",
                                                          Mass_g_final = NA,
                                                          class = "Reptilia")]
dat.m2[spp_name_corrected == "Phasmasaurus tillieri", `:=` (Order_final = "Squamata",
                                                            Mass_g_final = NA,
                                                            class = "Reptilia")]
dat.m2[spp_name_corrected == "Plestiodon longirostris", `:=` (Order_final = "Squamata",
                                                              Mass_g_final = NA,
                                                              class = "Reptilia")]
dat.m2[spp_name_corrected == "Podarcis lilfordi", `:=` (Order_final = "Squamata",
                                                        Mass_g_final = NA,
                                                        class = "Reptilia")]
dat.m2[spp_name_corrected == "Pterodroma rupinarum", `:=` (Order_final = "Procellariiformes",
                                                           Mass_g_final = NA,
                                                           class = "Birds")]
dat.m2[spp_name_corrected == "Pteropus vetula", `:=` (Order_final = "Chiroptera",
                                                      Mass_g_final = NA,
                                                      class = "Mammal")]
dat.m2[spp_name_corrected == "Raorchestes primarrumpfi", `:=` (Order_final = "Anura",
                                                               Mass_g_final = NA,
                                                               class = "Amphibia")]
dat.m2[spp_name_corrected == "Raorchestes tinniens", `:=` (Order_final = "Anura",
                                                           Mass_g_final = NA,
                                                           class = "Amphibia")]
dat.m2[spp_name_corrected == "Sauromalus klauberi", `:=` (Order_final = "Squamata",
                                                          Mass_g_final = NA,
                                                          class = "Reptilia")]
dat.m2[spp_name_corrected == "Strigops habroptilus", `:=` (Order_final = "Psittaciformes",
                                                           Mass_g_final = NA,
                                                           class = "Birds")]
dat.m2[spp_name_corrected == "Traversia lyalli", `:=` (Order_final = "Passeriformes",
                                                       Mass_g_final = NA,
                                                       class = "Birds")]
dat.m2[spp_name_corrected == "Tropidophis schwartzi", `:=` (Order_final = "Squamata",
                                                            Mass_g_final = NA,
                                                            class = "Reptilia")]
dat.m2[spp_name_corrected == "Urosaurus auriculatus", `:=` (Order_final = "Squamata",
                                                            Mass_g_final = NA,
                                                            class = "Reptilia")]
dat.m2[is.na(class) &
         exclude_species == "included_species", ]
# Going to let Arian clean this up


# 
unique(dat.m2$class)
# dat.m2[class %in% c("Reptilia", "Reptiles", "Amphibia"), class := "Reptiles & Amphibians"]
dat.m2[class %in% c("Mammal", "Mammals"), class := "Mammals"]
dat.m2[class %in% c("Reptilia", "Reptiles"), class := "Reptiles"]
dat.m2[class %in% c("Amphibia"), class := "Amphibians"]

# >>> Filter and save ---------------------------------------------------------

unique(dat.m2$evidence_type)

dat.m2
dat.m2 <- dat.m2[, .(scientificName, spp_name_corrected, assessmentId,
                     internalTaxonId, realm, systems, assessmentDate,
                     redlistCategory, criteriaVersion, populationTrend,
                     Synonyms_or_previous_lump, Cat_effect, EXCLUDE_cats_not_attributed,
                     Web_of_Science_no_claim,Snowball, Wallach_AU_mammal_no_claim, Opportunistic,
                     IUCN_references, Doherty_references0_NA, Dickman_AU_mammal,
                     Medina_references0_NA, Woinarski_AU_mammal, Garnett2020_AU_birds,
                     Garnett2010_AU_birds, Alberts_iguanas, Oedin_bats, Welch_bats,
                     Hume_EX_birds, Hess,
                     Search_conducted,Mass_g_final, exclude_species, class)]
unique(dat.m2$class)
fwrite(dat.m2, "builds/claims/species_claims_tidy_raw.csv")

