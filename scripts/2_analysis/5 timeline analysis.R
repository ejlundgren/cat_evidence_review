rm(list = ls())
# 1. Loading packages -----------------------------------------------------
library(readxl)
library(data.table)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(plotly)
library(writexl)

# 2. Load data or builds --------------------------------------------------
theme_lundy <- theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank())

data_evidence<- as.data.table(read_xlsx("data/timeline/Cat timeline.xlsx"))
names(data_evidence)
duplicated(names(data_evidence))

#Check data
unique(data_evidence$Prey_extirpation_year_earliest) #there are NA's
unique(data_evidence$Prey_extirpation_year_latest) #there are NA's

#For now exclude one row of Conilurus capricornensis that has NA values
data_evidence[is.na(Prey_extirpation_year_earliest), ]
data_evidence <- data_evidence[!is.na(Prey_extirpation_year_earliest), ]

unique(data_evidence$Cat_arrival_year_earliest) #okay
unique(data_evidence$Cat_arrival_year_latest) #okay

#Exclude Fregilupus varius (the IUCN does not claim that cats caused the extinction of this species)
data_evidence <- data_evidence[scientificName != "Fregilupus varius", ]

#Remove all “uncertain” rows
unique(data_evidence$AU_ONLY_Record_certainty)

data_evidence <- data_evidence[is.na(AU_ONLY_Record_certainty) | AU_ONLY_Record_certainty == "Confirmed"]
unique(data_evidence$AU_ONLY_Record_certainty)
unique(data_evidence$scientificName)

#How many species are in the dataset?
unique(data_evidence$scientificName) #113 species

#Change status category to IUCN threat status
iucn_status <- read.csv("data/timeline/iucn_status.csv")
iucn_status <- setDT(iucn_status)

full_data <- merge(iucn_status, data_evidence, by.x = "Scientific.name", by.y = "scientificName")

setnames(full_data, "Scientific.name", "Scientific name")
setnames(full_data, "Redlist.category", "Redlist category")

full_data[Class == "AVES", Class :="Birds"]
full_data[Class == "MAMMALIA", Class :="Mammals"]
full_data[Class == "REPTILIA", Class :="Reptiles"]

unique(full_data$`Redlist category`)
full_data[, IUCN_status:= NA_character_ ]
full_data[`Redlist category` == "Least Concern", IUCN_status:= "(LC)"]
full_data[`Redlist category` == "Near Threatened", IUCN_status:= "(NT)"]
full_data[`Redlist category` == "Vulnerable", IUCN_status:= "(VU)"]
full_data[`Redlist category` == "Endangered", IUCN_status:= "(EN)"]
full_data[`Redlist category` == "Critically Endangered", IUCN_status:= "(CR)"]
full_data[`Redlist category` == "Extinct in the Wild", IUCN_status:= "(EW)"]
full_data[`Redlist category` == "Extinct", IUCN_status:= "(EX)"]

#Assign color palette for species status
#IUCN palette
IUCN_palette <- list (
  "(LC)" = "#60c759",
  "(NT)" = "#A2CD5A",#"#cde321"
  "(VU)" = "#FFD700", #"#f9e90c",
  "(EN)" = "#fc803d",
  "(CR)" = "#d91801",
  "(EW)" = "#552243",
  "(EX)" = "#000000"
)

full_data <- full_data[, .(Class, Order, Family, Genus, `Scientific name`, `Redlist category`, IUCN_status,
                           Extinction_extirpation, Prey_extirpation_year_earliest, Prey_extirpation_year_earliest_CE_BP,
                           Prey_extirpation_year_latest, Prey_extirpation_year_latest_CE_BP,
                           Prey_extinction_timing_reference, Prey_quote, 
                           Cat_arrival_year_earliest, Cat_arrival_year_earliest_CE_BP, 
                           Cat_arrival_year_latest, Cat_arrival_year_latest_CE_BP, 
                           Cat_arrival_timing_reference, Cat_quote,
                           Location, Country, Latitude, Longitude, Realm, Systems)]

#Remove all LC species
unique(full_data$`Redlist category`)
full_data <- full_data[`Redlist category`!="Least Concern",]
full_data[, .N, by=`Redlist category`]
full_data[, .N, by= IUCN_status]
duplicated(unique(full_data[, .(`Scientific name`, `Redlist category`)])) #Okay.

#How many are EX?
spp_list <- unique(full_data[`Redlist category`== "Extinct" | `Redlist category` == "Extinct in the Wild",
                             .(`Scientific name`, `Redlist category`)]) #There are 84 species

#Separate between extinct (EX + EW) and extant threatened species
full_data[, status:= NA_character_]
full_data[, status:= ifelse(`Redlist category`== "Extinct" | `Redlist category`== "Extinct in the Wild", "Extinct", "Extant")]
unique(full_data[status=="Extinct", `Scientific name`]) #Okay

# 3. Convert dates from CE to BP--------------------------------------------------
#Earliest prey extirpation → BP (min prey extinction)
full_data[, min_prey_extirpation_year_BP :=
            ifelse(Prey_extirpation_year_earliest_CE_BP == "CE",
                   1950 - Prey_extirpation_year_earliest,
                   Prey_extirpation_year_earliest)
]

#Latest prey extirpation → BP (max prey extinction)
full_data[, max_prey_extirpation_year_BP :=
            ifelse(Prey_extirpation_year_latest_CE_BP == "CE",
                   1950 - Prey_extirpation_year_latest,
                   Prey_extirpation_year_latest)
]

#Earliest cat arrival → BP (min cat arrival)
full_data[, min_cat_arrival_year_BP :=
            ifelse(Cat_arrival_year_earliest_CE_BP == "CE",
                   1950 - Cat_arrival_year_earliest,
                   Cat_arrival_year_earliest)
]

#Latest cat arrival → BP (max cat arrival)
full_data[, max_cat_arrival_year_BP :=
            ifelse(Cat_arrival_year_latest_CE_BP == "CE",
                   1950 - Cat_arrival_year_latest,
                   Cat_arrival_year_latest)
]

#Define time differences (prey extinction − cat arrival)
full_data[, `:=`(
  min_difference = min_prey_extirpation_year_BP - max_cat_arrival_year_BP,
  max_difference = max_prey_extirpation_year_BP - min_cat_arrival_year_BP
)]

full_data[, median_difference := (min_difference + max_difference) / 2]

#Flip direction
full_data[, `:=`(
  min_difference = -min_difference,
  max_difference = -max_difference,
  median_difference = -median_difference
)]

full_data[, prey_window :=
            min_prey_extirpation_year_BP - max_prey_extirpation_year_BP]

full_data[, cat_window :=
            min_cat_arrival_year_BP - max_cat_arrival_year_BP]


summary(full_data[, .(prey_window, cat_window)])

full_data[, cat_arrival := NA_character_]

# Before arrival
full_data[min_difference < 0 & max_difference < 0,
          cat_arrival := "Extinction before arrival"]

# Overlaps arrival
full_data[min_difference < 0 & max_difference >= 0,
          cat_arrival := "Extinction overlaps arrival"]

# 0–100 years
full_data[min_difference >= 0 & min_difference <= 100,
          cat_arrival := "Extinction within 100 years of arrival"]

# 101-200 years
full_data[min_difference > 100 & min_difference <= 200,
          cat_arrival := "Extinction 101–200 years after arrival"]

# >200 years
full_data[min_difference > 200,
          cat_arrival := "Extinction 201 years or more after arrival"]

full_data$cat_arrival <- factor(full_data$cat_arrival,
                                levels = c(
                                  "Extinction 201 years or more after arrival",
                                  "Extinction 101–200 years after arrival",
                                  "Extinction within 100 years of arrival",
                                  "Extinction overlaps arrival",
                                  "Extinction before arrival"
                                ))

full_data <- full_data[, .(Class, Order, Family, Genus, `Scientific name`, status, Extinction_extirpation,
                           `Redlist category`, IUCN_status, Location, Country, Latitude, Longitude, Realm, Systems,
                           Prey_extirpation_year_earliest, Prey_extirpation_year_earliest_CE_BP,
                           Prey_extirpation_year_latest, Prey_extirpation_year_latest_CE_BP,
                           Prey_extinction_timing_reference, Prey_quote, 
                           Cat_arrival_year_earliest, Cat_arrival_year_earliest_CE_BP, 
                           Cat_arrival_year_latest, Cat_arrival_year_latest_CE_BP, 
                           Cat_arrival_timing_reference, Cat_quote,
                           min_prey_extirpation_year_BP, max_prey_extirpation_year_BP, 
                           min_cat_arrival_year_BP, max_cat_arrival_year_BP, min_difference, max_difference,
                           median_difference, prey_window, cat_window, cat_arrival
)]

full_data <- full_data[order(full_data$Class), ]
table(full_data$cat_arrival)

# 4. Summaries and figures--------------------------------------------------
spp_sum_ex <- unique(full_data[status == "Extinct", .(Class, `Scientific name`, `Redlist category`, status, cat_arrival, Country)])

#For text:
unique(spp_sum_ex$`Scientific name`) #Cats have been attributed with the extinction of 84 species 
unique(spp_sum_ex[`Redlist category`=="Extinct in the Wild", `Scientific name`]) #5 spp are EW
ex_records <- full_data[status == "Extinct", ]
nrow(ex_records) #We identified the last records of 119 populations

#Some summaries
sum_ex <- full_data[status=="Extinct", .N, by= .(Class, cat_arrival)]
sum_ex[, perc:= N / sum(N) * 100]
sum_ex[, sum(perc), by = cat_arrival]
sum_ex[, perc_class:= N / sum(N) * 100, by = Class]

sum(sum_ex$perc) #okay
sum(sum_ex$N) #okay 
sum(sum_ex[Class == "Birds", perc_class]) #okay

sum(sum_ex[cat_arrival == "Extinction before arrival", perc]) #5.882353
sum(sum_ex[cat_arrival == "Extinction overlaps arrival", perc]) #21.84874
sum(sum_ex[cat_arrival == "Extinction within 100 years of arrival", perc]) #49.57983
sum(sum_ex[cat_arrival == "Extinction 101–200 years after arrival", perc]) #15.12605
sum(sum_ex[cat_arrival == "Extinction 201 years or more after arrival", perc]) #7.563025

setnames(sum_ex, "cat_arrival", "Cat arrival")
setnames(sum_ex, "N", "Populations")
setnames(sum_ex, "perc", "Total contribution (%)")
setnames(sum_ex, "perc_class", "Contribution by class (%)")

str(sum_ex)
sum_ex$`Cat arrival`

arrival_levels <- c(
  "Extinction before arrival",
  "Extinction overlaps arrival",
  "Extinction within 100 years of arrival",
  "Extinction 101–200 years after arrival",
  "Extinction 201 years or more after arrival"
)

sum_ex[, `Cat arrival` := factor(`Cat arrival`,
                                 levels = arrival_levels)]

setorder(sum_ex, `Cat arrival`, `Cat arrival`)
setorder(sum_ex, Class, -Class)

#Save dataset
writexl::write_xlsx(sum_ex, "builds/timeline/cat_extinctions_summary.xlsx")

#Let's make some summary figures
sum_ex[, `Cat arrival` := factor(`Cat arrival`,
                                 levels = arrival_levels)]

#Set color palette
plot_col<-c(
  "Extinction 201 years or more after arrival" = "#a2a2a2",
  "Extinction 101–200 years after arrival" = "#828282",
  "Extinction within 100 years of arrival" = "dodgerblue4",
  "Extinction overlaps arrival" = "grey20",
  "Extinction before arrival" = "black"
)

#Barplot by class & extinctions only
ggplot(sum_ex, 
       aes(x= Class, y=Populations, fill= `Cat arrival`))+
  geom_col(width = 0.8, position = position_dodge(width = 0.9))+
  #geom_col()+
  scale_fill_manual(values = plot_col)+#, guide = guide_legend(nrow = 2)
  labs(x="", y="Number of attributed extinctions", fill= "")+
  theme_lundy +
  #theme(legend.position = "bottom")
  theme(legend.position = c(0.8, 0.9))

#Barplot where each bar is a type of record (before, during, <100y, 100-200, >200) and each colour is a country
sum_ex_country <- full_data[status=="Extinct", .N, by= .(Class, cat_arrival, Country)]

sum_ex_country[, cat_arrival := factor(cat_arrival,
                                       levels = arrival_levels)]

ggplot(sum_ex_country, 
       aes(x=cat_arrival, y=N, fill= Country))+
  geom_col()+
  labs(x="", y="Number of records", fill= "")+
  theme_lundy +
  theme(legend.position = "right")+
  scale_x_discrete(labels = c(c("Extinction before arrival" = "Extinction before arrival",
                                "Extinction overlaps arrival" = "Extinction overlaps arrival",
                                "Extinction within 100 years of arrival" = "Extinction within 100\nyears of arrival",
                                "Extinction 101–200 years after arrival" = "Extinction 101–200\nyears after arrival",
                                "Extinction 201 years or more after arrival" = "Extinction 201 years\nor more after arrival"
                                
  )))

#How does it look by realm?
sum_ex_realm <- full_data[status=="Extinct", .N, by= .(Class, cat_arrival, Realm)]

sum_ex_realm[, cat_arrival := factor(cat_arrival,
                                     levels = arrival_levels)]

ggplot(sum_ex_realm, 
       aes(x=cat_arrival, y=N, fill= Realm))+
  geom_col()+
  labs(x="", y="Number of records", fill= "")+
  theme_lundy +
  theme(legend.position = "right")+
  scale_x_discrete(labels = c(c("Extinction before arrival" = "Extinction before arrival",
                                "Extinction overlaps arrival" = "Extinction overlaps arrival",
                                "Extinction within 100 years of arrival" = "Extinction within 100\nyears of arrival",
                                "Extinction 101–200 years after arrival" = "Extinction 101–200\nyears after arrival",
                                "Extinction 201 years or more after arrival" = "Extinction 201 years\nor more after arrival"
                                
  )))

#By country and class
ggplot(sum_ex_country, 
       aes(x=cat_arrival, y=N, fill= Country))+
  geom_col()+
  facet_wrap(~Class, nrow=1)+
  #scale_fill_manual(values = plot_col)+#, guide = guide_legend(nrow = 2)
  scale_fill_discrete(guide = guide_legend(nrow = 2))+
  labs(x="", y="Number of records", fill= "")+
  theme_lundy +
  theme(legend.position = "bottom")+
  scale_x_discrete(labels = c(c("Extinction before arrival" = "Extinction before\narrival",
                                "Extinction overlaps arrival" = "Extinction overlaps\narrival",
                                "Extinction within 100 years of arrival" = "Extinction within 100\nyears of arrival",
                                "Extinction 101–200 years after arrival" = "Extinction 101–200\nyears after arrival",
                                "Extinction 201 years or more after arrival" = "Extinction 201 years\nor more after arrival"
                                
  )))

#Forest plot - species extinction and cat arrival times
plot_dt <- copy(full_data[`Redlist category`== "Extinct" | `Redlist category` == "Extinct in the Wild", ])

#Add a * to point out EW species
plot_dt[, sci_label:= ifelse(IUCN_status == "(EW)", paste(`Scientific name`, "*"), `Scientific name`)]

# Order species by Class, then alphabetically
plot_dt[, sci_label :=
          factor(sci_label,
                 levels = plot_dt[
                   order(Class, sci_label),
                   unique(sci_label)
                 ])
]

# Numeric index for background stripes
plot_dt[, x_id := as.numeric(sci_label)]

class_bounds <- plot_dt[, .(
  xmin = min(x_id) - 0.5,
  xmax = max(x_id) + 0.5
), by = Class]

class_labels <- plot_dt[, .(
  x = mean(x_id)
), by = Class]

#These three species don't show in the forest plot because there is no uncertainty in the cat and prey window. The max and min diff are the same, therefore the segment starts and end in the same place and becomes 0.
#To fix it add in the plot a tiny segment for these cases.
issue<-plot_dt[`Scientific name` %in% c("Todiramphus cinnamominus", "Cryptoblepharus egeriae", "Lepidodactylus listeri"), ]

ggplot() +
  
  # stripes
  geom_rect(
    data = plot_dt[, .(
      xmin = x_id - 0.5,
      xmax = x_id + 0.5,
      stripe = x_id %% 2
    )],
    aes(xmin = xmin, xmax = xmax,
        ymin = -Inf, ymax = Inf,
        fill = factor(stripe)),
    alpha = 0.4
  ) +
  scale_fill_manual(values = c("white", "grey90"), guide = "none") +
  
  # class separators
  geom_vline(
    data = class_bounds,
    aes(xintercept = xmax),
    linetype = "dashed",
    colour = "grey30"
  ) +
  
  # uncertainty bars
  geom_segment(
    data = plot_dt,
    aes(x = sci_label,
        xend = sci_label,
        y = min_difference,
        yend = max_difference),
    linewidth = 1
  ) +
  
  geom_hline(yintercept = 0) +
  
  geom_text(
    data = class_labels,
    aes(x = x, y = max(plot_dt$max_difference, na.rm = TRUE) + 40,
        label = Class),
    fontface = "bold",
    size = 8
  )+
  
  geom_segment(
    data = plot_dt[min_difference == max_difference],
    aes(x = sci_label, xend = sci_label,
        y = min_difference - 1, yend = max_difference + 1),
    linewidth = 1
  ) +
  
  labs(
    y = "Years between cat arrival and last record",
    x = NULL
  ) +
  
  theme_lundy +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3, face = "italic"),
    axis.text = element_text(size=12),
    axis.title.y = element_text(size = 16)
  )

ggsave("figures/SI/time analysis forest plot.pdf", width = 20, height = 10)

#World map plot of extinction records
world_sf <- st_as_sf(ne_countries(scale = "medium", returnclass = "sf"))
world_sf <- st_as_sf(world_sf, crs = 4326)

cat_ex_map <- st_as_sf(ex_records, coords = c("Longitude", "Latitude"), crs= 4326)
cat_ex_map$cat_arrival <- factor(cat_ex_map$cat_arrival, levels = arrival_levels)

ggplot() +
  geom_sf(data = world_sf, fill = "gray90", color = "black", size = 0.1) +
  geom_sf(
    data = cat_ex_map,
    aes(fill = cat_arrival, color= cat_arrival, shape = Class),
    size = 3,
    stroke = 0.6,
    alpha = 0.9
  ) +
  
  scale_fill_manual(values = plot_col,
                    labels = c(c("Extinction before arrival" = "Extinction before arrival",
                                 "Extinction overlaps arrival" = "Extinction overlaps arrival",
                                 "Extinction within 100 years of arrival" = "Extinction within 100\nyears of arrival",
                                 "Extinction 101–200 years after arrival" = "Extinction 101–200\nyears after arrival",
                                 "Extinction 201 years or more after arrival" = "Extinction 201 years\nor more after arrival"
                                 
                    ))) +
  
  scale_color_manual(values = plot_col,
                     labels = c(c("Extinction before arrival" = "Extinction before arrival",
                                  "Extinction overlaps arrival" = "Extinction overlaps arrival",
                                  "Extinction within 100 years of arrival" = "Extinction within 100\nyears of arrival",
                                  "Extinction 101–200 years after arrival" = "Extinction 101–200\nyears after arrival",
                                  "Extinction 201 years or more after arrival" = "Extinction 201 years\nor more after arrival"
                                  
                     ))) +
  
  scale_shape_manual(
    values = c(
      "Birds" = 21,
      "Mammals" = 24,
      "Reptiles" = 22
    )) +
  
  labs(fill="", shape="", color="")+
  theme_minimal() +
  
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank())+
  
  theme(
    legend.key.height = unit(0.6, "cm"),
    legend.key.width  = unit(1.2, "cm"),
    legend.text = element_text(size = 16)
  ) +
  coord_sf(crs = "+proj=moll")

ggsave("figures/SI/time analysis world map extinction records.pdf", width = 18, height = 10)
