#this script is for consolidation of field data for use in field rating
#in 2026. Adapted from NRCS pasture condition score sheet

library(tidyverse)
library(readxl)
library(writexl)
library(dplyr)
library(lubridate)


#reading in scripts###############

# plots at the field level from 2022 and 2025
vegplots<-read_csv("appleton_vegplots_since2022.csv", skip=5)

# elus summary julie 2020-2025 (needs cleaning before read in)
elu_summary<-read_excel("appleton_elus_since2020.xlsx")

#field name key
fieldnames<-read_excel("fieldnames.xlsx")



# production data (i.e. dominants)
production<-read_csv("2025_production_veg.csv")

# species list by field
species <- read_csv("2025_species_lists.csv", skip=5)

#
vegplots25<-read.csv("vegplot25.csv")


#averages drainage rows
vegplots25 <- vegplots25 %>%
  group_by(Field_2025) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

#___________________________________________________________________________
#combining data for field ratings

#grass:forb ratio
field_rating<-vegplots25%>%
  mutate(grass_forb=avg_grass/avg_forb)

#invasive grass
invasives25<-read.csv("invasives25.csv")
field_rating <- field_rating %>%
  left_join(
    invasives25 %>% select(Field_2025, avg_inv_grass),
    by = "Field_2025"
  )

#thatch and bare
thatch_bare <- vegplots %>%
  mutate(
    Date = case_when(
      str_detect(Date, "^\\d{4}$") ~ as.Date(paste0(Date, "-01-01")),
      str_detect(Date, "^\\d{5}$") ~ as.Date(as.numeric(Date), origin = "1899-12-30"),
      TRUE ~ as.Date(parse_date_time(Date, orders = c("mdy", "ymd", "dmy", "ymd HMS")))
    ),
    Year = year(Date),
    bare = as.numeric(as.character(bare)),
    thatch = as.numeric(as.character(thatch))
  ) %>%
  filter(Year == 2025) %>%
  group_by(Field_2025) %>%
  summarize(
    avg_bare = mean(bare, na.rm = TRUE),
    avg_thatch = mean(thatch, na.rm = TRUE),
    .groups = "drop"
  )

field_rating <- field_rating %>%
  left_join(
    thatch_bare %>% select(Field_2025, avg_bare, avg_thatch),
    by = "Field_2025"
  )

#Field production (DM/acre)

in_out1<-read.csv("in_out.csv")
in_out1<-in_out1%>%
  select(field,sum_acres)

in_out1_clean <- in_out1 %>%
  group_by(field) %>%
  summarize(sum_acres = first(sum_acres), .groups = "drop")

#joining
field_rating <- field_rating %>%
  left_join(in_out1_clean,
            by = c("Field_2025" = "field"))

####
#species diversity
species_long<-read.csv("species_long.csv")
species_long<-species_long%>%
  select(Field, total_sp)%>%
  rename(total_species=total_sp)

species_long <- species_long %>%
  mutate(Field = recode(Field,
                             "Drainage Dry" = "Drainage",
                        "Drainage Wet" = "Drainage"))
species_long <- species_long %>%
  mutate(Field = recode(Field,
                        "Broad Meadow Dry" = "Broad Meadow"))

species_long <- species_long %>%
  group_by(Field) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

#joining
field_rating<-field_rating%>%
  left_join(species_long,
            by = c("Field_2025" = "Field"))

#Forage quality: Using the production data. The column "Common Name" Lists
#the dominant species in each field as determined by the 2025 veg surveys
#5 most palatable, best growth, 4=palatable but less productive species, 
#3=palatability very dependent on season, growth stage, 
#2=lower palatability and/or low productivity, 
#1= not palatable, 0 = avoided by livestock

dom_species<-production%>%
  select(Field, 'Common Name', cover)

dom_species<-dom_species%>%
  mutate(
    rating = case_when(
      `Common Name` == "Reed Canary Grass" ~ 3,
      `Common Name` == "American Manna Grass" ~ 4,
      `Common Name` == "Sweet Vernal Grass" ~ 2,
      `Common Name` == "Fowl Blue Grass" ~ 4,
      `Common Name` == "Meadow Fescue" ~ 5,
      `Common Name` == "Soft Brome" ~ 2,
      `Common Name` == "Orchard Grass" ~ 5,
      `Common Name` == "Sweet Vernal Grass" ~ 2,
      `Common Name` == "Ryegrass sp." ~ 5,
      `Common Name` == "Tall Fescue" ~ 3,
      `Common Name` == "Red Fescue" ~ 2,
      `Common Name` == "Common Timothy" ~5,
      `Common Name` == "Bromus inermis" ~ 5,
      `Common Name` == "Quack Grasss"~3,
      `Common Name` == "Fowl Manna Grass"~4,
      `Common Name` == "Kentucky Blue Grass"~4,
      `Common Name` == "Sedge/Rush sp."~1,
      `Common Name` == "Smooth Brome"~5,
      `Common Name` == "Common Soft Rush"~0,
      `Common Name` == "Kentucky Bluegrass"~4,
      `Common Name` == "orchard grass"~5,
      `Common Name` == "Red Clover"~5,#####################
      `Common Name` == "Morrow's Honeysuckle"~1,
      `Common Name` == "Buckthorn sp."~2,
      `Common Name` == "Dewberry sp."~0,
      `Common Name` == "Pokeweed"~1,
      `Common Name` == "Poison Ivy"~1,
      `Common Name` == "Tall/Meadow Fescue"~5,
      `Common Name` == "Multiflora Rose"~1,
      `Common Name` == "Brome sp."~4,
      `Common Name` == "Perennial Ryegrass"~5,
      `Common Name` == "Clover sp."~5,
      `Common Name` == "English Plantain"~4,
      `Common Name` == "Fine-leaved Sheep Fescue"~2,
      `Common Name` == "Honeysuckle sp."~1,
      `Common Name` == "Wild Chervil"~2,
      `Common Name` == "Manna Grass sp."~5,
      `Common Name` == "Blue Grass sp."~4,
      `Common Name` == "Bird's Foot Trefoil"~5,###########
      `Common Name` == "Redtop Bentgrass"~3,
      `Common Name` == "Velvet Grass" ~2,
      `Common Name` == "Quack Grass"~3,
      `Common Name` == "Rough Bentgrass"~2,
      `Common Name` == "Sedge sp."~2,
      `Common Name` == "Canada Bluejoint Grass"~5,
      `Common Name` == "Bird Vetch"~4,
      `Common Name` == "Common Fox Sedge"~2,
      `Common Name` == "Bedstraw sp."~0,
      `Common Name` == "Awl Fruited Sedge"~1,
      `Common Name` == "Hairy Crabgrass"~2,
      `Common Name` == "Yellow Foxtail"~1,
      `Common Name` == "Tall Oat Grass"~5,
      `Common Name` == "Rice Cut Grass"~4,
      `Common Name` == "Bentgrass sp."~3,
      `Common Name` == "Creeping Bentgrass"~2,
      `Common Name` == "Queen Anne's Lace"~2,
      `Common Name` == "White Wood Aster"~1,
      `Common Name` == "Goldenrod sp."~2,
      `Common Name` == "Colonial Bentgrass"~3,
      `Common Name` == "Rush sp."~0,
      `Common Name` == "Common Bentgrass"~2
      
    )
  )

#cleaning
dom_species<-dom_species%>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
dom_species$cover <- as.numeric(dom_species$cover)


#taking only the top 5 dominant species, based on cover amounts
dom_species_top5 <- dom_species %>%
  group_by(Field) %>%
  slice_max(order_by = cover, n = 5, with_ties = FALSE) %>%
  ungroup()

#changes cover values from a percent to a proportion (100%->1)
dom_species_top5<-dom_species_top5%>%
  mutate(cover=cover/100)

#creates a score column that takes into account the amount of cover a scored
#species has. i.e. not all species rated a 5 will have a final score of 5
dom_species_top5<-dom_species_top5%>%
  mutate(score=cover*rating)

#total rating scores (summing scores from dom_species_top5)
#
dom_species1 <- dom_species_top5 %>%
  group_by(Field) %>%
  summarise(
    dominant_score = sum(score, na.rm = TRUE) / sum(cover, na.rm = TRUE),
    .groups = "drop"
  )

#manual entry for gp1B, since it didn't have any cover in veg survey
dom_species1<-dom_species1%>%
  mutate(
    dominant_score = case_when(
      Field == "GP1B" ~ 3,
      TRUE ~ dominant_score  
    )
  )
  
#write
write.csv(dom_species1, "dom_species1.csv")

#joining to field_rating
field_rating<-field_rating%>%
  left_join(dom_species1,
            by = c("Field_2025" = "Field"))

#################regrowth rate
regrowth <- read.csv("rec_slopes.csv")

#one unique slope per field
regrowth<-regrowth%>%
  group_by(field)%>%
  summarise(
    regrowth_rate=mean(slope, na.rm = TRUE),
    .groups="drop"
  )

#joining to field_rating
field_rating<-field_rating%>%
  left_join(regrowth,
            by = c("Field_2025" = "field"))

#cleaning
field_rating<-field_rating%>%
  rename(`Month Sampled` = Month)

field_rating<-field_rating%>%
  select(-X, -avg_flowering, -avg_forb, -avg_woody, -avg_grass, -avg_invasive)

field_rating<-field_rating%>%
  rename(total_harvested = sum_acres)

field_rating<-field_rating%>%
  rename(RCG = avg_inv_grass)

#adding in a "live" column 
field_rating<-field_rating%>%
  mutate(
    live=(100-(avg_bare+avg_thatch))
  )

#rating for live (based off nrcs rubric)
field_rating <- field_rating %>%
  mutate(
    live_rating = case_when(
      live <= 40 ~ 1,
      live >= 40.1 & live <= 65 ~ 2,
      live >= 65.1 & live<=80 ~ 3,
      live>=80.1 & live<=95~4,
      live>95~5,
      TRUE ~ NA_real_
    )
  )

#ranking for bare ground
field_rating <- field_rating %>%
  mutate(
    bare_rating = case_when(
      avg_bare == 0 ~ 5,
      avg_bare > 0  & avg_bare <= 2  ~ 4,
      avg_bare > 2  & avg_bare <= 5  ~ 3,
      avg_bare > 5  & avg_bare <= 10 ~ 2,
      avg_bare > 10 ~ 1,
      TRUE ~ NA_real_
    )
  )

#ranking legumes
field_rating <- field_rating %>%
  mutate(
    legume_rating = case_when(
      avg_legume <= 5 | avg_legume>50 ~ 1,
      avg_legume > 5 & avg_legume<10 | avg_legume>40 ~ 2,
      avg_legume > 11 & avg_legume<20~3,
      avg_legume > 20.1 & avg_legume<30~4,
      avg_legume > 31 & avg_legume<40~5,
            TRUE ~ NA_real_
    )
  )

#for maia planning
#field_rating <- field_rating %>%
  #mutate(
    #for_maia = (live_rating + legume_rating + bare_rating) / 15 * 10
  #)


#vigor - from production - how fast a field appears to recover
#incomplete data, will use as a comparison/check for using regrowth rate
production<-production%>%
  select(Field, vigor_recovery)

str(production$vigor_recovery)
production <- production %>%
  mutate(vigor_recovery = as.numeric(vigor_recovery))

production <- production %>%
  group_by(Field) %>%
  summarise(
    vigor_recovery = if (all(is.na(vigor_recovery))) {
      NA_real_
    } else {
      first(na.omit(vigor_recovery))
    },
    .groups = "drop"
  )

########
#regrowth = lbs/acre/day
field_rating <- field_rating %>%
  mutate(
    regrowth_rating = case_when(
      regrowth_rate <0 & regrowth_rate<=50 ~ 1,
      regrowth_rate>50.1 & regrowth_rate<=100~2,
      regrowth_rate>100.1 & regrowth_rate<=150~3,
      regrowth_rate>150.1 & regrowth_rate<=200~4,
      regrowth_rate>200.1~5,
      TRUE ~ NA_real_
    )
  )
#filling in some missing values from production
#(fields we did veg surveys but not platemetering)
#still many NA's in regrowth_rating due to lack of regrowth_rate or vigor rating
field_rating <- field_rating %>%
  mutate(
    regrowth_rating = case_when(
      Field_2025 == "Barberry Woods" ~ 5,
      Field_2025 == "Bull" ~ 5,
      Field_2025 == "GP7A" ~ 5,
      Field_2025 == "POW West" ~ 2,
      Field_2025 == "Williams Central" ~ 3,
      TRUE ~ regrowth_rating  # keep existing values
    )
  )

####
#erosion
#taken from CASH/IFSHA data
field_rating<-field_rating%>%
  mutate(
    erosion_rating = 5)

#compaction
#taken from CASH/IFSHA data
field_rating<-field_rating%>%
  mutate(
    compaction_rating = 1)
  

field_rating <- field_rating %>%
  mutate(
    compaction_rating = case_when(
      Field_2025 == "Broad Meadow" ~ 4,
      Field_2025 == "GP4A" ~ 2,
      Field_2025 == "GP4B" ~ 2,
      Field_2025 == "GP4C" ~ 2,
      Field_2025 == "Railroad" ~ 3,
      TRUE ~ compaction_rating  
    )
  )
     
#writing out
write_xlsx(field_rating, "field_rating.xlsx")

      




