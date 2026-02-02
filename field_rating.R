#

library(tidyverse)
library(readxl)

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
library(dplyr)

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
library(lubridate)
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
#each dominant species will be rated a 3 (desirable), 2(ambiguous), or 1(undesirable)

dom_species<-production%>%
  select(Field, 'Common Name', cover)

dom_species<-dom_species%>%
  mutate(
    rating = case_when(
      `Common Name` == "American Manna Grass" ~ 3,
      `Common Name` == "Reed Canary Grass" ~ 2,
      `Common Name` == "Sweet Vernal Grass" ~ 1,
      `Common Name` == "Fowl Blue Grass" ~ 3,
      `Common Name` == "Meadow Fescue" ~ 3,
      `Common Name` == "Soft Brome" ~ 3,
      `Common Name` == "Orchard Grass" ~ 3,
      `Common Name` == "Sweet Vernal Grass" ~ 1,
      `Common Name` == "Ryegrass sp." ~ 3,
      `Common Name` == "Tall Fescue" ~ 2,
      `Common Name` == "Red Fescue" ~ 1,
      `Common Name` == "Common Timothy" ~3,
      `Common Name` == "Bromus inermis" ~ 3,
      `Common Name` == "Quack Grasss"~3,
      `Common Name` == "Fowl Manna Grass"~3,
      `Common Name` == "Kentucky Blue Grass"~3,
      `Common Name` == "Sedge/Rush sp."~1,
      `Common Name` == "Smooth Brome"~3,
      `Common Name` == "Common Soft Rush"~1,
      `Common Name` == "Kentucky Bluegrass"~3,
      `Common Name` == "orchard grass"~3,
      `Common Name` == "Red Clover"~3,
      `Common Name` == "Morrow's Honeysuckle"~1,
      `Common Name` == "Buckthorn sp."~1,
      `Common Name` == "Dewberry sp."~1,
      `Common Name` == "Pokeweed"~1,
      `Common Name` == "Poison Ivy"~1,
      `Common Name` == "Tall/Meadow Fescue"~3,
      `Common Name` == "Multiflora Rose"~1,
      `Common Name` == "Brome sp."~3,
      `Common Name` == "Perennial Ryegrass"~3,
      `Common Name` == "Clover sp."~3,
      `Common Name` == "English Plantain"~3,
      `Common Name` == "Fine-leaved Sheep Fescue"~1,
      `Common Name` == "Honeysuckle sp."~1,
      `Common Name` == "Wild Chervil"~1,
      `Common Name` == "Manna Grass sp."~3,
      `Common Name` == "Blue Grass sp."~3,
      `Common Name` == "Bird's Foot Trefoil"~3
      
      
      
    )
  )

#cleaning
dom_species<-dom_species%>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))


