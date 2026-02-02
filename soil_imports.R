#script for importing soil chem
field_pH <- read.csv("field_pH.csv.csv")


library(dplyr)
library(lubridate)
library(stringr)

#fixing dates and selecting only latest pH readings (one per field)
field_pH_latest <- field_pH %>%
  mutate(
    DateSampled = case_when(
      str_detect(DateSampled, "^\\d{4}$") ~ ymd(paste0(DateSampled, "-01-01")),
      TRUE ~ ymd(DateSampled)
    ),
    YearSampled = year(DateSampled)
  ) %>%
  group_by(X2025FieldName) %>%
  slice_max(YearSampled, n = 1, with_ties = FALSE) %>%
  ungroup()

#changing names column for joining in arcpro
field_pH_latest <- field_pH_latest %>%
  rename(name_bore = `X2025FieldName`)

#exporting
#write.csv(field_pH_latest, "field_pH_latest.csv")

k_org_matter <-read.csv("k_org_matter.csv")

k_org_matter <- k_org_matter %>%
  mutate(name_bore = if_else(name_bore == "Plains ", 
                             "Plains", 
                             name_bore))
# Keep only the latest sample per field
k_org_latest <- k_org_matter %>%
  group_by(name_bore) %>%                        # group by field
  slice_max(Sample.YEAR, n = 1, with_ties = FALSE) %>%  # keep only latest year
  ungroup()

#writing out
write.csv(k_org_latest, "k_org_latest.csv")

###presence/absence
inv_pres_abs <- read.csv("invasives presense_abs.csv")

inv_pres_abs <- inv_pres_abs %>%
  rename(
    mf_rose = Multiflora.rose,
    glossy_buckthorn = glossy.buckthorn,
    asiatic_bittersweet = asiatic.bittersweet,
    purple_loosestrife = purple.loosestrife,
    wild_chervil = wild.chervil,
    rcg = reed.canary.grass
  )
inv_pres_abs<-inv_pres_abs|>
  select(-X)

inv_pres_abs<-inv_pres_abs%>%
  rename(name_bore = Field)

write.csv(inv_pres_abs, "inv_pres_abs.csv")  

#phosphorus
phosphorus<-read.csv("phosphorus.csv")

# Keep only the latest sample per field
phos_latest <- phosphorus %>%
  group_by(name_bore) %>%                        # group by field
  slice_max(Sample.YEAR, n = 1, with_ties = FALSE) %>%  # keep only latest year
  ungroup()

#write
write.csv(phos_latest, "phos_latest.csv")


tdn_protein <- read.csv("forage qual.csv")
tdn_protein<-tdn_protein%>%
  rename(
    name_bore=FIELD
  )

write.csv(tdn_protein, "tdn_protein.csv")

vegplots25<-read.csv("vegplot25.csv")

leg_grass<-vegplots25%>%
  mutate(legume_grass=avg_legume/avg_grass)

leg_grass<-leg_grass%>%
  select(name_bore,legume_grass)

write.csv(leg_grass, "legume_grass.csv")  

vegplots<-read.csv("appleton_vegplots_since2022.csv")
vegplots <- vegplots[-c(1, 2, 3, 4), ]   # removes rows 1, 3, and 5
colnames(vegplots) <- vegplots[1, ]
df <- df[-1, ]
vegplots <- vegplots[-1, ]


 

vegplot1 <- vegplots %>%
  mutate(
    Date = parse_date_time(Date, orders = c("mdy", "ymd", "dmy")),
    Year = year(Date),
    bare = as.numeric(as.character(bare))
  ) %>%
  filter(Year == 2025) %>%
  group_by(Field_2025) %>%
  summarize(
    avg_bare = mean(bare, na.rm = TRUE),
    avg_thatch = mean(thatch, na.rm=TRUE),
    .groups = "drop"
  )


vegplot1<-vegplot1%>%
  rename(
    name_bore=Field_2025
  )
write.csv(vegplot1, "bare.csv")




