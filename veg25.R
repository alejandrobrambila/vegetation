#

library(tidyverse)
library(readxl)

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

vegplot25<-vegplots%>%
  mutate(Date=mdy(Date))%>%
  mutate(Year=year(Date))%>%
  mutate(Month=month(Date))%>%
  filter(Year==2025)%>%
  mutate(woody_invasive=ifelse(is.na(woody_invasive), 0, woody_invasive))%>%
mutate(invasive_forb=ifelse(is.na(invasive_forb), 0, invasive_forb))%>%
mutate(invasive_grass=ifelse(is.na(invasive_grass), 0, invasive_grass))%>%

  mutate(invasive=woody_invasive+invasive_forb+invasive_grass)%>%
  group_by(Field_2025, `ELU Name`, Month)%>%
  summarize(avg_forb=mean(forb_inclusive), 
            avg_flowering=mean(flowering_forb), 
            avg_legume=mean(legume_forb), 
            avg_grass=mean(graminoids_inclusive),
            avg_woody=mean(woody_inclusive),
            avg_invasive=mean(woody_invasive+invasive_forb+invasive_grass))%>%
  mutate(Field=Field_2025)

ggplot(vegplot25, aes(reorder(Field_2025, -avg_forb), avg_forb)) + geom_point()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#forb to grass ratio
ggplot(vegplot25, aes(reorder(Field_2025, -avg_forb/avg_grass), avg_forb/avg_grass)) + geom_point()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#active flowering
ggplot(vegplot25, aes(reorder(Field_2025, -avg_flowering), avg_flowering, color=as.factor(Month))) + geom_point()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_color_manual(values = c("blue", "blue", "green", "green", "red", "red",  "red"))

ggplot(vegplot25, aes(reorder(Field_2025, -avg_legume/avg_forb), avg_legume/avg_forb)) + geom_point()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(vegplot25, aes(reorder(Field_2025, -avg_grass), avg_grass)) + geom_point()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(vegplot25, aes(reorder(Field_2025, -avg_woody), avg_woody)) + geom_point()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


ggplot(vegplot25, aes(reorder(Field_2025, -avg_invasive), avg_invasive)) + geom_point()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


#number of species

species_long<-species%>%
  pivot_longer(7:58, names_to = "Field" , values_to= "Present")%>%
  mutate(flowering=as.numeric(ifelse(Present!="f", NA, 1)))%>%
  mutate(present=as.numeric(ifelse(is.na(Present), NA, 1)))%>%
  mutate(
    flowering = replace_na(flowering, 0), # Replace NAs in column 'x' with 0
    present = replace_na(present, 0)  # Replace NAs in column 'y' with 0
  )%>%
  group_by(Field)%>%
  summarize(total_sp=sum(present),
            total_flowering=sum(flowering))%>%
  left_join(select(vegplot25, Field, Month))

#total species by field
ggplot(species_long, aes(reorder(Field, -total_sp), total_sp)) +geom_point()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#species flowering by field
ggplot(species_long, aes(reorder(Field, -total_flowering), total_flowering, color=as.factor(Month))) +geom_point()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_color_manual(values = c("blue", "blue", "green", "green", "red", "red",  "red"))


#do num invasive species per field colored by species
speciesgroup_long<-species%>%
  pivot_longer(7:58, names_to = "Field" , values_to= "Present")%>%
  mutate(flowering=as.numeric(ifelse(Present!="f", NA, 1)))%>%
  mutate(present=as.numeric(ifelse(is.na(Present), NA, 1)))%>%
  mutate(
    flowering = replace_na(flowering, 0), # Replace NAs in column 'x' with 0
    present = replace_na(present, 0))%>%  # Replace NAs in column 'y' with 0
  filter(Invasive=="x")
  summarize(total_sp=sum(present),
            total_flowering=sum(flowering))%>%
  left_join(select(vegplot25, Field, Month))

ggplot(speciesgroup_long, aes(reorder(Field, -present), present)) +
  geom_bar(stat="identity", position="stack", aes(fill=`Common Name`))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
ggplot(speciesgroup_long,  aes(reorder(Field, -present), `Common Name`, fill=as.factor(present))) +
  geom_tile()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_fill_manual(values = c("white", "black"))

# plot production (dominants)
production_lf<-production%>%
  left_join(select(species, `Common Name`, `Life form`))

ggplot(production, aes(Field, `Common Name`, fill=cover))+ geom_tile()

#LOOK AT SPECIES OF INTEREST FROM VEG PLOTS

# do a nmds of species presence,
library(codyn)
library(vegan)

species_matrix<-species_long<-species%>%
  pivot_longer(7:58, names_to = "Field" , values_to= "Present")%>%
  mutate(flowering=as.numeric(ifelse(Present!="f", NA, 1)))%>%
  mutate(present=as.numeric(ifelse(is.na(Present), NA, 1)))%>%
  mutate(
    flowering = replace_na(flowering, 0), # Replace NAs in column 'x' with 0
    present = replace_na(present, 0)  # Replace NAs in column 'y' with 0
  )%>%
  select(2, 7, 10)%>%
  pivot_wider(names_from=`Common Name`, values_from=present)%>%
  column_to_rownames(var = "Field")
  
set.seed(123)
nmds_result<-metaMDS(species_matrix, k=2, trymax=100, trace=FALSE)

scores_df<-as.data.frame(scores(nmds_result$points))
scores_df$Site<-rownames(scores_df)
species_scores <- as.data.frame(scores(nmds_result, display="species"))
species_scores$Species <- rownames(species_scores)

#filter to common species


filtered_speciesscores<-species_scores%>%
  filter(Species %in% unique(production$`Common Name`))

# Merge with your metadata (e.g., 'metadata_df' with 'Site' and 'Group')
#plot_data <- merge(scores_df, metadata_df, by.x = "Site", by.y = "Site")

ggplot(scores_df, aes(x=MDS1, y=MDS2)) +
  geom_point(size=3) +
  labs(title="NMDS Plot", stress=paste("Stress:", round(nmds_result$stress, 3))) +
  theme_minimal() +
  geom_text(data=filtered_speciesscores, aes(NMDS1, NMDS2, label=Species), vjust=1.5, hjust=0.5, size=3, color="blue")+
  geom_text(aes(label=Site), vjust=1.5, hjust=0.5, size=3)  # Add site labels


#and then one of species type (invasive forbs, woodies etc.), then also of production

structure_matrix<-vegplots%>%
  mutate(Date=mdy(Date))%>%
  mutate(Year=year(Date))%>%
  mutate(Month=month(Date))%>%
  filter(Year==2025)%>%
  mutate(woody_invasive=ifelse(is.na(woody_invasive), 0, woody_invasive))%>%
  mutate(invasive_forb=ifelse(is.na(invasive_forb), 0, invasive_forb))%>%
  mutate(invasive_grass=ifelse(is.na(invasive_grass), 0, invasive_grass))%>%
  
  mutate(invasive=woody_invasive+invasive_forb+invasive_grass)%>%
  select(3, 11, 16, 18, 19, 20, 21, 22, 23)%>%
  group_by(Field_2025)%>%
  mutate(
    woody_inclusive = replace_na(woody_inclusive, 0))%>%
  summarize(gram=mean(graminoids_inclusive), woody=mean(woody_inclusive, na.rm=T), forb=mean(forb_inclusive),
            legume=mean(legume_forb), bare=mean(bare), thatch=mean(thatch))%>%
  column_to_rownames(var = "Field_2025")

set.seed(123)
nmds_result_str<-metaMDS(structure_matrix, k=2, trymax=100, trace=FALSE)

scores_df<-as.data.frame(scores(nmds_result_str$points))
scores_df$Site<-rownames(scores_df)
species_scores <- as.data.frame(scores(nmds_result_str, display="species"))
species_scores$Species <- rownames(species_scores)

#filter to dominant grasses
filtered_speciesscores<-species_scores%>%
  filter(Species %in% unique(production$`Common Name`))

# Merge with your metadata (e.g., 'metadata_df' with 'Site' and 'Group')
#plot_data <- merge(scores_df, metadata_df, by.x = "Site", by.y = "Site")

ggplot(scores_df, aes(x=MDS1, y=MDS2)) +
  geom_point(size=3) +
  labs(title="NMDS Plot", stress=paste("Stress:", round(nmds_result$stress, 3))) +
  theme_minimal() +
  geom_text(data=species_scores, aes(NMDS1, NMDS2, label=Species), vjust=1.5, hjust=0.5, size=3, color="blue")+
  geom_text(aes(label=Site), vjust=1.5, hjust=0.5, size=3)  # Add site labels
