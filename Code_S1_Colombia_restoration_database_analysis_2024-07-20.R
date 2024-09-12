# Analysis of Colombia restoration project database
# E Warren-Thomas 20 June 2023
# Updated 19 Dec 2023
# Finalised 20 Jul 2024
# Bangor University

library(tidyverse)
library(sf)
library(viridis)
library(gridExtra)
library(RColorBrewer)
library(rcartocolor)
library(treemap)
library(cowplot)
library(ggsankey)
options(scipen = 99999)

#####
# choose some colour scales
#####

display_carto_all(colorblind_friendly = TRUE)
display.brewer.all(colorblindFriendly = TRUE)

display.brewer.pal(3, "Set2") # view a palette
brewer.pal(3, "Set2") # get hexademicals
compensation_pal <- c("#66C2A5", "#FC8D62", "#8DA0CB")
compensation_pal2 <- c("#c2e7db", "#fed1c0")
compensation_pal3 <- c("#c2e7db","#c7c7c7", "#fed1c0")
compensation_pal4 <- c("#66C2A5", "#FC8D62", "#c7c7c7")
compensation_pal5 <- c("#c2e7db", "#fed1c0", "#c7c7c7")
restorationtype_pal <- c("#FDDA0D", "#AFE1AF", "#088F8F", "#c7c7c7" )# "#8DA0CB" "#389640", )"#FFC300", 

unique(db$Potential_ecosystem)
#Andean forest, High Andean forest,Tropical dry forest,Sub-Andean forest,Tropical rain forest, Wetland,Paramo,Mangrove,Flooded savannah    
eco_pal<- c("#01665E", "#35978F", "#BF812D", "#80CDC1", "#003C30", "#8DA0CB",  "#DFC27D", "#C7EAE5", "#543005" ) #"#8C510A""#F6E8C3" "#F5F5F5" 

##############
#### # read in restoration database (CSV files saved from the individual worksheets in BD_Rest_Col_2022_10_07_Simple.xslx ####
#############

# This version of the database has duplicated rows for projects that span more than one municipality, 
# allowing mapping per municipality.

# For analysis per project, need to eliminate duplicates per project ID as below

db <- read_csv("Data_S1_Restoration_Project_Database_RowPerMunicipality_2024-07-20.csv")

##### clean and tidy database for analysis

# to clear all accented characters if needed
#db_mun$Title <- stringi::stri_trans_general(db_mun$Title, "Latin-ASCII")

# filter out observation that includes coral transplantation - non-terrestrial
db <- db %>% filter(Restoration_action_1 != "Transplantation of corals")

# set categorical variables to group/summarise by
# and convert numeric variables where needed

summary(db)
names(db)

# set NAs where necessary

db_row_per_municip <- db %>%
  mutate(Land_area_ha = case_when(Land_area_ha == "NI" ~ NA_character_,
                                  TRUE ~ Land_area_ha),
         Organisation = case_when(Organisation == "NI" ~ NA_character_,
                                  TRUE ~ Organisation),
         Organisation_type = case_when(Organisation_type == "NI" ~ NA_character_,
                                       TRUE ~ Organisation_type),
         Finance_type = case_when(Finance_type == "NI" ~ NA_character_,
                                  TRUE ~ Finance_type ),
         Executor_type= case_when(Executor_type  == "NI" ~ NA_character_,
                                  TRUE ~ Executor_type  ),
         Publication_date = case_when(Publication_date  == "NI" ~ NA_character_,
                                      TRUE ~ Publication_date  ),
         Activity_initiation_year = case_when(Activity_initiation_year   == "NI" ~ NA_character_,
                                              TRUE ~ Activity_initiation_year),
         Compensation = case_when(Compensation == "NI" ~ NA_character_,
                                  TRUE ~ Compensation),
         Project_phase = case_when(Project_phase == "NI" ~ NA_character_,
                                   TRUE ~ Project_phase),
         Monitoring = case_when(Monitoring == "NI" ~ NA_character_,
                                TRUE ~ Monitoring),
         Monitoring_years = case_when(Monitoring_years == "NI" ~ NA_character_,
                                      TRUE ~ Monitoring_years),
         Target_land_cover = case_when(Target_land_cover == "NI" ~ NA_character_,
                                       TRUE ~ Target_land_cover),
         Restoration_type = case_when(Restoration_type == "NI" ~ NA_character_,
                                      TRUE ~ Restoration_type),
         Restoration_action = case_when(Restoration_action_1 == "NI" ~ NA_character_,
                                          TRUE ~ Restoration_action),
         Restoration_action_1 = case_when(Restoration_action_1 == "NI" ~ NA_character_,
                                          TRUE ~ Restoration_action_1),
         Disturbance = case_when(Disturbance == "NI" ~ NA_character_,
                                   TRUE ~ Disturbance),
         Disturbance_1 = case_when(Disturbance_1 == "NI" ~ NA_character_,
                                   TRUE ~ Disturbance_1),
         Type_of_land_being_restored = case_when(Type_of_land_being_restored == "NI" ~ NA_character_,
                                                 TRUE ~ Type_of_land_being_restored),
         Main_aim_of_restoration = case_when(Main_aim_of_restoration == "NI" ~ NA_character_,
                                             TRUE ~ Main_aim_of_restoration),
         Location = case_when(Location == "NI" ~ NA_character_,
                                             TRUE ~ Location),
         Potential_ecosystem = case_when(Potential_ecosystem == "NI" ~ NA_character_,
                              TRUE ~ Potential_ecosystem),
         Costs = case_when(Costs == "-" ~ NA_character_,
                           Costs == "NA" ~ NA_character_,
                           TRUE ~ Costs)) %>%
  mutate_at(vars(Author_Contact_Contractor :Main_aim_of_restoration),factor) %>%
  mutate_at(vars(Department:Potential_ecosystem),factor) %>%
  mutate(Costs = str_remove_all(Costs, "[£$, ]")) %>%
  mutate_at(vars(Land_area_ha, Costs), as.numeric)

summary(db_row_per_municip)

######
# summarise based on project ID i.e. without municipality/location data for project counts
# keeps only the first record for each project i.e. does not capture multiple municipalities, etc. 
######

names(db)

db_row_per_project <- db_row_per_municip %>%
  distinct(Numero_ID, .keep_all = T) %>%
  select(-Dept_Mun, -Dept, -Mun)

#write_csv(db_row_per_project, "Restoration_Project_Database_RowPerProject.csv")

rm(db)

######
# plotting results per project
######

### number of projects per year, based on restoration activity initiation date ####

db_projcount_yr <- db_row_per_project %>%
  select(Numero_ID, Title, Compensation, Activity_initiation_year) %>%
  filter(Activity_initiation_year != "1963") %>%
  filter(Activity_initiation_year != "NI") %>%
  distinct()

(a <- ggplot(db_projcount_yr, aes(x = as.numeric(as.character(Activity_initiation_year)))) +
    geom_bar(aes(fill = Compensation), position = "stack") +
    scale_fill_manual(values = compensation_pal) +
    scale_y_continuous(limits = c(0, 60)) +
    scale_x_continuous(limits = c(1990, 2020), n.breaks = 20) +
    labs(subtitle = "Excludes a single project initiated in 1963") +
    ylab("Number of projects") +
    xlab(paste0("Year of restoration activity initiation (n = ", nrow(db_projcount_yr), ")")) +
    theme_classic())
ggsave("./figures/Project_count_by_year_barplot.pdf")

### number of projects per year, based on report publication date ####

db_projcount_yr2 <- db_row_per_project %>%
  select(Numero_ID, Title, Compensation, Publication_date) %>%
  filter(Publication_date != "1963") %>%
  filter(Publication_date != "NI") %>%
  distinct()

(b <- ggplot(db_projcount_yr2, aes(x = as.numeric(as.character(Publication_date)))) +
    geom_bar(aes(fill = Compensation), position = "stack") +
    scale_fill_manual(values = compensation_pal) +
    scale_y_continuous(limits = c(0, 60)) +
    scale_x_continuous(limits = c(1990, 2020),
                       n.breaks = 20) +
  #  labs(subtitle = "Excludes a single project initiated in 1963") +
    ylab("Number of projects") +
    xlab(paste0("Year of publication (n = ", nrow(db_projcount_yr2), ")")) +
    theme_classic())

pubs <- grid.arrange(a, b)
ggsave("./figures/Barplot_projects_byYear.pdf", pubs, width = 18, height = 10, units = "cm")
dev.off()

### number of projects reporting area restored

db_with_land_area <- db_row_per_project %>%
  filter(Land_area_ha != "NI")

print(paste("Proportion of all projects with area data = ", 404/650))

db_with_land_area$Land_area_ha <- as.numeric(db_with_land_area$Land_area_ha)
print(paste("Total land area (ha) under restoration summed = ", sum(db_with_land_area$Land_area_ha)))

### number of projects per organisation type, and compensation #####

db_projcount_org_comp <- db_row_per_project %>%
  select(Numero_ID, Title, Organisation_type, Compensation) %>%
  distinct() %>%
  mutate(Organisation_type = case_when(is.na(Organisation_type) ~ "No information",
                                       !is.na(Organisation_type) ~ paste(Organisation_type))) %>%
  mutate(Organisation_type = case_when(Organisation_type == "Ministry of the environment" ~ "Ministry of the Environment",
                                       TRUE ~ paste(Organisation_type))) %>%
  group_by(Organisation_type, Compensation) %>%
  summarise(n_projects = n())
#write_csv(db_projcount_org_comp, "./figures/organisations_compensation_table.csv")

db_projcount_total <- sum(db_projcount_org_comp$n_projects, na.rm = T)

db_projcount_comp <- db_projcount_org_comp %>%
  group_by(Compensation) %>%
  summarise(n_project = sum(n_projects))
#write_csv(db_projcount_comp, "./figures/compensation_table.csv")

ggplot(db_projcount_org_comp, aes(x = reorder(Organisation_type, n_projects), y = n_projects)) +
  geom_bar(aes(fill = Compensation), stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = compensation_pal) +
  xlab("Organisation type") +
  ylab("Number of projects") +
  labs(subtitle = paste0("Total n = (", sum(db_projcount_org_comp$n_projects), ")")) +
#  facet_wrap(.~ Compensation) +
  theme_classic() +
  theme(#strip.background = element_blank(), strip.placement = "outside",
    strip.background = element_blank(), strip.text.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("./figures/barplot_org_types_compensation.pdf", height = 10, width = 16, units = "cm")

### number of projects per organisation type, and compensation, and restoration type #####

db_projcount_org_comp_type <- db_row_per_project %>%
  select(Numero_ID, Title, Organisation_type, Compensation, Restoration_type) %>%
  distinct() %>%
  mutate(Organisation_type = case_when(is.na(Organisation_type) ~ "No information",
                                       !is.na(Organisation_type) ~ paste(Organisation_type))) %>%
  mutate(Organisation_type = case_when(Organisation_type == "Ministry of the environment" ~ "Ministry of the Environment",
                                       TRUE ~ paste(Organisation_type))) %>%
  group_by(Organisation_type, Compensation, Restoration_type) %>%
  summarise(n_projects = n())

sum(db_projcount_org_comp_type$n_projects, na.rm = T)

ggplot(db_projcount_org_comp_type, aes(x = reorder(Organisation_type, n_projects), y = n_projects)) +
  geom_bar(aes(fill = Restoration_type), stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = restorationtype_pal) +
  xlab("Organisation type") +
  ylab("Number of projects") +
  labs(subtitle = paste0("Total n = (", sum(db_projcount_org_comp_type$n_projects), ")")) +
  facet_wrap(.~ Compensation) +
  theme_classic() +
  theme(strip.background = element_blank(), strip.placement = "outside",
        #  strip.background = element_blank(), strip.text.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("./figures/barplot_org_types_compensation_type.pdf", height = 10, width = 25, units = "cm")

rest_type_table <- db_projcount_org_comp_type %>%
  group_by(Restoration_type) %>%
  summarise(n_projects = sum(n_projects))
#write_csv(rest_type_table, "./figures/restoration_type_table.csv")

### number of projects per potential ecosystem ###

db_projcount_eco <- db_row_per_project %>%
  select(Numero_ID, Title, Compensation, Potential_ecosystem) %>%
  distinct() %>%
  group_by( Compensation, Potential_ecosystem) %>%
  summarise(n_projects = n()) %>%
  filter(!is.na(Potential_ecosystem)) # NAs are not informative here

db_projcount_eco$Potential_ecosystem <- fct_reorder(db_projcount_eco$Potential_ecosystem,
                                                    db_projcount_eco$n_projects,
                                                       .fun = "sum")

sum(db_projcount_eco$n_projects, na.rm = T)

ggplot(db_projcount_eco, aes(x = Potential_ecosystem, y = n_projects)) +
  geom_bar(aes(fill = Compensation), stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = compensation_pal) +
  xlab("Potential ecosystem") +
  ylab("Number of projects") +
  labs(subtitle = paste0("Total n = (", sum(db_projcount_eco$n_projects), ")")) +
#  facet_wrap(.~ Compensation) +
  theme_minimal() +
  theme(#strip.background = element_blank(), strip.placement = "outside",
    strip.background = element_blank(), strip.text.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("./figures/barplot_potential_ecosystems_compensation.pdf", height = 5, width = 8)

### number of projects per potential ecosystem, split by restoration action ###

db_projcount_eco_type <- db_row_per_project %>%
  select(Numero_ID, Title, Compensation, Potential_ecosystem, Restoration_type) %>%
  distinct() %>%
  group_by(Compensation, Potential_ecosystem, Restoration_type) %>%
  summarise(n_projects = n()) %>%
  filter(!is.na(Potential_ecosystem)) # NAs are not informative here

db_projcount_eco_type$Potential_ecosystem <- fct_reorder(db_projcount_eco_type$Potential_ecosystem,
                                                         db_projcount_eco_type$n_projects,
                                                       .fun = "sum")

sum(db_projcount_eco_type$n_projects, na.rm = T)

ggplot(db_projcount_eco_type, aes(x = Potential_ecosystem, y = n_projects)) +
  geom_bar(aes(fill = Restoration_type), stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = restorationtype_pal) +
  xlab("Potential ecosystem") +
  ylab("Number of projects") +
  labs(subtitle = paste0("Total n = (", sum(db_projcount_eco_type$n_projects), ")")) +
  facet_wrap(.~ Compensation) +
  theme_classic() +
  theme(strip.background = element_blank(), strip.placement = "outside",
    #strip.background = element_blank(), strip.text.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("./figures/Potential_ecosystems_barplot_restorationtype_comp.pdf", height = 10, width = 25, units = "cm")

ggplot(db_projcount_eco_type, aes(x = Potential_ecosystem, y = n_projects)) +
  geom_bar(aes(fill = Restoration_type), stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = restorationtype_pal) +
  xlab("Potential ecosystem") +
  ylab("Number of projects") +
  labs(subtitle = paste0("Total n = (", sum(db_projcount_eco_type$n_projects), ")")) +
  #facet_wrap(.~ Compensation) +
  theme_classic() +
  theme(#strip.background = element_blank(), strip.placement = "outside",
        strip.background = element_blank(), strip.text.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("./figures/Potential_ecosystems_barplot_restorationtype_all.pdf",height = 10, width = 25, units = "cm")

### number of projects per target land cover ####

db_proj_targetland <- db_row_per_project %>%
  select(Numero_ID, Title, Compensation, Target_land_cover) %>%
  distinct() %>%
  group_by( Compensation, Target_land_cover) %>%
  summarise(n_projects = n()) %>%
  mutate(Target_land_cover = as.factor(Target_land_cover)) %>%
  filter(!is.na(Target_land_cover)) # NAs are not informative here

db_proj_targetland$Target_land_cover <- fct_reorder(db_proj_targetland$Target_land_cover,
                                                    db_proj_targetland$n_projects,
                                                       .fun = "sum")

sum(db_proj_targetland$n_projects, na.rm = T)

ggplot(db_proj_targetland, aes(x = Target_land_cover, y = n_projects)) +
  geom_bar(aes(fill = Compensation), stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = compensation_pal) +
  xlab("Target land cover") +
  ylab("Number of projects") +
  labs(subtitle = paste0("Total n = (", sum(db_proj_targetland$n_projects), ")")) +
#  facet_wrap(.~ Compensation) +
  theme_minimal() +
  theme(#strip.background = element_blank(), strip.placement = "outside",
    strip.background = element_blank(), strip.text.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("./figures/barplot_target_land_cover_comp.pdf", height = 5, width = 8)

### number of projects per target land cover and potential ecosystem ####

db_proj_targetland_eco <- db_row_per_project %>%
  select(Numero_ID, Title, Compensation, Target_land_cover, Potential_ecosystem) %>%
  distinct() %>%
  group_by( Compensation, Target_land_cover,Potential_ecosystem) %>%
  summarise(n_projects = n()) %>%
  mutate(Target_land_cover = as.factor(Target_land_cover)) %>%
  filter(!is.na(Target_land_cover)) %>% # NAs are not informative here
  filter(!is.na(Potential_ecosystem))

# db_proj_targetland_eco$db_proj_targetland_eco <- fct_reorder(db_proj_targetland_eco$Target_land_cover,
#                                                          db_proj_targetland_eco$n_projects,
#                                                     .fun = "sum")

sum(db_proj_targetland_eco$n_projects, na.rm = T)
unique(db_proj_targetland_eco$Potential_ecosystem)

#Andean forest, High Andean forest,Tropical dry forest,Sub-Andean forest,Tropical rain forest, Wetland,Paramo,Mangrove,Flooded savannah    
#eco_pal<- c("#01665E", "#35978F", "#BF812D", "#80CDC1", "#003C30", "#8DA0CB",  "#DFC27D", "#C7EAE5", "#543005" ) 
#High Andean forest,Wetland,Andean forest,Tropical dry forest,Paramo,Sub-Andean forest  , Tropical rain forest
eco_pal2 <- c("#01665E", "#35978F", "#DFC27D","#80CDC1", "#BF812D",  "#003C30", "#8DA0CB")#  "#C7EAE5" "#543005"

ggplot(db_proj_targetland_eco, aes(x = reorder(Target_land_cover, n_projects), y = n_projects)) +
  geom_bar(aes(fill = Potential_ecosystem), stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = eco_pal2) +
  xlab("Target land cover") +
  ylab("Number of projects") +
  labs(subtitle = paste0("Total n = (", sum(db_proj_targetland_eco$n_projects), "), all projects non-compensation")) +
#  facet_wrap(.~ Compensation) +
  theme_minimal() +
  theme(strip.background = element_blank(), strip.placement = "outside",
    #strip.background = element_blank(), strip.text.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("./figures/barplot_target_land_cover_comp_eco.pdf", height = 5, width = 8)

### restoration aim reported by projects ####

db_projcount_aim <- db_row_per_project %>%
  select(Numero_ID, Title, Compensation, Main_aim_of_restoration) %>%
  distinct() %>%
  group_by( Compensation, Main_aim_of_restoration) %>%
  summarise(n_projects = n()) %>%
  mutate(Main_aim_of_restoration = as.factor(Main_aim_of_restoration)) %>%
  filter(!is.na(Main_aim_of_restoration)) # NAs are not informative here

db_projcount_aim$Main_aim_of_restoration <- fct_reorder(db_projcount_aim$Main_aim_of_restoration,
                                                           db_projcount_aim$n_projects,
                                                     .fun = "sum")

sum(db_projcount_aim$n_projects, na.rm = T)


ggplot(db_projcount_aim, aes(x = Main_aim_of_restoration, y = n_projects)) +
  geom_bar(aes(fill = Compensation), stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = compensation_pal) +
  xlab("Restoration aim") +
  ylab("Number of projects") +
  labs(subtitle = paste0("Total n = (", sum(db_projcount_aim$n_projects), ")")) +
#  facet_wrap(.~ Compensation) +
  theme_minimal() +
  theme(#strip.background = element_blank(), strip.placement = "outside",
    strip.background = element_blank(), strip.text.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("./figures/barplot_restoration_aim.pdf", height = 5, width = 8)

### project monitoring ####

db_projcount_mon <- db_row_per_project %>%
  select(Numero_ID, Title, Compensation, Monitoring_years) %>%
  mutate(Monitoring_years = as_factor(case_when(is.na(Monitoring_years) ~ "No information",
                                      Monitoring_years == "<1year" ~ "<1 year",
                                      Monitoring_years == ">5year" ~ ">5 years",
                                      Monitoring_years == "1-3year" ~ "1-5 years",
                                      Monitoring_years == "1-5year" ~ "1-5 years",
                                      ))) %>%
  distinct()

db_projcount_mon$Monitoring_years <- fct_relevel(db_projcount_mon$Monitoring_years, 
                                                    c("<1 year", "1-5 years",">5 years","No information"))

mon_totals <- db_projcount_mon %>%
  group_by(Compensation, Monitoring_years) %>%
  summarise(n_projects = n(),
            pc_total = (n_projects/652)*100) %>%
  ungroup() %>%
  mutate(pc_total = round(pc_total, 2))

#write_csv(mon_totals, "./figures/monitoring_table.csv")

ggplot(db_projcount_mon, aes(x = Monitoring_years)) +
  geom_bar(aes(fill = Compensation)) +
  coord_flip() +
  scale_fill_manual(values = compensation_pal) +
  xlab("Years of monitoring") +
  ylab("Number of projects") +
  labs(subtitle = paste0("Total n = (", nrow(db_projcount_mon), ")")) +
 # facet_wrap(.~ Compensation) +
  theme_classic() +
  theme(#strip.background = element_blank(), strip.placement = "outside",
    strip.background = element_blank(), strip.text.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("./figures/barplot_monitoring.pdf", height = 2, width = 4)

## restoration techniques

# frequency across all projects; projects report >1 action
db_projcount_techniques <- db_row_per_project %>%
  select(Compensation, Restoration_action_1, Restoration_action_2,
         Restoration_action_3, Restoration_action_4, Restoration_action_5) %>%
  pivot_longer(Restoration_action_1:Restoration_action_5, names_to = "label", values_to = "Actions") %>%
  filter(!is.na(Actions)) # NAs are not meaningful here

# sort out duplications/typos
clean_actions <- read_csv("actions.csv")
clean_actions2 <- setNames(clean_actions$new, clean_actions$value)

db_projcount_techniques$Actions <- recode(db_projcount_techniques$Actions, !!!clean_actions2)
# end

db_projcount_techniques_summary <- db_projcount_techniques %>%
  group_by(Compensation, Actions) %>%
  summarise(n_actions = n()) %>%
  mutate(Compensation = case_when(is.na(Compensation) ~ "No information", 
                             !is.na(Compensation) ~ paste(Compensation))) 

db_projcount_techniques_summary$Compensation <- fct_relevel(db_projcount_techniques_summary$Compensation, 
                                                c("No", "Yes", "No information"))

db_projcount_techniques_summary$Actions <- fct_reorder(db_projcount_techniques_summary$Actions, 
                                                       db_projcount_techniques_summary$n_actions, .fun = sum)

db_techniques_samplesize <- db_row_per_project %>%
  select(Numero_ID, Title, Compensation, 
         Restoration_action_1, Restoration_action_2, Restoration_action_3, Restoration_action_4, Restoration_action_5) %>%
  filter(!is.na(Restoration_action_1)) %>% # NAs are not meaningful here
  distinct()

n_act <- length(unique(db_projcount_techniques_summary$Actions)) # n actions
n_prj <- nrow(db_techniques_samplesize) # n projects reporting actions

ggplot(db_projcount_techniques_summary, aes(x = Actions, y = n_actions)) +
  geom_bar(aes(fill = Compensation), stat = "identity") +
  coord_flip()+
  scale_fill_manual(values = compensation_pal4) +
  xlab("Restoration action type") +
  ylab("Number of projects reporting") +
  labs(subtitle = paste0("Restoration actions (n = ", n_act, ") ", "\n across ", n_prj, " projects reporting their activities")) +
  #facet_wrap(.~ Compensation) +
  theme_minimal() +
  theme(#strip.background = element_blank(), strip.placement = "outside",
    strip.background = element_blank(), strip.text.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("./figures/barplot_restoration_actions.pdf", height = 5, width = 8)

#write_csv(db_projcount_techniques_summary, "./figures/restoration_actions_table.csv")

### type of disturbance reported ####

# frequency across all projects; projects report >1 action
db_projcount_disturb <- db_row_per_project %>%
  select(Compensation, Disturbance_1, Disturbance_2, Disturbance_3, Disturbance_4) %>%
  pivot_longer(Disturbance_1:Disturbance_4, names_to = "label", values_to = "Disturbance") %>%
  filter(!is.na(Disturbance)) # NAs are not meaningful here

# sort out duplications/typos
clean_disturbs <- read_csv("disturbs.csv")
clean_disturbs2 <- setNames(clean_disturbs$new, clean_disturbs$value)

db_projcount_disturb$Disturbance <- recode(db_projcount_disturb$Disturbance, !!!clean_disturbs2)
# end

db_projcount_disturb_summary <- db_projcount_disturb %>%
  group_by(Compensation, Disturbance) %>%
  summarise(n_disturbs = n()) %>%
  mutate(Compensation = case_when(is.na(Compensation) ~ "No information", 
                                  !is.na(Compensation) ~ paste(Compensation))) %>%
  ungroup()

db_projcount_disturb_summary$Compensation <- fct_relevel(db_projcount_disturb_summary$Compensation, 
                                                  c("No", "Yes", "No information"))

db_projcount_disturb_summary$Disturbance <- fct_reorder(db_projcount_disturb_summary$Disturbance, db_projcount_disturb_summary$n_disturbs, .fun = sum)

db_disturb_samplesize <- db %>%
  select(Numero_ID, Title, Compensation, 
         Disturbance_1, Disturbance_2, Disturbance_3, Disturbance_4) %>%
  filter(!is.na(Disturbance_1)) %>% # NAs are not meaningful here
  distinct()

n_act <- length(unique(db_projcount_disturb_summary$Disturbance)) # n disturbances
n_prj <- nrow(db_techniques_samplesize) # n projects reporting disturbances

ggplot(db_projcount_disturb_summary, aes(x = Disturbance, y = n_disturbs)) +
  geom_bar(aes(fill = Compensation), stat = "identity") +
  coord_flip()+
  scale_fill_manual(values = compensation_pal4) +
  xlab("Disturbance type") +
  ylab("Number of projects reporting") +
  labs(subtitle = paste0("Disturbance type (n = ", n_act, ") ", "\n across ", n_prj, " projects reporting their activities")) +
  #facet_wrap(.~ Compensation) +
  theme_minimal() +
  theme(#strip.background = element_blank(), strip.placement = "outside",
    strip.background = element_blank(), strip.text.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("./figures/barplot_disturbance.pdf", height = 3, width = 8)

#write_csv(db_projcount_disturb_summary, "./figures/disturbances_table.csv")

# total area of land within each potential ecosystem type, split by aim of restoration

db_projarea_eco_aim <- db_row_per_project %>%
  select(Numero_ID, Title, Compensation, Main_aim_of_restoration, Potential_ecosystem, Land_area_ha) %>%
  filter(!is.na(Land_area_ha)) %>%
  distinct() %>%
  group_by( Compensation, Main_aim_of_restoration, Potential_ecosystem) %>%
  summarise(total_area = sum(Land_area_ha)) %>%
  mutate(Main_aim_of_restoration = as.factor(Main_aim_of_restoration)) %>%
  filter(Potential_ecosystem != "Several ecosystems") # NAs are not informative here

db_projarea_eco_aim$Main_aim_of_restoration <- fct_relevel(db_projarea_eco_aim$Main_aim_of_restoration, 
                                                           "Disaster risk management",
                                                           "Control and elimination of invasive species",
                                                           "Production (agroforestry/silvo-pasture)",
                                                           "Water resource protection",
                                                           "Hydrological restoration",
                                                           "Soil erosion prevention",
                                                           "Recovery and conservation of natural resources" ,
                                                           "Ecological restoration of mangroves",
                                                           "Ecological connectivity")

db_projcount_eco_aim <- db_row_per_project %>%
  select(Numero_ID, Title, Compensation, Main_aim_of_restoration, Potential_ecosystem, Land_area_ha) %>%
  filter(!is.na(Land_area_ha)) %>%
  distinct() %>%
  group_by( Compensation, Main_aim_of_restoration, Potential_ecosystem) %>%
  summarise(n_projects = n()) %>%
  mutate(Main_aim_of_restoration = as.factor(Main_aim_of_restoration))%>%
  filter(Potential_ecosystem != "Several ecosystems") 
#filter(!is.na(Main_aim_of_restoration)) # NAs are not informative here

db_projcount_eco_aim$Main_aim_of_restoration <- fct_relevel(db_projcount_eco_aim$Main_aim_of_restoration,
                                                            "Disaster risk management",
                                                            "Control and elimination of invasive species",
                                                            "Production (agroforestry/silvo-pasture)",
                                                            "Water resource protection",
                                                            "Hydrological restoration",
                                                            "Soil erosion prevention",
                                                            "Recovery and conservation of natural resources" ,
                                                            "Ecological restoration of mangroves",
                                                            "Ecological connectivity")

#Andean forest, High Andean forest,Tropical dry forest,Sub-Andean forest,Tropical rain forest, Wetland,Paramo,Mangrove,Flooded savannah    
#eco_pal<- c("#01665E", "#35978F", "#BF812D", "#80CDC1", "#003C30", "#8DA0CB",  "#DFC27D", "#C7EAE5", "#543005" ) 
#High Andean forest,Wetland,Andean forest,Tropical dry forest,Paramo,Sub-Andean forest  , Tropical rain forest
#eco_pal2 <- c("#01665E", "#35978F", "#DFC27D","#80CDC1", "#BF812D",  "#003C30", "#8DA0CB")#  "#C7EAE5" "#543005"
eco_pal3<- c("#01665E", "#543005", "#35978F",  "#C7EAE5", "#DFC27D", "#80CDC1","#BF812D", "#003C30", "#8DA0CB") 

(a <- ggplot(db_projarea_eco_aim, aes(x = Main_aim_of_restoration, y = total_area)) +
  geom_bar(aes(fill = Potential_ecosystem), stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = eco_pal3) +
  xlab("Restoration aim") +
  ylab("Total area (ha)") +
  labs(subtitle = paste0("Total projects n = (", sum(db_projcount_eco_aim$n_projects), "), excluding projects that reported 'several ecosystems'")) +
  #  facet_wrap(.~ Compensation) +
  theme_minimal() +
  scale_y_continuous(breaks = c(0, 5000, 10000, 15000, 20000, 25000)) +
  theme(strip.background = element_blank(), strip.placement = "outside",
        #strip.background = element_blank(), strip.text.x = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 1),
        axis.ticks.x.bottom = element_line(colour = "black")))

ggsave("./figures/barplot_area_aim_eco.pdf", height = 5, width = 10)

(b <- ggplot(db_projcount_eco_aim, aes(x = Main_aim_of_restoration, y = n_projects)) +
  geom_bar(aes(fill = Potential_ecosystem), stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = eco_pal3) +
  xlab("Restoration aim") +
  ylab("Number of projects") +
  labs(subtitle = paste0("Total projects n = (", sum(db_projcount_eco_aim$n_projects), "), excluding projects that reported 'several ecosystems'")) +
  #  facet_wrap(.~ Compensation) +
  theme_minimal() +
  theme(strip.background = element_blank(), strip.placement = "outside",
        #strip.background = element_blank(), strip.text.x = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 1),
        axis.ticks.x.bottom = element_line(colour = "black")))

 ggsave("./figures/barplot_count_aim_eco.pdf", height = 5, width = 10)

# combined plot

(a <- ggplot(db_projarea_eco_aim, aes(x = Main_aim_of_restoration, y = total_area)) +
    geom_bar(aes(fill = Potential_ecosystem), stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = eco_pal3) +
    xlab("Restoration aim") +
    ylab("Total area (ha)") +
  #  labs(subtitle = paste0("Total projects n = (", sum(db_projcount_eco_aim$n_projects), "), excluding projects that reported 'several ecosystems'")) +
    #  facet_wrap(.~ Compensation) +
    theme_minimal() +
    theme(strip.background = element_blank(), strip.placement = "outside",
          #strip.background = element_blank(), strip.text.x = element_blank(),
          axis.text.x = element_text(angle = 0, vjust = 1),
          axis.ticks.x.bottom = element_line(colour = "black"),
          legend.position="none")) 

(b <- ggplot(db_projcount_eco_aim, aes(x = Main_aim_of_restoration, y = n_projects)) +
    geom_bar(aes(fill = Potential_ecosystem), stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = eco_pal3) +
    xlab("Restoration aim") +
    ylab("Number of projects") +
 #   labs(subtitle = paste0("Total projects n = (", sum(db_projcount_eco_aim$n_projects), "), excluding projects that reported 'several ecosystems'")) +
    #  facet_wrap(.~ Compensation) +
    theme_minimal() +
    theme(strip.background = element_blank(), strip.placement = "outside",
          #strip.background = element_blank(), strip.text.x = element_blank(),
          axis.text.x = element_text(angle = 0, vjust = 1),
          axis.ticks.x.bottom = element_line(colour = "black"),
          legend.position="none")) 
 
(c <- ggplot(db_projcount_eco_aim, aes(x = reorder(Main_aim_of_restoration, n_projects), y = n_projects)) +
    geom_bar(aes(fill = Potential_ecosystem), stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = eco_pal3) +
    theme(legend.position="bottom")) 

l <- get_legend(c)

(projecoaim <- plot_grid(a, 
                         b,
                         l, 
                         nrow = 3, rel_heights = c(5,5,2)))
ggsave("./figures/barplot_aim_eco.pdf", projecoaim, height = 12, width = 25, units = "cm")
#dev.off()

# Sankey of the finance source (national, international, etc), implementer, and whether compensation, public or private

db_san <- db_row_per_project%>%
  select(Executor_type, Finance_type, Organisation_type, Activity_initiation_year, Publication_date, Compensation) %>%
  mutate(Activity_initiation_year = as.numeric(as.character(Activity_initiation_year)),
         Publication_date = as.numeric(as.character(Publication_date)),
         Earliest_record_date = pmin(Activity_initiation_year, Publication_date, na.rm = T),
         Finance_type = as.character(Finance_type),
         Executor_type = as.character(Executor_type),
         Organisation_type = as.character(Organisation_type),
         Finance_type = replace_na(Finance_type, "No information"),
         Executor_type = replace_na(Executor_type, "No information"),
         Organisation_type = replace_na(Organisation_type, "No information"),
         )

d_san_pre2014 <- db_san %>%
  filter(Earliest_record_date < 2014) 

d_san_post2014 <- db_san %>%
  filter(Earliest_record_date >= 2014)

d_san_s <- db_san %>%
  make_long(Finance_type, 
            Executor_type, 
            Organisation_type)

d_san_pre2014_s <- d_san_pre2014 %>%
  make_long(Finance_type, 
            Executor_type, 
            Organisation_type)

d_san_post2014_s <- d_san_post2014 %>%
  make_long(Finance_type, 
            Executor_type, 
            Organisation_type)

(san2 <- ggplot(d_san_pre2014_s, aes(x = x, next_x = next_x, 
                        node = node, next_node = next_node, 
                        fill = factor(node),
                        label = node)) +
    geom_sankey(flow.alpha = 0.5,          #This Creates the transparency of your node 
                node.color = "black",     # This is your node color        
                show.legend = F) +        # This determines if you want your legend to show
    geom_sankey_label(size = 2, 
                      color = "black", 
                      fill = "white",
                      alpha = 0.5) +
    scale_fill_viridis_d(option = "inferno") +
    labs(title = "Earliest project record pre-2014",
         subtitle = "Publication or activity initiation date (whichever is earlier)",
         caption = paste0("n projects = ", nrow(d_san_pre2014)))+#,
         #fill = 'Nodes') +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.title.x = element_blank()
          ))
ggsave("./figures/Sankey_Projects_Pre2014_OrgFunding.pdf",
       width = 15, height = 10, units = "cm")

(san3 <- ggplot(d_san_post2014_s, aes(x = x, next_x = next_x, 
                                     node = node, next_node = next_node, 
                                     fill = factor(node),
                                     label = node)) +
    geom_sankey(flow.alpha = 0.5,          #This Creates the transparency of your node 
                node.color = "black",     # This is your node color        
                show.legend = F) +        # This determines if you want your legend to show
    geom_sankey_label(size = 2, 
                      color = "black", 
                      fill = "white",
                      alpha = 0.5) +
    scale_fill_viridis_d(option = "inferno") +
    labs(title = "Earliest project record post-2014",
         subtitle = "Publication or activity initiation date (whichever is earlier)",
         caption = paste0("n projects = ", nrow(d_san_post2014)))+#,
    #fill = 'Nodes') +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.title.x = element_blank()
    ))
ggsave("./figures/Sankey_Projects_Post2014_OrgFunding.pdf",
       width = 15, height = 10, units = "cm")

(san4 <- ggplot(d_san_s, aes(x = x, next_x = next_x, 
                                      node = node, next_node = next_node, 
                                      fill = factor(node),
                                      label = node)) +
    geom_sankey(flow.alpha = 0.5,          #This Creates the transparency of your node 
                node.color = "black",     # This is your node color        
                show.legend = F) +        # This determines if you want your legend to show
    geom_sankey_label(size = 2, 
                      color = "black", 
                      fill = "white",
                      alpha = 0.5) +
    scale_fill_viridis_d(option = "inferno") +
    labs(title = "All projects",
         subtitle = " ",
         caption = paste0("n projects = ", nrow(db_san)))+#,
    #fill = 'Nodes') +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.title.x = element_blank()
    ))
ggsave("./figures/Sankey_Projects_All_OrgFunding.pdf",
       width = 15, height = 10, units = "cm")


################
#### baseline spatial data and settings ####
################

# theme for maps ####
map_theme <- theme_set(theme_void)
map_theme <- theme_update(panel.background = element_rect(colour = "black"), plot.margin = unit(c(1,1,1,1), "cm"))
# transparent fill for polygon
ellicol_trnsp <- adjustcolor("white", alpha.f = 0)

# Colombia country outline
outline <- read_sf("D:/Work - Bangor/Data/Colombia - general/DANE_Geografia/COLOMBIA_outline_EPSG3116.gpkg")
outline$Country <- "Colombia"; outline <- dplyr::select(outline, Country); plot(outline)

# read in the muncipality vectors with standardised department-municipality names
mun_vec <- st_read("D:/Work - Bangor/Data/Colombia - general/DANE_Geografia/MGN_MPIO_POLITICO_EWT_Agronet_EPSG3116.gpkg")
names(mun_vec)
#plot(mun_vec["depmun"])

dep_vec <- st_read("D:/Work - Bangor/Data/Colombia - general/DANE_Geografia/MGN_DPIO_POLITICO_EWT_EPSG3116.gpkg")
names(dep_vec)
#plot(dep_vec["DPTO_CCDGO"])

# standardise department names 
mun_vec_t <- st_drop_geometry(mun_vec)
dep_standard <- as.data.frame(str_split(mun_vec_t$depmun, pattern = "_", simplify = T))
mun_vec_t$dep_standard <- dep_standard$V1
mun_vec_s <- mun_vec_t %>%
  select(DPTO_CNMBR, dep_standard) %>%
  distinct()

dep_vec2 <- left_join(dep_vec, mun_vec_s)
names(dep_vec2)
#plot(dep_vec2["dep_standard"])

# Protected areas in Colombia 
# data downloaded from the World Database of Protected Areas in August 2021
wdpa <- st_read("D:/Work - Bangor/Data/Colombia - general/WDPA/WDPA_WDOECM_Aug2021_Public_COL_shp/WDPA_All_Merge.gpkg")
names(wdpa)
#plot(wdpa["DESIG_ENG"], key.pos = 1) # plot PAs based on designation type

#####
## analysis based on administrative units (municipality/departments)
#####

db_mun <- db_row_per_municip %>%
  select(Numero_ID, Title, Compensation, Department, Municipality, Dept_Mun, Code_DANE_Dep, Code_DANE_Mun) 

names(db_mun) # view column headings

# count number of projects per department ####
names(db_mun)

dep_projects_comp <- db_mun %>%
  group_by(Department, Compensation) %>%
  count()

dep_projects <- db_mun %>%
  group_by(Department) %>%
  count()

# map n projects by dep_mun
dep_vec_projcount <- left_join(dep_vec2, dep_projects, by = c("dep_standard" = "Department"))

# write out summary table
dep_projcount_table <- dep_vec_projcount %>%
  select(DPTO_CCDGO, DPTO_CNMBR, n) %>%
  st_drop_geometry()
#write_csv(dep_projcount_table, "./figures/Number_projects_per_department.csv")

# write out geopackage
dep_projcount_gpkg <- dep_vec_projcount %>%
  select(DPTO_CCDGO, DPTO_CNMBR, n) 
st_write(dep_projcount_gpkg, "./spatialout/Number_projects_per_department.gpkg")

# scale
scale_max <- max(dep_vec_projcount$n, na.rm = T)
scale_min <- min(dep_vec_projcount$n, na.rm = T)
e_scale_mun <- scale_fill_gradient(low = "lightblue1",  high = "royalblue4", na.value = "white",
                                   limits = c(0, scale_max),
                                   labels = c("0", "20", "40", "60", "80", "100"),
                                   breaks = c(0, 20, 40, 60, 80, 100),
                                   guide_colourbar(title = "Number of projects"))

t <- Sys.time(); t<- str_replace_all(t, ":", "_")

#pdf(file = paste0("./Simple_project_count_dep",t,".pdf"))
ggplot(dep_vec_projcount) +
  geom_sf(aes(fill = (n)), colour = NA) +
  geom_sf(data = outline, fill = ellicol_trnsp, colour = "black") +
  e_scale_mun +
  map_theme()
#dev.off()

# count number of projects per depmun (municipality) ##

# including compensation column (yes/no)
dep_mun_projects_comp <- db_mun %>%
  group_by(Dept_Mun, Compensation) %>%
  count()

# excluding compensation column i.e. all projects total
dep_mun_projects <- db_mun %>%
  group_by(Dept_Mun) %>%
  count()

# join the database with the spatial municipality file based on the standardised municipality  names
mun_vec_projcount <- left_join(mun_vec, dep_mun_projects, by = c("depmun" = "Dept_Mun"))
names(mun_vec_projcount)

# use the same scale as above for departments - uncomment this section to create a separate scale for the
# muncipality map if wanted

# create unique date/time stamp to use in the output file name (to avoid over-writing previous versions)
t <- Sys.time(); t<- str_replace_all(t, ":", "_")

# scale for the map
scale_max <- max(mun_vec_projcount$n, na.rm = T)
scale_min <- min(mun_vec_projcount$n, na.rm = T)

scale_national <- scale_fill_viridis(option = "magma",
                                     direction = -1,
                                     na.value = NA,
                                     limits = c(0, scale_max),
                                     labels = c("0", "10", "20", "30", "40", "50"),
                                     breaks = c(0,10,20,30,40,50))#,
# guide_colourbar(title = "Number of projects"))
# 
# scale_national <- scale_fill_gradient(low = "#a1d99b", 
#                                       high = "#00441b",
#                                       na.value = "white",
#                                       limits = c(0, scale_max),
#                                       labels = c("0", "10", "20", "30", "40", "50"),
#                                       breaks = c(0,10,20,30,40,50))#,
# # guide_colourbar(title = "Number of projects"))


(map_a <- ggplot(mun_vec_projcount) +
    geom_sf(data = outline, fill = "lightgrey", colour = "black") +
    geom_sf(aes(fill = (n)), colour = NA) +
    geom_sf(data = dep_vec, fill = ellicol_trnsp, colour = "grey", size = 1) +
    geom_sf(data = outline, fill = ellicol_trnsp, colour = "black") +
    scale_national +
    #title("Number of projects per municipality") +
    labs(fill = "n projects per municipality") +
    map_theme())
ggsave("./figures/map_projects_per_mun.pdf",
       height = 20, width = 15, units = "cm")

mun_vec_projcount_out <- mun_vec_projcount %>%
  select(DPTO_CCDGO,DPTO_CNMBR,MPIO_CCDGO,MPIO_CNMBR,depmun,n)

st_write(mun_vec_projcount_out, "./spatialout/Number_projects_per_municipality.gpkg")

#write_csv(st_drop_geometry(mun_vec_projcount_out), "./figures/Number_projects_per_municipality.csv") 

# add in project points

db_points<- read_sf("D:/Work - Bangor/Restoration database/Final version/Spatial_data/Project_points_fromDB_EWT_2023-12-09_EPSG3116.gpkg")

(map_a2 <- ggplot(mun_vec_projcount) +
    geom_sf(data = outline, fill = "#E6E6E6", colour = "black") +
    geom_sf(aes(fill = (n)), colour = NA) +
    geom_sf(data = dep_vec, fill = ellicol_trnsp, colour = "grey", size = 0.5) +
    #geom_sf(data = outline, fill = ellicol_trnsp, colour = "black") +
    geom_sf(data = db_points, pch = 21, fill = "black", 
            colour = "white", size = 1.5, alpha = 0.4) +
    scale_national +
    #title("Number of projects per municipality") +
    labs(fill = "n projects per municipality") +
    map_theme())
ggsave("./figures/map_projects_per_mun_w_points.pdf",
       height = 20, width = 15, units = "cm")

# join the database inc. compensation column with the spatial municipality file based on the standardised municipality  names
mun_vec_projcount_comp <- left_join(mun_vec, dep_mun_projects_comp, by = c("depmun" = "Dept_Mun"))
names(mun_vec_projcount_comp)

mun_vec_projcount_comp <- mun_vec_projcount_comp %>%
  filter(!is.na(Compensation))

(map_b <- ggplot(mun_vec_projcount_comp) +
    geom_sf(data = outline, fill = "lightgrey", colour = "black") +
    geom_sf(data = dep_vec, fill = ellicol_trnsp, colour = "grey") +
    geom_sf(aes(fill = (n)), colour = NA) +
    geom_sf(data = outline, fill = ellicol_trnsp, colour = "black") +
    scale_national +
    facet_grid(rows = vars(Compensation))+
    map_theme())
ggsave("./figures/map_projects_per_mun_comp.pdf",
       height = 20, width = 15, units = "cm")

# count number of projects per municp per 5-year time block

# including compensation column (yes/no)
dep_mun_projects_5yr <-
  db_row_per_municip %>%
  mutate(Activity_init_yr_num = as.numeric(as.character(Activity_initiation_year)),
         Activity_init_5year = case_when(Activity_init_yr_num < 2000 ~ "1960-2000",
                                         Activity_init_yr_num %in% 2000:2005 ~ "2000-2005",
                                         Activity_init_yr_num %in% 2005:2010 ~ "2005-2010",
                                         Activity_init_yr_num %in% 2010:2015 ~ "2010-2015",
                                         Activity_init_yr_num %in% 2015:2020 ~ "2015-2020",
                                         Activity_init_yr_num > 2020 ~ "2020 - present"))

dep_mun_projects_5yr_n <- dep_mun_projects_5yr %>%
  group_by(Dept_Mun, Activity_init_5year, Compensation) %>%
  count() 

mun_vec_projcount_5yr <- left_join(mun_vec, dep_mun_projects_5yr_n, by = c("depmun" = "Dept_Mun"))

mun_vec_projcount_5yr <- mun_vec_projcount_5yr %>%
  filter(!is.na(Compensation))

# scale for the map
scale_max <- max(mun_vec_projcount_5yr$n, na.rm = T)
scale_min <- min(mun_vec_projcount_5yr$n, na.rm = T)

scale_national_yr <- scale_fill_viridis(option = "magma",
                                     direction = -1,
                                     na.value = "white",
                                     limits = c(0, scale_max),
                                     labels = c("0", "5", "10", "15", "20", "25", "30"),
                                     breaks = c(0,5,10,15,20,25,30))#,


(map_c <- ggplot(mun_vec_projcount_5yr) +
    geom_sf(data = outline, fill = "lightgrey", colour = "black") +
    geom_sf(data = dep_vec, fill = ellicol_trnsp, colour = "grey") +
    geom_sf(aes(fill = (n)), colour = NA) +
    geom_sf(data = outline, fill = ellicol_trnsp, colour = "black") +
    scale_national_yr +
    facet_grid(Compensation~Activity_init_5year)+
    map_theme())
ggsave("./figures/Project_count_mun_5yr.pdf",
       height = 20, width = 105, units = "cm")


