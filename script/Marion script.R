# ======================================================================
# Build matrices for analyzes
# ======================================================================

# Load required packages
library(tidyverse)   
library(readxl)     

# Load original dataset containing species names
load("C:/Users/boisseaux/Dropbox/Mon PC (Jaboty20)/Postdoc_CEFE/Transplantation Alpine/Script_R_Marcet/Données initiales/TransPlantNetwork.RData")

# ======================================================================
# STEP 1. Species name harmonization & woodiness
# ======================================================================

# Import species name correspondences
species_corrections <- read.csv("TNRS_Correspondance_marion.csv", sep = ",")

# Standardize species names (replace spaces with underscores)
species_corrections <- species_corrections %>%
  mutate(name_species = gsub(" ", "_", name_species))

# Rename the column in species_corrections
species_corrections <- species_corrections %>%
  dplyr::rename(SpeciesName = Name_submitted)

# Apply corrections to main dataset (dat) - to load from the file RData - TransPlantNetwork
data_clean <- dat %>%
  left_join(species_corrections %>% select(SpeciesName, name_species),
            by = "SpeciesName")%>%
  mutate(SpeciesName = name_species) %>%
  select(-name_species)

#select only non-woody plants (herbs) with alltraits - to load from the file RData - TransPlantNetwork
woody_var <- alltraits %>% ungroup() %>% distinct(SpeciesName, woodiness, Plant_Veg_Height_cm)

data_clean <- data_clean %>%
  left_join(woody_var, by = "SpeciesName") %>%
  filter(woodiness != "woody" | is.na(woodiness)) %>%
  filter(Plant_Veg_Height_cm < 15 | is.na(woodiness)) #hist(data_clean$Plant_Veg_Height_cm) I removed Pinus



# ======================================================================
# STEP 2. Update treatments and controls
# ======================================================================

# Identify warm sites (both origin and destination) 
warm_sites <- data_clean %>%
  filter(Treatment == "Warm") %>% #there are 2 treatments: warm and LocalControl (origin control / destination control)
  group_by(Region)%>%
  distinct(originSiteID, destSiteID) #on ne peut differencier un site LocalControl s'il est origin ou destination que par rapport au "warm"

warm_sites$Region.OriginSiteID <- interaction(warm_sites$Region, warm_sites$originSiteID)
warm_sites$Region.destSiteID <- interaction(warm_sites$Region, warm_sites$destSiteID)

site_elevation_ref <- data_clean %>%
    filter(Treatment == "LocalControl") %>%       # Use only control plots
    group_by(Region, originSiteID) %>%                    # Each origin site
   summarise(Elevation = mean(Elevation, na.rm = TRUE), .groups = "drop") %>%
   arrange(desc(Elevation)) 

site_elevation_ref$Region.OriginSiteID<- interaction(site_elevation_ref$Region, site_elevation_ref$originSiteID)

site_elevation_origin <- data_clean %>%
  filter(Treatment== "LocalControl")%>%
  group_by(Region, originSiteID) %>%
  summarise(Elevation_origin = mean(Elevation, na.rm = TRUE), .groups = "drop")

site_elevation_origin$Region.OriginSiteID<- interaction(site_elevation_origin$Region, site_elevation_origin$originSiteID)

site_elevation_dest <- data_clean %>%
 filter(Treatment== "LocalControl")%>%
 group_by(Region, destSiteID) %>% #de toute facon 
  summarise(Elevation_dest = mean(Elevation, na.rm = TRUE), .groups = "drop")

site_elevation_dest$Region.destSiteID<- interaction(site_elevation_dest$Region, site_elevation_dest$destSiteID)

# Extraire les paires uniques Warm
warm_pairs <- data_clean %>%
  filter(Treatment == "Warm") %>%
  group_by(Region) %>%
  distinct(originSiteID, destSiteID)

warm_pairs$Region.OriginSiteID <- interaction(warm_pairs$Region, warm_pairs$originSiteID)
warm_pairs$Region.destSiteID <- interaction(warm_pairs$Region, warm_pairs$destSiteID)

warm_pairs <- warm_pairs %>%
  select(Region.OriginSiteID, Region.destSiteID)

# Joindre les élévations
warm_pairs <- warm_pairs %>%
  left_join(site_elevation_origin %>% select(Elevation_origin, Region.OriginSiteID), by = "Region.OriginSiteID") %>%
  left_join(site_elevation_dest %>% select(Elevation_dest, Region.destSiteID), by = "Region.destSiteID") %>%
  arrange(Elevation_origin)  

warm_pairs <- warm_pairs %>%
  mutate(downhill = Elevation_origin > Elevation_dest,
         diff = Elevation_origin - Elevation_dest)

print(warm_pairs, n=44)

# Reclassify control treatments 
data_clean$Region.OriginSiteID <- interaction(data_clean$Region, data_clean$originSiteID)
data_clean$Region.destSiteID <- interaction(data_clean$Region, data_clean$destSiteID)


data_clean <- data_clean %>%
  mutate(Treatment = case_when(
    Treatment == "Warm" ~ "Warm",
    Treatment == "LocalControl" & Region.OriginSiteID %in% warm_sites$Region.OriginSiteID ~ "Control_Origin",
    Treatment == "LocalControl" & Region.destSiteID %in% warm_sites$Region.destSiteID ~ "Control_Destination",
    TRUE ~ Treatment
  )) 

#controlling whether we do have the three levels
levels(factor(data_clean$Treatment))

# ======================================================================
# STEP 3. Survey dates summary table
# ======================================================================

survey_dates <- data_clean %>%
  select(Region.OriginSiteID, Region.destSiteID, Year) %>%
  distinct() %>%
  mutate(value = "surveyed") %>%
  pivot_wider(names_from = Year, values_from = value, values_fill = NA) %>%
  left_join(
    data_clean %>%
      select(Region.OriginSiteID, Region.destSiteID, destPlotID) %>%
      distinct(),
    by = c("Region.OriginSiteID", "Region.destSiteID")
  ) 

year_columns <- setdiff(names(survey_dates), c("Region.destSiteID", "Region.OriginSiteID"))

# Add number of survey years to table and filter in only plots monitored for ≥ 3 years and extract initial/final years
survey_dates_filtered <- survey_dates %>%
  mutate(n_years_surveyed = rowSums(across(all_of(year_columns)) == "surveyed", na.rm = TRUE)) %>%
  filter(n_years_surveyed >= 3)   # Keep only plots with ≥ 3 years
 
table(survey_dates_filtered$n_years_surveyed)

#before with Lula's script
2   3   4   5   6   7   8   9 
29  70  62 178 241 101  58  17 #sum:756

#marion's script:
3   4   5   6   8 
80 234 215 150  18 

# Identify year columns 
year_columns <- names(survey_dates_filtered)[grepl("^\\d{4}$", names(survey_dates_filtered))]


# Extract first and last survey years 
survey_long <- survey_dates_filtered %>%
  select(Region.OriginSiteID, Region.destSiteID, all_of(year_columns), destPlotID) %>%
  pivot_longer(
    cols = all_of(year_columns),
    names_to = "Year",
    values_to = "surveyed"
  ) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(surveyed))   # ne garder que les années avec données

# number of years surveyed, beg and end
survey_summary <- survey_long %>%
  group_by(Region.OriginSiteID, Region.destSiteID, destPlotID) %>%
  summarise(
    n_years_surveyed = n(),
    first_year = min(Year),
    last_year = max(Year),
    .groups = "drop"
  )

#keeping data only for those years surveyes at leasst 3 years
data_clean_filtered <- data_clean %>%
  semi_join(survey_summary, by = c("Region.OriginSiteID", "Region.destSiteID", "destPlotID"))

# ======================================================================
# STEP 4. Metadata tables
# ======================================================================

# Gradient-level metadata
gradient_metadata <- climdata %>%
  select(Country, PlotSize_m2, YearEstablished, gradient, destSiteID) %>%
  distinct(destSiteID, .keep_all = TRUE) %>%
  mutate(Measurement = if_else(gradient %in% unique(data_clean_filtered$Region), "Yes", NA))


unique(data_clean_filtered$Region) #only 20 sites surveyes at least 3 years
[1] "NO_Ulvhaugen"        "NO_Lavisdalen"       "NO_Gudmedalen"       "NO_Skjellingahaugen"
[5] "CH_Lavey"            "CH_Calanda"          "CH_Calanda2"         "US_Montana"         
[9] "US_Arizona"          "CN_Damxung"          "CN_Gongga"           "CN_Heibei"          
[13] "DE_Grainau"          "DE_Susalps"          "DE_TransAlps"        "FR_AlpeHuez"        
[17] "SE_Abisko"           "FR_Lautaret"         "IT_MatschMazia1"     "IT_MatschMazia2"  

# Plot-level metadata

gradient_metadata$Region.destSiteID <- interaction(gradient_metadata$gradient, gradient_metadata$destSiteID)

metadata <- data_clean_filtered %>%
  select(Region.OriginSiteID, Region.destSiteID, destPlotID, Elevation, Treatment) %>%
  distinct(destPlotID, .keep_all = TRUE) %>%
  left_join(survey_dates_filtered %>% select(destPlotID, n_years_surveyed), by = "destPlotID") %>%
  left_join(gradient_metadata, by = "Region.destSiteID") %>%
  select(Country, gradient, Region.destSiteID, YearEstablished, Measurement, PlotSize_m2,
         Region.OriginSiteID, Elevation, destPlotID, Treatment, n_years_surveyed)

# ======================================================================
# STEP 5. Absolute cover matrices
# ======================================================================

data_cover_abs <- data_clean_filtered %>%
  group_by(Region, Year, originSiteID, destSiteID, destPlotID, Treatment, SpeciesName) %>%
  summarise(Cover = sum(Cover, na.rm = TRUE), .groups = "drop") 

site_species_control_origin_abs <- data_cover_abs %>%
  filter(Treatment == "Control_Origin") %>%
  pivot_wider(names_from = SpeciesName, values_from = Cover) %>%
  left_join(metadata %>% distinct(destPlotID, Elevation), by = "destPlotID") %>%
  relocate(Elevation, .after = destPlotID)

site_species_control_destination_abs <- data_cover_abs %>%
  filter(Treatment == "Control_Destination") %>%
  pivot_wider(names_from = SpeciesName, values_from = Cover) %>%
  left_join(metadata %>% distinct(destPlotID, Elevation), by = "destPlotID") %>%
  relocate(Elevation, .after = destPlotID)

site_species_warm_abs <- data_cover_abs %>%
  filter(Treatment == "Warm") %>%
  pivot_wider(names_from = SpeciesName, values_from = Cover) %>%
  left_join(metadata %>% distinct(destPlotID, Elevation), by = "destPlotID") %>%
  relocate(Elevation, .after = destPlotID)


# ======================================================================
# STEP 6. Save outputs
# ======================================================================

write.csv(metadata, "metadata.csv", row.names = FALSE)
write.csv(site_species_control_origin_abs, "site_species_control_origin.csv", row.names = FALSE)
write.csv(site_species_control_destination_abs, "site_species_control_destination.csv", row.names = FALSE)
write.csv(site_species_warm_abs, "site_species_warm.csv", row.names = FALSE)

# Import raw matrices for control and warm treatments
site_species_control_origin <- read.csv("site_species_control_origin.csv", sep = ",")
site_species_control_destination <- read.csv("site_species_control_destination.csv", sep = ",")
site_species_warm <- read.csv("site_species_warm.csv", sep = ",")

# Convert wide → long format (species cover per site and year)
site_species_control_origin_long <- site_species_control_origin  %>%
  pivot_longer(
    cols = 8:ncol(.),
    names_to = "SpeciesName",
    values_to = "Cover"
  )

site_species_control_dest_long <- site_species_control_destination  %>%
  pivot_longer(
    cols = 8:ncol(.),
    names_to = "SpeciesName",
    values_to = "Cover"
  )

site_species_warm_long <- site_species_warm %>%
  pivot_longer(
    cols = 8:ncol(.),
    names_to = "SpeciesName",
    values_to = "Cover"
  )

# Summarize and reconstruct wide matrices (sum of covers per species)
site_species_control_origin <- site_species_control_origin_long %>%
  group_by(Region, Year, originSiteID, destSiteID, destPlotID, Treatment, SpeciesName) %>%
  summarise(Cover = sum(Cover, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = SpeciesName, values_from = Cover)

site_species_control_dest <- site_species_control_dest_long %>%
  group_by(Region, Year, originSiteID, destSiteID, destPlotID, Treatment, SpeciesName) %>%
  summarise(Cover = sum(Cover, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = SpeciesName, values_from = Cover)

site_species_warm <- site_species_warm_long %>%
  group_by(Region, Year, originSiteID, destSiteID, destPlotID, Treatment, SpeciesName) %>%
  summarise(Cover = sum(Cover, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = SpeciesName, values_from = Cover)

# ------------------------------------------------------------ #
# STEP 7. Data filtering and preparation
# ------------------------------------------------------------ #

site_species_control_origin_init <- site_species_control_origin %>%
  group_by(destPlotID) %>%
  filter(Year == min(Year)) %>%
  ungroup() %>%
  rename(Year_initial = Year)

site_species_control_dest_init <- site_species_control_dest %>%
  group_by(destPlotID) %>%
  filter(Year == min(Year)) %>%
  ungroup() %>%
  rename(Year_initial = Year)

site_species_warm_init <- site_species_warm %>%
  group_by(destPlotID) %>%
  filter(Year == min(Year)) %>%
  ungroup() %>%
  rename(Year_initial = Year)

site_species_control_origin_final <- site_species_control_origin %>%
  group_by(destPlotID) %>%
  filter(Year == max(Year)) %>%
  ungroup() %>%
  rename(Year_final = Year)

site_species_control_dest_final <- site_species_control_dest %>%
  group_by(destPlotID) %>%
  filter(Year == max(Year)) %>%
  ungroup() %>%
  rename(Year_final = Year)

site_species_warm_final <- site_species_warm %>%
  group_by(destPlotID) %>%
  filter(Year == max(Year)) %>%
  ungroup() %>%
  rename(Year_final = Year)

###
# Convert from wide to long format for initial and final datasets
site_species_control_origin_init <- site_species_control_origin_init %>%
  pivot_longer(cols = 7:ncol(.),
               names_to = "Species",
               values_to = "Cover") %>%
  distinct(Region, originSiteID, destSiteID, destPlotID, Species, Treatment, Cover, .keep_all = TRUE) %>%
  mutate(across(c(Region, originSiteID, destSiteID, destPlotID, Species, Treatment), as.character))

site_species_control_dest_init <- site_species_control_dest_init %>%
  pivot_longer(cols = 7:ncol(.),
               names_to = "Species",
               values_to = "Cover") %>%
  distinct(Region, originSiteID, destSiteID, destPlotID, Species, Treatment, Cover, .keep_all = TRUE) %>%
  mutate(across(c(Region, originSiteID, destSiteID, destPlotID, Species, Treatment), as.character))

site_species_warm_init <- site_species_warm_init %>%
  pivot_longer(cols = 7:ncol(.),
               names_to = "Species",
               values_to = "Cover") %>%
  distinct(Region, originSiteID, destSiteID, destPlotID, Species, Treatment, Cover, .keep_all = TRUE) %>%
  mutate(across(c(Region, originSiteID, destSiteID, destPlotID, Species, Treatment), as.character))

site_species_control_origin_final <- site_species_control_origin_final %>%
  pivot_longer(cols = 7:ncol(.),
               names_to = "Species",
               values_to = "Cover_final") %>%
  distinct(Region, originSiteID, destSiteID, destPlotID, Species, Treatment, Cover_final, .keep_all = TRUE) %>%
  mutate(across(c(Region, originSiteID, destSiteID, destPlotID, Species, Treatment), as.character))

site_species_control_dest_final <- site_species_control_dest_final %>%
  pivot_longer(cols = 7:ncol(.),
               names_to = "Species",
               values_to = "Cover_final") %>%
  distinct(Region, originSiteID, destSiteID, destPlotID, Species, Treatment, Cover_final, .keep_all = TRUE) %>%
  mutate(across(c(Region, originSiteID, destSiteID, destPlotID, Species, Treatment), as.character))

site_species_warm_final <- site_species_warm_final%>%
  pivot_longer(cols = 7:ncol(.),
               names_to = "Species",
               values_to = "Cover_final") %>%
  distinct(Region, originSiteID, destSiteID, destPlotID, Species, Treatment, Cover_final, .keep_all = TRUE) %>%
  mutate(across(c(Region, originSiteID, destSiteID, destPlotID, Species, Treatment), as.character))


# Merge initial and final datasets
site_species_control_origin <- bind_cols(
  site_species_control_origin_init,
  site_species_control_origin_final %>% dplyr::select(Year_final, Cover_final)
)

site_species_control_dest <- bind_cols(
  site_species_control_dest_init,
  site_species_control_dest_final %>% dplyr::select(Year_final, Cover_final)
)

site_species_warm <- bind_cols(
  site_species_warm_init,
  site_species_warm_final %>% dplyr::select(Year_final, Cover_final)
)

site_species <- bind_rows(site_species_control_origin, site_species_control_dest, site_species_warm)

site_species$delta_cover = log((site_species$Cover_final + 0.0002) / (site_species$Cover + 0.0002)) #with small value if species appears later (to enable log calculations)

# ------------------------------------------------------------ #
# STEP 8. Refine dynamic statuses : winners or losers
# ------------------------------------------------------------ #

# Combine 


write.csv(site_species, "site_species.csv", row.names = FALSE)

site_species <- read.csv("site_species.csv", sep = ",")


#info data
dim(site_species) #391660 11
length(unique(site_species$Species)) #802
colnames(site_species)
# [1] "Region"       "Year_initial" "originSiteID" "destSiteID"   "destPlotID"   "Treatment"   
# [7] "Species"      "Cover"        "Year_final"   "Cover_final"  "delta_cover" 
species_filtering <- site_species %>%
  group_by(Species) %>%
  summarise(
    n_obs = n(),
    n_treat = n_distinct(Treatment),
    warm_only = (n_treat == 1 & all(Treatment == "Warm")),
    .groups = "drop"
  )

table(
  single_obs = species_filtering$n_obs == 1,
  warm_only  = species_filtering$warm_only
)
# warm_only
# single_obs FALSE TRUE
# FALSE   738   64

#prepare for linera model
site_species <- site_species %>%
  mutate(
    Treatment = factor(Treatment),
    Species = factor(Species),
    destPlotID = factor(destPlotID),
    Year_final = as.numeric(Year_final)
  )

site_species <- site_species %>%
  mutate(
    Treatment = factor(
      Treatment,
      levels = c("Control_Origin", "Control_Destination", "Warm")
    )
  )

#site_species1 <- site_species %>% 
#  filter (Species %in% c("Achillea_millefolium", "Stellaria_media", "Gentiana_nivalis", "Hansenia_weberbaueriana", "Potentilla_nivea"))

mod_summaries <- list()

for (i in unique(site_species$Species)) {
  
  mod_data <- site_species %>%
    filter(Species == i) %>%
    select(
      delta_cover,
      Treatment,
      Year_final,
      Region,
      destSiteID,
      destPlotID
    ) 
  
  # Conditions of removing a species
  # Species only observed once (botanical error) are skipped, goes to the next one
  if (nrow(mod_data) == 1) next
  
  # Species that occur only in WARM but are absent from both controls are skipped
  if (n_distinct(mod_data$Treatment) == 1 &&
      unique(mod_data$Treatment) == "Warm") next
  
  #Minimal condition for a treatment effect - otherwise code does not work
  if (n_distinct(mod_data$Treatment) < 2) next
  
  
  mod_summaries[[i]] <- summary(
    lm(
      delta_cover ~ Treatment, #effet du réchauffement sur le delta cover (Bektas PhD p 79)
      #(1 | destPlotID) probleme de convergence...
      data = mod_data
    )
  )
}

#extract interaction effect of Treatment for each species (Bektas PhD p 79)
interaction_results <- data.frame()

for (sp in names(mod_summaries)) {
  
  coefs <- mod_summaries[[sp]]$coefficients
  
  if (!"TreatmentWarm" %in% rownames(coefs)) next
  
  interaction_results <- rbind(
    interaction_results,
    data.frame(
      Species = sp,
      estimate = coefs["TreatmentWarm", "Estimate"],
      p.value  = coefs["TreatmentWarm", "Pr(>|t|)"]
    )
  )
}




###essai avec random factor:
# mod_summaries_alea <- list()
# 
# for (i in unique(site_species$Species)) {
#   
#   mod_data <- site_species %>%
#     filter(Species == i) %>%
#     select(
#       delta_cover,
#       Treatment,
#       Year_final,
#       Region,
#       destSiteID,
#       destPlotID)
#   if (nrow(mod_data) == 1) next
#   if (n_distinct(mod_data$Treatment) == 1 &&
#       unique(mod_data$Treatment) == "Warm") next
#   if (n_distinct(mod_data$Treatment) < 2) next
#   
#   mod_summaries_alea[[i]] <- summary(
#     lmer(
#       delta_cover ~ Treatment + (1 | destPlotID), data = mod_data))}


####extract emmeans
library(emmeans)

mod_fits <- list()

for (i in unique(site_species$Species)) {
  
  mod_data <- site_species %>%
    filter(Species == i) %>%
    select(delta_cover, Treatment, destPlotID)
  
  # Exclusions
  if (nrow(mod_data) == 1) next
  
  if (n_distinct(mod_data$Treatment) == 1 &&
      unique(mod_data$Treatment) == "Warm") next
  
  if (n_distinct(mod_data$Treatment) < 2) next
  
  # Modèle
  mod_fits[[i]] <- lm(
    delta_cover ~ Treatment,
    data = mod_data
  )
}

emm_results <- data.frame()

for (sp in names(mod_fits)) {
  
  emm <- emmeans(mod_fits[[sp]], ~ Treatment)
  
  emm_df <- as.data.frame(emm)
  emm_df$Species <- sp
  
  emm_results <- rbind(emm_results, emm_df)
}

library(emmeans)


###contrast help:https://aosmith.rbind.io/2019/04/15/custom-contrasts-emmeans/
site_species1 <- site_species %>% filter(Species == "Achillea_millefolium")

mod1 <- lm(delta_cover ~ Treatment, data= site_species1)
summary(mod1)

emmeans(mod1, ~Treatment)
pairs(em1)
eff_size(em1, sigma= sigma(mod1), edf=686)

Control_Origin <- c(1,0,0)
Control_Dest <- c(0,1,0)
Warm <- c(0,0,1)

result1<- contrast(em1, method = list("Control_Origin - Warm" = Control_Origin - Warm,
                            "Control_Destination - Warm" = Control_Dest - Warm,
                            "Control_Origin - Control_Destination" = Control_Origin -Control_Dest),
         adjust = "mvt") #%>%#get multivariate t adjustment 
#confint() #get confint

result1<-as_tibble(result1) 

#then clasify into winner if estimate Control-Warm >0 & p.value is significant
#then classify into loser if estiamate control-warm<0 &p.value is significant


##all species

site_species <- site_species %>%
  mutate(Treatment = factor(Treatment, 
                            levels = c("Control_Origin", "Control_Destination", "Warm")))

contrast_results <- tibble()

for (sp in unique(site_species$Species)) {
  
  mod_data <- site_species %>% filter(Species == sp)
  
  # Skip species with too few observations
  if (nrow(mod_data) <= 1) next
  if (n_distinct(mod_data$Treatment) < 2) next
  if (n_distinct(mod_data$Treatment) == 1 && unique(mod_data$Treatment) == "Warm") next
  
  # Fit linear model
  mod_fit <- lm(delta_cover ~ Treatment, data = mod_data)
  
  # emmeans
  emm <- emmeans(mod_fit, ~ Treatment)
  lvls <- levels(factor(mod_data$Treatment))
  
  # Initialize empty list for contrasts
  cont_list <- list()
  
  # Dynamically define contrasts only for levels present
  if(all(c("Control_Origin","Warm") %in% lvls)){
    CO <- as.numeric(lvls == "Control_Origin")
    Warm <- as.numeric(lvls == "Warm")
    cont_list[["CO_vs_Warm"]] <- CO - Warm
  }
  
  if(all(c("Control_Destination","Warm") %in% lvls)){
    CD <- as.numeric(lvls == "Control_Destination")
    Warm <- as.numeric(lvls == "Warm")
    cont_list[["CD_vs_Warm"]] <- CD - Warm
  }
  
  if(all(c("Control_Origin","Control_Destination") %in% lvls)){
    CO <- as.numeric(lvls == "Control_Origin")
    CD <- as.numeric(lvls == "Control_Destination")
    cont_list[["CO_vs_CD"]] <- CO - CD
  }
  
  if(length(cont_list) == 0) next
  
  # compute contrasts
  contr <- contrast(emm, method = cont_list, adjust = "mvt")
  
  contr_df <- as_tibble(contr) %>%
    mutate(Species = sp)
  
  contrast_results <- bind_rows(contrast_results, contr_df)
}

# loser non-significant          winner 
# 8             421              48 
# 
# length(unique(site_species$Species))
# [1] 802


status <- contrast_results  %>%
  group_by(Species) %>%
  summarise(
    status = case_when(
      any(contrast == "CO_vs_Warm" & p.value < 0.05 & estimate > 0) ~ "winner",
      any(contrast == "CO_vs_Warm" & p.value < 0.05 & estimate < 0) ~ "loser",
      TRUE ~ "non-significant"
    ),
    Integration = case_when(
      status == "winner" & any(contrast == "CD_vs_Warm" & p.value < 0.05 & estimate > 0) ~ "doing better than locals",
      status == "winner" & any(contrast == "CD_vs_Warm" & p.value < 0.05 & estimate < 0) ~ "lag",
      status == "loser"  & any(contrast == "CD_vs_Warm" & p.value < 0.05 & estimate > 0) ~ "doing better than locals",
      status == "loser"  & any(contrast == "CD_vs_Warm" & p.value < 0.05 & estimate < 0) ~ "lag",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  )

# Quick check
table(status$status)
table(status$status,status$Integration, useNA = "ifany")

write.csv(status, "winner_losers.csv")

# ------------------------------------------------------------ #
# STEP 8. Refine dynamic statuses : winners or losers
# ------------------------------------------------------------ #

winner_species <- status %>%filter(status == "winner") %>% select(Species)
loser_species <- status %>%filter(status == "loser") %>% select(Species)

setdiff(loser_species,winner_species)
setdiff(winner_species, loser_species)
#le genre Potentilla is loser but some Potentilla species in winner

# Load imputed mean trait dataset (TRY database)
Sp_trait_TRY <- read.csv("imputed_traits_mean.csv", sep = ",", dec = ".")
Sp_trait_TRY$Species <- gsub(" ", "_", Sp_trait_TRY$Species_name)

# Log-transform continuous traits to normalize distributions
Sp_trait_TRY_trans <- Sp_trait_TRY
Sp_trait_TRY_trans[,5] <- log10(Sp_trait_TRY_trans[,5])    # Leaf area
Sp_trait_TRY_trans[,8] <- log10(Sp_trait_TRY_trans[,8])    # Maximum height
Sp_trait_TRY_trans[,9] <- ifelse(Sp_trait_TRY_trans[,9] == 0, 0.0001, Sp_trait_TRY_trans[,7])  # Avoid log(0) for seed mass
Sp_trait_TRY_trans[,9] <- log10(Sp_trait_TRY_trans[,9])

# Center and scale traits
Sp_trait_TRY_trans[,c(5:11)] <- scale(Sp_trait_TRY_trans[,c(5:11)])

#add status

species_traits_statut <- left_join(Sp_trait_TRY_trans, status) %>%drop_na(status)


# Plot trait differences across groups
# (SSD, plant height, leaf area)
library(ggpubr)

# Define comparison pairs for statistical tests
comparaisons <- list(
  # Within-treatment comparisons
  c("loser", "non-significant"),
  c("loser", "winner"),
  c("winner", "non-significant"))

P1 <- ggplot(species_traits_statut, aes(x = status, y = trait_SSD_mgmm.3, fill = status)) +
  geom_violin(trim = FALSE, alpha = 0.8, color = NA) + 
  geom_boxplot(width = 0.15, position = position_dodge(width = 0.9), alpha = 0.7, outlier.shape = NA) + 
  stat_compare_means(
    comparisons = comparaisons,
    method = "wilcox.test",
    label = "p.signif",
    hide.ns = TRUE,
    geom = "text"
  ) +
  labs(x = "", y = "SSD") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

P2 <- ggplot(species_traits_statut, aes(x = status, y = trait_Max_height_m, fill = status)) +
  geom_violin(trim = FALSE, alpha = 0.8, color = NA) + 
  geom_boxplot(width = 0.15, position = position_dodge(width = 0.9), alpha = 0.7, outlier.shape = NA) + 
  scale_y_log10() +
  stat_compare_means(
    comparisons = comparaisons,
    method = "wilcox.test",
    label = "p.signif",
    hide.ns = TRUE,
    geom = "text"
  ) +
  labs(x = "", y = "Plant height") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

P3 <- ggplot(species_traits_statut, aes(x = status, y = trait_Leaf_area_mm2, fill = status)) +
  geom_violin(trim = FALSE, alpha = 0.8, color = NA) + 
  geom_boxplot(width = 0.15, position = position_dodge(width = 0.9), alpha = 0.7, outlier.shape = NA) + 
  scale_y_log10() +
  stat_compare_means(
    comparisons = comparaisons,
    method = "wilcox.test",
    label = "p.signif",
    hide.ns = TRUE,
    geom = "text"
  ) +
  labs(x = "", y = "Leaf area") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------------------------------------------------------------ #
# STEP 6. PCA on trait data
# ------------------------------------------------------------ #
library(FactoMineR)
library(factoextra)

colnames(species_traits_statut)
# Run PCA on standardized traits
pca_result <- PCA(species_traits_statut[,5:11], scale.unit = TRUE, graph = TRUE)

# Variance explained by each axis
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100),
         main = "Variance explained per axis")

# Extract species coordinates in PCA space
eig_values <- pca_result$eig
significant_axes <- 1:ncol(pca_result$ind$coord)
coords <- pca_result$ind$coord[, significant_axes, drop = FALSE]
coord <- data.frame(Species = species_traits_statut$Species, coords)

# Visualize species and trait loadings

fviz_pca_ind(pca_result, label="none", habillage = factor(species_traits_statut$status),
             pointshape = 16, 
             pointsize =2,
             palette = c("red", #loser
                         "gray",
                         "green" ))#winner 
 

# Compute distance matrix among species in PCA space
coord2 <- coord
rownames(coord2) <- coord2$Species
coord2$Species <- NULL
species_names <- rownames(coord2)

library(funrar)
Distance <- compute_dist_matrix(coord2, metric = "euclidean", center = FALSE, scale = FALSE)
rownames(Distance) <- colnames(Distance) <- species_names

# ------------------------------------------------------------ #
# STEP 7. Compute species distinctiveness
# ------------------------------------------------------------ #
