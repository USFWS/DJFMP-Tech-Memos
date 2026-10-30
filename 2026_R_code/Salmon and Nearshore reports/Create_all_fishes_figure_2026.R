## Original author: Christian Gredzens, 2023-07-19
## Minor updates: L Mitchell, 2026-01-27

################################################################################
#STEP 2: Load Required Packages, Clear Environment, Assign Data Ranges 
################################################################################

# Detach any previously loaded packages
lapply(names(sessionInfo()$otherPkgs), function(pkgs) detach(paste0('package:', pkgs), character.only = T, unload = T, force = T))

#-------------------------------------------------------------------------------
# Packages
#-------------------------------------------------------------------------------
library(odbc) # database functions
library(tidyverse) # tidy core functions
library(plotrix) # standard error calculations
library(gridExtra) # paneled graphics functions
library(grid) # paneled graphics functions
library(egg) # figure size functions

#-------------------------------------------------------------------------------

options(scipen = 999) # Get rid of scientific notation

################################################################################
# Setting dates for sample queries: Change to current date range of interest
################################################################################

date_start <- "1995-12-31"
date_end <- "2026-07-31" # Change me
calendaryr <- 2026 # Change me

# List of species to include
# Based off list of 36 species (Table 2) in Stompe et al. (2020) Comparing and integrating fish surveys in the San Francisco Estuary: why diverse long-term monitoring programs are important. SFEWS.
# Note: no sturgeon [GST, WST] in dataset as of 2022, removed from species of interest list
# Note: last two species on list are bay species chosen by LFWO for inclusion (LPS and SAS placeholders)
# Note: you must have an even number of species (that is also divisible by 4 [i.e, 36, 40, 44]), otherwise the code for the figures will break
Fishies <- c("AMS", "BGS", "BKB", "BKS", "C", "CHC", "CHN", "DSM", "GF", "GSF", "HCH", 
             "LFS", "LMB", "LP", "MQF", "MSS", "PL", "PRS", "PSS", "RBT", "RES", "SAPM",
             "SASU", "SCB", "SHM", "SPB", "SPLT", "STB", "TFS", "TP", "TSS", "W", "WHC", 
             "YFG", "NOFISH", "LPS", "SAS")

################################################################################

set.seed(123)


#Import Data from Denver Database
################################################################################
# Step 3: Import Data From Denver Database
################################################################################

# Connect to Denver Database
# LY: removed server and database connection username/passwords.  
# Will replace with EDI published datasets shortly


# Pull in sample data tables from database
Sample_Table <- gsub("[\r\n]"," ", sprintf("SELECT * FROM Sample WHERE SampleDate BETWEEN '%s' AND '%s';", 
                                           date_start, date_end))

Samples <- DBI::dbGetQuery(conn = DJFMP_Database_Con, 
                           statement = Sample_Table)

# Seine regions
Station_Code_Table <- "SELECT * FROM ref_station_code;"
Stations <- DBI::dbGetQuery(conn = DJFMP_Database_Con, 
                            statement = Station_Code_Table)

# Import catch data
Catch_Table <- "SELECT * FROM Catch;"
Catch <- DBI::dbGetQuery(conn = DJFMP_Database_Con, 
                         statement = Catch_Table)

# Import common names of species
Organism_Code_Table <- "SELECT * FROM ref_organism_code;"
Organisms <- DBI::dbGetQuery(conn = DJFMP_Database_Con,
                             statement = Organism_Code_Table)

Organisms <- Organisms %>% rename("OrganismCode" = organism_code)

# Disconnect from Denver Database
dbDisconnect(DJFMP_Database_Con)

# Remove database and unneeded vectors to clean up environment
rm(list = setdiff(ls(), c("Catch", "Organisms", "Samples", "Stations", "Fishies", "calendaryr")))


#Create Sample Data
################################################################################
# Step 4: Merge and Filter Datasets
################################################################################

#-------------------------------------------------------------------------------
# Effort
#-------------------------------------------------------------------------------

Stations <- Stations %>%
  rename("StationCode" = station_code)

Effort_combined <- left_join(Samples, Stations, by="StationCode")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Diagnostic: QC row Counts before and after join of sample and station data
as.numeric(count(Samples))
as.numeric(count(Effort_combined)) # Looks good (271038/271038)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Effort <- Effort_combined %>%
# Filter to only seine sites and remove selected stations (e.g., Liberty Island, inconsistent sampling, etc.)
  filter(MethodCode == "SEIN", 
         !(StationCode %in% c("CR005S", "SR060AH", "SR144AH", "OR001X", "PR001W", 
                              "VC002N", "TC002E", "SP000E", "SJ063E", "SJ065W", "SJ070N",
                              "SJ072E", "SJ075E", "SJ087W", "SR015E", "SR163W", "SR184E",
                              "SR119E", "SJ074A", "SJ076W", "SJ079E", "SR055AH", "LI001E", 
                              "LI001W", "LI002E", "LI002W", "LI003E", "LI003W", "LI004E",
                              "LI004W", "LI005E", "LI005W", "LI006E", "LI006W", "LI007E", 
                              "LI007W", "LI008E", "LI008W", "LI009E", "LI009W", "LI010E", 
                              "LI010W", "LI011E", "LI011W"))) %>%
  # Calculate seine volume and create year/month columns
  mutate(SeineVolume = (SeineLength * SeineWidth * SeineDepth)/2, 
         Year = lubridate::year(SampleDate), 
         Month = lubridate::month(SampleDate)) %>%
  # Remove failed samples, seines not meeting minimum dimensions, and samples with no effort (volume)
  filter(GearConditionCode %in% c("1","2"), 
         !is.na(SeineVolume), 
         SeineLength >= 2 & SeineLength <= 15 & 
					SeineWidth >= 3 & SeineWidth <= 15 & 
          SeineDepth >= 0.2 & SeineDepth <= 1.5) %>%
  rename("Location" = description) %>%
  # Add Region Name column
  mutate(RegionCodeName = case_when(RegionCode %in% c("1")~"SAC River",
                                    RegionCode %in% c("2")~"North Delta",
                                    RegionCode %in% c("3")~"Central Delta",
                                    RegionCode %in% c("4")~"South Delta",
                                    RegionCode %in% c("5")~"SJ River",
                                    RegionCode %in% c("6")~"San Francisco Bay")) %>%
  # Remove seine sites outside of regions of interest
  filter(!is.na(RegionCodeName)) %>%
  # Parse down number of data columns to only those necessary
  select(SampleID, Location, RegionCodeName, StationCode, SampleDate, SampleTime, 
         GearConditionCode, SeineVolume, Month, Year) %>%
  # Change datatype of specified columns  
  mutate_at(c("Year", "Month", "StationCode", "GearConditionCode", "RegionCodeName"), as.factor) %>%
  # unique removes duplicate rows   
  unique() %>%
  # Remove any unused factor levels
  droplevels()

#-------------------------------------------------------------------------------
# Catch
#-------------------------------------------------------------------------------

Catch_Final <- Catch %>%
  # Combine catch data with organism information
  left_join(Organisms, by = "OrganismCode") %>%
  filter(OrganismCode != "NOFISH") %>%
  # Remove SampleIDs not shared between catch and sample datasets
  filter(SampleID %in% Effort$SampleID) %>%
  select(SampleID, OrganismCode, description, ForkLength, Count, NonNative, Phylum, Family, Genus, Species) %>%
  rename("CommonName" = description, "Count" = Count) %>%
  group_by(SampleID, OrganismCode, CommonName, ForkLength, NonNative, Phylum, Family, Genus, Species) %>%
  summarise(Count = sum(Count)) %>%
  as.data.frame()

# Merge Effort and Catch
Catch_Data <- left_join(Effort,Catch_Final, by = "SampleID")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Diagnostic: QC catch Counts before and after join
as.numeric(sum(Catch_Final$Count))
as.numeric(sum(Catch_Data$Count, NA, na.rm = T)) # Looks good (3602841/3602841)

# Diagnostic: Check if count of sampling events still matches earlier count (46927)
sampling_effort <- Catch_Data %>%
  select(SampleID, StationCode, SampleDate, Year) %>%
  group_by(SampleID) %>%
  unique()

station_effort <- Effort %>%
  select(SampleID, StationCode, SampleDate, Year) %>%
  group_by(SampleID) %>%
  unique()

summary(station_effort)
# 46927 total successful sampling events (the same, no losses)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Catch_Data <- Catch_Data %>%
  # Convert all catch not in species list to NOFISH
  mutate(OrganismCode = ifelse(is.na(OrganismCode), "NOFISH", OrganismCode)) %>%
  mutate(CommonName = ifelse(is.na(CommonName), "No catch", CommonName)) %>%
  mutate(OrganismCode = ifelse(!OrganismCode %in% Fishies, "Catch_Other", OrganismCode)) %>%
  # Convert samples under minimum lengths for later removal (keeps plus count fish for species of interest and stations with no catch)
  mutate(OrganismCode = ifelse((OrganismCode == "SPLT" | OrganismCode == "MQF" | OrganismCode == "TSS" | OrganismCode == "SASU" | OrganismCode == "RFK") & (between(ForkLength, 1, 19)) & (!is.na(ForkLength) | (OrganismCode != "SPLT" | OrganismCode != "MQF" | OrganismCode != "TSS" | OrganismCode != "SASU" | OrganismCode != "RFK" | OrganismCode != "NOFISH") & (between(ForkLength, 1, 24))), "Catch_Other", OrganismCode))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Diagnostic: Check if invertebrates were caught with fish or not
Catch_Other <- Catch_Data %>%
  select(SampleID, OrganismCode) %>%
  group_by(SampleID, OrganismCode) %>%
  unique() %>%
  group_by(SampleID) %>%
  filter(any(OrganismCode == "Catch_Other")) %>%
  filter(n_distinct(OrganismCode) == 1) %>%
  ungroup() %>%
  mutate(OrganismCode = ifelse(OrganismCode == "Catch_Other", "NOFISH", OrganismCode)) %>%
  add_column(CommonName = "No catch") %>%
  add_column(Count = 0)

summary(Catch_Other)
# 2731 stations only caught other species (need to keep in dataset, but change to NOFISH)
# 2730 stations when not using sample time to assign sampling, one site sampled twice in one day
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Catch_Data <- Catch_Data %>%
  # Join and replace original data with amended data from diagnostic for species of non-interest
  left_join(Catch_Other, by = c("SampleID")) %>%
  mutate(OrganismCode = coalesce(OrganismCode.y, OrganismCode.x)) %>%
  mutate(CommonName = coalesce(CommonName.y, CommonName.x)) %>%
  mutate(Count = coalesce(Count.y, Count.x)) %>%
  # Remove redundant data columns created during join function
  select(-c(CommonName.x, CommonName.y, OrganismCode.x, OrganismCode.y, Count.x, Count.y, ForkLength)) %>%
  filter(!OrganismCode == "Catch_Other") %>%
  mutate(Phylum = ifelse(OrganismCode == "NOFISH", NA, Phylum)) %>%
  mutate(Family = ifelse(OrganismCode == "NOFISH", NA, Family)) %>%
  mutate(Genus = ifelse(OrganismCode == "NOFISH", NA, Genus)) %>%
  mutate(Species = ifelse(OrganismCode == "NOFISH", NA, Species)) %>%
  mutate(NonNative = ifelse(OrganismCode == "NOFISH", NA, NonNative)) %>%
  # Add 0 counts for no catch
  mutate(Count = ifelse(OrganismCode == "NOFISH", 0, Count)) %>%
  # Remove unused factor levels  
  droplevels() %>%
  # Combine counts for each species at each sampling event
  group_by(SampleID, OrganismCode) %>%
  mutate(Count = sum(Count)) %>%
  ungroup() %>%
  as.data.frame() %>%
  unique() %>%
  # Add CPUE for each species at each station (fish/10000 m^3)
  mutate(CPUE = Count/SeineVolume * 10000)

# Create stations and convert to matrix, create 0 catch at stations where species weren't caught
Catch_Matrix <- Catch_Data %>%
  select(c(SampleID, Year, OrganismCode, CPUE)) %>%
  unique() %>%
  # Create species X site matrix
  spread(OrganismCode, CPUE, fill = 0) %>%
  # NOFISH column no longer needed, 0 catch for each species added to each species column
  select(!c(NOFISH))

# Calculate catch statistics
Catch_CPUE <- Catch_Matrix %>%
  # Gather all species data to create a single column for species and another for CPUE
  gather(OrganismCode, CPUE, 3:ncol(Catch_Matrix)) %>%
  mutate_at("OrganismCode", as.factor) %>%
  # Calculate overall statistics by species
  group_by(OrganismCode) %>%
  mutate(CPUE.mean.sp = mean(CPUE)) %>%
  mutate(CPUE.SE.sp = std.error(CPUE)) %>%
  ungroup() %>%
  # Calculate annual statistics by species
  group_by(Year, OrganismCode) %>%
  mutate(CPUE.mean.yr = mean(CPUE)) %>%
  mutate(CPUE.SE.yr = std.error(CPUE)) %>%
  ungroup() %>%
  as.data.frame()
  
# Create Raw Datasets
RawCPUEData <- Effort %>%
  left_join(Catch_CPUE, by = "SampleID")%>%
  select(!Year.y) %>%
  mutate_at("Year.x", as.character) %>%
  mutate_at("Year.x", as.numeric)

# Split into more manageable sizes so they can be opened in Excel
RawCPUEData2010 <- subset(RawCPUEData, Year.x <= 2010)
RawCPUEDataAfter2010 <- subset(RawCPUEData, Year.x > 2010)  


# Export Raw Dataset to .CSV files
# write.csv(RawCPUEData2010, file=paste("Seine Raw CPUE 1995-2010 Nearshore Fishes Annual Report.csv"), row.names = F)
# write.csv(RawCPUEDataAfter2010, file=paste("Seine Raw CPUE 2011-",calendaryr,"Nearshore Fishes Annual Report.csv"), row.names = F)
# write.csv(Catch_Data, file=paste("Seine Raw Catch",calendaryr,"Nearshore Fishes Annual Report.csv"), row.names = F)

# Create annual dataset
Catch_CPUE_Annual <- Catch_CPUE %>%
  # Filter dataset down to annual statistics for each species of interest
  select(c(Year, OrganismCode, CPUE.mean.yr, CPUE.SE.yr)) %>%
  unique()

# Create overall dataset
Catch_CPUE_Overall <- Catch_CPUE %>%
  # Filter dataset down to overall statistics for each species of interest
  select(c(OrganismCode, CPUE.mean.sp, CPUE.SE.sp)) %>%
  unique()

# Remove unneeded vectors to clean up environment
# rm(list = setdiff(ls(), c("Catch_CPUE_Annual", "Catch_CPUE_Overall", "Catch_CPUE", 
#                           "Catch_Matrix", "Fishies", "calendaryr")))


# Figure Creation and Export
################################################################################
# Step 5: Create Figures
################################################################################

# Split datasets (annual and overall) into 4 datasets for figure formatting
# Final plots will be two one-page paneled figures with 1/2 of species (split into two columns) on each plot
# Annual CPUE
Catch_CPUE_Annual.A <- Catch_CPUE_Annual %>%
  slice(1:(nrow(Catch_CPUE_Annual)/4)) %>%
  droplevels()

Catch_CPUE_Annual.B <- Catch_CPUE_Annual %>%
  slice(((nrow(Catch_CPUE_Annual)/4) + 1):(nrow(Catch_CPUE_Annual)/2)) %>%
  droplevels()

Catch_CPUE_Annual.C <- Catch_CPUE_Annual %>%
  slice(((nrow(Catch_CPUE_Annual)/2) + 1):(nrow(Catch_CPUE_Annual) * 0.75)) %>%
  droplevels()

Catch_CPUE_Annual.D <- Catch_CPUE_Annual %>%
  slice(((nrow(Catch_CPUE_Annual) * 0.75) + 1):nrow(Catch_CPUE_Annual)) %>%
  droplevels()

# Overall CPUE
Catch_CPUE_Overall.A <- Catch_CPUE_Overall %>%
  slice(1:(nrow(Catch_CPUE_Overall)/4)) %>%
  droplevels()

Catch_CPUE_Overall.B <- Catch_CPUE_Overall %>%
  slice(((nrow(Catch_CPUE_Overall)/4) + 1):(nrow(Catch_CPUE_Overall)/2)) %>%
  droplevels()

Catch_CPUE_Overall.C <- Catch_CPUE_Overall %>%
  slice(((nrow(Catch_CPUE_Overall)/2) + 1):(nrow(Catch_CPUE_Overall) * 0.75)) %>%
  droplevels()

Catch_CPUE_Overall.D <- Catch_CPUE_Overall %>%
  slice(((nrow(Catch_CPUE_Overall) * 0.75) + 1):nrow(Catch_CPUE_Overall)) %>%
  droplevels()


#===============================================================================
# Plot.A 
#===============================================================================
P.A <- ggplot() +
  # Create bar-plot based on annual mean CPUE value in CPUE.mean.yr column
  geom_bar(Catch_CPUE_Annual.A, mapping = aes(Year, CPUE.mean.yr), stat = "identity", fill = "black") +
  # Format x axis
  scale_x_discrete(expand = expansion(mult = c(0.025, 0.025)), breaks = seq(1995, calendaryr, 5)) +
  # Format y axis
  #scale_y_continuous(expand = expansion(mult = c(0,0.15)), breaks = scales::trans_breaks(identity, identity, n = 3)) +
  # Replacement code for setting y-axis breaks
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = scales::breaks_extended(n = 3)) +
  # Add SE error bars
  geom_errorbar(Catch_CPUE_Annual.A, mapping = aes(Year, ymin = CPUE.mean.yr - CPUE.SE.yr, ymax = CPUE.mean.yr + CPUE.SE.yr), width = 0.5) +
  # Add SE to overall mean for each plot
  geom_rect(merge(Catch_CPUE_Overall.A, Catch_CPUE_Annual.A), mapping = aes(xmin = -Inf, xmax = Inf, ymin = CPUE.mean.sp - CPUE.SE.sp, ymax = CPUE.mean.sp + CPUE.SE.sp, group = OrganismCode), alpha = 0.03, fill = "gray") +
  # Add overal mean to each plot
  geom_hline(Catch_CPUE_Overall.A, mapping = aes(yintercept = CPUE.mean.sp), color = "gray45", linewidth = 0.75, linetype = "dashed") +
  # Create paneled figure
  facet_grid(rows = vars(OrganismCode), scales = "free_y") +
  # Format theme (e.g., background, axis lines, text, etc.)
  theme_classic(base_size = 12) + # sets overall font size, makes background solid white
  theme(axis.text = element_text(color = "black", family = "sans", size = 12)) + # sets axis text color, font type
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + # rotate x-axis labels
  theme(axis.line.y = element_line(color = "black", linewidth = .75)) +
  theme(axis.line.x = element_blank()) +
  theme(panel.spacing.y = unit(0.85, "lines")) + # add space between panels
  # Remove axis labels
  labs(x = NULL, y = NULL) +
  # Add x axis lines to all panels
  annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, linewidth = 0.75, color = "black")


#===============================================================================
# Plot.B 
#===============================================================================
P.B <- ggplot() +
  geom_bar(Catch_CPUE_Annual.B, mapping = aes(Year, CPUE.mean.yr), stat = "identity", fill = "black") +
  scale_x_discrete(expand = expansion(mult = c(0.025, 0.025)), breaks = seq(1995, calendaryr, 5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = scales::breaks_extended(n = 3)) +
  geom_errorbar(Catch_CPUE_Annual.B, mapping = aes(Year, ymin = CPUE.mean.yr - CPUE.SE.yr, ymax = CPUE.mean.yr + CPUE.SE.yr), width = 0.5) +
  geom_rect(merge(Catch_CPUE_Overall.B, Catch_CPUE_Annual.B), mapping = aes(xmin = -Inf, xmax = Inf, ymin = CPUE.mean.sp - CPUE.SE.sp, ymax = CPUE.mean.sp + CPUE.SE.sp, group = OrganismCode), alpha = 0.03, fill = "gray") +
  geom_hline(Catch_CPUE_Overall.B, mapping = aes(yintercept = CPUE.mean.sp), color = "gray45", linewidth = 0.75, linetype = "dashed") +
  facet_grid(rows = vars(OrganismCode), scales = "free_y") +
  theme_classic(base_size = 12) +
  theme(axis.text = element_text(color = "black", family = "sans", size = 12)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(axis.line.y = element_line(color = "black", linewidth = .75)) +
  theme(axis.line.x = element_blank()) +
  theme(panel.spacing.y = unit(0.85, "lines")) +
  labs(x = NULL, y = NULL) +
  annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, linewidth = 0.75, color = "black")


#===============================================================================
# Plot.C
#===============================================================================
P.C <- ggplot() +
  geom_bar(Catch_CPUE_Annual.C, mapping = aes(Year, CPUE.mean.yr), stat = "identity", fill = "black") +
  scale_x_discrete(expand = expansion(mult = c(0.025, 0.025)), breaks = seq(1995, calendaryr, 5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = scales::breaks_extended(n = 3)) +
  geom_errorbar(Catch_CPUE_Annual.C, mapping = aes(Year, ymin = CPUE.mean.yr - CPUE.SE.yr, ymax = CPUE.mean.yr + CPUE.SE.yr), width = 0.5) +
  geom_rect(merge(Catch_CPUE_Overall.C, Catch_CPUE_Annual.C), mapping = aes(xmin = -Inf, xmax = Inf, ymin = CPUE.mean.sp - CPUE.SE.sp, ymax = CPUE.mean.sp + CPUE.SE.sp, group = OrganismCode), alpha = 0.03, fill = "gray") +
  geom_hline(Catch_CPUE_Overall.C, mapping = aes(yintercept = CPUE.mean.sp), color = "gray45", linewidth = 0.75, linetype = "dashed") +
  facet_grid(rows = vars(OrganismCode), scales = "free_y") +
  theme_classic(base_size = 12) +
  theme(axis.text = element_text(color = "black", family = "sans", size = 12)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(axis.line.y = element_line(color = "black", linewidth = .75)) +
  theme(axis.line.x = element_blank()) +
  theme(panel.spacing.y = unit(0.85, "lines")) +
  labs(x = NULL, y = NULL) +
  annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, linewidth = 0.75, color = "black")


#===============================================================================
# Plot.D
#===============================================================================
P.D <- ggplot() +
  geom_bar(Catch_CPUE_Annual.D, mapping = aes(Year, CPUE.mean.yr), stat = "identity", fill = "black") +
  scale_x_discrete(expand = expansion(mult = c(0.025, 0.025)), breaks = seq(1995, calendaryr, 5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = scales::breaks_extended(n = 3)) +
  geom_errorbar(Catch_CPUE_Annual.D, mapping = aes(Year, ymin = CPUE.mean.yr - CPUE.SE.yr, ymax = CPUE.mean.yr + CPUE.SE.yr), width = 0.5) +
  geom_rect(merge(Catch_CPUE_Overall.D, Catch_CPUE_Annual.D), mapping = aes(xmin = -Inf, xmax = Inf, ymin = CPUE.mean.sp - CPUE.SE.sp, ymax = CPUE.mean.sp + CPUE.SE.sp, group = OrganismCode), alpha = 0.03, fill = "gray") +
  geom_hline(Catch_CPUE_Overall.D, mapping = aes(yintercept = CPUE.mean.sp), color = "gray45", linewidth = 0.75, linetype = "dashed") +
  facet_grid(rows = vars(OrganismCode), scales = "free_y") +
  theme_classic(base_size = 12) +
  theme(axis.text = element_text(color = "black", family = "sans", size = 12)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(axis.line.y = element_line(color = "black", linewidth = .75)) +
  theme(axis.line.x = element_blank()) +
  theme(panel.spacing.y = unit(0.85, "lines")) +
  labs(x = NULL, y = NULL) +
  annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, linewidth = 0.75, color = "black")


# Set dimensions of each panel (width and height of each individual species plot in the paneled figure)
G.A <-  set_panel_size(P.A, width = unit(2.20, "in"), height = unit(0.74, "in"))
G.B <-  set_panel_size(P.B, width = unit(2.20, "in"), height = unit(0.74, "in"))
G.C <-  set_panel_size(P.C, width = unit(2.20, "in"), height = unit(0.74, "in"))
G.D <-  set_panel_size(P.D, width = unit(2.20, "in"), height = unit(0.74, "in"))


# Save final figures
# Figure 1
# ggsave(file.path("Figures",paste("Figure 1",calendaryr,"Nearshore Fishes Annual Report.png")),
#        grid.arrange(G.A, G.B, ncol=2,
#                     left=textGrob("     CPUE (fish" ~ "m"^-3* ~ "× 10"^4*")", rot=90,
#                                     gp=gpar(fontsize=12, fontfamily="sans"))),
#        dpi=300, width=6.5, height=8.5, units="in")

# Figure 2
# ggsave(file.path("Figures",paste("Figure 2",calendaryr,"Nearshore Fishes Annual Report.png")),
#        grid.arrange(G.C, G.D, ncol=2, left=textGrob("     CPUE (fish" ~ "m"^-3* ~ "× 10"^4*")", rot=90,
#                                     gp=gpar(fontsize=12, fontfamily="sans"))),
#        dpi=300, width=6.5, height=8.5, units="in")



## LM: for later development, here is a simpler way to achieve figure 1:
all_fishes_plot <- ggplot(data=Catch_CPUE_Annual, mapping=aes(Year, CPUE.mean.yr)) +
  geom_bar(stat="identity", fill="black") +
  scale_x_discrete(expand=expansion(mult=c(0.025, 0.025)), breaks=seq(1995, calendaryr, 5)) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.1)), breaks=scales::breaks_extended(n=3)) +
  geom_errorbar(mapping=aes(Year, ymin=CPUE.mean.yr - CPUE.SE.yr, ymax=CPUE.mean.yr + CPUE.SE.yr), width=0.5) +
  #geom_rect(merge(Catch_CPUE_Overall.A, Catch_CPUE_Annual.A), mapping=aes(xmin=-Inf, xmax=Inf, ymin=CPUE.mean.sp - CPUE.SE.sp, ymax=CPUE.mean.sp + CPUE.SE.sp, group=OrganismCode), alpha=0.03, fill="gray") +
  geom_hline(data=Catch_CPUE_Overall, mapping=aes(yintercept=CPUE.mean.sp), color="gray45",
             linewidth=0.75, linetype="dashed") + 
  facet_wrap(~ OrganismCode, ncol=4, scales="free_y", strip.position="right", dir="v") +
  theme_classic(base_size=12) + # sets overall font size, makes background solid white
  theme(axis.text=element_text(color="black", family="sans", size=12)) + # sets axis text color, font type
  theme(axis.text.x=element_text(angle=90, vjust=0.5)) + # rotate x-axis labels
  theme(axis.line.y=element_line(color="black", linewidth=.75)) +
  theme(axis.line.x=element_blank()) +
  theme(panel.spacing.y=unit(0.85, "lines")) + # add space between panels
  #labs(x=NULL, y=NULL) + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linewidth=0.75, color="black") + 
  labs(x=NULL, y="     CPUE (fish" ~ "m"^-3* ~ "× 10"^4*")")

ggsave(filename=file.path("Figures",paste("Figure 1",calendaryr,".png")), 
       plot=all_fishes_plot, dpi=300, width=12, height=8.5, units="in")

