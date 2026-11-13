# DJFMP Annual Report- Salmonid Data manipulation and CPUE calculations----------------------------------------

# This is the corrected version using the corrected CWT Code Table for hatchery fish captured before 2014. Removed Liberty Island Seines from dataset. 

### Author- Ryan Mckenzie
### Contact- ryan_mckenzie@fws.gov
### Date- 6/14/22
### R version- 4.0.5

#Enter water year for Current Report
fieldyear <- 2026

#Enter years that you want Chipps Island CHN FL distributions for. Last 5 years has been standard.
FL_years <- c("2026","2025", "2024","2023","2022")

# Target Dates ------------------------------------------------------------
#Currently use water year, so Aug '18 through July '19 is 2019
date_start <- "1999-08-01"
date_end <- paste(fieldyear,"-07-31",sep="")

# Working Environment-------------------------------------------------------

## Load Packages
#library(tidyverse)
library(tidyr)
library(dplyr)
library(lubridate)
library(readxl)
library(purrr)
options(scipen=100)

# The data for this R script are derived from the following EDI data package: 
# https://portal.edirepository.org/nis/mapbrowse?packageid=edi.244.17
# Select the following data entities for Download, and save them in the same folder as the script:
# 1. 1976-2001_DJFMP_trawl_fish_and_water_quality_data.csv (Samples, Catch)
# 2. 2002-2026_DJFMP_trawl_fish_and_water_quality_data.csv (Samples, Catch)
# 3. 1976-2026_DJFMP_beach_seine_fish_and_water_quality_data.csv  (Samples, Catch)
# 4. DJFMP_Fish_Taxonomy.csv (Organisms)
# 5. DJFMP_Site_Locations.csv (Stations)
# We will use 1-3 for this script, which in its current state are missing the key SampleID.  Contact FWS Lodi maintainers of this repo for the complete set of data. 

#################### Import Data from EDI publication - new #########################

## User needs to add the appropriate working directory of downloaded EDI published data##
# wd <- # user's working directory for data
data_dir <- "/Data/2026/edi.244.17/"
##

sample_catch_trawl_1 <- read.csv(file.path(paste0(wd,data_dir,"1976-2001_DJFMP_trawl_fish_and_water_quality_data_SID.csv"))) %>%
  filter(SampleDate >= date_start)
sample_catch_trawl_2 <- read.csv(file.path(paste0(wd,data_dir,"2002-2026_DJFMP_trawl_fish_and_water_quality_data_SID.csv"))) %>%
  filter(SampleDate <= date_end)
sample_catch_trawl <- bind_rows(sample_catch_trawl_1, sample_catch_trawl_2) %>%
  arrange(SampleDate)

sample_catch_beach_seine <- read.csv(file.path(paste0(wd,data_dir,"1976-2026_DJFMP_beach_seine_fish_and_water_quality_data_SID.csv"))) %>%
  filter(SampleDate >= date_start, SampleDate <= date_end)


# Pull in corrected CWT tag codes before 2014

CWT_codes <- read.csv(file="../DJFMP/Data/Corrected_CWT_codes.csv", 
                      header=TRUE)


# Format Trawl Catch and Effort Data------------------------------------------------------------------------

sample_catch_trawl <- sample_catch_trawl %>%
  filter(GearConditionCode %in% c("1","2")) %>% # filters data to only include good samples--i.e. gear condition code 1 and 2 ##LY: current GearConditionCode %in% c(1,2,3,9). Still remove 3, 9?
  filter(FlowDebris %in% c("N",NA)) %>% # only include samples where no debris was recorded. with flow debris present.  ## LY: current FlowDebris %in% c(NA, "N", "Y").  Still remove Y?
  mutate(Volume=case_when(Location=="Chipps Island" & MethodCode=="MWTR"~(FlowmeterDifference*18.58*0.026873), MethodCode=="KDTR"~(FlowmeterDifference*12.54*0.026873), Location=="Sherwood Harbor" & MethodCode=="MWTR"~(FlowmeterDifference*5.08*0.026873))) 


BeforeFY2014 <- sample_catch_trawl %>% filter(SampleDate <= "2014-05-12")
AfterFY2014 <- sample_catch_trawl %>% filter(SampleDate > "2014-05-12")

# Add Field year to the trawl data
sample_catch_trawl <- sample_catch_trawl %>% 
  mutate(SampleDate = lubridate::ymd(SampleDate)) %>%
  mutate(FieldYear = ifelse(month(SampleDate) %in% c(8,9,10,11,12), 
                            year(SampleDate) + 1, year(SampleDate))) %>%
  mutate(keep = case_when(MethodCode=="MWTR" & Location=="Sherwood Harbor" ~ "yes",
                          MethodCode=="KDTR" & Location=="Sherwood Harbor" ~ "yes",
                          MethodCode=="MWTR" & Location=="Chipps Island"~"yes",
                          MethodCode=="KDTR" & Location=="Mossdale" ~ "yes", 
                          TRUE ~ as.character("no"))) %>% 
  filter(keep == "yes")

sample_catch_trawl2 <- sample_catch_trawl %>% 
  filter(Volume > 1000 & Volume < 50000) #50,000 might be low volume, esp. in wet years

# Check for outliers in Trawl data ######

# LY: need to update the dataset from sample_catch_trawl2 to only exclude filtering for outliers
Outlier_Count <- as.numeric(count(sample_catch_trawl)) - as.numeric(count(sample_catch_trawl2))

# anti_join() here returns all rows from the original dataset without a match in the outlier-removed dataset.  These are the outliers -- double check that they should be excluded.
Outliers <- anti_join(sample_catch_trawl, sample_catch_trawl2, by="SampleID")

sample_catch_trawl <- sample_catch_trawl %>% 
  mutate(SampleDate = lubridate::ymd(SampleDate)) # make Dates so can join with other data frame

sample_catch_trawl$SampleDate <- as.character(sample_catch_trawl$SampleDate)


TrawlEffortFieldyear <- sample_catch_trawl2 %>% ##used for plotting unknown fish origins before 2007
  select(SampleID, SampleDate, SampleTime, StationCode, TowNumber, FieldYear) 


# Subset and clean up Seine Effort, add Field Year #############################-----
sample_catch_beach_seine <- sample_catch_beach_seine %>% 
  mutate(SampleDate = lubridate::ymd(SampleDate)) %>%
  mutate(FieldYear = ifelse(month(SampleDate) %in% c(8,9,10,11,12), 
                            year(SampleDate) + 1, year(SampleDate)))

SEIN_Effort <- sample_catch_beach_seine

# Format Dataset, filter for outliers based on SEIN length, width, depth. Remove Liberty Island Seines
sample_catch_beach_seine2 <- sample_catch_beach_seine %>% 
  mutate(Volume = SeineLength*SeineWidth*(SeineDepth/2)) %>% 
  filter(SeineLength <= 15|NA & SeineLength >= 3|NA & SeineWidth <= 15|NA & SeineWidth >= 3|NA & SeineDepth <= 1.5|NA) %>%
  filter(Volume!="NA", RegionCode!="NA", 
         !StationCode %in% c("LI001E", "LI001W","LI002E", "LI002W","LI003E","LI003W",
                             "LI004E","LI004W","LI005E","LI005W","LI006E","LI006W",
                             "LI007E","LI007W","LI008E","LI008W","LI009E","LI009W",
                             "LI010E","LI010W", "LI011E","LI011W")) #Remove Liberty Island Seines

SEINEffortFieldYear <- sample_catch_beach_seine2 %>% ##used for plotting unknown fish origins before 2007
  select(SampleID, SampleDate, SampleTime, StationCode, FieldYear) #LY: is there an equivalent of TowNumber for Beach seine, i.e., does each unique SampleDate, SampleTime, and StationCode represent a unique SampleID?


# Pull in corrected CWT tag codes before 2014

CWT_codes <- read.csv(file="2026_R_code/Salmon and Nearshore reports/Data/Corrected_CWT_codes.csv", 
                      header=TRUE)

# Format CWT Data---------------------------------------------------------------
#Pull in CWT tag codes

library(readxl)
library(dplyr)
library(purrr)

cwt_path <- file.path("2026_R_code", "Salmon and Nearshore reports", "Data", fieldyear, "All_CA-CWT_Newest.xlsx")

# 1. Read all 48 columns as text
CWT <- read_xlsx(
  path = cwt_path,
  sheet = 1,
  col_types = "text"  # Applies "text" type to every column automatically
)

# 2. Convert mark count column to numeric
CWT$cwt_1st_mark_count <- as.numeric(CWT$cwt_1st_mark_count)

# 3. Validation Check: Ensure key ID/mark fields stayed character
required_text_cols <- c(
  "tag_code_or_release_id", 
  "cwt_1st_mark", 
  "cwt_2nd_mark", 
  "non_cwt_1st_mark", 
  "non_cwt_2nd_mark"
)

if (!all(sapply(CWT[required_text_cols], is.character))) {
  stop("Execution halted: One or more CWT mark/tag columns were not imported as character type.")
}

# 4. Format NAs in numeric columns to 0
CWT <- CWT %>%
  map_if(is.numeric, ~ ifelse(is.na(.x), 0, .x)) %>%
  as.data.frame()
# ls removed
CWT$cwt_1st_mark_count <- as.numeric(CWT$cwt_1st_mark_count)
CWT$cwt_2nd_mark_count <- as.numeric(CWT$cwt_2nd_mark_count)
CWT$non_cwt_2nd_mark_count <- as.numeric(CWT$non_cwt_2nd_mark_count)
CWT$non_cwt_1st_mark_count <- as.numeric(CWT$non_cwt_1st_mark_count)


## Quickly look at the values for any obvious issues:
head(unique(CWT$tag_code_or_release_id))
unique(CWT$cwt_1st_mark)
unique(CWT$cwt_2nd_mark)
unique(CWT$non_cwt_1st_mark)
unique(CWT$non_cwt_2nd_mark)

# coalesce allows for replacing missing counts with 0's before summing
CWT <- CWT %>%
  group_by(tag_code_or_release_id) %>%
  mutate(MarkedCount=(if_else(cwt_1st_mark %in% c("0000","0009","9000","9009") | is.na(cwt_1st_mark), 0, coalesce(cwt_1st_mark_count, 0)))+
           (ifelse(cwt_2nd_mark %in% c("0000","0009","9000","9009") | is.na(cwt_2nd_mark), 0, coalesce(cwt_2nd_mark_count, 0)))+
           (ifelse(non_cwt_2nd_mark %in% c("0000","0009","9000","9009") | is.na(non_cwt_2nd_mark), 0, coalesce(non_cwt_2nd_mark_count, 0)))+
           (ifelse(non_cwt_1st_mark %in% c("0000","0009","9000","9009")| is.na(non_cwt_1st_mark), 0, coalesce(non_cwt_1st_mark_count, 0)))) %>%
  mutate(UnmarkedCount=ifelse(cwt_1st_mark == "0000" | is.na(cwt_1st_mark),coalesce(cwt_1st_mark_count, 0), 0) +
         (ifelse(cwt_2nd_mark == "0000" | is.na(cwt_2nd_mark),coalesce(cwt_2nd_mark_count, 0), 0))+
         (ifelse(non_cwt_2nd_mark == "0000" | is.na(non_cwt_2nd_mark),coalesce(non_cwt_2nd_mark_count, 0), 0)) +
         (ifelse(non_cwt_1st_mark == "0000" | is.na(non_cwt_1st_mark), coalesce(non_cwt_1st_mark_count, 0),0)) ) %>%
  #mutate(Unknown=(ifelse(cwt_1st_mark %in% c("0009","9000","9009"), coalesce(cwt_1st_mark_count, 0), 0))+
  #          (ifelse(cwt_2nd_mark %in% c("0009","9000","9009"), coalesce(cwt_2nd_mark_count, 0), 0))+
  #          (ifelse(non_cwt_2nd_mark %in% c("0009","9000","9009"), coalesce(non_cwt_2nd_mark_count, 0), 0))+
  #          (ifelse(non_cwt_1st_mark %in% c("0009","9000","9009"), coalesce(non_cwt_1st_mark_count, 0), 0)) ) %>% #LY: I took out Unknown because 000,9000,9009 aren't in this dataset
  mutate(Totalcount=sum(cwt_1st_mark_count,cwt_2nd_mark_count,non_cwt_1st_mark_count,non_cwt_2nd_mark_count, na.rm = TRUE)) %>%
  mutate(RatioUnmarkedvsMarked=(UnmarkedCount/MarkedCount)) %>%
  mutate(RaceByCWT=case_when(run %in% '1'~"Spring", run %in% '2'~ "Summer", run %in% '3' ~ "Fall", run %in% '5'~ "Hybrid",
                              run %in% '6'~ "Landlocked", run %in% '7'~ "LateFall",run %in% '4'~"Winter", run %in% '8'~"LateFall")) %>%
  select(tag_code_or_release_id, RaceByCWT, first_release_date, Totalcount, MarkedCount,
         UnmarkedCount,RatioUnmarkedvsMarked) %>% #  Unknown, 
  dplyr::rename(TagCode=tag_code_or_release_id) %>%
  mutate(RatioMarkedTotal = MarkedCount/Totalcount)



# Format Catch Data-------------------------------------------------------------
  # RaceByTag (dbo_CodeWireTag)---field used before 2014
  # RaceByTag (dbo_Catch)---field used after 2014

#LY: Combine BS and trawl datasets together
bs <- sample_catch_beach_seine2 %>%
  mutate(Secchi = NA, TowNumber = NA)
# format trawl data date
sample_catch_trawl2 <- sample_catch_trawl2 %>%
  mutate(SampleDate = as.Date(SampleDate))

Catch_effort <- bind_rows(sample_catch_trawl2, bs)
SalmonidCatch <- subset(Catch_effort, OrganismCode %in% c("CHN","RBT"))

# Split Catch up into pre and post 2014 CWT and non-CWT fish
SalmonidCatchAfter2014 <- SalmonidCatch %>%
  #filter(SampleID %in% AfterFY2014$SampleID) %>%
  filter(SampleDate > ymd("2014-05-12")) %>%
  select(SampleID, SampleDate, SampleTime, StationCode, TowNumber, OrganismCode, MarkCode, StageCode, TagCode, RaceByTag, RaceByLength,ForkLength, Count)

CWTCatchBefore2014 <- SalmonidCatch %>%
  filter(SampleDate <= ymd("2014-05-12")) %>%
  filter(!MarkCode %in% c("none","None"))  %>%
  select(SampleID,SampleDate, SampleTime, StationCode, TowNumber, OrganismCode, MarkCode, StageCode, TagCode, RaceByTag, RaceByLength, ForkLength, Count)

NonCWTCatchBefore2014 <- SalmonidCatch %>%
  filter(SampleDate <= ymd("2014-05-12")) %>%
  filter(MarkCode %in% c("none","None")) %>%
  select(SampleID,SampleDate, SampleTime, StationCode, TowNumber, OrganismCode, MarkCode, StageCode, TagCode, RaceByTag, RaceByLength,
         ForkLength, Count)

CWT_codes <- CWT_codes %>%
  filter(SampleID %in% BeforeFY2014$SampleID) %>%
  mutate(Count=CWTCount) %>%
  select(SampleID, OrganismCode, MarkCode, StageCode, TagCode, RaceByTag, RaceByLength,
         ForkLength, Count)

#QC Step. Numbers should be within 50. The mismatch is caused by more fish being present in the old CWT code table then what was recorded on the datasheet. 
as.numeric(sum(CWTCatchBefore2014$Count))
as.numeric(sum(CWT_codes$Count))

#2023: sum of CWT catch before 2014 is 55434, should not change drastically over years 
#Recombine catch using the CWT Code table in place of the Catch Table for CWT fish before 2014

SalmonidCatch2 <- bind_rows(CWT_codes, NonCWTCatchBefore2014, SalmonidCatchAfter2014)


#QC Step. Numbers should be within 50. The mismatch is caused by more fish being present in the old cWT code table then what was recorded on the datasheet. 
---------------------------------- 
as.numeric(sum(SalmonidCatch$Count))
as.numeric(sum(SalmonidCatch2$Count))

# Continued Formatting----------------make sure to check that mark code list makes sense i.e new mark codes made 
summary(as.factor(SalmonidCatch2$MarkCode))
summary(as.factor(SalmonidCatch2$RaceByLength))
summary(as.factor(SalmonidCatch2$RaceByTag))
summary(as.factor(SalmonidCatch2$TagCode))

# Should be FALSE:
any(!is.na(SalmonidCatch2$RaceByTag) & SalmonidCatch2$RaceByTag == "Adult")

## Note: using the character "NA" can be confusing with actual NA's. 
## Switching from "NA" to "NA_string" to avoid ambiguity. 
SalmonidCatch2 <- SalmonidCatch2 %>% 
  filter(MarkCode %in% c("Adclipped","AdClipped","none","None","Plvc","PlvcLft + Ad")) %>%  # remove trawl efficiency fish that have VIE markings and Acoustic tags
  filter(!TagCode %in% c("YeSd(86)","YeSd(77)","YeSd(76)2","YeSd(76)1","ReSd(86)",
                         "ReSd(77)2","ReSd(77)1","ReSd(76)2","ReSd(76)2",
                         "ReSd(76)1")) %>% # remove the rest of efficiency fish
  mutate(MarkCode=ifelse(MarkCode %in% c("none", "None"), "None", "Marked")) %>% #Make Mark Code consistent
  mutate(TagCode=ifelse(TagCode %in% c(" ","n/a","LT","NT","NR","None","none","NoTag",
                                       "DT","LF") | is.na(TagCode), NA, TagCode)) %>%# make tagcode NAs consistent
  mutate(RaceByTag=case_when(RaceByTag %in% c("fall","Fall")~"Fall",  ## aggregates columns into one
                             RaceByTag %in% c("Hybrid")~"Hybrid", 
                             RaceByTag %in% c("LateFall")~"LateFall", 
                             RaceByTag %in% c("spring","Spring")~"Spring", 
                             RaceByTag %in% c("winter","Winter")~"Winter",
                             RaceByTag %in% c("Not Raced", "n/p", "", NA)~"NA_string")) %>%
  mutate(RaceByLength=as.factor(case_when(RaceByLength %in% c("Fall")~"Fall", ## replaces n/p and not raced fish with NAs
                                          RaceByLength %in% c("LateFall")~"LateFall",
                                          RaceByLength %in% c("Adult")~"Adult",
                                          RaceByLength %in% c("Spring")~"Spring",
                                          RaceByLength %in% c("Winter")~"Winter",
                                          RaceByLength %in% c("Not Raced", "n/p", "", NA)~"NA_string"))) %>%
  filter(!RaceByLength %in% c("Adult")) ##Filters out adults

unique(SalmonidCatch2$RaceByTag)

summary(as.factor(SalmonidCatch2$MarkCode))
summary(as.factor(SalmonidCatch2$RaceByLength))
summary(as.factor(SalmonidCatch2$RaceByTag))

SalmonidCatch2Sum <- SalmonidCatch2 %>%
  select(SampleID, OrganismCode, MarkCode, StageCode, TagCode, RaceByTag, RaceByLength,
         Count) %>%
  group_by(SampleID, OrganismCode, MarkCode, StageCode, TagCode, RaceByTag, 
           RaceByLength) %>%
  transmute(SumOfCatchCount=sum(Count)) %>%
  unique() %>%
  as.data.frame()

SalmonidCatch2Sum$SumOfCatchCount <- as.numeric(as.character(SalmonidCatch2Sum$SumOfCatchCount))####Have to properly convert counts from factor to numeric variable
SalmonidCatch2Sum$SampleID <- as.factor(SalmonidCatch2Sum$SampleID)

## Make a dataset for Forklength Data
SalmonidFL <- SalmonidCatch2 %>%
  select(SampleID, OrganismCode, MarkCode, StageCode, TagCode, RaceByTag, RaceByLength, 
         ForkLength, Count) %>%
  group_by(SampleID, OrganismCode, MarkCode, StageCode, TagCode, RaceByTag, RaceByLength, 
           ForkLength) %>%
  dplyr::summarize(SumOfCatchCount=sum(Count),
                   .groups="drop") %>%
  arrange(SampleID, OrganismCode, MarkCode, StageCode, TagCode, RaceByTag, RaceByLength, 
          ForkLength) %>%
  as.data.frame()

SalmonidFL$SumOfCatchCount <- as.numeric(as.character(SalmonidFL$SumOfCatchCount))####Have to properly convert counts from factor to numeric variable
SalmonidFL$SampleID <- as.factor(SalmonidFL$SampleID)


#Subset based on Species
CHN_Catch <- subset(SalmonidCatch2Sum, OrganismCode == "CHN")
RBT_Catch <- subset(SalmonidCatch2Sum, OrganismCode == "RBT")

CHN_FL <- subset(SalmonidFL, OrganismCode == "CHN")
RBT_FL <- subset(SalmonidFL, OrganismCode == "RBT")

# remove unused data frames
rm(SalmonidCatch2, SalmonidCatch2Sum, SalmonidFL, AfterFY2014, BeforeFY2014)



## Note that there are duplicated tag codes, indicated with asterisks in the CWT file.
## These fish all appear to have RaceByTag values anyhow.
grep("\\*", CWT$TagCode, value=TRUE)

examine_duplicated_tagcodes <- subset(CWT, grepl("\\*", TagCode))
examine_duplicated_tagcodes$a <- sub("(.*)(\\*.*)", "\\1", 
                                     examine_duplicated_tagcodes$TagCode)
tmp <- subset(CHN_Catch, TagCode %in% examine_duplicated_tagcodes$a)
sum(tmp$SumOfCatchCount)
unique(tmp$RaceByTag)   # Spring



####################CHN Data Manipulation#################################
####CHN Trawls#####
# Step 1. Combine Chinook Catch and CWT Data sets, Assign Race to fish-----------------------------------------------------------

# CWT_codes needs to have leading 0's removed from TagCodes
CWT_codes <- CWT_codes %>% 
  mutate(TagCode = ifelse(TagCode == "NT", NA, TagCode)) %>%
  mutate(TagCode = sub("^0+", "", TagCode)) %>%
  mutate(TagCode = as.numeric(TagCode))

## Add TagCode information (race and ratio of unmarked to marked) to Catch Dataset
CHN_Trawl_Catch <- left_join(CHN_Catch, CWT, by="TagCode")

TrawlEffortFieldyear$SampleID <- as.factor(TrawlEffortFieldyear$SampleID)

## Add Sample Year to Catches so we can designate unknown origin Chinook in samples taken before 2008 field year
CHN_Trawl_Catch <- inner_join(TrawlEffortFieldyear, CHN_Trawl_Catch, by="SampleID")


## QC Data set and make corrections
CHN_Trawl_Catch <- CHN_Trawl_Catch %>% 
  mutate(RaceByTag=ifelse(is.na(RaceByCWT), RaceByTag, RaceByCWT)) %>%
  mutate(Race=case_when(RaceByTag=="NA_string" & MarkCode=="Marked" ~ "Unknown", 
                        RaceByTag=="NA_string" & MarkCode == "None" ~ as.character(RaceByLength),  
                        !RaceByTag=="NA_string" ~ as.character(RaceByTag))) %>%
  select(SampleID, FieldYear, OrganismCode, TagCode, RatioUnmarkedvsMarked, Race, 
         MarkCode, SumOfCatchCount)
CHN_Trawl_Catch$RatioUnmarkedvsMarked[is.na(CHN_Trawl_Catch$RatioUnmarkedvsMarked)] <- 0

## Calculate and add columns for Marked and Unmarked Chinook counts for specific tag codes within samples 
CHN_Trawl_Catch <- CHN_Trawl_Catch %>%
  mutate(Marked_Count=case_when(MarkCode %in% c("Marked") ~ SumOfCatchCount)) %>%  #create count of marked individuals
  group_by(SampleID, FieldYear, OrganismCode, TagCode, RatioUnmarkedvsMarked) %>% 
  mutate(Marked_Count_per_tagcode=case_when(MarkCode %in% c("Marked")&!is.na(TagCode)~sum(Marked_Count))) %>% #sum counts of marked individuals with specific tag codes within samples
  ungroup() %>%
  mutate(Unmarked_Count_per_tagcode=RatioUnmarkedvsMarked * Marked_Count_per_tagcode) #sum counts of unmarked individuals based on marking ratio and #of marked individuals for specific tag codes
  
CHN_Trawl_Catch$Marked_Count[is.na(CHN_Trawl_Catch$Marked_Count)] <- 0
CHN_Trawl_Catch$Marked_Count_per_tagcode[is.na(CHN_Trawl_Catch$Marked_Count_per_tagcode)] <- 0
CHN_Trawl_Catch$Unmarked_Count_per_tagcode[is.na(CHN_Trawl_Catch$Unmarked_Count_per_tagcode)] <- 0

sample_catch_trawl2$SampleID <- as.factor(sample_catch_trawl2$SampleID)
CHN_taggers_catch <- inner_join(subset(sample_catch_trawl2, FieldYear == fieldyear), 
                                CHN_Trawl_Catch, by="SampleID")
# Step 2. Subset Dataset by Races of interest------------------------------------------------------------------------------------

## Separate out Winter Run and Non-winter Run (grouped by Spring vs. all remaining 
## races combined):
CHNW_Trawl_Catch <- subset(CHN_Trawl_Catch, Race == "Winter") %>%
  dplyr::mutate(CustomRaceGroup=Race)

CHNnotW_Trawl_Catch <- subset(CHN_Trawl_Catch, !(Race %in% c("Winter"))) %>%
  dplyr::mutate(CustomRaceGroup=ifelse(Race == "Spring", "Spring", "Not_SpringWinter"))


# Step 3. Calculate and summarize Chinook catches for marked, unmarked, wild, and unknown individuals per sample for each race -------------------------------

## Winter Run
CHNW_Trawl_Catch_Summary <- CHNW_Trawl_Catch %>%
  dplyr::group_by(SampleID, OrganismCode, CustomRaceGroup) %>%
  dplyr::summarize(Total_CHN=sum(SumOfCatchCount),
									 Marked_Total=sum(Marked_Count),
									 Unmarked_Total_TMP=round(sum(Unmarked_Count_per_tagcode)),
									 .groups="drop") %>%
	dplyr::mutate(Unmarked_Total=ifelse((Total_CHN-Marked_Total) < Unmarked_Total_TMP, 
																			(Total_CHN-Marked_Total),
																			Unmarked_Total_TMP),
								Wild=ifelse((Total_CHN-(Marked_Total+Unmarked_Total)) > 0, 
														round(Total_CHN-(Marked_Total+Unmarked_Total)),
														0)) %>%
	dplyr::select(-Unmarked_Total_TMP) %>%
  tidyr::pivot_longer(cols=c(Marked_Total, Unmarked_Total, Wild), 
											names_to="Origin", 
											values_to="Count")

### Data QC: You can check to make sure that all fish are accounted for by making sure the counts below match
if(sum(CHNW_Trawl_Catch_Summary$Total_CHN)/3 != sum(CHNW_Trawl_Catch_Summary$Count)) {
	stop("There is a problem with CHNW_Trawl_Catch_Summary.\n")
}


## Non-winter Run
CHNnotW_Trawl_Catch_Summary <- CHNnotW_Trawl_Catch %>%
  dplyr::group_by(SampleID, FieldYear, OrganismCode, CustomRaceGroup) %>%
  dplyr::summarize(Total_CHN=sum(SumOfCatchCount),
									 Marked_Total=sum(Marked_Count),
									 Unmarked_Total_TMP=round(sum(Unmarked_Count_per_tagcode)),
									 .groups="drop") %>%
  mutate(Unknown=ifelse(FieldYear <= 2007, (Total_CHN-Marked_Total), 0), 
				 Unmarked_Total=ifelse((Total_CHN-Marked_Total) < Unmarked_Total_TMP,
															 (Total_CHN-Marked_Total),
																Unmarked_Total_TMP),
				 Unmarked_Total=ifelse(FieldYear >= 2008, Unmarked_Total, 0),
				 Wild=ifelse((Total_CHN-(Marked_Total+Unmarked_Total)) > 0,
										 round(Total_CHN-(Marked_Total+Unmarked_Total)),
										 0),
				 Wild=ifelse(FieldYear >= 2008, Wild, 0)) %>%
	dplyr::select(-c(Unmarked_Total_TMP, FieldYear)) %>%
		tidyr::pivot_longer(cols=c(Marked_Total, Unknown, Unmarked_Total, Wild), 
												names_to="Origin", 
												values_to="Count")
if(sum(CHNnotW_Trawl_Catch_Summary$Total_CHN)/4 != sum(CHNnotW_Trawl_Catch_Summary$Count)) {
	stop("There is a problem with CHNnotW_Trawl_Catch_Summary.\n")
}


CHNcustom_Trawl_Catch_Summary <- dplyr::bind_rows(CHNW_Trawl_Catch_Summary,
                                                  CHNnotW_Trawl_Catch_Summary)


# Step 4. Combine catch and effort datasets, format, add zero catches, calculate CPUE -----------------------------------------------------------------------------------------------------
# Add in MONTH and DAY
sample_catch_trawl3 <- sample_catch_trawl2 %>%
  mutate(MONTH = month(SampleDate), DAY = day(SampleDate)) %>%
  mutate(MONTH = case_when(MONTH == 8 ~ "Aug", MONTH == 9 ~ "Sep", MONTH == 10 ~ "Oct", 
                           MONTH == 11 ~ "Nov", MONTH == 12 ~ "Dec", MONTH == 1 ~ "Jan", 
                           MONTH == 2 ~ "Feb", MONTH == 3 ~ "Mar", MONTH == 4 ~ "Apr", 
                           MONTH == 5 ~ "May", MONTH == 6 ~ "Jun", MONTH == 7 ~ "Jul"))

sample_catch_trawl3$MONTH <- factor(sample_catch_trawl3$MONTH, levels=c("Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul"))

# limit to only effort again, so we can merge in the summary catch per sampleID
sample_trawl_only <- sample_catch_trawl3 %>%
  select(SampleID, SampleDate, FieldYear, MONTH, DAY,Location, StationCode, Volume, MethodCode) %>%
  unique()

CHNcustom_Trawl_CPUE <- left_join(sample_trawl_only, CHNcustom_Trawl_Catch_Summary, 
                                  by="SampleID") %>%
  select(SampleID, SampleDate, FieldYear, MONTH, DAY,Location, StationCode, Volume, MethodCode, OrganismCode, CustomRaceGroup, Origin, Count) %>%
  filter(!is.na(Volume))

## Format Dataset
## Placeholder values of "CHN" and "None" for samples with no CHN:
CHNcustom_Trawl_CPUE$OrganismCode[is.na(CHNcustom_Trawl_CPUE$OrganismCode)] <- "CHN"
CHNcustom_Trawl_CPUE$CustomRaceGroup[is.na(CHNcustom_Trawl_CPUE$CustomRaceGroup)] <- "None"
CHNcustom_Trawl_CPUE$Origin[is.na(CHNcustom_Trawl_CPUE$Origin)] <- "Wild"
CHNcustom_Trawl_CPUE$Count[is.na(CHNcustom_Trawl_CPUE$Count)] <- 0 
CHNcustom_Trawl_CPUE$Origin <- as.factor(CHNcustom_Trawl_CPUE$Origin)
CHNcustom_Trawl_CPUE$SampleID <- as.factor(CHNcustom_Trawl_CPUE$SampleID)
CHNcustom_Trawl_CPUE$FieldYear <- as.factor(CHNcustom_Trawl_CPUE$FieldYear)

## Add zeroes for samples with no catch
CHNcustom_Trawl_CPUE <- CHNcustom_Trawl_CPUE %>% 
  complete(nesting(SampleID, SampleDate, FieldYear, MONTH, DAY, Location, StationCode, Volume, 
                   MethodCode), OrganismCode, CustomRaceGroup, Origin, 
           fill=list(Count=0)) %>%
  mutate(CPUE=Count/Volume) %>%
  unique()


# Step 5. Calculate Monthly Means for Regions for Current sampling year -------------------------------------------------------                           

## With kodiak and mid water combined
CHNcustom_Trawl_Monthly_Means <- CHNcustom_Trawl_CPUE %>%
  group_by(FieldYear, MONTH, DAY, Location, StationCode, OrganismCode, 
           CustomRaceGroup, Origin) %>% ##mean daily CPUE by Station
  summarise(CPUE_day_station=mean(CPUE, na.rm=TRUE),
            .groups="drop") %>%
  group_by(FieldYear, MONTH, DAY, Location, OrganismCode, CustomRaceGroup, Origin) %>% ##mean daily CPUE by site
  summarise(CPUE_day_site=mean(CPUE_day_station, na.rm=TRUE),
            .groups="drop") %>%
  group_by(FieldYear, MONTH, Location, OrganismCode, CustomRaceGroup, Origin) %>%  ##mean monthly CPUE by site
  summarise(AvgCPUE=mean(CPUE_day_site, na.rm=TRUE),
            .groups="drop") %>%
  filter(!is.nan(AvgCPUE)) %>%
  dplyr::filter(CustomRaceGroup != "None") # drop placeholder for samples with no CHN

## Monthly Means for Current Year
CHNcustom_Trawl_Monthly_Means_CurrentYear <- CHNcustom_Trawl_Monthly_Means %>%
  filter(FieldYear == fieldyear)


# Step 6. Calculate Annual Means of CPUE for Regions ------------------------------------------------------------------                           

CHNcustom_Trawl_Annual_Means <- CHNcustom_Trawl_Monthly_Means %>%
  group_by(FieldYear, Location, OrganismCode, CustomRaceGroup, Origin) %>%
  summarise(AvgCPUE=mean(AvgCPUE, na.rm=TRUE),
            .groups="drop")



####CHN SEINES#####
# Step 1. Combine Chinook Catch and CWT Datasets, Assign Race to fish-----------------------------------------------------------

## Add TagCode information (race and ratio of unmarked to marked) to Catch Dataset
CWT$TagCode <- as.factor(CWT$TagCode)
CHN_SEIN_Catch <- left_join(CHN_Catch, CWT, by="TagCode")

## Add Sample Year to Catches so we can designate unknown origin Chinook in samples taken before 2008 field year
SEINEffortFieldYear$SampleID <- as.factor(SEINEffortFieldYear$SampleID)
CHN_SEIN_Catch <- inner_join(CHN_SEIN_Catch, SEINEffortFieldYear, by="SampleID")

## QC Datset and make corrections
CHN_SEIN_Catch <- CHN_SEIN_Catch %>% 
  mutate(MarkCode=ifelse(MarkCode %in% c("none", "None")|is.na(MarkCode),"None", "Marked")) %>%
  mutate(RaceByTag=ifelse(is.na(RaceByCWT), RaceByTag, RaceByCWT)) %>%
  mutate(Race=case_when(RaceByTag=="NA_string" & MarkCode=="Marked"~"Unknown", 
                        RaceByTag=="NA_string" & MarkCode == "None" ~as.character(RaceByLength),  
                        !RaceByTag=="NA_string"~as.character(RaceByTag))) %>%
  select(SampleID, FieldYear, OrganismCode, TagCode, RatioUnmarkedvsMarked, Race, 
         MarkCode, SumOfCatchCount)
CHN_SEIN_Catch$RatioUnmarkedvsMarked[is.na(CHN_SEIN_Catch$RatioUnmarkedvsMarked)] <- 0

## Calculate and add columns for Marked and Unmarked Chinook counts for specific tag codes within samples 
CHN_SEIN_Catch <- CHN_SEIN_Catch %>%
  mutate(Marked_Count= case_when(MarkCode %in% c("Marked")~SumOfCatchCount)) %>%  #create count of marked individuals
  group_by(SampleID, FieldYear, OrganismCode, TagCode, RatioUnmarkedvsMarked) %>% 
  mutate(Marked_Count_per_tagcode=case_when(MarkCode %in% c("Marked")&!is.na(TagCode)~sum(Marked_Count))) %>% #sum counts of marked individuals with specific tagcodes within samples
  ungroup() %>%
  mutate(Unmarked_Count_per_tagcode = RatioUnmarkedvsMarked * Marked_Count_per_tagcode) #sum counts of unmarked individuals based on marking ratio and #of marked individuals for specific tag codes

CHN_SEIN_Catch$Marked_Count[is.na(CHN_SEIN_Catch$Marked_Count)] <- 0
CHN_SEIN_Catch$Marked_Count_per_tagcode[is.na(CHN_SEIN_Catch$Marked_Count_per_tagcode)] <- 0
CHN_SEIN_Catch$Unmarked_Count_per_tagcode[is.na(CHN_SEIN_Catch$Unmarked_Count_per_tagcode)] <- 0


# Step 2. Subset Dataset by Races of interest------------------------------------------------------------------------------------

## Separate out Winter Run and Non-winter Run (grouped by Spring vs. all remaining 
## races combined):
CHNW_SEIN_Catch <- subset(CHN_SEIN_Catch, Race == "Winter") %>%
  dplyr::mutate(CustomRaceGroup=Race)

CHNnotW_SEIN_Catch <- subset(CHN_SEIN_Catch, !(Race %in% c("Winter"))) %>%
  dplyr::mutate(CustomRaceGroup=ifelse(Race == "Spring", "Spring", "Not_SpringWinter"))


# Step 3. Calculate and summarize chinook catches for marked, unmarked, wild, and unknown individuals per sample for each race -------------------------------

## Winter Run
CHNW_SEIN_Catch_Summary <- CHNW_SEIN_Catch %>%
  dplyr::group_by(SampleID, OrganismCode, CustomRaceGroup) %>%
  dplyr::summarize(Total_CHN=sum(SumOfCatchCount), 
									 Marked_Total=sum(Marked_Count),
									 Unmarked_Total_TMP=round(sum(Unmarked_Count_per_tagcode)),
									 .groups="drop") %>%
  dplyr::mutate(Unmarked_Total=ifelse((Total_CHN-Marked_Total) < Unmarked_Total_TMP,
																			(Total_CHN-Marked_Total), 
																			Unmarked_Total_TMP),
								Wild=ifelse((Total_CHN-(Marked_Total+Unmarked_Total)) > 0, 
														round(Total_CHN-(Marked_Total+Unmarked_Total)),
														0)) %>%
	dplyr::select(-Unmarked_Total_TMP) %>%
	tidyr::pivot_longer(cols=c(Marked_Total, Unmarked_Total, Wild), 
											names_to="Origin", 
											values_to="Count")
if(sum(CHNW_SEIN_Catch_Summary$Total_CHN)/3 != sum(CHNW_SEIN_Catch_Summary$Count)) {
	stop("There is a problem with CHNW_SEIN_Catch_Summary.\n")
}


## Non-winter Run
CHNnotW_SEIN_Catch_Summary <- CHNnotW_SEIN_Catch %>%
  dplyr::group_by(SampleID, FieldYear, OrganismCode, CustomRaceGroup) %>%
  dplyr::summarize(Total_CHN=sum(SumOfCatchCount), 
									 Marked_Total=sum(Marked_Count), 
									 Unmarked_Total_TMP=round(sum(Unmarked_Count_per_tagcode)),
									 .groups="drop") %>%
  dplyr::mutate(Unknown=ifelse(FieldYear <= 2007, (Total_CHN-Marked_Total), 0), 
								Unmarked_Total=ifelse((Total_CHN-Marked_Total) < Unmarked_Total_TMP, 
																			(Total_CHN-Marked_Total),
																			Unmarked_Total_TMP), 
								Unmarked_Total=ifelse(FieldYear >= 2008, Unmarked_Total, 0),
								Wild=ifelse((Total_CHN-(Marked_Total+Unmarked_Total)) > 0, 
														round(Total_CHN-(Marked_Total+Unmarked_Total)),
														0), 
								Wild=ifelse(FieldYear >= 2008, Wild, 0)) %>%
	dplyr::select(-c(Unmarked_Total_TMP, FieldYear)) %>%
		tidyr::pivot_longer(cols=c(Marked_Total, Unknown, Unmarked_Total, Wild), 
												names_to="Origin", 
												values_to="Count")
if(sum(CHNnotW_SEIN_Catch_Summary$Total_CHN)/4 != sum(CHNnotW_SEIN_Catch_Summary$Count)) {
	stop("There is a problem with CHNnotW_SEIN_Catch_Summary.\n")
}


CHNcustom_SEIN_Catch_Summary <- dplyr::bind_rows(CHNW_SEIN_Catch_Summary,
                                                 CHNnotW_SEIN_Catch_Summary)


# Step 4. Combine catch and effort datasets, format, add zero catches, calculate CPUE -----------------------------------------------------------------------------------------------------
SEINEffortFieldYear$SampleID <- as.factor(SEINEffortFieldYear$SampleID)

SEIN_Effort <- sample_catch_beach_seine2 %>%
  mutate(MONTH = month(SampleDate), DAY = day(SampleDate)) %>%
  mutate(MONTH = case_when(MONTH == 8 ~ "Aug", MONTH == 9 ~ "Sep", MONTH == 10 ~ "Oct", 
                           MONTH == 11 ~ "Nov", MONTH == 12 ~ "Dec", MONTH == 1 ~ "Jan", 
                           MONTH == 2 ~ "Feb", MONTH == 3 ~ "Mar", MONTH == 4 ~ "Apr", 
                           MONTH == 5 ~ "May", MONTH == 6 ~ "Jun", MONTH == 7 ~ "Jul")) 

SEIN_Effort$MONTH <- factor(SEIN_Effort$MONTH, levels=c("Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul"))

# limit to only effort again, so we can merge in the summary catch per sampleID
sample_seine_only <- SEIN_Effort %>%
  select(SampleID, FieldYear, MONTH, DAY,Location, RegionCode, StationCode, Volume, MethodCode) %>%
  unique()

sample_seine_only$SampleID <- as.factor(sample_seine_only$SampleID)

CHNcustom_SEIN_CPUE <- left_join(sample_seine_only, CHNcustom_SEIN_Catch_Summary, 
                                 by="SampleID") %>%
  select(SampleID, FieldYear, MONTH, DAY, Location, RegionCode, StationCode, Volume, 
         MethodCode, OrganismCode, CustomRaceGroup, Origin, Count) %>%
  filter(!is.na(Volume))


## Format Dataset
## Placeholder values of "CHN" and "None" for samples with no CHN:
CHNcustom_SEIN_CPUE$OrganismCode[is.na(CHNcustom_SEIN_CPUE$OrganismCode)] <- "CHN"
CHNcustom_SEIN_CPUE$CustomRaceGroup[is.na(CHNcustom_SEIN_CPUE$CustomRaceGroup)] <- "None"
CHNcustom_SEIN_CPUE$Origin[is.na(CHNcustom_SEIN_CPUE$Origin)] <- "Wild"
CHNcustom_SEIN_CPUE$Count[is.na(CHNcustom_SEIN_CPUE$Count)] <- 0 
CHNcustom_SEIN_CPUE$Origin <- as.factor(CHNcustom_SEIN_CPUE$Origin)
CHNcustom_SEIN_CPUE$SampleID <- as.factor(CHNcustom_SEIN_CPUE$SampleID)
CHNcustom_SEIN_CPUE$FieldYear <- as.factor(CHNcustom_SEIN_CPUE$FieldYear)

## Add zeroes for samples with no catch
CHNcustom_SEIN_CPUE <- CHNcustom_SEIN_CPUE %>% 
  complete(nesting(SampleID, FieldYear, MONTH, DAY, Location, RegionCode,
                   StationCode, Volume, MethodCode), 
           OrganismCode, CustomRaceGroup, Origin, fill=list(Count=0)) %>%
  mutate(CPUE=Count/Volume) %>%
  unique()


# Step 5. Calculate Monthly Means for Regions for Current sampling year-------------------------------------------------------                           

### Monthly CPUE averages per region were calculated by averaging all samples taken at each station per month. Monthly station averages within each region
### were then averaged together. We found a strong correlation between the average CPUE values calculated this way versus
### the previous way that started by averaging by station per week, then region per week, then region per month. Our new simplified calculations
### streamlined the process and removed the need to create an All date table in Access 

##  Model output comparing average CPUE values from simplified averaging and past averaging calculations: 
## lm(formula = CHNW_Catch$CPUE_month_region.x ~ CHNW_Catch$CPUE_month_region.y)

##Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.079620 -0.000027 -0.000027 -0.000027  0.065487 

#Coefficients:
#                                Estimate Std. Error t value            Pr(>|t|)    
#(Intercept)                    0.00002719 0.00004591   0.592               0.554    
#CHNW_Catch$CPUE_month_region.y 1.06525543 0.00499180 213.401 <0.0000000000000002 ***
#  ---

# Residual standard error: 0.002673 on 3412 degrees of freedom
# Multiple R-squared:  0.9303,	Adjusted R-squared:  0.9303 
# F-statistic: 4.554e+04 on 1 and 3412 DF,  p-value: < 0.00000000000000022


CHNcustom_SEIN_Monthly_Means <- CHNcustom_SEIN_CPUE %>%
  group_by(FieldYear, MONTH, RegionCode, StationCode, OrganismCode, CustomRaceGroup, 
           Origin) %>% ##mean monthly CPUE by Station
  summarise(CPUE_month_station=mean(CPUE, na.rm=TRUE),
            .groups="drop") %>%
  group_by(FieldYear, MONTH, RegionCode, OrganismCode, CustomRaceGroup, Origin) %>%  ##mean monthly CPUE by Region
  summarise(AvgCPUE=mean(CPUE_month_station, na.rm=TRUE),
            .groups="drop") %>%
  filter(!is.nan(AvgCPUE)) %>%
  dplyr::filter(CustomRaceGroup != "None") # drop placeholder for samples with no CHN

## Monthly Means for Current Year
CHNcustom_SEIN_Monthly_Means_CurrentYear <- CHNcustom_SEIN_Monthly_Means %>%
  filter(FieldYear == fieldyear)


# Step 6. Calculate Annual Means of CPUE for Regions ------------------------------------------------------------------                           

CHNcustom_SEIN_Annual_Means <- CHNcustom_SEIN_Monthly_Means %>%
  group_by(FieldYear, RegionCode, OrganismCode, CustomRaceGroup, Origin) %>%
  summarise(AvgCPUE=mean(AvgCPUE, na.rm=TRUE),
            .groups="drop")



####Annual CHN FL Distribution at Chipps#######################
CHN_Chipps_FLraw <- inner_join(subset(sample_trawl_only, Location %in% c("Chipps Island")), 
                               CHN_FL, by="SampleID")

CHN_Chipps_FLraw <- CHN_Chipps_FLraw %>% 
  mutate(RaceByLength=as.factor(case_when(RaceByLength %in% c("Fall")~"Fall", ## replaces n/p and not raced fish with NAs
                                          RaceByLength %in% c("LateFall")~"LateFall",
                                          RaceByLength %in% c("Adult")~"Adult",
                                          RaceByLength %in% c("Spring")~"Spring",
                                          RaceByLength %in% c("Winter")~"Winter"))) %>%
  mutate(YEAR = year(SampleDate)) %>%
         select(SampleID, FieldYear, YEAR, MONTH, DAY, OrganismCode, RaceByTag, RaceByLength, MarkCode,
         TagCode, ForkLength, StageCode, SumOfCatchCount) %>%
  filter(RaceByLength %in% c("LateFall", "Fall", "Spring" , "Winter")|RaceByLength=="NA_string") ##Filters out adults

CHN_Chipps_FLraw$YEAR <- as.numeric(CHN_Chipps_FLraw$FieldYear)
CHN_Chipps_FLraw$SumOfCatchCount <- as.numeric(as.character(CHN_Chipps_FLraw$SumOfCatchCount))####Have to properly convert counts from factor to numeric variable

CHN_Chipps_FLraw$MONTH <- factor(CHN_Chipps_FLraw$MONTH, 
                                 levels=c("Aug","Sep","Oct","Nov","Dec","Jan","Feb",
                                          "Mar","Apr","May","Jun","Jul"))
CHN_Chipps_FLraw$YEAR <- as.numeric(CHN_Chipps_FLraw$YEAR)
CHN_Chipps_FLraw$FieldYear <- ifelse (CHN_Chipps_FLraw$MONTH %in% c("Aug","Sep","Oct",
                                                                    "Nov","Dec"), 
                                      CHN_Chipps_FLraw$YEAR+1, 
                                      CHN_Chipps_FLraw$YEAR)
CHN_Chipps_FLraw$FieldYear <- as.factor(CHN_Chipps_FLraw$FieldYear) 
CHN_Chipps_FLraw$FieldYear <- factor(CHN_Chipps_FLraw$FieldYear, levels=FL_years)


## QC Datset and make corrections
CHN_Chipps_FL <- CHN_Chipps_FLraw %>% 
  mutate(MarkCode=ifelse(MarkCode %in% c("none", "None"),"None", "Marked")) %>%
  mutate(Race=case_when(RaceByTag=="NA_string" & MarkCode=="Marked"~"Unknown", 
                        RaceByTag=="NA_string" & MarkCode == "None" ~as.character(RaceByLength),  
                        !RaceByTag=="NA_string"~as.character(RaceByTag))) %>%
  select(SampleID, FieldYear, Race, MarkCode, ForkLength, StageCode, SumOfCatchCount) %>%
  filter(!Race %in% c("Unknown","Hybrid")) %>%
  filter(ForkLength>0) %>%
  filter(!is.na(FieldYear)) %>%
  uncount(weights=SumOfCatchCount, .remove=TRUE)%>% # This expands the dataset by removing the aggregated counts for each FL. So each fish is now in a seperate row. 
  add_count(FieldYear, MarkCode, name="N") # sample sizes of marked and unmarked chinook for each field year



####################RBT Data Manipulation#################################
####RBT Trawls#####
# Step 1. Combine Chinook Catch and CWT Datasets, Assign Race to fish-----------------------------------------------------------

## Add Sample Year to Catches so we can designate unknown origin Chinook in samples taken before 2008 field year
RBT_Trawl_Catch <- inner_join(RBT_Catch, TrawlEffortFieldyear, by="SampleID")

## QC Datset and make corrections
RBT_Trawl_Catch_Summary <- RBT_Trawl_Catch %>% 
  select(SampleID, FieldYear, OrganismCode, MarkCode, SumOfCatchCount) %>%
  mutate(MarkCode=ifelse(MarkCode %in% c("none", "None")|is.na(MarkCode),"None", "Marked")) %>%
  group_by(SampleID) %>%
  mutate(Total_RBT= sum(SumOfCatchCount)) %>%
  mutate(Marked_Count= sum(case_when(MarkCode %in% c("Marked")~SumOfCatchCount,
                                     MarkCode %in% c("None")~0))) %>%  #create count of marked individuals
  mutate(Wild= sum(case_when(MarkCode %in% c("None")~SumOfCatchCount,
                             MarkCode %in% c("Marked")~0))) %>%  #create count of marked individuals
  gather("Origin", "Count", 7:8) %>%
  select(SampleID, Total_RBT, OrganismCode, Origin, Count) %>%
  unique()

# Step 2. Combine catch and effort datasets, format, add zero catches, calculate CPUE -----------------------------------------------------------------------------------------------------
RBT_Trawl_CPUE <- left_join(sample_trawl_only, RBT_Trawl_Catch_Summary, by="SampleID") %>%
  select(SampleID, SampleDate, FieldYear, MONTH, DAY, Location, StationCode, Volume, 
         MethodCode, OrganismCode, Origin, Count) %>%
  filter(!is.na(Volume))

## Format Dataset
## Placeholder value of "RBT" for samples with no RBT:
RBT_Trawl_CPUE$OrganismCode[is.na(RBT_Trawl_CPUE$OrganismCode)] <- "RBT"
RBT_Trawl_CPUE$Origin[is.na(RBT_Trawl_CPUE$Origin)] <- "Wild"
RBT_Trawl_CPUE$Count[is.na(RBT_Trawl_CPUE$Count)] <- 0 
RBT_Trawl_CPUE$Origin <- as.factor(RBT_Trawl_CPUE$Origin)
RBT_Trawl_CPUE$SampleID <- as.factor(RBT_Trawl_CPUE$SampleID)
RBT_Trawl_CPUE$FieldYear <- as.factor(RBT_Trawl_CPUE$FieldYear)

## Add zeroes for samples with no catch
RBT_Trawl_CPUE <- RBT_Trawl_CPUE %>% 
  complete(nesting(SampleID, MONTH, DAY, FieldYear,Location, StationCode, Volume, 
                   MethodCode), OrganismCode, Origin, fill=list(Count=0)) %>%
  mutate(CPUE=Count/Volume) %>%
  unique()


# Step 3. Calculate Monthly Means for Regions for Current sampling year-------------------------------------------------------                           

### Monthly CPUE averages per region were calculated by averaging all samples taken at each station per month. Monthly station averages within each region
### were then averaged together. We found a strong correlation between the average CPUE values calculated this way versus
### the previous way that started by averaging by station per week, then region per week, then region per month. Our new simplified calculations
### streamlined the process and removed the need to create an All date table in Access 

RBT_Trawl_Monthly_Means <- RBT_Trawl_CPUE %>%
  group_by(FieldYear, MONTH, DAY, Location, StationCode, OrganismCode, Origin) %>% ##mean daily CPUE by Station
  summarise(CPUE_day_station=mean(CPUE, na.rm=TRUE),
            .groups="drop") %>%
  group_by(FieldYear, MONTH, DAY, Location, OrganismCode, Origin) %>% ##mean daily CPUE by site
  summarise(CPUE_day_site=mean(CPUE_day_station, na.rm=TRUE),
            .groups="drop") %>%
  group_by(FieldYear, MONTH, Location, OrganismCode, Origin) %>%  ##mean monthly CPUE by site
  summarise(AvgCPUE=mean(CPUE_day_site, na.rm=TRUE),
            .groups="drop") %>%
  filter(!is.nan(AvgCPUE))

## Monthly Means for Current Year
RBT_Trawl_Monthly_Means_CurrentYear <- RBT_Trawl_Monthly_Means %>%
  filter(FieldYear == fieldyear)


# Step 4. Calculate Annual Means of CPUE for Regions ------------------------------------------------------------------                           

RBT_Trawl_Annual_Means <- RBT_Trawl_Monthly_Means %>%
  group_by(FieldYear, Location, OrganismCode, Origin) %>%
  summarise(AvgCPUE=mean(AvgCPUE, na.rm=TRUE),
            .groups="drop")



####RBT SEINES#####
# Step 1. Combine Chinook Catch and CWT Datasets, Assign Race to fish-----------------------------------------------------------

## Add Sample Year to Catches so we can designate unknown origin Chinook in samples taken before 2008 field year
RBT_SEIN_Catch <- inner_join(RBT_Catch, SEINEffortFieldYear, by="SampleID")

## QC Datset and make corrections
RBT_SEIN_Catch_Summary <- RBT_SEIN_Catch %>% 
  select(SampleID, FieldYear, OrganismCode, MarkCode, SumOfCatchCount) %>%
  mutate(MarkCode=ifelse(MarkCode %in% c("none", "None")|is.na(MarkCode), 
                         "None", "Marked")) %>%
  group_by(SampleID) %>%
  mutate(Total_RBT=sum(SumOfCatchCount)) %>%
  mutate(Marked_Count=sum(case_when(MarkCode %in% c("Marked")~SumOfCatchCount,
                                    MarkCode %in% c("None")~0))) %>%  #create count of marked individuals
  mutate(Unmarked=sum(case_when(MarkCode %in% c("None")~SumOfCatchCount,
                            MarkCode %in% c("Marked")~0))) %>%  #create count of marked individuals
  gather("Origin", "Count", 7:8) %>%
  select(SampleID, Total_RBT, OrganismCode, Origin, Count) %>%
  unique()


# Step 2. Combine catch and effort datasets, format, add zero catches, calculate CPUE -----------------------------------------------------------------------------------------------------
RBT_SEIN_CPUE <- left_join(sample_seine_only, RBT_SEIN_Catch_Summary, by="SampleID") %>%
  select(SampleID, FieldYear, MONTH, DAY, Location, RegionCode, StationCode, 
         Volume, MethodCode, OrganismCode, Origin, Count) %>%
  filter(!is.na(Volume))

## Format Dataset
## Placeholder value of "RBT" for samples with no RBT:
RBT_SEIN_CPUE$OrganismCode[is.na(RBT_SEIN_CPUE$OrganismCode)] <- "RBT"
RBT_SEIN_CPUE$Origin[is.na(RBT_SEIN_CPUE$Origin)] <- "Wild"
RBT_SEIN_CPUE$Count[is.na(RBT_SEIN_CPUE$Count)] <- 0 
RBT_SEIN_CPUE$Origin <- as.factor(RBT_SEIN_CPUE$Origin)
RBT_SEIN_CPUE$SampleID <- as.factor(RBT_SEIN_CPUE$SampleID)
RBT_SEIN_CPUE$FieldYear <- as.factor(RBT_SEIN_CPUE$FieldYear)

## Add zeroes for samples with no catch
RBT_SEIN_CPUE <- RBT_SEIN_CPUE %>% 
  complete(nesting(SampleID, MONTH, DAY, FieldYear,Location, RegionCode, 
                   StationCode, Volume, MethodCode), 
           OrganismCode, Origin, fill=list(Count=0)) %>%
  mutate(CPUE=Count/Volume) %>%
  unique()


# Step 3. Calculate Monthly Means for Regions for Current sampling year-------------------------------------------------------                           

### Monthly CPUE averages per region were calculated by averaging all samples taken at each station per month. Monthly station averages within each region
### were then averaged together. We found a strong correlation between the average CPUE values calculated this way versus
### the previous way that started by averaging by station per week, then region per week, then region per month. Our new simplified calculations
### streamlined the process and removed the need to create an All date table in Access 

RBT_SEIN_Monthly_Means <- RBT_SEIN_CPUE %>%
  group_by(FieldYear, MONTH, RegionCode, StationCode, OrganismCode, Origin) %>% ##mean monthly CPUE by Station
  summarise(CPUE_month_station=mean(CPUE, na.rm=TRUE),
            .groups="drop") %>%
  group_by(FieldYear, MONTH, RegionCode, OrganismCode, Origin) %>%  ##mean monthly CPUE by Region
  summarise(AvgCPUE=mean(CPUE_month_station, na.rm=TRUE),
            .groups="drop") %>%
  filter(!is.nan(AvgCPUE))

## Monthly Means for Current Year
RBT_SEIN_Monthly_Means_CurrentYear <- RBT_SEIN_Monthly_Means %>%
  filter(FieldYear == fieldyear)


# Step 4. Calculate Annual Means of CPUE for Regions ------------------------------------------------------------------                           

RBT_SEIN_Annual_Means <- RBT_SEIN_Monthly_Means %>%
  group_by(FieldYear, RegionCode, OrganismCode, Origin) %>%
  summarise(AvgCPUE=mean(AvgCPUE, na.rm=TRUE),
            .groups="drop")



####Annual RBT FL Distribution at Chipps#######################
RBT_Chipps_FLraw <- inner_join(subset(sample_trawl_only, Location %in% c("Chipps Island")), 
                               RBT_FL, by="SampleID")

RBT_Chipps_FLraw <- RBT_Chipps_FLraw %>% 
  select(SampleID,SampleDate, FieldYear, MONTH, DAY, OrganismCode, MarkCode, ForkLength, StageCode, 
         SumOfCatchCount) 

RBT_Chipps_FLraw$SumOfCatchCount <- as.numeric(as.character(RBT_Chipps_FLraw$SumOfCatchCount))####Have to properly convert counts from factor to numeric variable

RBT_Chipps_FLraw$MONTH <- factor(RBT_Chipps_FLraw$MONTH, 
                                 levels=c("Aug","Sep","Oct","Nov","Dec","Jan","Feb",
                                          "Mar","Apr","May","Jun","Jul"))
RBT_Chipps_FLraw$YEAR <- as.numeric(year(RBT_Chipps_FLraw$SampleDate))
RBT_Chipps_FLraw$FieldYear <- ifelse(RBT_Chipps_FLraw$MONTH %in% c("Aug","Sep","Oct","Nov","Dec"), 
                                     RBT_Chipps_FLraw$YEAR+1, RBT_Chipps_FLraw$YEAR)
RBT_Chipps_FLraw$FieldYear <- as.factor(RBT_Chipps_FLraw$FieldYear) 
RBT_Chipps_FLraw$FieldYear <- factor(RBT_Chipps_FLraw$FieldYear, levels=FL_years)


## QC Datset and make corrections
RBT_Chipps_FL <- RBT_Chipps_FLraw %>% 
  mutate(MarkCode=ifelse(MarkCode %in% c("none", "None"),"None", "Marked")) %>%
  select(SampleID, YEAR, FieldYear, MarkCode, ForkLength, StageCode, SumOfCatchCount) %>%
  filter(ForkLength > 0 & ForkLength < 350) %>%
  filter(!is.na(FieldYear)) %>%
  uncount(weights=SumOfCatchCount, .remove=TRUE) %>% # This expands the dataset by removing the aggregated counts for each FL. So each fish is now in a seperate row. 
  add_count(FieldYear, MarkCode, name="N") # sample sizes of marked and unmarked chinook for each field year



###################Write CSV###############################################

# Write CSV for final index output

this_out_root <- file.path("2026_R_code", "Salmon and Nearshore reports", "Data",fieldyear)

write.csv(CHNcustom_Trawl_Monthly_Means_CurrentYear, 
          file=file.path(this_out_root,"CHNcustom_Trawl_Monthly_Means_CurrentYear_LY.csv"), 
          row.names=FALSE)
write.csv(CHNcustom_Trawl_Annual_Means, 
          file=file.path(this_out_root,"CHNcustom_Trawl_Annual_Means_LY.csv"), 
          row.names=FALSE)

write.csv(CHNcustom_SEIN_Monthly_Means_CurrentYear, 
          file=file.path(this_out_root,"CHNcustom_SEIN_Monthly_Means_CurrentYear_LY.csv"), 
          row.names=FALSE)
write.csv(CHNcustom_SEIN_Annual_Means, 
          file=file.path(this_out_root,"CHNcustom_SEIN_Annual_Means_LY.csv"), 
          row.names=FALSE)

write.csv(RBT_Trawl_Monthly_Means_CurrentYear, 
          file=file.path(this_out_root,"RBT_Trawl_Monthly_Means_CurrentYear_LY.csv"), 
          row.names=FALSE)
write.csv(RBT_Trawl_Annual_Means, 
          file=file.path(this_out_root,"RBT_Trawl_Annual_Means_LY.csv"), row.names=FALSE)

write.csv(RBT_SEIN_Monthly_Means_CurrentYear, 
          file=file.path(this_out_root,"RBT_SEIN_Monthly_Means_CurrentYear_LY.csv"), 
          row.names=FALSE)
write.csv(RBT_SEIN_Annual_Means,
          file=file.path(this_out_root,"RBT_SEIN_Annual_Means_LY.csv"), row.names=FALSE)

write.csv(CHN_Chipps_FL, 
          file=file.path(this_out_root,"CHN_Chipps_FL_LY.csv"), row.names=FALSE)
write.csv(RBT_Chipps_FL, 
          file=file.path(this_out_root,"RBT_Chipps_FL_LY.csv"), row.names=FALSE)


