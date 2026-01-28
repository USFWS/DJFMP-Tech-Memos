# DJFMP Annual Report- Resident Fishes CPUE calculations and Figures----------------------------------------

### Author- Ryan Mckenzie
### Contact- ryan_mckenzie@fws.gov
### Updated - Christian Gredzens
### Contact - christian_gredzens@fws.gov
### Date- 5/24/23
### R version- 4.3.0
### Updates to code:
###         5/22/22: Added overall mean line to figures. Created successful/missed seine figure.
###         6/14/2022: Removed Liberty Island Seines from dataset
###         5/24/2023: Updated server link; updated database fields that had been changed 
###                    in the Denver Server; updated deprecated ggplot code; "CatchCount" field 
###                    changed to "Count" in Denver Database (gives total # of fish caught 
###                    [measure + not measured])


# Setting dates for the sample queries
calendaryear <- 2025
date_start <- "1995-12-31"
date_end <- "2025-07-31"

# DELTA Species of interest (limited to 6)
species1 <- "STB"
species2 <- "GSN" 
species3 <- "RSN" 
species4 <- "SHM"
species5 <- "CSG"
species6 <- "LP"

# Bay Species of interest (limit to 2)
bay1 <- "DMT"
bay2 <- "DSP"



#length at maturity (LAM), need to look up in literature. Try and find fork length measures.
species1_LAM <- 350
species2_LAM <- 69
species3_LAM <- 29
species4_LAM <- 48
species5_LAM <- 27
species6_LAM <- 100
bay1_LAM <- 179
bay2_LAM <- 29


# Packages ----------------------------------------------------------------

if(!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if(!require('grid')) install.packages('grid'); library('grid')
if(!require('gridExtra')) install.packages('gridExtra'); library('gridExtra')
if(!require('lubridate')) install.packages('lubridate'); library('lubridate')
if(!require('odbc')) install.packages('odbc'); library('odbc') #database functions
if(!require('stringr')) install.packages('stringr'); library('stringr') #database functions
if(!require('ggpubr')) install.packages('ggpubr'); library('ggpubr') #database functions
if(!require('plotrix')) install.packages('plotrix'); library('plotrix') # standard error calculations
options(scipen=999) # Getting rid of scientific notation
windowsFonts(Arial=windowsFont("Arial"))


# Functions ---------------------------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))
#A custom function to perform the inverse of %in%.


# Import Data------------------------------------------------

DJFMP_Database_Con <- odbc::dbConnect(...)

# Importing CDFW Subarea data to link with sample data below for Splittail Index
Subareas <- read.csv("Data/CDFW_Subregions.csv")
head(Subareas)
str(Subareas)

# Importing sample data via link with access
Sample_Query <- gsub("[\r\n]"," ", sprintf("SELECT * FROM Sample WHERE SampleDate BETWEEN '%s' AND '%s';",
                                           date_start, date_end))
Sample_Table <- DBI::dbGetQuery(conn=DJFMP_Database_Con, statement=Sample_Query)

#seine regions
station_code_table <- "SELECT * FROM ref_station_code;"
Stations <- DBI::dbGetQuery(conn=DJFMP_Database_Con, statement=station_code_table)
Stations <- Stations %>%
  mutate(StationCode=station_code)

# Importing catch data
Catch_Query <- "SELECT * FROM Catch;"
Catch_Table <- DBI::dbGetQuery(conn=DJFMP_Database_Con, statement=Catch_Query)

# Import Common Names of Species
Species_Query <- ("SELECT * FROM ref_organism_code;")
Species_Table <- DBI::dbGetQuery(conn=DJFMP_Database_Con, statement=Species_Query)

Species_Table$CommonName <- str_to_title(Species_Table$CommonName)
Species_Table$OrganismCode <- Species_Table$organism_code
Species_Table$CommonName <- gsub("Splittail", "Sacramento Splittail", Species_Table$CommonName)


dbDisconnect(DJFMP_Database_Con)

# Seines -------------------------------------------------------------------

Sample_Table_Final <- Sample_Table %>%
  select(-c(PermanentRecord, GearCode, WeatherCode, DO, WaterTemp, Turbidity, Secchi, 
            WaterVelocity, StreamGauge, SpecificConductance, Tide, UserName, SubstrateCode, 
            MeterSerialNo, YsiSerialNo, HatchSerialNo, VelocitySerialNo, TowNumber, 
            SamplingDirection, FlowmeterStart, FlowmeterEnd, FlowmeterDifference, 
            TowDuration, Study, StationCodeRelease, DrainageCode, Comments, isSite, 
            FlowDebris, updated_by, updated_on, created_by, created_on, crew_name)) %>% 
  mutate(SeineVolume=(SeineLength * SeineWidth * SeineDepth)/2,
         Year=lubridate::year(SampleDate),
         Month=lubridate::month(SampleDate)) %>% 
  filter(MethodCode == "SEIN", 
         GearConditionCode %in% c("1","2"), 
         !is.na(SeineVolume), 
         StationCode %!in% c("CR005S", "SR060AH", "SR144AH", "OR001X", "PR001W", 
															"VC002N", "TC002E", "SP000E", "SJ063E", "SJ065W", "SJ070N", 
															"SJ072E", "SJ075E", "SJ087W", "SR015E", "SR163W", "SR184E", 
															"SR119E", "SJ074A", "SJ076W", "SJ079E", "SR055AH", "LI001E", 
															"LI001W", "LI002E", "LI002W", "LI003E", "LI003W", "LI004E", 
															"LI004W", "LI005E", "LI005W", "LI006E", "LI006W", "LI007E", 
															"LI007W", "LI008E", "LI008W", "LI009E", "LI009W", "LI010E", 
															"LI010W", "LI011E","LI011W"),
         SeineLength >= 2 & SeineLength <= 15 & 
           SeineWidth >= 3 & SeineWidth <= 15 & 
           SeineDepth >= 0.2 & SeineDepth <= 1.5) %>% 
  left_join(Subareas, by="StationCode")%>%
  left_join(Stations[ ,c("StationCode","RegionCode")], by="StationCode")%>%
  mutate(RegionCodeName= case_when(RegionCode %in% c("1") ~ "SAC River",
                                   RegionCode %in% c("2") ~ "North Delta",
                                   RegionCode %in% c("3") ~ "Central Delta",
                                   RegionCode %in% c("4") ~ "South Delta",
                                   RegionCode %in% c("5") ~ "SJ River",
                                   RegionCode %in% c("6") ~ "San Francisco Bay"))

str(Sample_Table_Final)
head(Sample_Table_Final)
Sample_Table_Final$Year <- as.factor(Sample_Table_Final$Year)
Sample_Table_Final$Month <- as.factor(Sample_Table_Final$Month)
Sample_Table_Final$RegionCodeName <- factor(Sample_Table_Final$RegionCodeName, 
                                            levels=c("SAC River", "North Delta", 
                                                     "Central Delta", "South Delta", 
                                                     "SJ River", "San Francisco Bay"))

# Seines Temp Salinity -------------------------------------------------------------------

Sample_Temp_Salinity_Final <- Sample_Table %>%
  select(-c(PermanentRecord, GearCode, WeatherCode, DO, Turbidity, Secchi, WaterVelocity, 
            StreamGauge, Tide, UserName, SubstrateCode, MeterSerialNo, YsiSerialNo, 
            HatchSerialNo, VelocitySerialNo, TowNumber, SamplingDirection, FlowmeterStart, 
            FlowmeterEnd, FlowmeterDifference,  TowDuration, Study, StationCodeRelease, 
            DrainageCode, Comments, isSite, FlowDebris, updated_by, updated_on, 
            created_by, created_on, crew_name)) %>% 
  mutate(Year=lubridate::year(SampleDate),
         Month=lubridate::month(SampleDate)) %>% 
  filter(MethodCode == "SEIN", GearConditionCode %in% c("1","2"),
         StationCode %!in% c("CR005S", "SR060AH", "SR144AH", "OR001X", "PR001W", "VC002N",
                             "TC002E", "SP000E", "SJ063E", "SJ065W", "SJ070N", "SJ072E", 
                             "SJ075E", "SJ087W", "SR015E", "SR163W", "SR184E", "SR119E",
                             "SJ074A", "SJ076W", "SJ079E", "SR055AH")) %>% 
  left_join(Stations[,c("StationCode","RegionCode")], by="StationCode")%>%
  mutate(RegionCodeName= case_when(RegionCode %in% c("1") ~ "SAC River",
                                   RegionCode %in% c("2") ~ "North Delta",
                                   RegionCode %in% c("3") ~ "Central Delta",
                                   RegionCode %in% c("4") ~ "South Delta",
                                   RegionCode %in% c("5") ~ "SJ River",
                                   RegionCode %in% c("6") ~ "San Francisco Bay"))

Sample_Temp_Salinity_Final$Year <- as.factor(Sample_Temp_Salinity_Final$Year)
Sample_Temp_Salinity_Final$Month <- as.factor(Sample_Temp_Salinity_Final$Month)
Sample_Temp_Salinity_Final$RegionCodeName <- factor(Sample_Temp_Salinity_Final$RegionCodeName,
                                                    levels=c("SAC River","North Delta", 
                                                             "Central Delta","South Delta", 
                                                             "SJ River", "San Francisco Bay"))

SpringSummerTemps <- Sample_Temp_Salinity_Final %>%
  select(SampleID, Year, Month, StationCode, WaterTemp, RegionCode, RegionCodeName) %>% 
  drop_na(WaterTemp) %>%
  filter(Month %in% c("3","4","5","6","7","8"))%>%
  group_by(RegionCodeName, Year, Month, StationCode) %>% 
  summarize(WaterTemp=mean(WaterTemp),
            .groups="drop") %>%
  group_by(RegionCodeName, Year, Month) %>%
  summarize(WaterTemp=mean(WaterTemp),
            .groups="drop") %>%
  group_by(RegionCodeName, Year) %>%
  summarize(WaterTemp=mean(WaterTemp),
            .groups="drop")

FallWinterTemps <- Sample_Temp_Salinity_Final %>% 
  select(SampleID, Year, Month, StationCode, WaterTemp, RegionCode, RegionCodeName)%>%
  drop_na(WaterTemp)%>%
  filter(Month %in% c("9","10","11","12","1","2"))%>%
  group_by(RegionCodeName,Year, Month, StationCode) %>% 
  summarize(WaterTemp=mean(WaterTemp),
            .groups="drop") %>%
  group_by(RegionCodeName, Year, Month) %>%
  summarize(WaterTemp=mean(WaterTemp),
            .groups="drop") %>%
  group_by(RegionCodeName, Year) %>%
  summarize(WaterTemp=mean(WaterTemp),
            .groups="drop")

#Successful/Non Successful Samples-----------------
summary(as.factor(Sample_Table$GearConditionCode))

SeineSamples <- Sample_Table %>% 
  select(-c(PermanentRecord, GearCode, WeatherCode, DO, Turbidity, Secchi, WaterVelocity, 
            StreamGauge, Tide, UserName, SubstrateCode, MeterSerialNo, YsiSerialNo, 
            HatchSerialNo, VelocitySerialNo, TowNumber, SamplingDirection, FlowmeterStart, 
            FlowmeterEnd, FlowmeterDifference,  TowDuration, Study, StationCodeRelease, 
            DrainageCode, Comments, isSite, FlowDebris, updated_by, updated_on, 
            created_by, created_on, crew_name)) %>% 
  mutate(Year=lubridate::year(SampleDate),
         Month=lubridate::month(SampleDate)) %>% 
  filter(MethodCode == "SEIN",
         StationCode %!in% c("CR005S", "SR060AH", "SR144AH", "OR001X", "PR001W", "VC002N", 
                             "TC002E", "SP000E", "SJ063E", "SJ065W", "SJ070N", "SJ072E", 
                             "SJ075E", "SJ087W", "SR015E", "SR163W", "SR184E", "SR119E",
                             "SJ074A", "SJ076W", "SJ079E", "SR055AH")) %>% 
  filter(Year > 2005) %>% #DJFMP Started recording code 4s in 2006
  left_join(Stations[,c("StationCode","RegionCode")], by="StationCode") %>%
  filter(RegionCode %in% c("1","2","3","4","5","6")) %>%
  mutate(RegionCodeName= case_when(RegionCode %in% c("1") ~ "SAC River",
                                   RegionCode %in% c("2") ~ "North Delta",
                                   RegionCode %in% c("3") ~ "Central Delta",
                                   RegionCode %in% c("4") ~ "South Delta",
                                   RegionCode %in% c("5") ~ "SJ River",
                                   RegionCode %in% c("6") ~ "San Francisco Bay")) %>%
  select(Year,RegionCodeName, SampleID,GearConditionCode) %>%
  mutate(SampleOutcome=case_when(GearConditionCode %in% c("1","2","3") ~ "Completed", 
                                 GearConditionCode %in% c("4") ~ "Missed"),
         SampleCount=1) %>%
  group_by(RegionCodeName, Year, SampleOutcome) %>% 
  summarize(SampleCount=sum(SampleCount),
            .groups="drop") %>%
  drop_na() %>%
  as.data.frame()

SamplesSpread <- spread(SeineSamples, SampleOutcome, SampleCount, fill=0) %>%
  mutate(PercentCompleted=(Completed/(Completed+Missed))*100) %>%
  select(RegionCodeName, Year, PercentCompleted)

SeineSamples <- left_join(SeineSamples, SamplesSpread, by=c("RegionCodeName","Year"))

SeineSamples$Year <- as.factor(SeineSamples$Year)
SeineSamples$Outcome <- as.factor(SeineSamples$Year)

#Fig------------------

png(paste0("Figures/","CompleteSamples.png"), width=5, height=9, units="in", pointsize=12, res=300)
ggplot(data=SeineSamples, aes(x=Year, y=SampleCount, fill=SampleOutcome)) +
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(values=c("#000000", "#DEDCDB")) +
  scale_y_continuous(name="Sample Count", expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0.5), breaks=seq(2006, calendaryear, 4)) +
  theme_classic(base_size=10) + 
  theme(axis.text=element_text(colour="black", family="Arial")) +
  theme(axis.line=element_line(colour="black", linewidth=0.65)) + 
  theme(axis.ticks.y=element_line(linewidth=0.65)) + #
  theme(plot.title=element_text(face="bold", hjust=0.5, family="Arial")) +
  theme(axis.title=element_text(family="Arial")) +
  theme(legend.title=element_blank(),
        legend.position="bottom") +
  theme(panel.spacing.y=unit(1, "lines")) +
  facet_grid(RegionCodeName ~ .)
dev.off()

#Line graph showing successful seines as a percentage, not currently used.
#Percent<-ggplot(data=SamplesSpread, aes(x=Year, y=PercentCompleted)) +
# geom_line(size=1.25, color="black")+
# scale_y_continuous(name="Percent Completed",expand=c(0,0), breaks=seq(0, 100, 25), limits=c(0,100)) +
# scale_x_continuous(breaks=seq(1995, calendaryear, 5)) +
# theme(axis.text=element_text(colour="black", family="Arial")) +
#  theme(axis.line=element_line(colour="black", size=.65)) +
#theme(axis.ticks.y=element_line(size=.65)) +
#theme(plot.title=element_text(face="bold", hjust=0.5, family="Arial")) +
#theme(axis.title=element_text(family="Arial")) +
#theme(legend.title=element_text(family="Arial")) +
#theme(panel.spacing.y=unit(1, "lines")) +
#facet_grid(RegionCodeName~.)

# _______Catch Data______ --------------------------------------

Catch_Table_Final <- Catch_Table %>% 
  select(-c(PermanentRecord, Weight, SpecialStudyID, MarkCode, StageCode, Dead, 
            CountRef, Expression, RaceByLength, TagCode, updated_by, updated_on, 
            created_by, created_on, SpecialStudy, RaceByTag, catchSequence)) %>% 
  filter(OrganismCode %in% c(species1, species2, species3, species4, species5, species6,
                             bay1, bay2))

#Check to make sure species of interest have been filtered out into the dataset
levels(factor(Catch_Table_Final$OrganismCode))

Catch_Table_All_Sp_Final <- Catch_Table %>% 
  select(-c(PermanentRecord, Weight, SpecialStudyID, MarkCode, StageCode, Dead, 
            CountRef, Expression, RaceByLength, TagCode, updated_by, updated_on, 
            created_by, created_on, SpecialStudy, RaceByTag, catchSequence))

# Fork Length Distribution Table and Figure---------------------
#Please QC the min and max lengths for each species. I have caught mistakes in data input which can make these inaccurate--e.g. 600mm for MSS instead of 60mm.

Length_data <- Catch_Table_Final %>%
  uncount(weights=Count, .remove=TRUE) %>% 
  left_join(Sample_Table_Final, by="SampleID") %>%
  filter(MethodCode == "SEIN", ForkLength > 10) %>%
  left_join(Species_Table, by="OrganismCode") %>%
  group_by(OrganismCode) %>%
  mutate(Length_at_maturity=case_when(OrganismCode == species1 ~ species1_LAM,
                                      OrganismCode == species2 ~ species2_LAM,
                                      OrganismCode == species3 ~ species3_LAM,
                                      OrganismCode == species4 ~ species4_LAM,
                                      OrganismCode == species5 ~ species5_LAM,
                                      OrganismCode == species6 ~ species6_LAM,
                                      OrganismCode == bay1 ~ bay1_LAM,
                                      OrganismCode == bay2 ~ bay2_LAM)) %>%
  mutate(Juvenile=ifelse(ForkLength < Length_at_maturity, 1, 0)) %>%
  mutate(Median=median(ForkLength), 
         Min_FL=min(ForkLength), 
         Max_FL=max(ForkLength), 
         Total_Measured=n(),
         Total_Juveniles=sum(Juvenile)) %>%
  mutate(Percent_Juveniles=Total_Juveniles/Total_Measured*100) %>%
  as.data.frame() %>%
  select(SampleID, CatchID, CommonName, ForkLength,Total_Measured, Percent_Juveniles, 
         Median, Min_FL, Max_FL)

#Check to see if any selected species are missing (there should be 8, if there are less, missing species were 
#likely not caught during seine sampling)
Length_data %>%
  group_by(CommonName) %>%
  summarize(no_rows=length(CommonName))

#export csv with data for Catch summary table in report
write.csv(Length_data %>%
            select(CommonName, Total_Measured, Percent_Juveniles, Median, Min_FL, Max_FL) %>%
            unique(), 
          file="Figures/Catch_Summary_Table.csv", row.names=FALSE)

#Reorder Common Names to ensure graphs are in correct order
Length_data$CommonName <- factor(Length_data$CommonName, 
                                 levels=c(Species_Table$CommonName[Species_Table$organism_code == species1], 
                                          Species_Table$CommonName[Species_Table$organism_code == species2],
                                          Species_Table$CommonName[Species_Table$organism_code == species3],
                                          Species_Table$CommonName[Species_Table$organism_code == species4],
                                          Species_Table$CommonName[Species_Table$organism_code == species5],
                                          Species_Table$CommonName[Species_Table$organism_code == species6],
                                          Species_Table$CommonName[Species_Table$organism_code == bay1],
                                          Species_Table$CommonName[Species_Table$organism_code == bay2]))
#Final Figure
png("Figures/Size_Distribution.png", width=7.5, height=4.5, units="in", pointsize=12, res=300)
par(family="Arial")
ggplot(Length_data, aes(x=ForkLength)) +
  geom_histogram(binwidth=1, colour="black") +
  scale_y_log10(expand=c(0,0)) +
  facet_wrap( ~ CommonName, ncol=4, scale="free_x") +
  geom_vline(aes(xintercept=Median), colour="orange", lwd=1) +
  geom_text(data=Length_data %>% 
              group_by(CommonName) %>% 
              summarise(n=n()), 
            aes(x=Inf, y=Inf, label=paste0("n=", n)), colour="black", 
            family="Arial", hjust=1, vjust=1.5, size=3) +
  xlab("Fork Length (mm)") +
  ylab(expression("Count"*" "*"("* "Log"[10]*")")) +
  theme_classic(base_size=12, base_family="Arial") + 
  theme(axis.text.x=element_text(colour="black", family="Arial", angle=45)) +
  theme(axis.text.y=element_text(colour="black", family="Arial")) +
  theme(axis.line=element_line(colour="black", linewidth=0.65)) +
  theme(axis.ticks=element_line(linewidth=0.85)) +
  theme(plot.title=element_text(face="bold", family="Arial")) +
  theme(axis.title=element_text(family="Arial")) +
  theme(plot.margin=margin(10, 10, 10, 10))
dev.off()

# _________species1 CPUE_________ --------------------------------------------------------

species1_Final <- Catch_Table_Final %>% 
  filter(OrganismCode == species1) %>% 
  group_by(OrganismCode, SampleID) %>% 
  summarise(TotalCatch=sum(Count),
            .groups="drop") %>%
  right_join(Sample_Table_Final, by="SampleID") %>% 
  replace_na(list(TotalCatch=0)) %>% 
  filter(RegionCode %in% c("1","2","3","4","5")) %>% # exclude bay seines from Delta species
  mutate(CPUE=TotalCatch/SeineVolume * 10000) %>% 
  group_by(Year, RegionCodeName, Month, StationCode) %>% 
  summarize(CPUE=mean(CPUE),
            .groups="drop") %>%
  group_by(Year, RegionCodeName, Month) %>%
  summarize(CPUE=mean(CPUE),
            .groups="drop") %>%
  group_by(Year, RegionCodeName) %>%
  summarize(FinalMeanCPUE=mean(CPUE),
            SEMeanCPUE=plotrix::std.error(CPUE),
            .groups="drop")
str(species1_Final)

sp1overallmean <- species1_Final %>%
  group_by(RegionCodeName) %>%
  summarize(CPUE=mean(FinalMeanCPUE))

# _________Figs__________ ------------------------------------------------

png(paste0("Figures/",species1,"_Fig.png"), width=3, height=4.5, units="in", pointsize=12, res=300)
par(family="Arial")
ggplot(data=species1_Final, aes(x=Year, y=FinalMeanCPUE )) +
  geom_bar(stat="identity", position=position_dodge(width=0.9), color="black") + 
  geom_errorbar(mapping=aes(ymin=(FinalMeanCPUE - SEMeanCPUE), 
                            ymax=(FinalMeanCPUE + SEMeanCPUE)), width=0.5) +
  labs(x= "", y=bquote('CPUE (fish/10,000' ~ 'm'^3*')')) +
  ggtitle(Species_Table$CommonName[Species_Table$organism_code == species1]) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.1)), breaks=scales::breaks_extended(n=3)) +
  scale_x_discrete(expand=c(0,0.5), breaks=seq(1995, calendaryear, 5)) +
  theme_classic(base_size=8) + 
  theme(axis.text=element_text(colour="black", family="Arial")) +
  theme(axis.line=element_line(colour="black", linewidth=0.65)) +
  theme(axis.ticks.y=element_line(linewidth=0.65)) +
  theme(plot.title=element_text(face="bold", hjust=0.5, family="Arial")) + 
  theme(axis.title=element_text(family="Arial")) +
  theme(legend.title=element_text(family="Arial")) +
  theme(panel.spacing.y=unit(1, "lines")) +
  facet_grid(RegionCodeName ~ .) +
  geom_hline(data=sp1overallmean, aes(yintercept=CPUE), color="orange", linewidth=1, alpha=0.5)
dev.off()


# _________species2 CPUE_________ --------------------------------------------------------

# Estimating relative abundance
species2_Final <- Catch_Table_Final %>% 
  filter(OrganismCode == species2) %>% 
  group_by(OrganismCode, SampleID) %>% 
  summarise(TotalCatch=sum(Count),
            .groups="drop") %>%
  right_join(Sample_Table_Final, by="SampleID") %>% 
  replace_na(list(TotalCatch=0)) %>% 
  filter(RegionCode %in% c("1","2","3","4","5")) %>% # exclude bay seines from Delta species
  mutate(CPUE=TotalCatch/SeineVolume * 10000) %>% 
  group_by(Year, RegionCodeName, Month, StationCode) %>% 
  summarize(CPUE=mean(CPUE),
            .groups="drop") %>%
  group_by(Year, RegionCodeName, Month) %>%
  summarize(CPUE=mean(CPUE),
            .groups="drop") %>%
  group_by(Year, RegionCodeName) %>%
  summarize(FinalMeanCPUE=mean(CPUE),
            SEMeanCPUE=plotrix::std.error(CPUE),
            .groups="drop")
str(species2_Final)

sp2overallmean <- species2_Final %>%
  group_by(RegionCodeName) %>%
  summarize(CPUE=mean(FinalMeanCPUE))


# _________Figs__________ ------------------------------------------------

png(paste0("Figures/",species2,"_Fig.png"), width=3, height=4.5, units="in", pointsize=12, res=300)
par(family="Arial")
ggplot(data=species2_Final, aes(x=Year, y=FinalMeanCPUE)) +
  geom_bar(stat="identity",position=position_dodge(width=0.9), color="black") + 
  geom_errorbar(mapping=aes(ymin=(FinalMeanCPUE - SEMeanCPUE), 
                            ymax=(FinalMeanCPUE + SEMeanCPUE)), width=0.5) + 
  labs(x= "", y=bquote('CPUE (fish/10,000' ~ 'm'^3*')')) +
  ggtitle(Species_Table$CommonName[Species_Table$organism_code==species2]) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.1)), breaks=scales::breaks_extended(n=3)) +
  scale_x_discrete(expand=c(0,0.5), breaks=seq(1995, calendaryear, 5)) +
  theme_classic(base_size=8) + 
  theme(axis.text=element_text(colour="black", family="Arial")) +
  theme(axis.line=element_line(colour="black", linewidth=0.65)) +
  theme(axis.ticks.y=element_line(linewidth=0.65)) +
  theme(plot.title=element_text(face="bold", hjust=0.5, family="Arial")) +
  theme(axis.title=element_text(family="Arial")) +
  theme(legend.title=element_text(family="Arial")) +
  theme(panel.spacing.y=unit(1, "lines")) +
  facet_grid(RegionCodeName ~ .) +
  geom_hline(data=sp2overallmean, aes(yintercept=CPUE), color="orange", linewidth=1, alpha=.5)
dev.off()


# _________species3 CPUE_________ --------------------------------------------------------

# Estimating relative abundance
species3_Final <- Catch_Table_Final %>% 
  filter(OrganismCode == species3) %>% 
  group_by(OrganismCode, SampleID) %>% 
  summarise(TotalCatch=sum(Count),
            .groups="drop") %>% 
  right_join(Sample_Table_Final, by="SampleID") %>% 
  replace_na(list(TotalCatch=0)) %>% 
  filter(RegionCode %in% c("1","2","3","4","5")) %>% # exclude bay seines from Delta species
  mutate(CPUE=TotalCatch/SeineVolume * 10000) %>% 
  group_by(Year, RegionCodeName, Month, StationCode) %>% 
  summarize(CPUE=mean(CPUE),
            .groups="drop") %>%
  group_by(Year, RegionCodeName, Month) %>%
  summarize(CPUE=mean(CPUE),
            .groups="drop") %>%
  group_by(Year, RegionCodeName) %>%
  summarize(FinalMeanCPUE=mean(CPUE),
            SEMeanCPUE=plotrix::std.error(CPUE),
            .groups="drop")
str(species3_Final)

sp3overallmean <- species3_Final %>%
  group_by(RegionCodeName) %>%
  summarize(CPUE=mean(FinalMeanCPUE))

# _________Figs__________ ------------------------------------------------

png(paste0("Figures/",species3,"_Fig.png"), width=3, height=4.5, units="in", pointsize=12, res=300)
par(family="Arial")
ggplot(data=species3_Final, aes(x=Year, y=FinalMeanCPUE)) +
  geom_bar(stat="identity", position=position_dodge(width=0.9), color="black") + 
  geom_errorbar(mapping=aes(ymin=(FinalMeanCPUE - SEMeanCPUE), 
                            ymax=(FinalMeanCPUE + SEMeanCPUE)), 
                width=0.5) + 
  labs(x= "",y=bquote('CPUE (fish/10,000' ~ 'm'^3*')')) +
  ggtitle(Species_Table$CommonName[Species_Table$organism_code==species3]) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.1)), breaks=scales::breaks_extended(n=3)) +
  scale_x_discrete(expand=c(0,0.5), breaks=seq(1995, calendaryear, 5)) +
  theme_classic(base_size=8) + 
  theme(axis.text=element_text(colour="black", family="Arial")) +
  theme(axis.line=element_line(colour="black", linewidth=0.65)) +
  theme(axis.ticks.y=element_line(linewidth=.65)) +
  theme(plot.title=element_text(face="bold", hjust=0.5, family="Arial")) +
  theme(axis.title=element_text(family="Arial")) +
  theme(legend.title=element_text(family="Arial")) +
  theme(panel.spacing.y=unit(1, "lines")) +
  facet_grid(RegionCodeName ~ .) +
  geom_hline(data=sp3overallmean, aes(yintercept=CPUE), color="orange", linewidth=1, alpha=0.5)
dev.off()


# _________species4 CPUE_________ --------------------------------------------------------

# Estimating relative abundance
species4_Final <- Catch_Table_Final %>% 
  filter(OrganismCode == species4) %>% 
  group_by(OrganismCode, SampleID) %>% 
  summarise(TotalCatch=sum(Count),
            .groups="drop") %>% 
  right_join(Sample_Table_Final, by="SampleID") %>% 
  replace_na(list(TotalCatch=0)) %>% 
  filter(RegionCode %in% c("1","2","3","4","5")) %>% # exclude bay seines from Delta species
  mutate(CPUE=TotalCatch/SeineVolume * 10000) %>% 
  group_by(Year, RegionCodeName, Month, StationCode) %>% 
  summarize(CPUE=mean(CPUE),
            .groups="drop") %>%
  group_by(Year, RegionCodeName, Month) %>%
  summarize(CPUE=mean(CPUE),
            .groups="drop") %>%
  group_by(Year, RegionCodeName) %>%
  summarize(FinalMeanCPUE=mean(CPUE),
            SEMeanCPUE=plotrix::std.error(CPUE),
            .groups="drop")
str(species4_Final)

sp4overallmean <- species4_Final %>%
  group_by(RegionCodeName) %>%
  summarize(CPUE=mean(FinalMeanCPUE))

# _________Figs__________ ------------------------------------------------

png(paste0("Figures/",species4,"_Fig.png"), width=3, height=4.5, units="in", pointsize=12, res=300)
par(family="Arial")
ggplot(data=species4_Final, aes(x=Year, y=FinalMeanCPUE)) +
  geom_bar(stat="identity",position=position_dodge(width=0.9), color="black") + 
  geom_errorbar(mapping=aes(ymin=(FinalMeanCPUE - SEMeanCPUE), 
                            ymax=(FinalMeanCPUE + SEMeanCPUE)), 
                width=0.5) + 
  labs(x= "",y=bquote('CPUE (fish/10,000' ~ 'm'^3*')')) +
  ggtitle(Species_Table$CommonName[Species_Table$organism_code==species4]) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.1)), breaks=scales::breaks_extended(n=3)) +
  scale_x_discrete(expand=c(0,0.5), breaks=seq(1995, calendaryear, 5)) +
  theme_classic(base_size=8) + 
  theme(axis.text=element_text(colour="black", family="Arial")) +
  theme(axis.line=element_line(colour="black", linewidth=0.65)) +
  theme(axis.ticks.y=element_line(linewidth=.65)) +
  theme(plot.title=element_text(face="bold", hjust=0.5, family="Arial")) +
  theme(axis.title=element_text(family="Arial")) +
  theme(legend.title=element_text(family="Arial")) +
  theme(panel.spacing.y=unit(1, "lines")) +
  facet_grid(RegionCodeName ~ .) +
  geom_hline(data=sp4overallmean, aes(yintercept=CPUE), color="orange", linewidth=1, alpha=0.5)
dev.off()


# _________species5 CPUE_________ --------------------------------------------------------

# Estimating relative abundance
species5_Final <- Catch_Table_Final %>% 
  filter(OrganismCode == species5) %>% 
  group_by(OrganismCode, SampleID) %>% 
  summarise(TotalCatch=sum(Count),
            .groups="drop") %>% 
  right_join(Sample_Table_Final, by="SampleID") %>% 
  replace_na(list(TotalCatch=0)) %>% 
  filter(RegionCode %in% c("1","2","3","4","5")) %>% # exclude bay seines from Delta species
  mutate(CPUE=TotalCatch/SeineVolume * 10000) %>% 
  group_by(Year, RegionCodeName, Month, StationCode) %>% 
  summarize(CPUE=mean(CPUE),
            .groups="drop") %>%
  group_by(Year, RegionCodeName, Month) %>%
  summarize(CPUE=mean(CPUE),
            .groups="drop") %>%
  group_by(Year, RegionCodeName) %>%
  summarize(FinalMeanCPUE=mean(CPUE),
            SEMeanCPUE=plotrix::std.error(CPUE),
            .groups="drop")
str(species5_Final)

sp5overallmean <- species5_Final %>%
  group_by(RegionCodeName) %>%
  summarize(CPUE=mean(FinalMeanCPUE))


# _________Figs__________ ------------------------------------------------

png(paste0("Figures/",species5,"_Fig.png"), width=3, height=4.5, units="in", pointsize=12, res=300)
par(family="Arial")
ggplot(data=species5_Final, aes(x=Year, y=FinalMeanCPUE)) +
  geom_bar(stat="identity",position=position_dodge(width=0.9), color="black") + 
  geom_errorbar(mapping=aes(ymin=(FinalMeanCPUE - SEMeanCPUE), 
                            ymax=(FinalMeanCPUE + SEMeanCPUE)), 
                width=0.5) + 
  labs(x= "",y=bquote('CPUE (fish/10,000' ~ 'm'^3*')')) +
  ggtitle(Species_Table$CommonName[Species_Table$organism_code==species5]) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.1)), breaks=scales::breaks_extended(n=3)) +
  scale_x_discrete(expand=c(0,0.5), breaks=seq(1995, calendaryear, 5)) +
  theme_classic(base_size=8) + 
  theme(axis.text=element_text(colour="black", family="Arial")) +
  theme(axis.line=element_line(colour="black", linewidth=0.65)) +
  theme(axis.ticks.y=element_line(linewidth=0.65)) +
  theme(plot.title=element_text(face="bold", hjust=0.5, family="Arial")) +
  theme(axis.title=element_text(family="Arial")) +
  theme(legend.title=element_text(family="Arial")) +
  theme(panel.spacing.y=unit(1, "lines")) +
  facet_grid(RegionCodeName ~ .) + 
  geom_hline(data=sp5overallmean, aes(yintercept=CPUE), color="orange", linewidth=1, alpha=0.5)
dev.off()


# _________species6 CPUE_________ --------------------------------------------------------

# Estimating relative abundance
species6_Final <- 
  Catch_Table_Final %>% 
  filter(OrganismCode == species6) %>% 
  group_by(OrganismCode, SampleID) %>% 
  summarise(TotalCatch=sum(Count),
            .groups="drop") %>% 
  right_join(Sample_Table_Final, by="SampleID") %>% 
  replace_na(list(TotalCatch=0)) %>% 
  filter(RegionCode %in% c("1","2","3","4","5")) %>% # exclude bay seines from Delta species
  mutate(CPUE=TotalCatch/SeineVolume * 10000) %>% 
  group_by(Year, RegionCodeName, Month, StationCode) %>% 
  summarize(CPUE=mean(CPUE),
            .groups="drop") %>%
  group_by(Year, RegionCodeName, Month) %>%
  summarize(CPUE=mean(CPUE),
            .groups="drop") %>%
  group_by(Year, RegionCodeName) %>%
  summarize(FinalMeanCPUE=mean(CPUE),
            SEMeanCPUE=plotrix::std.error(CPUE),
            .groups="drop")
str(species6_Final)

sp6overallmean <- species6_Final %>%
  group_by(RegionCodeName) %>%
  summarize(CPUE=mean(FinalMeanCPUE))

# _________Figs__________ ------------------------------------------------

png(paste0("Figures/",species6,"_Fig.png"), width=3, height=4.5, units="in", pointsize=12, res=300)
par(family="Arial")
ggplot(data=species6_Final, aes(x=Year, y=FinalMeanCPUE)) +
  geom_bar(stat="identity",position=position_dodge(width=0.9), color="black") + 
  geom_errorbar(mapping=aes(ymin=(FinalMeanCPUE - SEMeanCPUE), 
                            ymax=(FinalMeanCPUE + SEMeanCPUE)), 
                width=0.5) + 
  labs(x= "",y=bquote('CPUE (fish/10,000' ~ 'm'^3*')')) +
  ggtitle(Species_Table$CommonName[Species_Table$organism_code==species6]) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.1)), breaks=scales::breaks_extended(n=3)) +
  scale_x_discrete(expand=c(0,0.5), breaks=seq(1995, calendaryear, 5)) +
  theme_classic(base_size=8) + 
  theme(axis.text=element_text(colour="black", family="Arial")) +
  theme(axis.line=element_line(colour="black", linewidth=0.65)) +
  theme(axis.ticks.y=element_line(linewidth=0.65)) +
  theme(plot.title=element_text(face="bold", hjust=0.5, family="Arial")) +
  theme(axis.title=element_text(family="Arial")) +
  theme(legend.title=element_text(family="Arial")) +
  theme(panel.spacing.y=unit(1, "lines")) +
  facet_grid(RegionCodeName ~ .) +
  geom_hline(data=sp6overallmean, aes(yintercept=CPUE), color="orange", linewidth=1, alpha=0.5)
dev.off()


# _________bay1 CPUE_________ --------------------------------------------------------

# Estimating relative abundance
bay1_Final <- Catch_Table_Final %>% 
  filter(OrganismCode == bay1) %>% 
  group_by(OrganismCode, SampleID) %>% 
  summarise(TotalCatch=sum(Count),
            .groups="drop") %>% 
  right_join(Sample_Table_Final, by="SampleID") %>% 
  replace_na(list(TotalCatch=0)) %>% 
  filter(RegionCode %in% c("6")) %>% # only include bay seines
  mutate(CPUE=TotalCatch/SeineVolume * 10000) %>% 
  group_by(Year, RegionCodeName, Month, StationCode) %>% 
  summarize(CPUE=mean(CPUE),
            .groups="drop") %>%
  group_by(Year, RegionCodeName, Month) %>%
  summarize(CPUE=mean(CPUE),
            .groups="drop") %>%
  group_by(Year, RegionCodeName) %>%
  summarize(FinalMeanCPUE=mean(CPUE), 
            SEMeanCPUE=plotrix::std.error(CPUE), 
            .groups="drop")


bay1overallmean <- bay1_Final %>%
  group_by(RegionCodeName) %>%
  summarize(CPUE=mean(FinalMeanCPUE))

# _________Figs__________ ------------------------------------------------

png(paste0("Figures/",bay1,"_Fig.png"), width=3, height=2, units="in", pointsize=12, res=300)
par(family="Arial")
ggplot(data=bay1_Final, aes(x=Year, y=FinalMeanCPUE)) +
  geom_bar(stat="identity",position=position_dodge(width=0.9), color="black") + 
  geom_errorbar(mapping=aes(ymin=(FinalMeanCPUE - SEMeanCPUE), 
                            ymax=(FinalMeanCPUE + SEMeanCPUE)), 
                width=0.5) + 
  labs(x= "",y=bquote('CPUE (fish/10,000' ~ 'm'^3*')')) +
  ggtitle(Species_Table$CommonName[Species_Table$organism_code==bay1]) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.1)), breaks=scales::breaks_extended(n=3)) +
  scale_x_discrete(expand=c(0,0.5), breaks=seq(1995, calendaryear, 5)) +
  theme_classic(base_size=8) + 
  theme(axis.text=element_text(colour="black", family="Arial")) +
  theme(axis.line=element_line(colour="black", linewidth=0.65)) +
  theme(axis.ticks.y=element_line(linewidth=0.65)) +
  theme(plot.title=element_text(face="bold", hjust=0.5, family="Arial")) +
  theme(axis.title=element_text(family="Arial")) +
  theme(legend.title=element_text(family="Arial")) +
  theme(panel.spacing.y=unit(1, "lines")) +
  facet_grid(RegionCodeName ~ .) +
  geom_hline(data=bay1overallmean, aes(yintercept=CPUE), color="orange", linewidth=1,alpha=.5)
dev.off()


# _________bay2 CPUE_________ --------------------------------------------------------

# Estimating relative abundance
bay2_Final <- Catch_Table_Final %>% 
  filter(OrganismCode == bay2) %>% 
  group_by(OrganismCode, SampleID) %>% 
  summarise(TotalCatch=sum(Count),
            .groups="drop") %>% 
  right_join(Sample_Table_Final, by="SampleID") %>% 
  replace_na(list(TotalCatch=0)) %>% 
  filter(RegionCode %in% c("6")) %>% # only include bay seines
  mutate(CPUE=TotalCatch/SeineVolume * 10000) %>% 
  group_by(Year, RegionCodeName, Month, StationCode) %>% 
  summarize(CPUE=mean(CPUE),
            .groups="drop") %>%
  group_by(Year, RegionCodeName, Month) %>%
  summarize(CPUE=mean(CPUE),
            .groups="drop") %>%
  group_by(Year, RegionCodeName) %>%
  summarize(FinalMeanCPUE=mean(CPUE),
            SEMeanCPUE=plotrix::std.error(CPUE),
            .groups="drop")

bay2overallmean <- bay2_Final %>%
  group_by(RegionCodeName) %>%
  summarize(CPUE=mean(FinalMeanCPUE))

# _________Figs__________ ------------------------------------------------

png(paste0("Figures/",bay2,"_Fig.png"), width=3, height=2, units="in", pointsize=12, res=300)
par(family="Arial")
ggplot(data=bay2_Final, aes(x=Year, y=FinalMeanCPUE)) +
  geom_bar(stat="identity",position=position_dodge(width=0.9), color="black") +
  geom_errorbar(mapping=aes(ymin=(FinalMeanCPUE - SEMeanCPUE), 
                            ymax=(FinalMeanCPUE + SEMeanCPUE)), width=0.5) +
  labs(x= "",y=bquote('CPUE (fish/10,000' ~ 'm'^3*')')) +
  ggtitle(Species_Table$CommonName[Species_Table$organism_code==bay2]) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.1)), breaks=scales::breaks_extended(n=3)) +
  scale_x_discrete(expand=c(0,0.5), breaks=seq(1995, calendaryear, 5)) +
  theme_classic(base_size=8) + 
  theme(axis.text=element_text(colour="black", family="Arial")) +
  theme(axis.line=element_line(colour="black", linewidth=0.65)) +
  theme(axis.ticks.y=element_line(linewidth=0.65)) +
  theme(plot.title=element_text(face="bold", hjust=0.5, family="Arial")) +
  theme(axis.title=element_text(family="Arial")) +
  theme(legend.title=element_text(family="Arial")) +
  theme(panel.spacing.y=unit(1, "lines")) +
  facet_grid(RegionCodeName ~ .) +
  geom_hline(data=bay2overallmean, aes(yintercept=CPUE), color="orange", linewidth=1, alpha=0.5)
dev.off()

