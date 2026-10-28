##2026 USBR Technical Memo
##Jacob Stagg @ jacob_stagg@fws.gov
##20261016

# Library -----------------------------------------------------------------
library('odbc')
library('dplyr') 
library('tidyr') 
library('compareDF') 
library('lubridate')
library('stringr')
library('ggplot2')
library('ggthemes')
library('ggridges')
library('ggspatial')
library('ggpubr')
library('extrafont')
library('scales')
library('extrafont')
library('sf')
library("DBI")
library('ggbump')
library('patchwork')

# Utility objects -------------------------------------------------------------------

loadfonts(device="win")

report_theme <- theme_bw() + 
  theme(plot.title=element_text(colour="black", size=8),
        axis.text=element_text(colour="black", size=8),
        axis.title=element_text(colour="black", size=8),
        legend.title=element_text(colour="black", size=8),
        legend.text=element_text(colour="black", size=8),
        strip.text=element_text(colour="black", size=8),
        legend.box.spacing=margin(0,0,0,0),
        legend.margin=margin(0,0,0,0),
        legend.position="bottom")

# Read in data from EDI posting-------------------------------------------------------

EFISH_data <- read.csv(file.path("2021-2026_USFWS_EFISH_data.csv"), header=TRUE)

# Defining Field Year, Month, & Period for field years 2022-2026---------------------------------------------------

unique(EFISH_data$Subregion)

EFISH_working <- EFISH_data %>%
  dplyr::mutate(TransectID=paste0(SampleDate,Subregion,StartTime)) %>%
  dplyr::mutate(SampleDate=as.Date(SampleDate, "%m/%d/%Y"),
                Year=lubridate::year(SampleDate),
                MonthNum=lubridate::month(SampleDate),
                MonthAbb=lubridate::month(SampleDate, label=TRUE, abbr=TRUE), 
                Month_f=factor(MonthAbb, 
                               levels=c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", 
                                        "Mar","Apr","May", "Jun", "Jul" ))) %>%
  dplyr::mutate(FieldYear=ifelse(MonthNum %in% 8:12, Year + 1, Year), 
                FieldMonth_f=factor(MonthNum, 
                                    levels=c(8:12,1:7), 
                                    ordered=TRUE)) %>%
  mutate(Period=factor(case_when(MonthAbb %in% c("Jul","Aug","Sep") ~ "Q3", 
                                 MonthAbb %in% c("Oct","Nov","Dec") ~ "Q4",
                                 MonthAbb %in% c("Jan","Feb","Mar") ~ "Q1",
                                 MonthAbb %in% c("Apr","May","Jun") ~ "Q2"), 
                                 levels=c("Q1","Q2","Q3","Q4"))) %>%
  mutate(SubRegion=case_when(Subregion=="CLS"~"Cache Slough and Lindsey Slough",
                             Subregion=="LI"~"Liberty Island",
                             Subregion=="DSC"~"Sacramento Shipping Channel",
                             Subregion=="RIO"~"Rio Vista",
                             Subregion=="TWITCH"~"Twitchell Island",
                             Subregion=="FRT"~"Franks Tract",
                             Subregion=="SAC"~"Sacramento River",
                             Subregion=="MOK"~"Mokelumne River",
                             Subregion=="MINER"~"Miner and Steamboat Slough",
                             Subregion=="UPSJR"~"Upper San Joaquin River",
                             Subregion=="MOS"~"Mossdale",
                             Subregion=="GRANT"~"Grant Line Canal",
                             Subregion=="SJR"~"San Joaquin River",
                             Subregion=="HOL"~"Holland Cut",
                             Subregion=="MIL"~"Mildred Island",
                             Subregion=="LSAC"~"Lower Sacramento River",
                             Subregion=="LSJR"~"Lower San Joaquin River",
                             Subregion=="CON"~"Confluence")) %>%
  dplyr::filter(!FieldYear %in% c("2021"))

#Sampling tables check---------------------------

samples_by_2026regionmonth <- EFISH_working %>%
  dplyr::filter(FieldYear %in% c("2026")) %>%
  dplyr::distinct(RegionCode, MonthAbb, TransectID) %>%
  dplyr::group_by(RegionCode, MonthAbb) %>%
  dplyr::summarize(N=dplyr::n(), 
                   .groups="drop") %>%
  tidyr::pivot_wider(id_cols=RegionCode, 
                     names_from=MonthAbb, 
                     values_from=N,
                     values_fill=0)

write.csv(samples_by_2026regionmonth, file.path("FY_2026_sample_table.csv"),
          row.names=FALSE)


# Total Catch over FY 2022-2026 ------------------------------------------------

CPUE_Totals <- EFISH_working %>%
  dplyr::select(TransectID, FieldYear, SegmentNumber, SecondsPerSegment, 
                CommonName, OrganismCode, Count) %>%
  dplyr::mutate(CommonName=ifelse(OrganismCode == "LAMAM","Lamprey ammocete",
                                  CommonName)) %>%
  dplyr::filter(!is.na(SecondsPerSegment)) %>% 
  dplyr::filter(!CommonName %in% c("No Catch", "no catch")) %>%
  dplyr::group_by(TransectID, FieldYear, SegmentNumber, SecondsPerSegment, 
                  CommonName) %>%
  dplyr::summarize(TotalCount=sum(Count),
                   .groups="drop") %>%
  tidyr::pivot_wider(id_cols=c(TransectID, FieldYear, SegmentNumber, SecondsPerSegment),
                     names_from=CommonName,
                     values_from=TotalCount,
                     values_fill=0) %>%
  tidyr::pivot_longer(cols=-c(TransectID, FieldYear, SegmentNumber, SecondsPerSegment),
                      names_to="CommonName",
                      values_to="TotalCount") %>%
  dplyr::mutate(CPUE_per_minute=TotalCount/(SecondsPerSegment/60)) %>%
  dplyr::group_by(CommonName) %>%
  dplyr::summarize(TotalCount=sum(TotalCount),
                   MeanCPUE=mean(CPUE_per_minute),
                   SDCPUE=sd(CPUE_per_minute),
                   .groups="drop") %>%
  dplyr::mutate(PercentCatch=TotalCount/sum(TotalCount)*100)

# Catch for FY 2026 ------------------------------------------------------------
CPUE_2026 <- EFISH_working %>%
  dplyr::filter(FieldYear %in% c("2026")) %>%
  dplyr::select(TransectID, FieldYear, SegmentNumber, SecondsPerSegment, 
                CommonName, OrganismCode, Count) %>%
  dplyr::mutate(CommonName=ifelse(OrganismCode == "LAMAM","Lamprey ammocete",
                                  CommonName)) %>%
  dplyr::filter(!is.na(SecondsPerSegment)) %>% 
  dplyr::filter(!CommonName %in% c("No Catch", "no catch")) %>%
  dplyr::group_by(TransectID, FieldYear, SegmentNumber, SecondsPerSegment, 
                  CommonName) %>%
  dplyr::summarize(TotalCount=sum(Count),
                   .groups="drop") %>%
  tidyr::pivot_wider(id_cols=c(TransectID, FieldYear, SegmentNumber, SecondsPerSegment),
                     names_from=CommonName,
                     values_from=TotalCount,
                     values_fill=0) %>%
  tidyr::pivot_longer(cols=-c(TransectID, FieldYear, SegmentNumber, SecondsPerSegment),
                      names_to="CommonName",
                      values_to="TotalCount") %>%
  dplyr::mutate(CPUE_per_minute=TotalCount/(SecondsPerSegment/60)) %>%
  dplyr::group_by(CommonName) %>%
  dplyr::summarize(TotalCount=sum(TotalCount),
                   MeanCPUE=mean(CPUE_per_minute),
                   SDCPUE=sd(CPUE_per_minute),
                   .groups="drop") %>%
  dplyr::mutate(PercentCatch=TotalCount/sum(TotalCount)*100)

# Species of concern------------------------------------------------------------

Species_of_Concern_by_Fieldyear <- EFISH_working %>%
  dplyr::filter(CommonName %in% c("Splittail", "steelhead", "Lamprey unknown", "Longfin Smelt",
                                  "White Sturgeon", "Delta Smelt", "Chinook Salmon")) %>%
  dplyr::group_by(FieldYear, CommonName) %>%
  dplyr::summarize(N=sum(Count), 
                   .groups="drop") %>%
  tidyr::pivot_wider(id_cols=FieldYear, 
                     names_from=CommonName, 
                     values_from=N,
                     values_fill=0)

write.csv(Species_of_Concern_by_Fieldyear, file.path("SpfCon_table.csv"),
          row.names=FALSE)

# Salmon Breakdown--------------------------------------------------------------

Salmon_Breakdown <- EFISH_working %>%
  dplyr::filter(CommonName %in% c("Chinook Salmon")) %>%
  dplyr::group_by(FieldYear, RaceByLength) %>%
  dplyr::summarize(N=sum(Count), 
                   .groups="drop") %>%
  tidyr::pivot_wider(id_cols=FieldYear, 
                     names_from=RaceByLength, 
                     values_from=N,
                     values_fill=0)

CWT_Breakdown <- EFISH_working %>%
  dplyr::filter(MarkCode %in% c("AdClipped")) %>%
  dplyr::group_by(FieldYear, RaceByTag) %>%
  dplyr::summarize(N=sum(Count), 
                   .groups="drop") %>%
  tidyr::pivot_wider(id_cols=FieldYear, 
                     names_from=RaceByTag, 
                     values_from=N,
                     values_fill=0)

# Build group catch graph ------------------------------------------------------------

Top_Catch <- EFISH_working %>%
  dplyr::filter(SecondsPerSegment != "NA") %>%
  dplyr::filter(Count != "NA") %>%
  dplyr::group_by(RegionCode, SubRegion, TransectID, CommonName, SegmentNumber, SecondsPerSegment,
                  FieldYear, Month_f, Period) %>%
  dplyr::summarize(Count=sum(Count),
                   .groups="drop") %>%
  tidyr::pivot_wider(id_cols=c(RegionCode, SubRegion, TransectID, SegmentNumber, 
                               SecondsPerSegment, FieldYear, Month_f, Period), 
                     names_from=CommonName, 
                     values_from=Count, 
                     values_fill=0) %>%
  tidyr::pivot_longer(cols=!c(RegionCode, SubRegion, TransectID, SegmentNumber, 
                              SecondsPerSegment, FieldYear, Month_f, Period), 
                      names_to="CommonName", 
                      values_to="Count") %>%
  dplyr::mutate(CPUE_minute=Count/(SecondsPerSegment/60)) %>%
  dplyr::group_by(FieldYear, CommonName) %>%
  dplyr::summarize(Average_CPUE_minute=mean(CPUE_minute),
                   SDCPUE=sd(CPUE_minute), 
                   MaxCatch=max(Count), 
                   .groups="drop") %>%
  dplyr::filter(CommonName %in% c("Largemouth Bass", "Bluegill", "Redear Sunfish", "Threadfin Shad", 
                                  "Mississippi Silverside", "Hitch")) 

Year_Top_adj <- ggplot(Top_Catch, aes(x=FieldYear, y=sqrt(Average_CPUE_minute))) +
  geom_bar(stat = "identity") +
  stat_smooth(geom="line", method=lm, color="orange", linewidth=2, alpha=0.6, se=FALSE) +
  xlab("Field Year") +
  ylab("Square Root of Average Catch per Minute") +
  report_theme +
  facet_wrap( ~ CommonName, nrow=1)
Year_Top_adj


G2_Catch <- EFISH_working %>%
  dplyr::filter(SecondsPerSegment != "NA") %>%
  dplyr::filter(Count != "NA") %>%
  dplyr::group_by(RegionCode, SubRegion, TransectID, CommonName, SegmentNumber, SecondsPerSegment,
                  FieldYear, Month_f, Period) %>%
  dplyr::summarize(Count=sum(Count),
                   .groups="drop") %>%
  tidyr::pivot_wider(id_cols=c(RegionCode, SubRegion, TransectID, SegmentNumber, 
                               SecondsPerSegment, FieldYear, Month_f, Period), 
                     names_from=CommonName, 
                     values_from=Count, 
                     values_fill=0) %>%
  tidyr::pivot_longer(cols=!c(RegionCode, SubRegion, TransectID, SegmentNumber, 
                              SecondsPerSegment, FieldYear, Month_f, Period), 
                      names_to="CommonName", 
                      values_to="Count") %>%
  dplyr::mutate(CPUE_minute=Count/(SecondsPerSegment/60)) %>%
  dplyr::group_by(FieldYear, CommonName) %>%
  dplyr::summarize(Average_CPUE_minute=mean(CPUE_minute),
                   SDCPUE=sd(CPUE_minute), 
                   MaxCatch=max(Count), 
                   .groups="drop") %>%
  dplyr::filter(CommonName %in% c("Golden Shiner","Tule Perch", "Sacramento Sucker", "Sacramento Pikeminnow", 
                                  "Striped Bass", "Warmouth")) 

Year_G2_adj <- ggplot(G2_Catch, aes(x=FieldYear, y=sqrt(Average_CPUE_minute))) +
  geom_bar(stat = "identity") +
  stat_smooth(geom="line", method=lm, color="orange", linewidth=2, alpha=0.6, se=FALSE) +
  xlab("Field Year") +
  ylab("Square Root of Average Catch per Minute") +
  report_theme +
  facet_wrap( ~ CommonName, nrow=1)
Year_G2_adj


G3_Catch <- EFISH_working %>%
  dplyr::filter(SecondsPerSegment != "NA") %>%
  dplyr::filter(Count != "NA") %>%
  dplyr::group_by(RegionCode, SubRegion, TransectID, CommonName, SegmentNumber, SecondsPerSegment,
                  FieldYear, Month_f, Period) %>%
  dplyr::summarize(Count=sum(Count),
                   .groups="drop") %>%
  tidyr::pivot_wider(id_cols=c(RegionCode, SubRegion, TransectID, SegmentNumber, 
                               SecondsPerSegment, FieldYear, Month_f, Period), 
                     names_from=CommonName, 
                     values_from=Count, 
                     values_fill=0) %>%
  tidyr::pivot_longer(cols=!c(RegionCode, SubRegion, TransectID, SegmentNumber, 
                              SecondsPerSegment, FieldYear, Month_f, Period), 
                      names_to="CommonName", 
                      values_to="Count") %>%
  dplyr::mutate(CPUE_minute=Count/(SecondsPerSegment/60)) %>%
  dplyr::group_by(FieldYear, CommonName) %>%
  dplyr::summarize(Average_CPUE_minute=mean(CPUE_minute),
                   SDCPUE=sd(CPUE_minute), 
                   MaxCatch=max(Count), 
                   .groups="drop") %>%
  dplyr::filter(CommonName %in% c("Prickly Sculpin", "Common Carp", "Yellowfin Goby", "Splittail", 
                                  "White Catfish", "Bigscale Logperch")) 


Year_G3_adj <- ggplot(G3_Catch, aes(x=FieldYear, y=sqrt(Average_CPUE_minute))) +
  geom_bar(stat = "identity") +
  stat_smooth(geom="line", method=lm, color="orange", linewidth=2, alpha=0.6, se=FALSE) +
  xlab("Field Year") +
  ylab("Square Root of Average Catch per Minute") +
  report_theme +
  facet_wrap( ~ CommonName, nrow=1)
Year_G3_adj


G4_Catch <- EFISH_working %>%
  dplyr::filter(SecondsPerSegment != "NA") %>%
  dplyr::filter(Count != "NA") %>%
  dplyr::group_by(RegionCode, SubRegion, TransectID, CommonName, SegmentNumber, SecondsPerSegment,
                  FieldYear, Month_f, Period) %>%
  dplyr::summarize(Count=sum(Count),
                   .groups="drop") %>%
  tidyr::pivot_wider(id_cols=c(RegionCode, SubRegion, TransectID, SegmentNumber, 
                               SecondsPerSegment, FieldYear, Month_f, Period), 
                     names_from=CommonName, 
                     values_from=Count, 
                     values_fill=0) %>%
  tidyr::pivot_longer(cols=!c(RegionCode, SubRegion, TransectID, SegmentNumber, 
                              SecondsPerSegment, FieldYear, Month_f, Period), 
                      names_to="CommonName", 
                      values_to="Count") %>%
  dplyr::mutate(CPUE_minute=Count/(SecondsPerSegment/60)) %>%
  dplyr::group_by(FieldYear, CommonName) %>%
  dplyr::summarize(Average_CPUE_minute=mean(CPUE_minute),
                   SDCPUE=sd(CPUE_minute), 
                   MaxCatch=max(Count), 
                   .groups="drop") %>%
  dplyr::filter(CommonName %in% c("Rainwater Killifish", "Black Crappie", "Chinook Salmon",
                                  "American Shad", "Shimofuri Goby", "Spotted Bass")) 

Year_G4_adj <- ggplot(G4_Catch, aes(x=FieldYear, y=sqrt(Average_CPUE_minute))) +
  geom_bar(stat = "identity") +
  stat_smooth(geom="line", method=lm, color="orange", linewidth=2, alpha=0.6, se=FALSE) +
  xlab("Field Year") +
  ylab("Square Root of Average Catch per Minute") +
  report_theme +
  facet_wrap( ~ CommonName, nrow=1)
Year_G4_adj


G5_Catch <- EFISH_working %>%
  dplyr::filter(SecondsPerSegment != "NA") %>%
  dplyr::filter(Count != "NA") %>%
  dplyr::group_by(RegionCode, SubRegion, TransectID, CommonName, SegmentNumber, SecondsPerSegment,
                  FieldYear, Month_f, Period) %>%
  dplyr::summarize(Count=sum(Count),
                   .groups="drop") %>%
  tidyr::pivot_wider(id_cols=c(RegionCode, SubRegion, TransectID, SegmentNumber, 
                               SecondsPerSegment, FieldYear, Month_f, Period), 
                     names_from=CommonName, 
                     values_from=Count, 
                     values_fill=0) %>%
  tidyr::pivot_longer(cols=!c(RegionCode, SubRegion, TransectID, SegmentNumber, 
                              SecondsPerSegment, FieldYear, Month_f, Period), 
                      names_to="CommonName", 
                      values_to="Count") %>%
  dplyr::mutate(CPUE_minute=Count/(SecondsPerSegment/60)) %>%
  dplyr::group_by(FieldYear, CommonName) %>%
  dplyr::summarize(Average_CPUE_minute=mean(CPUE_minute),
                   SDCPUE=sd(CPUE_minute), 
                   MaxCatch=max(Count), 
                   .groups="drop") %>%
  dplyr::filter(CommonName %in% c("Bluefin Killifish", "Smallmouth Bass", "Red Shiner", "Brown Bullhead",
                                  "Black Bullhead", "Green Sunfish"))

Year_G5_adj <- ggplot(G5_Catch, aes(x=FieldYear, y=sqrt(Average_CPUE_minute))) +
  geom_bar(stat = "identity") +
  stat_smooth(geom="line", method=lm, color="orange", linewidth=2, alpha=0.6, se=FALSE) +
  xlab("Field Year") +
  ylab("Square Root of Average Catch per Minute") +
  report_theme +
  facet_wrap( ~ CommonName, nrow=1)
Year_G5_adj


G6_Catch <- EFISH_working %>%
  dplyr::filter(SecondsPerSegment != "NA") %>%
  dplyr::filter(Count != "NA") %>%
  dplyr::group_by(RegionCode, SubRegion, TransectID, CommonName, SegmentNumber, SecondsPerSegment,
                  FieldYear, Month_f, Period) %>%
  dplyr::summarize(Count=sum(Count),
                   .groups="drop") %>%
  tidyr::pivot_wider(id_cols=c(RegionCode, SubRegion, TransectID, SegmentNumber, 
                               SecondsPerSegment, FieldYear, Month_f, Period), 
                     names_from=CommonName, 
                     values_from=Count, 
                     values_fill=0) %>%
  tidyr::pivot_longer(cols=!c(RegionCode, SubRegion, TransectID, SegmentNumber, 
                              SecondsPerSegment, FieldYear, Month_f, Period), 
                      names_to="CommonName", 
                      values_to="Count") %>%
  dplyr::mutate(CPUE_minute=Count/(SecondsPerSegment/60)) %>%
  dplyr::group_by(FieldYear, CommonName) %>%
  dplyr::summarize(Average_CPUE_minute=mean(CPUE_minute),
                   SDCPUE=sd(CPUE_minute), 
                   MaxCatch=max(Count), 
                   .groups="drop") %>%
dplyr::filter(CommonName %in% c("Goldfish", "Redeye Bass", "Sacramento Blackfish", 
                                "Western Mosquitofish", "Wakasagi","Channel Catfish"))

Year_G6_adj <- ggplot(G6_Catch, aes(x=FieldYear, y=sqrt(Average_CPUE_minute))) +
  geom_bar(stat = "identity") +
  stat_smooth(geom="line", method=lm, color="orange", linewidth=2, alpha=0.6, se=FALSE) +  
  xlab("Field Year") +
  ylab("Square Root of Average Catch per Minute") +
  report_theme +
  facet_wrap( ~ CommonName, nrow=1)
Year_G6_adj


Catch_combined <- Year_Top_adj + 
Year_G2_adj + 
Year_G3_adj + Year_G4_adj + Year_G5_adj + 
Year_G6_adj + 
patchwork::plot_layout(nrow=6, ncol=1, axes="collect") + 
  xlab("Field Year") + 
  theme(plot.background=element_rect(fill="white", color=NA))
Catch_combined

ggsave(filename=file.path("EFISH_Catch_Rate.png"), 
       plot=Catch_combined, height=8, width=11, units="in") 

#Habitat @ Segment level---------------------------------------------------
Habitat <- EFISH_working %>%
  dplyr::select(TransectID, FieldYear, Month_f, Period, RegionCode, SubRegion, SegmentNumber, 
                SubstrateSegment, BankType, SAV, LWD, Floating, Emergent) %>%
  dplyr::distinct(TransectID, FieldYear, Month_f, Period, RegionCode, SubRegion, SegmentNumber, 
                  SubstrateSegment, BankType, SAV, LWD, Floating, Emergent) %>%
  dplyr::mutate(BankType_f=case_when(BankType=="1"~"Rip-rap",
                                     BankType=="2"~"Mud Bank",
                                     BankType=="3"~"Sand\nBeach",
                                     BankType=="4"~"Mud Flat",
                                     BankType=="5"~"Artifical\nStructure")) %>%
  dplyr::mutate(SAV_tmp=case_when(SAV=="0"~"0%",
                                  SAV=="1"~"<25%",
                                  SAV=="2"~"25-75%",
                                  SAV=="3"~">75%",
                                  SAV=="4"~"Overgrown"),
                SAV_f=factor(SAV_tmp, 
                             levels=c("0%","<25%","25-75%",">75%","Over-\ngrown"),
                             ordered=TRUE)) %>%
  dplyr::mutate(LWD_tmp=case_when(LWD=="0"~"0%",
                                  LWD=="1"~"25%",
                                  LWD=="2"~"50%",
                                  LWD=="3"~"75%",
                                  LWD=="4"~"100%"),
                LWD_f=factor(LWD_tmp, 
                             levels=c("0%","25%","50%","75%","100%"),
                             ordered=TRUE)) %>%
  dplyr::mutate(Floating_tmp=case_when(Floating=="0"~"0%",
                                       Floating=="1"~"25%",
                                       Floating=="2"~"50%",
                                       Floating=="3"~"75%",
                                       Floating=="4"~"100%"),
                Floating_f=factor(Floating_tmp, 
                                  levels=c("0%","25%","50%","75%","100%"),
                                  ordered=TRUE)) %>%
  dplyr::mutate(Emergent_tmp=case_when(Emergent=="0"~"0%",
                                       Emergent=="1"~"25%",
                                       Emergent=="2"~"50%",
                                       Emergent=="3"~"75%",
                                       Emergent=="4"~"100%"),
                Emergent_f=factor(Emergent_tmp, 
                                  levels=c("0%","25%","50%","75%","100%"),
                                  ordered=TRUE)) %>%
  dplyr::mutate(Substrate_tmp=case_when(SubstrateSegment=="Rocky/Pebb"~"Rocky/\nPebble",
                                        .default=SubstrateSegment),
                Substrate_f=factor(Substrate_tmp)) %>%
  dplyr::mutate(FieldYear_f=factor(x=FieldYear))


SAV_prop <- Habitat %>%
  dplyr::filter(!is.na(SAV_f)) %>%
  dplyr::group_by(FieldYear_f, SAV_f, .drop=FALSE) %>%
  dplyr::summarize(freq=dplyr::n(),
                   .groups="drop") %>%
  dplyr::group_by(FieldYear_f, .drop=FALSE) %>%
  dplyr::mutate(Proportion=freq/sum(freq),
                total=sum(freq),
                .keep="all") %>%
  dplyr::ungroup()

Floating_prop <- Habitat %>%
  dplyr::filter(!is.na(Floating_f)) %>%
  dplyr::group_by(FieldYear_f, Floating_f, .drop=FALSE) %>%
  dplyr::summarize(freq=dplyr::n(),
                   .groups="drop") %>%
  dplyr::group_by(FieldYear_f, .drop=FALSE) %>%
  dplyr::mutate(Proportion=freq/sum(freq),
                total=sum(freq),
                .keep="all") %>%
  dplyr::ungroup()

Emergent_prop <- Habitat %>%
  dplyr::filter(!is.na(Emergent_f)) %>%
  dplyr::group_by(FieldYear_f, Emergent_f, .drop=FALSE) %>%
  dplyr::summarize(freq=dplyr::n(),
                   .groups="drop") %>%
  dplyr::group_by(FieldYear_f, .drop=FALSE) %>%
  dplyr::mutate(Proportion=freq/sum(freq),
                total=sum(freq),
                .keep="all") %>%
  dplyr::ungroup()

LWD_prop <- Habitat %>%
  dplyr::filter(!is.na(LWD_f)) %>%
  dplyr::group_by(FieldYear_f, LWD_f, .drop=FALSE) %>%
  dplyr::summarize(freq=dplyr::n(),
                   .groups="drop") %>%
  dplyr::group_by(FieldYear_f, .drop=FALSE) %>%
  dplyr::mutate(Proportion=freq/sum(freq),
                total=sum(freq),
                .keep="all") %>%
  dplyr::ungroup()

Substrate_prop <- Habitat %>%
  dplyr::filter(!is.na(Substrate_f)) %>%
  dplyr::group_by(FieldYear_f, Substrate_f, .drop=FALSE) %>%
  dplyr::summarize(freq=dplyr::n(),
                   .groups="drop") %>%
  dplyr::group_by(FieldYear_f, .drop=FALSE) %>%
  dplyr::mutate(Proportion=freq/sum(freq),
                total=sum(freq),
                .keep="all") %>%
  dplyr::ungroup()

BankType_prop <- Habitat %>%
  dplyr::filter(!is.na(BankType_f)) %>%
  dplyr::group_by(FieldYear_f, BankType_f, .drop=FALSE) %>%
  dplyr::summarize(freq=dplyr::n(),
                   .groups="drop") %>%
  dplyr::group_by(FieldYear_f, .drop=FALSE) %>%
  dplyr::mutate(Proportion=freq/sum(freq),
                total=sum(freq),
                .keep="all") %>%
  dplyr::ungroup()


xmax <- max(c(SAV_prop$Proportion, Floating_prop$Proportion, 
              Emergent_prop$Proportion, LWD_prop$Proportion, 
              Substrate_prop$Proportion, BankType_prop$Proportion), 
            na.rm=TRUE)

p_SAV <- ggplot(data=SAV_prop, mapping=aes(y=SAV_f, x=Proportion)) +
  geom_col() + 
  xlim(0, xmax) + 
  ylab("Submerged Aquatic\nVegetation Coverage") + 
  report_theme + 
  facet_wrap( ~ FieldYear_f, nrow=1)
p_SAV

p_Floating <- ggplot(data=Floating_prop, mapping=aes(y=Floating_f, x=Proportion)) +
  geom_col() + 
  xlim(0, xmax) + 
  ylab("Floating Vegetation\nCoverage") + 
  report_theme + 
  facet_wrap( ~ FieldYear_f, nrow=1)
p_Floating

p_Emergent <- ggplot(data=Emergent_prop, mapping=aes(y=Emergent_f, x=Proportion)) +
  geom_col() + 
  xlim(0, xmax) + 
  ylab("Emergent Vegetation\nCoverage") + 
  report_theme + 
  facet_wrap( ~ FieldYear_f, nrow=1)
p_Emergent

p_LWD <- ggplot(data=LWD_prop, mapping=aes(y=LWD_f, x=Proportion)) +
  geom_col() + 
  xlim(0, xmax) + 
  ylab("Large Woody Debris\nCoverage") + 
  report_theme + 
  facet_wrap( ~ FieldYear_f, nrow=1)
p_LWD

p_Substrate <- ggplot(data=Substrate_prop, mapping=aes(y=Substrate_f, x=Proportion)) +
  geom_col() + 
  xlim(0, xmax) + 
  ylab("Substrate Type") + 
  report_theme + 
  facet_wrap( ~ FieldYear_f, nrow=1)
p_Substrate

p_BankType <- ggplot(data=BankType_prop, mapping=aes(y=BankType_f, x=Proportion)) +
  geom_col() + 
  xlim(0, xmax) + 
  ylab("Shore Type") + 
  report_theme + 
  facet_wrap( ~ FieldYear_f, nrow=1)
p_BankType


Habitat_combined <- p_SAV + 
  p_Floating + theme(strip.text.x=element_blank()) + 
  p_Emergent + theme(strip.text.x=element_blank()) + 
  p_LWD + theme(strip.text.x=element_blank()) + 
  p_Substrate + theme(strip.text.x=element_blank()) + 
  p_BankType + theme(strip.text.x=element_blank()) + 
  patchwork::plot_layout(nrow=6, axes="collect_x") + 
  theme(plot.background=element_rect(fill="white", color=NA))
Habitat_combined


ggsave(filename=file.path("EFISH_Habitat_breakdown.png"), 
       plot=Habitat_combined, height=8, width=11, units="in") 

################################################################################
################################################################################