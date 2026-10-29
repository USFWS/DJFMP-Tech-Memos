# This script was written by Russell Perry (USGS) and updated by Lauren Yamane (FWS) to accommodate new versions of catch and genetic data

library(tidyverse)

all_catch_21 = readRDS("Processed Data\\catch with gen assignments_20261016.rds") %>%
  filter(mig_yr < 2022) %>%
  select(-c(ForkLength, SampleDate, LAD_Race)) 
all_catch_25 = readRDS("Processed Data\\catch with gen assignments_2022_25_OOS_valid_v4.rds") %>% 
  select(-c(ForkLength.x, ForkLength.y))
all_catch <- bind_rows(all_catch_21, all_catch_25)
write.csv(all_catch, "catch with gen assignments_2016_2025.csv")

catch_effort_21 = readRDS("Processed Data/catch and effort by tow_20261016.rds") %>%
  select(Location, StationCode, Sample_Date, SampleTime, TowNumber, yr, mig_yr, Count:known_WR_Spring) %>%
  filter(mig_yr < 2022)
catch_2022_25_OOS <- readRDS("Processed Data/catch and effort by tow_2022_25_OOS_valid_v4.rds") %>%
  select(Location, StationCode, Sample_Date, SampleTime, TowNumber, mig_yr, Count, ad_catch:known_WR_Spring)
catch_effort <- bind_rows(catch_effort_21, catch_2022_25_OOS) 
write.csv(catch_effort, "catch and effort by tow_2016_2025.csv")

#----- Subset tissue samples collected for DNA ------
DNA_samples = all_catch %>% 
  filter(mig_yr != 2016) %>%
  filter(grepl("DNA", SpecialStudy) | !is.na(Genetic_Assignment)) %>% 
  mutate(Genetic_Assignment = ifelse(Genetic_Assignment %in% c("Fall/LateFall", "Fall / LateFall", "Fall or Late Fall", "LateFall", "Fall", "Late Fall"), "Fall_LateFall", Genetic_Assignment)) %>% select(-c(MethodCode:Conductivity, TowDirectionCode:Expression, RegionCode)) %>%
  left_join(catch_effort %>% select(-c(TowNumber, RaceByLength, Count, yr)), by = c("Location", "StationCode", "Sample_Date", "SampleTime","SpecialStudyID","mig_yr"))%>% #
  arrange(Location, Sample_Date)

#--------- Summarize genetics by year -----------

DNA_collected = DNA_samples %>% 
  group_by(Location, mig_yr) %>% 
  summarize(dna_collected = n())

# DNA collected by Location, mig_yr, and LAD
DNA_collected_LAD = DNA_samples %>%
  group_by(Location, mig_yr, RaceByLength) %>%
  summarize(dna_collected = n())

Gen_ID_sum = DNA_samples %>% 
  filter(!is.na(Genetic_Assignment)) %>% 
  group_by(Location, mig_yr, Genetic_Assignment) %>% 
  summarize(gen_id = n())

DNA_sum_annual = full_join(DNA_collected, Gen_ID_sum) %>% 
  mutate(no_gen_id = dna_collected - gen_id,
         p_no_gen_id = round(no_gen_id / dna_collected, 3)) %>% 
  filter(mig_yr != 2016)

#--------- Summarize DNA results by year and LAD ------------
gen_sum_1 = DNA_samples %>%
  filter(!is.na(Genetic_Assignment)) %>% 
  filter(RaceByLength != "n/p") %>% 
  #mutate(Genetic_Assignment = ifelse(Genetic_Assignment %in% c("Fall or Late Fall", "Late Fall","Fall", "LateFall"), "Fall_LateFall", Genetic_Assignment)) %>% # LY added, may need to fix later #
  group_by(Location, mig_yr, RaceByLength, Genetic_Assignment)%>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = "Genetic_Assignment",
              values_from = "n",
              values_fill = list(n = 0)) %>% 
  mutate(dna_analyzed = Fall_LateFall + Spring + Winter + Unassigned)

# Total catch by LAD group, loc, mig_yr
gen_sum_2 = all_catch %>% 
  filter(!is.na(RaceByLength)) %>% 
  filter(RaceByLength != "n/p") %>%  
  group_by(Location, mig_yr, RaceByLength) %>% 
  summarize(catch = sum(Count))


gen_sum_3 = DNA_samples %>% 
  filter(grepl("DNA", SpecialStudy) | !is.na(Genetic_Assignment)) %>% 
  filter(RaceByLength != "n/p") %>%  
  group_by(Location, mig_yr, RaceByLength) %>% 
  summarize(dna_collected = n())

DNA_sum_annual_LAD = full_join(gen_sum_1, gen_sum_2) %>% 
  full_join(gen_sum_3) %>% 
  mutate(p_not_analyzed = round((dna_collected - dna_analyzed) / dna_collected, 3)) %>% 
  select(Location:RaceByLength, catch, dna_collected, dna_analyzed, 
         p_not_analyzed, Fall_LateFall:Unassigned) %>% 
  arrange(Location, mig_yr, RaceByLength) %>% 
  filter(mig_yr != 2016)

write.csv(DNA_sum_annual_LAD, "DNA_sum_annual_LAD_2017_25_v2.csv")

# LY: Summarize for DJFMP tech memo - genetics portion.  THIS IS ONLY THE CRAMER ANALYZED SAMPLES, NOT MARIAH MEEK RESULTS. WOULD NEED TO BRING IN BRIAN PYPER'S SPREADSHEET FOR MEEK RESULTS.


## Chipps Island
LAD_gen_assign <- DNA_sum_annual_LAD %>% 
  #mutate(FallLateFall_final = sum(FallLateFall, LateFall)) %>%
  select(Location, mig_yr, RaceByLength, Spring, Winter, Unassigned, Fall_LateFall) %>%
  #rename(FallLateFall = FallLateFall_final) %>%
  pivot_longer(
    cols = c(Spring, Winter, Fall_LateFall, Unassigned),
    names_to = "Genetic_Assignment",
    values_to = "Total")

LAD_gen_assign_gg_chipps <- LAD_gen_assign %>%
  mutate(GeneticID = as.factor(Genetic_Assignment)) %>%
  filter(Location == "Chipps Island") %>%
  ggplot(aes(x = RaceByLength, y = Total, fill = Genetic_Assignment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~ mig_yr, nrow = 2) +
  labs(x = "Length At Date Category", y = "Number of Assignments", title = "Genetic Assignments per LAD: Chipps Island") + 
  guides(fill = guide_legend(title = "Genetic ID")) +
  scale_fill_viridis_d(option = "viridis")

ggsave(filename = "DJFMP_tech_memo_gen_per_LAD_Chipps_2017_25.tif", plot = LAD_gen_assign_gg_chipps)

LAD_gen_assign_prop <- LAD_gen_assign %>%
  mutate(Genetic_Assignment = as.factor(Genetic_Assignment)) %>%
  group_by(Location, mig_yr, RaceByLength) %>%
  summarise(all = sum(Total)) %>%
  ungroup() 

LAD_gen_assign_gg_chipps_prop <- LAD_gen_assign %>% 
  left_join(LAD_gen_assign_prop, by = join_by(Location, mig_yr, RaceByLength)) %>%
  mutate(prop_all = Total/all) %>%
  filter(Location == "Chipps Island") %>%
  ggplot(aes(x = RaceByLength, y = prop_all, fill = Genetic_Assignment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~ mig_yr, nrow = 2) +
  labs(x = "Length At Date Category", y = "Genetic Proportion", title = "Proportions of Genetic Assignments per LAD category: Chipps Island") + 
  guides(fill = guide_legend(title = "Genetic ID")) +
  scale_fill_viridis_d(option = "viridis")

ggsave(filename = "DJFMP_tech_memo_gen_prop_per_LAD_Chipps_2017_25.tif", plot = LAD_gen_assign_gg_chipps_prop)

## Sherwood Harbor
#manual_hex <- c("#4D76A1", viridis(10), "violet", "gold")
LAD_gen_assign_gg_sh <- LAD_gen_assign %>%
  mutate(GeneticID = as.factor(Genetic_Assignment)) %>%
  filter(Location == "Sherwood Harbor") %>%
  ggplot(aes(x = RaceByLength, y = Total, fill = Genetic_Assignment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~ mig_yr, nrow = 2) +
  labs(x = "Length At Date Category", y = "Number of Assignments", title = "Genetic Assignments per LAD: Sherwood Harbor") + 
  guides(fill = guide_legend(title = "Genetic ID")) +
  scale_fill_viridis_d(option = "viridis")
  #scale_colour_manual(values = manual_hex, name = "Group")

ggsave(filename = "DJFMP_tech_memo_gen_per_LAD_Sherwood_2017_25.tif", plot = LAD_gen_assign_gg_sh)

LAD_gen_assign_gg_sh_prop <- LAD_gen_assign %>% 
  left_join(LAD_gen_assign_prop, by = join_by(Location, mig_yr, RaceByLength)) %>%
  mutate(prop_all = Total/all) %>%
  filter(Location == "Sherwood Harbor") %>%
  ggplot(aes(x = RaceByLength, y = prop_all, fill = Genetic_Assignment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~ mig_yr, nrow = 2) +
  labs(x = "Length At Date Category", y = "Genetic Proportion", title = "Proportions of Genetic Assignments per LAD category: Sherwood Harbor") + 
  guides(fill = guide_legend(title = "Genetic ID")) +
  scale_fill_viridis_d(option = "viridis")

ggsave(filename = "DJFMP_tech_memo_gen_prop_per_LAD_Sherwood_2017_25.tif", plot = LAD_gen_assign_gg_sh_prop)

gen_sum_annual = DNA_sum_annual_LAD %>% 
  group_by(Location, mig_yr) %>% 
  summarize(dna_analyzed = sum(dna_analyzed, na.rm = T),
            FallLateFall = sum(FallLateFall, na.rm = T),
            Spring = sum(Spring, na.rm = T),
            Winter = sum(Winter, na.rm = T),
            Fall = sum(Fall, na.rm = T),
            LateFall = sum(LateFall, na.rm = T))

#*******************************************************************************
# Summarize daily catch data, including sampling rates for genetics.
#*******************************************************************************
daily_catch = catch_effort %>% 
  group_by(mig_yr, Sample_Date, Location) %>% 
  summarize_at(vars(Count:known_WR_Spring), sum, na.rm = TRUE) %>% 
  arrange(Sample_Date)

daily_catch %>% 
  filter(mig_yr != 2016) %>% 
  ggplot(aes(x = Sample_Date, y = Count, group = Location, col = Location)) +
  geom_line() +
  facet_wrap(vars(mig_yr), scales = "free", nrow = 3)

# daily catch by LAD group
daily_catch %>% 
    filter(mig_yr != 2016) %>% 
    select(Sample_Date, Location, mig_yr, lad_Winter:lad_Spring) %>% 
    pivot_longer(cols = -(Sample_Date:mig_yr),
                 names_to = "race",
                 values_to = "catch") %>% 
    ggplot(aes(x = Sample_Date, y = catch, group = Location, col = Location)) +
    geom_line() +
    facet_wrap(race ~ mig_yr, scales = "free")

# daily catch capped at maximum number of samples
daily_catch %>% 
  filter(mig_yr != 2016) %>% 
  select(Sample_Date, Location, mig_yr, lad_Winter:lad_Spring) %>% 
  mutate(lad_Fall = case_when(Location == "Chipps" ~  pmin(lad_Fall, 25),
                              Location == "Sherwood Harbor" ~  pmin(lad_Fall, 10),
                              TRUE ~ lad_Fall),
         lad_Spring = pmin(lad_Spring, 10)) %>%
  pivot_longer(cols = -(Sample_Date:mig_yr),
               names_to = "race",
               values_to = "catch") 

dna_plot_dat = daily_catch %>% 
  filter(mig_yr != 2016) %>% 
  select(Sample_Date, Location, mig_yr, 
         lad_Winter:lad_Spring, 
         dna_Winter:dna_Spring,
         known.WR_LateFall =  known_WR_LateFall,
         known.WR_Fall =  known_WR_Fall,
         known.WR_Winter =  known_WR_Winter,
         known.WR_Spring =  known_WR_Spring,
         gen_LateFall:gen_Winter) %>% 
  mutate(lad_Fall = case_when(Location == "Chipps Island" ~  pmin(lad_Fall, 25),
                              Location == "Sherwood Harbor" ~  pmin(lad_Fall, 10),
                              TRUE ~ lad_Fall),
         lad_Spring = pmin(lad_Spring, 10)) %>%
  pivot_longer(cols = -(Sample_Date:mig_yr),
               names_to = c("type", "race"),
               values_to = "catch",
               names_sep = "_") %>% 
  mutate(Type = case_when(type == "lad" ~ "LAD catch capped at max tissue sample size",
                           type == "dna" ~ "DNA samples collected",
                           type == "gen" ~ "DNA samples analyzed",
                           type == "known.WR" ~ "Genetic Winter run",
                           TRUE ~ type))

dna_plot_dat %>% 
  group_by(mig_yr, race, Type) %>% 
  filter(type %in% c("lad", "dna")) %>% 
  filter(Location == "Chipps Island") %>% 
  summarize(catch = sum(catch)) %>% 
  pivot_wider(values_from = catch, names_from = Type)# %>% 
  #write_csv(path = "Chipps DNA sample summary.csv")
  
#------------------------
# plot number of dna samples collected per day versus number caught by LAD group
#------------------------
plot_dna_catch = function(loc){
  dna_plot_dat %>% 
    filter(!type %in% c("known.WR", "gen")) %>% 
    filter(Location == loc) %>% 
    ggplot(aes(x = Sample_Date, y = catch, group = desc(Type), col = Type)) +
    geom_line(aes(size = Type)) +
    scale_size_discrete(range = c(0.75, 1.25)) +
    scale_color_manual(values = c("magenta2", "gray50")) +
    facet_wrap(mig_yr ~ race, nrow = 5, scales = "free") +
    labs(title = loc) +
    theme_bw() +
    theme(legend.position = "top",
          legend.title = element_blank()) +
    ylab("Number of samples")
}

sac_dna_catch = plot_dna_catch(loc = "Sherwood Harbor")
chipps_dna_catch = plot_dna_catch(loc = "Chipps Island")

#------------------------
# plot number of genetic results per day versus dna samples collected by LAD group
#------------------------
plot_dna_gen = function(loc){
  dna_plot_dat %>% 
  filter(!type %in% c("known.WR", "lad")) %>% 
  filter(Location == loc) %>% 
  ggplot(aes(x = Sample_Date, y = catch, group = desc(Type), col = Type)) +
  geom_line(aes(size = Type)) +
  scale_size_discrete(range = c(0.75, 1.25)) +
  scale_color_manual(values = c("magenta2", "gray50")) +
  facet_wrap(mig_yr ~ race, nrow = 5, scales = "free") +
  labs(title = loc) +    
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_blank()) +
    ylab("Number of samples")
}

sac_dna_analyzed = plot_dna_gen(loc = "Sherwood Harbor")
chipps_dna_analyzed = plot_dna_gen(loc = "Chipps Island")

#------------------------
# plot true winter run versus number of samples genetically analyzed. 
#------------------------
plot_gen_true.win = function(loc){
  dna_plot_dat %>% 
    filter(!type %in% c("dna", "lad")) %>% 
    filter(Location == loc) %>% 
    ggplot(aes(x = Sample_Date, y = catch, group = Type, col = Type)) +
    geom_line(aes(size = Type)) +
    scale_size_discrete(range = c(1.25, 0.75)) +
    scale_color_manual(values = c("gray50", "magenta2")) +
    facet_wrap(mig_yr ~ race, nrow = 5, scales = "free") +
    labs(title = loc) +    
    theme_bw() +
    theme(legend.position = "top",
          legend.title = element_blank()) +
    ylab("Number of samples")
}

chipps_wr_lad = plot_gen_true.win(loc = "Chipps Island")
sac_wr_lad = plot_gen_true.win(loc = "Sherwood Harbor")

# Focus just on winter run
plot_lad_winter = function(loc){
  dna_plot_dat %>% 
    filter(!type %in% c("dna", "lad")) %>% 
    filter(race == "Winter") %>%
    filter(Location == loc) %>% 
    mutate(month = month(Sample_Date)) %>% 
    filter(month %in% c(1:6, 12)) %>% 
    ggplot(aes(x = Sample_Date, y = catch, group = Type, col = Type)) +
    geom_line(aes(size = Type)) +
    scale_size_discrete(range = c(1.25, 0.75)) +
    scale_color_manual(values = c("gray50", "magenta2")) +
    facet_wrap(~mig_yr, nrow = 5, scales = "free") +
    labs(title = loc) +    
    theme_bw() +
    theme(legend.position = "top",
          legend.title = element_blank())
}

sac_wr_2 = plot_lad_winter(loc = "Sherwood Harbor")
chipps_wr_2 = plot_lad_winter(loc = "Chipps Island")

