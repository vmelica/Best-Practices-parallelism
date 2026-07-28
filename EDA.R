

# clear enviroment if needed
rm(list=ls())

# Loading packages -----------------------------------------------

# data manipulation
suppressMessages(library(openxlsx))
suppressMessages(library(reader))
suppressMessages(library(plyr))
suppressMessages(library(here))
suppressMessages(library(dplyr))
suppressMessages(library(tidyverse))
# data visualization
suppressMessages(library(cowplot))
suppressMessages(library(plotrix))
suppressMessages(library(patchwork))
suppressMessages(library(RColorBrewer))
suppressMessages(library(ggpubr))
suppressMessages(library(ggplot2))

suppressMessages(library(lattice))

# Import data -----------------------------------------------

# import literature review merged data 
lit_rev <- read.csv(here("./input/Litreview.csv")) 
#write.csv(lit_rev_s, "lit_rev_s.csv", row.names = FALSE)
colnames(lit_rev)
# clean up original file
lit_rev%>%
  filter(Parallelism.reporting == "Yes", Stats != "N") %>%
  group_by(Stats.type) %>%
  summarize(n = n())
lit_rev$Stats <-as.factor(lit_rev$Stats)

levels(lit_rev$Stats)
lit_rev <- lit_rev %>%
  # filter(number != 127) %>% # remove one row that need to be excluded
  rename(refPalme = Reference.number.in.Dr..Palme.list ) %>% # rename some columns that have long names
  rename(DilutionLin = Do.they.test.Dilution.linearity. ) %>%
  mutate(refPalme = case_when(refPalme == "not in the list"~ "NA", 
                              TRUE ~ refPalme)) %>%
  filter(Parallelism.reporting != "") %>% # removing ghost rows
  # filter(refPalme != "NA" ) %>% # if we decide to remove the non 2022 papers
  select(-starts_with("X")) %>% # removing ghost columns
  # mutate(Stats.group = case_when(Stats.type %in% c("ANCOVA","F-test", "ANOVA ", "T-test", "Test of equal slopes", "Pearson's chi_square" ) ~ "Equality of slopes",
                                 # Stats.type %in% c("Linear regression", "R-squared ","Correlation test") ~ "Correlation/regression",
                                 # Stats.type == "Descriptive stats-CV" ~ "Descriptive",
                                 # Stats == "N" ~ "No stats",
                                 # Stats == "" ~ "No stats",
                                 # TRUE ~ "Other" )) %>% # add statistical group column
  select(c(refPalme, number, RefID, Journal, Research.area, Title, Population, Animal.Group, immunoassay, Parallelism.reporting, Graph, Graph.type, Stats, Stats.type, DilutionLin, Stats.group, Stats.group.KF)) # selecting relevant columns

lit_rev%>%
  group_by(Stats.group) %>%
  summarize(n = n())

write.csv(lit_rev, here("./input/Litreview_rev.csv"), row.names = FALSE) # save as a new csv

lit_rev_s<- read.csv(here("./input/Litreview_rev.csv")) 

  
# colnames(lit_rev_s) # to check


# summary of parallelism reporting
lit_rev_s %>%
  group_by(Parallelism.reporting) %>%
  summarize(n = n()) %>%
  mutate(prop = round(n / sum(n) * 100)) %>%
  ggplot(aes(x = "", y = prop, fill = Parallelism.reporting)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  
  scale_fill_manual(values = c(
    "Yes" = "#4EA72E",
    "No" = "#7A0000",
    "Prev" = "#D9D9D9",
  "Statement" = "#8ED973"))+
  labs(fill = "Parallelism reporting",
       title = " ") +
  theme(text = element_text (size =12, family = "Arial"))+
  geom_text(aes(label = paste0(round(prop, 1), "%"), fontface = "bold"),
            position = position_stack(vjust = 0.5))

ggsave(here("./outputs/pie_plot.png"), width = 6, height = 6, dpi = 600)

###### PROPORTION OF STUDIES THAT APPLIED STATS TEST 

lit_rev_s %>%
  filter(Parallelism.reporting == "Yes") %>%
  group_by(Stats) %>%
  summarize(n = n()) %>%
  mutate(prop = round(n / sum(n) * 100))
# List of stats test used- saved in table format as csv
Stats_test <- lit_rev_s %>%
  filter(Parallelism.reporting == "Yes", Stats != "N") %>%
  group_by(Stats.type) %>%
  summarize(n = n()) 

Stats_test <- Stats_test %>%
  rename(Statistical_test =Stats.type )
write.csv(Stats_test , here("./outputs/Stats_test.csv"), row.names = FALSE)
# adding dilution linearity as a stats group ( NOTE: 3 studies have applied dilution linearity, 
#two of them did it in addition to a statistical test for equal slopes, therefore they already have a stats group assigned)

# lit_rev_s <-lit_rev_s %>%
# mutate(Stats.group = case_when((Stats.group == "Other" | Stats.group == "No stats") & 
#                     DilutionLin == "Y" ~ "Dilution Linearity",
#                     TRUE ~ Stats.group)) %>%
# mutate(Stats.group = case_when(Stats.group == "Equality of slopes" & 
#                     DilutionLin == "Y" ~ "Equality of slopes/Dilution Linearity", 
#                     TRUE ~ Stats.group))


lit_rev_s %>%
  filter(Parallelism.reporting == "Yes") %>%
  group_by(Stats.group) %>%
  summarize(n = n()) %>%
  mutate(prop = n/ sum(n) *100)

lit_rev_s %>%
  filter(Stats.group !="Other"& Stats.group != "No stats" ) %>%
  group_by(Stats.group) %>%
  summarize(n = n())  %>%
  mutate(prop = round(n / sum(n) * 100)) %>%
  ggplot(aes(x= Stats.group, y= prop))+
  geom_bar(stat= "identity", width=0.7, fill="steelblue")+
  theme_minimal()+ ylab("proportion of studies (%)") + xlab("")
ggsave(here("./outputs/stats_group.png"), width = 6, height = 6, dpi = 600)
# need to update the merged excel file with the stats group and finish table S1

lit_rev_s %>%
  filter(Parallelism.reporting == "Yes") %>%
  group_by(Graph) %>%
  summarize(n = n()) %>%
  mutate(prop = n/ sum(n) *100)

lit_rev_s %>%
  filter(Parallelism.reporting == "Yes") %>%
  group_by(Graph.type) %>%
  summarize(n = n()) %>%
  mutate(prop = n/ sum(n) *100)

lit_rev_s %>%
  filter(Parallelism.reporting == "Yes" ) %>%
  group_by(Stats) %>%
  summarize(n = n()) %>%
  mutate(prop = n/ sum(n) *100)

lit_rev_s %>%
  filter(Parallelism.reporting == "Yes" & Graph=="Y") %>%
  group_by(Stats) %>%
  summarize(n = n()) %>%
  mutate(prop = n/ sum(n) *100)

lit_rev_s %>%
  filter(Parallelism.reporting == "Yes" & Stats=="Y") %>%
  group_by(Graph) %>%
  summarize(n = n()) %>%
  mutate(prop = n/ sum(n) *100)
