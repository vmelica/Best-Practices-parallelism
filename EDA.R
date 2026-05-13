

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
lit_rev_s <- lit_rev %>%
  rename(refPalme = Reference.number.in.Dr..Palme.list ) %>%
  mutate(refPalme = case_when(refPalme == "not in the list"~ "NA", 
                              TRUE ~ refPalme)) %>%
  filter(Parallelism.reporting != "") %>% #removing ghost rows
  filter(refPalme != "NA" ) %>% # if we decide to remove the non 2022 papers
  select(-starts_with("X")) %>% #removing ghost columns
  select(-c(number,Author.list, City, Country, Class, Latin.Species,Species, number....of.fecal.samples.)) #selcting relevant columns

# colnames(lit_rev_s) # to check

# summary of parallelism reporting
lit_rev_s %>%
  group_by(Parallelism.reporting) %>%
  summarize(n = n()) 

ggplot(lit_rev_s, aes(x=Parallelism.reporting))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ xlab("")
lit_rev_s %>%
  filter(Parallelism.reporting == "Yes") %>%
  group_by(Graph) %>%
  summarize(n = n()) 

lit_rev_s %>%
  filter(Parallelism.reporting == "Yes") %>%
  ggplot(aes(x=Graph))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ xlab("")

lit_rev_s %>%
  filter(Parallelism.reporting == "Yes") %>%
  group_by(Stats) %>%
  summarize(n = n()) 

lit_rev_s %>%
  filter(Parallelism.reporting == "Yes") %>%
  ggplot(aes(x=Stats))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ xlab("")
