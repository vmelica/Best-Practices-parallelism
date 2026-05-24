

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
lit_rev <- lit_rev_s %>%
  mutate(Stats.group = case_when(Stats.type %in% c("ANCOVA","F-test", "ANOVA ", "T-test", "Test of equal slopes", "Pearson's chi_square" ,"Correlation test") ~ "Slopes comparison",
                                 Stats.type %in% c("Linear regression", "R-squared ") ~ "Regression analysis",
                                 Stats.type == "Descriptive stas-CV" ~ "Descriptive",
                                 TRUE ~ "Other" ))

lit_rev_s <- lit_rev %>%
  rename(refPalme = Reference.number.in.Dr..Palme.list ) %>%
  mutate(refPalme = case_when(refPalme == "not in the list"~ "NA", 
                              TRUE ~ refPalme)) %>%
  filter(Parallelism.reporting != "") %>% #removing ghost rows
  # filter(refPalme != "NA" ) %>% # if we decide to remove the non 2022 papers
  select(-starts_with("X")) %>% #removing ghost columns
  select(-c(number,Author.list, City, Country, Class, Latin.Species,Species, number....of.fecal.samples.)) #selcting relevant columns

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
  labs(fill = "Parallelism reporting",
       title = " ") +
  geom_text(aes(label = paste0(round(prop, 1), "%")),
            position = position_stack(vjust = 0.5))
ggsave(here("./outputs/pie_plot.png"), width = 6, height = 6, dpi = 600)

ggplot(lit_rev_s, aes(x=Parallelism.reporting))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ xlab("")



lit_rev_s %>%
  filter(Parallelism.reporting == "Yes") %>%
  group_by(Graph) %>%
  summarize(n = n()) %>%
  mutate(prop = n/ sum(n) *100)

# lit_rev_s %>%
#   filter(Parallelism.reporting == "Yes") %>%
#   ggplot(aes(x=Graph))+
#   geom_bar(stat="count", width=0.7, fill="steelblue")+
#   theme_minimal()+ xlab("")
lit_rev_s %>%
  filter(Parallelism.reporting == "Yes") %>%
  group_by(Stats) %>%
  summarize(n = n()) %>%
  mutate(prop = round(n / sum(n) * 100))
Stats_test <- lit_rev_s %>%
  filter(Parallelism.reporting == "Yes", Stats != "N") %>%
  group_by(Stats.type) %>%
  summarize(n = n()) 
write.csv(Stats_test , "Stats_test.csv", row.names = FALSE)


lit_rev_s <- lit_rev_s %>%
  mutate(Stats.group = case_when(Stats.type %in% c("ANCOVA","F-test", "ANOVA ", "T-test", "Test of equal slopes", "Pearson's chi_square" ,"Correlation test") ~ "Slopes comparison",
                                 Stats.type %in% c("Linear regression", "R-squared ") ~ "Regression analysis",
                                 Stats.type == "Descriptive stas-CV" ~ "Descriptive",
                              TRUE ~ "Other" ))

lit_rev_s %>%
  filter(Parallelism.reporting == "Yes", Stats == "Y") %>%
  group_by(Stats.group) %>%
  summarize(n = n()) 

lit_rev_s %>%
  filter(Parallelism.reporting == "Yes") %>%
  ggplot(aes(x=Stats))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ xlab("")

# need to muutate some of the names of the stats type
