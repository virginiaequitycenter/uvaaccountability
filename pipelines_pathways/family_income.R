# UVA accountability metrics
# Pipelines and Pathways
# Testing and Example: family survival wages, Cville MSA
# Created: 2022-01-28
# Michele Claibourn

# .................................
# Set up ----
library(tidyverse)
library(tidycensus)
library(tigris)
library(lubridate)
library(ggthemes)
library(patchwork)
library(plotly)


# The Charlottesville MSA includes
#   Charlottesville, Albemarle, Fluvanna, Greene, Nelson
#   excludes Louisa (and Buckingham) 
#   so totals/percents will not match past orange dot reports

# Localities included in Orange Dot
# lookup_code("VA", "Buckingham")
localfips <- c("003", "029", "065", "079", "109", "125", "540")


# .................................
# Get community data ----
# Family income (B19101)

# test <- get_acs(
#   year = 2019, 
#   geography = "metropolitan statistical area/micropolitan statistical area",
#   table = "B19101",
#   survey = "acs1", 
#   summary_var = "B19101_001",
#   cache = TRUE) %>% 
#   filter(GEOID == "16820")

# Charlottesville MSA, ACS 1-year estimates 
cmsa_all <-
  map_df(2009:2019,
         ~ get_acs(
           year = .x,
           geography = "metropolitan statistical area/micropolitan statistical area",
           table = "B19101",
           survey = "acs1", 
           summary_var = "B19101_001",
           cache = TRUE
         ) %>%
           mutate(year = .x)
         ) %>% 
  filter(GEOID == "16820")

# Charlottesville region localities, ACS 5-year estimates 
local_all <-
  map_df(2009:2019,
         ~ get_acs(
           year = .x,
           state = "VA",
           geography = "county",
           county = localfips,
           table = "B19101",
           survey = "acs5",
           summary_var = "B19101_001",
           cache = TRUE
         ) %>% 
           mutate(year = .x)
  )

# Charlottesville MSA, by race, ACS 1-year estimates

cmsa_black <- 
  map_df(2009:2019,
         ~ get_acs(
           year = .x,
           geography = "metropolitan statistical area/micropolitan statistical area",
           table = "B19101B",
           survey = "acs1", 
           summary_var = "B19101B_001",
           cache = TRUE
         ) %>%
           mutate(year = .x)
  ) %>% 
  filter(GEOID == "16820")
# missing in 2009, 2011, 2017, 2018, 2019
# B19101D Asian missing all years
# B19101I Hispanic missing all years

# Charlottesville region localities, by race, ACS 5-year estimates 
local_black <-
  map_df(2009:2019,
         ~ get_acs(
           year = .x,
           state = "VA",
           geography = "county",
           county = localfips,
           table = "B19101B",
           survey = "acs5",
           summary_var = "B19101B_001",
           cache = TRUE
         ) %>% 
           mutate(year = .x)
  )

local_hisp <-
  map_df(2009:2019,
         ~ get_acs(
           year = .x,
           state = "VA",
           geography = "county",
           county = localfips,
           table = "B19101I",
           survey = "acs5",
           summary_var = "B19101I_001",
           cache = TRUE
         ) %>% 
           mutate(year = .x)
  )

local_white <-
  map_df(2009:2019,
         ~ get_acs(
           year = .x,
           state = "VA",
           geography = "county",
           county = localfips,
           table = "B19101H",
           survey = "acs5",
           summary_var = "B19101H_001",
           cache = TRUE
         ) %>% 
           mutate(year = .x)
  )

local_asian <-
  map_df(2009:2019,
         ~ get_acs(
           year = .x,
           state = "VA",
           geography = "county",
           county = localfips,
           table = "B19101D",
           survey = "acs5",
           summary_var = "B19101D_001",
           cache = TRUE
         ) %>% 
           mutate(year = .x)
  )

local_multi <-
  map_df(2009:2019,
         ~ get_acs(
           year = .x,
           state = "VA",
           geography = "county",
           county = localfips,
           table = "B19101G",
           survey = "acs5",
           summary_var = "B19101G_001",
           cache = TRUE
         ) %>% 
           mutate(year = .x)
  )

# .................................
# Wrangle community data ----

# test_fsw <- test %>%
#   filter(variable != "B19101_001") %>%
#   mutate(income = variable,
#          income = fct_collapse(income,
#                                "$0 - $9,999" = "B19101_002",
#                                "$10,000 - $14,999" = "B19101_003",
#                                "$15,000 - $24,999" = c("B19101_004", "B19101_005"),
#                                "$25,000 - $34,999" = c("B19101_006", "B19101_007"),
#                                other_level = "$35,000 or more")) %>%
#   group_by(income) %>%
#   summarize(families = sum(estimate),
#             total = first(summary_est)) %>%
#   mutate(percent = round(families/total*100,1))

# Charlottesville MSA, ACS 1-year estimates
cmsa_fsw <- cmsa_all %>% 
  filter(variable != "B19101_001") %>% 
  mutate(income = variable,
         income = fct_collapse(income,
                               "$0 - $9,999" = "B19101_002",
                               "$10,000 - $14,999" = "B19101_003",
                               "$15,000 - $24,999" = c("B19101_004", "B19101_005"),
                               "$25,000 - $34,999" = c("B19101_006", "B19101_007"),
                               other_level = "$35,000 or more")) %>%
  group_by(year, income) %>%
  summarize(families = sum(estimate),
            total = first(summary_est)) %>%
  mutate(percent = round(families/total*100,1))

# Charlottesville region localities, ACS 5-year estimates 
local_fsw <- local_all %>% 
  filter(variable != "B19101_001") %>% 
  mutate(income = variable,
         income = fct_collapse(income,
                               "$0 - $9,999" = "B19101_002",
                               "$10,000 - $14,999" = "B19101_003",
                               "$15,000 - $24,999" = c("B19101_004", "B19101_005"),
                               "$25,000 - $34,999" = c("B19101_006", "B19101_007"),
                               other_level = "$35,000 or more")) %>%
  group_by(year, NAME, income) %>%
  summarize(families = sum(estimate),
            total = first(summary_est)) %>%
  mutate(percent = round(families/total*100,1))

local_fsw_sum <- local_fsw %>% 
  group_by(year, income) %>% 
  summarize(families = sum(families),
            total = sum(total)) %>% 
  mutate(percent = round(families/total*100,1))


# Charlottesville MSA, by race, ACS 1-year estimates
# too much missing


# Charlottesville region localities, by race, ACS 5-year estimates 
# reduce

# Black
local_black_fsw <- local_black %>% 
  filter(variable != "B19101B_001") %>% 
  mutate(income = variable,
         income = fct_collapse(income,
                               "$0 - $9,999" = "B19101B_002",
                               "$10,000 - $14,999" = "B19101B_003",
                               "$15,000 - $24,999" = c("B19101B_004", "B19101B_005"),
                               "$25,000 - $34,999" = c("B19101B_006", "B19101B_007"),
                               other_level = "$35,000 or more")) %>%
  group_by(year, NAME, income) %>%
  summarize(families = sum(estimate),
            total = first(summary_est)) %>%
  mutate(percent = round(families/total*100,1))

local_black_fsw_sum <- local_black_fsw %>% 
  group_by(year, income) %>% 
  summarize(families = sum(families),
            total = sum(total)) %>% 
  mutate(percent = round(families/total*100,1),
         race = "Black")

# Hispanic
local_hisp_fsw <- local_hisp %>% 
  filter(variable != "B19101I_001") %>% 
  mutate(income = variable,
         income = fct_collapse(income,
                               "$0 - $9,999" = "B19101I_002",
                               "$10,000 - $14,999" = "B19101I_003",
                               "$15,000 - $24,999" = c("B19101I_004", "B19101I_005"),
                               "$25,000 - $34,999" = c("B19101I_006", "B19101I_007"),
                               other_level = "$35,000 or more")) %>%
  group_by(year, NAME, income) %>%
  summarize(families = sum(estimate),
            total = first(summary_est)) %>%
  mutate(percent = round(families/total*100,1))

local_hisp_fsw_sum <- local_hisp_fsw %>% 
  group_by(year, income) %>% 
  summarize(families = sum(families),
            total = sum(total)) %>% 
  mutate(percent = round(families/total*100,1),
         race = "Hispanic")

# Asian
local_asian_fsw <- local_asian %>% 
  filter(variable != "B19101D_001") %>% 
  mutate(income = variable,
         income = fct_collapse(income,
                               "$0 - $9,999" = "B19101D_002",
                               "$10,000 - $14,999" = "B19101D_003",
                               "$15,000 - $24,999" = c("B19101D_004", "B19101D_005"),
                               "$25,000 - $34,999" = c("B19101D_006", "B19101D_007"),
                               other_level = "$35,000 or more")) %>%
  group_by(year, NAME, income) %>%
  summarize(families = sum(estimate),
            total = first(summary_est)) %>%
  mutate(percent = round(families/total*100,1))

local_asian_fsw_sum <- local_asian_fsw %>% 
  group_by(year, income) %>% 
  summarize(families = sum(families),
            total = sum(total)) %>% 
  mutate(percent = round(families/total*100,1),
         race = "Asian")


# Multiracial
local_multi_fsw <- local_multi %>% 
  filter(variable != "B19101G_001") %>% 
  mutate(income = variable,
         income = fct_collapse(income,
                               "$0 - $9,999" = "B19101G_002",
                               "$10,000 - $14,999" = "B19101G_003",
                               "$15,000 - $24,999" = c("B19101G_004", "B19101G_005"),
                               "$25,000 - $34,999" = c("B19101G_006", "B19101G_007"),
                               other_level = "$35,000 or more")) %>%
  group_by(year, NAME, income) %>%
  summarize(families = sum(estimate),
            total = first(summary_est)) %>%
  mutate(percent = round(families/total*100,1))

local_multi_fsw_sum <- local_multi_fsw %>% 
  group_by(year, income) %>% 
  summarize(families = sum(families),
            total = sum(total)) %>% 
  mutate(percent = round(families/total*100,1),
         race = "Multiracial")


# White
local_white_fsw <- local_white %>% 
  filter(variable != "B19101H_001") %>% 
  mutate(income = variable,
         income = fct_collapse(income,
                               "$0 - $9,999" = "B19101H_002",
                               "$10,000 - $14,999" = "B19101H_003",
                               "$15,000 - $24,999" = c("B19101H_004", "B19101H_005"),
                               "$25,000 - $34,999" = c("B19101H_006", "B19101H_007"),
                               other_level = "$35,000 or more")) %>%
  group_by(year, NAME, income) %>%
  summarize(families = sum(estimate),
            total = first(summary_est)) %>%
  mutate(percent = round(families/total*100,1))

local_white_fsw_sum <- local_white_fsw %>% 
  group_by(year, income) %>% 
  summarize(families = sum(families),
            total = sum(total)) %>% 
  mutate(percent = round(families/total*100,1),
         race = "White")

# join
local_fsw_all <- bind_rows(local_black_fsw_sum,
                           local_hisp_fsw_sum,
                           local_asian_fsw_sum,
                           local_multi_fsw_sum,
                           local_white_fsw_sum)
  

# .................................
# Save data ----
save.image(file = "family_income_work.Rdata")
saveRDS(local_fsw_sum, file = "family_income_all.RDS")
saveRDS(local_fsw_all, file = "family_income_race.RDS")


# .................................
# Visualize community data ----
library(tidyverse)
library(ggthemes)
library(plotly)

fsw <- readRDS("family_income_all.RDS")
fsw_race <- readRDS("family_income_race.rDS")

# Charlottesville MSA, ACS 1-year estimates
fsw_num <- cmsa_fsw %>% 
  filter(income != "$35,000 or more") %>% 
  ggplot(aes(x = year, y = families, fill = income)) +
  geom_area() +
  scale_x_continuous(name = "", breaks = seq(2009, 2019, 1)) +
  scale_fill_brewer(name = "Income Range", palette = "BuPu") +
  labs(y = "# of Families Struggling") +
  theme_minimal() 

fsw_per <- cmsa_fsw %>% 
  filter(income != "$35,000 or more") %>% 
  group_by(year) %>% 
  summarize(families = sum(families),
            total = first(total)) %>% 
  mutate(percent_families = round(families/total*100, 1)) %>% 
  ggplot(aes(x = year, y = percent_families)) +
  geom_line() +
  scale_y_continuous(name = "% of Families Struggling", limits = c(0,30)) +
  scale_x_continuous(name = "", breaks = seq(2009, 2019, 1)) +
  theme_bw()

fsw_num / fsw_per


# Charlottesville region localities, ACS 5-year estimates 
loc_num <- fsw %>% 
  filter(income != "$35,000 or more") %>% 
  ggplot(aes(x = year, y = families, fill = income, label = percent)) +
  geom_area() +
  scale_x_continuous(name = "", breaks = seq(2009, 2019, 1)) +
  scale_fill_brewer(name = "Income Range", palette = "BuPu") +
  labs(y = "# of Families Struggling") +
  theme_minimal() 

loc_num

loc_per <- fsw %>% 
  filter(income != "$35,000 or more") %>% 
  group_by(year) %>% 
  summarize(families = sum(families),
            total = first(total)) %>% 
  mutate(percent_families = round(families/total*100, 1)) %>% 
  ggplot(aes(x = year, y = percent_families, label = families)) +
  geom_line() +
  annotate("text", x = 2019, y = 17.4, label = "15.4%", 
           color = "#88419d", size = 4) +
  coord_cartesian(xlim = c(2009, 2019), 
                  clip = 'off') +   
  scale_y_continuous(name = "% of Families Struggling", limits = c(0,30)) +
  scale_x_continuous(name = "", breaks = seq(2009, 2019, 1)) +
  theme_minimal()

loc_per 

subplot(loc_num, loc_per, nrows = 2, shareX = F, 
        titleX = T, titleY = T, which_layout = 1) %>% 
  layout(
    yaxis = list(
      dtick = 2000, 
      tick0 = 0, 
      tickmode = "linear"
    ))



# Charlottesville MSA, by race, 1-year estimates
# too much missing


# Charlottesville region localities, by race, ACS 5-year estimates 
loc_race_num <- fsw_race %>% 
  filter(income != "$35,000 or more") %>% 
  group_by(year, race) %>% 
  summarize(families = sum(families),
            total = first(total)) %>% 
  mutate(percent_families = round(families/total*100, 1)) %>% 
  ggplot(aes(x = year, y = families, fill = race, label = percent_families)) +
  geom_area() +
  scale_x_continuous(name = "", breaks = seq(2009, 2019, 1)) +
  scale_fill_brewer(name = "Family Race", palette = "Set1") +
  labs(y = "# of Families Struggling") +
  theme_minimal() 

loc_race_data <- fsw_race %>% 
  filter(income != "$35,000 or more") %>% 
  group_by(year, race) %>% 
  summarize(families = sum(families),
            total = first(total)) %>% 
  mutate(percent_families = round(families/total*100, 1))

loc_race_data_2019 <- loc_race_data %>% 
  filter(year == 2019)

loc_race_per <- loc_race_data %>% 
  ggplot(aes(x = year, y = percent_families, color = race, label = families)) +
  geom_line() +
  annotate("text", x = Inf, 
           y = loc_race_data_2019$percent_families, 
           label = loc_race_data_2019$percent_families, 
           size = 3) +
  coord_cartesian(xlim = c(2009, 2019), 
                  clip = 'off') +   
  scale_color_brewer(name = "Family Race", palette = "Set1") +
  scale_y_continuous(name = "% of Families Struggling", breaks = seq(10,60,10)) +
  scale_x_continuous(name = "", breaks = seq(2009, 2019, 1)) +
  theme_minimal()

loc_race_per

subplot(loc_race_num, loc_race_per, nrows = 2, shareX = T, 
        titleX = T, titleY = T, which_layout = 1) %>% 
  layout(
    yaxis = list(
      dtick = 2000, 
      tick0 = 0, 
      tickmode = "linear"
    )) %>% 
  style(showlegend = FALSE, traces = 6:12)

# original
loc_race_num <- local_fsw_all %>% 
  filter(income != "$35,000 or more") %>% 
  group_by(year, race) %>% 
  summarize(families = sum(families),
            total = first(total)) %>% 
  mutate(percent_families = round(families/total*100, 1)) %>% 
  ggplot(aes(x = year, y = families, fill = race, label = percent_families)) +
  geom_area() +
  geom_text(aes(label = n, x = Inf, y = y), hjust = -1)  +
  coord_cartesian(xlim = c(2009, 2019), 
                  clip = 'off') +   
  scale_x_continuous(name = "", breaks = seq(2009, 2019, 1)) +
  scale_fill_brewer(name = "Family Race", palette = "Set1") +
  labs(y = "# of Families Struggling") +
  theme_minimal() 

ggplotly(loc_race_num)

loc_race_per <- local_fsw_all %>% 
  filter(income != "$35,000 or more") %>% 
  group_by(year, race) %>% 
  summarize(families = sum(families),
            total = first(total)) %>% 
  mutate(percent_families = round(families/total*100, 1)) %>% 
  ggplot(aes(x = year, y = percent_families, color = race, label = families)) +
  geom_line() +
  scale_color_brewer(name = "Family Race", palette = "Set1") +
  scale_y_continuous(name = "% of Families Struggling", breaks = seq(10,60,10)) +
  scale_x_continuous(name = "", breaks = seq(2009, 2019, 1)) +
  theme_bw()

ggplotly(loc_race_per)

loc_race_num / loc_race_per

subplot(loc_race_num, loc_race_per, nrows = 2, shareX = TRUE) %>% 
  style(showlegend = FALSE, traces = 6:12)
