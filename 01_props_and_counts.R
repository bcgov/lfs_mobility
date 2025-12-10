# Copyright 2025 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(tidyverse)
library(here)
library(janitor)
library(vroom)
library(plotly)
library(assertthat)
library(readxl)
library(transport)
library(gganimate)
library(parallel)
library(ggalluvial)
library(conflicted)
conflicts_prefer(plotly::filter)
conflicts_prefer(vroom::cols)
conflicts_prefer(vroom::col_double)
conflicts_prefer(vroom::col_character)
#constants-------------------------------
cut_off <- .004 #for source destination plot
#functions--------------------------
source("functions.R")
#storage lists for objects------------------------------
lfs <- list()
skills <- list()
#read in skills data---------------------------------------
skills$mapping <- read_excel(here("data","mapping", "onet2019_soc2018_noc2016_noc2021_crosswalk_consolodated.xlsx"))%>%
  mutate(noc_2021=str_pad(noc2021, "left", pad="0", width=5))%>%
  select(noc_2021, noc2021_title, o_net_soc_code = onetsoc2019)%>%
  distinct()

skills$nocs_we_want <- skills$mapping |>
  select(noc_2021, noc2021_title) |>
  distinct()|>
  mutate(noc_plus_title=paste(noc_2021, noc2021_title, sep=": "))|>
  select(noc_2021, noc_plus_title)

skills$onet_raw <- tibble(file=c("Skills.xlsx", "Abilities.xlsx", "Knowledge.xlsx", "Work Activities.xlsx"))%>%
  mutate(data=map(file, read_data))%>%
  select(-file)%>%
  unnest(data)%>%
  pivot_wider(id_cols = o_net_soc_code, names_from = element_name, values_from = score)%>%
  inner_join(skills$mapping)%>%
  ungroup()%>%
  select(-o_net_soc_code, noc2021_title)%>%
  select(noc_2021, everything())

skills$onet_mapped <- skills$onet_raw|>
  group_by(noc_2021)%>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

skills$two_digit <- skills$onet_raw|>
  mutate(noc_two=str_sub(noc_2021,1, 2))|>
  group_by(noc_two)|>
  summarise(across(contains(":"), ~mean(.x, na.rm = TRUE)))

skills$four_digit <- skills$onet_raw|>
  mutate(noc_four=str_sub(noc_2021,1, 4))|>
  group_by(noc_four)|>
  summarise(across(contains(":"), ~mean(.x, na.rm = TRUE)))

skills$missing_four <- anti_join(skills$nocs_we_want, skills$onet_raw|>select(noc_2021))|>
  mutate(noc_four=str_sub(noc_2021, 1, 4),
         .after=noc_2021)|>
  inner_join(skills$four_digit)|>
  select(-noc_four, -noc_plus_title)

skills$missing_two <- anti_join(skills$nocs_we_want, skills$onet_raw|>select(noc_2021))|>
  anti_join(skills$missing_four|>select(noc_2021))|>
  mutate(noc_two=str_sub(noc_2021, 1, 2),
         .after=noc_2021)|>
  inner_join(skills$two_digit)|>
  select(-noc_two, -noc_plus_title)

skills$onet_full <- bind_rows(skills$onet_mapped, skills$missing_four, skills$missing_two)|>
  ungroup()|>
  arrange(noc_2021)|>
  column_to_rownames("noc_2021")

skills$onet_pca <- prcomp(skills$onet_full, center=TRUE, scale=TRUE)
skills$noc_coords <- skills$onet_pca$x[, 1:10]#keep first 10 components
skills$skills_noc_dist<- dist(skills$noc_coords, method = "euclidean")|>
  as.matrix()
skills$max_dist <- dist(skills$noc_coords, method = "euclidean")|>
  max()

skills$mds2 <- cmdscale(skills$skills_noc_dist, k = 2)|>
  as.data.frame()

#read in lfs data

lfs$lfs_data <- vroom(here("data","lfs", list.files(here("data","lfs"))),
                      col_types = cols(
                        SYEAR = col_double(),
                        NOC_5 = col_character(),
                        AGE10 = col_character(),
                        `_COUNT_` = col_double()
                      ))|>
  na.omit()|>
  clean_names()|>
  filter(age10!="nwa",
         noc_5!="no_no",
         syear<max(syear))|>
  mutate(noc_5=if_else(noc_5 %in% paste0("000",11:15), "00018", noc_5))|>
  group_by(syear, age10, noc_5)|>
  summarize(count=sum(count)/12)|>
  semi_join(skills$nocs_we_want, by=c("noc_5"="noc_2021"))

lfs$from <- lfs$lfs_data|>
  filter(syear<2015,
         age10!="55-64")|> #retired in end year
  group_by(syear, age10)|>
  mutate(prop=count/sum(count))|>
  select(from_year=syear,
         age_in_from_year=age10,
         from_noc=noc_5,
         from_prop=prop)

lfs$to <- lfs$lfs_data|>
  filter(syear>2020,
         age10!="15-24")|> #babies in start year
  group_by(syear, age10)|>
  mutate(prop=count/sum(count))|>
  select(to_year=syear,
         age_in_to_year=age10,
         to_noc=noc_5,
         to_prop=prop)

#analysis-------------------------------------

results <- list()

results$source_dest_plots <- bind_cols(lfs$from, lfs$to)

#bind_cols is fragile... check binding
assert_that(all.equal(results$source_dest_plots$from_noc, results$source_dest_plots$to_noc))
assert_that(all(results$source_dest_plots$to_year - results$source_dest_plots$from_year == 10)) #end year 10 years later
assert_that(all(age_to_num(results$source_dest_plots$age_in_to_year) -
                  age_to_num(results$source_dest_plots$age_in_from_year) == 10)) #end age 10 years older

results$source_dest_plots <- results$source_dest_plots|>
  mutate(difference=to_prop-from_prop)|>
  left_join(skills$nocs_we_want, by=c("from_noc"="noc_2021"))|>
  group_by(age_in_from_year)|>
  nest()|>
  mutate(large_diffs=map(data, get_large_diffs, cut_off),
         data=map2(data, large_diffs, semi_join, by = join_by(noc_plus_title)),
         plot=pmap(list(age_in_from_year, data, large_diffs), source_dest_plot)
  )

write_rds(results$source_dest_plots, here("out","source_dest_plots.rds"))

#aggregate across years-------------------

results$from_sorted <- lfs$lfs_data|>
  filter(syear<2015,
         age10!="55-64")|> #retired 10 years later
  group_by(age10, noc_5)|>
  summarize(count=mean(count))|>  # 4 year mean
  group_by(age10)|>
  mutate(prop=count/sum(count))|>
  select(noc_5,
         age_in_from_year=age10,
         from_prop=prop,
         from_count=count)|>
  group_by(age_in_from_year)|>
  nest()|>
  rename(from=data)|>
  mutate(from=map(from, column_to_rownames, "noc_5"))

results$to_sorted <- lfs$lfs_data|>
  filter(syear>2020,
         age10!="15-24")|> #babies in start year
  group_by(age10, noc_5)|>
  summarize(count=mean(count))|> # 4 year mean
  group_by(age10)|>
  mutate(prop=count/sum(count))|>
  select(age_in_to_year=age10,
         noc_5,
         to_prop=prop,
         to_count=count)|>
  group_by(age_in_to_year)|>
  nest()|>
  rename(to=data)|>
  mutate(to=map(to, column_to_rownames, "noc_5"))


results$tbbl <- bind_cols(results$from_sorted, results$to_sorted)|>
  ungroup()|>
  mutate(movie_name=paste(age_in_from_year, age_in_to_year, sep=" to "),
         mds_coords=list(skills$mds2),
         first_ten=list(skills$noc_coords),
         source_props=map2(first_ten, from, wpp_wrapper, "from_prop"),
         target_props=map2(first_ten, to, wpp_wrapper, "to_prop"),
         props=map2(source_props, target_props, unbalanced_wrapper, p=1, C=skills$max_dist, output="all"), #default is distance^(p=2), big C no +/-
         props_cost=map(props, "cost"),
         max_prop=map(props, "plan"),
         max_prop=map(max_prop, "mass"),
         max_prop=map_dbl(max_prop, max),
         max_prop=max(max_prop),
         prop_segments=map2(props, mds_coords, make_segment_data),
         prop_long=map(prop_segments, make_long),
         prop_base_plot=pmap(list(prop_long, props_cost, max_prop),  make_base_plot),
         source_counts=map2(first_ten, from, wpp_wrapper, "from_count"),
         target_counts=map2(first_ten, to, wpp_wrapper, "to_count"),
         counts=map2(source_counts, target_counts, unbalanced_wrapper, p=1, C=skills$max_dist/2, output="all"),
         counts_cost=map(counts, "cost"), #set to NA_real_ to suppress in plots
         max_count=map(counts, "plan"),
         max_count=map(max_count, "mass"),
         max_count=map_dbl(max_count, max),
         max_count=max(max_count),
         count_segments=map2(counts, mds_coords, make_segment_data),
         count_long=map(count_segments, make_long),
         count_base_plot=pmap(list(count_long, counts_cost, max_count), make_base_plot)
         )

animate_wrapper(results$tbbl$prop_base_plot, results$tbbl$movie_name, "props")
animate_wrapper(results$tbbl$count_base_plot, results$tbbl$movie_name, "counts")













