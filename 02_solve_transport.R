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
#read data-------------------
priority <- read_csv(here("data", "canadian_immigration_priority_nocs.csv"))|>
  clean_names()|>
  mutate(noc_2021=str_pad(noc_2021, width=5, side="left", pad="0"),
         category="Priority")
skills <- read_rds(here("out", "skills.rds"))
alternative_distance <- read_rds(here("out", "alternative_distance.rds"))
lfs <- list()

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
  arrange(noc_5)|>
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
  arrange(noc_5)|>
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
         props=map2(source_props, target_props, unbalanced_wrapper, p=1, C=10*skills$mean_dist, output="all"), #default distance^(p=2), big C no +/- (balanced)
         props_teer=map(props, convert_to_teer),
         props_cost=map(props, "cost"),
         teer_alluvium_plot=pmap(list(props_teer, age_in_from_year, age_in_to_year, props_cost), alluvial_plot),
         max_prop= max_mass(props),
         prop_segments=map2(props, mds_coords, make_segment_data),
         prop_long=map(prop_segments, make_long),
         prop_base_plot=pmap(list(prop_long, props_cost, max_prop),  make_base_plot),
         source_counts=map2(first_ten, from, wpp_wrapper, "from_count"),
         target_counts=map2(first_ten, to, wpp_wrapper, "to_count"),
         counts=map2(source_counts, target_counts, unbalanced_wrapper, p=1, C=skills$mean_dist, output="all"),
         counts_cost=map(counts, "cost"), #set to NA_real_ to suppress in plots
         max_count= max_mass(counts),
         count_segments=map2(counts, mds_coords, make_segment_data),
         count_long=map(count_segments, make_long),
         count_base_plot=pmap(list(count_long, counts_cost, max_count), make_base_plot),
         aextra=map2(counts, movie_name, plot_extra, "aextra", "Deletions", "from_id"),
         bextra=map2(counts, movie_name, plot_extra, "bextra", "Insertions", "to_id"),
         hier=map2(from, to, hier_wrapper),
         joined=map2(props, hier, join_transitions),
         top=map(joined, get_top),
         top_plot=map2(top, movie_name, top_plot)
         )

results$tbbl|>
  select(teer_alluvium_plot)|>
  write_rds(here("out", "alluvium_plots.rds"))

results$tbbl|>
  select(aextra, bextra)|>
  write_rds(here("out", "extras.rds"))

results$tbbl|>
  select(top_plot)|>
  write_rds(here("out", "top_plot.rds"))


#animate_wrapper(results$tbbl$prop_base_plot, results$tbbl$movie_name, "props")
#animate_wrapper(results$tbbl$count_base_plot, results$tbbl$movie_name, "counts")
