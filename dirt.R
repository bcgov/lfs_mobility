library(tidyverse)
library(transport)
library(janitor)
library(here)

coords <- tibble(name=LETTERS[1:4], x=c(0,0,1,1), y=c(0,1,0,1), from=1:4)

dist <- coords|>
  select(-from)|>
  column_to_rownames("name")|>
  dist()|>
  as.matrix()

dist_long <- dist|>
  as.data.frame()|>
  rownames_to_column("from")|>
  pivot_longer(cols=-from, names_to = "to", values_to = "distance")

from <- as.numeric(1:4)
names(from) <- coords$name
to <- rep(2.5, 4)
names(to) <- coords$name

opt_mass <- transport(from, to, dist)|>
  mutate(from=coords$name[from],
         to=coords$name[to])

naive_transport <- function(from, to){
  net_supply <- (from-to)[from-to>0]
  net_demand <- (to-from)[from-to<0]
  stopifnot(near(sum(net_demand), sum(net_supply)))
  stationary <- full_join(enframe(from), enframe(net_supply), by="name")|>
    mutate(value.y=if_else(is.na(value.y), 0, value.y),
           mass=value.x-value.y,
           from=name,
           to=name)|>
    select(from, to, mass)
  demand_props <- net_demand/sum(net_demand)
  net_supply <- enframe(net_supply, name="from", value="net_supply")
  demand_props <- enframe(demand_props, name="to", value="proportion")
  crossing(net_supply, demand_props)|>
    mutate(mass=net_supply*proportion)|>
    select(from, to, mass)|>
    bind_rows(stationary)|>
    arrange(from, to)
}

naive_mass <- naive_transport(from, to)

#calculate transport costs-----------------------

opt_cost <- left_join(opt_mass, dist_long)|>
  mutate(cost=mass*distance)|>
  filter(cost>0)|>
  adorn_totals()

naive_cost <- left_join(naive_mass, dist_long)|>
  mutate(cost=mass*distance)|>
  filter(cost>0)|>
  adorn_totals()

list(naive_cost=naive_cost, opt_cost=opt_cost)|>
  write_rds(here("out", "dirt.rds"))








