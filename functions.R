read_data <- function(file_name){
  read_excel(here("data", "onet", file_name))%>%
    clean_names()%>%
    select(o_net_soc_code, element_name, scale_name, data_value)%>%
    pivot_wider(names_from = scale_name, values_from = data_value)%>%
    mutate(score=sqrt(Importance*Level), #geometric mean of importance and level
           #mutate(score=Level,
           category=(str_split(file_name,"\\.")[[1]][1]))%>%
    unite(element_name, category, element_name, sep=": ")%>%
    select(-Importance, -Level)
}

age_to_num <- function(x) as.numeric(word(x, 1, sep = "-"))


get_large_diffs <- function(tbbl, cut_off){
  tbbl|>
    group_by(from_noc, to_noc)|>
    summarize(from_prop=mean(from_prop),
              to_prop=mean(to_prop)
    )|>
    mutate(difference=to_prop-from_prop)|>
    filter(abs(difference)>cut_off)
}

source_dest_plot <- function(age, data, aggregated){
  ggplot()+
    geom_col(data=aggregated,
             mapping=aes(x=difference,
                         y=fct_reorder(from_noc, difference, .desc = TRUE)),
             alpha=.5)+
    geom_point(data=data,
               mapping=aes(x=difference,
                           y=fct_reorder(from_noc, difference, .fun=mean, .desc = TRUE),
                           colour=factor(from_year)))+
    labs(title=paste("Changes in occupational proportions greater than", cut_off),
         subtitle=paste("Transition from age bracket ", age, " to ", data$age_in_to_year[[1]]),
         x="Difference in Proportion",
         y=NULL,
         colour="Starting Year",
         caption="Source: Labour Force Survey via RTRA")+
    theme_minimal(base_size = 8)+
    coord_cartesian(xlim = c(-.075,.025))
}

get_cost <- function(tbbl, distance_matrix){
  sum(tbbl$mass * distance_matrix[cbind(tbbl$from, tbbl$to)])
}

arrange_and_pull <- function(tbbl, noc, prop){
  tbbl|>
    ungroup()|>
    arrange({{  noc  }})|>
    pull({{  prop  }})
}

naive_transport <- function(from, to){
  names(from) <- 1:length(from)
  names(to) <- 1:length(to)
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
    mutate(from=as.integer(from),
           to=as.integer(to))
}


make_segment_data <- function(transitions, coordinates){
  transitions|>
    left_join(coordinates, by = c("from" = "index")) %>%
    rename(x_from = V1, y_from = V2) %>%
    left_join(coordinates, by = c("to" = "index")) %>%
    rename(x_to = V1, y_to = V2)
}

make_long <- function(tbbl){
  tbbl <- tbbl|>
    left_join(skills$index_to_noc, by=c("from"="index"))|>
    select(-from)|>
    rename(from=noc)|>
    left_join(skills$index_to_noc, by=c("to"="index"))|>
    select(-to)|>
    rename(to=noc)
  initial <- tbbl|>
    select(from, to, mass, x=x_from, y=y_from)|>
    mutate(time=0)
  subsequent <- tbbl|>
    select(from, to, mass, x=x_to, y=y_to)|>
    mutate(time=10)
  long_segments <- bind_rows(initial, subsequent)
  top8 <- slice_max(long_segments[long_segments$from!=long_segments$to,], order_by = mass, n=16)
  bottom <- anti_join(long_segments, top8)
  list(top8=top8, bottom=bottom)
}

make_base_plot <- function(lst, cost, size_limits = NULL){
  ggplot(mapping = aes(x = x, y = y, size = mass)) +
    geom_point(data = lst$bottom, alpha = 0.05) +
    geom_point(data = lst$top8,
               mapping = aes(fill = fct_reorder(paste0(from,"\n",to,"\n"), mass, .desc = TRUE)),
               shape = 21,
               alpha = 0.9) +
    scale_fill_brewer(
      palette = "Dark2",
      guide = guide_legend(override.aes = list(size = 6))
    ) +
    scale_size_continuous(range = c(.5, 30), limits = size_limits, guide = "none") +
    coord_equal() +
    theme_minimal(base_size = 14) +
    labs(
      title = "Year: {round(frame_time,1)}",
      subtitle = paste0("Total Cost = ",round(cost,2)),
      fill = "Top 8 transfers",
      x = NULL,
      y = NULL
    ) +
    theme(
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 22)
    ) +
    transition_time(time) +
    ease_aes("linear")
}

convert_to_teer <- function(tbbl){
  tbbl <- tbbl|>
    left_join(skills$index_to_noc, by=c("from"="index"))|>
    select(-from)|>
    rename(from=noc)|>
    left_join(skills$index_to_noc, by=c("to"="index"))|>
    select(-to)|>
    rename(to=noc)
  tbbl|>
    mutate(to=str_sub(to,2,2),
           from=str_sub(from,2,2))|>
    group_by(to, from)|>
    summarize(mass=sum(mass))
}
alluvial_plot <- function(tbbl, initial_age, subsequent_age, cost){
  tbbl|>
    ggplot(aes(axis1 = from, axis2 = to, y = mass)) +
    geom_alluvium(aes(fill = from)) +
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = paste(after_stat(stratum)))) +
    scale_x_discrete(limits = str_replace_all(c(initial_age, subsequent_age), "_"," "))+
    theme_minimal()+
    labs(title=paste0("TEER flows between adjacent age brackets: min(cost)=", round(cost,2)),
         x="Age bracket",
         y=NULL,
         fill="Initial TEER"
    )+
    theme(
      panel.grid = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    )
}

animate_wrapper <- function(plots, names, folder){
  mclapply(
    seq_along(plots),
    function(i) {
      anim_save(
        here("out", folder, paste0(names[i], ".mp4")),
        animation = animate(
          plots[[i]],
          nframes = 240,
          fps = 20,
          width = 1920,
          height = 1080,
          renderer = ffmpeg_renderer()
        ),
        extra_args = c("-c:v", "libx264", "-pix_fmt", "yuv420p")
      )
      NULL
    },
    mc.cores = parallel::detectCores() - 1
  )
}


