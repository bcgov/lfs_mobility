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
    group_by(noc_plus_title)|>
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
                         y=fct_reorder(noc_plus_title, difference, .desc = TRUE)),
             alpha=.5)+
    geom_point(data=data,
               mapping=aes(x=difference,
                           y=fct_reorder(noc_plus_title, difference, .fun=mean, .desc = TRUE),
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

make_base_plot <- function(lst, cost, max_size){
 # browser()
  subtitle <- if(!is.na(cost)){
    paste0("Total Cost = ",fmt_auto(cost))
  }else{
    NULL
  }
  ggplot(mapping = aes(x = x, y = y, size = mass)) +
    geom_point(data = lst$bottom, alpha = 0.1) +
    geom_point(data = lst$top8,
               mapping = aes(fill = fct_reorder(paste0(from,"\n",to,"\n"), mass, .desc = TRUE)),
               shape = 21,
               alpha = 0.9) +
    scale_fill_brewer(
      palette = "Dark2",
      guide = guide_legend(override.aes = list(size = 6))
    ) +
    scale_size_continuous(range = c(.5, 30), limits = c(0, max_size), guide = "none") +
    coord_equal() +
    theme_minimal(base_size = 14) +
    labs(
      title = "Year: {round(frame_time,1)}",
      subtitle = subtitle,
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

convert_to_teer <- function(lst){
  lst$plan|>
    mutate(to=str_sub(to_id,2,2),
           from=str_sub(from_id,2,2))|>
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


#' Wrapper for weighted pairwise potentials with ID preservation
#'
#' Computes weighted pairwise potentials using wpp() after
#' removing zero-mass observations, and re-attaches the original
#' row identifiers to the result.
#'
#' @param coords A numeric matrix or data frame of coordinates.
#'   Rows correspond to units; columns to dimensions.
#' @param mass A data frame or tibble containing mass variables.
#'   Row names must align with \code{coords}.
#' @param var A single character string giving the column name in
#'   \code{mass} to be used as weights.
#'
#' @details
#' Observations with non-positive mass are excluded prior to calling
#' wpp(), as required by the underlying algorithm. The output
#' is augmented with original_id, preserving the row names of
#' the retained observations from mass.
#'
#' @return
#' An object returned by wpp(), with an additional column
#' original_id identifying the original rows used in the
#' computation.
#'
#' @seealso wpp
wpp_wrapper <- function(coords, mass, var){
  ids <- row.names(mass)
  mass <- mass[[var]]
  keep <- which(mass > 0)
  w <- wpp(coords[keep, , drop=FALSE], mass[keep])
  w$original_id <- ids[keep]
  return(w)
}

unbalanced_wrapper <- function(from, to, ...) {
  res <- transport::unbalanced(from, to, ...)
  plan <- res$plan
  from_ids <- from$original_id
  to_ids   <- to$original_id
  plan$from_id <- from_ids[plan$from]
  plan$to_id   <- to_ids[plan$to]
  plan <- plan[, c("from_id", "to_id", "mass")]
  aextra <- data.frame(
    from = seq_along(res$aextra),
    from_id = from_ids,
    extra_mass = res$aextra
  )
  bextra <- data.frame(
    to = seq_along(res$bextra),
    to_id = to_ids,
    extra_mass = res$bextra
  )
  list(
    plan   = plan,
    aextra = aextra,
    bextra = bextra,
    cost   = res$dist
  )
}

hier_wrapper <- function(from, to){
  a <- setNames(from$from_prop, rownames(from))
  b <- setNames(to$to_prop, rownames(to))
  costm <- alternative_distance[names(a), names(b), drop = FALSE]
  sol <- transport::transport(a, b, costm)
  transform(
    sol,
    from_id = names(a)[from],
    to_id   = names(b)[to]
  )[, c("from_id", "to_id", "mass")]
}


make_segment_data <- function(transitions, coordinates){
  transitions$plan|>
    left_join(coordinates, by = c("from_id" = "noc_2021")) %>%
    rename(x_from = V1, y_from = V2) %>%
    left_join(coordinates, by = c("to_id" = "noc_2021")) %>%
    rename(x_to = V1, y_to = V2)
}

make_long <- function(tbbl){
  tbbl <- tbbl|>
    left_join(skills[["nocs_we_want"]], by=c("from_id"="noc_2021"))|>
    select(-from_id)|>
    rename(from=noc_plus_title)|>
    left_join(skills[["nocs_we_want"]], by=c("to_id"="noc_2021"))|>
    select(-to_id)|>
    rename(to=noc_plus_title)
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
fmt_auto <- function(x, digits = 1) {
  sapply(x, function(val) {
    abs_val <- abs(val)

    if (abs_val >= 1e9) {
      paste0(round(val / 1e9, digits), "B")
    } else if (abs_val >= 1e6) {
      paste0(round(val / 1e6, digits), "M")
    } else if (abs_val >= 1e3) {
      paste0(round(val / 1e3, digits), "K")
    } else {
      format(val, big.mark = ",", scientific = FALSE)
    }
  })
}

max_mass <- function(x) {
  purrr::map(x, "plan") |>
    purrr::map("mass") |>
    purrr::map_dbl(max) |>
    max()
}

plot_extra <- function(lst, movie_name, component, xlab, join_var){
  tbbl <- skills$nocs_we_want|>
    inner_join(lst[[component]], by = setNames(join_var, "noc_2021"))|>
    filter(extra_mass>0)|>
    slice_max(extra_mass, n=40)|>
    left_join(priority)
  if(join_var=="from_id"){
    plt <- ggplot(tbbl, aes(extra_mass, fct_reorder(noc_plus_title, extra_mass)))
  }else{
    plt <- ggplot(tbbl, aes(extra_mass, fct_reorder(noc_plus_title, extra_mass), fill=category))
  }
  plt+
    geom_col(alpha=.5)+
    scale_x_continuous(labels=scales::comma, limits = c(0,120000))+
    theme_minimal()+
    labs(title=paste(xlab, "for", movie_name),
         y=NULL,
         x=xlab,
         fill="Immigration")
}

join_transitions <- function(props, hier){

  skill_transitions <- props[["plan"]]|>
    filter(from_id!=to_id)|>
    arrange(desc(mass))|>
    rename(`Based on skill`=mass)

  hier_transtitions <- hier|>
    filter(from_id!=to_id)|>
    arrange(desc(mass))|>
    rename(`Based on hierarchy`=mass)

  full_join(skill_transitions, hier_transtitions)|>
    left_join(skills[["nocs_we_want"]], by=c("from_id"="noc_2021"))|>
    mutate(from=str_trunc(noc_plus_title, width=50))|>
    select(-noc_plus_title)|>
    left_join(skills[["nocs_we_want"]], by=c("to_id"="noc_2021"))|>
    mutate(to=str_trunc(noc_plus_title, width=50))|>
    select(-noc_plus_title)|>
    unite(label, from, to, sep = "->")
}

get_top <- function(joined){

  top_both <- joined|>
    na.omit()|>
    select(-from_id, -to_id)|>
    mutate(mass_sum=`Based on hierarchy`+`Based on skill`)|>
    slice_max(mass_sum, n=10)|>
    select(-mass_sum)|>
    pivot_longer(cols=-label)|>
    mutate(facet="Both")

  top_hier <- joined|>
    slice_max(`Based on hierarchy`,  n=10)|>
    select(`Based on hierarchy`, label)|>
    pivot_longer(cols=-label)|>
    mutate(facet="Hierarchy Distance")

  top_skill <- joined|>
    slice_max(`Based on skill`,  n=10)|>
    select(`Based on skill`, label)|>
    pivot_longer(cols=-label)|>
    mutate(facet="Skill Distance")

  bind_rows(top_both, top_skill, top_hier)
}

top_plot <- function(top, title){

  top|>
    ggplot(aes(value,
               tidytext::reorder_within(label, value, facet),
               fill=name))+
    geom_col(position = "dodge")+
    scale_x_continuous(labels=scales::percent, limits = c(0, .02))+
    labs(x="Mass Transfer",
         y=NULL,
         fill=NULL,
         subtitle=paste("Hierarchical vs Skill: Top transfers", title))+
    theme(legend.position = "bottom")+
    facet_wrap(~facet, ncol=1, scales="free_y")+
    tidytext::scale_y_reordered()
}


