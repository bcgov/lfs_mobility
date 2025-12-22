library(ggplot2)
library(ggpattern)

# --- Geometry ---
x0 <- 2
y0 <- 8

v_drop <- 2.5
p1 <- c(x0, y0)
p2 <- c(x0, y0 - v_drop)

L2 <- 3.0
p3 <- p2 + c(L2 * cos(pi/3), -L2 * sin(pi/3))

L3 <- 4.0
p4 <- p3 + c(L3 * cos(pi/6), -L3 * sin(pi/6))

flat_right <- 3.5
p5 <- p4 + c(flat_right, 0)

pts <- data.frame(
  x = c(p1[1], p2[1], p3[1], p4[1], p5[1]),
  y = c(p1[2], p2[2], p3[2], p4[2], p5[2])
)

# --- Canvas and cutoff ---
x_cut <- 3.5
x_max <- max(pts$x) + 1
y_max <- max(pts$y) + 1

# --- Full feasible region (above polyline) ---
shade_full <- rbind(
  pts,
  data.frame(x = rev(pts$x), y = y_max),
  pts[1, , drop = FALSE]
)

# --- Clip polyline at x_cut (for constrained region) ---
pts_clip <- pts[pts$x <= x_cut, , drop = FALSE]

if (nrow(pts_clip) < nrow(pts)) {
  last_pt <- tail(pts_clip, 1)
  next_pt <- pts[nrow(pts_clip) + 1, , drop = FALSE]

  t <- (x_cut - last_pt$x) / (next_pt$x - last_pt$x)
  y_cut <- last_pt$y + t * (next_pt$y - last_pt$y)

  pts_clip <- rbind(pts_clip, data.frame(x = x_cut, y = y_cut))
}

shade_intersection <- rbind(
  pts_clip,
  data.frame(x = rev(pts_clip$x), y = y_max),
  pts_clip[1, , drop = FALSE]
)

# --- Iso-cost slope (flat so global optimum lies right of cutoff) ---
theta <- 10                     # degrees from horizontal
m <- -tan(theta * pi / 180)

# For iso-costs y = m x + b, minimizing cost = minimizing b = y - m x
b_all <- with(pts, y - m * x)
global_opt <- pts[which.min(b_all), , drop = FALSE]
b_global <- global_opt$y - m * global_opt$x

b_con <- with(pts_clip, y - m * x)
constr_opt <- pts_clip[which.min(b_con), , drop = FALSE]

b_constr <- constr_opt$y - m * constr_opt$x

x_grid <- seq(0, x_max, length.out = 300)

isocost <- rbind(
  data.frame(
    x = x_grid,
    y = m * x_grid + b_global,
    type = "Global optimum"
  ),
  data.frame(
    x = x_grid,
    y = m * x_grid + b_constr,
    type = "Greedy optimum"
  )
)

# --- Plot ---
plt <- ggplot() +
  # Full feasible region
  geom_polygon(
    data = shade_full,
    aes(x, y),
    fill = "steelblue",
    alpha = 0.18
  ) +
  # Constrained subset
  geom_polygon_pattern(
    data = shade_intersection,
    aes(x, y),
    pattern = "crosshatch",
    pattern_density = 0.5,
    pattern_spacing = 0.04,
    pattern_colour = "steelblue",
    fill = NA
  ) +
  # Iso-cost curves (COLOUR, not linetype)
  geom_line(
    data = isocost,
    aes(x, y, colour = type),
    linewidth = 1.2
  ) +
  # Constraint boundary
  geom_path(
    data = pts,
    aes(x, y),
    linewidth = 1.2,
    lineend = "round"
  ) +
  geom_point(data = pts, aes(x, y), size = 2) +
  geom_vline(xintercept = x_cut, linetype = 2) +
  coord_equal(
    xlim = c(0, x_max),
    ylim = c(0, y_max),
    expand = FALSE
  ) +
  labs(
    subtitle = "Greedy search imposes artificial constraints.",
    x = NULL,
    y = NULL,
    colour = NULL
  ) +
  theme_minimal(base_size = 13)+
  theme(legend.position = "bottom")

write_rds(plt, here("out","greedy_vs_optimal.rds"))
















