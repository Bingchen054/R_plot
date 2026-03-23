library(ggplot2)
library(dplyr)
library(scales)

# ===== 1) Read data =====
corr <- read.csv(
  "/Users/libingchen/Desktop/R_plot/6_Correlation-2/correlation_matrix.csv",
  row.names = 1,
  check.names = FALSE
)
pval <- read.csv(
  "/Users/libingchen/Desktop/R_plot/6_Correlation-2/fixed_p_values.csv",
  row.names = 1,
  check.names = FALSE
)
rval <- read.csv(
  "/Users/libingchen/Desktop/R_plot/6_Correlation-2/fixed_r_values.csv",
  row.names = 1,
  check.names = FALSE
)

corr <- as.matrix(corr)
pval <- as.matrix(pval)
rval <- as.matrix(rval)

mode(corr) <- "numeric"
mode(pval) <- "numeric"
mode(rval) <- "numeric"

# ===== 2) Align names =====
# corr is square; pval/rval are element x spec
vars <- Reduce(intersect, list(
  rownames(corr), colnames(corr),
  rownames(pval), rownames(rval)
))

specs <- Reduce(intersect, list(
  colnames(pval), colnames(rval)
))

if (length(vars) < 2) stop("Too few shared variables in corr/pval/rval.")
if (length(specs) < 1) stop("No shared spec columns between pval and rval.")

corr <- corr[vars, vars, drop = FALSE]
pval <- pval[vars, specs, drop = FALSE]
rval <- rval[vars, specs, drop = FALSE]

n <- length(vars)

# ===== 3) Heatmap data (lower triangle only) =====
heat_df <- expand.grid(row = vars, col = vars, stringsAsFactors = FALSE) %>%
  mutate(
    row_i = match(row, vars),
    col_i = match(col, vars),
    r = corr[cbind(row, col)]
  ) %>%
  filter(row_i >= col_i) %>%
  mutate(label = sprintf("%.2f", r))

# ===== 4) Node positions =====
elem_pos <- data.frame(
  name = vars,
  x = seq_along(vars),
  y = seq_along(vars),
  stringsAsFactors = FALSE
)

spec_pos <- data.frame(
  name = specs,
  x = n + 1.7,
  y = seq(length(specs), 1, length.out = length(specs)),
  stringsAsFactors = FALSE
)

# ===== 5) Edge table =====
edge_df <- expand.grid(
  element = vars,
  spec = specs,
  stringsAsFactors = FALSE
) %>%
  mutate(
    p = pval[cbind(element, spec)],
    r = rval[cbind(element, spec)],
    p_cat = cut(
      p,
      breaks = c(-Inf, 0.01, 0.05, Inf),
      labels = c("< 0.01", "0.01 – 0.05", "≥ 0.05")
    ),
    r_cat = cut(
      abs(r),
      breaks = c(-Inf, 0.2, 0.4, Inf),
      labels = c("< 0.2", "0.2 – 0.4", "≥ 0.4")
    ),
    sign_cat = ifelse(r >= 0, "positive", "negative")
  ) %>%
  filter(!is.na(p), !is.na(r)) %>%
  left_join(elem_pos, by = c("element" = "name")) %>%
  rename(xend = x, yend = y) %>%
  left_join(spec_pos, by = c("spec" = "name")) %>%
  rename(x = x, y = y) %>%
  mutate(
    p_cat = factor(p_cat, levels = c("< 0.01", "0.01 – 0.05", "≥ 0.05")),
    r_cat = factor(r_cat, levels = c("< 0.2", "0.2 – 0.4", "≥ 0.4")),
    sign_cat = factor(sign_cat, levels = c("positive", "negative"))
  )

# ===== 6) Plot =====
p <- ggplot() +
  geom_tile(
    data = heat_df,
    aes(x = match(col, vars), y = match(row, vars), fill = r),
    color = "white",
    linewidth = 0.35
  ) +
  geom_text(
    data = heat_df,
    aes(x = match(col, vars), y = match(row, vars), label = label),
    size = 2.6,
    color = "black"
  ) +
  geom_curve(
    data = edge_df,
    aes(
      x = x, y = y, xend = xend, yend = yend,
      colour = p_cat, linewidth = r_cat, linetype = sign_cat
    ),
    curvature = 0.14,
    alpha = 0.55,
    lineend = "butt",
    linejoin = "round"
  ) +
  geom_point(
    data = elem_pos,
    aes(x = x, y = y),
    shape = 21, size = 3.2,
    fill = "pink", colour = "#999999",
    stroke = 0.4
  ) +
  geom_point(
    data = spec_pos,
    aes(x = x, y = y),
    shape = 21, size = 3.2,
    fill = "pink", colour = "#999999",
    stroke = 0.4
  ) +
  geom_text(
    data = spec_pos,
    aes(x = x + 0.35, y = y, label = name),
    hjust = 0, size = 4, family = "Times New Roman"
  ) +
  scale_fill_gradient2(
    name = "Pearson's r",
    low = "#5bc2cd", mid = "white", high = "#ffa273",
    limits = c(-1, 1)
  ) +
  scale_colour_manual(
    name = "Mantel's p",
    values = c(
      "< 0.01" = "#d9d9d9",
      "0.01 – 0.05" = "#b5e3e8",
      "≥ 0.05" = "#ffc9ad"
    )
  ) +
  scale_linewidth_manual(
    name = "Mantel's r",
    values = c(
      "< 0.2" = 0.35,
      "0.2 – 0.4" = 0.70,
      "≥ 0.4" = 1.05
    )
  ) +
  scale_linetype_manual(
    values = c("positive" = "solid", "negative" = "dashed"),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = 1:n,
    labels = vars,
    expand = c(0, 0)
  ) +
  scale_y_reverse(
    breaks = 1:n,
    labels = vars,
    expand = c(0, 0)
  ) +
  coord_equal(
    xlim = c(0.5, n + 6),
    clip = "off"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    legend.box = "vertical",
    plot.margin = margin(5, 50, 5, 5)
  ) +
  guides(
    colour = guide_legend(order = 1, override.aes = list(linewidth = 1.2, linetype = "solid")),
    linewidth = guide_legend(order = 2, override.aes = list(colour = "black")),
    fill = guide_colorbar(order = 3)
  )

print(p)

ggsave(
  "/Users/libingchen/Desktop/R_plot/6_Correlation-2/network_heatmap.png",
  p,
  width = 8,
  height = 8,
  dpi = 300
)