# Creation of Summary Plots for each factor ----

# Set options
options(echo = FALSE)

# Source to data preparation
source("data-prepare.R")

# Set theme
theme_set(theme_minimal(base_family = "Roboto Condensed"))

myTheme <- theme(
  plot.title = element_text(size = 22, colour = "steelblue"),
  plot.subtitle = element_text(size = 18),
  panel.grid = element_blank(),
  panel.grid.major.y = element_line(colour = "grey70", linetype = "dotted", size = .4)
)

# Annual Revenue

annual_revenue_plot <- data_tbl %>%
  select(order_date, total_revenue) %>%
  mutate(year = order_date %>% year()) %>%
  group_by(year) %>%
  summarise(revenue = sum(total_revenue), .groups = "drop") %>%
  ggplot(aes(year, revenue)) +
  geom_col(fill = "yellowgreen") +
  labs(
    title = "Annual Revenue",
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, .4)), labels = scales::comma_format(scale = 1e-3, prefix = "£", suffix = "k")) +
  myTheme


annual_revenue_plot
