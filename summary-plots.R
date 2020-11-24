# Creation of Summary Plots for each factor ----

# Set options
options(echo = FALSE)

# Source to data preparation
source("data-prepare.R")

# Set theme
theme_set(theme_minimal(base_family = "Roboto Condensed", base_size = 20))

# Custom theme settings
myTheme <- theme(
  plot.margin = unit(c(.3,1,.3,0), "cm"),
  plot.title = element_text(size = 22, colour = "steelblue", face = "bold"),
  plot.subtitle = element_text(size = 16),
  plot.caption = element_text(size = 11, colour = "grey70", hjust = 0),
  panel.spacing.y = unit(.5, "cm"),
  panel.grid = element_blank(),
  panel.grid.major.y = element_line(colour = "grey70", linetype = "dashed", size = .6),
  strip.text = element_text(hjust = 0, face = "bold")
)

# Plot by Annual Revenue

annual_revenue_plot <- data_tbl %>%
  select(order_date, total_revenue) %>%
  mutate(year = order_date %>% year() %>% as_factor()) %>%
  group_by(year) %>%
  summarise(revenue = sum(total_revenue), .groups = "drop") %>%
  ggplot(aes(year, revenue)) +
  geom_col(fill = "yellowgreen") +
  geom_hline(aes(yintercept = 0), colour = "black") +
  labs(
    title = "Total Annual Revenue",
    subtitle = paste0("Total Revenue of ", scales::dollar(sum(data_tbl$total_revenue), prefix = "\u00A3")),
    caption = paste0("Plot name: annual_revenue_plot\nProduced on: ", format(Sys.Date(), "%d %b %Y")),
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, .2)), labels = scales::comma_format(scale = 1e-3, prefix = "\u00A3", suffix = "k")) +
  myTheme

annual_revenue_plot

annual_monthly_revenue_plot <- data_tbl %>%
  select(order_date, total_revenue) %>%
  mutate(year = order_date %>% year() %>% as_factor(),
         month = floor_date(order_date, "1 month")) %>%
  group_by(year, month) %>%
  summarise(revenue = sum(total_revenue), .groups = "drop") %>%
  ggplot(aes(month, revenue, fill = year)) +
  geom_col(show.legend = FALSE) +
  geom_hline(aes(yintercept = 0), colour = "black") +
  labs(
    title = "Total Revenue by Year and Month",
    subtitle = paste0("Total Revenue of ", scales::dollar(sum(data_tbl$total_revenue), prefix = "£")),
    caption = paste0("Plot name: annual_monthly_revenue_plot\nProduced on: ", format(Sys.Date(), "%d %b %Y")),
    x = NULL,
    y = NULL
  ) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  scale_y_continuous(expand = expansion(mult = c(0, .25)), labels = scales::comma_format(scale = 1e-3, prefix = "£", suffix = "k")) +
  scale_fill_futurama() +
  facet_wrap(~ year, scales = "free_x") +
  myTheme

annual_monthly_revenue_plot

