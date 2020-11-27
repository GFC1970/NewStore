# Creation of Summary Plots for each factor ----

# Set options
options(echo = FALSE)

# Source to data preparation
source("data-prepare.R")

# Set theme
theme_set(theme_minimal(base_family = "Roboto Condensed",
                        base_size = 20))

# Custom theme settings
myTheme <- theme(
  # Plot parameters
  plot.margin = unit(c(.3, 1, .3, .1), "cm"),
  plot.title = element_text(
    size = 22,
    colour = "steelblue",
    face = "bold"
  ),
  plot.subtitle = element_text(size = 16),
  plot.caption = element_text(
    size = 11,
    colour = "grey70",
    hjust = 0
  ),
  # panel parameters
  panel.spacing.y = unit(.5, "cm"),
  panel.grid = element_blank(),
  panel.grid.major.y = element_line(
    colour = "grey70",
    linetype = "dashed",
    size = .6
  ),
  # Axis parameters
  axis.text = element_text(size = 12),
  # Strip parameters for facets
  strip.text = element_text(hjust = 0, face = "bold")
)

# Plot by Annual Revenue
# Plot total revenue for each year. Create year column from order date column
# Plot data as a column chart
annual_revenue_plot <- data_tbl %>%
  # Select required columns
  select(order_date, total_revenue) %>%
  # Add yer column
  mutate(year = order_date %>% year() %>% as_factor()) %>%
  # Group by year
  group_by(year) %>%
  # Calculate revenue for year year
  summarise(revenue = sum(total_revenue), .groups = "drop") %>%
  # Create plot
  ggplot(aes(year, revenue)) +
  # Set plot type
  geom_col(fill = "#4682B4") +
  # Add labels
  labs(
    title = "Total Annual Revenue",
    subtitle = paste0("Total Revenue of ", scales::dollar(sum(
      data_tbl$total_revenue
    ), prefix = "\u00A3")),
    caption = paste0(
      "Plot name: annual_revenue_plot\nProduced on: ",
      format(Sys.Date(), "%d %b %Y")
    ),
    x = NULL,
    y = NULL
  ) +
  # Set format of Y axis labels
  scale_y_continuous(
    expand = expansion(mult = c(0, .2)),
    labels = scales::comma_format(
      scale = 1e-3,
      prefix = "\u00A3",
      suffix = "k"
    )
  ) +
  # Set myTheme
  myTheme


# Plot monthly revenue for each year
# Plot total revenue for each month for each year. Create year column from order date column
# Plot data as a column chart and create facets by year
annual_monthly_revenue_plot <- data_tbl %>%
  # Select required columns
  select(order_date, total_revenue) %>%
  # Create year and month columns
  # Ensure year column is a factor to ensure correct faceting
  mutate(year = order_date %>% year() %>% as_factor(),
         # use floor_date to make all dates 1st day of month
         month = floor_date(order_date, "1 month")) %>%
  # Group by year and month values
  group_by(year, month) %>%
  # Calculate revenue
  summarise(revenue = sum(total_revenue), .groups = "drop") %>%
  # Create plot
  ggplot(aes(month, revenue, fill = year)) +
  # Set plot type, hiding legend
  geom_col(show.legend = FALSE) +
  # Add labels
  labs(
    title = "Total Revenue by Year and Month",
    subtitle = paste0("Total Revenue of ", scales::dollar(sum(
      data_tbl$total_revenue
    ), prefix = "\u00A3")),
    caption = paste0(
      "Plot name: annual_monthly_revenue_plot\nProduced on: ",
      format(Sys.Date(), "%d %b %Y")
    ),
    x = NULL,
    y = NULL
  ) +
  # Format x axis labels as short month format
  scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  # Format y axis labels
  scale_y_continuous(
    expand = expansion(mult = c(0, .25)),
    labels = scales::comma_format(
      scale = 1e-3,
      prefix = "\u00A3",
      suffix = "k"
    )
  ) +
  # Set colour scheme
  scale_fill_futurama() +
  # Facet plot by year
  facet_wrap( ~ year, scales = "free_x") +
  # Set myTheme
  myTheme


variance_data <- data_tbl %>%
  select(order_date, order_units) %>%
  mutate(year = order_date %>% year() %>% as_factor()) %>%
  mutate(month = floor_date(order_date, unit = "months")) %>%
  group_by(year, month) %>%
  summarise(units = sum(order_units), .groups = "drop") %>%
  mutate(units_lag = lag(units, n = 1)) %>%
  mutate(units_lag = case_when(
    is.na(units_lag) ~ units,
    TRUE ~ units_lag
  )) %>%
  mutate(units_var = units - units_lag) %>%
  mutate(units_var_percent = units_var / units_lag) %>%
  mutate(units_var_percent_text = units_var_percent %>% scales::percent(accuracy = 2)) %>%
  select(year, month, units_var_percent_text, units_var_percent)

monthly_variance_plot <- variance_data %>%
  ggplot(aes(month, units_var_percent, colour = year)) +
  geom_line(size = 2) +
  geom_area(aes(fill = year), alpha = .2) +
  geom_hline(aes(yintercept = 0)) +
  geom_text(aes(label = units_var_percent_text,
                vjust = case_when(
                  units_var_percent > 0 ~ -1.5,
                  TRUE ~ 1.5
                )), fontface = "bold", size = 4) +
  facet_wrap(~ year, scales = "free_x") +
  scale_fill_aaas() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(-1, 1)) +
  myTheme +
  theme(legend.position = "none")

monthly_variance_plot

yoy_variance <- data_tbl %>%
  select(order_date, order_units) %>%
  mutate(year = order_date %>% year() %>% as_factor()) %>%
  mutate(month = floor_date(order_date, unit = "months")) %>%
  group_by(year, month) %>%
  summarise(units = sum(order_units), .groups = "drop") %>%
  mutate(prev_y_m = lag(units, n = 12)) %>%
  mutate(prev_y_m = case_when(
    is.na(prev_y_m) ~ units,
    TRUE ~ prev_y_m)
  ) %>%
  mutate(prev_y_m_var = units - prev_y_m,
         prev_y_m_var_pc = prev_y_m_var / prev_y_m)



yoy_variance_plot <- yoy_variance %>%
  filter(!year == 2015) %>%
  ggplot(aes(month, prev_y_m_var_pc, fill = year)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::percent(prev_y_m_var_pc, accuracy = .01, trim = TRUE),
            vjust = case_when(
              prev_y_m_var_pc > 0 ~ -1.1,
              TRUE ~ 1.1
            )), size = 3) +
  facet_wrap(~ year, scales = "free_x") +
  scale_fill_d3() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(-1, 1)) +
  myTheme

yoy_variance_plot
