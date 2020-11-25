# Creation of Summary Plots for each factor ----

# Set options
options(echo = FALSE)

# Source to data preparation
source("data-prepare.R")

# Set theme
theme_set(
  theme_minimal(
    base_family = "Roboto Condensed",
    base_size = 20
    )
  )

# Custom theme settings
myTheme <- theme(
  # Plot parameters
  plot.margin = unit(c(.3,1,.3,.1), "cm"),
  plot.title = element_text(size = 22, colour = "steelblue", face = "bold"),
  plot.subtitle = element_text(size = 16),
  plot.caption = element_text(size = 11, colour = "grey70", hjust = 0),
  # panel parameters
  panel.spacing.y = unit(.5, "cm"),
  panel.grid = element_blank(),
  panel.grid.major.y = element_line(colour = "grey70", linetype = "dashed", size = .6),
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
    subtitle = paste0("Total Revenue of ", scales::dollar(sum(data_tbl$total_revenue), prefix = "\u00A3")),
    caption = paste0("Plot name: annual_revenue_plot\nProduced on: ", format(Sys.Date(), "%d %b %Y")),
    x = NULL,
    y = NULL
  ) +
  # Set format of Y axis labels
  scale_y_continuous(expand = expansion(mult = c(0, .2)), labels = scales::comma_format(scale = 1e-3, prefix = "\u00A3", suffix = "k")) +
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
    subtitle = paste0("Total Revenue of ", scales::dollar(sum(data_tbl$total_revenue), prefix = "\u00A3")),
    caption = paste0("Plot name: annual_monthly_revenue_plot\nProduced on: ", format(Sys.Date(), "%d %b %Y")),
    x = NULL,
    y = NULL
  ) +
  # Format x axis labels as short month format
  scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  # Format y axis labels
  scale_y_continuous(expand = expansion(mult = c(0, .25)), labels = scales::comma_format(scale = 1e-3, prefix = "\u00A3", suffix = "k")) +
  # Set colour scheme
  scale_fill_futurama() +
  # Facet plot by year
  facet_wrap(~ year, scales = "free_x") +
  # Set myTheme
  myTheme



