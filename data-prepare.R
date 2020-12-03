# ---- Main Data Load & Transformations ----

msg <- paste0("Start Process : \t\t", Sys.time())

options(echo = FALSE, message = FALSE, warning = FALSE)

msg <- paste0(msg, "\nLoad Libraries : \t\t", Sys.time())

source("config.R")

# Load Libraries
# library(tidytext)
# library(tidyverse)
# library(tidyquant)
# library(lubridate)
# library(extrafont)
# library(ggridges)
# library(ggsci)

msg <- paste0(msg, "\nLoad Dimension data : \t\t", Sys.time())

# Load Dimensions Data ----
customers_tbl <- read_csv("dimensions/customers.csv")
products_tbl <- read_csv("dimensions/products.csv")

msg <- paste0(msg, "\nLoad Fact data : \t\t", Sys.time())

# Load fact data ----
# Data directory containing fact data files
data_dir <- "data"
# Load data from directory and use map to load all files into a data frame
# join to dimension tibbles
# transform columns by splitting and mutating
data_tbl <- data_dir %>%
  # Create a list of files from the data directory
  fs::dir_ls() %>%
  # Load files from list of files into data frame using read_csv and
  # map_dfr to create one dataframe tibble
  map_dfr(read_csv) %>%
  # Join fact tibble to dimension tibbles using inner joins
  # products table contains different naming for main lookup column
  # use by within inner_join to identify connecting lookup tables
  inner_join(products_tbl, by = c("product_index" = "product_id")) %>%
  # Join fact table to customers tibble
  inner_join(customers_tbl) %>%
  # Separate product column into new columns based on - delimiter
  separate(col = product,
           into = c("product_name", "product_size", "product_colour", "product_category"),
           sep = "-") %>%
  # Mutate customer type into Retail ad Wholesale
  # 0 = Retail
  # 1 = Wholesale
  mutate(customer_type = case_when(
    customer_type == 0 ~ "Retail",
    TRUE ~ "Wholesale"
  )) %>%
  # Amend column types to factors and dates
  mutate(
    order_date = order_date %>% dmy(),
    product_name = product_name %>% as_factor(),
    product_size = product_size %>% as_factor(),
    product_colour = product_colour %>% as_factor(),
    product_category = product_category %>% as_factor(),
    customer_name = customer_name %>% as_factor(),
    customer_city = customer_city %>% as_factor(),
    customer_type = customer_type %>% as_factor()
  ) %>%
  # Add calculated column for total cost and total revenue
  mutate(
    total_cogs = order_units * product_cost,
    total_revenue = order_units * product_price
  ) %>%
  # drop columns no longer required
  # product index, customer index
  select(-contains(c("index", "id")), -c("product_cost", "product_price"))

msg <- paste0(msg, "\nRemove dimension objects : \t", Sys.time())

# Remove dimension tibbles for customers and products
rm(customers_tbl)
rm(products_tbl)

# Remove data directory object
rm(data_dir)

# Clear console to output process step timings
cat("\014")

# Output timings
message(msg)

rm(msg)


