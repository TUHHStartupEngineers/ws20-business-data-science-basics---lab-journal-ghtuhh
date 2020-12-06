# 1.0 Lollipop Chart: Top N Customers ----
library(tidyverse)
library(lubridate)

bike_orderlines_tbl <- read_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")


n <- 10
# Data Manipulation
top_customers_tbl <- bike_orderlines_tbl %>%
  
  # Select relevant columns
  select(bikeshop, total_price) %>%
  
  # Collapse the least frequent values into “other”
  mutate(bikeshop = as_factor(bikeshop) %>% fct_lump(n = n, w = total_price)) %>%
  
  # Group and summarize
  group_by(bikeshop) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup() %>%
  
  # Reorder the column customer_city by revenue
  mutate(bikeshop = bikeshop %>% fct_reorder(revenue)) %>%
  # Place "Other" at the beginning
  mutate(bikeshop = bikeshop %>% fct_relevel("Other", after = 0)) %>%
  # Sort by this column
  arrange(desc(bikeshop)) %>%
  
  # Add Revenue Text
  mutate(revenue_text = scales::dollar(revenue, 
                                       scale  = 1e-6, 
                                       prefix = "", 
                                       suffix = "M €")) %>%
  
  # Add Cumulative Percent
  mutate(cum_pct = cumsum(revenue) / sum(revenue)) %>%
  mutate(cum_pct_text = scales::percent(cum_pct)) %>%
  
  # Add Rank
  mutate(rank = row_number()) %>%
  mutate(rank = case_when(
    rank == max(rank) ~ NA_integer_,
    TRUE ~ rank
  )) %>%
  
  # Add Label text
  mutate(label_text = str_glue("Rank: {rank}\nRev: {revenue_text}\nCumPct: {cum_pct_text}"))

# Data Visualization
top_customers_tbl %>%
  
  # Canvas
  ggplot(aes(revenue, bikeshop)) +
  
  # Geometries
  geom_segment(aes(xend = 0, yend = bikeshop), 
               color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11],
               size  = 1) +
  
  geom_point(aes(size = revenue),
             color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11]) +
  
  geom_label(aes(label = label_text), 
             hjust = "inward",
             size  = 3,
             color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11]) +
  
  # Formatting
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, 
                                                    prefix = "",
                                                    suffix = "M €")) +
  labs(
    title = str_glue("Top {n} Customers"),
    subtitle = str_glue(
      "Start: {year(min(bike_orderlines_tbl$order_date))}
               End:  {year(max(bike_orderlines_tbl$order_date))}"),
    x = "Revenue (M €)",
    y = "Customer",
    caption = str_glue("Top 6 customers contribute
                           52% of purchasing power.")
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "bold.italic")
  )


# --------- CASE 2

# Select columns and filter categories
pct_sales_by_customer_tbl <- bike_orderlines_tbl %>%
  
  select(bikeshop, category_1, category_2, quantity) %>%
  filter(category_1 %in% c("Mountain","Road")) %>% 
  
  # Group by category and summarize
  group_by(bikeshop, category_1, category_2) %>%
  summarise(total_qty = sum(quantity)) %>%
  ungroup() %>%
  
  # Add missing groups (not necessarily mandatory, but we'd get holes in the plot)
  # complete() creates NAs. We need to set those to 0.
  complete(bikeshop, nesting(category_1, category_2)) %>% 
  mutate(across(total_qty, ~replace_na(., 0))) %>%  
  
  # Group by bikeshop and calculate revenue ratio
  group_by(bikeshop) %>%
  mutate(pct = total_qty / sum(total_qty)) %>%
  ungroup() %>%
  
  # Reverse order of bikeshops
  mutate(bikeshop = as.factor(bikeshop) %>% fct_rev()) %>%
  # Just to verify
  mutate(bikeshop_num = as.numeric(bikeshop))


# Data Visualization
pct_sales_by_customer_tbl %>%
  
  ggplot(aes(category_2, bikeshop)) +
  
  # Geometries
  geom_tile(aes(fill = pct)) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1L)), 
            size = 3) +
  facet_wrap(~ category_1, scales = "free_x") +
  
  # Formatting
  scale_fill_gradient(low = "white", high = "#2C3E50") +
  labs(
    title = "Heatmap of Purchasing Habits",
    x = "Bike Type (Category 2)",
    y = "Customer",
    caption = str_glue(
      "Customers that prefer Road: 
        To be discussed ...
        
        Customers that prefer Mountain: 
        To be discussed ...")
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "bold.italic")
  )



