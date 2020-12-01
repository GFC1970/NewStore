library(tidyverse)
library(tidytext)

my_list <- list(
 Team = c("Team A", "Team A", "Team V", "Team B", "Team C", "Team B", "Team C", "Team V", "Admin", "Team A"),
  Value = c(1, 3, 17.34, 16, 46.87, 10, 12.19, 32, 67.94, 89)

) %>% as.data.frame() %>%
  group_by(Team) %>%
  summarise(`Total Items` = n(),
            `Total Value` = sum(Value),
            `Average Value` = mean(Value),
            `Median Value` = median(Value)
            ) %>%
  arrange(desc(`Total Value`)) %>%
  gather(key = "Item", value = "item_val", -Team) %>%
  mutate(Item = Item %>% as_factor()) %>%
  mutate(Team = reorder_within(Team, -item_val, Item)) %>%
  ggplot(aes(Team, item_val, fill = Item)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = item_val), vjust = -.5, fontface = "bold") +
  labs(
    title = "Just an example",
    subtitle = "And just a subtitle",
    caption = paste0("Produced on ", format(Sys.time(), "%d %b %Y")),
    x = "Team Name",
    y = "Value"
  ) +
  scale_x_reordered() +
  scale_y_continuous(expand = expansion(mult = c(0, .2))) +
  facet_wrap(~ Item, scales = "free_x") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 22, face = "bold", colour = "steelblue"),
    plot.subtitle = element_text(face = "italic", colour = "grey60"),
    plot.caption = element_text(size = 10, colour = "grey80"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(colour = "grey30", linetype = "dotted"),
    strip.background = element_rect(fill = "steelblue", colour = "steelblue"),
    strip.text = element_text(face = "bold", colour = "white", hjust = 0)
  )

my_list
