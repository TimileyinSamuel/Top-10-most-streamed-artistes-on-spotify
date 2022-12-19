###################### MOST-STREAMED ARTISTES ON SPOTIFY VISUALIZATION ######################

##### Importing Libraries
library(tidyverse)
library(ggimage)
library(showtext)
library(ggtext)

##### Importing Data set
df <- read_csv2("https://raw.githubusercontent.com/TimileyinSamuel/Top-10-most-streamed-artistes-on-spotify/main/most_streamed_artistes.csv") |> 
      janitor::clean_names()

##### adding fonts
font_add_google("Source Code Pro", family = "source")
showtext_auto()

##### Reorder artistes based on streams (highest to lowest)
df$artiste <- fct_reorder(.f = df$artiste, .x = df$streams, .desc = TRUE)

##### plot title, sub-title and caption (ggtext format)
title <-"<p style='color:#FFFFFF;'>KINGS AND QUEENS OF<span style='color:#50C878;'> SPOTIFY</span></p>"
subtitle <- "<p style='color:#FFFFFF;'>Top 10<span style='color:#50C878;'> most-streamed artistes of all 
              time on spotify<span style='color:#FFFFFF;'> as of December 5, 2022.</span></p>"
caption <- "Graphic: @Timmy1Tesla"

##### plotting
ggplot(data = df, mapping = aes(x = artiste, y = streams)) +
  geom_col(width = 0.6, size = 0.5, fill = "#438D80") +
  ## add stream figure as text
  geom_text(mapping = aes(x = artiste, y = streams, label = streams),
            colour = "white", nudge_y = 0.70, size = 3.0, family = "source") +
  ## add artistes' images
  geom_image(mapping = aes(x = artiste, y = streams, image = image),
             size = 0.05, nudge_y = 3.8) +
  ## change y-axis' name and make the axis label from 0 to 50
  scale_y_continuous(name = "streams (billion)", breaks = c(0,10,20,30,40,50,60)) +
  ## force y-axis scale to end at 60
  expand_limits(y = c(0, 60)) +
  ## add plot title, subtitle and caption
  labs(title = title,
       subtitle = subtitle,
       caption = caption) +
  theme(
    ## make plot and panel background colour black
    plot.background = element_rect(fill = "#0C090A", colour = "#0C090A"),
    panel.background = element_rect(fill = "#0C090A", colour = "#0C090A"),
    ## remove grid lines
    panel.grid = element_blank(),
    ## make x-axis labels white colour
    axis.text = element_text(colour = "#FFFFFF"),
    ## make x-axis labels slant
    axis.text.x = element_text(angle = 45),
    ## remove ticks from x, y axis
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    ## make y-axis title white colour
    axis.title.y = element_text(colour = "#FFFFFF"),
    ## remove x-axis title
    axis.title.x = element_blank(),
    ## set font and colour for all texts used inn the chart
    text = element_text(colour = "#FFFFFF", family = "source"),
    ## plot title size
    plot.title = element_textbox_simple(size = 36.5),
    ## Align plot title and plot caption position
    plot.title.position = "plot",
    plot.caption.position = "plot",
    ## move plot caption to the right and change text size
    plot.caption = element_text(hjust = 1.0, size = 7.0),
    ## change text size of subtitle and give space between subtitle and title (margin)
    plot.subtitle = element_textbox_simple(size = 12.5, margin = margin(t = 4.5)))

### Saving the plot
ggsave(filename="most_streamed_artistes.png", width =9.3 , height=8, units="in", bg="white")
