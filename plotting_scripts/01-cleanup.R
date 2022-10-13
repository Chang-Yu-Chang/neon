#' This script cleans up the raw zip data downloaded from NEON
#' According to the NEON tutorial https://www.neonscience.org/resources/learning-hub/tutorials/download-explore-neon-data
#'

library(neonUtilities)
library(tidyverse)
library(cowplot)
library(lubridate)

# Stack the downloaded data files
if (!file.exists("~/Dropbox/lab/neon/data/raw/NEON_temp-soil")) {
    # Only run this line once. If the unzipped folder exists, dont run it
    stackByTable("~/Dropbox/lab/neon/data/raw/NEON_temp-soil.zip", nCores = parallel::detectCores(), )
}

# Read the stacked data
st30 <- readTableNEON(
    dataFile = "~/Dropbox/lab/neon/data/raw/NEON_temp-soil/stackedFiles/ST_30_minute.csv",
    varFile = "~/Dropbox/lab/neon/data/raw/NEON_temp-soil/stackedFiles/variables_00041.csv") %>%
    as_tibble()

sensor_positions <- read_csv("~/Dropbox/lab/neon/data/raw/NEON_temp-soil/stackedFiles/sensor_positions_00041.csv", show_col_types = F) %>%
    select(-publicationDate)

st30 <- st30 %>%
    unite(col = "HOR.VER", horizontalPosition, verticalPosition, sep = ".") %>%
    left_join(sensor_positions, by = c("siteID", "HOR.VER"))


# 15 sites are within the depth of 0.2 m
sensor_positions %>%
    filter(zOffset > -0.2)

#
month_indicator <- make_datetime(c(rep(2021, 12), 2022), c(1:12, 1), c(rep(1,12), 1), 0, 0)
month_highlight <- tibble(
    xmin = month_indicator[1:12],
    xmax = month_indicator[2:13],
    ymin = -Inf, ymax = Inf,
    Fill = rep(c("odd", "even"), 6)
)


# Every 30 minutes
p1 <- st30 %>%
    filter(zOffset > -0.2) %>%
    ggplot() +
    geom_rect(data = month_highlight, ymin = -Inf, ymax = Inf, alpha = .1,
              aes(xmin = xmin, xmax = xmax, fill = Fill)) +
    geom_line(aes(x = startDateTime, y = soilTempMean, group = HOR.VER, color = zOffset), size = .1) +
    scale_x_datetime(expand = c(0,0), breaks = make_datetime(2021, 1:12, 15, 0, 0), labels = month.name) +
    scale_fill_manual(values = c(odd = "grey", even = "red")) +
    scale_color_gradient(low = grey(0.1), high = grey(0.7)) +
    theme_classic() +
    theme(legend.position = "right") +
    guides(fill = "none") +
    labs(x = "", y = "Mean tempeature [Celsius]") +
    ggtitle("every 30 minutes")

# Every day
p2 <- st30 %>%
    filter(zOffset > -0.2) %>%
    mutate(Date = make_datetime(year(startDateTime), month(startDateTime), day(startDateTime))) %>%
    group_by(HOR.VER, Date, zOffset) %>%
    summarize(HOR.VER, soilTempMean = mean(soilTempMean)) %>%
    ggplot() +
    geom_rect(data = month_highlight, ymin = -Inf, ymax = Inf, alpha = .1,
              aes(xmin = xmin, xmax = xmax, fill = Fill)) +
    geom_line(aes(x = Date, y = soilTempMean, group = HOR.VER, color = zOffset), size = .1) +
    scale_x_datetime(expand = c(0,0), breaks = make_datetime(2021, 1:12, 15, 0, 0), labels = month.name) +
    scale_fill_manual(values = c(odd = "grey", even = "red")) +
    scale_color_gradient(low = grey(0.1), high = grey(0.7)) +
    theme_classic() +
    guides(fill = "none", color = "none") +
    labs(x = "", y = "Mean tempeature [Celsius]") +
    ggtitle("every day")

p <- plot_grid(p1, p2, nrow = 2, axis = "tblr", align = "vh")

ggsave(here::here("plots/01-line_plot.png"), p, width = 10, height = 6)

# Violin plot every 30 minutes
p3 <- st30 %>%
    filter(zOffset > -0.2) %>%
    filter(!is.na(soilTempMean)) %>%
    mutate(Month = month(startDateTime)) %>%
    ggplot(aes(x = Month, y = soilTempMean, group = Month)) +
    geom_violin() +
    geom_boxplot(width = 0.1, outlier.shape = 21, outlier.size = .6) +
    scale_x_continuous(expand = c(0,0), breaks = 1:12, labels = month.name) +
    theme_classic() +
    theme(panel.grid.major.y = element_line(color = gray(0.8), linetype = 2)) +
    labs(x = "", y = "Mean tempeature [Celsius]")
p3
ggsave(here::here("plots/02-violin_plot.png"), p3, width = 10, height = 3)





















