for (ext in c("load.r", "read_lts.r")) source(ext)
load(
    "dplyr",
    "magrittr",
    "ggplot2",
    "this.path",
    "lubridate",
    "forcats",
    "extrafont",
    "yaml",
    "ggridges",
    "tidyr",
    "scales"
)

pdf(NULL)

output_dir <- file.path(
    this.dir(), "..", "output", "plots", "funding", format(Sys.time(), format = "%y-%m-%d_%H-%M-%S")
) %T>%
    dir.create()
data <- read_lts(
    file.path(
        this.dir(), "..", "data", "funding"
    ),
    encoding = "UTF-8"
)

#####################
# FUNDING OVER TIME #
#####################

# options(repr.plot.width = 33, repr.plot.height = 20) # For Jupyter.

time_series <- data %>%
    mutate(
        Alkupvm = mdy(Alkupvm),
        Loppupvm = mdy(Loppupvm),
        Funding.quarterly = Ulkopuolinen.rahoitus / (interval(Alkupvm, Loppupvm) / months(3)),
        Duration.days = interval(Alkupvm, Loppupvm) / days(1)
    ) %>%
    arrange(desc(Funding.quarterly)) %>%
    # head(20) %>% # In case you want to delimit the data.
    group_by(Projektin.nimi..en.) %>% # The names must be unique.
    expand(
        Project.day = seq(Alkupvm, Loppupvm, by = "days"), # Assume funding is available on the last day.
        Funding.quarterly = rep(Funding.quarterly, Duration.days)
    ) 
base_ridge_plot <- time_series %>%
    ggplot(
        aes(
            Project.day, 
            fct_reorder(Projektin.nimi..en., Funding.quarterly), 
            height = Funding.quarterly / 10000, # Divide by 10 000: now the height of each tile will reflect 10K EUR.
            group = Projektin.nimi..en.,
            fill = fct_reorder(Projektin.nimi..en., Funding.quarterly)
        )
    ) + 
    geom_ridgeline(
        alpha = 0.8,
        linetype = "blank"
    ) +
    scale_x_date(
        breaks = "2 years",
        minor_breaks = "3 months",
        date_labels = "%Y",
        expand = c(0, 0),
        limits = c(
            min(mdy(data$Alkupvm)) - months(8),
            max(mdy(data$Loppupvm)) + months(11)
        )
    ) +
    # scale_y_discrete( # How to ignore duplicate project names on the Y axis?
    #    labels = fct_reorder(
    #        time_series$Project.name, 
    #        time_series$Funding.quarterly
    #    )
    # ) +
    scale_fill_cyclical(
        values = c( # Use this for the complete data.
            "#F1563F", # JYU secondary orange
            "#002957", # JYU primary blue
            "#C29A5B" # JYU secondary gold
        ),
    ) +
    # scale_fill_cyclical(
    #     values = c( # Use this for the top 20.
    #         "#F1563F", # JYU secondary orange
    #         "#002957", # JYU primary blue
    #         "#C29A5B" # JYU secondary gold
    #     ),
    # ) +
    labs(
        x = "", 
        y = "",
        title = "Neurotieteen hankkeiden ulkop. rahoitus per kvartaali\n(Skaala 10 000 €)\n",
    )     
base_ridge_plot +
    theme(
        plot.background = element_rect(
            fill = "#FFFFFF", # Change the backgrounds to white or beige depending on the JYU PowerPoint slide background color.
            color = "#FFFFFF",
        ),
        panel.background = element_rect(
            fill = "#FFFFFF",
            colour = "#FFFFFF",
        ),
        panel.grid.minor = element_line(
            colour = "#EFF0F1"
        ),
        panel.grid.major = element_line(
            colour = "#EFF0F1"
        ),
        plot.margin = margin(10, 250, 10, 250), # This is for the axis text not to be cropped.
        axis.ticks = element_blank(),
        axis.text = element_text( # Tick labels.
            family = "Lato Semibold",
            colour = "#002957",
        ),
        axis.text.y = element_text( # Tick labels. (Projects)
            size = 12
        ),
        axis.text.x = element_text( # Tick labels.
            size = 20,
            margin = margin(t = 25)
        ),
        title = element_text( # Main title.
            family = "Aleo",
            face = "bold",
            colour = "#002957",
            size = 28
        ) 
    )
ggsave(
    file.path(output_dir, "white_proj_preview.png"),
    device = png,
    width = 33,
    height = 20,
)
ggsave(
    file.path(output_dir, "white_proj_plot.svg"),
    device = svg,
    width = 33, 
    height = 20
)
base_ridge_plot +
    theme(
        plot.background = element_rect(
            fill = "#EDE1CE", # JYU primary beige
            color = "#EDE1CE",
        ),
        panel.background = element_rect(
            fill = "#EDE1CE",
            colour = "#EDE1CE",
        ),
        panel.grid.minor = element_line(
            colour = "#EFF0F1"
        ),
        panel.grid.major = element_line(
            colour = "#EFF0F1"
        ),
        plot.margin = margin(10, 250, 10, 250),
        axis.ticks = element_blank(),
        axis.text = element_text( 
            family = "Lato Semibold",
            colour = "#002957",
        ),
        axis.text.y = element_text(
            size = 12
        ),
        axis.text.x = element_text(
            size = 20,
            margin = margin(t = 25)
        ),
        title = element_text(
            family = "Aleo",
            face = "bold",
            colour = "#002957",
            size = 28
        ) 
    )
ggsave(
    file.path(output_dir, "beige_proj_preview.png"),
    device = png,
    width = 33,
    height = 20,
)
ggsave(
    file.path(output_dir, "beige_proj_plot.svg"),
    device = svg,
    width = 33, 
    height = 20
)

##################
# SUMS OVER TIME #
##################

# options(repr.plot.width = 22.75, repr.plot.height = 15) # For Jupyter

base_area_plot <- time_series %>%
    group_by(Project.day) %>%
    summarise(Sum = sum(Funding.quarterly)) %>%
    ggplot(aes(Project.day, Sum)) +
    geom_area(fill = "#C29A5B", alpha = 1) + # JYU secondary gold
    scale_y_continuous(
        labels = comma_format(
            big.mark = " ", 
            decimal.mark = ",",
            suffix = " €"
        )
    ) +
    scale_x_date(
        breaks = "2 years",
        minor_breaks = "3 months",
        date_labels = "%Y",
        expand = c(0, 0),
        limits = c(
            min(mdy(data$Alkupvm)) - months(8),
            max(mdy(data$Loppupvm)) + months(11)
        )
    ) +
    labs(
        x = "", 
        y = "",
        title = "Neurotieteen hankkeiden yhteenlaskettu ulkop. rahoitus per kvartaali\n",
    )
base_area_plot + 
    theme(
        plot.background = element_rect(
            fill = "#FFFFFF", 
            color = "#FFFFFF",
        ),
        panel.background = element_rect(
            fill = "#FFFFFF",
            colour = "#FFFFFF",
        ),
        panel.grid = element_line(
            colour = "#EFF0F1"
        ),
        plot.margin = margin(10, 250, 10, 250),
        axis.ticks = element_blank(),
        axis.text = element_text( 
            family = "Lato Semibold",
            colour = "#002957",
            size = 20
        ),
        axis.text.y = element_text(
            margin = margin(r = 50)
        ),
        title = element_text(
            family = "Aleo",
            face = "bold",
            colour = "#002957",
            size = 28
        ) 
    )
ggsave(
    file.path(output_dir, "white_sum_preview.png"),
    device = png,
    width = 22.75,
    height = 15
)
ggsave(
    file.path(output_dir, "white_sum_plot.svg"),
    device = svg,
    width = 22.75,
    height = 15
)
base_area_plot + 
    theme(
        plot.background = element_rect(
            fill = "#EDE1CE",
            color = "#EDE1CE",
        ),
        panel.background = element_rect(
            fill = "#EDE1CE",
            colour = "#EDE1CE",
        ),
        panel.grid = element_line(
            colour = "#EFF0F1"
        ),
        plot.margin = margin(10, 250, 10, 250),
        axis.ticks = element_blank(),
        axis.text = element_text( 
            family = "Lato Semibold",
            colour = "#002957",
            size = 20
        ),
        axis.text.y = element_text( 
            margin = margin(r = 50)
        ),
        title = element_text( 
            family = "Aleo",
            face = "bold",
            colour = "#002957",
            size = 28
        ) 
    )
ggsave(
    file.path(output_dir, "beige_sum_preview.png"),
    device = png,
    width = 22.75,
    height = 15
)
ggsave(
    file.path(output_dir, "beige_sum_plot.svg"),
    device = svg,
    width = 22.75,
    height = 15
)
print("Finished.")