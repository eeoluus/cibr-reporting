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
    "purrr"
)

pdf(NULL)

get_academic_year <- function(date) { # Helper function to find the academic year that a date belongs to.
    parsed <- dmy(date)
    academic_year_start <- update(parsed, month = 8, mday = 1) # The academic year starts from October 1.
    return(
        ifelse(
            parsed < academic_year_start,
            year(parsed) - 1,
            year(parsed)
        ) %>% as.integer
    )
}
default_plot <- function(base_plot, colors) {
    base_plot + 
        scale_fill_manual(
            values = unlist(colors$default$courses)
        ) +
        theme(
            plot.background = element_rect(
                fill = "#FFFFFF", # Change the backgrounds to white or beige depending on the JYU PowerPoint slide background color.
                color = "#FFFFFF",
            ),
            panel.background = element_rect(
                fill = "#FFFFFF", 
                colour = "#FFFFFF"
            ),
            legend.background = element_rect(
                fill = "#FFFFFF",
                colour = "#FFFFFF",
            ),
            panel.grid.major = element_line(
                colour = "#EFF0F1"
            ),
            panel.grid.minor = element_blank(),
            aspect.ratio = 5/4, # This is to control the gap between bars.
            axis.ticks = element_blank(),
            text = element_text( # Legend explanations.
                family = "Lato Semibold",
                colour = "#002957", # JYU primary blue
                size = 12
            ),
            axis.text = element_text( # Tick labels.
                family = "Lato Semibold",
                colour = "#002957",
                size = 12
            ),
            title = element_text( # Main title.
                family = "Aleo",
                face = "bold",
                colour = "#002957",
                size = 20
            ),
            legend.title = element_text( # Legend title.
                family = "Lato Semibold",
                colour = "#002957",
                size = 16
            )
        )
}
alt_plot <- function(base_plot, colors) {
    base_plot +
        scale_fill_manual(
            values = unlist(colors$alt$courses)
        ) +
        theme(
            plot.background = element_rect(
                fill = "#EDE1CE",
                color = "#EDE1CE" # JYU primary beige
            ),
            panel.background = element_rect(
                fill = "#EDE1CE",
                colour = "#EDE1CE",
            ),
            legend.background = element_rect(
                fill = "#EDE1CE",
                colour = "#EDE1CE",
            ),
            panel.grid.major = element_line(
                colour = "#EFF0F1"
            ),
            panel.grid.minor = element_blank(),
            aspect.ratio = 5/4, 
            axis.ticks = element_blank(),
            text = element_text( 
                family = "Lato Semibold",
                colour = "#002957", 
                size = 12
            ),
            axis.text = element_text( 
                family = "Lato Semibold",
                colour = "#002957",
                size = 12
            ),
            title = element_text(
                family = "Aleo",
                face = "bold",
                colour = "#002957",
                size = 20
            ),
            legend.title = element_text(
                family = "Lato Semibold",
                colour = "#002957",
                size = 16
            )
        )
}
config <- read_yaml(file.path(this.dir(), "..", "colors.yml"))

output_dir <- file.path(
    this.dir(), "..", "output", "plots", "courses", format(Sys.time(), format = "%y-%m-%d_%H-%M-%S")
) %T>%
    dir.create()
data <- read_lts(
    file.path(
        this.dir(), "..", "data", "courses"
    ),
    encoding = "latin1"
)

###############
# ENROLLMENTS #
###############

base_plot_enrols <- data %>%
    filter(
        grepl("CIBA", Opintojakson.koodi)
    ) %>%
    group_by(Opintojakson.koodi) %>%
    mutate(
        Lukuvuosi = get_academic_year(Alkaa),
        Yhteensä = sum(Hyv.) # We calculate the sum of students over time for each course
    ) %>%                   # because we want to use this information to plot the most popular courses on top.
    filter(
        Lukuvuosi < 2024 # At the time of writing this was the ongoing year.
    ) %>%
    ggplot(
        aes(
            x = Lukuvuosi, 
            y = Hyv.,
            fill = fct_reorder(Opintojakson.koodi, Yhteensä, .desc = TRUE) # Plot the most popular courses on top.
        )
    ) +
    geom_bar(stat = "identity", key_glyph = "point") +
    scale_x_continuous(breaks = seq(2019, 2024, 1)) +
    ylim(0, 300) +
    guides(fill = 
       guide_legend(
           override.aes = list(shape = 21, size = 10)
       )
    ) +
    labs(
        x = "", 
        y = "",
        title = "CIBA-kursseille hyväksytyt opiskelijat lukuvuosittain\n",
        fill = "Opintojakson koodi"
    )

default_plot(base_plot_enrols, config)

ggsave(
    file.path(output_dir, "white_enrollments_preview.png"),
    device = png,
    width = 15,
    height = 7
)
ggsave(
    file.path(output_dir, "white_enrollments_plot.svg"),
    device = svg,
    width = 15, 
    height = 7
)

alt_plot(base_plot_enrols, config)

ggsave(
    file.path(output_dir, "beige_enrollments_preview.png"),
    device = png,
    width = 15,
    height = 7
)
ggsave(
    file.path(output_dir, "beige_enrollments_plot.svg"),
    device = svg,
    width = 15, 
    height = 7
)

###############
# COMPLETIONS #
###############

base_plot_compls <- data %>%
    filter(
        grepl("CIBA", Opintojakson.koodi)
    ) %>%
    group_by(Opintojakson.koodi) %>%
    mutate(
        Lukuvuosi = get_academic_year(Alkaa),
        Yhteensä = sum(Hyv.)
    ) %>%
    filter(
        Lukuvuosi < 2024 # At the time of writing this was the ongoing year.
    ) %>%
    ggplot(
        aes(
            x = Lukuvuosi, 
            y = Suorit.,
            fill = fct_reorder(Opintojakson.koodi, Yhteensä, .desc = TRUE)
        )
    ) +
    geom_bar(stat = "identity", key_glyph = "point") +
    scale_x_continuous(breaks = seq(2019, 2024, 1)) +
    ylim(0, 300) +
    guides(
        fill = guide_legend(
            override.aes = list(
                shape = 21, 
                size = 10
            )
        )
    ) +
    labs(
        x = "", 
        y = "",
        title = "CIBA-kurssin suorittaneet opiskelijat lukuvuosittain\n",
        fill = "Opintojakson koodi"
    )

default_plot(base_plot_compls, config)

ggsave(
    file.path(output_dir, "white_completions_preview.png"),
    device = png,
    width = 15,
    height = 7
)
ggsave(
    file.path(output_dir, "white_completions_plot.svg"),
    device = svg,
    width = 15, 
    height = 7
)

alt_plot(base_plot_compls, config)

ggsave(
    file.path(output_dir, "beige_completions_preview.png"),
    device = png,
    width = 15,
    height = 7
)
ggsave(
    file.path(output_dir, "beige_completions_plot.svg"),
    device = svg,
    width = 15, 
    height = 7
)
print("Finished.")