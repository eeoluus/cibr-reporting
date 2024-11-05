for (ext in c("load.r", "read_lts.r")) source(ext)
load(
    "this.path",
    "extrafont",
    "yaml",
    "magrittr",
    "ggplot2",
    "svglite",
    "dplyr",
    "purrr",
    "forcats"
)

options(repr.plot.width = 10, repr.plot.height = 7)
pdf(NULL)

config <- read_yaml(file.path(this.dir(), "..", "colors.yml"))

output_dir <- file.path(
    this.dir(), "..", "output", "plots", "pubs", format(Sys.time(), format = "%y-%m-%d_%H-%M-%S")
) %T>%
    dir.create()
data <- read_lts(
    file.path(
        this.dir(), "..", "data", "pubs", "labeled"
    )
)
base_plot <- data %>%
    mutate(Category = replace(Category, grepl(";", Category), "Useita menetelmiä")) %>%
    add_count(Category) %>%
    ggplot(
        aes(
            x = Publication.year, 
            fill = fct_reorder(Category, n, .desc = TRUE)
        )
    ) +
    geom_bar(key_glyph = "point") +
    xlim(2005, 2025) +
    ylim(0, 100) +
    guides(fill = guide_legend(
        override.aes = list(
            shape = 21, 
            size = 10)
        )
    ) +
    labs(
        x = "", 
        y = "",
        title = "Vertaisarvioidut aivotutkimusjulkaisut vuosittain\n",
        fill = "Kategoria"
    )
base_plot +
    scale_fill_manual(
        values = unlist(config$default$pubs),
        limits = names(config$default$pubs)
    ) +
    theme(
        plot.background = element_rect(
            fill = "#FFFFFF", # Change the backgrounds to white or beige depending on the JYU PowerPoint slide background color.
            color = "#FFFFFF"
        ),
        panel.background = element_rect(
            fill = "#FFFFFF",
            colour = "#FFFFFF"
        ),
        legend.background = element_rect(
            fill = "#FFFFFF",
            colour = "#FFFFFF"
        ),
        panel.grid.major = element_line(
            colour = "#EFF0F1"
        ),
        panel.grid.minor = element_blank(),
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
ggsave(
    file.path(output_dir, "white_pubs_preview.png"),
    device = png,
    width = 10, 
    height = 7
)
ggsave(
    file.path(output_dir, "white_pubs_plot.svg"),
    device = svg,
    width = 10, 
    height = 7
)
base_plot +
    scale_fill_manual(
        values = unlist(config$alt$pubs),
        limits = names(config$alt$pubs)
    ) +
    theme(
        plot.background = element_rect(
            fill = "#EDE1CE",
            color = "#EDE1CE" # JYU primary beige
        ),
        panel.background = element_rect(
            fill = "#EDE1CE",
            colour = "#EDE1CE"
        ),
        legend.background = element_rect(
            fill = "#EDE1CE",
            colour = "#EDE1CE"
        ),
        panel.grid.major = element_line(
            colour = "#EFF0F1"
        ),
        panel.grid.minor = element_blank(),
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
ggsave(
    file.path(output_dir, "beige_pubs_preview.png"),
    device = png,
    width = 10, 
    height = 7
)
ggsave(
    file.path(output_dir, "beige_pubs_plot.svg"),
    device = svg,
    width = 10, 
    height = 7
)
print("Finished.")