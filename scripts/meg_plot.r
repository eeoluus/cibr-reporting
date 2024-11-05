for (ext in c("load.r", "read_lts.r")) source(ext)
load(
    "this.path",
    "extrafont",
    "yaml",
    "magrittr",
    "ggplot2",
    "svglite",
    "dplyr",
    "forcats",
    "stringr"
)

# options(repr.plot.width = 10, repr.plot.height = 7) # Set plot dimensions for Jupyter here. Use the same values as in ggsave.
pdf(NULL)

config <- read_yaml(file.path(this.dir(), "..", "colors.yml"))

output_dir <- file.path(
    this.dir(), "..", "output", "plots", "meg", format(Sys.time(), format = "%y-%m-%d_%H-%M-%S")
) %T>%
    dir.create()
data <- read_lts(
    file.path(
        this.dir(), "..", "data", "meg"
    )
)
base_plot <- data %>%
    group_by(Tiedekunta) %>%
    mutate(
        Tiedekunta = gsub(";", ", ", Tiedekunta) %>% 
            str_to_sentence(),
        Yhteensä = sum(Tunnit)
    ) %>%
    ggplot(
        aes(
            x = Vuosi, 
            y = Tunnit,
            fill = fct_reorder(Tiedekunta, Yhteensä, .desc = TRUE)
        )
    ) +
    geom_bar(stat = "identity", key_glyph = "point") +
    scale_x_continuous(breaks = seq(2015, 2024, 1)) +
    ylim(0, 2000) +
    guides(fill = guide_legend(
        override.aes = list(
            shape = 21, 
            size = 10)
        )
    ) +
    labs(
        x = "", 
        y = "",
        title = "MEG-laboratorion käyttötunnit vuosittain\n",
        fill = "Tiedekunta-affiliaatiot"
    ) 
base_plot +
    scale_fill_manual(
        values = unlist(config$default$meg),
        limits = names(config$default$meg)
    ) +
    theme(
        plot.background = element_rect(
            fill = "#FFFFFF", 
            color = "#FFFFFF",
        ),
        panel.background = element_rect(
            fill = "#FFFFFF",
            colour = "#FFFFFF",
        ),
        legend.background = element_rect(
            fill = "#FFFFFF",
            colour = "#FFFFFF",
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
    file.path(output_dir, "white_usage_preview.png"),
    device = png,
    width = 10, 
    height = 7
)
ggsave(
    file.path(output_dir, "white_usage_plot.svg"),
    device = svg,
    width = 10, 
    height = 7
)
base_plot +
    scale_fill_manual(
        values = unlist(config$alt$meg),
        limits = names(config$alt$meg)
    ) +
    theme(
        plot.background = element_rect(
            fill = "#EDE1CE", # JYU primary beige
            color = "#EDE1CE",
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
    file.path(output_dir, "beige_usage_preview.png"),
    device = png,
    width = 10, 
    height = 7
)
ggsave(
    file.path(output_dir, "beige_usage_plot.svg"),
    device = svg,
    width = 10, 
    height = 7
)
print("Finished.")