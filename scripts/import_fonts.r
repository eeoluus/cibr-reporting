source("load.r")
load(
    "extrafont",
    "this.path"
)

font_path <- file.path(this.dir(), "..", "assets", "fonts")

# From https://www.latofonts.com/lato-free-fonts/.
font_import(
    file.path(font_path, "lato", "Lato2OFLWeb", "Lato"),
    prompt = FALSE,
    pattern = "Lato"
)

# From https://fonts.google.com/specimen/Aleo.
font_import(
    file.path(font_path, "aleo", "Aleo"),
    prompt = FALSE,
    pattern = "Aleo"
)
