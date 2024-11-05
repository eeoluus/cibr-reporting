# An import hook.
#
# This will import the requested packages and install them
# if have not been installed. The benefit is that we do not
# need a separate setup script that has to track
# dependencies.

load <- function(...) {
    pkgs <- c(...)
    installed <- installed.packages()[,"Package"]
    missing <- pkgs[
        is.na(match(pkgs, installed))
    ]
    if (length(missing) > 0) {
        install.packages(missing)
    } else {
        print("All requested packages already installed.")
    }
    for (pkg in pkgs) {
        suppressPackageStartupMessages(
            library(pkg, character.only = TRUE, quietly = TRUE)
        )
    }
}