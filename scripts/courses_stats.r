for (ext in c("load.r", "read_lts.r")) source(ext)
load(
    "dplyr",
    "magrittr",
    "this.path",
    "lubridate",
    "forcats",
    "tidyr",
    "scales"
)
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

output_dir <- file.path(
    this.dir(), "..", "output", "stats", "courses", format(Sys.time(), format = "%y-%m-%d_%H-%M-%S")
) %T>%
    dir.create()
data <- read_lts(
    file.path(
        this.dir(), "..", "data", "courses"
    ),
    encoding = "latin1"
)

print("Mean yearly completion rate of CIBA courses:")
data %>%
    group_by(
        Lukuvuosi = get_academic_year(Alkaa)
    ) %>%
    filter(
        Lukuvuosi < 2024 # At the time of writing this was the ongoing year.
    ) %>%
    summarize(
        Enrollments = sum(Hyv., na.rm = TRUE),
        Completions = sum(Suorit., na.rm = TRUE),
        Completion.rate = Completions / Enrollments * 100
    ) %T>%
    write.csv(file.path(output_dir, "stats.csv")) %>%
    pull(Completion.rate) %>%
    mean()
print("Finished.")