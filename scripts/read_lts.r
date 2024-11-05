# A .csv reader.
#
# This will read the latest data in the
# folder specified by the path parameter, assuming
# that the direct folders containing the data are order by date and time,
# with a special "lts" folder being able to override the others.
#
# The function will complain if there are multiple files in a single folder.
# The benefit being, the file name doesn't matter, as long as
# it's a .csv file.

read_lts <- function(path, encoding = "unknown") {
    dirs <- rev(list.dirs(path, recursive = FALSE))
    usr <- file.path(dirs[1], list.files(dirs[1]))
    if (length(usr) == 1) {
        print("Reading user-entered data...")
        data <- read.csv(usr, header = TRUE, encoding = encoding)
        print("User-entered data successfully read.")
        return(data)
    } else if (length(usr) > 1 | is.na(dirs[2])) {
        stop("\nYou either need to remove unnecessary files from the lts folder, or are missing files to read.\nIn the latter case, try putting a .csv file into the lts folder or run a data-producing script.")
    } else {
        auto <- file.path(dirs[2], list.files(dirs[2]))
        if (length(auto) == 1) {
            print("Reading automatically produced data...")
            data <- read.csv(auto, header = TRUE, encoding = encoding)
            print("Automatically produced data successfully read.")
            return(data)
        } else {
            stop("\nEither you need to remove unnecessary files from the latest produced data, or there has been an error accessing it.\nIn the latter case, re-run the data-producing script.\nIf re-running the script does not help, check the script. If nothing else helps, put a .csv file into the lts folder.")
        }
    }
}
