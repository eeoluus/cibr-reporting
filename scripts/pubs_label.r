for (ext in c("load.r", "read_lts.r")) source(ext)
load(
    "tidyr",
    "dplyr",
    "this.path",
    "magrittr"
)
output_dir <- file.path(
    this.dir(), "..", "data", "pubs", "labeled", format(Sys.time(), format = "%y-%m-%d_%H-%M-%S")
) %T>%
    dir.create()
data <- read_lts(
    file.path(
        this.dir(), "..", "data", "pubs", "publications"
    )
)
data %>%
    mutate( # Add columns, which we will later collapse into a column for category.
        eeg = case_when( # You can use attributes(data) to get a sense of which fields are availables, e.g. YSO.keywords..en..
            grepl("EEG", YSO.keywords..en.) | # Categorization logic goes here.
            grepl("([Ee]lectroencephalography|EEG)", Free.keywords..en.) |
            grepl("([Ee]lektroenkefalografia|EEG|[Aa]ivosähkökäyrä)", Free.keywords..fi.) |
            grepl("[Ee]lectro(?=encephalography)", Abstract..en., perl = TRUE) ~ "EEG",
            .default = NA
        ),
        meg = case_when( # We rely on searching for regular expressions (https://regexr.com/).
            grepl("MEG", YSO.keywords..en.) | # These are determined by the JYU Open Science Center.
            grepl("([Mm]agnetoencephalography|MEG)", Free.keywords..en.) | # The researcher can add their own keywords as "free keywords".
            grepl("([Mm]agnetoenkefalografia|MEG|[Aa]ivomagneettikäyrä)", Free.keywords..fi.) |
            grepl("[Mm]agneto(?=encephalography)", Abstract..en., perl = TRUE) ~ "MEG", # In case the keywords aren't informative,
            .default = NA                                                              # look at the abstract. Here, for example,
        ),                                                                            # the regex will match "magnetoencephalography"
        mri = case_when(                                                             # and "magneto- and electroencephalography".
            grepl("magnetic resonance imaging", YSO.keywords..en.) |
            grepl("([Mm]agnetic resonance imaging|MRI)", Free.keywords..en.) |
            grepl("([Mm]agneetti(resonanssi)?(kuvaus|kuvantaminen)|MRI)", Free.keywords..fi.) |
            grepl("[Mm]agnetic resonance imaging", Abstract..en.) ~ "Magneettikuvaus",
            .default = NA
        ),
        dti = case_when(
            grepl("diffusion tensor imaging", YSO.keywords..en.) |
            grepl("([Dd]iffusion tensor imaging|DTI)", Free.keywords..en.) |
            grepl("([Dd]iffuusio(tensori)?(kuvaus|kuvantaminen)|DTI)", Free.keywords..fi.) |
            grepl("[Dd]iffusion tensor imaging", Abstract..en.) ~ "Diffuusiotensorikuvaus",
            .default = NA
        ),
        tms = case_when(
            grepl("transcranial magnetic stimulation", YSO.keywords..en.) |
            grepl("([Tt]ranscranial magnetic stimulation|TMS)", Free.keywords..en.) |
            grepl("(([Tt]ranskraniaalinen )?[Mm]agneettistimulaatio|TMS)", Free.keywords..fi.) |
            grepl("[Tt]ranscranial magnetic stimulation", Abstract..en.) ~ "Aivojen magneettistimulaatio",
            .default = NA
        )                                             
    ) %>%
    unite(eeg:tms, col = Category, na.rm = TRUE, sep = ";") %>% # Depending on what columns you list here,
    mutate(                                                    # you can choose which methods are included in the plot.
        Category = case_when(
            Category == "" &
            grepl("3112", Field.of.science.code) ~ "Muu neurotiede", # We use the code for "Neurosciences" as a catch-all
            .default = Category                                     # criterion to include e.g. electrophysiological studies.    
        )
    ) %>%
    filter(                                                   
        grepl("^A", Publication.type.code) & # Keep only peer-reviewed publications.
        Category != "" # Here we exclude all the non-neuro, but you can comment this out to if you want to include them in a .csv.
    ) %>%
    write.csv(file.path(output_dir, "brain-publications.csv")) %T>%
    print("Finished.")
