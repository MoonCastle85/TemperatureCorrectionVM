library(tidyverse)

files <- list.files(paste0("C:/Users/vanja/OneDrive - Profu/Fjärrkontrollen - Profu - Documents/Admin/",
                           "3. Underlag profiler, COP, inv kostnader osv/Profiler värme och el för fastigheterna/Analys profiler"),
                    pattern = "+.csv", full.names = TRUE)

combined <- map(files, read_csv2) %>% bind_rows()

write_csv2(combined, file = "Profiler_alla.csv")
