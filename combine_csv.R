library(tidyverse)

setwd(paste0("C:/Users/vanja/OneDrive - Profu/Fjärrkontrollen - Profu - Documents/Admin/",
             "3. Underlag profiler, COP, inv kostnader osv/Profiler värme och el för fastigheterna/Analys profiler"))

files <- list.files(".", pattern = "(hus|villa|kontor)\\.csv$")

combined <- map(files, \(x) read_csv2(x, col_select = -c("profile_id"), id = "profile_id")) %>%
  bind_rows() 

combined2 <- combined %>%
  mutate(profile_id = str_extract(profile_id, ".*(?=\\.csv$)"))

### Filter ###
combined2_filtered <- combined2 %>%
  filter(str_detect(source_file, "Finspang"))

write_csv2(combined2_filtered, file = "Profiler_filtrerade.csv")

### All ###
setwd("C:/Git/TemperatureCorrectionVM")
write_csv2(combined2, file = "Profiler_alla.csv")
