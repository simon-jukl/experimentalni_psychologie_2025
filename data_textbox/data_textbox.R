library(tidyverse)
library(here)
library(janitor)


files <- list.files(here("FOLDER"), pattern = ".*\\.csv", full.names = T)
  
# combines all the data files into one dataframe
dt_text <-
  files |> 
  map(\(f) read_csv(f, 
                    col_select = c(participant, textbox.text),
                    col_types =  cols(participant = "c", textbox.text = "c"))) |> 
  list_rbind() |> 
  clean_names() |>
  filter(!is.na(textbox_text))
      
write_rds(dt_text, here("data_textbox/data_text.rds"))
