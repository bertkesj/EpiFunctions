## code to prepare `DATASET` dataset goes here

library(tidyverse)

example_person <- read_csv('C:/Users/inh4.CDC/OneDrive - CDC/EpiFunctions/person.csv',
                           col_types = cols(.default = "c"))
use_data(example_person, overwrite = T)

example_history <- read_csv('C:/Users/inh4.CDC/OneDrive - CDC/EpiFunctions/history.csv',
                            col_types = cols(.default = "c"))
use_data(example_history, overwrite = T)
