library(readxl)

library(tidyverse)


protein_raw <- read_xlsx(here::here('data/protein analysis/Gideons protein summary.xlsx'))

colnames(protein_raw) <- c('vial_num', 'sampleID', 'mg_subsample', 
                           'lowry_protein_IGG', 'bradford_protein_IGG',
                           'lowry_protein_percent', 'bradford_protein_percent')
protein_raw[which(protein_raw$sampleID == 'F2 S20'),]$sampleID <- 'F2 S20 PR'


protein_data  <-  protein_raw %>% 
  separate(sampleID, c("feeding", "ghop_fate_ID", "prey_remains"))

    mutate(splitID = str_split(sampleID, " ")) %>% 
  unnest(cols = c(splitID))

  


splitID <- str_split(protein_raw$sampleID, " ")


# protein_raw %>% 
#   tidyr::separate(sampleID, 
#                   sep = seq_len(max(nchar(.$sampleID)) - 1),
#                   into = paste0('group', seq_len(max(nchar(.$sampleID)))))


# foo <- data.frame(do.call('rbind', strsplit(as.character(protein_raw$sampleID),' ',fixed=TRUE)))


# splitID <- separate(protein_raw$sampleID, "feeding", "ghop_fate_ID", "remains")



protein_data <- protein_raw %>% 
  mutate(feeding = splitID[,1]),
