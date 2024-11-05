library(tidyverse)
CAN_genes <- read_csv('data/RNAseq/GenesExpressed_in_CAN-thrs2.csv') %>%
  mutate("Gene name" = toupper(`Gene name`))
MFS_family <- readxl::read_xlsx('data/RNAseq/MFSFamily.xlsx') %>%
  separate(`Gene name`, into = c("Gene name", "isoform"), sep = ",")

MFS_in_CAN <- full_join(CAN_genes, MFS_family, by = "Gene name") %>%
  filter(!is.na(Family)) %>%
  arrange("Expression level")

write_csv(MFS_in_CAN, file = 'data/RNAseq/MFS_in_CAN.csv')
