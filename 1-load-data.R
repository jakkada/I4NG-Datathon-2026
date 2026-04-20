library(tidyverse)
library(here)

# ============================== read me first ================================
# reproducibility note: #### 
# to reproduce our analysis please create the catalog "data"
# in your working directory
# download datasets for CRONOS3 Wave 1-5 (as a single merged file or as five files for each round, requiring subsequent merging), and ESS rounds 11, 10SC (self-completion), and 10 (again, either as a single merged file or as three files for each round, requiring subsequent merging), 
# in IBM SPSS .sav format, 
# unzip downloaded catalogs, 
# move all .sav data files to the catalog "data"
# in your working directory.
# Here we are using merged files. Bare in mind that Czechia did not participate in ESS 11th round.
# =============================================================================


## load CRONOS3 Wave 1-5 data #### 
# DOI: 
# Wave 1: https://doi.org/10.21338/cronos3-w1
# Wave 2: https://doi.org/10.21338/cronos3-w2
# Wave 3: https://doi.org/10.21338/cronos3-w3
# Wave 4: https://doi.org/10.21338/cronos3-w4
# Wave 5: https://doi.org/10.21338/cronos3-w5
# 
# Citation:
# Wave 1: European Social Survey European Research Infrastructure (ESS ERIC) (2025) CRONOS3 Wave 1. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/cronos3-w1
# Wave 2: European Social Survey European Research Infrastructure (ESS ERIC) (2025) CRONOS3 Wave 2. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/cronos3-w2
# Wave 3: European Social Survey European Research Infrastructure (ESS ERIC) (2025) CRONOS3 Wave 3. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/cronos3-w3
# Wave 4: European Social Survey European Research Infrastructure (ESS ERIC) (2026) CRONOS3 Wave 4. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/cronos3-w4
# Wave 5: European Social Survey European Research Infrastructure (ESS ERIC) (2025) CRONOS3 Wave 5. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/cronos3-w5

cronos <- haven::read_spss(here::here("data", "CRONOS-3 Theme - All items Waves 1-5.sav"))


# miss_cronos <- haven::read_spss(here::here("data", "CRONOS-3 Theme - All items Waves 1-5.sav"),
                                user_na = TRUE) # to preserve user-defined missings

## load ESS11, 10SC, and 10 data #### 
# DOI: 
# Round 11: https://doi.org/10.21338/ess11e04_1
# Round 10SC: https://doi.org/10.21338/ess10sce03_2
# Round 10: https://doi.org/10.21338/ess10e03_3
# 
# Citation:
# Round 11: European Social Survey European Research Infrastructure (ESS ERIC) (2025) ESS11 - integrated file, edition 4.1 [Data set]. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/ess11e04_1
# Round 10SC: European Social Survey European Research Infrastructure (ESS ERIC) (2025) ESS10SC - integrated file, edition 3.2 [Data set]. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/ess10sce03_2
# Round 10: European Social Survey European Research Infrastructure (ESS ERIC) (2023) ESS10 - integrated file, edition 3.3 [Data set]. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/ess10e03_3

ess10_11 <- haven::read_spss(here::here("data", "ESS10e03_3-ESS10SCe03_2-ESS11e04_1-subset.sav"))


var_labels <- data.frame(
  variable = names(cronos),
  label = sapply(cronos, function(x) attr(x, "label")),
  stringsAsFactors = FALSE
)

View(var_labels)
