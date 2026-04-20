library(tidyverse)
library(DataExplorer)
library(haven)
library(sjmisc)
library(sjPlot)

d <- read_spss("CRONOS-3 Theme - All items Waves 1-5.sav", 
               user_na = FALSE)

d_na <- read_spss("CRONOS-3 Theme - All items Waves 1-5.sav", 
               user_na = TRUE)

# seeking for missings
DataExplorer::create_report(d)

frq(d$w3dq42)

d_na_w3dq42 <- d %>% filter(is.na(w3dq42))


DataExplorer::create_report(d_na_w3dq42[, 1:30])

frq(d$w3mode, out = "v")

tab_xtab(d$w3dq42, d$w3mode, show.na = T)

frq(d_na$w3mode, out = "v")

tab_xtab(d_na$w3dq42, d_na$w3mode, show.na = T)

tab_xtab(d_na$cntry, d_na$w3mode, show.na = T)

# https://ess.sikt.no/en/study/8ff4d5a3-20c6-4d22-b7db-69e8bdf0d68f/

tab_xtab(is.na(d_na$w1mode), is.na(d_na$w3mode), show.na = T)
tab_xtab(is.na(d_na$w2mode), is.na(d_na$w3mode), show.na = T)
tab_xtab(is.na(d_na$w1mode), is.na(d_na$w2mode), show.na = T)

# recoding to use in main script ########### Kuba, to dla Ciebie :) 

d_recoded <- d %>% ### change dataobject to proper name
  # Recode "know how" variables: 1 = "Very true of me" (value 5), 0 = all others
  mutate(across(
    .cols = c(w1dq13, w5dq10),
    .fns = ~ if_else(.x == 5, 1, 0),
    .names = "{.col}_bin"
  )) %>%
  # Recode internet use: 1 = "Almost all the time" (value 1), 0 = all others
  # "How often typically use internet on any device for work or personal use last month"
  mutate(
    NETUALL = if_else(w1dq3 == 1, 1, 0)
  ) %>%
  # Recode to numeric
  # "In general, how much do you trust scientists"
  # "Social media exposes people to misinformation"
  # "Social media undermines personal privacy"
  # "Social media prevents you from fulfilling work and family duties"
  mutate(
    SCNTR = as.numeric(w1sq16),
    SM_EPM = as.numeric(w3dq47),
    SM_UPP = as.numeric(w3dq48),
    SM_WFD = as.numeric(w3dq45)
  ) %>%
  # # Convert general trust variables to numeric and compute row mean
  # mutate(
  #   pplfair_num = as.numeric(pplfair), # "Most people try to take advantage of you, or try to be fair"
  #   pplhlp_num  = as.numeric(pplhlp), # "Most of the time people helpful or mostly looking out for themselves"
  #   ppltrst_num = as.numeric(ppltrst), # "Most people can be trusted or you can't be too careful"
  #   GENTR = rowMeans(across(c(pplfair_num, pplhlp_num, ppltrst_num)), na.rm = TRUE)
  # ) %>%
  # Sum index for intentionally avoiding using digital devices
  mutate(
    IAUDD = rowSums(across(w5dq1_1:w5dq1_7), na.rm = TRUE)
  ) %>% 
  # How often scroll, post, react on social media
  # How well adapt to tech advancements
  # What changes will tech advancements bring
  mutate(
    SCRLL = if_else(w3dq44 %in% 1:3, 1, 0),
    LIKE = if_else(w3dq43 %in% 1:3, 1, 0),
    POST = if_else(w3dq42 %in% 1:5, 1, 0), 
    SCRLL2 = if_else(w3dq44 %in% 1:2, 1, 0),
    LIKE2 = if_else(w3dq43 %in% 1:2, 1, 0),
    WTADV = if_else(w4dq8 %in% 1:2, 1, 0),
    BTADV = if_else(w4dq11 %in% 4:5, 1, 0)
  )






