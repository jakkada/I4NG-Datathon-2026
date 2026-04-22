# install.packages(c("sjlabelled","sjPlot","sjmisc","DataExplorer"))
# install.packages("psych")
library(sjlabelled)
library(sjPlot)
library(sjmisc)
library(DataExplorer)
library(psych)

# ============================== read me first ================================
# reproducibility note: #### 
# to run this script you need to source 1-load-data.R
# =============================================================================

# select variables #### 
c_selected <- cronos %>% select( # merging variables
                                idno,
                                cntry,
                                mode, 
                                essround,
                                # control variables
                                gndr,
                                eduyrs,
                                age,
                                hinctnta,
                                w1dq3,
                                # filter variables
                                netusoft,
                                w1dq2_7,
                                # dependent variable
                                w1sq4_4,
                                # social media use variables:
                                w1dq13,
                                w3dq44,
                                w3dq43,
                                w3dq42,
                                w3dq47,
                                w3dq48,
                                w3dq45,
                                # general use of digital devices and general topics related
                                # to science and technology variables
                                w5dq1_1:w5dq1_11,
                                w1sq4_3,
                                w1sq4_1,
                                # technological adaptation variables
                                w4dq8,
                                w5dq10,
                                # attitude to new technologies variable
                                w4dq11,
                                # trust variables
                                w1sq16,
                                # weighing variables 
                                w1weight, w2weight, w3weight, w4weight, w5weight)


# filtering ####

# create new variable no_int_access_or_use 
# to exclude respondents who did not use the internet
# or had no access to it 

# UWAGA! TUTAJ albo cos posżło nie tak albo jest różnica w wielkosci próby, bo wychodzi, ze trzeba odfiltrować 467 bo 441 to wszystkie 1 dla netusoft, 189 to jedynki dla w1dq2_7, a przecinają się na wartości 163, która jest zatem liczna podwójnie więc trzeba ją odjąć. W poprzednich analizach wychodziło nam: # 320 + 189 - 163 = 346 # these resp. should be excluded.A kodu nie zmieniałem

c_mutated <- c_selected %>%
  mutate(no_int_access_or_use = case_when(
    netusoft == 1 | w1dq2_7 == 1 ~ 1, .default = 0))

# frq(c_mutated$no_int_access_or_use) # value 1 is 467 resp. 

c_mutated <- c_mutated %>% filter(no_int_access_or_use==0)

# tab_xtab(c_selected$netusoft, c_selected$w1dq2_7, show.na = T)
# 441 + 189 - 163 = 467 # these resp. should be excluded


# prepare country names #### 

c_mutated$CTR <- as_label(c_mutated$cntry)

c_mutated <- left_join(x = c_mutated, 
                       y = ess10_11)

# prepare control variables #### 

c_mutated <- c_mutated %>% 
  mutate(
    GND = if_else(gndr == 2, 1, 0), # Female is 1
    EDY = as.numeric(eduyrs), # Years of full-time education completed
    HIN = as.numeric(hinctnta), # Household's total net income, all sources in deciles
    AGE = as.numeric(age) # Age of respondent, calculated
  )

# prepare all other dependent and independent variables #### 

c_mutated <- c_mutated %>%
  # Recode "know how" variables: 1 = "Very true of me" (value 5), 0 = all others
  mutate(across(
    .cols = c(w1dq13, w5dq10),
    .fns = ~ if_else(.x == 5, 1, 0),
    .names = "{.col}_bin"
  )) %>%
  rename(
    SML = w1dq13_bin,  # Limit access to social media content
    AIR = w5dq10_bin   # Recognize AI-generated or altered text, image, or video
  ) %>%
  
  # Recode internet use: 1 = "Almost all the time" (value 1), 0 = all others
  # "How often typically use internet on any device for work or personal use last month"
  mutate(
    UNA = if_else(w1dq3 == 1, 1, 0)
  ) %>%
  
  # Recode to numeric
  mutate(
    STR = as.numeric(w1sq16), # "In general, how much do you trust scientists"
    SMM = as.numeric(w3dq47), # "Social media exposes people to misinformation"
    SMU = as.numeric(w3dq48), # "Social media undermines personal privacy"
    SMF = as.numeric(w3dq45)  # "Social media prevents you from fulfilling work and family duties"  
  ) %>%
  
  # Convert general trust variables to numeric and compute row mean
  mutate(
    pplfair_num = as.numeric(pplfair), # "Most people try to take advantage of you, or try to be fair"
    pplhlp_num  = as.numeric(pplhlp),  # "Most of the time people helpful or mostly looking out for themselves"
    ppltrst_num = as.numeric(ppltrst), # "Most people can be trusted or you can't be too careful"
    GTR = rowMeans(across(c(pplfair_num, pplhlp_num, ppltrst_num)), na.rm = TRUE)
  ) %>%
  
  # Sum index for intentionally avoiding using digital devices in various situations
  mutate(
    AUD = rowSums(across(w5dq1_1:w5dq1_8), na.rm = TRUE) 
  ) %>% 
  # Recoding other variables
  mutate(
    SMS = if_else(w3dq44 %in% 1:3, 1, 0),   # How often scroll through social media feeds
    SMR = if_else(w3dq43 %in% 1:3, 1, 0),   # How often react to content on social media
    SMP = if_else(w3dq42 %in% 1:5, 1, 0),   # How often post content on social media
    SMS2 = if_else(w3dq44 %in% 1:2, 1, 0),  # How often scroll through social media feeds - version 2
    SMR2 = if_else(w3dq43 %in% 1:2, 1, 0),  # How often react to content on social media - version 2
    ADA = if_else(w4dq8 %in% 1:2, 1, 0),    # How well adapt to technological advancements
    CTA = if_else(w4dq11 %in% 4:5, 1, 0)    # What changes will technological advancements bring to society
  ) %>%
  
  # Renaming variables that need no recoding
  mutate(
    TMD = w1sq4_4, # Topics taught more in compulsory education: Critical thinking, media and democracy [dependent variable]
    DTP = w1sq4_3, # Topics taught more in compulsory education: Digital tools and programming
    MAS = w1sq4_1  # Topics taught more in compulsory education: Maths and sciences
  )

# a <- alpha(c_mutated[, paste0("w5dq1_", 1:8)]) 
# a$total$raw_alpha
# a$alpha.drop
# items <- c_mutated[, paste0("w5dq1_", 1:8)]
# cor(items, use = "pairwise.complete.obs")

a <- alpha(c_mutated[, c("pplfair","pplhlp","ppltrst")]) # Alpha 0,76; warrning can be ignored - it applies to
                                                        # frequencies not alpha itself
# a$total$raw_alpha

# set descriptive variable labels #### 

c_mutated <- c_mutated %>%
  mutate(
    # dependent variable
    TMD = set_label(TMD, "Topics taught more in compulsory education: Critical thinking, media and democracy (1 = 'Marked', 0 = 'Not marked')"),
    # control variables
    GND = set_label(GND, "Gender (1 = Female, 0 = Male)"),
    EDY = set_label(EDY, "Years of full-time education completed (num)"),
    AGE = set_label(AGE, "Age of respondent (num)"),
    HIN = set_label(HIN, "Household total net income decile (num)"),
    UNA = set_label(UNA, "Uses internet almost all the time (1 = yes, 0 = other)"),
    # social media use variables:
    SML = set_label(SML, "Limit access to social media content (1 = 'Very true of me', 0 = other)"),
    SMS = set_label(SMS, "How often scroll through social media feeds (1 = At least once a day, 0 = less often"),
    SMS2 = set_label(SMS2, "How often scroll through social media feeds (1 = At least several times a day, 0 = less often"),
    SMR = set_label(SMR, "How often react to content on social media (1 = At least once a day, 0 = less often)"),
    SMR2 = set_label(SMR, "How often react to content on social media (1 = At least several times a day, 0 = less often)"),
    SMP = set_label(SMP, "How often post content on social media (1 = At least once a week, 0 = less often)"),
    SMM = set_label(SMM, "Social media exposes people to misinformation (num)"),
    SMU = set_label(SMU, "Social media undermines personal privacy (num)"),
    SMF = set_label(SMF, "Social media prevents you from fulfilling work and family duties (num)"),
    # general use of digital devices and general topics related
    # to science and technology variables
    AUD = set_label(AUD, "Intentionally avoiding using digital devices in various situations (num; sum index)"),
    DTP = set_label(DTP, "Topics taught more in compulsory education: Digital tools and programming (1 = 'Marked', 0 = 'Not marked')"),
    MAS = set_label(MAS, "Topics taught more in compulsory education: Maths and sciences (1 = 'Marked', 0 = 'Not marked')"),
    # technological adaptation variables
    ADA = set_label(ADA, "How well adapt to technological advancements (1 = 'Very well' & 'Well', 0 = worse)"),
    AIR = set_label(AIR, "Recognize AI-generated or altered text, image, or video (1 = 'Very true of me', 0 = other)"),
    # attitude to new technologies variable
    CTA = set_label(CTA, "What changes will technological advancements bring to society (1 = 'Negative' & 'Very negative', 0 = more positive)"),
    # trust variables
    STR = set_label(STR, "Trust in scientists (num)"),
    GTR = set_label(GTR, "General trust: mean of pplfair, pplhlp, ppltrst (num)")
  )

# create final dataset #### 

analysis_var_names <- c_mutated %>%
  select(all_of(names(.)[grepl("^[A-Z]", names(.))])) %>% 
  names() # set prepared variables' names

c_mutated <- c_mutated %>% 
  mutate(id = paste(idno, cntry, mode, essround, sep = "_")) # make unique ids

c_fin <- c_mutated %>% select(id, analysis_var_names, w1weight, w2weight, w3weight, w4weight, w5weight)

# haven::write_sav(data = c_fin, path = "data/cronos3_gotowe_hackaton.sav")
## save file 