# ============================== read me first ================================
# reproducibility note: #### 
# to run this script you need to source 2-preparation.R
# =============================================================================


library(survey)

c_fin <- c_fin %>% mutate(w1to5weight_mean = rowMeans(across(w1weight:w5weight), na.rm = TRUE))

c_fin_w1weight <- c_fin[!is.na(c_fin$w1weight), ]
c_fin_w1to5 <- c_fin[!is.na(c_fin$w1to5weight_mean), ]

sjPlot::view_df(c_fin, show.frq = TRUE, show.prc = TRUE, show.na = TRUE, file = "c_fin_view.html")

# define the survey design #### 

## weights from wave 1 #####
design1 <- svydesign(
  ids = ~1,
  weights = ~w1weight,
  data = c_fin_w1weight
)

## mean weights across all waves #### 

design_mean <- svydesign(
  ids = ~1,
  weights = ~w1to5weight_mean,
  data = c_fin_w1to5
)

# run M0 without income for higher N #### 

M0 <- svyglm(
  TMD ~ factor(CTR) + 
  # control variables
  GND +
  EDY +
  AGE +
  #HIN +
  UNA +
  # social media use variables:
  SML +
  SMS +
  SMR +
  SMP +
  SMM +
  SMU +
  SMF +
  # general use of digital devices and general topics related to science and technology variables
  AUD +
  DTP +
  MAS +
  # technological adaptation variables
  ADA +
  AIR +
  # attitude to new technologies variable
  CTA +
  # trust variables
  STR +
  GTR,
  design = design1,
  family = quasibinomial()
)

summary(M0)


exp(cbind(OR = coef(M0), confint(M0)))

#TODO nagelkerke R2 and confusion matrix + AUC 

# M1 with Household total net income decile HIN #### 

M1 <- svyglm(
  TMD ~ factor(CTR) + 
    # control variables
    GND +
    EDY +
    AGE +
    HIN +
    UNA +
    # social media use variables:
    SML +
    SMS +
    SMR +
    SMP +
    SMM +
    SMU +
    SMF +
    # general use of digital devices and general topics related to science and technology variables
    AUD +
    DTP +
    MAS +
    # technological adaptation variables
    ADA +
    AIR +
    # attitude to new technologies variable
    CTA +
    # trust variables
    STR +
    GTR,
  design = design1,
  family = quasibinomial()
)

summary(M1)


exp(cbind(OR = coef(M1), confint(M1)))




tab_model(M0, M1, pred.labels = FALSE, dv.labels = c("M0", "M1"))




# M00 as M0 = without income, but mean weights across all waves for even higher N #### 

M00 <- svyglm(
  TMD ~ factor(CTR) + 
    # control variables
    GND +
    EDY +
    AGE +
    #HIN +
    UNA +
    # social media use variables:
    SML +
    SMS +
    SMR +
    SMP +
    SMM +
    SMU +
    SMF +
    # general use of digital devices and general topics related to science and technology variables
    AUD +
    DTP +
    MAS +
    # technological adaptation variables
    ADA +
    AIR +
    # attitude to new technologies variable
    CTA +
    # trust variables
    STR +
    GTR,
  design = design_mean,
  family = quasibinomial()
)

summary(M00)


exp(cbind(OR = coef(M00), confint(M00)))

# M11 as M1 = with Household total net income decile HIN, mean weights across all waves #### 

M11 <- svyglm(
  TMD ~ factor(CTR) + 
    # control variables
    GND +
    EDY +
    AGE +
    HIN +
    UNA +
    # social media use variables:
    SML +
    SMS +
    SMR +
    SMP +
    SMM +
    SMU +
    SMF +
    # general use of digital devices and general topics related to science and technology variables
    AUD +
    DTP +
    MAS +
    # technological adaptation variables
    ADA +
    AIR +
    # attitude to new technologies variable
    CTA +
    # trust variables
    STR +
    GTR,
  design = design_mean,
  family = quasibinomial()
)

summary(M11)


exp(cbind(OR = coef(M11), confint(M11)))


# model stats #### 

tab_model(M0, M00, M1, M11, pred.labels = FALSE, dv.labels = c("M0", "M00", "M1", "M11"))

plot_models(M0, M00, M1, M11, 
            m.labels = c("Model 0", "Model 00", "Model 1", "Model 11"), 
            p.shape = TRUE)

# ggsave("plot_models.png", width = 20, height = 50, units = "cm")


# Function to extract stats for each model

library(pROC)
model_stats <- function(model, name) {
  tibble(
    Model = name,
    N = nobs(model),
    Nagelkerke_R2 = round(psrsq(model, method = "Nagelkerke"), 3),
    AUC = round(auc(roc(model$y, fitted(model))), 3)
  )
}

# Combine into one table
table_stats <- bind_rows(
  model_stats(M0,  "Model 0"),
  model_stats(M00, "Model 00"),
  model_stats(M1,  "Model 1"),
  model_stats(M11, "Model 11")
)

print(table_stats)

library(caret)

# Function to print confusion matrix
conf_matrix <- function(model, name, threshold = 0.5) {
  cat("\n===", name, "===\n")
  predicted <- factor(ifelse(fitted(model) >= threshold, 1, 0), levels = c(1, 0))
  actual    <- factor(model$y, levels = c(1, 0))
  print(confusionMatrix(predicted, actual, positive = "1"))
}

conf_matrix(M0,  "Model 0")
conf_matrix(M00, "Model 00")
conf_matrix(M1,  "Model 1")
conf_matrix(M11, "Model 11")

AIC(M0, M00, M1, M11)


conf_full <- function(model, name, threshold = 0.5) {
  predicted <- factor(ifelse(fitted(model) >= threshold, 1, 0), levels = c(1, 0))
  actual    <- factor(model$y, levels = c(1, 0))
  cm        <- confusionMatrix(predicted, actual)
  
  tibble(
    Model       = name,
    TN          = cm$table[2, 2],
    FP          = cm$table[1, 2],
    FN          = cm$table[2, 1],
    TP          = cm$table[1, 1],
    Accuracy    = round(cm$overall["Accuracy"], 3),
    Sensitivity = round(cm$byClass["Sensitivity"], 3),
    Specificity = round(cm$byClass["Specificity"], 3),
    PPV         = round(cm$byClass["Pos Pred Value"], 3),
    NPV         = round(cm$byClass["Neg Pred Value"], 3)
  )
}

conf_full_table <- bind_rows(
  conf_full(M0,  "Model 0"),
  conf_full(M00, "Model 00"),
  conf_full(M1,  "Model 1"),
  conf_full(M11, "Model 11")
)

print(conf_full_table)

# find wave weights where N is the highest - that wave weights should be used #### 

cronos %>% 
  select(contains("weight")) %>%
  mutate(across(
  .cols = c(w1weight:w5weight),
  .fns = ~ !is.na(.x),
  .names = "{.col}_na"
)) %>% 
  select(contains("_na")) %>% 
  frq() # max is 85.96% for w2weight

# final model MFIN for dathaton, w2weight and income #### 

c_fin_w2weight <- c_fin[!is.na(c_fin$w2weight), ]

## weights from wave 2 #####
design2 <- svydesign(
  ids = ~1,
  weights = ~w2weight,
  data = c_fin_w2weight
)

## run MFIN #### 

MFIN <- svyglm(
  TMD ~ factor(CTR) + 
    # control variables
    GND +
    EDY +
    AGE +
    HIN +
    UNA +
    # social media use variables:
    SML +
    SMS +
    SMR +
    SMP +
    SMM +
    SMU +
    SMF +
    # general use of digital devices and general topics related to science and technology variables
    AUD +
    DTP +
    MAS +
    # technological adaptation variables
    ADA +
    AIR +
    # attitude to new technologies variable
    CTA +
    # trust variables
    STR +
    GTR,
  design = design1,
  family = quasibinomial()
)

summary(MFIN)

# model stats final #### 

tab_model(M0, M00, M1, M11, MFIN, pred.labels = FALSE, dv.labels = c("M0", "M00", "M1", "M11", "MFIN"))

plot_models(M0, M00, M1, M11, MFIN, 
            m.labels = c("Model 0", "Model 00", "Model 1", "Model 11", "MFIN"), 
            p.shape = TRUE)

# ggsave("plot_models_fin.png", width = 20, height = 50, units = "cm")


# Function to extract stats for each model

# Combine into one table
table_stats_fin <- bind_rows(
  model_stats(M0,  "Model 0"),
  model_stats(M00, "Model 00"),
  model_stats(M1,  "Model 1"),
  model_stats(M11, "Model 11"),
  model_stats(MFIN, "Final w2weights")
)

table_stats_fin

plot_model(MFIN, sort.est = TRUE, auto.label = F, title = "MFIN", show.values = T, show.p = T)
tab_model(MFIN, pred.labels = F, file = "MFIN_tab.html")


fin_conf_full_table <- bind_rows(
  conf_full(M0,  "Model 0"),
  conf_full(M00, "Model 00"),
  conf_full(M1,  "Model 1"),
  conf_full(M11, "Model 11"), 
  conf_full(MFIN, "Final model")
)

print(fin_conf_full_table)


