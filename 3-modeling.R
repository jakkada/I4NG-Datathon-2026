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
  predicted <- ifelse(fitted(model) >= threshold, 1, 0)
  actual    <- model$y
  print(confusionMatrix(factor(predicted), factor(actual)))
}

conf_matrix(M0,  "Model 0")
conf_matrix(M00, "Model 00")
conf_matrix(M1,  "Model 1")
conf_matrix(M11, "Model 11")

AIC(M0, M00, M1, M11)
