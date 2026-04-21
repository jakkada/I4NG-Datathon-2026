# ============================== read me first ================================
# reproducibility note: #### 
# to run this script you need to source 2-preparation.R
# =============================================================================


library(survey)

c_fin <- c_fin[!is.na(c_fin$w1weight), ]

sjPlot::view_df(c_fin, show.frq = TRUE, show.prc = TRUE, show.na = TRUE, file = "c_fin_view.html")

# define the survey design #### 
design <- svydesign(
  ids = ~1,
  weights = ~w1weight,
  data = c_fin
)

# run logistic regression with all predictors #### 

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
  design = design,
  family = quasibinomial()
)

summary(M0)


exp(cbind(OR = coef(M0), confint(M0)))

#TODO nagelkerke R2 and confusion matrix + AUC 


# two models without countries #### 

#TODO add M1 model with income and compare N 
#TODO mean weight across all waves - add in data preparation 

## limited predictors M1, keep more observations #### 

M1 <- svyglm(
  TRC ~ FLC + PGW + IFS + SMP + LMA + BSC + # know-hows
    NETUALL + SCNTR + DIGSKILLT + # int usage, trust in scientists, participated in digi training
    GDF_C + EDY_C + AGE_C, # controls without income
  design = design,
  family = quasibinomial()
)

summary(M1)


exp(cbind(OR = coef(M1), confint(M1)))

## more substantive predictors M2 #### 

M2 <- svyglm(
  TRC ~ FLC + PGW + IFS + SMP + LMA + BSC + # know-hows
    NETUALL + SCNTR + GENTR + DIGSKILLT + # int usage, trust in scientists, gen trust, part. in digi training
    GDF_C + EDY_C + AGE_C + HTI_C, # controls with income
  design = design,
  family = quasibinomial()
)

summary(M2)


exp(cbind(OR = coef(M2), confint(M2)))

# M2 is a final model for the policy brief

# M3 as M0 but without income and gentrust 

M3 <- svyglm(
  TRC ~ CNTR_C + FLC + PGW + IFS + SMP + LMA + BSC + # know-hows
    NETUALL + SCNTR  + DIGSKILLT + # int usage, trust in scientists, part. in digi training
    GDF_C + EDY_C + AGE_C, # controls without income
  design = design,
  family = quasibinomial()
)

# M4 without age and trusts 

M4 <- svyglm(
  TRC ~ CNTR_C + 
    FLC + PGW + IFS + SMP + LMA + BSC + # know-hows
    NETUALL + DIGSKILLT + # int usage, without trust, part. in digi training
    GDF_C + EDY_C + HTI_C, # controls with income, without age
  design = design,
  family = quasibinomial()
)