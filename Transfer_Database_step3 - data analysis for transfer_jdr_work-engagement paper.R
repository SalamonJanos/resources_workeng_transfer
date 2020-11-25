
# loading packages
library(haven)
library(tidyverse)
library(lavaan) # for calculating Cronbach alpha
library(semTools)  # for calculating Cronbach alpha
library(semPlot)  # for making SEM path models
library(flextable) # for making regression table and descriptive table publication ready


# import database
work_data <- read_sav("data/Transfer_factors.sav")


## -------------------------------------------- preparation for analysis --------------------------------------

# creating necessary variables for visualization
work_data2 <- work_data %>% 
  filter(Study == 2) %>% 
  filter(Open_or_Closed_Skills == "Open") %>% 
  filter(timediff >= 13 & timediff <= 120) %>%
  filter(T_length_1 != "NA")


## internal consistency reliability (Cronbach's alpha and omega total) calculation
## --------------------------------------------------------------------------------------------------------
## internal consistency reliability (Cronbach's alpha and omega total) calculation

# calculating Cronbach alpha for Job resources
resources_factor <- '
res_factor =~ jdr1 + jdr3 + jdr5 + jdr7 + jdr11'

fit_resources <- cfa(resources_factor, data = work_data2, estimator = 'MLR')
resources_rel <- as.data.frame(reliability(fit_resources))


# calculating Cronbach alpha for Job demands
demands_factor <- '
dem_factor =~ jdr2 + jdr4 + jdr6 + jdr8 + jdr10'

fit_demands <- cfa(demands_factor, data = work_data2, estimator = 'MLR')
demands_rel <- as.data.frame(reliability(fit_demands))


# calculating Cronbach alpha for Engagement
engagement_factor <- '
engage_factor =~ uwes1 + uwes2 + uwes3 + uwes4 + uwes5 + uwes6 + uwes7 + uwes8 + uwes9'

fit_engagement <- cfa(engagement_factor, data = work_data2, estimator = 'MLR')
eng_rel <- as.data.frame(reliability(fit_engagement))


# calculating Cronbach alpha for Motivation
motivation_factor <- '
motiv_factor =~ mot26 + mot28 + mot212'

fit_motivation <- cfa(motivation_factor, data = work_data2, estimator = 'MLR')
motiv_rel <- as.data.frame(reliability(fit_motivation))


# calculating Cronbach alpha for Opportunity
opportunity_factor <- '
opport_factor =~ opp37 + opp39 + opp312'

fit_opportunity <- cfa(opportunity_factor, data = work_data2, estimator = 'MLR')
opport_rel <- as.data.frame(reliability(fit_opportunity))


# calculating Cronbach alpha for Transfer
transfer_factor <- '
use_factor =~ use1 + use3 + use5 + use7'

fit_transf <- cfa(transfer_factor, data = work_data2, estimator = 'MLR')
transfer_rel <- as.data.frame(reliability(fit_transf))


reliabilities <- cbind(resources_rel, demands_rel, eng_rel, motiv_rel, opport_rel, transfer_rel)
round(reliabilities[1:2,], 2)

## ---------------------------------------------- preparatory analysis -------------------------------------------

## ---------------------------------------- preparation for correlation table ------------------------------------

# check variables normality
shapiro.test(work_data2$job_resources)
shapiro.test(work_data2$job_demands)
shapiro.test(work_data2$uwes_all)
shapiro.test(work_data2$motivation)
shapiro.test(work_data2$opportunity)
shapiro.test(work_data2$use)

# significant Shapiro-Wilk normality tests --> Spearman correlations are necessary

corr_input <- c("job_resources", "job_demands", "uwes_all", "opportunity", "motivation", "use")

corr_table <- work_data2 %>% 
  select(., one_of(corr_input))

# # checking descriptives of items
# job_demands_inp <- c("jdr2", "jdr4", "jdr6", "jdr8", "jdr10")
# jdem_descriptives <- as.data.frame(psych::describe(work_data2[,job_demands_inp], skew = TRUE))

## ------------------------------------------ function to create Spearman corr table  ---------------------------------------

## function necessary for Spearman correlation table with stars indicating significance levels

# source of the following function: http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "***", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

## ---------------------------------  calculating correlations and creating correlation matrix ---------------------------------

correlation_table <- corstars(corr_table, method = "spearman")

# adding column to be able to combine correlation table with descriptive table
correlation_table <- correlation_table %>%
  mutate(use = "")


## ---------------------------------  calculating descriptives for final correlation table ---------------------------------

all_descriptives <- as.data.frame(psych::describe(work_data2[,corr_input], skew = TRUE))
descr1 <- all_descriptives[,c("mean", "sd")]
descr_table1 <- descr1

# source of the following function: https://jeromyanglim.tumblr.com/post/50228877196/round-numbers-in-data-frame-that-contains-non
round_df <- function(x, digits) {
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

# rounding numbers
descr_table1_2 <- round_df(descr_table1, 2)

## --------------------------------------  combining descriptives and correlation tables -------------------------------------

#combined_tables <- rbind(correlation_table, descr_table1_2)
combined_tables <- cbind(descr_table1_2, correlation_table)

row.names(combined_tables) <- c("1. Job Resources", "2. Job Demands", "3. Work Engagement", 
                                "4. Opportunity to Transfer", "5. Motivation to Transfer", 
                                "6. Training Transfer– Use")

# setting row names to first column
combined_tables <- tibble::rownames_to_column(combined_tables, " ")

combined_tables <- combined_tables %>%  
  rename(
    "1" = job_resources,
    "2" = job_demands,
    "3" = uwes_all,
    "4" = opportunity,
    "5" = motivation,
    "6" = use)



# designing final correlation table
designed_table <- combined_tables %>% 
  flextable() %>% 
  hline(i = 6, part = "body", border = officer::fp_border()) %>% 
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "left", part = "all") %>% 
  width(width = 0.59) %>% 
  #  set_table_properties(width = 1) %>% 
  add_footer_lines(c("Note. N = 311, M = Mean, SD = standard deviation", 
                     "* p < .05, ** p < .01, *** p < .001"))

# saving final table to word file
save_as_docx("Table 1. Descriptive statistics and Spearman bivariate correlations between variables" = designed_table, path = "JDR, WE, Transfer correlation table.docx")




## -----------------------------------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------------------------------
## ---------------------------------------------------- MAIN ANALYSIS -----------------------------------------------
## -----------------------------------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------------------------------

# Goodness-of-fit statistics for the estimated measurement models

# # job reosurces model 1 
# jres_model <- '
# # regressions
# jres =~  jdr1 + jdr3 + jdr5 + jdr7 + jdr9 + jdr11
# '
# 
# # CFA results - transfer (MLR)
# fit_jres <- cfa(jres_model, data = work_data2, estimator = 'MLR')
# summary(fit_jres, fit.measures = TRUE, standardized = TRUE)
# fit_jres_t1 <- round(fitMeasures(fit_jres)[c("chisq.scaled", "df.scaled", "pvalue.scaled",
#                                              "cfi.scaled", "tli.scaled", "rmsea.scaled", 
#                                              "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
#                                              "srmr", "aic", "bic")], 3)
# # # Fit indices highlighted that the hypothesized representation of the constructs 
# # # is acceptable (CFI = .977, TLI = .962, RMSEA = .056 [90% CI = 0.011 - 0.095]).

## --------------------------


# job reosurces and demands model 2 (without item jdr9)
jdr_model2 <- '
# regressions
jres2 =~  jdr1 + jdr3 + jdr5 + jdr7 + jdr11
jdem =~ jdr2 + jdr4 + jdr6 + jdr8 + jdr10
jres2 ~~ jdem
'

# CFA results - transfer (MLR)
fit_jdr2 <- cfa(jdr_model2, data = work_data2, estimator = 'MLR')
summary(fit_jdr2, fit.measures = TRUE, standardized = TRUE)
fit_jdr_t1 <- round(fitMeasures(fit_jdr2)[c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                              "cfi.scaled", "tli.scaled", "rmsea.scaled",
                                              "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
                                              "srmr", "aic", "bic")], 3)

## ------------------------------------------------------------------------------------


# job reosurces model 2 (without item jdr9) 
jres_model2 <- '
# regressions
jres2 =~  jdr1 + jdr3 + jdr5 + jdr7 + jdr11
'

# CFA results - transfer (MLR)
fit_jres2 <- cfa(jres_model2, data = work_data2, estimator = 'MLR')
summary(fit_jres2, fit.measures = TRUE, standardized = TRUE)
fit_jres_t1 <- round(fitMeasures(fit_jres2)[c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                              "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                              "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                              "srmr", "aic", "bic")], 3)

## ------------------------------------------------------------------------------------

# job demands model 
jdem_model <- '
# regressions
jdem =~ jdr2 + jdr4 + jdr6 + jdr8 + jdr10
'

# CFA results - transfer (MLR)
fit_jdem <- cfa(jdem_model, data = work_data2, estimator = 'MLR')
summary(fit_jdem, fit.measures = TRUE, standardized = TRUE)
fit_jdem_t1 <- round(fitMeasures(fit_jdem)[c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                             "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                             "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                             "srmr", "aic", "bic")], 3)


## ------------------------------------------------------------------------------------

# work engagement model 
we_model <- '
# regressions
eng =~ uwes1 + uwes2 + uwes3 + uwes4 + uwes5 + uwes6 + uwes7 + uwes8 + uwes9
'

# CFA results - transfer (MLR)
fit_we <- cfa(we_model, data = work_data2, estimator = 'MLR')
summary(fit_we, fit.measures = TRUE, standardized = TRUE)
fit_eng_t1 <- round(fitMeasures(fit_we)[c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                          "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                          "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                          "srmr", "aic", "bic")], 3)


## ------------------------------------------------------------------------------------

# motivation to transfer model 
motiv_model <- '
# regressions
motiv =~ mot26 + mot28 + mot212
'

# CFA results - transfer (MLR)
fit_motiv <- cfa(motiv_model, data = work_data2, estimator = 'MLR')
summary(fit_motiv, fit.measures = TRUE, standardized = TRUE)
fit_motiv_t1 <- round(fitMeasures(fit_motiv)[c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                               "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                               "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                               "srmr", "aic", "bic")], 3)


## ------------------------------------------------------------------------------------

# opportunity to transfer model 
opport_model <- '
# regressions
opport =~ opp37 + opp39 + opp312
'

# CFA results - transfer (MLR)
fit_opport <- cfa(opport_model, data = work_data2, estimator = 'MLR')
summary(fit_opport, fit.measures = TRUE, standardized = TRUE)
fit_opport_t1 <- round(fitMeasures(fit_opport)[c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                                 "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                                 "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                                 "srmr", "aic", "bic")], 3)

## ------------------------------------------------------------------------------------

# training transfer model 
tr_model <- '
# regressions
transfer =~ use1 + use3 + use5 + use7
'

# CFA results - transfer (MLR)
fit_tr <- cfa(tr_model, data = work_data2, estimator = 'MLR')
summary(fit_tr, fit.measures = TRUE, standardized = TRUE)
fit_transfer_t1 <- round(fitMeasures(fit_tr)[c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                               "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                               "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                               "srmr", "aic", "bic")], 3)


## ------------------------------------------------------------------------------------

## -----------------------------------------------------------------------------------------------------------------------

## Predictive model

# transfer model 1 (jdr9 was removed because its loading was below the .3 threshold)
transfer_model1 <- '
# regressions
eng =~ uwes1 + uwes2 + uwes3 + uwes4 + uwes5 + uwes6 + uwes7 + uwes8 + uwes9
jres =~  jdr1 + jdr3 + jdr5 + jdr7 + jdr11 
jdem =~ jdr2 + jdr4 + jdr6 + jdr8 + jdr10

transfer =~ use1 + use3 + use5 + use7

motiv =~ mot26 + mot28 + mot212
opport =~ opp37 + opp39 + opp312

transfer ~ eng + jres + jdem + motiv + opport
motiv ~ eng + jres + jdem
opport ~ eng + jres + jdem
eng ~ jres + jdem

# correlations
jres ~~ jdem
motiv ~~ opport
'


fit_transfer1 <- sem(transfer_model1, data = work_data2, estimator = 'MLR')
summary(fit_transfer1, fit.measures = TRUE, standardized = TRUE)
fit_transfer_t3 <- round(fitMeasures(fit_transfer1)[c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                  "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                  "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                  "srmr", "aic", "bic")], 3)

lavInspect(fit_transfer1, "rsquare")
# transfer r2 = .684
# eng r2 = .294
# motiv r2 = .103
# opport r2 = .137

## ------------------------------------------------------------------------------------

modificationindices(fit_transfer1, sort = TRUE)
# this suggests to correlate these two items: uwes1 ~~ uwes2
# which makes sense, based on their wording:
#   uwes1: At my work, I feel bursting with energy.
#   uwes2: At my job, I feel strong and vigorous.

## ------------------------------------------------------------------------------------

## Predictive model (modified)

# transfer model 1b (with correlated uwes1 ~~ uwes2)
transfer_model1b <- '
# regressions
eng =~ uwes1 + uwes2 + uwes3 + uwes4 + uwes5 + uwes6 + uwes7 + uwes8 + uwes9
jres =~  jdr1 + jdr3 + jdr5 + jdr7 + jdr11 
jdem =~ jdr2 + jdr4 + jdr6 + jdr8 + jdr10

transfer =~ use1 + use3 + use5 + use7

motiv =~ mot26 + mot28 + mot212
opport =~ opp37 + opp39 + opp312

transfer ~ eng + jres + jdem + motiv + opport
motiv ~ eng + jres + jdem
opport ~ eng + jres + jdem
eng ~ jres + jdem

# correlations
jres ~~ jdem
motiv ~~ opport
uwes1 ~~ uwes2
'

fit_transfer1b <- sem(transfer_model1b, data = work_data2, estimator = 'MLR')
summary(fit_transfer1b, fit.measures = TRUE, standardized = TRUE)
fit_transfer_t4 <- round(fitMeasures(fit_transfer1b)[c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                   "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                   "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                   "srmr", "aic", "bic")], 3)

beta_values <- parameterEstimates(fit_transfer1b, standardized = TRUE)
beta_values[30:44,]

# anova(fit_transfer1, fit_transfer1b)
# # Scaled Chi-Squared Difference Test suggests the superiority of fit_transfer1b 
# # (delta_df = 1, delta_Chisq = 71.141, p < .001)


lavInspect(fit_transfer1b, "rsquare")
# transfer r2 = .685
# eng r2 = .299
# motiv r2 = .103
# opport r2 = .139


## ------------------------------------------------------------------------------------

# ## Predictive model (modified, trimmed)
# 
# # transfer model 2 (trimmed model of 1b)
# transfer_model2 <- '
# # regressions
# eng =~ uwes1 + uwes2 + uwes3 + uwes4 + uwes5 + uwes6 + uwes7 + uwes8 + uwes9
# jres =~  jdr1 + jdr3 + jdr5 + jdr7 + jdr11 
# transfer =~ use1 + use3 + use5 + use7
# motiv =~ mot26 + mot28 + mot212
# opport =~ opp37 + opp39 + opp312
# 
# transfer ~ motiv + opport
# motiv ~ eng + jres
# opport ~ eng + jres
# eng ~ jres
# 
# # correlations
# motiv ~~ opport
# uwes1 ~~ uwes2
# '
# 
# fit_transfer2 <- sem(transfer_model2, data = work_data2, estimator = 'MLR')
# summary(fit_transfer2, fit.measures = TRUE, standardized = TRUE)
# fit_transfer_t5 <- round(fitMeasures(fit_transfer2)[c("chisq.scaled", "df.scaled", "pvalue.scaled",
#                                     "cfi.scaled", "tli.scaled", "rmsea.scaled", 
#                                     "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
#                                     "srmr", "aic", "bic")], 3)
# 
# lavInspect(fit_transfer2, "rsquare")
# # transfer r2 = .675
# # eng r2 = .270
# # motiv r2 = .092
# # opport r2 = .128


############################################################################################################
######### --------------------------------------------------------------------------------------############
############################################################################################################

# preparation for creating table for Goodness-of-fit statistics for the estimatted models


# fit_jres_t1
fit_jres_t1 <- as.data.frame(fit_jres_t1)
# setting row names to first column
fit_jres_t1 <- tibble::rownames_to_column(fit_jres_t1, " ")
names(fit_jres_t1) <- NULL

fit_jres_t1b <- t(fit_jres_t1)
fit_jres_t1b <- as.data.frame(fit_jres_t1b)


# fit_jdem_t1
fit_jdem_t1 <- as.data.frame(fit_jdem_t1)
# setting row names to first column
fit_jdem_t1 <- tibble::rownames_to_column(fit_jdem_t1, " ")
names(fit_jdem_t1) <- NULL

fit_jdem_t1b <- t(fit_jdem_t1)
fit_jdem_t1b <- as.data.frame(fit_jdem_t1b)


# fit_eng_t1
fit_eng_t1 <- as.data.frame(fit_eng_t1)
# setting row names to first column
fit_eng_t1 <- tibble::rownames_to_column(fit_eng_t1, " ")
names(fit_eng_t1) <- NULL

fit_eng_t1b <- t(fit_eng_t1)
fit_eng_t1b <- as.data.frame(fit_eng_t1b)


# fit_motiv_t1
fit_motiv_t1 <- as.data.frame(fit_motiv_t1)
# setting row names to first column
fit_motiv_t1 <- tibble::rownames_to_column(fit_motiv_t1, " ")
names(fit_motiv_t1) <- NULL

fit_motiv_t1b <- t(fit_motiv_t1)
fit_motiv_t1b <- as.data.frame(fit_motiv_t1b)


# fit_opport_t1
fit_opport_t1 <- as.data.frame(fit_opport_t1)
# setting row names to first column
fit_opport_t1 <- tibble::rownames_to_column(fit_opport_t1, " ")
names(fit_opport_t1) <- NULL

fit_opport_t1b <- t(fit_opport_t1)
fit_opport_t1b <- as.data.frame(fit_opport_t1b)




# fit_transfer_t1
fit_transfer_t1 <- as.data.frame(fit_transfer_t1)
# setting row names to first column
fit_transfer_t1 <- tibble::rownames_to_column(fit_transfer_t1, " ")
names(fit_transfer_t1) <- NULL

fit_transfer_t1b <- t(fit_transfer_t1)
fit_transfer_t1b <- as.data.frame(fit_transfer_t1b)



# fit_transfer_t3
fit_transfer_t3 <- as.data.frame(fit_transfer_t3)
# setting row names to first column
fit_transfer_t3 <- tibble::rownames_to_column(fit_transfer_t3, " ")
names(fit_transfer_t3) <- NULL

fit_transfer_t3b <- t(fit_transfer_t3)
fit_transfer_t3b <- as.data.frame(fit_transfer_t3b)


# fit_transfer_t4
fit_transfer_t4 <- as.data.frame(fit_transfer_t4)
# setting row names to first column
fit_transfer_t4 <- tibble::rownames_to_column(fit_transfer_t4, " ")
names(fit_transfer_t4) <- NULL

fit_transfer_t4b <- t(fit_transfer_t4)
fit_transfer_t4b <- as.data.frame(fit_transfer_t4b)


# # fit_transfer_t5
# fit_transfer_t5 <- as.data.frame(fit_transfer_t5)
# # setting row names to first column
# fit_transfer_t5 <- tibble::rownames_to_column(fit_transfer_t5, " ")
# names(fit_transfer_t5) <- NULL
# 
# fit_transfer_t5b <- t(fit_transfer_t5)
# fit_transfer_t5b <- as.data.frame(fit_transfer_t5b)


header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

fittable01 <- header.true(fit_jres_t1b)
fittable02 <- header.true(fit_jdem_t1b)
fittable03 <- header.true(fit_eng_t1b)
fittable04 <- header.true(fit_motiv_t1b)
fittable05 <- header.true(fit_opport_t1b)

fittable1 <- header.true(fit_transfer_t1b)
#fittable2 <- header.true(fit_transfer_t2b)
fittable3 <- header.true(fit_transfer_t3b)
fittable4 <- header.true(fit_transfer_t4b)
# fittable5 <- header.true(fit_transfer_t5b)


combined_fit_tables <- rbind(fittable01, fittable02, fittable03, fittable04, fittable05,
                             fittable1, fittable3, fittable4#, 
                             #fittable5
                             )

combined_fit_tables[] <- lapply(combined_fit_tables, function(x) {
  if(is.character(x)) as.numeric(as.character(x)) else x
})
sapply(combined_fit_tables, class)

combined_fit_tables2 <- data.frame(lapply(combined_fit_tables, function(x) gsub("^0\\.", "\\.", gsub("^-0\\.", "-\\.", sprintf("%.3f", x)))), stringsAsFactors = FALSE)

combined_fit_tables2 <- combined_fit_tables2 %>% 
  unite(RMSEA_CI, c(rmsea.ci.lower.scaled, rmsea.ci.upper.scaled), sep = " - ", remove = TRUE)

#Model <- c("JDR - Transfer", "JDR, WE - Transfer", "Predictive model", "Predictive model (modified)", "Predictive model (modified, trimmed)")
Model <- c("Job Resources", "Job Demands", "Work Engagement", 
           "Motivation to Transfer", "Opportunity to Transfer", "Training Transfer",  
           "Predictive model", "Predictive model (modified)"
           #, "Predictive model (modified, trimmed)"
           )
models <- as.data.frame(Model)

combined_fit_tables3 <- cbind(models, combined_fit_tables2)

# # setting row names to first column
# combined_fit_tables3 <- tibble::rownames_to_column(combined_fit_tables3, " ")

combined_fit_tables3 <- combined_fit_tables3 %>%  
  rename(
    "Chi2" = chisq.scaled,
    "df" = df.scaled,
    "p value" = pvalue.scaled,
    "CFI" = cfi.scaled,
    "TLI" = tli.scaled,
    "RMSEA" = rmsea.scaled,
    "RMSEA 90% CI" = RMSEA_CI,
    "SRMR" = srmr,
    "AIC" = aic,
    "BIC" = bic)



# designing final correlation table
designed_table <- combined_fit_tables3 %>% 
  flextable() %>% 
  hline(i = 6, part = "body", border = officer::fp_border()) %>% 
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "left", part = "all") %>% 
  autofit() %>% 
  add_footer_lines(c("Note. N = 311, χ2 chi-square test of exact fit, df degrees of freedom, CFI Comparative fit index, TLI Tucker–Lewis index, RMSEA root mean square error of approximation, 90% CI 90% confidence interval of the RMSEA, SRMR standardized root square residual", 
                     "* p < .05, ** p < .01, *** p < .001"))

# saving final table to word file
save_as_docx("Table 2. Goodness-of-fit statistics for the estimated measurement and predictive models" = designed_table, path = "Goodness-of-fit statistics - measurement and predictive models table.docx")



############################################################################################################
######### --------------------------------------------------------------------------------------############
############################################################################################################




pathdiagram1 <- semPaths(fit_transfer1b, 
                         layout = "tree2", centerLevels = TRUE,
                         whatLabels = "std", edge.label.cex = 1,
                         style="lisrel")


# pathdiagram1 <- semPaths(fit_transfer1b, 
#                          rotation = 2,
#                          layout = "tree3", centerLevels = TRUE,
#                          whatLabels = "std", edge.label.cex = 1,
#                          style="lisrel", layoutSplit = TRUE)


edgelabels = pathdiagram1$graphAttributes$Edges$labels  #this and the next line remove leading zeros from factor loading coefficients for easier reading (e.g., 0.78 becomes .78)
edgelabels[which(pathdiagram1$graphAttributes$Edges$curve == 0)] = gsub("0[.]", ".", edgelabels[which(pathdiagram1$graphAttributes$Edges$curve == 0)])
pathdiagram1$graphAttributes$Edges$lty[pathdiagram1$graphAttributes$Edges$lty != 1] = 1    # this and the next line change dotted lines to solid lines
pathdiagram1$graphAttributes$Edges$labels = edgelabels
# pathdiagram1$graphAttributes$Nodes$labels = names(pathdiagram1$graphAttributes$Nodes$labels)

plot(pathdiagram1)  












