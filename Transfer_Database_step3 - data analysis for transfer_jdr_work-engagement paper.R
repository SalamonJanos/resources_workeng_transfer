
# loading packages
library(haven)
library(tidyverse)
library(lavaan) # for calculating Cronbach alpha
library(semTools)  # for calculating Cronbach alpha
library(semPlot)  # for making SEM path models
library(flextable) # for making regression table and descriptive table publication ready
library(gtools) # for adding significance stars e.g., to standardized parameter estimates
library(reshape2) # for creating matrix from data frame columns used for correlations of latent variables 

# import database
work_data <- read_sav("data/Transfer_factors.sav")


## -------------------------------------------------- Preparation for analysis ---------------------------------------------------

# creating necessary variables for analysis
work_data2 <- work_data %>% 
  filter(Study == 2) %>% 
  filter(Open_or_Closed_Skills == "Open") %>% 
  filter(timediff >= 13 & timediff <= 120) %>%
  filter(T_length_1 != "NA")


## ---------------------------------------------------- 1. Preliminary analyses -----------------------------------------------------

## -------------------------- 1.1. Internal consistency (alpha and omega) calculation ---------------------------

# calculating Cronbach alpha for Job resources

# resources_factor_original <- '
# res_factor_orig =~ jdr1 + jdr3 + jdr5 + jdr7 + jdr9 + jdr11'
# fit_resources_orig <- cfa(resources_factor_original, data = work_data2, estimator = 'MLR')
# summary(fit_resources_orig, fit.measures = TRUE, standardized = TRUE, rsquare=T, ci = TRUE)
# # factor loading of jdr9 did not reach 0.3 so it will be removed from the further phases of the analysis

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
eng     =~ uwes1 + uwes2 + uwes3 + uwes4 + uwes5 + uwes6 + uwes7 + uwes8 + uwes9'

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
reliabilities2 <- round(reliabilities[1:2,], 3)


## ---------------------------------------- 1.2. CORRELATION of MEASURED variables ------------------------------------


# 1.2.1. preparation for correlation table --------------------------------

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


# 1.2.2. function to create Spearman corr table ---------------------------

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
  
  ## build a new matrix that includes the correlations with their appropriate stars
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



# 1.2.3. calculating correlations and creating correlation matrix ---------

correlation_table <- corstars(corr_table, method = "spearman")

# adding column to be able to combine correlation table with descriptive table
correlation_table <- correlation_table %>%
  mutate(use = "")




# 1.2.4. calculating descriptives for final correlation table -------------

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



# 1.2.5. combining descriptives and correlation tables --------------------



combined_tables <- cbind(descr_table1_2, correlation_table)

row.names(combined_tables) <- c("1. Job Resources", "2. Job Demands", "3. Work Engagement", 
                                "4. Opportunity to Transfer", "5. Motivation to Transfer", 
                                "6. Training Transfer– Use")

# setting row names to first column
combined_tables <- tibble::rownames_to_column(combined_tables, " ")

# removing last (empty) column
combined_tables[,9] <- NULL

# renaming column names
combined_tables <- combined_tables %>%  
  rename(
    "M" = mean,
    "SD" = sd,
    "1" = job_resources,
    "2" = job_demands,
    "3" = uwes_all,
    "4" = opportunity,
    "5" = motivation)


# (Table) Descr stat and Spearman bivariate corr btw MEASURED vars --------

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
save_as_docx("Table -. Descriptive statistics and Spearman bivariate correlations between measured variables" = designed_table, 
             path = "Table - JDR, WE, Transfer correlation table of measured variables.docx")



## ---------------------------------------- 1.3. CORRELATION of LATENT factors ------------------------------------


# 1.3.1. correlation model --------------------------------

# transfer model 1 
transfer_corr_model <- '
# regressions
jres =~  jdr1 + jdr3 + jdr5 + jdr7 + jdr11 
jdem =~ jdr2 + jdr4 + jdr6 + jdr8 + jdr10

eng  =~ uwes1 + uwes2 + uwes3 + uwes4 + uwes5 + uwes6 + uwes7 + uwes8 + uwes9

opport =~ opp37 + opp39 + opp312
motiv =~ mot26 + mot28 + mot212
transfer =~ use1 + use3 + use5 + use7

# correlations
jres ~~ eng
jres ~~ opport
jres ~~ motiv
jres ~~ transfer
jdem ~~ jres
jdem ~~ eng
jdem ~~ opport
jdem ~~ motiv
jdem ~~ transfer
eng ~~ opport
eng ~~ motiv
transfer ~~ eng

opport ~~ motiv
opport ~~ transfer

motiv ~~ transfer
'


fit_transf_corr <- sem(transfer_corr_model, data = work_data2, estimator = 'MLR')
sem_corr <- summary(fit_transf_corr, fit.measures = TRUE, standardized = TRUE, rsquare=T, ci = TRUE)
fit_corr <- round(fitMeasures(fit_transf_corr)[c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                                      "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                                      "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                                      "srmr", "aic", "bic")], 3)

# 1.3.2. creating dataframe for Table 1. (corr between latent vars) --------------

corr_estimates <- sem_corr$PE[30:44,c("lhs", "op", "rhs", "pvalue", "ci.lower", "ci.upper", "std.all")]
corr_estimates_nr <- sem_corr$PE[30:44,c("pvalue", "ci.lower", "ci.upper", "std.all")]
med_estimates_txt <- sem_corr$PE[30:44,c("lhs", "op", "rhs")]


# add significance stars from p values
corr_estimates_nr$sign <- stars.pval(corr_estimates_nr$pvalue)
corr_estimates_sign_nr <- corr_estimates_nr[1:15,"sign"]
corr_estimates_nr <- corr_estimates_nr[1:15,1:4]

# change class to numeric
# solution was found here: https://stackoverflow.com/questions/26391921/how-to-convert-entire-dataframe-to-numeric-while-preserving-decimals
corr_estimates_nr[] <- lapply(corr_estimates_nr, function(x) {
  if(is.character(x)) as.numeric(as.character(x)) else x
})
sapply(corr_estimates_nr, class)

# removing leading zeros in numbers
# solution was found here: https://stackoverflow.com/questions/53740145/remove-leading-zeros-in-numbers-within-a-data-frame
corr_estimates_nr2 <- data.frame(lapply(corr_estimates_nr, function(x) gsub("^0\\.", "\\.", gsub("^-0\\.", "-\\.", sprintf("%.3f", x)))), stringsAsFactors = FALSE)


# bind columns that contain text, modified numeric values and significance stars 
corr_estimates2 <- cbind(med_estimates_txt, corr_estimates_nr2, corr_estimates_sign_nr)

corr_estimates2 <- corr_estimates2 %>% 
  unite("95% CI", c(ci.lower, ci.upper), sep = ", ", remove = TRUE) %>% 
  unite("standardized", c(std.all, corr_estimates_sign_nr), sep = "", remove = TRUE)

corr_estimates2$`95% CI` <- corr_estimates2$`95% CI` %>%
  paste("[", ., "]")

corr_estimates3 <- corr_estimates2[1:15,c(1,3,6)]

# library(reshape2) used here for creating matrix from data frame columns
latent_corr <- acast(corr_estimates3, lhs~rhs, value.var="standardized")

latent_corr[1, 2] <- latent_corr[2, 1]
latent_corr[4, 1] <- latent_corr[1, 3]
latent_corr[5, 1] <- latent_corr[1, 4]
latent_corr[4, 2] <- latent_corr[2, 3]
latent_corr[5, 2] <- latent_corr[2, 4]
latent_corr[4, 4] <- latent_corr[5, 3]


latent_corr1 <- latent_corr[1:5, 1:2]
latent_corr2 <- latent_corr[1:5, 3:5]


transf <- as.matrix(t(latent_corr[,5]))
rownames(transf) <- c("transfer")
transf_col <- as.matrix(NA)
colnames(transf_col) <- c("transfer")
rownames(transf_col) <- c("transfer")
transf <- cbind(transf, transf_col)




jres <- as.matrix(latent_corr[3,])
colnames(jres) <- c("jres")

jres1 <- as.matrix(jres[1:2, 1])
jres2 <- as.matrix(jres[3:4, 1])

jres_row <- as.matrix(NA)
rownames(jres_row) <- c("jres")
jres_new <- rbind(jres1, jres_row, jres2)
colnames(jres_new) <- c("jres")

latent_corr_tab <- cbind(latent_corr1, jres_new, latent_corr2)

latent_corr_tab2 <- rbind(latent_corr_tab, transf)


col.order <- c("jres","jdem","eng","motiv","opport", "transfer")
row.order <- c("jres","jdem","eng","motiv","opport", "transfer")
latent_corr_tab3 <- latent_corr_tab2[row.order,col.order]
latent_corr_tab4 <- as.data.frame(latent_corr_tab3)

latent_corr_tab4[upper.tri(latent_corr_tab4)] <- NA




row.names(latent_corr_tab4) <- c("1. Job Resources", "2. Job Demands", "3. Work Engagement", 
                                "4. Motivation to Transfer", "5. Opportunity to Transfer",
                                "6. Training Transfer– Use")

# setting row names to first column
latent_corr_tab4 <- tibble::rownames_to_column(latent_corr_tab4, " ")

# removing last (empty) column
latent_corr_tab4[,7] <- NULL

# renaming column names
latent_corr_tab4 <- latent_corr_tab4 %>%  
  rename(
    "1" = jres,
    "2" = jdem,
    "3" = eng,
    "4" = motiv,
    "5" = opport)


# Table 1. Bivariate correlations between LATENT variables ----------------

# designing final correlation table
designed_corr_table <- latent_corr_tab4 %>% 
  flextable() %>% 
  # hline(i = 6, part = "body", border = officer::fp_border()) %>% 
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "left", part = "all") %>% 
  # width(width = 0.59) %>% 
  autofit() %>% 
  #  set_table_properties(width = 1) %>% 
  add_footer_lines(c("Note. N = 311.", 
                     "* p < .05, ** p < .01, *** p < .001"))

# saving final table to word file
save_as_docx("Table 1. Bivariate correlations between latent variables" = designed_corr_table, 
             path = "Table 1 - JDR, WE, Transfer correlation table of latent variables.docx")




## ----------------------------------------------- MAIN ANALYSIS ------------------------------------------


# predictive model --------------------------------------------------------

# transfer model 
transfer_model <- '
# regressions
jres =~  jdr1 + jdr3 + jdr5 + jdr7 + jdr11 
jdem =~ jdr2 + jdr4 + jdr6 + jdr8 + jdr10

eng  =~ uwes1 + uwes2 + uwes3 + uwes4 + uwes5 + uwes6 + uwes7 + uwes8 + uwes9

opport =~ opp37 + opp39 + opp312
motiv =~ mot26 + mot28 + mot212
transfer =~ use1 + use3 + use5 + use7


# paths
transfer ~ opport + motiv + eng + jres + jdem

opport ~ eng + jres + jdem

motiv ~ eng + jres + jdem

eng ~ jres + jdem


# correlations
jres ~~ jdem
opport ~~ motiv
'


fit_transfer <- sem(transfer_model, data = work_data2, estimator = 'MLR')
summary(fit_transfer, fit.measures = TRUE, standardized = TRUE, rsquare=T)
fit_transfer_t1 <- round(fitMeasures(fit_transfer)[c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                                      "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                                      "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                                      "srmr", "aic", "bic")], 3)

lavInspect(fit_transfer, "rsquare")
# transfer r2 = .684
# eng r2 = .294
# motiv r2 = .103
# opport r2 = .137

beta_values <- standardizedSolution(fit_transfer)
beta_values[30:44,]

## ----------------- Table 2. Goodness-of-fit statistics for the estimated measurement and predictive models  --------------------


# preparation for creating table for Goodness-of-fit statistics for the estimated models

# fit_transfer_t1
fit_transfer_t1 <- as.data.frame(fit_transfer_t1)
# setting row names to first column
fit_transfer_t1 <- tibble::rownames_to_column(fit_transfer_t1, " ")
names(fit_transfer_t1) <- NULL

fit_transfer_t1b <- t(fit_transfer_t1)
fit_transfer_t1b <- as.data.frame(fit_transfer_t1b)


header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

fittable1 <- header.true(fit_transfer_t1b)


# solution was found here: https://stackoverflow.com/questions/26391921/how-to-convert-entire-dataframe-to-numeric-while-preserving-decimals
fittable1[] <- lapply(fittable1, function(x) {
  if(is.character(x)) as.numeric(as.character(x)) else x
})
sapply(fittable1, class)

# removing leading zeros in numbers
# solution was found here: https://stackoverflow.com/questions/53740145/remove-leading-zeros-in-numbers-within-a-data-frame
fittable1_2 <- data.frame(lapply(fittable1, function(x) gsub("^0\\.", "\\.", gsub("^-0\\.", "-\\.", sprintf("%.3f", x)))), stringsAsFactors = FALSE)

fittable1_2 <- fittable1_2 %>% 
  unite(RMSEA_CI, c(rmsea.ci.lower.scaled, rmsea.ci.upper.scaled), sep = " - ", remove = TRUE)

Model <- c("Proposed model")
models <- as.data.frame(Model)

combined_fit_table <- cbind(models, fittable1_2)

combined_fit_table <- combined_fit_table %>%  
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



# designing final goodness-of-fit statistics table
designed_table <- combined_fit_table %>% 
  flextable() %>% 
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "left", part = "all") %>% 
  autofit() %>% 
  add_footer_lines(c("Note. N = 311, χ2 chi-square test of exact fit, df degrees of freedom, CFI Comparative fit index, TLI Tucker–Lewis index, RMSEA root mean square error of approximation, 90% CI 90% confidence interval of the RMSEA, SRMR standardized root square residual", 
                     "* p < .05, ** p < .01, *** p < .001"))

# saving final table to word file
save_as_docx("Table 2. Goodness-of-fit statistics for the predictive model" = designed_table, path = "Table 2 - Goodness-of-fit statistics - predictive model table.docx")




## -------------------------------------------------- Table 3. Mediation analysis ------------------------------------------------

# ALL path model ----------------------------------------------------------

## all mediation models combined between job resources and transfer

# job resources -> motivation to transfer -> transfer
# job resources -> work engagement -> opportunity -> transfer
# job demands -> work engagement -> opportunity -> transfer 


modXall <- '
jres =~  jdr1 + jdr3 + jdr5 + jdr7 + jdr11
jdem =~ jdr2 + jdr4 + jdr6 + jdr8 + jdr10
eng =~ uwes1 + uwes2 + uwes3 + uwes4 + uwes5 + uwes6 + uwes7 + uwes8 + uwes9
motiv =~ mot26 + mot28 + mot212
opport =~ opp37 + opp39 + opp312
transfer =~ use1 + use3 + use5 + use7


 # a1 path
 eng ~ a1 * jres

 # b1 path
 transfer ~ b1 * eng
 
 # a2 path
 opport ~ a2 * jres

 # b2 path
 transfer ~ b2 * opport
 
 # d1 path
 opport ~ d1 * eng

 # a3 path
 motiv ~ a3 * jres

 # b3 path
 transfer ~ b3 * motiv

 # d2 path
 motiv ~ d2 * eng

  # c prime path 
 transfer ~ cp * jres



 # indirect and total effects
 a1b1 := a1 * b1          #res -> we -> transfer
 a2b2 := a2 * b2          #res -> opp -> transfer
 a3b3 := a3 * b3          #res -> mot -> transfer
 a1d1b2 := a1 * d1 * b2   #res -> we -> opp -> transfer
 a1d2b3 := a1 * d2 * b3   #res -> we -> mot -> transfer
 
 tot_ind := a1b1 + a2b2 + a3b3 + a1d1b2 + a1d2b3
 total := cp + a1b1 + a2b2 + a3b3 + a1d1b2 + a1d2b3


 # f1 path
 eng ~ f1 * jdem

 # f2 path
 opport ~ f2 * jdem

 # f3 path
 motiv ~ f3 * jdem


 # c prime path 
 transfer ~ cpd * jdem


 # indirect and total effects
 dem_f1b1 := f1 * b1          #dem -> we -> transfer
 dem_f2b2 := f2 * b2          #dem -> opp -> transfer
 dem_f3b3 := f3 * b3          #dem -> mot -> transfer
 dem_f1d1b2 := f1 * d1 * b2   #dem -> we -> opp -> transfer
 dem_f1d2b3 := f1 * d2 * b3   #dem -> we -> mot -> transfer

 dem_tot_ind := dem_f1b1 + dem_f2b2 + dem_f3b3 + dem_f1d1b2 + dem_f1d2b3
 dem_total := cpd + dem_f1b1 + dem_f2b2 + dem_f3b3 + dem_f1d1b2 + dem_f1d2b3

# correlations
jres ~~ jdem
opport ~~ motiv
'


fit_modXall <- sem(modXall, data = work_data2, se = "bootstrap", bootstrap = 5000)

med_estimates_fullXall <- parameterestimates(fit_modXall, boot.ci.type = "bca.simple", standardized = TRUE)

med_estimatesbx <- med_estimates_fullXall[c(30:42, 80:93),c("lhs", "op", "rhs", "label", "est", "pvalue", "ci.lower", "ci.upper", "std.all")]
med_estimates_nrbx <- med_estimates_fullXall[c(30:42, 80:93),c("est", "pvalue", "ci.lower", "ci.upper", "std.all")]
med_estimates_txtbx <- med_estimates_fullXall[c(30:42, 80:93),c("lhs", "op", "rhs", "label")]

# add significance stars from p values
med_estimates_nrbx$sign <- stars.pval(med_estimates_nrbx$pvalue)
med_estimates_signbx <- med_estimates_nrbx[1:27,"sign"]
med_estimates_nrbx <- med_estimates_nrbx[1:27,1:5]

# change class to numeric
# solution was found here: https://stackoverflow.com/questions/26391921/how-to-convert-entire-dataframe-to-numeric-while-preserving-decimals
med_estimates_nrbx[] <- lapply(med_estimates_nrbx, function(x) {
  if(is.character(x)) as.numeric(as.character(x)) else x
})
sapply(med_estimates_nrbx, class)

# removing leading zeros in numbers
# solution was found here: https://stackoverflow.com/questions/53740145/remove-leading-zeros-in-numbers-within-a-data-frame
med_estimates_nr2bx <- data.frame(lapply(med_estimates_nrbx, function(x) gsub("^0\\.", "\\.", gsub("^-0\\.", "-\\.", sprintf("%.3f", x)))), stringsAsFactors = FALSE)


# bind columns that contain text, modified numeric values and significance stars 
med_estimates_2bx <- cbind(med_estimates_txtbx, med_estimates_nr2bx, med_estimates_signbx)

med_estimates_2bx <- med_estimates_2bx %>%
  unite("95% CI", c(ci.lower, ci.upper), sep = ", ", remove = TRUE) %>%
  unite("standardized", c(std.all, med_estimates_signbx), sep = "", remove = TRUE)

med_estimates_2bx$`95% CI` <- med_estimates_2bx$`95% CI` %>% 
  paste("[", ., "]")



# Job resources paths -----------------------------------------------------


# total
variables0 <- "Job Resources -> Transfer"
variables <- as.data.frame(variables0) 

total_est <- med_estimates_2bx[20, 5]
total_e <- as.data.frame(total_est)

total <- med_estimates_2bx[20, c("standardized", "95% CI")]
total <- total %>% 
  rename("total_beta" = standardized,
         "total_ci" = `95% CI`)

total_beta <- total[,1]
total_confint <- total[,2]


direct_est <- med_estimates_2bx[9, 5]
direct_e <- as.data.frame(direct_est)

direct <- med_estimates_2bx[9, c("standardized", "95% CI")]
direct <- direct %>% 
  rename("direct_beta" = standardized,
         "direct_ci" = `95% CI`)

direct_beta <- direct[,1]
direct_confint <- direct[,2]


indirect_est <- med_estimates_2bx[19, 5]
indirect_e <- as.data.frame(indirect_est)

indirect <- med_estimates_2bx[19, c("standardized", "95% CI")]
indirect <- indirect %>% 
  rename("indirect_beta" = standardized,
         "indirect_ci" = `95% CI`)

indirect_beta <- indirect[,1]
indirect_confint <- indirect[,2]


mod_table_jrfull0 <- cbind(variables, total_e, total_confint, total_beta, 
                           direct_e, direct_confint, direct_beta,
                           indirect_e, indirect_confint,indirect_beta)



# job resources -> work engagement -> transfer path

# mediator: work engagement

variables0 <- "via WE"
variables <- as.data.frame(variables0) 

total_est <- ""
total_e <- as.data.frame(total_est)
total_beta <- ""
total_bet <- as.data.frame(total_beta) 
total_confint <- "" 
total_ci <- as.data.frame(total_confint) 

direct_est <- ""
direct_e <- as.data.frame(direct_est)
direct_beta <- ""
direct_bet <- as.data.frame(direct_beta) 
direct_confint <- "" 
direct_ci <- as.data.frame(direct_confint) 

indirect_est <- med_estimates_2bx[14, 5]
indirect_e <- as.data.frame(indirect_est)

indirect <- med_estimates_2bx[14, c("standardized", "95% CI")]
indirect <- indirect %>% 
  rename("indirect_beta" = standardized,
         "indirect_ci" = `95% CI`)

indirect_beta <- indirect[,1]
indirect_confint <- indirect[,2]

mod_table_jrfull1 <- cbind(variables, total_e, total_ci, total_bet, 
                           direct_e, direct_ci, direct_bet,
                           indirect_e, indirect_confint,indirect_beta)


# job resources -> opportunity -> transfer path

# mediator: opportunity

variables0 <- "via OTT"
variables <- as.data.frame(variables0) 

total_est <- ""
total_e <- as.data.frame(total_est)
total_beta <- ""
total_bet <- as.data.frame(total_beta) 
total_confint <- "" 
total_ci <- as.data.frame(total_confint) 

direct_est <- ""
direct_e <- as.data.frame(direct_est)
direct_beta <- ""
direct_bet <- as.data.frame(direct_beta) 
direct_confint <- "" 
direct_ci <- as.data.frame(direct_confint) 

indirect_est <- med_estimates_2bx[15, 5]
indirect_e <- as.data.frame(indirect_est)

indirect <- med_estimates_2bx[15, c("standardized", "95% CI")]
indirect <- indirect %>% 
  rename("indirect_beta" = standardized,
         "indirect_ci" = `95% CI`)

indirect_beta <- indirect[,1]
indirect_confint <- indirect[,2]

mod_table_jrfull2 <- cbind(variables, total_e, total_ci, total_bet, 
                           direct_e, direct_ci, direct_bet,
                           indirect_e, indirect_confint,indirect_beta)



# job resources -> motivation to transfer -> transfer path

# mediator: motivation to transfer

variables0 <- "via MTT"
variables <- as.data.frame(variables0) 

total_est <- ""
total_e <- as.data.frame(total_est)
total_beta <- ""
total_bet <- as.data.frame(total_beta) 
total_confint <- "" 
total_ci <- as.data.frame(total_confint) 

direct_est <- ""
direct_e <- as.data.frame(direct_est)
direct_beta <- ""
direct_bet <- as.data.frame(direct_beta) 
direct_confint <- "" 
direct_ci <- as.data.frame(direct_confint) 

indirect_est <- med_estimates_2bx[16, 5]
indirect_e <- as.data.frame(indirect_est)

indirect <- med_estimates_2bx[16, c("standardized", "95% CI")]
indirect <- indirect %>% 
  rename("indirect_beta" = standardized,
         "indirect_ci" = `95% CI`)

indirect_beta <- indirect[,1]
indirect_confint <- indirect[,2]

mod_table_jrfull3 <- cbind(variables, total_e, total_ci, total_bet, 
                           direct_e, direct_ci, direct_bet,
                           indirect_e, indirect_confint,indirect_beta)




# job resources -> work engagement -> opportunity -> transfer path

# mediator: both Work Engagement and Opportunity to Transfer

variables0 <- "via WE -> OTT"
variables <- as.data.frame(variables0) 

total_est <- ""
total_e <- as.data.frame(total_est)
total_beta <- ""
total_bet <- as.data.frame(total_beta) 
total_confint <- "" 
total_ci <- as.data.frame(total_confint) 

direct_est <- ""
direct_e <- as.data.frame(direct_est)
direct_beta <- ""
direct_bet <- as.data.frame(direct_beta) 
direct_confint <- "" 
direct_ci <- as.data.frame(direct_confint) 

indirect_est <- med_estimates_2bx[17, 5]
indirect_e <- as.data.frame(indirect_est)

indirect <- med_estimates_2bx[17, c("standardized", "95% CI")]
indirect <- indirect %>% 
  rename("indirect_beta" = standardized,
         "indirect_ci" = `95% CI`)


indirect_beta <- indirect[,1]
indirect_confint <- indirect[,2]

mod_table_jrfull4 <- cbind(variables, total_e, total_ci, total_bet, 
                    direct_e, direct_ci, direct_bet,
                    indirect_e, indirect_confint,indirect_beta)



# job resources -> work engagement -> motivation -> transfer path

# mediator: both Work Engagement and Motivation to Transfer

variables0 <- "via WE -> MTT"
variables <- as.data.frame(variables0) 

total_est <- ""
total_e <- as.data.frame(total_est)
total_beta <- ""
total_bet <- as.data.frame(total_beta) 
total_confint <- "" 
total_ci <- as.data.frame(total_confint) 

direct_est <- ""
direct_e <- as.data.frame(direct_est)
direct_beta <- ""
direct_bet <- as.data.frame(direct_beta) 
direct_confint <- "" 
direct_ci <- as.data.frame(direct_confint) 

indirect_est <- med_estimates_2bx[18, 5]
indirect_e <- as.data.frame(indirect_est)

indirect <- med_estimates_2bx[18, c("standardized", "95% CI")]
indirect <- indirect %>% 
  rename("indirect_beta" = standardized,
         "indirect_ci" = `95% CI`)


indirect_beta <- indirect[,1]
indirect_confint <- indirect[,2]

mod_table_jrfull5 <- cbind(variables, total_e, total_ci, total_bet, 
                           direct_e, direct_ci, direct_bet,
                           indirect_e, indirect_confint,indirect_beta)



mod_table_jrfull <- rbind(mod_table_jrfull0, mod_table_jrfull1, mod_table_jrfull2, mod_table_jrfull3, 
                          mod_table_jrfull4, mod_table_jrfull5)




# Job demands paths -------------------------------------------------------

# total
variables0 <- "Job Demands -> Transfer"
variables <- as.data.frame(variables0) 

total_est <- med_estimates_2bx[27, 5]
total_e <- as.data.frame(total_est)

total <- med_estimates_2bx[27, c("standardized", "95% CI")]
total <- total %>% 
  rename("total_beta" = standardized,
         "total_ci" = `95% CI`)
total_beta <- total[,1]
total_confint <- total[,2]


direct_est <- med_estimates_2bx[13, 5]
direct_e <- as.data.frame(direct_est)

direct <- med_estimates_2bx[13, c("standardized", "95% CI")]
direct <- direct %>% 
  rename("direct_beta" = standardized,
         "direct_ci" = `95% CI`)
direct_beta <- direct[,1]
direct_confint <- direct[,2]


indirect_est <- med_estimates_2bx[26, 5]
indirect_e <- as.data.frame(indirect_est)

indirect <- med_estimates_2bx[26, c("standardized", "95% CI")]
indirect <- indirect %>% 
  rename("indirect_beta" = standardized,
         "indirect_ci" = `95% CI`)
indirect_beta <- indirect[,1]
indirect_confint <- indirect[,2]

mod_table_jdfull0 <- cbind(variables, total_e, total_confint, total_beta, 
                    direct_e, direct_confint, direct_beta,
                    indirect_e, indirect_confint,indirect_beta)


# job demands -> work engagement -> transfer path

# mediator: work engagement

variables0 <- "via WE"
variables <- as.data.frame(variables0) 

total_est <- ""
total_e <- as.data.frame(total_est)
total_beta <- ""
total_bet <- as.data.frame(total_beta) 
total_confint <- "" 
total_ci <- as.data.frame(total_confint) 

direct_est <- ""
direct_e <- as.data.frame(direct_est)
direct_beta <- ""
direct_bet <- as.data.frame(direct_beta) 
direct_confint <- "" 
direct_ci <- as.data.frame(direct_confint) 

indirect_est <- med_estimates_2bx[21, 5]
indirect_e <- as.data.frame(indirect_est)

indirect <- med_estimates_2bx[21, c("standardized", "95% CI")]
indirect <- indirect %>% 
  rename("indirect_beta" = standardized,
         "indirect_ci" = `95% CI`)

indirect_beta <- indirect[,1]
indirect_confint <- indirect[,2]

mod_table_jdfull1 <- cbind(variables, total_e, total_ci, total_bet, 
                    direct_e, direct_ci, direct_bet,
                    indirect_e, indirect_confint,indirect_beta)




# job demands -> opportunity -> transfer path

# mediator: opportunity

variables0 <- "via OTT"
variables <- as.data.frame(variables0) 

total_est <- ""
total_e <- as.data.frame(total_est)
total_beta <- ""
total_bet <- as.data.frame(total_beta) 
total_confint <- "" 
total_ci <- as.data.frame(total_confint) 

direct_est <- ""
direct_e <- as.data.frame(direct_est)
direct_beta <- ""
direct_bet <- as.data.frame(direct_beta) 
direct_confint <- "" 
direct_ci <- as.data.frame(direct_confint) 

indirect_est <- med_estimates_2bx[22, 5]
indirect_e <- as.data.frame(indirect_est)

indirect <- med_estimates_2bx[22, c("standardized", "95% CI")]
indirect <- indirect %>% 
  rename("indirect_beta" = standardized,
         "indirect_ci" = `95% CI`)

indirect_beta <- indirect[,1]
indirect_confint <- indirect[,2]

mod_table_jdfull2 <- cbind(variables, total_e, total_ci, total_bet, 
                    direct_e, direct_ci, direct_bet,
                    indirect_e, indirect_confint,indirect_beta)



# job demands -> motivation -> transfer path

# mediator: motivation

variables0 <- "via MTT"
variables <- as.data.frame(variables0) 

total_est <- ""
total_e <- as.data.frame(total_est)
total_beta <- ""
total_bet <- as.data.frame(total_beta) 
total_confint <- "" 
total_ci <- as.data.frame(total_confint) 

direct_est <- ""
direct_e <- as.data.frame(direct_est)
direct_beta <- ""
direct_bet <- as.data.frame(direct_beta) 
direct_confint <- "" 
direct_ci <- as.data.frame(direct_confint) 

indirect_est <- med_estimates_2bx[23, 5]
indirect_e <- as.data.frame(indirect_est)

indirect <- med_estimates_2bx[23, c("standardized", "95% CI")]
indirect <- indirect %>% 
  rename("indirect_beta" = standardized,
         "indirect_ci" = `95% CI`)

indirect_beta <- indirect[,1]
indirect_confint <- indirect[,2]

mod_table_jdfull3 <- cbind(variables, total_e, total_ci, total_bet, 
                           direct_e, direct_ci, direct_bet,
                           indirect_e, indirect_confint,indirect_beta)




# job demands -> work engagement -> opportunity -> transfer path

# mediator: both Work Engagement and Opportunity to Transfer

variables0 <- "via WE -> OTT"
variables <- as.data.frame(variables0) 

total_est <- ""
total_e <- as.data.frame(total_est)
total_beta <- ""
total_bet <- as.data.frame(total_beta) 
total_confint <- "" 
total_ci <- as.data.frame(total_confint) 

direct_est <- ""
direct_e <- as.data.frame(direct_est)
direct_beta <- ""
direct_bet <- as.data.frame(direct_beta) 
direct_confint <- "" 
direct_ci <- as.data.frame(direct_confint) 

indirect_est <- med_estimates_2bx[24, 5]
indirect_e <- as.data.frame(indirect_est)

indirect <- med_estimates_2bx[24, c("standardized", "95% CI")]
indirect <- indirect %>% 
  rename("indirect_beta" = standardized,
         "indirect_ci" = `95% CI`)
indirect_beta <- indirect[,1]
indirect_confint <- indirect[,2]

mod_table_jdfull4 <- cbind(variables, total_e, total_ci, total_bet, 
                    direct_e, direct_ci, direct_bet,
                    indirect_e, indirect_confint,indirect_beta)



# job demands -> work engagement -> motivation -> transfer path

# mediator: both Work Engagement and Motivation to Transfer

variables0 <- "via WE -> MTT"
variables <- as.data.frame(variables0) 

total_est <- ""
total_e <- as.data.frame(total_est)
total_beta <- ""
total_bet <- as.data.frame(total_beta) 
total_confint <- "" 
total_ci <- as.data.frame(total_confint) 

direct_est <- ""
direct_e <- as.data.frame(direct_est)
direct_beta <- ""
direct_bet <- as.data.frame(direct_beta) 
direct_confint <- "" 
direct_ci <- as.data.frame(direct_confint) 

indirect_est <- med_estimates_2bx[25, 5]
indirect_e <- as.data.frame(indirect_est)

indirect <- med_estimates_2bx[25, c("standardized", "95% CI")]
indirect <- indirect %>% 
  rename("indirect_beta" = standardized,
         "indirect_ci" = `95% CI`)
indirect_beta <- indirect[,1]
indirect_confint <- indirect[,2]

mod_table_jdfull5 <- cbind(variables, total_e, total_ci, total_bet, 
                           direct_e, direct_ci, direct_bet,
                           indirect_e, indirect_confint,indirect_beta)




mod_table_jdfull <- rbind(mod_table_jdfull0, mod_table_jdfull1, mod_table_jdfull2, mod_table_jdfull3, 
                          mod_table_jdfull4, mod_table_jdfull5)




# adding each mediation models to one table -------------------------------

mod_tables <- rbind(mod_table_jrfull, mod_table_jdfull)

# designing final mediation table
designed_table_med <- mod_tables %>%
  flextable() %>%
  set_header_labels(
    variables0 = "Paths",
    total_est = "Total effect",
    total_confint = "",
    total_beta = "",
    direct_est = "Direct effect",
    direct_confint = "",
    direct_beta = "",
    indirect_est = "Indirect effect",
    indirect_confint = "",
    indirect_beta = ""
  ) %>%
  add_header_row(values = c("", "b", "95% CI", "β", "b", "95% CI", "β", "b", "95% CI", "β"),
                 top = FALSE) %>%
  merge_at(i = 1, j = 2:4, part = "head") %>%
  merge_at(i = 1, j = 5:7, part = "head") %>%
  merge_at(i = 1, j = 8:10, part = "head") %>%
  hline(i = 2, part = "head", border = officer::fp_border(width = 2)) %>%
  hline(i = 1, part = "head", border = officer::fp_border("white")) %>%
  hline(i = 6, part = "body", border = officer::fp_border()) %>%
  fontsize(size = 11, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "left", part = "all") %>%
  # width(width = 0.86) %>%
  autofit() %>%
  add_footer_lines(
    c(
      "Note. Bootstrapped confidence intervals were based on 10,000 replications and were estimated with maximum likelihood estimation method given that bootstrapping is not available for the MLR estimator",
      "WE = Work Engagement, MTT = Motivation to Transfer, OTT = Opportunity to Transfer. β standardized regression weights, 95% CI bias-corrected bootstrapped confidence intervals",
      "* p < .05, ** p < .01"
    )
  )

# saving final table to word file
save_as_docx("Table 3. Standardized estimates of total, direct, and indirect effects with 95% bias-corrected bootstrapped confidence intervals" = designed_table_med, 
             path = "Table 3 extra modified 7 - Mediation analysis table.docx")





## ------------------- Table S1. Standardized Parameter Estimates from the Predictive model (modified) ---------------------------


lambda <- inspect(fit_transfer,what="std")$lambda # lambda (standardized factor loadings) - λ = Factor loading
#inspect(fit_transfer,what="std")$theta # theta (observed error covariance matrix) - δ = Item uniqueness 
estimates <- standardizedSolution(fit_transfer) # it shows what we need, both lamda and theta (est.std column)

#estimates2 <- estimates[c(1:29,46:74),c("lhs", "op", "rhs", "est.std")]
#lambda2 <- estimates[1:29,c("lhs", "op", "rhs", "est.std")]
unique <- estimates[45:73,c("lhs", "est.std")]

estimates_t <- cbind(lambda, unique)
# after checking each rows contain lambda and theta in the appropriate line, we can remove unnecessary column
estimates_t2 <- subset(estimates_t, select = -c(lhs))
estimates_t2 <- round(estimates_t2, 3)
##
estimates_t3 <- data.frame(lapply(estimates_t2, function(x) gsub("^0\\.", "\\.", gsub("^-0\\.", "-\\.", sprintf("%.3f", x)))), stringsAsFactors = FALSE)
estimates_t3[estimates_t3 == ".000"] <- ""

item_nr <- c(
  "Item 1", "Item 2", "Item 3", "Item 4", "Item 5",
  "Item 1", "Item 2", "Item 3", "Item 4", "Item 5",
  "Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Item 7", "Item 8", "Item 9",
  "Item 1", "Item 2", "Item 3",
  "Item 1", "Item 2", "Item 3",
  "Item 1", "Item 2", "Item 3", "Item 4")

items <- as.data.frame(item_nr)

itemname <- c("a", "a", "a", "a", "a",
              "b", "b", "b", "b", "b",
              "c", "c", "c", "c", "c", "c", "c", "c", "c",
              "d", "d", "d",
              "e", "e", "e",
              "f", "f", "f", "f")

itemnames <- as.data.frame(itemname)

estimate_table <- cbind(itemnames, items, estimates_t3)

estimate_table_new <- as.data.frame(lapply(estimate_table, as.character), stringsAsFactors = FALSE)
estimate_table2 <- head(do.call(rbind, by(estimate_table_new, estimate_table_new$itemname, rbind, "")), -1 )

estimate_table2 <- rbind("", estimate_table2)
estimate_table2 <- subset(estimate_table2, select = -c(itemname))

estimate_table3 <- estimate_table2 %>%  
  rename(
    " " = item_nr,
    "WE (A)" = eng,
    "JR (A)" = jres,
    "JD (A)" = jdem,
    "MTT (A)" = motiv,
    "OTT (A)" = opport,
    "TT (A)" = transfer,
    "Theta" = est.std)


# designing final standardized parameter estimates table
designed_table_est <- estimate_table3 %>% 
  flextable() %>% 
  #  hline(i = 6, part = "body", border = officer::fp_border()) %>% 
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "left", part = "all") %>% 
  autofit() %>% 
  add_footer_lines(c("Note. N = 311; λ = Factor loading; δ = Item uniqueness; ω = model-based omega composite reliability.", 
                     "* p < .05, ** p < .01, *** p < .001"))

# saving final table to word file
save_as_docx("Table S1. Parameter Estimates" = designed_table_est, path = " Table S1 - Parameter Estimates table.docx")



## ------------------------------- Figure 1 (will be changed probably to DiagrammeR / TidySEM) -----------------------------------


pathdiagram1 <- semPaths(fit_transfer, 
                         layout = "tree2", centerLevels = TRUE,
                         whatLabels = "std", edge.label.cex = 1,
                         style="lisrel")


# pathdiagram1 <- semPaths(fit_transfer, 
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




