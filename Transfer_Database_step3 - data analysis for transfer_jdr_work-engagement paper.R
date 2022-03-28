
# loading packages
library(haven)
library(tidyverse)
library(lavaan) # for calculating Cronbach alpha
#library(semTools)  # for calculating Cronbach alpha
library(psych) # for EFA
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

work_data2$cent_timediff <- work_data2$timediff %>% 
  scale(center = TRUE) %>% 
  as.numeric()


## ---------------------------------------------------- Demographics -----------------------------------------------------

work_data2 %>% 
  summarise(min(timediff),
            max(timediff),
            mean(timediff),
            sd(timediff))

work_data2 %>% 
  summarise(min(age),
            max(age),
            mean(age),
            sd(age))

work_data2 %>% 
  group_by(Gender) %>%
  summarise(N = n(),
            Percent = n()/311*100)

work_data2 %>% 
  group_by(Management) %>%
  summarise(N = n(),
            Percent = n()/311*100)


## ---------------------------------------------------- 1. Preliminary analyses -----------------------------------------------------

## -------------------------- 1.1. Internal consistency (alpha and omega) calculation ---------------------------


# 1.1.1. Calculating  Cronbach alpha --------------------------------------

# Calculating Cronbach alpha for Job resources

# resources_factor_original <- '
# res_factor_orig =~ jdr1 + jdr3 + jdr5 + jdr7 + jdr9 + jdr11'
# fit_resources_orig <- cfa(resources_factor_original, data = work_data2, estimator = 'MLR')
# summary(fit_resources_orig, fit.measures = TRUE, standardized = TRUE, rsquare=T, ci = TRUE)
# # factor loading of jdr9 did not reach 0.3 so it will be removed from the further phases of the analysis


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
reliabilities2 <- round(reliabilities[1,], 3)


# 1.1.2. Calculating composite reliability, McDonald's omega ---------------------

# calculating composite reliability, McDonald's omega

# transfer model 1 reliability check
transfer_corr_model_rel <- '
# regressions
jres =~ j1*jdr1 + j3*jdr3 + j5*jdr5 + j7*jdr7 + j11*jdr11

# Error Variance
jdr1~~ej1*jdr1
jdr3~~ej3*jdr3
jdr5~~ej5*jdr5
jdr7~~ej7*jdr7
jdr11~~ej11*jdr11

#Reliability
omega.jr := 
((j1 + j3 + j5 + j7 + j11)^2) 
/ 
((j1 + j3 + j5 + j7 + j11)^2 + 
(ej1+ej3+ej5+ej7+ej11))


jdem =~ j2*jdr2 + j4*jdr4 + j6*jdr6 + j8*jdr8 + j10*jdr10

# Error Variance
jdr2~~ej2*jdr2
jdr4~~ej4*jdr4
jdr6~~ej6*jdr6
jdr8~~ej8*jdr8
jdr10~~ej10*jdr10

#Reliability
omega.jd := 
((j2 + j4 + j6 + j8 + j10)^2) 
/ 
((j2 + j4 + j6 + j8 + j10)^2 + 
(ej2+ej4+ej6+ej8+ej10))


eng =~ u1*uwes1 + u2*uwes2 + u3*uwes3 + u4*uwes4 + u5*uwes5 + u6*uwes6 + u7*uwes7 + u8*uwes8 + u9*uwes9

# Error Variance
uwes1~~eu1*uwes1
uwes2~~eu2*uwes2
uwes3~~eu3*uwes3
uwes4~~eu4*uwes4
uwes5~~eu5*uwes5
uwes6~~eu6*uwes6
uwes7~~eu7*uwes7
uwes8~~eu8*uwes8
uwes9~~eu9*uwes9

#Reliability
omega.u := 
((u1 + u2 + u3 + u4 + u5 + u6 + u7 + u8 + u9)^2) 
/ 
((u1 + u2 + u3 + u4 + u5 + u6 + u7 + u8 + u9)^2 + 
(eu1 + eu2 + eu3 + eu4 + eu5 + eu6 + eu7 + eu8 + eu9))


opport =~ o1*opp37 + o2*opp39 + o3*opp312

# Error Variance
opp37~~eo1*opp37
opp39~~eo2*opp39
opp312~~eo3*opp312

#Reliability
omega.o := 
((o1 + o2 + o3)^2) 
/ 
((o1 + o2 + o3)^2 + 
(eo1 + eo2 + eo3))


motiv =~ m1*mot26 + m2*mot28 + m3*mot212

# Error Variance
mot26~~em1*mot26
mot28~~em2*mot28
mot212~~em3*mot212

#Reliability
omega.m := 
((m1 + m2 + m3)^2) 
/ 
((m1 + m2 + m3)^2 + 
(em1 + em2 + em3))


transfer =~ t1*use1 + t2*use3 + t3*use5 + t4*use7

# Error Variance
use1~~et1*use1
use3~~et2*use3
use5~~et3*use5
use7~~et4*use7


#Reliability
omega.t := 
((t1 + t2 + t3 + t4)^2) 
/ 
((t1 + t2 + t3 + t4)^2 + 
(et1 + et2 + et3 + et4))

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

cent_timediff ~~ jres
cent_timediff ~~ jdem
cent_timediff ~~ eng
cent_timediff ~~ opport
cent_timediff ~~ motiv
cent_timediff ~~ transfer

'

fit_transfer <- sem(transfer_corr_model_rel, data = work_data2, estimator = 'MLR', std.lv = TRUE)
summary(fit_transfer, fit.measures = TRUE, standardized = TRUE, rsquare=T)

# Fit indices
round(fitMeasures(fit_transfer)[c("chisq.scaled", "df", "cfi.scaled", "tli.scaled",
                                  "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "aic", "bic")], 3)



# 1.1.3. Assessing the extent to which Common Method Variance may be a problem -------------

# 1.1.3.1. Harman's single factor model (EFA)
one_fact <- work_data2 %>%
  as_tibble() %>%
  select(jdr1, jdr3, jdr5, jdr7, jdr11, 
           jdr2, jdr4, jdr6, jdr8, jdr10,
           uwes1, uwes2, uwes3, uwes4, uwes5, uwes6, uwes7, uwes8, uwes9,
           opp37, opp39, opp312,
           mot26, mot28, mot212, 
           use1, use3, use5, use7)

onef_correl <- mixedCor(one_fact, c=NULL, p=1:29)
str(onef_correl)
onef_polychoric <- onef_correl$rho

library(polycor)
cortest.bartlett(onef_polychoric, n = 311)
KMO(onef_polychoric)
# Both the p.value attribute of cortest.bartlett()’s output is very much lower than 0.05 and 
# the MSA attribute of KMO()’s output, 0.91, is close to 1, 
# which means that they both recommend that EFA.

# Scree test and the Kaiser-Guttman criterion
scree(onef_polychoric)

# Parallel analysis for estimation with the minres extraction method
fa.parallel(onef_polychoric, n.obs = 311, fm = "minres", fa = "fa")
## -> Parallel analysis suggests that the number of factors =  4

# Parallel analysis for estimation with the mle extraction method
fa.parallel(onef_polychoric, n.obs = 311, fm = "mle", fa = "fa")
## -> Parallel analysis suggests that the number of factors =  5

fa(onef_polychoric, nfactors = 1, fm = "mle")
# Proportion Var: 0.28 (the overall variance the factor accounts for out of all the variables)


# 1.1.3.2. Harman's single factor model (CFA)
Harman_model <- '
# regressions
single =~ jdr1 + jdr3 + jdr5 + jdr7 + jdr11 + 
  jdr2 + jdr4 + jdr6 + jdr8 + jdr10 +
  uwes1 + uwes2 + uwes3 + uwes4 + uwes5 + uwes6 + uwes7 + uwes8 + uwes9 +
  opp37 + opp39 + opp312 +
  mot26 + mot28 + mot212 + 
  use1 + use3 + use5 + use7
'

fit_single <- sem(Harman_model, data = work_data2, estimator = 'MLR', std.lv = TRUE)
summary(fit_single, fit.measures = TRUE, standardized = TRUE, rsquare=T)

# Fit indices
round(fitMeasures(fit_single)[c("chisq.scaled", "df", "cfi.scaled", "tli.scaled",
                                  "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "aic", "bic")], 3)


## ---------------------------------------- 1.2. CORRELATION of MEASURED variables ------------------------------------


# 1.2.1. preparation for correlation table --------------------------------

# check variables normality
shapiro.test(work_data2$timediff)
shapiro.test(work_data2$job_resources)
shapiro.test(work_data2$job_demands)
shapiro.test(work_data2$uwes_all)
shapiro.test(work_data2$motivation)
shapiro.test(work_data2$opportunity)
shapiro.test(work_data2$use)

# significant Shapiro-Wilk normality tests --> Spearman correlations are necessary

corr_input <- c("timediff", "job_resources", "job_demands", "uwes_all", "opportunity", "motivation", "use")

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

# 1.3.1. correlation model - with time lag --------------------------------

# transfer corr model 2 
transfer_corr_model2 <- '
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

cent_timediff ~~ jres
cent_timediff ~~ jdem
cent_timediff ~~ eng
cent_timediff ~~ opport
cent_timediff ~~ motiv
cent_timediff ~~ transfer

'


fit_transf_corr2 <- sem(transfer_corr_model2, data = work_data2, estimator = 'MLR')
sem_corr2 <- summary(fit_transf_corr2, fit.measures = TRUE, standardized = TRUE, rsquare=T, ci = TRUE)
fit_corr2 <- round(fitMeasures(fit_transf_corr2)[c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                                 "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                                 "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                                 "srmr", "aic", "bic")], 3)


# # 1.3.2. creating dataframe for Table 1. (corr between latent vars + time lag) --------


corr_estimates <- sem_corr2$PE[30:50,c("lhs", "op", "rhs", "pvalue", "ci.lower", "ci.upper", "std.all")]
corr_estimates_nr <- sem_corr2$PE[30:50,c("pvalue", "ci.lower", "ci.upper", "std.all")]
med_estimates_txt <- sem_corr2$PE[30:50,c("lhs", "op", "rhs")]


# add significance stars from p values
corr_estimates_nr$sign <- stars.pval(corr_estimates_nr$pvalue)
corr_estimates_sign_nr <- corr_estimates_nr[1:21,"sign"]
corr_estimates_nr <- corr_estimates_nr[1:21,1:4]

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

corr_estimates3 <- corr_estimates2[1:21,c(1,3,6)]

# library(reshape2) used here for creating matrix from data frame columns
latent_corr <- acast(corr_estimates3, lhs~rhs, value.var="standardized")

latent_corr[4, 2] <- latent_corr[1, 4]
latent_corr[5, 2] <- latent_corr[1, 5]
latent_corr[6, 2] <- latent_corr[1, 6]

latent_corr[1, 3] <- latent_corr[2, 2]
latent_corr[4, 3] <- latent_corr[2, 4]
latent_corr[5, 3] <- latent_corr[2, 5]
latent_corr[6, 3] <- latent_corr[2, 6]

latent_corr[6, 4] <- latent_corr[4, 6]
latent_corr[4, 5] <- latent_corr[5, 4]
latent_corr[6, 5] <- latent_corr[5, 6]


latent_corr1 <- latent_corr[1:6, 1:3]
latent_corr2 <- latent_corr[1:6, 4:6]


tdiff <- as.matrix(t(latent_corr[,1]))
tdiff_col <- as.matrix(NA)
colnames(tdiff_col) <- c("cent_timediff")
rownames(tdiff_col) <- c("cent_timediff")
tdiff <- cbind(tdiff_col, tdiff)


jres <- as.matrix(latent_corr[3,])
colnames(jres) <- c("jres")

jres1 <- as.matrix(jres[1:3, 1])
jres2 <- as.matrix(jres[4:6, 1])

jres_row <- as.matrix(NA)
rownames(jres_row) <- c("jres")
jres_new <- rbind(jres1, jres_row, jres2)
colnames(jres_new) <- c("jres")

jres_new <- jres_new[2:7,]

latent_corr_tab <- cbind(latent_corr1, jres_new, latent_corr2)

latent_corr_tab2 <- rbind(tdiff, latent_corr_tab)




col.order <- c("cent_timediff", "jres","jdem","eng","motiv","opport", "transfer")
row.order <- c("cent_timediff", "jres","jdem","eng","motiv","opport", "transfer")
latent_corr_tab3 <- latent_corr_tab2[row.order,col.order]
latent_corr_tab4 <- as.data.frame(latent_corr_tab3)

latent_corr_tab4[upper.tri(latent_corr_tab4)] <- NA




row.names(latent_corr_tab4) <- c("1. Time Lag", "2. Job Resources", "3. Job Demands", "4. Work Engagement", 
                                 "5. Motivation to Transfer", "6. Opportunity to Transfer",
                                 "7. Training Transfer")

# setting row names to first column
latent_corr_tab4 <- tibble::rownames_to_column(latent_corr_tab4, " ")

# removing last (empty) column
latent_corr_tab4[,8] <- NULL

# renaming column names
latent_corr_tab4 <- latent_corr_tab4 %>%  
  rename(
    "1" = cent_timediff,
    "2" = jres,
    "3" = jdem,
    "4" = eng,
    "5" = motiv,
    "6" = opport)


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


# predictive model with control variable --------------------------------------------------------

# transfer model 
transfer_model2 <- '
# regressions
jres =~  jdr1 + jdr3 + jdr5 + jdr7 + jdr11 
jdem =~ jdr2 + jdr4 + jdr6 + jdr8 + jdr10

eng  =~ uwes1 + uwes2 + uwes3 + uwes4 + uwes5 + uwes6 + uwes7 + uwes8 + uwes9

opport =~ opp37 + opp39 + opp312
motiv =~ mot26 + mot28 + mot212
transfer =~ use1 + use3 + use5 + use7


# paths
transfer ~ opport + motiv + eng + jres + jdem + cent_timediff

opport ~ eng + jres + jdem + cent_timediff

motiv ~ eng + jres + jdem + cent_timediff

eng ~ jres + jdem + cent_timediff


# correlations
jres ~~ jdem
opport ~~ motiv
'


fit_transfer2 <- sem(transfer_model2, data = work_data2, estimator = 'MLR')
summary(fit_transfer2, fit.measures = TRUE, standardized = TRUE, rsquare=T)
fit_transfer_t1_2 <- round(fitMeasures(fit_transfer2)[c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                                     "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                                     "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                                     "srmr", "aic", "bic")], 3)

fit_configural <- sem(transfer_model2, data = work_data2, estimator = 'MLR', group = "Company")
summary(fit_configural, fit.measures = TRUE, standardized = TRUE, rsquare=T)
fit_configural_t1_2 <- round(fitMeasures(transfer_model2)[c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                                        "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                                        "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                                        "srmr", "aic", "bic")], 3)


lavInspect(fit_transfer2, "rsquare")
# transfer r2 = .684
# eng r2 = .294
# motiv r2 = .103
# opport r2 = .137

beta_values2 <- standardizedSolution(fit_transfer2)
beta_values2[30:44,]

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




