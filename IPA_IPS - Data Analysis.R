library(stargazer)
library(dplyr)
library(knitr)
library(tidyr)
library(VGAM)
library(survival)
library(survminer)
library(Publish)
library(tableHTML)
library(arsenal)
library(MatchIt)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.csv("Cleaned Data.csv")
df$NIU <- as.character(df$NIU)

head(df)
names(df)

sapply(df, class)
lenunique <- function(x){
  list(length(unique(x)))
}
sapply(df, lenunique)


##### Descriptive Statistics #####

desc_stat0 <- tableby(Major ~ Gender + kwt(Age_startprog,"median") + Program + D_tested_admission +
                        kwt(Family_Income,"median"), data = df)
summary(desc_stat0, title = "Descriptive Statistics, part 1")
write2word(desc_stat0, "desc_stat0.doc")

desc_stat1 <- tableby(HS_program ~ D_Graduated + Major + Program + kwt(GPA_Agg,"median") + 
                        kwt(GPA_Quant_mean,"median") + kwt(GPA_Qual_mean,"median") + 
                        kwt(Study_Duration,"median"), data = df)
summary(desc_stat1, title = "Descriptive Statistics, part 2", text=TRUE)
write2word(desc_stat0, "desc_stat1.doc")

desc_stat2 <- tableby(HS_program ~ Gender + kwt(Age_startprog,"median") + Program +
                        D_tested_admission + kwt(Family_Income,"median") +
                        D_Graduated + Major + kwt(GPA_Agg,"median") + 
                        kwt(GPA_Quant_mean,"median") + kwt(GPA_Qual_mean,"median") + 
                        kwt(Study_Duration,"median"), data = df)
summary(desc_stat2, title = "Descriptive Statistics, part 3", text=TRUE)
write2word(desc_stat2, "desc_stat2.doc")



##### Exploratory Data Analysis #####

  # Distribution of Grades
  ggplot(df, aes(x=GPA_Agg)) + 
    geom_histogram(position="dodge", bins=100)+
    geom_density(alpha=.2, fill="#FF6666") 

  # Scatterplot of Grades
  ggplot(df, aes(x=GPA_Quant_mean, y=GPA_Agg)) +
    geom_point()
  
  # Boxplot of Grades 1
  ggplot(df, aes(x=Major, y=GPA_Quant_mean)) +
    geom_boxplot()
  
  # Boxplot of Grades 2
  ggplot(df, aes(x=D_tested_admission,y=GPA_Agg)) +
    geom_boxplot()

  # Boxplot of Grades 3
  ggplot(df, aes(x=Job_Father,y=GPA_Agg)) +
    geom_boxplot()
  
  # Cross Tabulation of Composite GPA, by HS Program (All students)
  df %>% 
    group_by(HS_program,Major) %>% 
    summarise(x = round(mean(GPA_Agg,na.rm=T),2)) %>% 
    spread(Major, x) %>% 
    kable()
  
  # Cross Tabulation of Quantitative GPA, by HS Program (All students)
  df %>% 
    group_by(HS_program,Major) %>% 
    summarise(x = round(mean(GPA_Quant_mean,na.rm=T),2)) %>% 
    spread(Major, x) %>% 
    kable()
  
  # Cross Tabulation of Qualitative GPA, by HS Program (All students)
  df %>% 
    group_by(HS_program,Major) %>% 
    summarise(x = round(mean(GPA_Qual_mean,na.rm=T),2)) %>% 
    spread(Major, x) %>% 
    kable()
  
  # Cross Tabulation of Composite GPA, by Batch (Graduated students only)
  df[df$D_Graduated==1,] %>% 
    group_by(Batch ,Major) %>% 
    summarise(x = round(mean(GPA_Qual_mean,na.rm=T),2)) %>% 
    spread(Major, x) %>% 
    kable()
  


  
##### Propensity Score Matching: Generating Matched Samples #####
  
  # Prepare the data
  #df_psm <- subset(df, HS_program=="IPA" | HS_program=="IPS")[!is.na(df$Family_Income),]
  df_psm <- df
  df_psm$HS_program <- ifelse(df_psm$HS_program=="IPA", 1, 0)
  df_psm$ln_Family_Income <- log(df_psm$Family_Income)
  
  # Easier organisation of LM formula, list of the matching variables (categorical and numeric)
  
  names(df_psm)
  #dump c()
  matchvar_categorical <- c("Gender" , "Major" , "Program" , "D_tested_admission",
                            "Job_Father" , "Job_Mother" , "HS_province")
  matchvar_numeric <- c("Age_startprog", "Batch" )
  matchvar <- c(matchvar_categorical,matchvar_numeric)
  matching_formula <- as.formula(paste("HS_program",
                              paste(matchvar, collapse = " + "), 
                              sep = " ~ "))
  
  # Generate the propensity score predictor regression (logit, binomial)
  ps_reg <- glm(matching_formula, 
                family = binomial(), data = df_psm)
  summary(ps_reg)
  tableHTML(summary(ps_reg)$coefficients)
  write2word(summary(ps_reg)$coefficients, "matching regression.doc")

  # Matching Algorithm
    # 1. Split the data first
    sapply(df_psm, function(x) sum(is.na(x))) # see which var has high missing values
    df_psm_matching <- df_psm %>% 
      select(one_of("NIU","HS_program",matchvar)) %>% 
      na.omit() # omit the NAs since PSM cannot handle missing values
    df_nonmatching <- df_psm %>%
      select(-one_of("HS_program",matchvar))
    nrow(df_psm_matching)
    
    # 2. Do the matching
    library(optmatch)
    mod_match <- matchit(matching_formula, 
                         method = "nearest", data = df_psm_matching,
                         distance= "logit", discard="both", standardize = TRUE)
    summary(mod_match)
    
    # 3. Remerge the data!
    df_postPSM <- match.data(mod_match)
    df_matched <- merge(df_postPSM, df_nonmatching, by="NIU")
    dim(df_matched)
  
  # Examine the region of common support
    ggplot(df_matched, aes(distance, fill=factor(HS_program))) + 
      geom_histogram(alpha=0.5, position="identity", bins=50) +
      xlab(paste("Predicted Probability, method:",mod_match$call$method)) +
      ylab("n of Samples") +
      theme_bw()
  
  # Formal Test of Covariate Balances
    # Categorical Data
    p_val_list <- list()
    for (i in matchvar_categorical){
      table <- df_matched[c("HS_program",i)] %>% 
        group_by_at(c(1,2)) %>% 
        summarize(x = n()) %>% 
        spread(HS_program, x) %>% 
        select_at(-1) %>% 
        as.matrix()
      print(paste(i,"    :",signif(prop.test(table)$p.value,5)))
      p_val_list[[i]] <- c(i,signif(prop.test(table)$p.value))
    }
    p_val_table <- do.call(rbind, p_val_list)
  
    # Numeric Data
    p_val_list <- list()
    for (i in matchvar_numeric){
      IPS <- df_matched[df_matched$HS_program==0,][c("HS_program",i)]
      IPA <- df_matched[df_matched$HS_program==1,][c("HS_program",i)]
      print(paste(i,"    :",signif(t.test(IPA[i],IPS[i])$p.value,5)))
      p_val_list[[i]] <- c(i,signif(t.test(IPA[i],IPS[i])$p.value))
    }
    p_val_table0 <- do.call(rbind, p_val_list)
    p_val_table <- rbind(p_val_table,p_val_table0)
    tableHTML(p_val_table)
    
    # Calculate the average absolute standardized difference
    match_stddif <- as.data.frame(summary(mod_match)$sum.matched)
    all_stddif <- as.data.frame(summary(mod_match)$sum.all)
    mean(abs(match_stddif$`Mean Diff`)) - mean(abs(all_stddif$`Mean Diff`))
    
  
##### Statistical Analysis: Unmatched Samples #####
  
  # ESSENTIAL CODE TO BE RUN FIRST ON THIS SECTION!!!!! #
  df_used <- df[df$HS_program!="Lainnya",]
  df_used$HS_program <- ifelse(df_used$HS_program=="IPA", 1, 0)
  
  # On this section, HS_program dummy is a little reversed. Dont get confused. 
    
  # Paired T-test of dependent samples
    
    test1 <- t.test(df_IPA$GPA_Agg, df_IPS$GPA_Agg, paired=FALSE)
    test2 <- t.test(df_IPA$GPA_Quant_mean, df_IPS$GPA_Quant_mean, paired=FALSE)
    test3 <- t.test(df_IPA$GPA_Qual_mean, df_IPS$GPA_Qual_mean, paired=FALSE)
    
    p1 <- test1$p.value
    d1 <- test1$estimate
    cl1 <- test1$conf.int[1]
    cu1 <- test1$conf.int[2]
    
    p2 <- test2$p.value
    d2 <- test2$estimate
    cl2 <- test2$conf.int[1]
    cu2 <- test2$conf.int[2]
    
    p3 <- test3$p.value
    d3 <- test3$estimate
    cl3 <- test3$conf.int[1]
    cu3 <- test3$conf.int[2]
    
    table_t_pair <- matrix(c(p1,p2,p3,
                             d1,d2,d3,
                             cl1,cl2,cl3,
                             cu1,cu2,cu3),nrow=3)
    rownames(table_t_pair) <- c("GPA_Agg","GPA_Quant_mean","GPA_Qual_mean")
    colnames(table_t_pair) <- c("p-value","Estimated IPA","Estimated IPS","CI:LL","CI:UL")
    table_t_pair <- round(as.data.frame(table_t_pair),5)
    tableHTML(table_t_pair)
    
    ### GPA, quants and quals ###
    
    # Tobit 
    m0 <- vglm(GPA_Agg ~ HS_program*Major*Program, 
               tobit(Upper=4,Lower=0), data=df_used)
    mx1 <- vglm(GPA_Agg ~ Major*Program, 
                tobit(Upper=4,Lower=0), data=df_used)
    tableHTML(summary(m0)@coef3)
    m1 <- vglm(GPA_Quant_mean ~ HS_program*Major*Program, 
               tobit(Upper=4,Lower=0), data=df_used)
    tableHTML(summary(m1)@coef3)
    m2 <- vglm(GPA_Qual_mean ~ HS_program*Major*Program, 
               tobit(Upper=4,Lower=0), data=df_used)
    tableHTML(summary(m2)@coef3)
    
    # LR-Test for Tobit Models
    print(pchisq(2*(logLik(m0)-logLik(mx1)), df=2, lower.tail=FALSE))
    
    ### Statistical Analysis: Duration Model ###
    ### Kaplan-Meier Survival Function ###
    
    ## Based on High School Program ##
    surv_fit <- survfit(Surv(Study_Duration,D_Graduated)~HS_program, data=df_used)
    print(surv_fit)
    
    survival_plot <- ggsurvplot(surv_fit, pval = TRUE,
                                conf.int = TRUE, conf.int.style = "step",
                                xlab = "Time in days",
                                ylab = "Probability of Survival (Not yet graduated)",
                                break.time.by = 365,
                                legend.labs = c("Social Science/IPS","Natural Science/IPA"),
                                ggtheme = theme_light(), 
                                risk.table = "percentage",
                                risk.table.y.text.col = T,
                                risk.table.y.text = FALSE,  
                                surv.median.line = "hv",
                                palette = c("#E7B800", "#0b5dd9"),
                                xlim=c(1000,max(df$Study_Duration)))
    survival_plot
    
    ## LR-Test for Kaplan-Meier Survival Function ##
    surv_diff <- survdiff(Surv(Study_Duration,D_Graduated)~HS_program, data=df_used)
    print(surv_diff)
    
    ### Regressions ###
    
    # Cox Proportional-Hazard Model
    cox_reg0 <- coxph(Surv(Study_Duration,D_Graduated) ~ GPA_Quant_mean + GPA_Qual_mean + HS_program + D_Graduated + 
                        Program + Major + Gender + Age_startprog + factor(Curriculum) +
                        factor(Batch),
                      data=df_used)
    
    cox_reg1 <- coxph(Surv(Study_Duration,D_Graduated) ~ GPA_Quant_mean + GPA_Qual_mean + HS_program + D_Graduated + 
                        Program + Major + Gender + Age_startprog + factor(Curriculum) +
                        factor(Batch) + factor(HS_province),
                      data=df_used)
    
    cox_reg2 <- coxph(Surv(Study_Duration,D_Graduated) ~ GPA_Quant_mean + GPA_Qual_mean + HS_program + D_Graduated + 
                        Program + Major + Gender + Age_startprog + factor(Curriculum) +
                        factor(Batch) + factor(HS_province) + factor(Thesis_spv),
                      data=df_used)
    
    
    coeff_cox_full_0 <- data.frame(varnames=row.names(summary(cox_reg0)$coefficients),
                                   summary(cox_reg0)$coefficients)
    coeff_cox_part_0 <- coeff_cox_full_0[!(grepl("Batch", coeff_cox_full_0$varnames)|
                                             grepl("HS_province", coeff_cox_full_0$varnames)|
                                             grepl("Thesis_spv", coeff_cox_full_0$varnames)),]
    
    coeff_cox_full_1 <- data.frame(varnames=row.names(summary(cox_reg1)$coefficients),
                                   summary(cox_reg1)$coefficients)
    coeff_cox_part_1 <- coeff_cox_full_1[!(grepl("Batch", coeff_cox_full_1$varnames)|
                                             grepl("HS_province", coeff_cox_full_1$varnames)|
                                             grepl("Thesis_spv", coeff_cox_full_1$varnames)),]
    
    coeff_cox_full_2 <- data.frame(varnames=row.names(summary(cox_reg2)$coefficients),
                                   summary(cox_reg2)$coefficients)
    coeff_cox_part_2 <- coeff_cox_full_2[!(grepl("Batch", coeff_cox_full_2$varnames)|
                                             grepl("HS_province", coeff_cox_full_2$varnames)|
                                             grepl("Thesis_spv", coeff_cox_full_2$varnames)),]
    
    
    table_cox_reg0 <- tableHTML(coeff_cox_part_0[,-1])
    table_cox_reg1 <- tableHTML(coeff_cox_part_1[,-1])
    table_cox_reg2 <- tableHTML(coeff_cox_part_2[,-1])
    table_cox_reg0
    table_cox_reg1
    table_cox_reg2
    
    ggsurvplot(survfit(cox_reg1), conf.int = TRUE, data=df_used)
    
    # LR-Test for Cox Models
    print(pchisq(2*(logLik(cox_reg1)[1]-logLik(cox_reg0)[1]), df=2, lower.tail=FALSE))
    
    x <- df_used %>% group_by(Batch,D_Graduated) %>% summarize(IPK = mean(GPA_Agg))
    x[sort(x),]
  

##### Statistical Analysis: Matched Samples #####
  
  # ESSENTIAL CODE TO BE RUN FIRST ON THIS SECTION!!!!! #
  df_used <- df_matched
  
  # On this section, HS_program dummy is a little reversed. Dont get confused. 
  
  # Paired T-test of dependent samples
  
  df_IPA <- df_used[df_used$HS_program==1,]
  df_IPA <- df_IPA[order(df_IPA$distance),]
  df_IPS <- df_used[df_used$HS_program==0,]
  df_IPS <- df_IPS[order(df_IPS$distance),]
  
  test1 <- t.test(df_IPA$GPA_Agg, df_IPS$GPA_Agg, paired=TRUE)
  test2 <- t.test(df_IPA$GPA_Quant_mean, df_IPS$GPA_Quant_mean, paired=TRUE)
  test3 <- t.test(df_IPA$GPA_Qual_mean, df_IPS$GPA_Qual_mean, paired=TRUE)
  
  p1 <- test1$p.value
  d1 <- test1$estimate
  cl1 <- test1$conf.int[1]
  cu1 <- test1$conf.int[2]
  
  p2 <- test2$p.value
  d2 <- test2$estimate
  cl2 <- test2$conf.int[1]
  cu2 <- test2$conf.int[2]
  
  p3 <- test3$p.value
  d3 <- test3$estimate
  cl3 <- test3$conf.int[1]
  cu3 <- test3$conf.int[2]
  
  table_t_pair <- matrix(c(p1,p2,p3,
                           d1,d2,d3,
                           cl1,cl2,cl3,
                           cu1,cu2,cu3),nrow=3)
  rownames(table_t_pair) <- c("GPA_Agg","GPA_Quant_mean","GPA_Qual_mean")
  colnames(table_t_pair) <- c("p-value","Estimated distance","CI:LL","CI:UL")
  table_t_pair <- round(as.data.frame(table_t_pair),5)
  tableHTML(table_t_pair)
  
  ### GPA, quants and quals ###
  
    # Tobit 
    m0 <- vglm(GPA_Agg ~ HS_program*Major*Program, 
                       tobit(Upper=4,Lower=0), data=df_used)
    mx1 <- vglm(GPA_Agg ~ Major*Program, 
               tobit(Upper=4,Lower=0), data=df_used)
    tableHTML(summary(m0)@coef3)
    m1 <- vglm(GPA_Quant_mean ~ HS_program*Major*Program, 
                       tobit(Upper=4,Lower=0), data=df_used)
    tableHTML(summary(m1)@coef3)
    m2 <- vglm(GPA_Qual_mean ~ HS_program*Major*Program, 
                       tobit(Upper=4,Lower=0), data=df_used)
    tableHTML(summary(m2)@coef3)
    
    # LR-Test for Tobit Models
    print(pchisq(2*(logLik(m0)-logLik(mx1)), df=2, lower.tail=FALSE))
  
  ### Statistical Analysis: Duration Model ###
    ### Kaplan-Meier Survival Function ###
    
      ## Based on High School Program ##
      surv_fit <- survfit(Surv(Study_Duration,D_Graduated)~HS_program, data=df_used)
      print(surv_fit)
      
      survival_plot <- ggsurvplot(surv_fit, pval = TRUE,
                                  conf.int = TRUE, conf.int.style = "step",
                                  xlab = "Time in days",
                                  ylab = "Probability of Survival (Not yet graduated)",
                                  break.time.by = 365,
                                  legend.labs = c("Social Science/IPS","Natural Science/IPA"),
                                  ggtheme = theme_light(), 
                                  risk.table = "percentage",
                                  risk.table.y.text.col = T,
                                  risk.table.y.text = FALSE,  
                                  surv.median.line = "hv",
                                  palette = c("#E7B800", "#0b5dd9"),
                                  xlim=c(1000,max(df$Study_Duration)))
      survival_plot
      
      ## LR-Test for Kaplan-Meier Survival Function ##
      surv_diff <- survdiff(Surv(Study_Duration,D_Graduated)~HS_program, data=df_used)
      print(surv_diff)
    
    ### Regressions ###
    
      # Cox Proportional-Hazard Model
      cox_reg0 <- coxph(Surv(Study_Duration,D_Graduated) ~ GPA_Agg + HS_program*Major*Program,
                       data=df_used)
      cox_reg1 <- coxph(Surv(Study_Duration,D_Graduated) ~ GPA_Agg + HS_program*Major*Program
                        + Curriculum,
                       data=df_used)
      cox_reg2 <- coxph(Surv(Study_Duration,D_Graduated) ~ GPA_Agg + HS_program*Major*Program
                        + Curriculum + Thesis_spv,
                        data=df_used)
      
      coeff_cox_full_0 <- data.frame(varnames=row.names(summary(cox_reg0)$coefficients),
                                   summary(cox_reg0)$coefficients)
      coeff_cox_part_0 <- coeff_cox_full_0[!(grepl("regency", coeff_cox_full_0$varnames)|
                                           grepl("Thesis", coeff_cox_full_0$varnames)|
                                           grepl("Curriculum", coeff_cox_full_0$varnames)),]
      
      coeff_cox_full_1 <- data.frame(varnames=row.names(summary(cox_reg1)$coefficients),
                                     summary(cox_reg1)$coefficients)
      coeff_cox_part_1 <- coeff_cox_full_1[!(grepl("regency", coeff_cox_full_1$varnames)|
                                               grepl("Thesis", coeff_cox_full_1$varnames)|
                                               grepl("Curriculum", coeff_cox_full_1$varnames)),]
      
      coeff_cox_full_2 <- data.frame(varnames=row.names(summary(cox_reg2)$coefficients),
                                     summary(cox_reg2)$coefficients)
      coeff_cox_part_2 <- coeff_cox_full_2[!(grepl("regency", coeff_cox_full_2$varnames)|
                                               grepl("Thesis", coeff_cox_full_2$varnames)|
                                               grepl("Curriculum", coeff_cox_full_2$varnames)),]
      
      
      table_cox_reg0 <- tableHTML(coeff_cox_part_0[,-1])
      table_cox_reg1 <- tableHTML(coeff_cox_part_1[,-1])
      table_cox_reg2 <- tableHTML(coeff_cox_part_2[,-1])
      table_cox_reg0
      table_cox_reg1
      table_cox_reg2
    
      ggsurvplot(survfit(cox_reg1), conf.int = TRUE, data=df_used)
                 
    # LR-Test for Cox Models
      print(pchisq(2*(logLik(cox_reg1)[1]-logLik(cox_reg0)[1]), df=2, lower.tail=FALSE))
        
    x <- df_used %>% group_by(Batch,D_Graduated) %>% summarize(IPK = mean(GPA_Agg))
    x[sort(x),]
  
  
