require(tidyverse)
require(data.table)
require(haven)
require(sjlabelled)
require(survival)
require(survminer)
require(ggplot2)

setwd("F:/Box Sync/Archives2020LLY/Zhengting/Duke Kunshan University Intern (zh133@duke.edu)/4 healthy aging-CLHLS/CLHLS data and codebook_Yaxi (zh133@duke.edu)/CLHLS data and codebook_Yaxi/Longitudinal data_with survival time_SAS version")


# --------------------------------------------------------------------------------------------------------------------------- #
################################################### import data of all waves ##################################################
# --------------------------------------------------------------------------------------------------------------------------- #

filenames <- list.files()
file <- str_replace(filenames, ".sas7bdat", "") # c("dat98_14", "dat00_14", "dat02_14", "dat05_14", "dat08_14", "dat11_14", "dat14_18_1125")

dat <- list()
for(i in 1:length(filenames)) {
    dat[[file[i]]] <- read_sas(filenames[i])
}

# clean heart rate
for(wave in c("dat98_14", "dat00_14", "dat02_14", "dat05_14", "dat11_14", "dat14_18_1125")) {
    dat[[wave]] <- dat[[wave]] %>% mutate(hr = g7) %>% select(id, hr)
}
dat[["dat08_14"]] <- dat[["dat08_14"]] %>% mutate(hr = (g71 + g72)/2) %>% select(id, hr)

# bind dataset
data_hr <- dat[[file[1]]]
for(i in 2:length(file)) {
    data_hr <- data_hr %>% bind_rows(dat[[file[i]]])
}

# change 999 -> NA
data_hr <- data_hr %>% mutate(hr = ifelse(hr == 888 | hr == 999, NA, hr))

# merge with else data
data <- readRDS("F:/Box Sync/Archives2020LLY/Zhengting/Duke Kunshan University Intern (zh133@duke.edu)/4 healthy aging-CLHLS/P06 Healthy lifestyle/formal analysis by ZH/cleaned data/lifesty_ana18_non_repeated.rds")
data <- data %>% full_join(data_hr, by = "id")
data <- data %>% mutate(bmihr = bmi * hr) %>% filter(is.na(bmihr) == FALSE)

# merge with survival time and status
data_surtime <- read_dta("F:/Box Sync/Archives2020LLY/Zhengting/Duke Kunshan University Intern (zh133@duke.edu)/4 healthy aging-CLHLS/P06 Healthy lifestyle/formal analysis by ZH/cleaned data/idnum_survival18_v13.dta")
data_surtime <- data_surtime[, c(1, 3:5)]

data <- data %>% left_join(data_surtime, by = "id")

# exploration

## drop Obs with problematic heart rate
data <- data %>% mutate(bmihr = ifelse(hr >= 200, NA, bmihr)) %>% filter(is.na(bmihr) == FALSE)

## recode bmihr to quartile
data <- data %>% mutate(bmihr_quartile = case_when(bmihr <= 1236.3 ~ "Q1",
                                                   1236.3 < bmihr & bmihr <= 1427.3 ~ "Q2",
                                                   1427.3 < bmihr & bmihr <= 1655.6 ~ "Q3",
                                                   bmihr > 1655.6 ~ "Q4")) %>%
                     mutate(bmihr_quartile = factor(bmihr_quartile, levels = c("Q1", "Q2", "Q3", "Q4")))

## fit K-M
fit <- survfit(Surv(survival_bas18,censor18) ~ bmihr_quartile, data = data)
print(summary(fit)$table)
tiff("F:/Box Sync/Archives2020LLY/Zhengting/Figure 1. BMIHR K-M survival curve.tiff", 
     width = 8000, height = 7230, pointsize = 12, res = 600)
ggsurvplot(fit, 
           data = data,
           palette = "jco",
           linetype = 1,
           censor.size = 0.5, # change the width of the curve by this command
           xlab = "Duration of follow-up (years)",
           ylab = "Probability of survival",
           break.x.by = 2,
           xlim = c(0,14),
           legend.title = "BMI * HR",
           #legend.labs = c("Q1", "Q2", "Q3", "Q4"), 
           risk.table = TRUE,
           fontsize = 6,
           risk.table.title = "Number at risk (Deaths)",
           font.main = c(15, "bold", "black"),
           font.x = c(15, "bold", "black"),
           font.y = c(15, "bold", "black"),
           font.tickslab = c(15, "bold", "black"),
           font.legend = c(15, "bold", "black"),
           tables.theme = theme_survminer(font.main = c(15, "bold", "black"),
                                          font.x = c(15, "bold", "black"),
                                          font.y = c(15, "bold", "black"),
                                          font.tickslab = c(15, "bold")))
dev.off()

# only BMI: fit K-M
fit <- survfit(Surv(survival_bas18,censor18) ~ bmi_cat, data = data)
print(summary(fit)$table)
tiff("F:/Box Sync/Archives2020LLY/Zhengting/Figure 2. BMI K-M survival curve.tiff", 
     width = 8000, height = 7230, pointsize = 12, res = 600)
ggsurvplot(fit, 
           data = data,
           palette = "jco",
           linetype = 1,
           censor.size = 0.5, # change the width of the curve by this command
           xlab = "Duration of follow-up (years)",
           ylab = "Probability of survival",
           break.x.by = 2,
           xlim = c(0,14),
           legend.title = "BMI",
           legend.labs = c("underweight", "normal weight", "overweight"), 
           risk.table = TRUE,
           fontsize = 6,
           risk.table.title = "Number at risk (Deaths)",
           font.main = c(15, "bold", "black"),
           font.x = c(15, "bold", "black"),
           font.y = c(15, "bold", "black"),
           font.tickslab = c(15, "bold", "black"),
           font.legend = c(15, "bold", "black"),
           tables.theme = theme_survminer(font.main = c(15, "bold", "black"),
                                          font.x = c(15, "bold", "black"),
                                          font.y = c(15, "bold", "black"),
                                          font.tickslab = c(15, "bold")))
dev.off()

# only heart rate
data <- data %>% mutate(hr_quartile = case_when(hr <= 68 ~ "Q1",
                                                68 < hr & hr <= 72 ~ "Q2",
                                                72 < hr & hr <= 79 ~ "Q3",
                                                hr > 79 ~ "Q4")) %>%
                     mutate(hr_quartile = factor(hr_quartile, levels = c("Q1", "Q2", "Q3", "Q4")))
fit <- survfit(Surv(survival_bas18,censor18) ~ hr_quartile, data = data)
print(summary(fit)$table)
tiff("F:/Box Sync/Archives2020LLY/Zhengting/Figure 3. HR K-M survival curve.tiff", 
     width = 8000, height = 7230, pointsize = 12, res = 600)
ggsurvplot(fit, 
           data = data,
           palette = "jco",
           linetype = 1,
           censor.size = 0.5, # change the width of the curve by this command
           xlab = "Duration of follow-up (years)",
           ylab = "Probability of survival",
           break.x.by = 2,
           xlim = c(0,14),
           legend.title = "HR",
           legend.labs = c("Q1", "Q2", "Q3", "Q4"), 
           risk.table = TRUE,
           fontsize = 6,
           risk.table.title = "Number at risk (Deaths)",
           font.main = c(15, "bold", "black"),
           font.x = c(15, "bold", "black"),
           font.y = c(15, "bold", "black"),
           font.tickslab = c(15, "bold", "black"),
           font.legend = c(15, "bold", "black"),
           tables.theme = theme_survminer(font.main = c(15, "bold", "black"),
                                          font.x = c(15, "bold", "black"),
                                          font.y = c(15, "bold", "black"),
                                          font.tickslab = c(15, "bold")))
dev.off()

# export dataset
saveRDS(data, "F:/Box Sync/Archives2020LLY/Zhengting/clhls_bmi_hr_pilot.rds")
write_dta(data, "F:/Box Sync/Archives2020LLY/Zhengting/clhls_bmi_hr_pilot.dta")