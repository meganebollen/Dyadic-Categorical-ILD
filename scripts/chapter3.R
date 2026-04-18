# ====================================================
# Chapter 3 : Dyadic Pattern Bivariate Single-Case
# ====================================================

# ---- 0. Load the data and set the parameter ----

#pattern identification
load("data/chap3/dataBivariateSingleCase.RData") 
s <- 2

#sensitive analysis
load("data/chap3/dataComplete30.RData")
load("data/chap3/dataComplete60.RData")
load("data/chap3/dataComplete90.RData")
load("data/chap3/dataComplete180.RData")
load("data/chap3/dataComplete720.RData")

#aic and bic comparison
load("data/chap3/aic_bic.RData")

# ---- 1. Analysis for the romantic satisfaction ----

## ---- 1.1 Woman ----

#V1 = X, main partner = woman
countEmpW_X <- countEmpBivariate(states = s, 
                                 chainFM_V1 = data$XW, chainSM_V1 = data$XM, 
                                 chainFM_V2 = data$YW, chainSM_V2 = data$YM)
countEmpW_X

#identification of the case
case <- bivariateCase(empirical = countEmpW_X, alpha = 0.05) 
case

#identification of the pattern
romantic_resW <- univariatePattern(states = s, chainFM = data$XW, chainSM = data$XM, alpha = 0.05)
romantic_resW

## ---- 1.2 Man ----

#V1 = X, main partner = man
countEmpM_X <- countEmpBivariate(states = s, 
                                 chainFM_V1 = data$XM, chainSM_V1 = data$XW, 
                                 chainFM_V2 = data$YM, chainSM_V2 = data$YW)
countEmpM_X

#identification of the case
case <-bivariateCase(empirical = countEmpM_X, alpha = 0.05) 
case

#identification of the pattern
romantic_resM <- partialPattern(empirical = countEmpM_X)
romantic_resM

# ---- 2. Analysis for the emotional distress ----

## ---- 2.1 Woman ----

#V1 = Y, main partner = woman
countEmpW_Y <- countEmpBivariate(states = s, 
                                 chainFM_V1 = data$YW, chainSM_V1 = data$YM, 
                                 chainFM_V2 = data$XW, chainSM_V2 = data$XM)

#identification of the case
case <- bivariateCase(empirical = countEmpW_Y, alpha = 0.05) 
case

#identification of the pattern
emotional_resW <- completePattern(empirical = countEmpW_Y)
emotional_resW

## ---- 2.2 Man ----

#V1 = Y, main partner = man
countEmpM_Y <- countEmpBivariate(states = s, 
                                 chainFM_V1 = data$YM, chainSM_V1 = data$YW, 
                                 chainFM_V2 = data$XM, chainSM_V2 = data$XW)
countEmpM_Y

#identification of the case
case <- bivariateCase(empirical = countEmpM_Y, alpha = 0.05) 
case 

#identification of the pattern
emotional_resM <- completePattern(empirical = countEmpM_Y)
emotional_resM

# ---- 3. Sensitivity analysis ----

## ---- 3.1 Data management ----

dataComplete30$Length <- rep(30, nrow(dataComplete30))
dataComplete60$Length <- rep(60, nrow(dataComplete60))
dataComplete90$Length <- rep(90, nrow(dataComplete90))
dataComplete180$Length <- rep(180, nrow(dataComplete180))
dataComplete720$Length <- rep(720, nrow(dataComplete720))

data <- rbind(dataComplete30[,31:36],
              dataComplete60[,61:66],
              dataComplete90[,91:96], 
              dataComplete180[,181:186], 
              dataComplete720[,721:726])

data$predictor_A1 <- recode(data$caseSimulated, "A1" = 1, "B2" = 0, "D4" = 0, "E4" = 0)
data$predictor_B2 <- recode(data$caseSimulated, "A1" = 0, "B2" = 1, "D4" = 0, "E4" = 0)
data$predictor_D4 <- recode(data$caseSimulated, "A1" = 0, "B2" = 0, "D4" = 1, "E4" = 0)
data$predictor_E4 <- recode(data$caseSimulated, "A1" = 0, "B2" = 0, "D4" = 0, "E4" = 1)

data$TYPE_A1 <- ifelse(data$local == "APM", 1, 0)
data$TYPE_B2 <- ifelse(data$local == "partial actor", 1, 0)
data$TYPE_D4 <- ifelse(data$local == "complete actor partner on the main, actor on the second", 1, 0)
data$TYPE_E4 <- ifelse(data$local == "complete actor on both", 1, 0)

## ---- 3.2 Global plot ----

data$correct_A1 <- ifelse(data$predictor_A1 == 1 & data$TYPE_A1 == 1, 1, 0)
data$correct_B2 <- ifelse(data$predictor_B2 == 1 & data$TYPE_B2 == 1, 1, 0)
data$correct_D4 <- ifelse(data$predictor_D4 == 1 & data$TYPE_D4 == 1, 1, 0)
data$correct_E4 <- ifelse(data$predictor_E4 == 1 & data$TYPE_E4 == 1, 1, 0)

data$pattern_correct <- ifelse(
  (data$predictor_A1 == 1 & data$TYPE_A1 == 1) |
    (data$predictor_B2 == 1 & data$TYPE_B2 == 1) |
    (data$predictor_D4 == 1 & data$TYPE_D4 == 1) |
    (data$predictor_E4 == 1 & data$TYPE_E4 == 1),
  1, 0
)

plot_data <- data %>%
  mutate(
    Length = factor(Length, levels = c(30, 60, 90, 180, 720)),
    caseSimulated = factor(caseSimulated, levels = c("A1", "B2", "D4", "E4")),
    pattern_correct = factor(pattern_correct,
                             levels = c(0, 1),
                             labels = c("Incorrect", "Correct"))
  ) %>%
  count(Length, caseSimulated, pattern_correct) %>%
  group_by(Length, caseSimulated) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(plot_data,
       aes(x = caseSimulated,
           y = prop,
           fill = pattern_correct)) +
  
  geom_bar(stat = "identity",
           position = "stack",
           width = 0.6) +
  
  facet_grid(vars(Length)) +
  
  scale_fill_manual(values = c("Correct" = "#31A354",
                               "Incorrect" = "#2C7FB8")) +
  
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, 0.2)) +
  
  labs(x = "Simulated Case",
       y = "Relative Frequency",
       fill = "Identification") +
  
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 16),
        panel.spacing = unit(1, "lines"))

## ---- 3.3 Univariate actor partner analysis ----

ggplot(data, aes(as.factor(TYPE_A1), y = after_stat(prop), group = 1)) +
  geom_bar(fill = "lightblue") +
  facet_grid(vars(Length)) +
  xlab("Patterns of interaction") +
  ylab("Frequency") +
  theme(axis.title.x = element_text(size = 18),  # cex.lab = 1.5
        axis.title.y = element_text(size = 18),  # cex.lab = 1.5
        axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust=1),  # cex.axis = 1.2
        axis.text.y = element_text(size = 14),  # cex.axis = 1.2
        strip.text = element_text(size = 16),  # size of facet labels (similar to strip.text in base R)
        panel.spacing = unit(1, "lines"))

table(data[data$Length==30,]$TYPE_A1, data[data$Length==30,]$predictor_A1)
confusionMatrix(as.factor(data[data$Length==30,]$TYPE_A1),
                as.factor(data[data$Length==30,]$predictor_A1), 
                positive = levels(as.factor(data[data$Length==30,]$predictor_A1))[2])

table(data[data$Length==60,]$TYPE_A1, data[data$Length==60,]$predictor_A1)
confusionMatrix(as.factor(data[data$Length==60,]$TYPE_A1),
                as.factor(data[data$Length==60,]$predictor_A1), 
                positive = levels(as.factor(data[data$Length==60,]$predictor_A1))[2])

table(data[data$Length==90,]$TYPE_A1, data[data$Length==90,]$predictor_A1)
confusionMatrix(as.factor(data[data$Length==90,]$TYPE_A1),
                as.factor(data[data$Length==90,]$predictor_A1), 
                positive = levels(as.factor(data[data$Length==90,]$predictor_A1))[2])

table(data[data$Length==180,]$TYPE_A1, data[data$Length==180,]$predictor_A1)
confusionMatrix(as.factor(data[data$Length==180,]$TYPE_A1),
                as.factor(data[data$Length==180,]$predictor_A1), 
                positive = levels(as.factor(data[data$Length==180,]$predictor_A1))[2])

table(data[data$Length==720,]$TYPE_A1, data[data$Length==720,]$predictor_A1)
confusionMatrix(as.factor(data[data$Length==720,]$TYPE_A1),
                as.factor(data[data$Length==720,]$predictor_A1), 
                positive = levels(as.factor(data[data$Length==720,]$predictor_A1))[2])

## ---- 3.4 Partial actor analysis ----

ggplot(data, aes(as.factor(TYPE_B2), y = after_stat(prop), group = 1)) +
  geom_bar(fill = "lightblue") +
  facet_grid(vars(Length)) +
  xlab("Patterns of interaction") +
  ylab("Frequency") +
  theme(axis.title.x = element_text(size = 18),  # cex.lab = 1.5
        axis.title.y = element_text(size = 18),  # cex.lab = 1.5
        axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust=1),  # cex.axis = 1.2
        axis.text.y = element_text(size = 14),  # cex.axis = 1.2
        strip.text = element_text(size = 16),  # size of facet labels (similar to strip.text in base R)
        panel.spacing = unit(1, "lines"))

table(data[data$Length==30,]$TYPE_B2, data[data$Length==30,]$predictor_B2)
confusionMatrix(as.factor(data[data$Length==30,]$TYPE_B2),
                as.factor(data[data$Length==30,]$predictor_B2), 
                positive = levels(as.factor(data[data$Length==30,]$predictor_B2))[2])

table(data[data$Length==60,]$TYPE_B2, data[data$Length==60,]$predictor_B2)
confusionMatrix(as.factor(data[data$Length==60,]$TYPE_B2),
                as.factor(data[data$Length==60,]$predictor_B2), 
                positive = levels(as.factor(data[data$Length==60,]$predictor_B2))[2])

table(data[data$Length==90,]$TYPE_B2, data[data$Length==90,]$predictor_B2)
confusionMatrix(as.factor(data[data$Length==90,]$TYPE_B2),
                as.factor(data[data$Length==90,]$predictor_B2), 
                positive = levels(as.factor(data[data$Length==90,]$predictor_B2))[2])

table(data[data$Length==180,]$TYPE_B2, data[data$Length==180,]$predictor_B2)
confusionMatrix(as.factor(data[data$Length==180,]$TYPE_B2),
                as.factor(data[data$Length==180,]$predictor_B2), 
                positive = levels(as.factor(data[data$Length==180,]$predictor_B2))[2])

table(data[data$Length==720,]$TYPE_B2, data[data$Length==720,]$predictor_B2)
confusionMatrix(as.factor(data[data$Length==720,]$TYPE_B2),
                as.factor(data[data$Length==720,]$predictor_B2), 
                positive = levels(as.factor(data[data$Length==720,]$predictor_B2))[2])

## ---- 3.5 Complete actor partner on the main, actor on the second analysis ----

ggplot(data, aes(as.factor(TYPE_D4), y = after_stat(prop), group = 1)) +
  geom_bar(fill = "lightblue") +
  facet_grid(vars(Length)) +
  xlab("Patterns of interaction") +
  ylab("Frequency") +
  theme(axis.title.x = element_text(size = 18),  # cex.lab = 1.5
        axis.title.y = element_text(size = 18),  # cex.lab = 1.5
        axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust=1),  # cex.axis = 1.2
        axis.text.y = element_text(size = 14),  # cex.axis = 1.2
        strip.text = element_text(size = 16),  # size of facet labels (similar to strip.text in base R)
        panel.spacing = unit(1, "lines"))

table(data[data$Length==30,]$TYPE_D4, data[data$Length==30,]$predictor_D4)
confusionMatrix(as.factor(data[data$Length==30,]$TYPE_D4),
                as.factor(data[data$Length==30,]$predictor_D4), 
                positive = levels(as.factor(data[data$Length==30,]$predictor_D4))[2])

table(data[data$Length==60,]$TYPE_D4, data[data$Length==60,]$predictor_D4)
confusionMatrix(as.factor(data[data$Length==60,]$TYPE_D4),
                as.factor(data[data$Length==60,]$predictor_D4), 
                positive = levels(as.factor(data[data$Length==60,]$predictor_D4))[2])

table(data[data$Length==90,]$TYPE_D4, data[data$Length==90,]$predictor_D4)
confusionMatrix(as.factor(data[data$Length==90,]$TYPE_D4),
                as.factor(data[data$Length==90,]$predictor_D4), 
                positive = levels(as.factor(data[data$Length==90,]$predictor_D4))[2])

table(data[data$Length==180,]$TYPE_D4, data[data$Length==180,]$predictor_D4)
confusionMatrix(as.factor(data[data$Length==180,]$TYPE_D4),
                as.factor(data[data$Length==180,]$predictor_D4), 
                positive = levels(as.factor(data[data$Length==180,]$predictor_D4))[2])

table(data[data$Length==720,]$TYPE_D4, data[data$Length==720,]$predictor_D4)
confusionMatrix(as.factor(data[data$Length==720,]$TYPE_D4),
                as.factor(data[data$Length==720,]$predictor_D4), 
                positive = levels(as.factor(data[data$Length==720,]$predictor_D4))[2])

## ---- 3.5 Complete actor on both analysis ----

ggplot(data, aes(as.factor(TYPE_D4), y = after_stat(prop), group = 1)) +
  geom_bar(fill = "lightblue") +
  facet_grid(vars(Length)) +
  xlab("Patterns of interaction") +
  ylab("Frequency") +
  theme(axis.title.x = element_text(size = 18),  # cex.lab = 1.5
        axis.title.y = element_text(size = 18),  # cex.lab = 1.5
        axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust=1),  # cex.axis = 1.2
        axis.text.y = element_text(size = 14),  # cex.axis = 1.2
        strip.text = element_text(size = 16),  # size of facet labels (similar to strip.text in base R)
        panel.spacing = unit(1, "lines"))

table(data[data$Length==30,]$TYPE_E4, data[data$Length==30,]$predictor_E4)
confusionMatrix(as.factor(data[data$Length==30,]$TYPE_E4),
                as.factor(data[data$Length==30,]$predictor_E4), 
                positive = levels(as.factor(data[data$Length==30,]$predictor_E4))[2])

table(data[data$Length==60,]$TYPE_E4, data[data$Length==60,]$predictor_E4)
confusionMatrix(as.factor(data[data$Length==60,]$TYPE_E4),
                as.factor(data[data$Length==60,]$predictor_E4), 
                positive = levels(as.factor(data[data$Length==60,]$predictor_E4))[2])

table(data[data$Length==90,]$TYPE_E4, data[data$Length==90,]$predictor_E4)
confusionMatrix(as.factor(data[data$Length==90,]$TYPE_E4),
                as.factor(data[data$Length==90,]$predictor_E4), 
                positive = levels(as.factor(data[data$Length==90,]$predictor_E4))[2])

table(data[data$Length==180,]$TYPE_E4, data[data$Length==180,]$predictor_E4)
confusionMatrix(as.factor(data[data$Length==180,]$TYPE_E4),
                as.factor(data[data$Length==180,]$predictor_E4), 
                positive = levels(as.factor(data[data$Length==180,]$predictor_E4))[2])

table(data[data$Length==720,]$TYPE_E4, data[data$Length==720,]$predictor_E4)
confusionMatrix(as.factor(data[data$Length==720,]$TYPE_E4),
                as.factor(data[data$Length==720,]$predictor_E4), 
                positive = levels(as.factor(data[data$Length==720,]$predictor_E4))[2])

# ---- 4. AIC vs BIC ----

ggplot(df, aes(x=type, y=se, group = 1)) +
  geom_point() + geom_line() +
  xlab("Patterns of interaction") +
  ylab("Frequency") +
  ylim(0,1) +
  facet_grid(vars(indice)) +
  scale_x_discrete(limit=c("A1", "B2", "E4", "D4")) +
  theme(axis.title.x = element_text(size = 18),  # cex.lab = 1.5
        axis.title.y = element_text(size = 18),  # cex.lab = 1.5
        axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust=1),  # cex.axis = 1.2
        axis.text.y = element_text(size = 14),  # cex.axis = 1.2
        strip.text = element_text(size = 16),  # size of facet labels (similar to strip.text in base R)
        panel.spacing = unit(1, "lines"))
