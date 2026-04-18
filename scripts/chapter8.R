# ====================================================
# Chapter 8 : Extension I: Stress communication and Dyadic Coping
# ====================================================

# ---- 0. Load the data and set the parameter ----

load("data/chap8/data.RData")
s <- 2
df[,1:48] <- lapply(df[,1:48], function(x) as.numeric(as.character(x)) + 1)

# ---- 1. Pattern identification ----

patternTest <- NULL

for(i in unique(df$dyad)){
  
  fm <- as.numeric(df[df$dyad==i & df$members=="FM",1:48])
  sm <- as.numeric(df[df$dyad==i & df$members=="SM",1:48])
  
  fm.test <- univariatePattern(states = s, chainFM = fm, chainSM = sm, alpha = 0.05)
  sm.test <- univariatePattern(states = s, chainFM = sm, chainSM = fm, alpha = 0.05)
  
  patternTest <- rbind(patternTest, c(fm.test$pattern, sm.test$pattern))
  
}

table(patternTest[, 1])
table(patternTest[, 2])

# ---- 2. Pattern analysis ----

TAB <- table(
  PatternFM = patternTest[,1],
  PatternSM = patternTest[,2]
)
dep <- chisq.test(TAB) 
dep
dep$observed
dep$expected
dep$residuals

ordre <- c("AM (A2)", "PM (A3)", "APM (A1)", "IM (A0)")
TAB_ord <- TAB[ordre, ordre]
TAB_ord <- TAB_ord[,nrow(TAB_ord):1]
assocplot(
  TAB_ord,
  col = c("#e6b8b7", "#b7cde6"),
  xlab = "Patterns of the women",
  ylab = "Patterns of the men"
)

