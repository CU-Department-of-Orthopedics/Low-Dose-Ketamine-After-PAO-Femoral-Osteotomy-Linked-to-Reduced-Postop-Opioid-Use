## Pain Analysis 
library(readxl)
library(tidyverse)

source("Data Clean.R")

rm(dat)
rm(dat_day1)

# Data Clean 
dat_pain <- ...
dat_pain <- dat_pain %>% 
  filter(
    MRN %in% keep_ID
  )

dat_pain$MRN <- as.factor(dat_pain$MRN)
dat_pain$ReceivedKetamine <- as.factor(dat_pain$ReceivedKetamine)
dat_pain$PainScore <- as.numeric(dat_pain$PainScore)

dat_pain <- dat_pain %>% 
  group_by(
    PainDay, 
    MRN,
    ReceivedKetamine
    ) %>% 
  dplyr::summarise(
    mean_pain = mean(PainScore, na.rm = T)
  ) %>% 
  ungroup(
    
  ) %>% 
  filter(
    PainDay <= 7 
  )

dat_pain$MRN <- as.factor(dat_pain$MRN)

## Analysis 

# Covariance Structure 

library(nlme)

# Data 
y <- dat_pain$mean_pain
time.t <- as.factor(dat_pain$PainDay)  # As visit number 
S <- dat_pain$MRN
pred <- dat_pain$ReceivedKetamine

## CS 
corr.n = corCompSymm(form = ~1|S)
var.n = varIdent(form = ~-1)   # Equal variance  
fit.gls = gls(y ~ pred + time.t, weights = var.n, correlation = corr.n, method = "REML", na.action = na.omit)    

aic.cs = AIC(fit.gls)
bic.cs = BIC(fit.gls)

rm(fit.gls)

## CHS 
corr.n = corCompSymm(form = ~1|S)
var.n = varIdent(form = ~1|time.t) 
fit.gls = gls(y ~ pred + time.t, weights = var.n, correlation = corr.n, method = "REML", na.action = na.omit)    

aic.chs = AIC(fit.gls)
bic.chs = BIC(fit.gls)

rm(fit.gls)

## AR1
corr.n = corAR1(form = ~1|S)    
var.n = varIdent(form = ~-1)
fit.gls = gls(y ~ pred + time.t, weights = var.n, correlation = corr.n, method = "REML", na.action = na.omit)    

aic.ar1 = AIC(fit.gls)
bic.ar1 = BIC(fit.gls)

rm(fit.gls)

## ARH1
corr.n = corAR1(form = ~1|S) 
var.n = varIdent(form = ~1|time.t)    
fit.gls = gls(y ~ pred + time.t, weights = var.n, correlation = corr.n, method = "REML", na.action = na.omit)    

aic.arh1 = AIC(fit.gls)
bic.arh1 = BIC(fit.gls)

rm(fit.gls)

## Structure Comparison 
ic.comp_pain = data.frame(
  Structure = c("CS", "CHS", "AR1", "ARH1"),
  cor_str = c("corCompSymm(form = ~1|S)", "corCompSymm(form = ~1|S)", "corAR1(form = ~1|S)", "corAR1(form = ~1|S)"),
  var_str = c("varIdent(form = ~-1)", "varIdent(form = ~1|time.t)", "varIdent(form = ~-1)", "varIdent(form = ~1|time.t)"),
  AIC = c(aic.cs, aic.chs, aic.ar1, aic.arh1),
  BIC = c(bic.cs, bic.chs, bic.ar1, bic.arh1)
)

ic.comp_pain

# Best Covar Structure 
corr.n.best = corAR1(form = ~1|S)
var.n.best = varIdent(form = ~1|time.t)

## Analysis 
fit.gls_pain_int <- gls(mean_pain ~ ReceivedKetamine + PainDay + ReceivedKetamine*PainDay, data = dat_pain, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)

summary(fit.gls_pain_int)
plot(fit.gls_pain_int)

fit.gls_pain_int_sum <- summary(fit.gls_pain_int)
fit.gls_pain_int_sum <- round(as.data.frame(fit.gls_pain_int_sum$tTable), 3)
rownames(fit.gls_pain_int_sum) <- c(
  "(Intercept)",
  "Ketamine vs Control",
  "Time",
  "Interaction (Ketamine x Time)"
)

fit.gls_pain_int_sum <- fit.gls_pain_int_sum %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  )

fit.gls_pain_int_sum


## Plots 

pain_p <- ggplot(
  data = dat_pain, 
  aes(
    x = as.factor(PainDay),
    y = mean_pain,
    fill = ReceivedKetamine
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  stat_summary(fun.y = mean, color = "grey", position = position_dodge(0.75),
               geom = "point", shape = 18, size = 2,
               show.legend = FALSE) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "bottom"
  )  + 
  labs(
    x = "Post-Surgery Measurement #",
    y = "Mean Pain Score",
    fill = "Received Ketamine?"
  ) + 
  scale_x_discrete(
    labels= c("DOS", 2, 3, 4, 5, 6, 7)
  ) + 
  geom_vline(
    xintercept = 1.5,
    color = "grey",
    linetype = "dashed"
  )

pain_p

## Summary Tables 

library(table1)

dat_pain_sum <- dat_pain

dat_pain_sum$ReceivedKetamine <- factor(dat_pain_sum$ReceivedKetamine, labels = c("Did Not Receive Ketamine", "Received Ketamine"))
dat_pain_sum$PainDay <- factor(dat_pain_sum$PainDay, labels= c("DOS", 2, 3, 4, 5, 6, 7))

names(dat_pain_sum)[names(dat_pain_sum) == 'mean_pain'] <- 'Mean Pain Score'

render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits = 3, round.integers = F, digits.pct = 2), 
       c("", "Mean (SD)" = sprintf("%s (&plusmn;%s)", MEAN, SD)))
}

render.cat <- function(x) {
  c("", 
    sapply(stats.default(x), 
           function(y) with(y, sprintf("%d (%0.0f%%)", FREQ, PCT))))
}


pvalue <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times = sapply(x, length)))
  if (is.numeric(y)) {
    p <- t.test(y ~ g)$p.value
  } else {
    p <- chisq.test(table(y, g))$p.value
  }
  c("", sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)))
}

t1_pain <- table1(
  ~ `Mean Pain Score` | ReceivedKetamine,
  data = dat_pain_sum,
  overall = F,
  render.continuous = render.cont,
  render.categorical = render.cat,
  # extra.col = list(`p-value` = pvalue),
  render.missing = NULL,
  footnote = "Mean Pain Score, averaged over all Pain Days"
)

t2.1_pain <- table1(
  ~ `Mean Pain Score` | ReceivedKetamine + PainDay,
  data = subset(dat_pain_sum, dat_pain_sum$ReceivedKetamine != "Did Not Receive Ketamine"),
  overall = F,
  render.continuous = render.cont,
  render.categorical = render.cat,
  render.missing = NULL,
  rowlabelhead = "Day Post-Surgery",
  footnote = "Mean Pain Scores for Ketamine patients, by post-surgery Pain Day."
)


t2.2_pain <- table1(
  ~ `Mean Pain Score` | ReceivedKetamine + PainDay,
  data = subset(dat_pain_sum, dat_pain_sum$ReceivedKetamine == "Did Not Receive Ketamine"),
  overall = F,
  render.continuous = render.cont,
  render.categorical = render.cat,
  render.missing = NULL,
  rowlabelhead = "Day Post-Surgery",
  footnote = "Mean Pain Scores for Non-Ketamine patients, by post-surgery Pain Day."
)



t1_pain
t2.1_pain
t2.2_pain
