### Secondary Analysis 

library(lme4)

dat_stay <- dat %>% 
  group_by(PID1) %>% top_n(1, PainDay) %>% 
  distinct(PID1,.keep_all = T)

ttest <- t.test(dat_stay$PainDay ~ dat_stay$ReceivedKetamine)

wilcox.test(dat_stay$PainDay ~ dat_stay$ReceivedKetamine)

library(broom)
library(purrr)

t_table <- map_df(list(ttest), tidy)
t_table <- t_table[, c(1, 4, 5, 7:9)]
names(t_table) <- c("Mean Difference", "t-stat", "p-value", "95% CI (LB)", "95% CI (UB)", "Method")

library(tidyverse)

t_p <- ggplot(
  data = dat_stay,
  aes(
    x = ReceivedKetamine,
    y = PainDay,
    fill = ReceivedKetamine
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  stat_summary(fun.y = mean, color = "grey", position = position_dodge(0.75),
               geom = "point", shape = 18, size = 4,
               show.legend = FALSE) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "none"
  ) + 
  labs(
    x = "Received Ketamine?",
    y = "Max. Days of Treatment"
  )

dat_stay %>% 
  group_by(
    ReceivedKetamine
  ) %>% 
  summarize(
    mean = mean(PainDay),
    sd = sd(PainDay)
  )
