### Data Clean 

rm(list = ls())

library(readxl)
library(tidyverse)

dat <- ...

keep_ID <- unique(dat$MRN)

# dat2 <- read_excel("Data/Ketamine_MME_PAO_JAN_30.xlsx", sheet = "For Data Analysis")
# length(unique(dat2$MRN))
# 
# dat3 <- read_excel("Data/Ketamine_Pain_Dec2022.xlsx", sheet = "Data Analysis")
# length(unique(dat3$MRN))
# 
# dat3$PainScore <- as.numeric(dat3$PainScore)
# 
# dat3 <- dat3 %>% 
#   group_by(MRN, PainDay, ReceivedKetamine) %>% 
#   summarise(
#     mean_pain = mean(PainScore)
#   )
# 
# dat3$MRN <- as.factor(dat3$MRN)
# dat2$MRN <- as.factor(dat2$MRN)
# dat$MRN <- as.factor(dat$MRN)

# keep_MRN <- dat$MRN
# 
# dat3 <- dat3 %>% 
#   filter(
#     MRN %in% keep_MRN
#   ) %>% 
#   filter(
#     PainDay <=7
#   ) %>% 
#   na.omit(
#     
#   )
# 
# dat_final <- dat2 %>% 
#   filter(
#     MRN %in% keep_MRN
#   )

# dat <- dat_final 
dat$PrimaryCSN <- as.factor(dat$PrimaryCSN)

dat <- dat %>% 
  mutate(
    PID1 = group_indices(
      dat, 
      .dots=c("MRN")
      ),
    PID2 = group_indices(
      dat, 
      .dots=c("PrimaryCSN")
      )
    )


# length(unique(dat$PID1))  # n = 145


dat <- dat %>% 
  select(
    ReceivedKetamine, PainDay, `Total MME`, PID1, PID2
  )

dat$PID1 <- as.factor(dat$PID1)
dat$PID2 <- as.factor(dat$PID2)

dat <- dat %>% 
  filter(
    PainDay <= 7
  )

names(dat)[names(dat) == 'Total MME'] <- 'MME'
# length(unique(dat$PID1)) # n = 145

dat_day1 <- dat %>% 
  group_by(PID1) %>% 
  filter(
    PainDay == 1
  ) %>% 
  mutate(Code_frequency = n()) %>%
  distinct(PID1, .keep_all = T) 

table(dat_day1$Code_frequency)
