# read in data, analyse and produce graphs, by Wendy Olsen Oct. 2024
# it is not clear who authored the code initially. Their name & date was absent.
# The dataset is Asian Barometers India 2019.
# It is free data. The code #is protected by Creative Commons 2.0.
# Queries to Wendy.olsen@manchester.ac.uk
library(haven)
IndiaW5 <- read_dta("c:/AsianBaroIndia/data/AsianBaro2019revForEntropy2.dta")
#IndiaW5 <- W5_India_merged_core_20220905_released
variable_names <- colnames(IndiaW5)
print(variable_names)

l= lapply(IndiaW5, attr, "label")
l

library(tidyverse)

#### Cleaning ####

IndiaW5 <- IndiaW5 %>%
  mutate(Q63r = ifelse(Q63 == 7 | Q63 == 8 | Q63 == 9, NA, Q63))
count(IndiaW5, Q63, Q63r)

IndiaW5 <- IndiaW5 %>%
  mutate(Q69r = ifelse(Q69 == 7 | Q69 == 8 | Q69 == 9, NA, Q69))
count(IndiaW5, Q69, Q69r)

IndiaW5 <- IndiaW5 %>%
  mutate(Q146r = ifelse(Q146 == 7 | Q146 == 8 | Q146 == 9, NA, Q146))
count(IndiaW5, Q146, Q146r)

count(IndiaW5, SE2, Q146r)
library(forcats)

IndiaW5 <- IndiaW5 %>%
  mutate(SE2f = as_factor(SE2))
count(IndiaW5, SE2f, SE2)

IndiaW5 <- IndiaW5 %>%
  mutate(Q63b = ifelse(Q63 == 3, 4,
                       ifelse(Q63 == 4, 5,
                              ifelse(Q63 == 7 | Q63 == 8 | Q63 == 9, 3, Q63))))
count(IndiaW5, Q63, Q63b)

IndiaW5 <- IndiaW5 %>%
  mutate(Q69b = ifelse(Q69 == 3, 4,
                       ifelse(Q69 == 4, 5,
                              ifelse(Q69 == 7 | Q69 == 8 | Q69 == 9, 3, Q69))))
count(IndiaW5, Q69, Q69b)

IndiaW5 <- IndiaW5 %>%
  mutate(Q146b = ifelse(Q146 == 3, 4,
                       ifelse(Q146 == 4, 5,
                              ifelse(Q146 == 7 | Q146 == 8 | Q146 == 9, 3, Q146))))
count(IndiaW5, Q146, Q146b)

# Education

count(IndiaW5, SE5)
IndiaW5 <- IndiaW5 %>%
  mutate(SE5r = ifelse(SE5 == 99, NA, SE5))
count(IndiaW5, SE5, SE5r)

count(IndiaW5, SE5a) %>%
  print(n = 33)
IndiaW5 <- IndiaW5 %>%
  mutate(SE5ar = ifelse(SE5a == 99 | SE5a == 98, NA, SE5a))
count(IndiaW5, SE5a, SE5ar) %>%
  print(n = 33)

count(IndiaW5, Se3_1) %>%
  print(n = 100)

#### Crosstabs ####

# NA crosstabs
IndiaW5 %>%
  filter(!is.na(Q63r)) %>%
  count(SE2, Q63r) %>%
  group_by(SE2) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

IndiaW5 %>%
  group_by(SE2) %>%
  summarize(mean_Q63r = mean(Q63r, na.rm = TRUE),
            sd_Q63r = sd(Q63r, na.rm = TRUE))

IndiaW5 %>%
  filter(!is.na(Q69r)) %>%
  count(SE2, Q69r) %>%
  group_by(SE2) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

IndiaW5 %>%
  group_by(SE2) %>%
  summarize(mean_Q69r = mean(Q69r, na.rm = TRUE),
            sd_Q69r = sd(Q69r, na.rm = TRUE))

IndiaW5 %>%
  filter(!is.na(Q146r)) %>%
  count(SE2, Q146r) %>%
  group_by(SE2) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

IndiaW5 %>%
  group_by(SE2) %>%
  summarize(mean_Q146r = mean(Q146r, na.rm = TRUE),
            sd_Q146r = sd(Q146r, na.rm = TRUE))

# Neutral crosstabs

# Q63

IndiaW5 %>%
  filter(!is.na(Q63b)) %>%
  count(SE2, Q63b) %>%
  group_by(SE2) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

IndiaW5 %>%
  group_by(SE2) %>%
  summarize(mean_Q63b = mean(Q63b, na.rm = TRUE),
            sd_Q63b = sd(Q63b, na.rm = TRUE))

# Q69

IndiaW5 %>%
  filter(!is.na(Q69b)) %>%
  count(SE2, Q69b) %>%
  group_by(SE2) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

IndiaW5 %>%
  group_by(SE2) %>%
  summarize(mean_Q69b = mean(Q69b, na.rm = TRUE),
            sd_Q69b = sd(Q69b, na.rm = TRUE))

# Q146

IndiaW5 %>%
  filter(!is.na(Q146b)) %>%
  count(SE2, Q146b) %>%
  group_by(SE2) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

IndiaW5 %>%
  group_by(SE2) %>%
  summarize(mean_Q146b = mean(Q146b, na.rm = TRUE),
            sd_Q146b = sd(Q146b, na.rm = TRUE))

# Education crosstabs 
IndiaW5 %>%
  group_by(SE2) %>%
  summarize(mean_SE5ar = mean(SE5ar, na.rm = TRUE),
            SE5ar = sd(SE5ar, na.rm = TRUE))


count(IndiaW5, SE5)
IndiaW5 %>%
filter(!is.na(SE5r)) %>%
  count(SE2, SE5r) %>%
  group_by(SE2) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

# Age mean and SD

IndiaW5 %>%
  group_by(SE2) %>%
  summarize(mean_Se3_1 = mean(Se3_1, na.rm = TRUE),
            sd_Se3_1 = sd(Se3_1, na.rm = TRUE))


#### Visualisation ####

# NA graphs
mean_data <- IndiaW5 %>%
  group_by(SE2) %>%
  summarize(mean_Q63r = mean(Q63r, na.rm = TRUE))


IndiaW5 %>%
  filter(!is.na(Q63r)) %>%
  count(SE2, Q63r) %>%
  group_by(SE2) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = factor(Q63r), y = percentage, fill = factor(SE2))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(data = mean_data, aes(xintercept = mean_Q63r, color = factor(SE2)), linetype = "dashed", size = 1.5) + # Add mean line
  facet_wrap(~SE2, scales = "free_y", ncol = 2, labeller = labeller(SE2 = c("1" = "Male", "2" = "Female"))) +
  labs(x = "Percentage agreeing/disagreeing",
       y = "Percentage", fill = "Gender") +
  scale_x_discrete(
    breaks = c("1", "2", "3", "4"),
    labels = c("1" = "Strongly agree", "2" = "Somewhat agree", "3" = "Somewhat disagree", "4" = "Strongly disagree")
  ) +
  scale_fill_manual(values = c("1" = "lightcyan3", "2" = "thistle2"), 
                    labels = c("1" = "Male", "2" = "Female")) +
  scale_color_manual(values = c("1" = "black", "2" = "black")) + 
  theme_minimal() +
  ggtitle("Mother-in-law and daughter-in-law conflict graph") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Neutral graphs

# Q63

mean_data <- IndiaW5 %>%
  group_by(SE2) %>%
  summarize(mean_Q63b = mean(Q63b, na.rm = TRUE))


IndiaW5 %>%
  filter(!is.na(Q63b)) %>%
  count(SE2, Q63b) %>%
  group_by(SE2) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = factor(Q63b), y = percentage, fill = factor(SE2))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(data = mean_data, aes(xintercept = mean_Q63b, color = factor(SE2)), linetype = "dashed", size = 1.5) + 
  facet_wrap(~SE2, scales = "free_y", ncol = 2, labeller = labeller(SE2 = c("1" = "Male", "2" = "Female"))) +
  labs(x = "Particpants agree/disagree",
       y = "Percentage", fill = "Gender") +
  scale_x_discrete(
    breaks = c("1", "2", "3", "4", "5"),
    labels = c("1" = "Strongly agree", "2" = "Somewhat agree", "3" = "NA", "4" = "Somewhat disagree", "5" = "Strongly disagree")
  ) +
  scale_fill_manual(values = c("1" = "lightcyan3", "2" = "thistle2"), 
                    labels = c("1" = "Male", "2" = "Female")) +
  scale_color_manual(values = c("1" = "black", "2" = "black")) + 
  theme_minimal() +
  ggtitle("Mother-in-law and daughter-in-law conflict graph") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Q69

mean_data <- IndiaW5 %>%
  group_by(SE2) %>%
  summarize(mean_Q69b = mean(Q69b, na.rm = TRUE))


IndiaW5 %>%
  filter(!is.na(Q69b)) %>%
  count(SE2, Q69b) %>%
  group_by(SE2) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = factor(Q69b), y = percentage, fill = factor(SE2))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(data = mean_data, aes(xintercept = mean_Q69b, color = factor(SE2)), linetype = "dashed", size = 1.5) + 
  facet_wrap(~SE2, scales = "free_y", ncol = 2, labeller = labeller(SE2 = c("1" = "Male", "2" = "Female"))) +
  labs(x = "Particpants agree/disagree",
       y = "Percentage", fill = "Gender") +
  scale_x_discrete(
    breaks = c("1", "2", "3", "4", "5"),
    labels = c("1" = "Strongly agree", "2" = "Somewhat agree", "3" = "NA", "4" = "Somewhat disagree", "5" = "Strongly disagree")
  ) +
  scale_fill_manual(values = c("1" = "lightcyan3", "2" = "thistle2"), 
                    labels = c("1" = "Male", "2" = "Female")) +
  scale_color_manual(values = c("1" = "black", "2" = "black")) + 
  theme_minimal() +
  ggtitle("It is more preferable to have a boy than a girl") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Q146

mean_data <- IndiaW5 %>%
  group_by(SE2) %>%
  summarize(mean_Q146b = mean(Q146b, na.rm = TRUE))


IndiaW5 %>%
  filter(!is.na(Q146b)) %>%
  count(SE2, Q146b) %>%
  group_by(SE2) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = factor(Q146b), y = percentage, fill = factor(SE2))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(data = mean_data, aes(xintercept = mean_Q146b, color = factor(SE2)), linetype = "dashed", size = 1.5) + 
  facet_wrap(~SE2, scales = "free_y", ncol = 2, labeller = labeller(SE2 = c("1" = "Male", "2" = "Female"))) +
  labs(x = "Particpants agree/disagree",
       y = "Percentage", fill = "Gender") +
  scale_x_discrete(
    breaks = c("1", "2", "3", "4", "5"),
    labels = c("1" = "Strongly agree", "2" = "Somewhat agree", "3" = "NA", "4" = "Somewhat disagree", "5" = "Strongly disagree")
  ) +
  scale_fill_manual(values = c("1" = "lightcyan3", "2" = "thistle2"), 
                    labels = c("1" = "Male", "2" = "Female")) +
  scale_color_manual(values = c("1" = "black", "2" = "black")) + 
  theme_minimal() +
  ggtitle("Women should not be involved in politics as much as men") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
