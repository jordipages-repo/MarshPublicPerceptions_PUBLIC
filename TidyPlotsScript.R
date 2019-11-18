# # # # # # # # # # # # # # # # # # # # 
# Redoing some plots and              #
# analysis for the Public Perceptions #
# Paper - National Questionnaire      #
# using tidyverse                     #
# 2019-03-01 - Train Bcn-Sils         #
# # # # # # # # # # # # # # # # # # # # 


library(tidyverse)
library(tidylog)
library(forcats)
library(RColorBrewer)
library(gridExtra)
library(ggsci)

# All colour brewer palettes
# display.brewer.all()


# # # # # # # # # # 
# Loading data ----
# # # # # # # # # # 
natsurv <- read_csv('Data/PPR Nationwide Data_Sept18_edited_JFP.csv')
glimpse(natsurv)


# Parsing data (using forcats for factors) and recoding when necessary
natsurv2 <- natsurv %>% 
  filter(Gender %in% c(1,2)) %>% 
  mutate(
    Visited_marsh = factor(recode(Visited_marsh, `2` = 0, `1` = 1)),
    Gender = factor(Gender),
    Employment = factor(Employment),
    Member_environ_org = !is.na(Member_environ_org),
    Income = factor(Income),
    Highest_Edu = factor(Highest_Edu),
    Age = factor(Age),
    Abbr_MiniPostcode = abbreviate(MiniPostcode, minlength = 2, strict = T)
    )
  
  # mutate(Income2 = recode_factor(Income, 
  #                        "Don’t know/declined to answer" = 1, 
  #                        "Ddim yn gwybod/ddim eisiau ateb" = 2,
  #                        .default = levels(Income)))

# Should convert to factor most columns

glimpse(natsurv2)


# # # # # # # # # # # # 
# Data exploration ----
# # # # # # # # # # # # 

# Visited a marsh before? ----
natsurv2 %>% 
  na.omit() %>% 
  group_by(Visited_marsh) %>% 
  summarise(n = n())
ggplot(natsurv2) +
  geom_bar(aes(Visited_marsh))

# Rate your knowledge ----
natsurv2 %>% 
  filter(!is.na(Rate_knowledge)) %>% 
  mutate(Rate_knowledge = factor(recode(Rate_knowledge,
                                        `1` = "No knowledge",
                                        `2` = "Basic knowledge",
                                        `3` = "Knowledgeable",
                                        `4` = "Expert knowledge"),
                                 levels = rev(c("No knowledge",
                                            "Basic knowledge",
                                            "Knowledgeable",
                                            "Expert knowledge")))) %>% 
  group_by(Rate_knowledge) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n/sum(n)*100) %>% 
  ggplot(aes(y = percent, x = Rate_knowledge)) +
  geom_bar(aes(fill = Rate_knowledge), stat = 'identity') +
  # geom_text(aes(label = paste(round(percent), "%")), nudge_y = 3) +
  scale_fill_d3(palette = "category10") +
  ylab("% of respondents") +
  xlab("") +
  ggtitle("How would you rate your knowledge of saltmarshes?") +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.6),
        legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))
# ggsave(filename = "Figs&Tables/RateYourKnowledge.pdf")


# # # # # # # # # # # # # # # # 
# Plots AGREE_1 for Nat Survey ----
# # # # # # # # # # # # # # # # 

natsurv2 %>% 
  # select(A_SM_protected:A_SM_impacted_cities) %>% 
  select(A_SM_protected:A_improve_water_qual) %>% 
  gather(key = items, value = answer) %>% 
  mutate(answer = factor(recode(answer, `1` = "Strongly disagree", 
                                `2` = "Disagree", 
                                `3`= "Neither agree or disagree", 
                                `4` = "Agree",
                                `5` = "Strongly agree", 
                                `6` = "Unsure"),
                         levels = rev(c("Strongly disagree", 
                                        "Disagree",
                                        "Unsure",
                                        "Neither agree or disagree",
                                        "Agree",
                                        "Strongly agree"))),
         items = factor(items)) %>% 
  ggplot(aes(x = items)) +
  geom_bar(aes(fill = answer), position = "fill", colour = "black", size = 0.2) +
  # scale_fill_manual(values = c("#9FDA3AFF", "#4AC16DFF", "#1FA187FF", "#277F8EFF", "#365C8DFF", 
  #                              "#46337EFF", "#440154FF")) + 
  scale_fill_manual(values = rev(c("#D73027", "#FC8D59", "#FEE090", "#ffffff", "#91BFDB", "#4575B4"))) +
  scale_x_discrete(labels = c("Changing climates can be positive\nfor coastal areas",
                              "Climate change means there is a need to\nensure salt marshes are well managed",
                              "Salt marshes improve water quality",
                              "Managed realignment has a positive\nimpact on salt marshes",
                              "Salt marshes provide communities\nwith protection from flooding",
                              "Salt marshes are important\nhabitats for wildlife",
                              "Salt marshes are impacted\nby urban development",
                              "Salt marshes are impacted\nby sea level rise",
                              "Salt marshes are important\nfor agriculture",
                              "Salt marshes can help to\nprevent coastal erosion",
                              "Salt marshes are protected\nby existing legislation",
                              "Salt marshes provide food\nand shelter for young fish",
                              "Salt marshes are an under-\nvalued resource",
                              "Salt marsh plants are a\nvaluable resource")) +
  # labs(fill = '', x = NULL, y = NULL, title = 'Please indicate how much you agree with the following statements about salt marshes,\nthe threats they face and their management') +
  ylab("") + 
  xlab("") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        legend.position = "bottom", 
        legend.title = element_blank())
# ggsave(filename = "Figs&Tables/Barplot_Stacked_AgreesQ4&Q5.pdf")


# # # # # # # # # # # # # # # # 
# T-test AGREE_1 for Nat Survey ----
# # # # # # # # # # # # # # # # 


agree1 <- natsurv2 %>% 
  select(A_SM_protected:A_SM_impacted_cities)

# Checking normality of variables
out <- data.frame(item = length(colnames(agree1)), p.value = length(colnames(agree1)))
for(i in 1:length(colnames(agree1))){
  data_now <- agree1 %>% 
    select(i) %>% 
    filter(. != 6)
  data_now <- as.data.frame(data_now)
  shapiro <- shapiro.test(data_now[,1])
  out[i,1] <- colnames(data_now)
  out[i,2] <- shapiro$p.value
}
# All of the response variables are non-normal

# Instead of t-test we must do Wilcoxon tests
testimates <- NULL
tconfint1 <- NULL
tconfint2 <- NULL
unsuretot <- NULL
pvalue <- NULL
for(i in 1:length(colnames(agree1))){
  data_now <- agree1 %>% 
    select(i) %>% 
    filter(. != 6)
  data_now <- as.data.frame(data_now)
  unsure <- 1-(nrow(data_now)/nrow(agree1))
  tstudent <- wilcox.test(data_now[,1], mu = 3, conf.int = T)
  testimates <- c(testimates, tstudent$estimate)
  tconfint1 <- c(tconfint1, tstudent$conf.int[1])
  tconfint2 <- c(tconfint2, tstudent$conf.int[2])
  unsuretot <- c(unsuretot, unsure)
  pvalue <- c(pvalue, tstudent$p.value)
}
names(testimates) <- colnames(agree1)
names(tconfint1) <- colnames(tconfint1)
names(tconfint2) <- colnames(tconfint2)
names(unsuretot) <- colnames(unsuretot)
names(pvalue) <- colnames(pvalue)

tests_agree1 <- data.frame(testimates, tconfint1, tconfint2, unsuretot, pvalue)
tests_agree1$x <- rownames(tests_agree1)
tests_agree1$unsuretot <- round(tests_agree1$unsuretot*100)
tests_agree1

p1 <- ggplot(data = tests_agree1, aes(x = x, y = testimates)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = tconfint1, ymax = tconfint2), width=0, position = position_dodge(.9)) +
  scale_y_continuous(breaks = 1:5, 
                     labels = c("Strongly\ndisagree",
                                "Disagree",
                                "Neither agree\nor disagree",
                                "Agree",
                                "Strongly\nagree"), 
                     limits = c(1,5)) +
  scale_x_discrete(labels = c("Changing climates can be positive\nfor coastal areas",
                              "Climate change means there is a need to\nensure salt marshes are well managed",
                              "Managed realignment has a positive\nimpact on salt marshes",
                              "Salt marshes are impacted\nby urban development",
                              "Salt marshes are impacted\nby sea level rise",
                              "Salt marshes are protected\nby existing legislation",
                              "Salt marshes are an under-\nvalued resource")) +
  labs(fill = '', x = NULL, y = NULL, title = '') +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(data = tests_agree1, aes(x = x, y = unsuretot)) +
  # geom_point(size = 2) +
  geom_bar(stat = "identity") +
  ylim(c(0,50)) +
  scale_x_discrete(labels = c("",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "")) +
  labs(fill = '', x = NULL, y = '% unsure', title = '') +
  coord_flip() +
  theme(plot.margin = unit(c(5.5, 6, 35, 0), "pt")) #top, right, bottom, left

t <- grid.arrange(p1, p2, widths = c(3,1), nrow = 1)

# ggsave("Figs&Tables/WilcoxTestAgree_Q4.pdf", plot = t)



# # # # # # # # # # # # # # # # 
# Plots AGREE_2 for Nat Survey ----
# # # # # # # # # # # # # # # # 

natsurv2 %>% 
  select(A_SM_important_agric:A_improve_water_qual) %>% 
  gather(key = items, value = answer) %>% 
  mutate(answer = factor(recode(answer, `1` = "Strongly disagree", 
                                `2` = "Disagree", 
                                `3`= "Neither agree or disagree", 
                                `4` = "Agree",
                                `5` = "Strongly agree", 
                                `6` = "Unsure"),
                         levels = rev(c("Strongly disagree", 
                                        "Disagree",
                                        "Unsure",
                                        "Neither agree or disagree",
                                        "Agree",
                                        "Strongly agree"))),
         items = factor(items)) %>% 
  ggplot(aes(x = items)) +
  geom_bar(aes(fill = answer), position = "fill", colour = "black", size = 0.2) +
  # scale_fill_manual(values = c("#9FDA3AFF", "#4AC16DFF", "#1FA187FF", "#277F8EFF", "#365C8DFF", 
  #                              "#46337EFF", "#440154FF")) + 
  scale_fill_manual(values = rev(c("#D73027", "#FC8D59", "#FEE090", "#ffffff", "#91BFDB", "#4575B4"))) +
  scale_x_discrete(labels = c("Salt marshes improve water quality",
                              "Salt marshes provide communities\nwith protection from flooding",
                              "Salt marshes are important\nhabitats for wildlife",
                              "Salt marshes are important\nfor agriculture",
                              "Salt marshes can help to\nprevent coastal erosion",
                              "Salt marshes provide food\nand shelter for young fish",
                              "Salt marsh plants are a\nvaluable resource")) +
  labs(fill = '', x = NULL, y = NULL, title = 'Please indicate how much you agree with the following statements\nabout coastal marshes and their contribution to society') +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.6),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))

# ggsave(filename = "Figs&Tables/Barplot_Stacked_Agrees2.pdf")

                              
# # # # # # # # # # # # # # # # 
# T-test AGREE_2 for Nat Survey ----
# # # # # # # # # # # # # # # # 

agree2 <- natsurv2 %>% 
  select(A_SM_important_agric:A_improve_water_qual)

# Checking normality of variables
out <- data.frame(item = length(colnames(agree2)), p.value = length(colnames(agree2)))
for(i in 1:length(colnames(agree2))){
  data_now <- agree2 %>% 
    select(i) %>% 
    filter(. != 6)
  data_now <- as.data.frame(data_now)
  shapiro <- shapiro.test(data_now[,1])
  out[i,1] <- colnames(data_now)
  out[i,2] <- shapiro$p.value
}
# All of the response variables are non-normal

# Instead of t-test we must do Wilcoxon tests
testimates <- NULL
tconfint1 <- NULL
tconfint2 <- NULL
unsuretot <- NULL
for(i in 1:length(colnames(agree2))){
  data_now <- agree2 %>% 
    select(i) %>% 
    filter(. != 6)
  data_now <- as.data.frame(data_now)
  unsure <- 1-(nrow(data_now)/nrow(agree2))
  tstudent <- wilcox.test(data_now[,1], mu = 3, conf.int = T)
  testimates <- c(testimates, tstudent$estimate)
  tconfint1 <- c(tconfint1, tstudent$conf.int[1])
  tconfint2 <- c(tconfint2, tstudent$conf.int[2])
  unsuretot <- c(unsuretot, unsure)
}
names(testimates) <- colnames(agree2)
names(tconfint1) <- colnames(tconfint1)
names(tconfint2) <- colnames(tconfint2)
names(unsuretot) <- colnames(unsuretot)

tests_agree2 <- data.frame(testimates, tconfint1, tconfint2, unsuretot)
tests_agree2$x <- rownames(tests_agree2)
tests_agree2$unsuretot <- round(tests_agree2$unsuretot*100)
tests_agree2

p1 <- ggplot(data = tests_agree2, aes(x = x, y = testimates)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = tconfint1, ymax = tconfint2), width=0, position = position_dodge(.9)) +
  scale_y_continuous(breaks = 1:5, 
                     labels = c("Strongly\ndisagree",
                                "Disagree",
                                "Neither agree\nor disagree",
                                "Agree",
                                "Strongly\nagree"), 
                     limits = c(1,5)) +
  scale_x_discrete(labels = c("Salt marshes improve water quality",
                              "Salt marshes provide communities\nwith protection from flooding",
                              "Salt marshes are important\nhabitats for wildlife",
                              "Salt marshes are important\nfor agriculture",
                              "Salt marshes can help to\nprevent coastal erosion",
                              "Salt marshes provide food\nand shelter for young fish",
                              "Salt marsh plants are a\nvaluable resource")) +
  labs(fill = '', x = NULL, y = NULL, title = '') +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(data = tests_agree2, aes(x = x, y = unsuretot)) +
  # geom_point(size = 2) +
  geom_bar(stat = "identity") +
  ylim(c(0,50)) +
  scale_x_discrete(labels = c("",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "")) +
  labs(fill = '', x = NULL, y = '% unsure', title = '') +
  coord_flip() +
  theme(plot.margin = unit(c(5.5, 6, 35, 0), "pt")) #top, right, bottom, left

t2 <- grid.arrange(p1, p2, widths = c(3,1), nrow = 1)
# ggsave("Figs&Tables/WilcoxTestAgree_Q5.pdf", plot = t2)


# # # # # # # # # # # # # # # # 
# Plots INDICATE for Nat Survey ----
# # # # # # # # # # # # # # # # 

natsurv2 %>% 
  select(starts_with("I_")) %>% 
  gather(key = items, value = answer) %>% 
  mutate(answer = factor(recode(answer, `1` = "No benefit", 
                                `2` = "Slight benefit", 
                                `3`= "Somewhat beneficial", 
                                `4` = "Moderately beneficial",
                                `5` = "Very beneficial", 
                                `6` = "Unsure"),
                         levels = rev(c("No benefit", 
                                        "Unsure",
                                        "Slight benefit",
                                        "Somewhat beneficial",
                                        "Moderately beneficial",
                                        "Very beneficial"))),
         items = factor(items)) %>% 
  ggplot(aes(x = items)) +
  geom_bar(aes(fill = answer), position = "fill", colour = "black", size = 0.2) +
  # scale_fill_manual(values = c("#9FDA3AFF", "#4AC16DFF", "#1FA187FF", "#277F8EFF", "#365C8DFF", 
  #                              "#46337EFF", "#440154FF")) + 
  scale_fill_manual(values = rev(c("#D73027", "#FEE090", "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4"))) +
  scale_x_discrete(labels = c("Agricultural land",
                              "Coastal protection\nfrom flooding",
                              "Storage of carbon dioxide",
                              "Provision of wild food",
                              "Habitats for wildlife",
                              "Natural landscape",
                              "Reducing climate\nchange impacts",
                              "Nursery habitats\nfor fisheries",
                              "Pollination",
                              "Prevention of\ncoastal erosion",
                              "Recreation\ne.g. birdwatching",
                              "Tourism",
                              "Reducing impacts\nof waste and pollution",
                              "Health and wellbeing")) +
  labs(fill = '', x = NULL, y = NULL, title = 'Please indicate the importance of these benefits\nand services provided by salt marshes to society') +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        legend.position = "bottom", 
        legend.title = element_blank())

# ggsave(filename = "Figs&Tables/Barplot_Stacked_Indicate.pdf")



# # # # # # # # # # # # # # # # 
# T-test INDICATE for Nat Survey ----
# # # # # # # # # # # # # # # # 

indicate <- natsurv2 %>% 
  select(starts_with("I_"))

# Checking normality of variables
out <- data.frame(item = length(colnames(indicate)), p.value = length(colnames(indicate)))
for(i in 1:length(colnames(indicate))){
  data_now <- indicate %>% 
    select(i) %>% 
    filter(. != 6)
  data_now <- as.data.frame(data_now)
  shapiro <- shapiro.test(data_now[,1])
  out[i,1] <- colnames(data_now)
  out[i,2] <- shapiro$p.value
}
# All of the response variables are non-normal

# Instead of t-test we must do Wilcoxon tests
testimates <- NULL
tconfint1 <- NULL
tconfint2 <- NULL
unsuretot <- NULL
pvalues <- NULL
for(i in 1:length(colnames(indicate))){
  data_now <- indicate %>% 
    select(i) %>% 
    filter(. != 6)
  data_now <- as.data.frame(data_now)
  unsure <- 1-(nrow(data_now)/nrow(indicate))
  tstudent <- wilcox.test(data_now[,1], mu = 3, conf.int = T)
  testimates <- c(testimates, tstudent$estimate)
  tconfint1 <- c(tconfint1, tstudent$conf.int[1])
  tconfint2 <- c(tconfint2, tstudent$conf.int[2])
  pvalues <- c(pvalues, tstudent$p.value)
  unsuretot <- c(unsuretot, unsure)
}
names(testimates) <- colnames(indicate)
names(tconfint1) <- colnames(tconfint1)
names(tconfint2) <- colnames(tconfint2)
names(unsuretot) <- colnames(unsuretot)
names(pvalues) <- colnames(pvalues)

tests_indicate <- data.frame(testimates, tconfint1, tconfint2, unsuretot, pvalues)
tests_indicate$x <- rownames(tests_indicate)
tests_indicate$unsuretot <- round(tests_indicate$unsuretot*100)
tests_indicate

p1 <- ggplot(data = tests_indicate, aes(x = x, y = testimates)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = tconfint1, ymax = tconfint2), width=0, position = position_dodge(.9)) +
  scale_y_continuous(breaks = 1:5, 
                     labels = c("No benefit", 
                                "Slight\nbenefit",
                                "Somewhat\nbeneficial",
                                "Moderately\nbeneficial",
                                "Very\nbeneficial"), 
                     limits = c(1,5)) +
  scale_x_discrete(labels = c("Agricultural land",
                              "Coastal protection\nfrom flooding",
                              "Storage of carbon",
                              "Provision of wild food",
                              "Habitats for wildlife",
                              "Natural landscape",
                              "Reducing climate\nchange impacts",
                              "Nursery habitats\nfor fisheries",
                              "Pollination",
                              "Prevention of\ncoastal erosion",
                              "Recreation\ne.g. birdwatching",
                              "Tourism",
                              "Reducing impacts\nof waste and pollution",
                              "Health and wellbeing")) +
  labs(fill = '', x = NULL, y = NULL, title = '') +
  coord_flip() + 
  theme(text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(data = tests_indicate, aes(x = x, y = unsuretot)) +
  # geom_point(size = 2) +
  geom_bar(stat = "identity") +
  ylim(c(0,50)) +
  scale_x_discrete(labels = c("",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "")) +
  labs(fill = '', x = NULL, y = '% unsure', title = '') +
  coord_flip() +
  theme(text = element_text(size = 13),
        plot.margin = unit(c(5.5, 6, 27, 0), "pt")) #top, right, bottom, left

t3 <- grid.arrange(p1, p2, widths = c(3,1), nrow = 1)
# ggsave("Figs&Tables/WilcoxTestIndicate_Q6.pdf", plot = t3)






##########

# # # # # # # # # # # # # # # # 
# T-test AGREE_ALL for Nat Survey ----
# # # # # # # # # # # # # # # # 


agree1 <- natsurv2 %>% 
  select(A_SM_protected:A_improve_water_qual)

testimates <- NULL
tconfint1 <- NULL
tconfint2 <- NULL
unsuretot <- NULL
for(i in 1:length(colnames(agree1))){
  data_now <- agree1 %>% 
    select(i) %>% 
    filter(. != 6)
  data_now <- as.data.frame(data_now)
  unsure <- 1-(nrow(data_now)/nrow(agree1))
  tstudent <- wilcox.test(data_now[,1], mu = 3, conf.int = T)
  testimates <- c(testimates, tstudent$estimate)
  tconfint1 <- c(tconfint1, tstudent$conf.int[1])
  tconfint2 <- c(tconfint2, tstudent$conf.int[2])
  unsuretot <- c(unsuretot, unsure)
}
names(testimates) <- colnames(agree1)
names(tconfint1) <- colnames(tconfint1)
names(tconfint2) <- colnames(tconfint2)
names(unsuretot) <- colnames(unsuretot)

tests_agree1 <- data.frame(testimates, tconfint1, tconfint2, unsuretot)
tests_agree1$x <- rownames(tests_agree1)
tests_agree1$unsuretot <- round(tests_agree1$unsuretot*100)
tests_agree1

p1 <- ggplot(data = tests_agree1, aes(x = x, y = testimates)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = tconfint1, ymax = tconfint2), width=0, position = position_dodge(.9)) +
  scale_y_continuous(breaks = 1:5, 
                     labels = c("Strongly\ndisagree",
                                "Disagree",
                                "Neither agree\nor disagree",
                                "Agree",
                                "Strongly\nagree"), 
                     limits = c(1,5)) +
  scale_x_discrete(labels = c("Changing climates can be positive\nfor coastal areas",
                              "Climate change means there is a need to\nensure salt marshes are well managed",
                              "Salt marshes improve water quality",
                              "Managed realignment has a positive\nimpact on salt marshes",
                              "Salt marshes provide communities\nwith protection from flooding",
                              "Salt marshes are important\nhabitats for wildlife",
                              "Salt marshes are impacted\nby urban development",
                              "Salt marshes are impacted\nby sea level rise",
                              "Salt marshes are important\nfor agriculture",
                              "Salt marshes can help to\nprevent coastal erosion",
                              "Salt marshes are protected\nby existing legislation",
                              "Salt marshes provide food\nand shelter for young fish",
                              "Salt marshes are an under-\nvalued resource",
                              "Salt marsh plants are a\nvaluable resource")) +
  labs(fill = '', x = NULL, y = NULL, title = '') +
  coord_flip() + 
  theme(text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(data = tests_agree1, aes(x = x, y = unsuretot)) +
  # geom_point(size = 2) +
  geom_bar(stat = "identity") +
  ylim(c(0,50)) +
  scale_x_discrete(labels = c("",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "")) +
  labs(fill = '', x = NULL, y = '% unsure', title = '') +
  coord_flip() +
  theme(text = element_text(size = 13),
        plot.margin = unit(c(5.5, 6, 35, 0), "pt")) #top, right, bottom, left

t <- grid.arrange(p1, p2, widths = c(3,1), nrow = 1)

# ggsave("Figs&Tables/WilcoxTestALLAgree(Q4+Q5).pdf", plot = t)








