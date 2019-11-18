
# # # # # # # # # # # # # # # # 
# Plots AGREE_1 for Nat Survey ----
# # # # # # # # # # # # # # # # 
library(cowplot)
p1 <- natsurv2 %>% 
  # select(A_SM_protected:A_SM_impacted_cities) %>% 
  select(A_SM_protected:A_improve_water_qual) %>% 
  gather(key = items, value = answer) %>% 
  filter(answer != 6) %>% 
  mutate(answer = factor(recode(answer, `1` = "Strongly disagree", 
                                `2` = "Disagree", 
                                `3`= "Neither agree or disagree", 
                                `4` = "Agree",
                                `5` = "Strongly agree"),
                         levels = rev(c("Strongly disagree", 
                                        "Disagree",
                                        "Neither agree or disagree",
                                        "Agree",
                                        "Strongly agree"))),
         items = factor(items)) %>% 
  ggplot(aes(x = items)) +
  geom_bar(aes(fill = answer), position = "fill", colour = "black", size = 0.2) +
  # scale_fill_manual(values = c("#9FDA3AFF", "#4AC16DFF", "#1FA187FF", "#277F8EFF", "#365C8DFF", 
  #                              "#46337EFF", "#440154FF")) + 
  scale_fill_manual(values = rev(c("#D73027", "#FC8D59", "#ffffff", "#91BFDB", "#4575B4"))) +
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
  scale_y_continuous(labels = scales::percent_format()) +
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
        legend.position = "none", 
        legend.title = element_blank())


p2 <- natsurv2 %>% 
  select(A_SM_protected:A_improve_water_qual) %>% 
  gather(key = items, value = answer) %>% 
  mutate(answer = factor(recode(answer, `1` = "Strongly disagree", 
                                `2` = "Disagree", 
                                `3`= "Neither agree or disagree", 
                                `4` = "Agree",
                                `5` = "Strongly agree",
                                `6` = "Unsure")
                         ),
         items = factor(items)) %>% 
  group_by(items, answer) %>% 
  summarise(n = n()) %>% 
  mutate(percent = (n/sum(n))) %>% 
  filter(answer == "Unsure") %>% 
  ggplot() +
  geom_bar(aes(x = items, y = percent), stat = "identity", fill = "#FEE090", colour = "black", size = 0.2) + 
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
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(fill = '', x = NULL, y = '% unsure', title = '') +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        legend.position = "none", 
        legend.title = element_blank())
  
plot_grid(p1, p2, rel_widths = c(4, 1), align = "h")
# ggsave2(filename = "Figs&Tables/Barplot_Stacked_AgreesQ4&Q5_unsureSeparated.pdf")


# # # # # # # # # # # # # # # # 
# Plots AGREE_1 for Nat Survey ----
# # # # # # # # # # # # # # # # 

p1 <- natsurv2 %>% 
  select(starts_with("I_")) %>% 
  gather(key = items, value = answer) %>% 
  filter(answer != 6) %>% 
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
  scale_fill_manual(values = rev(c("#D73027", "#FC8D59", "#ffffff", "#91BFDB", "#4575B4"))) +
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
  scale_y_continuous(labels = scales::percent_format()) +
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
        legend.position = "none", 
        legend.title = element_blank())


p2 <- natsurv2 %>% 
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
  group_by(items, answer) %>% 
  summarise(n = n()) %>% 
  mutate(percent = (n/sum(n))) %>% 
  filter(answer == "Unsure") %>% 
  ggplot() +
  geom_bar(aes(x = items, y = percent), stat = "identity", fill = "#FEE090", colour = "black", size = 0.2) + 
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
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(fill = '', x = NULL, y = '% unsure', title = '') +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        legend.position = "none", 
        legend.title = element_blank())

plot_grid(p1, p2, rel_widths = c(4, 1), align = "h")
# ggsave2(filename = "Figs&Tables/Barplot_Stacked_Indicate_unsureSeparated.pdf")




