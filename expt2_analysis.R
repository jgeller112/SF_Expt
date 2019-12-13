install.packages("qualtRics")
install.packages("tidyverse")
install.packages("afex")
install.packages("emmeans")
install.packages("ggbeeswarm")

library(qualtRics)
library(tidyverse)
library(afex)
library(emmeans)
library(here)
library(ggpol)

Here()

ground <- qualtRics::read_survey("december13.csv")

ground_native <- ground %>% 
  dplyr::select(ResponseId,Finished, Progress, Q145, Q192, Q434, Q435, Q436, Q437, Q438, Q439, Q440, Q441, Q442, Q443, Q444, FL_149_DO) %>% 
  dplyr::filter(Finished==TRUE | Progress==99 | Q145=="Yes") %>%
  dplyr::select(-Q145) 

# see how many Ps thrown out due to not finishing and not being native English speakers

ground_acc <- ground_native %>% 
  dplyr::select(ResponseId,Finished, Progress, Q145, Q192, Q434, Q435, Q436, Q437, Q438, Q439, Q440, Q441, Q442, Q443, Q444, FL_149_DO) %>% 
  dplyr::filter(Finished==TRUE | Progress==99 | Q145=="Yes") %>%
  dplyr::select(-Q145)%>%
  tidyr::pivot_longer(cols = starts_with("Q"), names_to="question", values_to = "response") %>%
  arrange(question)
# data to get recall accuracy

scored_data<-read_csv(gg.csv) # read in scored data 
# get data rad for ANOVA and clean up vars
scored_ground_aov<- scored_data %>% 
  group_by(ResponseId, FL_149_DO) %>% 
  rename(Passage="FL_149_DO") %>% 
  summarise(acc=mean(acc)) %>% 
  ungroup() %>% 
  mutate(Passage=ifelse(Passage=="Passage", "SF", Passage))

#AOV 
a1 <- aov_ez("ResponseId", "acc", scored_ground_aov, 
             between = c("Passage")) # 2 X 2 Mixed ANOVA

er=glmer(mean ~FL_149_DO + (1|ResponseId), data=gg, family="binomial")
#plot the results
ls1 <- emmeans(a1, specs="Passage") # get the simple effects test for signifcant interaction. 
kable(nice(a1))
pairs(ls1)
af1=afex_plot(a1, x = "Passage", 
               data_geom=ggplot2::geom_violin, mapping = c("color", "fill"))


po2 <- afex_plot(a1, x = "Passage", error = "between", 
                 data_geom = ggpol::geom_boxjitter, 
                 mapping = "fill", data_alpha = 0.7, 
                 data_arg = list(
                   width = 0.6, 
                   jitter.width = 0.05,
                   jitter.height = 10,
                   outlier.intersect = TRUE
                 ))
