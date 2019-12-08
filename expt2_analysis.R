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


ground_data <- qualtRics::read_survey("expt2.csv")

ground <- ground_data %>% 
  dplyr::select(ResponseId,Finished, Progress, Q192, Q434, Q435, Q436, Q437, Q438, Q439, Q440, Q441, Q442, Q443, Q444, FL_149_DO) %>% 
  dplyr::filter(Finished==TRUE | Progress==99)


