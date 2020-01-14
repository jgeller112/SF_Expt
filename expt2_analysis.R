install.packages("qualtRics")
install.packages("tidyverse")
install.packages("afex")
install.packages("emmeans")
install.packages("hunspell")
install.packages("tidytext")

library(qualtRics)
library(tidyverse)
library(afex)
library(emmeans)
library(here)
library(ggpol)



#spelling
library(hunspell)
library(tidytext)
library(stringi)

here()

set.seed(456)

ground <- qualtRics::read_survey("ground_water_memory01202.csv")

#data was collected until the last day of the fall semester 2019 Decemeber13th. 

#get manipulation check question to merge with scored data. 
ground_native <- ground %>% 
  dplyr::select(ResponseId,Finished, Progress, Q163_1, Q164, FL_149_DO, Q192, Q145, Q434, Q435, Q436, Q437, Q438, Q439, Q440, Q441, Q442, Q433, Q444) %>%
  
ground_native <- pivot_longer(ground_native,cols=Q434:Q444, names_to="Question", values_to="Response") %>%
  dplyr::filter(Q192=="Ground water", Progress==100, Q145=="Yes")
  
ground_native <- pivot_longer(ground_native,cols=Q434:Q444, names_to="Question", values_to="Response") %>%
  dplyr::filter(Q192=="Ground water", Progress==100)



question<-read.csv("question_reponse.csv") # read in correct responses 

ground_native_question<-dplyr::left_join(ground_native, question) #merge

tokens <- unnest_tokens(tbl = ground_native_question, output = token, input = Response)
wordlist <- unique(tokens$token)

# Spell check the words
spelling.errors <- hunspell(wordlist)
spelling.errors <- unique(unlist(spelling.errors))
spelling.sugg <- hunspell_suggest(spelling.errors, dict = dictionary("en_US"))

# Pick the first suggestion
spelling.sugg <- unlist(lapply(spelling.sugg, function(x) x[1]))
spelling.dict <- as.data.frame(cbind(spelling.errors,spelling.sugg))
spelling.dict$spelling.pattern <- paste0("\\b", spelling.dict$spelling.errors, "\\b")
# Write out spelling dictionary


write.csv(x = spelling.dict, file = "../output_data/spelling.dict.csv",
          fileEncoding = "utf8", row.names = F)

# Parse features
tokens <- unnest_tokens(tbl = ground_native_question, output = token,
                        input = response, token = stringr::str_split,
                        pattern = " |\\, |\\.|\\,|\\;")

tokens$auto_acc <- ifelse(tokens$Correct==tokens$token, 1, 0)

tokens[is.na(tokens)] <- 0 #change all NAs to 0 

scored_ground_plot<- tt %>% 
  group_by(ResponseId, FL_149_DO) %>% 
  rename(Passage="FL_149_DO") %>% 
  summarise(acc=mean(acc)) %>% 
  ungroup() %>% 
  mutate(Passage=ifelse(Passage=="Passage", "SF", Passage))

scored_ground_aov<- replace %>% 
  dplyr::group_by(ResponseId, FL_149_DO) %>% 
  dplyr::rename(Passage="FL_149_DO") %>% 
  dplyr::summarise(acc=mean(auto_acc)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Passagetype=ifelse(Passage=="Passage", "SF", Passage))


a1 <- aov_ez("ResponseId", "acc", scored_ground_aov, 
             between = c("Passage")) # one way

#plot the results

kable(nice(a1))

ls1 <- emmeans(a1, specs = "Passage") # get the simple effects test for signifcant interaction. 

flex1=pairs(ls1)

kable(flex1)


source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

g <- 
  ggplot(data = repalce, 
         aes(x = Passage, y = acc, fill = Passage)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = acc, color = Passage), 
             position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 4.00) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() + # flip or not
  theme_bw() + labs(x="Passage Type", y="Accuracy") + ggtitle("Passage Performance Across Manipualtion Type")
  raincloud_theme

g

ggsave("SF_raincloud.png", width=8, height=4, dpi=500, type="cairo")
