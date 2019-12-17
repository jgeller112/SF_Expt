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


ground <- qualtRics::read_survey("SF_ground_water_december14th.csv")

#data was collected until the last day of the fall semester 2019 Decemeber13th. 

#get manipulation check question to merge with scored data. 
ground_native <- ground %>% 
  dplyr::select(ResponseId,Finished, Progress, Q192, Q145, Q377)

#read in scored data
scored_gound_sf<-read_csv("scored_ground_sf.csv")

scored_merged <- merge(scored_ground_sf, ground_native)

#all
glmer_acc_all <- scored_merged %>% dplyr::filter(Q192=="Ground water" & Q377 >17)

tokens <- unnest_tokens(tbl = glmer_acc_all, output = token, input = response)
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
tokens <- unnest_tokens(tbl = glmer_acc_all, output = token,
                        input = response, token = stringr::str_split,
                        pattern = " |\\, |\\.|\\,|\\;")

#with native english speakers
glmer_acc_native <- scored_merged %>% dplyr::filter(Q145=="Yes", Q192=="Ground water") # error with qualtrics experiment and it did not display when Ps learned English so could not get accurate english prof profile for Ps.

scored_ground_plot<- glmer_acc_all %>% 
  group_by(ResponseId, FL_149_DO) %>% 
  rename(Passage="FL_149_DO") %>% 
  summarise(acc=mean(acc)) %>% 
  ungroup() %>% 
  mutate(Passage=ifelse(Passage=="Passage", "SF", Passage))

scored_ground_aov<- glmer_acc_all %>% 
  dplyr::group_by(ResponseId, FL_149_DO) %>% 
  dplyr::rename(Passage="FL_149_DO") %>% 
  dplyr::summarise(acc=mean(auto_spell_acc)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Passagetype=ifelse(Passage=="Passage", "SF", Passage))


a1 <- aov_ez("ResponseId", "acc", scored_ground_aov, 
             between = c("Passage")) # one way

#plot the results

kable(nice(a1))


ls1 <- emmeans(a1, specs = "FL_149_DO") # get the simple effects test for signifcant interaction. 

flex1=pairs(ls1)

kable(flex1)


source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

g <- 
  ggplot(data = scored_ground_aov, 
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
