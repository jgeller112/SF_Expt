install.packages("qualtRics")
install.packages("tidyverse")
install.packages("afex")
install.packages("emmeans")
install.packages("hunspell")
install.packages("tidytext")
install.packages("brms")
install.packages("bayestestR")

library(qualtRics)
library(tidyverse)
library(afex)
library(emmeans)
library(here)
library(bayestestR)
library(brms)





#spelling
library(hunspell)
library(tidytext)
library(stringi)

here()

set.seed(456)

ground <- qualtRics::read_survey("memory_for_words_final.csv")

#data was collected until the last day of the fall semester 2019 Decemeber13th. 

#get manipulation check question to merge with scored data. 
ground_native <-dplyr::select(ground, ResponseId,Finished, Progress, Q163_1, Q164, FL_149_DO, Q192, Q145, Q434, Q435, Q436, Q437, Q438, Q439, Q440, Q441, Q442, Q444)
  
q433<- read_csv("Q433.csv")

ground_native<-full_join(ground_native, q433)


ground_native <- pivot_longer(ground_native,cols=Q434:Q443, names_to="Question", values_to="Response") %>%
  dplyr::filter(Q192=="Ground water", Progress==100, Q145=="Yes")
  


question<-read_csv("question_response.csv") # read in correct responses 

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


# Parse features
tokens <- unnest_tokens(tbl = ground_native_question, output = token,
                        input = Response, token = stringr::str_split,
                        pattern = " |\\, |\\.|\\,|\\;")

tokens$auto_acc <- ifelse(tokens$Correct==tokens$token, 1, 0)

tokens[is.na(tokens)] <- 0 #change all NAs to 0 

write.csv(tokens, file="memory_acc_gw_final.csv")


ground <- qualtRics::read_survey(here("Expt2_Data", "memory_acc_gw_final.csv"))

ground_change <- ground %>%
  mutate(Passage=ifelse(FL_149_DO=="Highlight", "Pre-highlighted", ifelse(FL_149_DO=="Passage", "Sans Forgetica", "Unmodified")))

ground_change_agg<-ground_change %>%
  group_by(ResponseId, Passage) %>%
  summarise(mean_acc=mean(auto_acc))

ground_change_agg$Passage <- factor(ground_change_agg$Passage, level=c("Pre-highlighted", "Unmodified", "Sans Forgetica"))     

#data was collected until the last day of the fall semester 2019 Decemeber13th. 
# loading needed libraries
full_model=glmer(auto_acc~Passage+(1|ResponseId) + (1|Question), data=ground_change, family="binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))


prior<-prior(normal(0,1), class="b") # weakly informed prior on the coefficents

#fit the brms model
groundbrms=brm(auto_acc~Passage+(1|ResponseId) + (1|Question), data=ground_change, family=bernoulli(link="logit"), prior=prior, sample_prior = TRUE,  cores = 4) 


#get BF for interaction which is difference in dprime from brms model
SansvsUnmod=hypothesis(groundbrms, 'PassageSF + PasageUnmodified = 0')
#get means and CIs
means<-estimate_means(full_model)

ef1 <- effect("Passage", full_model) #take final glmer model 
x1 <- as.data.frame(ef1)
#plot
p1<- ggplot(ground_change_agg, aes(Passage, mean_acc, fill=Passage))+
  geom_violin() + 
  geom_jitter2(width=0.11, alpha=.5)+ 
  geom_line(data=means,aes(y=Probability, group=1), size=1)+ 
  geom_pointrange(data=means, aes(y=Probability, ymin=CI_low, ymax=CI_high), size=1, color="white")+ 
  theme_bw(base_size=14)+
  labs(y="Proportion Recalled on Test", x="Passage Type") + 
  theme(legend.position = "none") + 
  ggplot2::coord_cartesian(ylim = c(0, 1)) + 
  theme(axis.text=bold) 



