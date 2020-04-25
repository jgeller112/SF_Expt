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

full_model=glmer(auto_acc~FL_149_DO+(1|ResponseId) + (1|Question), data=tokens, family="binomial")
#fit full model

ef1 <- effect("FL_149_DO", full_model) #take final glmer model 
summary(ef1)
x1 <- as.data.frame(ef1)

bold <- element_text(face = "bold", color = "black", size = 14) #axis bold
p<- ggplot(x1, aes(FL_149_DO, fit, fill=FL_149_DO))+ 
  geom_bar(stat="identity", position="dodge") + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2, position=position_dodge(width=0.9),color="red") + theme_bw(base_size=14)+labs(y="", x="Passage Type") + 
  scale_fill_manual(values=c("grey", "black", "yellow"))+
  theme(axis.text=bold, legend.position = "none") + ggplot2::coord_cartesian(ylim = c(0, 1))




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



#Bayesian

dis=brm(acc~condition*dis+ (1+dis|ResponseID)+(1+condition*dis|target), data=gen, family=bernoulli(), prior=prior1, sample_prior=TRUE)

model ran

```
c_color_main <- pairs(emmeans(dis, ~ dis))

c_color_cond <- pairs(emmeans(dis, ~ condition))

em_color_simple<-emmeans(dis, ~dis*condition)


pairs(em_color_simple, by = "condition") #

c_color_all <- rbind(c_color_main,color_cond,
                     c_color_shape_interaction)
c_color_shape_interaction <- contrast(em_color_simple, interaction = c("pairwise","pairwise"))

bayestestR::describe_posterior(c_color_all,
                               estimate = "median", dispersion = TRUE,
                               ci = .9, ci_method = "hdi",
                               test = c("bayesfactor"),
                               bf_prior = dis)

# get the BF for model with and without interaction. Wealk informed prior Gleman


