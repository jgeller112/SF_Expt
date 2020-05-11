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
library(Rmisc)
library(BayesFactor)
library(lrd)



#spelling


library(lrd)


here()

set.seed(456)

ground <- qualtRics::read_survey(here::here("Expt2_data", "memory_for_words_final.csv"))

#data was collected until the last day of the fall semester 2019 Decemeber13th. 

#get attention check question to merge with scored data. 
ground_native <-dplyr::select(ground, ResponseId,Finished, Progress, Q163_1, Q164, FL_149_DO, Q192, Q145, Q434, Q435, Q436, Q437, Q438, Q439, Q440, Q441, Q442, Q444)
  
q433<- read_csv(here::here("Expt2_data", "Q433.csv"))

ground_native<-full_join(ground_native, q433)

# how many did not meet criteria
ground_native %>%
dplyr::filter(Q192=="Ground water", Progress==100, Q145=="Yes")

ground_native <- pivot_longer(ground_native,cols=Q434:Q443, names_to="Question", values_to="Response") %>%
  dplyr::filter(Q192=="Ground water", Progress==100, Q145=="Yes")
  


question<-read_csv(here::here("Expt2_data", "question_response.csv")) # read in correct responses 

ground_native_question<-dplyr::left_join(ground_native, question) #merge

# Compute percent match automatic scoring

matched = percent_match(ground_native_question$Reponse, key = ground_native_question$Correct, id = ground_native_question$ResponseId)

score_recall(matched, set.cutoff = .65) 

ground<-qualtRics::read_survey(here("Expt2_Data", "final_scored_acc"))

ground_change <- ground %>%
  mutate(Passage=ifelse(FL_149_DO=="Highlight", "Pre-highlighted", ifelse(FL_149_DO=="Passage", "Sans Forgetica", "Unmodified")))

ground_change_agg<-ground_change %>%
  dplyr::group_by(id, Passage) %>%
  dplyr::summarise(mean_acc=mean(Scored))

#Classic ANOVA

a1 <- aov_ez("id", "mean_acc", ground_change_agg, 
             between = c("Passage")) # one way

ls1 <- emmeans(a1, specs = "Passage") # get the simple effects test for signifcant interaction. 



flex1=pairs(ls1)

#effsexp2=eff_size(flex1, sigma = sigma(a1$lm), edf = 525, method = "identity")

ls1<-as.data.frame(ls1)

ground_change_agg$Passage <- factor(ground_change_agg$Passage, level=c("Pre-highlighted", "Unmodified", "Sans Forgetica"))     

#bf1<- ground_change_agg %>%
#filter(Passage!="Pre-highlighted")

#bf2<-ground_change_agg %>%
#filter(Passage!="Unmodified")

#bf = ttestBF(formula = mean_acc ~ filter(Passage!="Pre-highlighted"), data = bf1)
#bf


#GLMER model
#data was collected until the last day of the fall semester 2019 Decemeber13th. 
# loading needed libraries
#full_model=glmer(auto_acc~Passage+(1|ResponseId) + (1|Question), data=ground_change, family="binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
#fit full model

#m2<-read_rds(here("Brms", "brms_gw.rds.gz"))

#c_main <- emmeans(m2, ~ FL_149_DO, type="response")

#c_main1=plot(c_main, horizontal =FALSE, colors="darkgreen")  + labs(x="Passage Type", y= "Probability Answering Question") + theme_bw(base_size = 14) + ggplot2::coord_cartesian(ylim = c(0, 1)) + theme(axis.text=bold)

#means<-estimate_means(full_model)

#ef1 <- effect("Passage", full_model) #take final glmer model 
#x1 <- as.data.frame(ef1)

#old <- element_text(face = "bold", color = "black", size = 14) #axis bold
#p<- ggplot(x1, aes(Passage, fit, fill=Passage))+ 
#  geom_bar(stat="identity", position="dodge") + 
#  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2, position=position_dodge(width=0.9),color="red") + theme_bw(base_size=14)+labs(y="Pr Recall ", x="Passage Type") + 
# scale_fill_manual(values=c("grey", "yellow", "black"))+
#  theme(axis.text=bold, legend.position = "none") + ggplot2::coord_cartesian(ylim = c(0, 1))


p1<- ggplot(ground_change_agg, aes(Passage, mean_acc, fill=Passage))+
  geom_violin() + 
  geom_jitter2(width=0.11, alpha=.5)+ 
  geom_line(data=ls1,aes(y=emmean, group=1), size=1)+ 
  geom_pointrange(data=ls1, aes(y=emmean, ymin=lower.CL, ymax=upper.CL), size=1, color="white")+ 
  theme_bw(base_size=14)+
  labs(y="Proportion Recalled on Test", x="Passage Type") + 
  theme(legend.position = "none") + 
  ggplot2::coord_cartesian(ylim = c(0, 1)) + 
  theme(axis.text=bold) 

p1


ground <- qualtRics::read_survey(here::here("Expt2_Data", "memory_acc_gw_final.csv"))

jols=ground %>% 
  dplr::group_by(ResponseId, FL_149_DO)%>%
  dplr::summarise(jols=mean(Q163_1)) %>%
  dplyr::ungroup() %>%
  dplyr::rename(Passage="FL_149_DO") %>% 
  dplyr::mutate(Passage=ifelse(Passage=="Highlight", "Pre-highlighted",     ifelse(Passage=="Passage", "Sans Forgetica", "Unmodified")))



jols$Passage <- factor(jols$Passage, level=c("Pre-highlighted", "Unmodified", "Sans Forgetica"))     



a1 <- aov_ez("ResponseId", "jols", jols, 
             between = c("Passage")) # one way

#plot the results


ls1 <- emmeans(a1, specs = "Passage") # get the simple effects test for signifcant interaction. 

flex1=pairs(ls1)
flex1=as.data.frame(ls1)


kable(flex1)
jols<-as.data.frame(jols)



p1<- ggplot(jols, aes(Passage, jols, fill=Passage))+
  geom_violin() + 
  geom_jitter2(width=0.11, alpha=.5)+ 
  geom_line(data=flex1,aes(y=emmean, group=1), size=1)+ 
  geom_pointrange(data=flex1, aes(y=emmean, ymin=lower.CL, ymax=upper.CL), size=1, color="white")+ 
  theme_bw(base_size=14)+
  labs(y= "Judgements of Learning", x="Passage Type") + 
  theme(legend.position = "none")  + 
  theme(axis.text=bold) 





