
install.packages("qualtRics")
install.packages("tidyverse")
install.packages("afex")
install.packages("emmeans")
install.packages("ggbeeswarm")
install.packages("hunspell")
install.packages("tidytext")


library(qualtRics)
library(tidyverse)
library(afex)
library(emmeans)
library(hunspell)
library(tidytext)

#Read in each qualtrics file. For SF and Generate conditions, we used two counterbalanced lists so each cue-target pair was presented in the fluent (read) and disfluent conditions (generate or SF font) across participants. 


gen1 <- qualtRics::read_survey("generate1cb.csv", legacy=TRUE) %>%
  rename(ResponseID="V1")


# cb 1 
gen2 <- qualtRics::read_survey("generate2cb.csv", legacy=TRUE) %>%
  rename(ResponseID="V1")
# cb 2
sf1 <- qualtRics::read_survey("sf1cb.csv", legacy=TRUE) %>%
  rename(ResponseID="V1")
# Sans F 1
sf2 <- qualtRics::read_survey("sf2cb.csv", legacy=TRUE) %>%
  rename(ResponseID="V1")

#In the code below we extract the relevant information from the qualtrics files we loaded in earlier. 



#cb1gen
gen1= gen1 %>% 
  select(ResponseID,Finished, Progress, Q434, Q435, Q436, Q437, Q438, Q439, Q440, Q441, Q442, Q443, Q444, Q445, Q447, Q447, Q448, Q449, Q450, Q451, Q452, Q452, Q453, Q454, Q455, Q456, Q457, Q458) # select the columns relevant for

#cb2gen

gen2= gen2 %>% 
  dplyr::select(ResponseID,Finished, Progress, Q434, Q435, Q436, Q437, Q438, Q439, Q440, Q441, Q442, Q443, Q444, Q445, Q447, Q447, Q448, Q449, Q450, Q451, Q452, Q452, Q453, Q454, Q455, Q456, Q457, Q458) 

sf1= sf1 %>% 
  dplyr::select(ResponseID,Finished, Progress, Q434, Q435, Q436, Q437, Q438, Q439, Q440, Q441, Q442, Q443, Q444, Q445, Q447, Q447, Q448, Q449, Q450, Q451, Q452, Q452, Q453, Q454, Q455, Q456, Q457, Q458) 
#cb2gen
sf2 = sf2 %>%
  dplyr::select(ResponseID,Finished, Progress, Q434, Q435, Q436, Q437, Q438, Q439, Q440, Q441, Q442, Q443, Q444, Q445, Q447, Q447, Q448, Q449, Q450, Q451, Q452, Q452, Q453, Q454, Q455, Q456, Q457, Q458) 



#Per our pre-registration criteria, we only include individuals that finished the experiment. We also include individuals with 99% completion rate (it appears they completed the experiment but did not click off the last page). 


gen1fin <- subset(gen1, gen1$Finished==TRUE | gen1$Progress==99) 
gen2fin <- subset(gen2, gen2$Finished==TRUE | gen2$Progress==99) 
gen2fin1 <- gen2fin[sample(nrow(gen2fin), 58), ] # randomly select 58 
sf1fin <- subset(sf1, sf1$Finished==TRUE | sf1$Progress==99) #
sf2fin <- subset(sf2, sf2$Finished==TRUE | sf2$Progress==99) 
sf2fin1 <- sf2fin[sample(nrow(sf2fin), 58),]


#Qualtrics files are imported in wide format. We want them in long format. The 'gather' function will do this for us. 

t1=tidyr::gather(gen1fin, "question", "answer", Q434:Q458)

t2=tidyr::gather(gen2fin1, "question", "answer", Q434:Q458)

t3=tidyr::gather(sf1fin, "question", "answer", Q434:Q458)

t4=tidyr::gather(sf2fin1, "question", "answer", Q434:Q458)


#Load the words lists for each cb. Each line needs to be repeated 58 number of times (the number of respondents)

cb1gen=read.csv("CB1.csv", header=TRUE)

cb1=data.frame(cue=rep(cb1gen$cue1, each=58), target=rep(cb1gen$targ1, each=58), cond=rep(cb1gen$font, each=58))

cb2gen=read.csv("CB2.csv", header=TRUE)

cb2=data.frame(cue=rep(cb2gen$cue1, each=58), target=rep(cb2gen$targ1, each=58), cond=rep(cb2gen$font, each=58))

cb1sf=read.csv("CB1_SF.csv", header=TRUE)

sfcb1=data.frame(cue=rep(cb1sf$cue1, each=58), target=rep(cb1sf$targ1, each=58), cond=rep(cb1sf$font, each=58))

cb2sf=read.csv("CB2_SF.csv", header=TRUE)

sfcb2=data.frame(cue=rep(cb2sf$cue1, each=58), target=rep(cb2sf$targ1, each=58), cond=rep(cb2sf$font, each=58))



#Merge each cb list with participant responses

gencb1=cbind(cb1, t1)

gencb2=cbind(cb2, t2)

sfcb1=cbind(sfcb1, t3)

sfcb2=cbind(sfcb2, t4)

# combine each condition list
gen12 <- rbind(gencb1, gencb2) # generate cb 1 and 2 

sf12 <- rbind(sfcb1, sfcb2) # sf cb 1 and 2 

gen12$condition <- "Generate"

sf12$dis <- ifelse(sf12$cond=="flu", "fluent", "disfluent")
gen12$dis <- ifelse(gen12$cond=="flu", "fluent", "disfluent")
sf12$condition <- "Sans Forgetica"



#We can Combine all the lists, but before we run our statistical analysis, we need to remove two cue-target pairs. There was an error in the generate CB 1 list wherein *train-plane* was presented twice during encoding and *rifle-range* was not presented at all. 
sfgen<-rbind(gen12, sf12)

#auto spellcheck
# Extract a list of words
tokens <- unnest_tokens(tbl = sfgen, output = token, input = answer)
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
tokens <- unnest_tokens(tbl = sfgen, output = token,
                        input = answer, token = stringr::str_split,
                        pattern = " |\\, |\\.|\\,|\\;")

tokens$acc <-ifelse(tokens$target==tokens$token, 1, 0)

#error in expt these targets were not presented

sfgen1<- tokens %>% 
  dplyr::filter(target!="plane", target!="rifle")

sfgen1$acc <-ifelse(sfgen1$target==sfgen1$token, 1, 0)
#exact match accuracy
sfgen1[is.na(sfgen1)] <- 0 #change all NAs to 0 
## get aggreagte recall per subject, condition, and dis

full_model=glmer(acc~condition*dis + (1+ dis|ResponseID) + (0+dis+condition|target), data=sfgen1, contrasts = list(dis="contr.sum", condition="contr.sum"), family="binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

write.csv(tokens, file="sfgenerate_final.csv")

ef1 <- effect("condition:dis", full_model) #take final glmer model 
summary(ef1)
x1 <- as.data.frame(ef1)

bold <- element_text(face = "bold", color = "black", size = 14) #axis bold
p<- ggplot(x1, aes(dis, fit, fill=dis))+ facet_grid(~condition)+ 
  geom_bar(stat="identity", position="dodge") + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2, position=position_dodge(width=0.9),color="red") + theme_bw(base_size=14)+labs(y="Proporition Recalled on Final Test", x="Disfluency") + 
  theme(legend.position = "none") +
  scale_fill_manual(values=c("grey", "black")) + ggplot2::coord_cartesian(ylim = c(0, 1))


