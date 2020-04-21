library(janitor)


here('Expt3_data' 'Gorilla_raw_data')

data=here('Expt3_data', 'Gorilla_raw_data')  # path to data files

file_list=list.files(data, pattern=".csv") # list of data files
 
# read in all files
dataset <-
  do.call("rbind", lapply(file_list, FUN=function(files){
    
    for (i in 1:length(files)){ 
      if(file.exists(files[i])){
        message( "now processing:", files[i])
      }
    }
    fread(files, header=TRUE, sep=",", na.strings = "", fill=TRUE)})) #fread makes reading in files quick
#

dd<-dataset %>% filter(Zone.Type=="response_button_text")
#response as character
dd$Response<-as.character(dd$Response)

rt<-dataset %>% janitor::clean_names(.) %>%  filter(zone_type=="continue_button", display=="study") # get RT


rt$reaction_time<-as.numeric(rt$reaction_time)



rt1<- rt %>% 
  group_by(Participant.Private.ID , condition) %>% 
  summarise(mean=mean(Reaction.Time, na.rm=TRUE))


#response as character

#recode Response as sayold and old.new as isold
dd$sayold=ifelse(dd$Response=="old ", 1, 0)
dd$isold=ifelse(dd$old.new== "new", 0, 1)


#contrast code condition and old.new for the model
ex3=dd %>% mutate(condition1= case_when( 
  condition == "SF" ~ 0.5, 
  condition =="normal" ~  -0.5, 
), isold= case_when (
  old.new== "old" ~ 0.5, 
  old.new== "new" ~ -0.5))

#fit GLMM in brms to extract the BF

oldnewglme=glmer(sayold~isold*condition1+(1+condition1|Participant.Private.ID)+ (1+condition1|Stims), data=ex3, family=binomial(link="probit"))

prior<-prior(normal(0,1), class="b") # weakly informed prior on the coefficents

#fit the brms model
oldnewbrm=brm(sayold~isold*condition1+(1+isold*condition1|Participant.Private.ID)+ (1+isold*condition1|Stims), data=ex3, family=bernoulli(link="probit"), prior=prior, sample_prior = TRUE,  cores = 4) 


#get BF for interaction which is difference in dprime from brms model
dprime=hypothesis(oldnewbrm, 'isold:condition1 = 0')



#classic SDT for those wanting to compare
sdt <- dd %>% 
  mutate(type = "hit",
         type = ifelse(isold==1 & sayold==0, "miss", type),
         type = ifelse(isold==0 & sayold==0, "cr", type),  # Correct rejection
         type = ifelse(isold==0 & sayold==1, "fa", type))  # False alarm

sdt <- sdt %>% 
  group_by(Participant.Private.ID, type, condition) %>% 
  summarise(count = n()) %>% 
  spread(type, count)  # Format data to one row per person

sdt <- sdt %>% 
  group_by(Participant.Private.ID, condition)%>%
  mutate(zhr = qnorm(hit / (hit+miss)),
         zfa = qnorm(fa / (fa+cr)),
         dprime = zhr-zfa,
         crit = -zfa)

p<- ggplot(dd, aes(condition, Propability, fill=condition))+ geom_bar(stat="identity", position="dodge") + geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=0.2, position=position_dodge(width=0.9),color="red") + theme_bw(base_size=14)+labs(y="", x="Semantic Type", fill="Relatedness") + scale_fill_manual(values=c("grey", "black"))+theme(axis.text=bold)
ggsave('../results/fig1.png', p, width=12, height=6)


p1<- ggplot(sdr, aes(condition, dprime, fill=condition))+
  geom_violin() + 
  geom_jitter2(width=0.11, alpha=.5)+ 
  theme_bw(base_size=14)+
  labs(y="Recall on Test (Pr)", x="Passage Type") + 
  theme(legend.position = "none") + 
  theme(axis.text=bold) 

