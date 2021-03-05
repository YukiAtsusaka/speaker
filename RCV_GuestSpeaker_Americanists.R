######################################################################################
# RCV_GuestSpeaker_Americanists.R
# Created on: 3/4/2021
# Created by: Yuki Atsusaka
# Aim: to tally and visualize Americanists' ranked preferences for the Guest Speaker
######################################################################################

rm(list=ls())
library(tidyverse)
devtools::install_github("ds-elections/rcv")
library(rcv)
library(networkD3) # Sankey diagram


######################################################
# USING THE GUEST SPEAKER DATA
######################################################

# Randomized and Anonymized Ranked Ballots
speaker <- rbind(
           c("Lublin", "Sen", "Collingwood", "Enos", "Juenke", "McDermott"), 
           c("Lublin", "Sen", "Juenke", "Enos", "Collingwood", "McDermott"),           
           c("Collingwood", "Juenke", "Enos", "Lublin", "Sen", "McDermott"),
           c("Collingwood", "Sen", "Enos", "McDermott", "Juenke", "Lublin"),
           c("McDermott", "Juenke", "Collingwood", "Enos", "Sen", "Lublin"), 
           c("Lublin", "Collingwood", "Enos", "Sen", "McDermott", "Juenke"),
           c("Enos", "McDermott", "Sen", "Collingwood", "Lublin", "Juenke")  
)
speaker <- data.frame(speaker)


speaker

# TRANSFORM THE DATA INTO "rcv" Package Format
speaker2 <- speaker %>% 
           rename(choice1=X1, choice2=X2,
                  choice3=X3, choice4=X4,
                  choice5=X5, choice6=X6) %>%
           mutate(pref_voter_id = row_number()) %>%
           dplyr::select(pref_voter_id, everything()) %>%
           gather(starts_with("choice"), key="vote_rank", value="candidate") %>%
           mutate(vote_rank = case_when(vote_rank=="choice1" ~ 1,
                                        vote_rank=="choice2" ~ 2,
                                        vote_rank=="choice3" ~ 3,
                                        vote_rank=="choice4" ~ 4,
                                        vote_rank=="choice5" ~ 5,
                                        vote_rank=="choice6" ~ 6,                                        
                                        )) %>%
           arrange(pref_voter_id, vote_rank)


speaker_tally <- rcv_tally(speaker2)[-7,]
speaker_tally 



# VISUALIZE VOTE TRANSFER VIA SANKEY DIAGRAM (NOT WORKING PROPERLY)
speaker_d3 <- rcv::make_d3list(results = speaker_tally)
networkD3::sankeyNetwork(Links = speaker_d3$values, Nodes = speaker_d3$names,
                         Source = "source", Target = "target",
                         Value = "value", NodeID = "candidate", units = "voters",
                         fontSize = 20, nodeWidth = 40)

