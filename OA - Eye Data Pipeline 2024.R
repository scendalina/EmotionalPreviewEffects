##############################################################
#************************************************************#
# Author: Clara Louise Lopes -
#                      2024 Dissertation Eye Movement Data   #
#   Cleaning, analyzing, and plotting master script          #
#             $$$ Updated and ammended 2/8/2024 $$$0         #
# If you have questions about the code or our analysis 
#         pipeline, I am available at clara.lopes@utah.edu   #
#************************************************************#
##############################################################

#--Reading in necessary packages--#

library(readr)
library(ggplot2)
library(lme4)
library(Rmisc)


library(doBy)
library(sjPlot)
# Interest Area Report - Loading in the Dataset
#Make sure you saved as a csv and changed "." to NA's 

library(readr)


OAdata <- read.csv("C:/Users/coyot/OneDrive/Desktop/Dissertation/EmoPrev/Raw Data/OA Raw Data Files/OA_31_final.csv", na = ".")

#make sure that condition is a factor!

OAdata$condition <- factor(OAdata$condition, levels = c("Neu/Neu", "UnNeu/Neu", "P/Neu", "N/Neu"))
is.factor(OAdata$condition)

data<-OAdata
# Rename first column ' Subject ' 

names(data)[1]<- "Subject" 


# See how many unique subjects we have 

(data$Subject)

# Look and see how many trials of each condition we have

unique(data$Subject)
table(OAdata$Subject, OAdata$condition)


#make sure that condition is a factor
is.factor(data$condition)



#******************************************************************************#
################################################################################
# Note: When loading in the data, we need to make sure that there are 
# no "." or FALSE present - this is how DataViewer spits out the files
################################################################################


# #Comprehension  = Comp_orig in WD 
# Comp_orig <- read_csv("C:/Users/clara/OneDrive/Desktop/WD/Comp_orig.csv")
# 
# #combining so I can score comprehension questions 
# comb<-inner_join(CleanEye,Comp_orig, by="comprehension")
# 
# #Now creating a 'compaccuracy' variable so I can score with COMPREHENSION_KEY_PRESS
# 
# comb$Accuracy<- ifelse(comb$Correct == comb$COMPREHENSION_KEYPRESS_VALUE,"right","wrong")
# describe(comb$Accuracy)
# #sum(correct)/length(correct)*100
# 
# 
# describe(comb$Correct)
# accuracy<-aggregate(Correct, data=comb, FUN=mean)
# 
# summary(comb$Correct)


library(psych)# <- can use to look at 'describe ' function
#alternatively can use 'table' to look at measures and condition
########################################################################
#**********************************************************************#
#  Renaming & Filtering out target words only                          #
#**********************************************************************#
########################################################################

#names(data)[1]<- "Subject" 
unique(data$Subject)

############
library(dplyr)
## Now we need to select target words only and scrap other words
cleaneye<-data

#if you need to remove specific subjects 
#cleaneye2<- subset(cleaneye, Subject != 's017')


unique(cleaneye$Subject)

cleaneye2 <- cleaneye %>%
 filter(!Subject %in% c('OA032'))

CleanEye<-subset(cleaneye2, cleaneye2$IA_ID == cleaneye2$indexofia)


#how many trials of each kind do we have 

table(CleanEye$Subject, CleanEye$condition) #<- this is how many trials per condition per subject

#This is how many trials per condition per item 

#check levels of condition 
levels(CleanEye$condition)

# Make sure you took out subject 32
length(unique(CleanEye$Subject))
################################################################################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
# SANITY CHECK! - Look at this measure to make sure nothing funky is going on
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
################################################################################

#ffd untrimmed
means <- summarySE(CleanEye, measurevar="IA_FIRST_FIXATION_DURATION", groupvars=c("condition", "Subject"), na.rm = TRUE)
ggplot(means, aes(x=condition, y=IA_FIRST_FIXATION_DURATION)) + 
  geom_errorbar(aes(ymin=IA_FIRST_FIXATION_DURATION-se, ymax=IA_FIRST_FIXATION_DURATION+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("First Fixation duration ")+ facet_grid(~Subject)

# so you can see the levels if it's hard to see in the graph
levels(CleanEye$condition) # [1] "Neu/Neu"   "UnNeu/Neu" "P/Neu"     "N/Neu" 

# #checking out first fix progressive 
# 
# means <- summarySE(CleanEye, measurevar="IA_FIRST_FIX_PROGRESSIVE", groupvars=c("condition", "Subject"), na.rm = TRUE)
# ggplot(means, aes(x=condition, y=IA_FIRST_FIX_PROGRESSIVE)) + 
#   geom_errorbar(aes(ymin=IA_FIRST_FIX_PROGRESSIVE-se, ymax=IA_FIRST_FIX_PROGRESSIVE+se), width=.1) +
#   geom_line() +
#   geom_point()+
#   xlab("Condition") + 
#   ylab("First FIX PROGRESSIVE ")+ facet_grid(~Subject)



################################################
# # OK if everything looks good - plz proceed! #
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#
# Loading in necessary packages & functions    #
#
################################################

library(dplyr)
library(ggplot2)


########################################################################
# *********************************************************************#                                                                      #
# The following script was borrowed with permission from B.R.Payne     #
# Creating fixation variables (filtering non-progressive eye movements)#
# *********************************************************************#                                                                      #
########################################################################

# First, make acopy of CleanEYE called IA_report_combined 

IA_report_combined <- CleanEye



########################################################################
# *********************************************************************#
# Now,finish processing the fixation measures of interest, 
# and make sure to trim super short/long fixations 
# *********************************************************************#
########################################################################


IA_report_combined$ffd<- ifelse(IA_report_combined$IA_FIRST_FIX_PROGRESSIVE==1, IA_report_combined$IA_FIRST_FIXATION_DURATION, NA)

#adding sfd - Need to double check on SR Support Forum - definition from SR Forum
#The duration of a fixation on a target region, 
#provided that region is fixated one time only, 
#and that the one fixation does not occur after fixations on words further along in the text.
IA_report_combined$sfd<- ifelse(IA_report_combined$IA_FIRST_FIX_PROGRESSIVE==1& IA_report_combined$IA_FIXATION_COUNT==1, IA_report_combined$IA_FIRST_FIXATION_DURATION,  NA)


IA_report_combined$gd<- ifelse(IA_report_combined$IA_FIRST_FIX_PROGRESSIVE==1, IA_report_combined$IA_FIRST_RUN_DWELL_TIME, NA)


means <- summarySE(IA_report_combined, measurevar="gd", groupvars=c("condition", "Subject"), na.rm = TRUE)
ggplot(means, aes(x=condition, y=gd)) + 
  geom_errorbar(aes(ymin=gd-se, ymax=gd+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("Gaze duration (Untrimmed) By Subject ")+ facet_grid(~Subject)





IA_report_combined$rpd<- ifelse(IA_report_combined$IA_FIRST_FIX_PROGRESSIVE==1, IA_report_combined$IA_REGRESSION_PATH_DURATION, NA)





#
IA_report_combined$trt<-IA_report_combined$IA_DWELL_TIME
IA_report_combined$reReading<-IA_report_combined$trt-IA_report_combined$gd # this is LATER / TOTAL reading time - GD as opposed to RPD-GD #Posthoc exploratory test...


IA_report_combined$refixProb<- ifelse(IA_report_combined$IA_FIRST_RUN_FIXATION_COUNT== 1, 0, 1)


#To trim based on individual (subject) quantiles:

#FFD

maxFFD<-aggregate(IA_report_combined$ffd,list(IA_report_combined$Subject,IA_report_combined$condition), quantile, p =0.997, na.rm = TRUE) #find max FFD for each subject

colnames(maxFFD)<-c("Subject", "condition", "maxFFD") #change names for merging

IA_report_combined2<-merge(IA_report_combined, maxFFD, by = c("Subject","condition"), all.x = TRUE) #merge

IA_report_combined2$ffd_trim<- ifelse(IA_report_combined2$ffd > IA_report_combined2$maxFFD,
                                      
                                      NA, IA_report_combined2$ffd) #trim

#SFD

maxsfd<-aggregate(IA_report_combined$sfd,list(IA_report_combined$Subject,IA_report_combined$condition), quantile, p =0.997, na.rm = TRUE) #find max sfd for each subject

colnames(maxsfd)<-c("Subject", "condition", "maxsfd") #change names for merging

IA_report_combined3<-merge(IA_report_combined2, maxsfd, by = c("Subject","condition"), all.x = TRUE) #merge

IA_report_combined3$sfd_trim<- ifelse(IA_report_combined3$sfd > IA_report_combined3$maxsfd,
                                      
                                      NA, IA_report_combined3$sfd) #trim

#GD
maxGD<-aggregate(IA_report_combined$gd,list(IA_report_combined$Subject,IA_report_combined$condition), quantile, p =0.997, na.rm = TRUE) #find max GD for each subject

colnames(maxGD)<-c("Subject", "condition", "maxGD") #change names for merging

IA_report_combined4<-merge(IA_report_combined3, maxGD, by = c("Subject","condition"), all.x = TRUE) #merge

IA_report_combined4$GD_trim<- ifelse(IA_report_combined4$gd > IA_report_combined4$maxGD,
                                     
                                     NA, IA_report_combined4$gd) #trim

#PostTrimm look
means <- summarySE(IA_report_combined4, measurevar="GD_trim", groupvars=c("condition", "Subject"), na.rm = TRUE)
ggplot(means, aes(x=condition, y=GD_trim)) + 
  geom_errorbar(aes(ymin=GD_trim-se, ymax=GD_trim+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("Gaze duration (Trimmed) By Subject ")+ facet_grid(~Subject)



#rpd

maxrpd<-aggregate(IA_report_combined$rpd,list(IA_report_combined$Subject,IA_report_combined$condition), quantile, p =0.997, na.rm = TRUE) #find max rpd for each subject

colnames(maxrpd)<-c("Subject", "condition", "maxrpd") #change names for merging

IA_report_combined5<-merge(IA_report_combined4, maxrpd, by = c("Subject","condition"), all.x = TRUE) #merge

IA_report_combined5$rpd_trim<- ifelse(IA_report_combined5$rpd > IA_report_combined5$maxrpd,
                                      
                                      NA, IA_report_combined5$rpd) #trim

#reReading

maxreReading<-aggregate(IA_report_combined$reReading,list(IA_report_combined$Subject,IA_report_combined$condition), quantile, p =0.997, na.rm = TRUE) #find max reReading for each subject

colnames(maxreReading)<-c("Subject", "condition", "maxreReading") #change names for merging

IA_report_combined6<-merge(IA_report_combined5, maxreReading, by = c("Subject","condition"), all.x = TRUE) #merge

IA_report_combined6$reReading_trim<- ifelse(IA_report_combined6$reReading > IA_report_combined6$maxreReading,
                                            
                                            NA, IA_report_combined6$reReading) #trim


#trt

maxtrt<-aggregate(IA_report_combined$trt,list(IA_report_combined$Subject,IA_report_combined$condition), quantile, p =0.997, na.rm = TRUE) #find max trt for each subject

colnames(maxtrt)<-c("Subject", "condition", "maxtrt") #change names for merging

IA_report_combined7<-merge(IA_report_combined6, maxtrt, by = c("Subject","condition"), all.x = TRUE) #merge

IA_report_combined7$trt_trim<- ifelse(IA_report_combined7$trt > IA_report_combined7$maxtrt,
                                      
                                      NA, IA_report_combined7$trt) #trim


################################################################################
library(plyr)

nodup<- IA_report_combined7 #making a copy

################################################################################
# eye<-IA_report_combined2
# names(eye)[1]<-"Subject"
# #eye2<- select (eye, c('Subject', 'ffd','gd','rpd','reReading','trt','maxFFD','ffd_trim'))
#combinging the conditional / message data frame with the "cleaned" IA Report = "EYE"
# EYE<-merge(condi, eye2, by ="Subject")

################################################################################

# ANALYSIS -Here is some code that may be helpful for visualizing your outcome 
#measures as a function of condition: 
#You will obviously have to change the variable names and data.frame. 
#The first chunk does kernel density plots by condition for each outcome. 
#The second chunk calculates means (via summarySE),and plots those means for each outcome. 
#In this case, we were using between subject error-bars, because the cond variable was between subject. 
#You may want to use within-subject error bars (e.g., summarySEwithin from RMisc, as in the last example here: https://www.niklasjohannes.com/post/calculating-and-visualizing-error-bars-for-within-subjects-designs/ ).
#Plotting basic distributions and means

library(Rmisc)
################################
ggplot(nodup, aes(x=ffd, colour=condition)) + geom_density()+coord_cartesian(xlim = c(0,1000))+
  xlab("First Fixation Duration") 


ggplot(nodup, aes(x=gd, colour=condition)) + geom_density()+coord_cartesian(xlim = c(0,1200))
ggplot(nodup, aes(x=rpd, colour=condition)) + geom_density()+coord_cartesian(xlim = c(0,2000))
ggplot(nodup, aes(x=reReading, colour=condition)) + geom_density()+coord_cartesian(xlim = c(0,2000))
ggplot(nodup, aes(x=trt, colour=condition)) + geom_density()+coord_cartesian(xlim = c(0,2000))

#ffd
means <- summarySE(nodup, measurevar="ffd_trim", groupvars=c("condition"), na.rm = TRUE)

ggplot(means, aes(x=condition, y=ffd_trim)) + 
  geom_errorbar(aes(ymin=ffd_trim-se, ymax=ffd_trim+se), width=.1) +
  geom_line() +
  geom_point()


#sfd 

means <- summarySE(nodup, measurevar="sfd_trim", groupvars=c("condition"), na.rm = TRUE)
ggplot(means, aes(x=condition, y=sfd_trim)) + 
  geom_errorbar(aes(ymin=sfd_trim-se, ymax=sfd_trim+se), width=.1) +
  geom_line() +
  geom_point()

#gd

means <- summarySE(nodup, measurevar="GD_trim", groupvars=c("condition"), na.rm = TRUE)
ggplot(means, aes(x=condition, y=GD_trim)) + 
  geom_errorbar(aes(ymin=GD_trim-se, ymax=GD_trim+se), width=.1) +
  geom_line() +
  geom_point()

#rpd

means <- summarySE(nodup, measurevar="rpd_trim", groupvars=c("condition"), na.rm = TRUE)
ggplot(means, aes(x=condition, y=rpd_trim)) + 
  geom_errorbar(aes(ymin=rpd_trim-se, ymax=rpd_trim+se), width=.1) +
  geom_line() +
  geom_point()

#re reading
means <- summarySE(nodup, measurevar="reReading_trim", groupvars=c("condition"), na.rm = TRUE)
ggplot(means, aes(x=condition, y=reReading_trim)) + 
  geom_errorbar(aes(ymin=reReading_trim-se, ymax=reReading_trim+se), width=.1) +
  geom_line() +
  geom_point()


#total reading time
means <- summarySE(nodup, measurevar="trt_trim", groupvars=c("condition"), na.rm = TRUE)
ggplot(means, aes(x=condition, y=trt_trim)) + 
  geom_errorbar(aes(ymin=trt_trim-se, ymax=trt_trim+se), width=.1) +
  geom_line() +
  geom_point()
#regressions out
means <- summarySE(nodup, measurevar="IA_REGRESSION_OUT", groupvars=c("condition"), na.rm = TRUE)
ggplot(means, aes(x=condition, y=IA_REGRESSION_OUT)) + 
  geom_errorbar(aes(ymin=IA_REGRESSION_OUT-se, ymax=IA_REGRESSION_OUT+se), width=.1) +
  geom_line() +
  geom_point()

#word skipping
means <- summarySE(nodup, measurevar="IA_SKIP", groupvars=c("condition"), na.rm = TRUE)
ggplot(means, aes(x=condition, y=IA_SKIP)) + 
  geom_errorbar(aes(ymin=IA_SKIP-se, ymax=IA_SKIP+se), width=.1) +
  geom_line() +
  geom_point()



#ffd
means <- summarySE(nodup, measurevar="ffd_trim", groupvars=c("condition"), na.rm = TRUE)

ggplot(means, aes(x=condition, y=ffd_trim)) + 
  geom_errorbar(aes(ymin=ffd_trim-se, ymax=ffd_trim+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("First Fixation Duration") 


#ffd trimmed - FACET WRAP to look at subjects & trials and make sure things look kosher
means <- summarySE(nodup, measurevar="ffd_trim", groupvars=c("condition", "Subject"), na.rm = TRUE)
ggplot(means, aes(x=condition, y=ffd_trim)) + 
  geom_errorbar(aes(ymin=ffd_trim-se, ymax=ffd_trim+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("First Fixation Duration")+ facet_grid(~Subject)

#sfd
means <- summarySE(nodup, measurevar="sfd_trim", groupvars=c("condition"), na.rm = TRUE)

ggplot(means, aes(x=condition, y=sfd_trim)) + 
  geom_errorbar(aes(ymin=sfd_trim-se, ymax=sfd_trim+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("Single Fixation Duration")


#gd
means <- summarySE(nodup, measurevar="GD_trim", groupvars=c("condition"), na.rm = TRUE)

ggplot(means, aes(x=condition, y=GD_trim)) + 
  geom_errorbar(aes(ymin=GD_trim-se, ymax=GD_trim+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("Gaze Duration")
#rpd
means <- summarySE(nodup, measurevar="rpd_trim", groupvars=c("condition"), na.rm = TRUE)
ggplot(means, aes(x=condition, y=rpd_trim)) + 
  geom_errorbar(aes(ymin=rpd_trim-se, ymax=rpd_trim+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("Regression Path Duration (trimmed)")

#re reading
means <- summarySE(nodup, measurevar="reReading_trim", groupvars=c("condition"), na.rm = TRUE)
ggplot(means, aes(x=condition, y=reReading_trim)) + 
  geom_errorbar(aes(ymin=reReading_trim-se, ymax=reReading_trim+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("Re-reading Time")
#total reading time
means <- summarySE(nodup, measurevar="trt_trim", groupvars=c("condition"), na.rm = TRUE)
ggplot(means, aes(x=condition, y=trt_trim)) + 
  geom_errorbar(aes(ymin=trt_trim-se, ymax=trt_trim+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("Total Reading Time")

#regressions out
means <- summarySE(nodup, measurevar="IA_REGRESSION_OUT", groupvars=c("condition"), na.rm = TRUE)
ggplot(means, aes(x=condition, y=IA_REGRESSION_OUT)) + 
  geom_errorbar(aes(ymin=IA_REGRESSION_OUT-se, ymax=IA_REGRESSION_OUT+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("Regressions Out of Target")



#word skipping
means <- summarySE(nodup, measurevar="IA_SKIP", groupvars=c("condition"), na.rm = TRUE)
ggplot(means, aes(x=condition, y=IA_SKIP)) + 
  geom_errorbar(aes(ymin=IA_SKIP-se, ymax=IA_SKIP+se), width=.1) +
  geom_line() +
  geom_point()+ xlab("Condition") + 
  ylab("Word Skipping")


Emoeye<-nodup


Emoeye2<-Emoeye

# Write to csv for individual difference analyses later on... 

write.csv(Emoeye2, "C:/Users/coyot/OneDrive/Desktop/Dissertation/EmoPrev/Raw Data/OAeyeClean.csv", row.names = FALSE)


###################################################################################################################
# Extra How-To Code in case you need it 
#Emoeye2$PreviewVal <- ifelse(Emoeye2$condition == "UnNeu/Neu" | Emoeye2$condition == "Neu/Neu","Invalid", "Valid")
#as.factor(Emoeye2$PreviewVal)

# as. factor == Levels Invalid, Valid 
#moving the conditions column next to my new Preview Validity column
require(dplyr)
#Emoeye2 <- Emoeye2 %>% relocate(condition, .before = PreviewVal)
####################################################################################################################

##################################################################################################
#************************************************************************************************#

#              This is contrast-coding that is NOT Dummy coding, and is the equivalent
#   to using the 'mixed' function etc. 

#************************************************************************************************#
###################################################################################################


Emo_IDBaseline<- Emoeye2

Emo_UnrelatedBaseline<-Emoeye2
Emo_PositiveBaseline<- Emoeye2
Emo_NegativeBaseline<-Emoeye2


Emo_IDBaseline$condition<-relevel(Emo_IDBaseline$condition, ref = "Neu/Neu")
Emo_UnrelatedBaseline$condition <- relevel(Emo_UnrelatedBaseline$condition, ref = "UnNeu/Neu") #<-Baseline reference
Emo_PositiveBaseline$condition<-relevel(Emo_PositiveBaseline$condition, ref = "P/Neu")
Emo_NegativeBaseline$condition<-relevel(Emo_NegativeBaseline$condition, ref = "N/Neu")

library(afex)
library(emmeans)

# Now go to OA_Data_Analyses_and_Viz script to continue!




























# ###############################################################################
# # A little data viz
# 
library(ggplot2)

library(RColorBrewer)


Emoeye2_summary <- Emoeye2 %>%
  group_by(condition) %>%
  summarise(sfd_trim_mean = mean(sfd_trim, na.rm = TRUE),
            se = sd(sfd_trim, na.rm = TRUE) / sqrt(n()),
            ci = qt(0.975, df=n()-1) * se) # for 95% CI

# Position adjustment for dodging
pd <- position_dodge(0.1)
# 
# 
# #******************************************************************************
# This is the bar chart you want! ********************************************
#******************************************************************************
ggplot(Emoeye2_summary, aes(x=condition, y=sfd_trim_mean, fill=condition)) + # Replace 'grouping_variable' as needed
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=sfd_trim_mean-se, ymax=sfd_trim_mean+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Condition") +
  ylab("Single Fixation Duration (ms)") +
  ggtitle("N=18") +
  theme_minimal() +
  scale_y_continuous(breaks=seq(0, max(Emoeye2_summary$ffd_trim_mean+Emoeye2_summary$se), by=50)) # Adjust 'by' as needed for your data range


###############################################################################
#*****************************************************************************#
#*#############################################################################

# Creating a means and errors plot using summary SE cookbook approach - 
#          Gaze Duration 

library(dplyr)
library(ggplot2)
###############################################################################
#*****************************************************************************#
#*#############################################################################

# Calculate summary statistics for GD_trim
Emoeye2_summary <- Emoeye2 %>%
  group_by(condition) %>%
  summarise(GD_trim_mean = mean(GD_trim, na.rm = TRUE),
            se = sd(GD_trim, na.rm = TRUE) / sqrt(n()),
            ci = qt(0.975, df=n()-1) * se) # for 95% CI

# Position adjustment for dodging
pd <- position_dodge(0.1)

# Using standard error for error bars
ggplot(Emoeye2_summary, aes(x=condition, y=GD_trim_mean)) + 
  geom_errorbar(aes(ymin=GD_trim_mean-se, ymax=GD_trim_mean+se), width=.1, position=pd) +
  geom_line(aes(group=condition), position=pd) + # Ensure lines are grouped by condition
  geom_point(position=pd) +
  labs(x="Condition", y="Gaze Duration (ms)") +
  theme_minimal()

# Using 95% confidence interval for error bars
ggplot(Emoeye2_summary, aes(x=condition, y=GD_trim_mean)) + 
  geom_errorbar(aes(ymin=GD_trim_mean-ci, ymax=GD_trim_mean+ci), width=.1, position=pd) +
  geom_line(aes(group=condition), position=pd) +
  geom_point(position=pd) +
  labs(x="Condition", y="Gaze Duration (ms)") +
  theme_minimal()

# With black error bars and dodging
ggplot(Emoeye2_summary, aes(x=condition, y=GD_trim_mean, group=condition)) + 
  geom_errorbar(aes(ymin=GD_trim_mean-ci, ymax=GD_trim_mean+ci), colour="red", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  labs(x="Condition", y="Gaze Duration (ms)") +
  theme_minimal()


library(ggplot2)
library(dplyr)

# This is the bar chart you want! 
ggplot(Emoeye2_summary, aes(x=condition, y=GD_trim_mean, fill=condition)) + # Replace 'grouping_variable' as needed
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=GD_trim_mean-se, ymax=GD_trim_mean+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Condition") +
  ylab("Gaze Duration (ms)") +
  ggtitle("N=10") +
  theme_minimal() +
  scale_y_continuous(breaks=seq(0, max(Emoeye2_summary$GD_trim_mean+Emoeye2_summary$se), by=50)) # Adjust 'by' as needed for your data range

# sfd Bar chart

Emoeye2_summary <- Emoeye2 %>%
  group_by(condition) %>%
  summarise(sfd_trim_mean = mean(sfd_trim, na.rm = TRUE),
            se = sd(sfd_trim, na.rm = TRUE) / sqrt(n()),
            ci = qt(0.975, df=n()-1) * se) # for 95% CI

ggplot(Emoeye2_summary, aes(x=condition, y=sfd_trim_mean, fill=condition)) + # Replace 'grouping_variable' as needed
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=sfd_trim_mean-se, ymax=sfd_trim_mean+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Condition") +
  ylab("Single Fixation Duration (ms)") +
  ggtitle("N=10") +
  theme_minimal() +
  scale_y_continuous(breaks=seq(0, max(Emoeye2_summary$sfd_trim_mean+Emoeye2_summary$se), by=50)) # Adjust 'by' as needed for your data range


# rpd Bar chart


Emoeye2_summary <- Emoeye2 %>%
  group_by(condition) %>%
  summarise(rpd_trim_mean = mean(rpd_trim, na.rm = TRUE),
            se = sd(rpd_trim, na.rm = TRUE) / sqrt(n()),
            ci = qt(0.975, df=n()-1) * se) # for 95% CI


ggplot(Emoeye2_summary, aes(x=condition, y=rpd_trim_mean, fill=condition)) + # Replace 'grouping_variable' as needed
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=rpd_trim_mean-se, ymax=rpd_trim_mean+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Condition") +
  ylab("Regression Path Duration (ms)") +
  ggtitle("N=10") +
  theme_minimal() +
  scale_y_continuous(breaks=seq(0, max(Emoeye2_summary$rpd_trim_mean+Emoeye2_summary$se), by=50)) # Adjust 'by' as needed for your data range


# trt Bar chart


Emoeye2_summary <- Emoeye2 %>%
  group_by(condition) %>%
  summarise(trt_trim_mean = mean(trt_trim, na.rm = TRUE),
            se = sd(rpd_trim, na.rm = TRUE) / sqrt(n()),
            ci = qt(0.975, df=n()-1) * se) # for 95% CI

ggplot(Emoeye2_summary, aes(x=condition, y=trt_trim_mean, fill=condition)) + # Replace 'grouping_variable' as needed
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=trt_trim_mean-se, ymax=trt_trim_mean+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Condition") +
  ylab("Total Reading Time (ms)") +
  ggtitle("N=10") +
  theme_minimal() +
  scale_y_continuous(breaks=seq(0, max(Emoeye2_summary$trt_trim_mean+Emoeye2_summary$se), by=50)) # Adjust 'by' as needed for your data range





