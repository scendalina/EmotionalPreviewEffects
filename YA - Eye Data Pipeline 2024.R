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

# Interest Area Report - Loading in the Dataset
#Make sure you saved as a csv and changed "." to NA's 

library(readr)

YAdata <- read.csv("C:/Users/coyot/OneDrive/Desktop/Dissertation/EmoPrev/Raw Data/YA Raw Data Files/Eye Data/ya_32.csv", na = ".")


#make sure that condition is a factor!
YAdata$condition <- factor(YAdata$condition, levels = c("Neu/Neu", "UnNeu/Neu", "P/Neu", "N/Neu"))

is.factor(YAdata$condition)

# Rename first column ' Subject ' 
#names(data)[1]<- "Subject" 

data<-YAdata


# See how many unique subjects we have 

(data$Subject)

# Look and see how many trials of each condition we have

unique(data$Subject)
table(data$Subject,data$condition)


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


unique(cleaneye$Subject)

n_unique_subjectsBefore <- length(unique(cleaneye$Subject))

# Printing the number of unique subjects
print(n_unique_subjectsBefore)


# Removing subjects who did appear not to have done the reading task


cleaneye2 <- cleaneye %>%
  filter(!Subject %in% c('s032'))




CleanEye<-cleaneye2
CleanEye<-subset(CleanEye, CleanEye$IA_ID == CleanEye$indexofia)

#now see how many subjects we have in this dataset

n_unique_subjects <- length(unique(cleaneye2$Subject))

# Printing the number of unique subjects
print(n_unique_subjects)


#how many trials of each kind do we have 

table(CleanEye$Subject, CleanEye$condition) #<- this is how many trials per condition per subject

#This is how many trials per condition per item 

#check levels of condition 
levels(CleanEye$condition)


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


IA_report_combined$reReading<-IA_report_combined$rp-IA_report_combined$gd


#
IA_report_combined$trt<-IA_report_combined$IA_DWELL_TIME

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

GDplot1<-ggplot(means, aes(x=condition, y=GD_trim)) + 
  geom_errorbar(aes(ymin=GD_trim-se, ymax=GD_trim+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("Gaze Duration")
#rpd
means <- summarySE(nodup, measurevar="rpd_trim", groupvars=c("condition"), na.rm = TRUE)
rpdplot1<-ggplot(means, aes(x=condition, y=rpd_trim)) + 
  geom_errorbar(aes(ymin=rpd_trim-se, ymax=rpd_trim+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("Regression Path Duration (trimmed)")

#re reading
means <- summarySE(nodup, measurevar="reReading_trim", groupvars=c("condition"), na.rm = TRUE)

RRplot1<-ggplot(means, aes(x=condition, y=reReading_trim)) + 
  geom_errorbar(aes(ymin=reReading_trim-se, ymax=reReading_trim+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("Re-reading Time")

#total reading time
means <- summarySE(nodup, measurevar="trt_trim", groupvars=c("condition"), na.rm = TRUE)
trtplot1<-ggplot(means, aes(x=condition, y=trt_trim)) + 
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

write.csv(Emoeye2, "C:/Users/coyot/OneDrive/Desktop/Dissertation/EmoPrev/Raw Data/YAeyeClean.csv", row.names = FALSE)

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
# If you need to check the time sample display - 
#   •	  time_sample_display: the overall delay between the triggering sample and display change (start of the retrace that the screen was actually drawn / redrawn);
# •	time_sample_boundary: the delay between the occurrence of the sample and the trigger firing;
# •	time_boundary_display: the delay between the Invisible Boundary trigger and the display change.

