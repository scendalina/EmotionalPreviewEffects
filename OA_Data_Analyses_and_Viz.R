################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#        EmoPrev OA - Statistical Modeling and DataViz!        #
#    Author: Clara Louise Lopes 
#                                     Date started: 3/29/2024  #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################################################


# Data set comes from the product of YA Eye Data Pipeline 
# Called cleaneye2 and 
#  from models Emo_UnrelatedBaseline dataframe - - 
# Run that first to have it or upload this cleaned file: 
# 


Emo_IDBaseline<- Emoeye2
Emo_UnrelatedBaseline<-Emoeye2
Emo_PositiveBaseline<- Emoeye2
Emo_NegativeBaseline<-Emoeye2


Emo_IDBaseline$condition<-relevel(Emo_IDBaseline$condition, ref = "Neu/Neu")
Emo_UnrelatedBaseline$condition <- relevel(Emo_UnrelatedBaseline$condition, ref = "UnNeu/Neu") #<-Baseline reference
Emo_PositiveBaseline$condition<-relevel(Emo_PositiveBaseline$condition, ref = "P/Neu")
Emo_NegativeBaseline$condition<-relevel(Emo_NegativeBaseline$condition, ref = "N/Neu")

#  OR just upload it from here


#Emo_UnrelatedBaseline<- 
# read.csv(Emo_UnrelatedBaseline, "C:/Users/coyot/OneDrive/Desktop/Dissertation/EmoPrev/Cleaned Data/Emo_UnrelatedBaselineOA.csv",
# row.names = FALSE)

Emo_UnrelatedBaseline$condition <- relevel(Emo_UnrelatedBaseline$condition, ref = "UnNeu/Neu") #<-Baseline reference
levels(Emo_UnrelatedBaseline$condition)

library(afex)
library(emmeans)
library(afex)
library(emmeans)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################################################
# 1a. First Fixation Duration Model 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################################################

# Fit the lmer_alt model for First Fixation Duration
FFDmod1_Unrelated <- lmer_alt(ffd_trim ~ condition + (1|Subject) + (1|item), data=Emo_UnrelatedBaseline, method="LRT", check_contrasts = FALSE)

# Display the model summary
FFDmod1_Unrelated
summary(FFDmod1_Unrelated)

# Calculate Marginal means using emmeans
ffd.emmeans <- emmeans(FFDmod1_Unrelated, specs = c("condition"))

# Convert emmeans to a data frame for plotting and rename conditions
ffd.emmeans_df <- summary(ffd.emmeans) %>%
  as.data.frame() %>%
  mutate(Condition = factor(levels(Emo_UnrelatedBaseline$condition)[condition],
                            levels = levels(Emo_UnrelatedBaseline$condition),
                            labels = c("UnRelated Preview (Baseline)", "Identity Preview", "Positive Preview", "Negative Preview")))

# Calculate pairwise comparisons
ffd.pairs <- contrast(ffd.emmeans, method = "pairwise")

# Convert pairwise comparison results to a data frame
ffd.pairs_df <- as.data.frame(summary(ffd.pairs))

# Print the pairwise comparisons to ensure they are calculated correctly
print(ffd.pairs_df)

# Filter for specific contrasts and adjust labels
ffd_specific_contrasts <- ffd.pairs_df %>%
  filter(contrast %in% c("(UnNeu/Neu) - (Neu/Neu)", "(UnNeu/Neu) - (P/Neu)", "(UnNeu/Neu) - (N/Neu)")) %>%
  mutate(contrast = factor(contrast,
                           levels = c("(UnNeu/Neu) - (Neu/Neu)", "(UnNeu/Neu) - (P/Neu)", "(UnNeu/Neu) - (N/Neu)"),
                           labels = c("Unrelated Preview - Identity", "Unrelated Preview - Positive Preview", "Unrelated Preview - Negative Preview")))

# Calculate confidence intervals manually
alpha <- 0.05
z_value <- qnorm(1 - alpha / 2)
ffd_specific_contrasts$conf.low <- ffd_specific_contrasts$estimate - z_value * ffd_specific_contrasts$SE
ffd_specific_contrasts$conf.high <- ffd_specific_contrasts$estimate + z_value * ffd_specific_contrasts$SE

# Print filtered contrasts to ensure they are correct
print(ffd_specific_contrasts)

# Define custom colors for each contrast
custom_colors <- c("Unrelated Preview - Identity" = "#89CFF0", 
                   "Unrelated Preview - Positive Preview" = "#77DD77", 
                   "Unrelated Preview - Negative Preview" = "#FF6961")

# Create the plot with adjusted font sizes, renamed contrasts, and pastel colors
ffd_plot <- ggplot(ffd_specific_contrasts, aes(x = estimate, y = contrast, xmin = conf.low, xmax = conf.high, color = contrast)) +
  geom_point(size = 3) +
  geom_errorbarh(height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  labs(x = "Difference in Marginal Means of First Fixation Duration", y = "Condition Pair",
       title = "Specific Pairwise Comparison of Conditions for First Fixation Duration",
       color = "Condition") +
  theme(
    axis.text.y = element_text(size = 12), # Adjust y-axis text size
    axis.title.x = element_text(size = 14), # Adjust x-axis title size
    axis.title.y = element_text(size = 14), # Adjust y-axis title size
    plot.title = element_text(size = 16, face = "bold"), # Adjust plot title size
    legend.position = "none"
  )

# Print the plot
print(ffd_plot)

# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################################################
# 2a. Single Fixation Duration Model 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################################################

SFDmod1_Unrelated <- lmer_alt(sfd_trim ~ condition + (1|Subject) + (1|item), data=Emo_UnrelatedBaseline, method="LRT", check_contrasts = FALSE)

# Display the model summary
summary(SFDmod1_Unrelated)

# Calculate Marginal means using emmeans
sfd.emmeans <- emmeans(SFDmod1_Unrelated, specs = c("condition"))

# Convert emmeans to a data frame for plotting and rename conditions
sfd.emmeans_df <- summary(sfd.emmeans) %>%
  as.data.frame() %>%
  mutate(Condition = factor(levels(Emo_UnrelatedBaseline$condition)[condition],
                            levels = levels(Emo_UnrelatedBaseline$condition),
                            labels = c("UnRelated Preview (Baseline)", "Identity Preview", "Positive Preview", "Negative Preview")))

# Calculate pairwise comparisons
sfd.pairs <- contrast(sfd.emmeans, method = "pairwise")

# Convert pairwise comparison results to a data frame
sfd.pairs_df <- as.data.frame(summary(sfd.pairs))

# Print the pairwise comparisons to ensure they are calculated correctly
print(sfd.pairs_df)

# Filter for specific contrasts and adjust labels
sfd_specific_contrasts <- sfd.pairs_df %>%
  filter(contrast %in% c("(UnNeu/Neu) - (Neu/Neu)", "(UnNeu/Neu) - (P/Neu)", "(UnNeu/Neu) - (N/Neu)")) %>%
  mutate(contrast = factor(contrast,
                           levels = c("(UnNeu/Neu) - (Neu/Neu)", "(UnNeu/Neu) - (P/Neu)", "(UnNeu/Neu) - (N/Neu)"),
                           labels = c("Unrelated Preview - Identity", "Unrelated Preview - Positive Preview", "Unrelated Preview - Negative Preview")))

# Calculate confidence intervals manually
alpha <- 0.05
z_value <- qnorm(1 - alpha / 2)
sfd_specific_contrasts$conf.low <- sfd_specific_contrasts$estimate - z_value * sfd_specific_contrasts$SE
sfd_specific_contrasts$conf.high <- sfd_specific_contrasts$estimate + z_value * sfd_specific_contrasts$SE

# Print filtered contrasts to ensure they are correct
print(sfd_specific_contrasts)

# Define custom colors for each contrast
custom_colors <- c("Unrelated Preview - Identity" = "#89CFF0", 
                   "Unrelated Preview - Positive Preview" = "#77DD77", 
                   "Unrelated Preview - Negative Preview" = "#FF6961")

# Create the plot with adjusted font sizes, renamed contrasts, and pastel colors
sfd_plot <- ggplot(sfd_specific_contrasts, aes(x = estimate, y = contrast, xmin = conf.low, xmax = conf.high, color = contrast)) +
  geom_point(size = 3) +
  geom_errorbarh(height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  labs(x = "Difference in Marginal Means of Single Fixation Duration", y = "Condition Pair",
       title = "Specific Pairwise Comparison of Conditions for Single Fixation Duration",
       color = "Condition") +
  theme(
    axis.text.y = element_text(size = 12), # Adjust y-axis text size
    axis.title.x = element_text(size = 14), # Adjust x-axis title size
    axis.title.y = element_text(size = 14), # Adjust y-axis title size
    plot.title = element_text(size = 16, face = "bold"), # Adjust plot title size
    legend.position = "bottom"
  )

# Print the plot
print(sfd_plot)
########################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Make a panel with ffd and sfd bc they are on a v similar time-course
library(cowplot)

# Combine the plots using plot_grid with minimal spacing
combinedPlot <- plot_grid(sfd_plot,ffd_plot, ncol = 1, 
                          labels = c("A", "B"), align = 'v', 
                          rel_heights = c(1, 1)) 

                            # Equal heights for both plots



# Print the combined plot
print(combinedPlot)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#########################################################################
# 3a. Gaze Duration Model 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Fit the lmer_alt model
GDmod1_Unrelated <- lmer_alt(GD_trim ~ condition + (1|Subject) + (1|item), data=Emo_UnrelatedBaseline, method="LRT", check_contrasts = FALSE)

# Display the model summary
summary(GDmod1_Unrelated)

# Calculate Marginal means using emmeans
GD.emmeans <- emmeans(GDmod1_Unrelated, specs = c("condition"))

# Perform pairwise comparisons
GD.pairs <- contrast(GD.emmeans, method = "pairwise")

# Convert pairwise comparison results to a data frame
GD_pairs_df <- as.data.frame(summary(GD.pairs))

# Print the pairwise comparisons to ensure they are calculated correctly
print(GD_pairs_df)

# Filter for specific contrasts and adjust labels
GD_specific_contrasts <- GD_pairs_df %>%
  filter(contrast %in% c("(UnNeu/Neu) - (Neu/Neu)", "(UnNeu/Neu) - (P/Neu)", "(UnNeu/Neu) - (N/Neu)")) %>%
  mutate(contrast = factor(contrast,
                           levels = c("(UnNeu/Neu) - (Neu/Neu)", "(UnNeu/Neu) - (P/Neu)", "(UnNeu/Neu) - (N/Neu)"),
                           labels = c("Unrelated Preview - Identity", "Unrelated Preview - Positive Preview", "Unrelated Preview - Negative Preview")))

# Calculate confidence intervals manually
alpha <- 0.05
z_value <- qnorm(1 - alpha / 2)
GD_specific_contrasts$conf.low <- GD_specific_contrasts$estimate - z_value * GD_specific_contrasts$SE
GD_specific_contrasts$conf.high <- GD_specific_contrasts$estimate + z_value * GD_specific_contrasts$SE

# Print filtered contrasts to ensure they are correct
print(GD_specific_contrasts)

# Define custom colors for each contrast
custom_colors <- c("Unrelated Preview - Identity" = "#89CFF0", 
                   "Unrelated Preview - Positive Preview" = "#77DD77", 
                   "Unrelated Preview - Negative Preview" = "#FF6961")


# Create the plot with adjusted font sizes, renamed contrasts, and custom colors
GD_plot3 <- ggplot(GD_specific_contrasts, aes(x = estimate, y = contrast, xmin = conf.low, xmax = conf.high, color = contrast)) +
  geom_point(size = 3) +
  geom_errorbarh(height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  labs(x = "Difference in Marginal Means of Gaze Duration", y = "Condition Pair",
       title = "Specific Pairwise Comparison of Conditions for Gaze Duration",
       color = "Condition") +
  theme(
    axis.text.y = element_text(size = 12), # Adjust y-axis text size
    axis.title.x = element_text(size = 14), # Adjust x-axis title size
    axis.title.y = element_text(size = 14), # Adjust y-axis title size
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "none" # Remove legend# Adjust plot title size
  )

# Print the plot
print(GD_plot3)




# If models can't converge use || stepping down first by random slope
# read up on 'afex ||' uncorrelated random effects 
# figure out if you use lmer_alt to turn ON dummy codes and turn OFF effect-coding in lmer_alt() package
# ok or use emmeans to follow up on contrasts - run pairs on that lmer_alt model 


################################################################
# 4a. Regression Path Duration Model 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################################################

# Fit the lmer_alt model for Regression Path Duration
RPDmod1_Unrelated <- lmer_alt(rpd_trim ~ condition + (1|Subject) + (1|item), data=Emo_UnrelatedBaseline, method="LRT", check_contrasts = FALSE)

# Display the model summary
summary(RPDmod1_Unrelated)

# Calculate Marginal means using emmeans
rpd.emmeans <- emmeans(RPDmod1_Unrelated, specs = c("condition"))

# Convert emmeans to a data frame for plotting and rename conditions
rpd.emmeans_df <- summary(rpd.emmeans) %>%
  as.data.frame() %>%
  mutate(Condition = factor(levels(Emo_UnrelatedBaseline$condition)[condition],
                            levels = levels(Emo_UnrelatedBaseline$condition),
                            labels = c("UnRelated Preview (Baseline)", "Identity Preview", "Positive Preview", "Negative Preview")))

# Calculate pairwise comparisons
rpd.pairs <- contrast(rpd.emmeans, method = "pairwise")

# Convert pairwise comparison results to a data frame
rpd.pairs_df <- as.data.frame(summary(rpd.pairs))

# Print the pairwise comparisons to ensure they are calculated correctly
print(rpd.pairs_df)

# Filter for specific contrasts and adjust labels
rpd_specific_contrasts <- rpd.pairs_df %>%
  filter(contrast %in% c("(UnNeu/Neu) - (Neu/Neu)", "(UnNeu/Neu) - (P/Neu)", "(UnNeu/Neu) - (N/Neu)")) %>%
  mutate(contrast = factor(contrast,
                           levels = c("(UnNeu/Neu) - (Neu/Neu)", "(UnNeu/Neu) - (P/Neu)", "(UnNeu/Neu) - (N/Neu)"),
                           labels = c("Unrelated Preview - Identity", "Unrelated Preview - Positive Preview", "Unrelated Preview - Negative Preview")))

# Calculate confidence intervals manually
alpha <- 0.05
z_value <- qnorm(1 - alpha / 2)
rpd_specific_contrasts$conf.low <- rpd_specific_contrasts$estimate - z_value * rpd_specific_contrasts$SE
rpd_specific_contrasts$conf.high <- rpd_specific_contrasts$estimate + z_value * rpd_specific_contrasts$SE

# Print filtered contrasts to ensure they are correct
print(rpd_specific_contrasts)

# Define custom colors for each contrast
custom_colors <- c("Unrelated Preview - Identity" = "#89CFF0", 
                   "Unrelated Preview - Positive Preview" = "#77DD77", 
                   "Unrelated Preview - Negative Preview" = "#FF6961")

# Create the plot with adjusted font sizes, renamed contrasts, and pastel colors
rpd_plot <- ggplot(rpd_specific_contrasts, aes(x = estimate, y = contrast, xmin = conf.low, xmax = conf.high, color = contrast)) +
  geom_point(size = 3) +
  geom_errorbarh(height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  labs(x = "Difference in Marginal Means of Regression Path Duration", y = "Condition Pair",
       title = "Specific Pairwise Comparison of Conditions for Regression Path Duration",
       color = "Condition") +
  theme(
    axis.text.y = element_text(size = 12), # Adjust y-axis text size
    axis.title.x = element_text(size = 14), # Adjust x-axis title size
    axis.title.y = element_text(size = 14), # Adjust y-axis title size
    plot.title = element_text(size = 16, face = "bold"), # Adjust plot title size
    legend.position = "bottom"
  )

# Print the plot
print(rpd_plot)



# If models can't converge use || stepping down first by random slope
# read up on 'afex ||' uncorrelated random effects 
# figure out if you use lmer_alt to turn ON dummy codes and turn OFF effect-coding in lmer_alt() package
# ok or use emmeans to follow up on contrasts - run pairs on that lmer_alt model 





########################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Make a panel with GD and RPD  bc they are on a v similar time-course


# Combine the plots using plot_grid with minimal spacing
combinedPlot2 <- plot_grid(GD_plot3, rpd_plot, ncol = 1, 
                           labels = c("A", "B"), align = 'v', 
                           rel_heights = c(1, 1)) + # Equal heights for both plots
  theme(plot.title = element_text(size = 14)) # Common theme adjustments

combinedPlot2



################################################################
# 5a. Total Reading Time Model 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################################################

TRTmod1_Unrelated <- lmer_alt(trt_trim ~ condition + (1|Subject) + (1|item), data=Emo_UnrelatedBaseline, method="LRT", check_contrasts = FALSE)

# Display the model summary
summary(TRTmod1_Unrelated)

# Calculate Marginal means using emmeans
trt.emmeans <- emmeans(TRTmod1_Unrelated, specs = c("condition"))

# Convert emmeans to a data frame for plotting and rename conditions
trt.emmeans_df <- summary(trt.emmeans) %>%
  as.data.frame() %>%
  mutate(Condition = factor(levels(Emo_UnrelatedBaseline$condition)[condition],
                            levels = levels(Emo_UnrelatedBaseline$condition),
                            labels = c("UnRelated Preview (Baseline)", "Identity Preview", "Positive Preview", "Negative Preview")))

# Calculate pairwise comparisons
trt.pairs <- contrast(trt.emmeans, method = "pairwise")

# Convert pairwise comparison results to a data frame
trt.pairs_df <- as.data.frame(summary(trt.pairs))

# Print the pairwise comparisons to ensure they are calculated correctly
print(trt.pairs_df)

# Filter for specific contrasts and adjust labels
trt_specific_contrasts <- trt.pairs_df %>%
  filter(contrast %in% c("(UnNeu/Neu) - (Neu/Neu)", "(UnNeu/Neu) - (P/Neu)", "(UnNeu/Neu) - (N/Neu)")) %>%
  mutate(contrast = factor(contrast,
                           levels = c("(UnNeu/Neu) - (Neu/Neu)", "(UnNeu/Neu) - (P/Neu)", "(UnNeu/Neu) - (N/Neu)"),
                           labels = c("Unrelated Preview - Identity", "Unrelated Preview - Positive Preview", "Unrelated Preview - Negative Preview")))

# Calculate confidence intervals manually
alpha <- 0.05
z_value <- qnorm(1 - alpha / 2)
trt_specific_contrasts$conf.low <- trt_specific_contrasts$estimate - z_value * trt_specific_contrasts$SE
trt_specific_contrasts$conf.high <- trt_specific_contrasts$estimate + z_value * trt_specific_contrasts$SE

# Print filtered contrasts to ensure they are correct
print(trt_specific_contrasts)

# Define custom colors for each contrast
custom_colors <- c("Unrelated Preview - Identity" = "#89CFF0", 
                   "Unrelated Preview - Positive Preview" = "#77DD77", 
                   "Unrelated Preview - Negative Preview" = "#FF6961")

# Create the plot with adjusted font sizes, renamed contrasts, and pastel colors
trt_plot <- ggplot(trt_specific_contrasts, aes(x = estimate, y = contrast, xmin = conf.low, xmax = conf.high, color = contrast)) +
  geom_point(size = 3) +
  geom_errorbarh(height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  labs(x = "Difference in Marginal Means of Total Reading Time", y = "Condition Pair",
       title = "Specific Pairwise Comparison of Conditions for Total Reading Time",
       color = "Condition") +
  theme(
    axis.text.y = element_text(size = 12), # Adjust y-axis text size
    axis.title.x = element_text(size = 14), # Adjust x-axis title size
    axis.title.y = element_text(size = 14), # Adjust y-axis title size
    plot.title = element_text(size = 16, face = "bold"), # Adjust plot title size
    legend.position = "bottom"
  )

# Print the plot
print(trt_plot)

################################################################
# RE-Reading (Back in to the mix...... checking for Brenn)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################################################

RRmod1_Unrelated <- lmer_alt(reReading_trim ~ condition + (1|Subject) + (1|item), data=Emo_UnrelatedBaseline, method="LRT", check_contrasts = FALSE)

# Display the model summary
summary(RRmod1_Unrelated)

# Calculate Marginal means using emmeans
RR.emmeans <- emmeans(RRmod1_Unrelated, specs = c("condition"))

# Convert emmeans to a data frame for plotting and rename conditions
RR.emmeans_df <- summary(RR.emmeans) %>%
  as.data.frame() %>%
  mutate(Condition = factor(levels(Emo_UnrelatedBaseline$condition)[condition],
                            levels = levels(Emo_UnrelatedBaseline$condition),
                            labels = c("UnRelated Preview (Baseline)", "Identity Preview", "Positive Preview", "Negative Preview")))

# Calculate pairwise comparisons
RR.pairs <- contrast(RR.emmeans, method = "pairwise")

# Convert pairwise comparison results to a data frame
RR.pairs_df <- as.data.frame(summary(RR.pairs))

# Print the pairwise comparisons to ensure they are calculated correctly
print(RR.pairs_df)

# Filter for specific contrasts and adjust labels
RR_specific_contrasts <- RR.pairs_df %>%
  filter(contrast %in% c("(UnNeu/Neu) - (Neu/Neu)", "(UnNeu/Neu) - (P/Neu)", "(UnNeu/Neu) - (N/Neu)")) %>%
  mutate(contrast = factor(contrast,
                           levels = c("(UnNeu/Neu) - (Neu/Neu)", "(UnNeu/Neu) - (P/Neu)", "(UnNeu/Neu) - (N/Neu)"),
                           labels = c("Unrelated Preview - Identity", "Unrelated Preview - Positive Preview", "Unrelated Preview - Negative Preview")))

# Calculate confidence intervals manually
alpha <- 0.05
z_value <- qnorm(1 - alpha / 2)
RR_specific_contrasts$conf.low <- RR_specific_contrasts$estimate - z_value * RR_specific_contrasts$SE
RR_specific_contrasts$conf.high <- RR_specific_contrasts$estimate + z_value * RR_specific_contrasts$SE

# Print filtered contrasts to ensure they are correct
print(RR_specific_contrasts)

# Define custom colors for each contrast
custom_colors <- c("Unrelated Preview - Identity" = "#89CFF0", 
                   "Unrelated Preview - Positive Preview" = "#77DD77", 
                   "Unrelated Preview - Negative Preview" = "#FF6961")

# Create the plot with adjusted font sizes, renamed contrasts, and pastel colors
RR_plot <- ggplot(RR_specific_contrasts, aes(x = estimate, y = contrast, xmin = conf.low, xmax = conf.high, color = contrast)) +
  geom_point(size = 3) +
  geom_errorbarh(height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  labs(x = "Difference in Marginal Means of Re-Reading Time", y = "Condition Pair",
       title = "Specific Pairwise Comparison of Conditions for Re-Reading Time",
       color = "Condition") +
  theme(
    axis.text.y = element_text(size = 12), # Adjust y-axis text size
    axis.title.x = element_text(size = 14), # Adjust x-axis title size
    axis.title.y = element_text(size = 14), # Adjust y-axis title size
    plot.title = element_text(size = 16, face = "bold"), # Adjust plot title size
    legend.position = "bottom"
  )

# Print the plot
print(RR_plot)








################################################################################
# 6a. Probability of Skipping The Target Model 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################################################



contrasts(Emo_UnrelatedBaseline$condition) <- contr.treatment(levels(Emo_UnrelatedBaseline$condition))

# Fit the lmer_alt model - logit!

probofskip_Unrelated <- lmer_alt(IA_SKIP~condition+(1|Subject)+(1|item), data = Emo_UnrelatedBaseline, 
                                 family=binomial(logit),method = "LRT", check_contrasts = FALSE,
                                 control = glmerControl(optCtrl = list(maxfun = 1e6)),
                                 expand_re = TRUE)

# Display the model summary
probofskip_Unrelated # Not significant
summary(probofskip_Unrelated)

# Display the emmeans
pskip.emmeans

# Using your existing model ffdmod1_Unrelated, calculate emmeans
pskip.emmeans <- emmeans(probofskip_Unrelated, specs = c("condition"))

# Do a pair-wise comparison to see what is driving the main effect
pskip.pairs<- pairs(pskip.emmeans, adjust="none")

################################################################
# 7a. Probability of Regressing Out Target Model 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################################################


contrasts(Emo_UnrelatedBaseline$condition) <- contr.treatment(levels(Emo_UnrelatedBaseline$condition))

# Fit the lmer_alt model - logit!

probofregOUT_Unrelated <- lmer_alt(IA_REGRESSION_OUT~condition+(1|Subject)+(1|item), data = Emo_UnrelatedBaseline, 
                                   family=binomial(logit),method = "LRT", check_contrasts = FALSE,
                                   control = glmerControl(optCtrl = list(maxfun = 1e6)),
                                   expand_re = TRUE)

# Display the model summary
probofregOUT_Unrelated # Not significant
summary(probofregOUT_Unrelated)

# Display the emmeans
probofregOUT.emmeans<- emmeans(probofregOUT_Unrelated, specs = c("condition"))

# Do a pair-wise comparison to see what is driving the main effect
pairs(probofregOUT.emmeans, adjust="none")


################################################################
# 7a. Probability of Refixating the Target
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################################################


contrasts(Emo_UnrelatedBaseline$condition) <- contr.treatment(levels(Emo_UnrelatedBaseline$condition))

# Fit the lmer_alt model - logit!

refixProb_Unrelated <- lmer_alt(refixProb~condition+(1|Subject)+(1|item), data = Emo_UnrelatedBaseline, 
                                family=binomial(logit),method = "LRT", check_contrasts = FALSE,
                                control = glmerControl(optCtrl = list(maxfun = 1e6)),
                                expand_re = TRUE)

# Display the model summary
refixProb_Unrelated # Significant! 
summary(refixProb_Unrelated)
# Calculate Marginal means
refixProb.emmeans <- emmeans(refixProb_Unrelated, specs = c("condition"))
emmeans::emm_options(lmer.df = "asymptotic")

# Display the emmeans
refixProb.emmeans

# Do a pair-wise comparison to see what is driving the main effect
refixProb.pairs(refixProb.emmeans, adjust="none")

###########################################################
# Check the levels of the condition variable and this is how I can confirm which condition 1, 2 ,3 are
levels(Emo_UnrelatedBaseline$condition)

# Or directly inspect the contrasts to see how they have been set up
contrasts(Emo_UnrelatedBaseline$condition)


# If models can't converge use || stepping down first by random slope
# read up on 'afex ||' uncorrelated random effects 
# figure out if you use lmer_alt to turn ON dummy codes and turn OFF effect-coding in lmer_alt() package
# ok or use emmeans to follow up on contrasts - run pairs on that lmer_alt model 



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################################################
# 6b. Prob of Refix Data Viz

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################################################

# Marginal means plot - allows us to visually compare the means 
# (estimated marginal means or EMMs) of the first fixation duration across conditions

library(ggplot2)
emm_df <- as.data.frame(refixProb.emmeans)
refixplot1<-ggplot(emm_df, aes(x = condition, y = emmean, group = 1)) +
  geom_line() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = .1) +
  geom_point() +
  theme_minimal() +
  labs(x = "Condition", y = "Estimated Marginal Mean (refixProb)", title = "Marginal Means of p(Refixating the target) by Condition")


library(ggplot2)
library(dplyr)
library(emmeans)

# Ensure that condition uses dummy coding
contrasts(Emo_UnrelatedBaseline$condition) <- contr.treatment(levels(Emo_UnrelatedBaseline$condition))

# Using your existing model, calculate emmeans
refixProb.emmeans <- emmeans(reReadingmod1_Unrelated, specs = c("condition"))

# Convert emmeans to a data frame for plotting and rename conditions
refixProb.emmeans_df <- summary(refixProb.emmeans) %>%
  as.data.frame() %>%
  mutate(Condition = factor(levels(Emo_UnrelatedBaseline$condition)[condition],
                            levels = levels(Emo_UnrelatedBaseline$condition),
                            labels = c("UnRelated Preview (Baseline)", "Identity Preview", "Positive Preview", "Negative Preview")))

# Plotting
refixplot2<-ggplot(emmeans_df, aes(x = Condition, y = emmean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position = position_dodge(.9), width = 0.25) +
  labs(x = "Condition", y = "Estimated Marginal Mean (refixProb)", title = "Marginal Means of Total Reading Time with 95% CI") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none") # Hides the legend


# Pairwise comparison plot - allows us to look at differences in contrasts

refixProb.pairs_df <- as.data.frame(pairs(refixProb.emmeans, adjust = "none"))
ggplot(pairs_df, aes(x = contrast, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), width = .2) +
  theme_minimal() +
  labs(x = "Condition Pair", y = "Difference in Marginal Means of the Probability of Refixating the Target", title = "Pairwise Comparison of Conditions") +
  coord_flip()

# Let's first inspect the contrast names to ensure we filter correctly
unique(pairs_df$contrast)


library(dplyr)
library(ggplot2)

# Filter for the specific contrasts I want - that make the most sense... UnrelatedNeutral as the baseline 

specific_contrasts <- pairs_df %>%
  filter(grepl("\\(UnNeu/Neu\\) - \\(Neu/Neu\\)|\\(UnNeu/Neu\\) - \\(P/Neu\\)|\\(UnNeu/Neu\\) - \\(N/Neu\\)", contrast))

# Plot the filtered pairwise comparisons
refixplot3<-ggplot(specific_contrasts, aes(x = contrast, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), width = .2) +
  theme_minimal() +
  labs(x = "Condition Pair", y = "Difference in Marginal Means of the Probability of Refixating the Target", title = "Specific Pairwise Comparison of Conditions") +
  coord_flip()

################################################################################
#  Raw Data Plots 

ggplot(nodup, aes(x=ffd, colour=condition)) + geom_density()+coord_cartesian(xlim = c(0,1000))+
  xlab("First Fixation Duration") 



ggplot(nodup, aes(x=gd, colour=condition)) + geom_density()+coord_cartesian(xlim = c(0,1200))
ggplot(nodup, aes(x=rpd, colour=condition)) + geom_density()+coord_cartesian(xlim = c(0,2000))
ggplot(nodup, aes(x=reReading, colour=condition)) + geom_density()+coord_cartesian(xlim = c(0,2000))
ggplot(nodup, aes(x=trt, colour=condition)) + geom_density()+coord_cartesian(xlim = c(0,2000))



#ffd trimmed - FACET WRAP to look at subjects & trials and make sure things look kosher
means <- summarySE(nodup, measurevar="ffd_trim", groupvars=c("condition", "Subject"), na.rm = TRUE)
ggplot(means, aes(x=condition, y=ffd_trim)) + 
  geom_errorbar(aes(ymin=ffd_trim-se, ymax=ffd_trim+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("First Fixation Duration")+ facet_grid(~Subject)

#ffd
means <- summarySE(nodup, measurevar="ffd_trim", groupvars=c("condition"), na.rm = TRUE)

ffdplot1<-ggplot(means, aes(x=condition, y=ffd_trim)) + 
  geom_errorbar(aes(ymin=ffd_trim-se, ymax=ffd_trim+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("First Fixation Duration") 

ffdplot1


#sfd
means <- summarySE(nodup, measurevar="sfd_trim", groupvars=c("condition"), na.rm = TRUE)

sfdplot1<-ggplot(means, aes(x=condition, y=sfd_trim)) + 
  geom_errorbar(aes(ymin=sfd_trim-se, ymax=sfd_trim+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("Single Fixation Duration")

sfdplot1

#gd
means <- summarySE(nodup, measurevar="GD_trim", groupvars=c("condition"), na.rm = TRUE)

GDplot1<-ggplot(means, aes(x=condition, y=GD_trim)) + 
  geom_errorbar(aes(ymin=GD_trim-se, ymax=GD_trim+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("Gaze Duration")

# Adding significance bars/stars ***
GDplot1 <- GDplot1 +
  # Horizontal bar for Neu/Neu vs. baseline
  annotate("segment", x = 1, xend = 2, y = 405, yend = 405, colour = "black", size = 0.8) +
  # Adding shorter downward pointing arms at the ends of the horizontal bar for Neu/Neu vs. baseline
  annotate("segment", x = 1, xend = 1, y = 405, yend = 404, colour = "black", size = 0.8) +
  annotate("segment", x = 2, xend = 2, y = 405, yend = 404, colour = "black", size = 0.8) +
  # Stars positioned above the bar for Neu/Neu vs. baseline
  annotate("text", x = 1.5, y = 406, label = "***", size = 6, color = "black")

print(GDplot1)



###############################################################################################


# rpd
means <- summarySE(nodup, measurevar="rpd_trim", groupvars=c("condition"), na.rm = TRUE)
print(means)

rpdplot1 <- ggplot(means, aes(x = condition, y = rpd_trim)) + 
  geom_errorbar(aes(ymin = rpd_trim - se, ymax = rpd_trim + se), width = .1) +
  geom_point() +
  xlab("Condition") + 
  ylab("Regression Path Duration")+ 
  expand_limits(y=610) # <<< use this not ylim() !!!!!

# Adding significance bars/stars ***
rpdplot1 <- rpdplot1 +
  # Horizontal bar for Neu/Neu vs. baseline
  annotate("segment", x = 1, xend = 2, y = 605, yend = 605, colour = "black", size = 0.8) +
  # Adding shorter downward pointing arms at the ends of the horizontal bar for Neu/Neu vs. baseline
  annotate("segment", x = 1, xend = 1, y = 605, yend = 600, colour = "black", size = 0.8) +
  annotate("segment", x = 2, xend = 2, y = 605, yend = 600, colour = "black", size = 0.8) +
  # Stars positioned above the bar for Neu/Neu vs. baseline
  annotate("text", x = 1.5, y = 610, label = "***", size = 6, color = "black")

print(rpdplot1)


###################################################################################################

# re reading
means <- summarySE(nodup, measurevar="reReading_trim", groupvars=c("condition"), na.rm = TRUE)
print(means)

RRplot1 <- ggplot(means, aes(x=condition, y=reReading_trim)) + 
  geom_errorbar(aes(ymin=reReading_trim-se, ymax=reReading_trim+se), width=.1) +
  geom_point() +
  xlab("Condition") + 
  ylab("Re-reading Time") +
  expand_limits(y=185)

# Adding significance bars/stars ***
RRplot1 <- RRplot1 +
  # Horizontal bar for Neu/Neu vs. baseline
  annotate("segment", x = 1, xend = 2, y = 180, yend = 180, colour = "black", size = 0.8) +
  # Adding shorter downward pointing arms at the ends of the horizontal bar for Neu/Neu vs. baseline
  annotate("segment", x = 1, xend = 1, y = 180, yend = 179, colour = "black", size = 0.8) +
  annotate("segment", x = 2, xend = 2, y = 180, yend = 179, colour = "black", size = 0.8) +
  # Stars positioned above the bar for Neu/Neu vs. baseline
  annotate("text", x = 1.5, y = 181, label = "**", size = 6, color = "black")

print(RRplot1)
#_____________________________________________________________________________________________

# total reading time
means <- summarySE(nodup, measurevar="trt_trim", groupvars=c("condition"), na.rm = TRUE)
print(means)

trtplot1 <- ggplot(means, aes(x=condition, y=trt_trim)) + 
  geom_errorbar(aes(ymin=trt_trim-se, ymax=trt_trim+se), width=.1) +
  geom_point() +
  xlab("Condition") + 
  ylab("Total Reading Time") +
  expand_limits(y=1080)

trtplot1 <- trtplot1 +
  # Horizontal bar for Neu/Neu vs. baseline
  annotate("segment", x = 1, xend = 2, y = 1078, yend = 1078, colour = "black", size = 0.8) +
  # Adding shorter downward pointing arms at the ends of the horizontal bar for Neu/Neu vs. baseline
  annotate("segment", x = 1, xend = 1, y = 1078, yend = 1076, colour = "black", size = 0.8) +
  annotate("segment", x = 2, xend = 2, y = 1078, yend = 1076, colour = "black", size = 0.8) +
  # Stars positioned above the bar for Neu/Neu vs. baseline
  annotate("text", x = 1.5, y = 1083, label = "***", size = 6, color = "black") +
  # Horizontal bar for P/Neu vs. baseline
  annotate("segment", x = 3, xend = 2, y = 1078, yend = 1078, colour = "black", size = 0.8) +
  # Adding shorter downward pointing arms at the ends of the horizontal bar for P/Neu vs. baseline
  annotate("segment", x = 3, xend = 3, y = 1078, yend = 1076, colour = "black", size = 0.8) +
  annotate("segment", x = 2, xend = 2, y = 1078, yend = 1076, colour = "black", size = 0.8) +
  # Stars positioned above the bar for P/Neu vs. baseline
  annotate("text", x = 2.5, y = 1083, label = "**", size = 6, color = "black")

print(trtplot1)


####
#Re-Reading 

#
means <- summarySE(nodup, measurevar="reReading_trim", groupvars=c("condition"), na.rm = TRUE)
print(means)

RRplot1 <- ggplot(means, aes(x=condition, y=reReading_trim)) + 
  geom_errorbar(aes(ymin=reReading_trim-se, ymax=reReading_trim+se), width=.1) +
  geom_point() +
  xlab("Condition") + 
  ylab("Re-Reading Time") +
  expand_limits(y=800)

RRplot1 <- RRplot1 +
  # Horizontal bar for Neu/Neu vs. baseline
  annotate("segment", x = 1, xend = 2, y = 800, yend = 800, colour = "black", size = 0.8) +
  # Adding shorter downward pointing arms at the ends of the horizontal bar for Neu/Neu vs. baseline
  annotate("segment", x = 1, xend = 1, y = 800, yend = 798, colour = "black", size = 0.8) +
  annotate("segment", x = 2, xend = 2, y = 800, yend = 798, colour = "black", size = 0.8) +
  # Stars positioned above the bar for Neu/Neu vs. baseline
  annotate("text", x = 1.5, y = 804, label = "***", size = 6, color = "black") +
  # Horizontal bar for P/Neu vs. baseline
  annotate("segment", x = 3, xend = 2, y = 800, yend = 800, colour = "black", size = 0.8) +
  # Adding shorter downward pointing arms at the ends of the horizontal bar for P/Neu vs. baseline
  annotate("segment", x = 3, xend = 3, y = 800, yend = 798, colour = "black", size = 0.8) +
  annotate("segment", x = 2, xend = 2, y = 800, yend = 798, colour = "black", size = 0.8) +
  # Stars positioned above the bar for P/Neu vs. baseline
  annotate("text", x = 2.5, y = 804, label = "**", size = 6, color = "black")

print(RRplot1)


###################################################################################################

#regressions out
means <- summarySE(nodup, measurevar="IA_REGRESSION_OUT", groupvars=c("condition"), na.rm = TRUE)
regOUTplot1<-ggplot(means, aes(x=condition, y=IA_REGRESSION_OUT)) + 
  geom_errorbar(aes(ymin=IA_REGRESSION_OUT-se, ymax=IA_REGRESSION_OUT+se), width=.1) +
  geom_line() +
  geom_point()+
  xlab("Condition") + 
  ylab("Regressions Out of Target")+
  expand_limits(y=.26)


# Adding significance bars/stars ***
regOUTplot1 <- regOUTplot1 +
  # Horizontal bar for Neu/Neu vs. baseline
  annotate("segment", x = 1, xend = 2, y = 0.26, yend = 0.26, colour = "black", size = 0.8) +
  # Adding shorter downward pointing arms at the ends of the horizontal bar for Neu/Neu vs. baseline
  annotate("segment", x = 1, xend = 1, y = 0.26, yend = 0.258, colour = "black", size = 0.8) +
  annotate("segment", x = 2, xend = 2, y = 0.26, yend = 0.258, colour = "black", size = 0.8) +
  # Stars positioned above the bar for Neu/Neu vs. baseline
  annotate("text", x = 1.5, y = 0.265, label = "*", size = 6, color = "black")

print(regOUTplot1)

####################################################################################################

#Refix

means <- summarySE(nodup, measurevar="refixProb", groupvars=c("condition"), na.rm = TRUE)
refixplot1 <- ggplot(means, aes(x=condition, y=refixProb)) + 
  geom_errorbar(aes(ymin=refixProb-se, ymax=refixProb+se), width=.1) +
  geom_point() + 
  xlab("Condition") + 
  ylab("p(refixating the target)") + 
  expand_limits(y=.35)

# Adding significance bars and stars
refixplot1 <- refixplot1 +
  # Horizontal bar for Neu/Neu vs. baseline
  annotate("segment", x = 1, xend = 2, y = 0.352, yend = 0.352, colour = "black", size = 0.8) +
  # Adding shorter downward pointing arms at the ends of the horizontal bar for Neu/Neu vs. baseline
  annotate("segment", x = 1, xend = 1, y = 0.352, yend = 0.35, colour = "black", size = 0.8) +
  annotate("segment", x = 2, xend = 2, y = 0.352, yend = 0.35, colour = "black", size = 0.8) +
  # Stars positioned above the bar for Neu/Neu vs. baseline
  annotate("text", x = 1.5, y = 0.355, label = "*", size = 6, color = "black")

print(refixplot1)





################################################################################################
#word skipping
means <- summarySE(nodup, measurevar="IA_SKIP", groupvars=c("condition"), na.rm = TRUE)
skipplot1<-ggplot(means, aes(x=condition, y=IA_SKIP)) + 
  geom_errorbar(aes(ymin=IA_SKIP-se, ymax=IA_SKIP+se), width=.1) +
  geom_line() +
  geom_point()+ xlab("Condition") + 
  ylab("Word Skipping")

##################################!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(cowplot)


# Plot the reading times together and then the p(based) together

rawplotPub<-plot_grid( sfdplot1,ffdplot1, GDplot1, rpdplot1, trtplot1, ncol = 5, 
                      labels=c("A", "B","C", "D", "E"))+
  theme(legend.position="bottom")
rawplotPub


rplotprob<-plot_grid(refixplot1, regOUTplot1, skipplot1, ncol = 3, 
                     labels=c("A", "B","C"))+
  theme(legend.position="bottom")
rplotprob



# Plot the significant ones (only) in a grid for the manuscript! 
#GD
GDplotPub<-plot_grid(GDplot, GDplot2, GDplot3,ncol = 3, 
                     labels=c("A", "B","C"))+
  theme(legend.position="right")
GDplotPub

#RPD
rpdplotPub<-plot_grid(rpdplot, rpdplot2, rpdplot3,ncol = 3, 
                      labels=c("A", "B","C"))+
  theme(legend.position="right")

#rpd with means not emmeans
rpdplotPub<-plot_grid(rpdplot1, rpdplot2, rpdplot3,ncol = 3, 
                      labels=c("A", "B","C"))+
  theme(legend.position="right")
rpdplotPub

#ReReading
RRplotPub<-plot_grid(RRplot, RRplot2, RRplot3,ncol = 3, 
                     labels=c("A", "B","C"))+
  theme(legend.position="right")
RRplotPub


#trt
trtplotPub<-plot_grid(trtplot, trtplot2, trtplot3,ncol = 3, 
                      labels=c("A", "B","C"))+
  theme(legend.position="right")
trtplotPub

#refixprob
refixplotPub<-plot_grid(refixplot, refixplot2, refixplot3,ncol = 3, 
                        labels=c("A", "B","C"))+
  theme(legend.position="right")
refixplotPub


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
#   to using the 'lmer_alt' function etc. 

#************************************************************************************************#
###################################################################################################

