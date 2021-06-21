#Hidden Falsified Data in Synthetic Data: A Cautionary Example
#Andrew J. Kelly


##--LIBRARIES--##################################################################################
library(effectsize) #to get effect sizes easily
library(ggplot2) #to visualize the data
library(synthpop) #to make the synthetic data
library(cowplot) #to make ggplot less ggplot and more nice

##--GENERATING THE DATA--##################################################################################

#creates the pre-data dataframe
set.seed(1234) #sets seed of random values for pre-data
pre_seed <- rnorm(70, mean=30, sd=3) #generates random values for post-data
pre_values <- round(pre_seed) #rounds random values from generation
pre_values #reports the generated pre-data
mean(pre_values) #reports the mean of the generated and rounded values
sd(pre_values)  #reports the sd of the generated and rounded values

pre_data_df <- data.frame(pre_values) #creates the dataframe for the pre-data

#creates the post-data dataframe
set.seed(5678) #sets seed of random values for post-data
post_seed <- rnorm(70, mean = 29, sd = 3) #generates random values for post-data
post_values <- round(post_seed) #rounds random values from generation
post_values #reports the generated post-data
mean(post_values) #reports the mean of the generated and rounded values
sd(post_values)  #reports the sd of the generated and rounded values

post_data_df <- data.frame(post_values) #creates the dataframe for the post-data

##--CREATING THE DATAFRAME--##################################################################################library(effectsize) #to get effect sizes easily

#creates the combined dataframe
created_data_df <- cbind(pre_data_df, post_data_df) #combines the column from the pre-data and post-data dataframes
created_data_df #reports the combined dataframe

##--FALSIFYING THE DATA--##################################################################################

#falsifying the post-data; only falsifying post-data as it would allow the hypothesis to become statistically significant 
fal_post_values <- (post_values - 2.5) #subtracts 2.5 from each case in the pre-data
fal_post_values #reports the falsified pre-data

fal_post_data_df <- data.frame(fal_post_values) #creates the dataframe for the falsified post-data
fal_post_data_df

falsified_data_df <- cbind(pre_data_df, fal_post_data_df) #combines the column from the pre-data and falsified post-data dataframes
falsified_data_df #reports the combined frabicated dataframe

##--SYNTHESIZING THE DATA--##################################################################################

#creates synthetic data of frabricated data
syn_data <- syn(falsified_data_df, seed = 1590) #creates the synthetic data

#replications in synthetic data
replicated_from_falsified <- replicated.uniques(syn_data, falsified_data_df) #checks for replicated values from falsified dataframe in synthetic dataframe
replicated_from_falsified #reports any replications

#creates an identifier that the synthetic data is not real
syn_data_identification <- sdc(syn_data, #synthetic dataset
                               falsified_data_df, #original dataset
                               label = "SYNTHETIC_DATA") #creates a column filled with "FAKE_DATE"

#puts the data in a dataframe
#to share the data, copy from dataframe and post with paper, poster, etc. on something like OSF (https://osf.io/) or Github (https://github.com/)
syn_data_df <- syn_data_identification$syn


##--T TEST AND COHEND D TESTING--##################################################################################

#paired t-test of pre- and post-data 
created_data_t <- t.test(created_data_df$pre_values,
                         created_data_df$post_values,
                         paired = TRUE)
created_data_t #reports the outcome of the paired t-test

#effect size for the created data
created_data_d <- cohens_d(created_data_df$pre_values,
                           created_data_df$post_values,
                           paired = TRUE)
created_data_d #reports the effect size for the created data

#paired t-test of pre-data and the falsified post-data
falsified_data_t <- t.test(falsified_data_df$pre_values,
                            falsified_data_df$fal_post_values,
                            paired = TRUE)
falsified_data_t #reports the outcome of the paired t-test

#effect size for the falsified data
falsified_data_d <- cohens_d(falsified_data_df$pre_values,
                              falsified_data_df$fal_post_values,
                              paired = TRUE)
falsified_data_d #reports the effect size for the created data

##--GENERATING THE FIGURE--##################################################################################

#creates a comparison figure for the original and synthetic datasets - SESBI intensity only
fal_and_syn_comparison_fig <- compare(
  syn_data, #synthetic data
  falsified_data_df, #original data
  vars = c("pre_values", 
           "fal_post_values"),
  print.coef = TRUE, # Print tables of estimates for original and synthetic data
  ncol = 2, #number of columns in the plot; should equal the number of variables 
  breaks = 10, #gap between the columns
  stat = "counts", #the raw count of the variable
  cols = c("#74b9ff", "#0984e3")#the colors of the bars, first color is original data and second is synthetic
)

fal_and_syn_comparison_fig <- fal_and_syn_comparison_fig$plots

fal_and_syn_comparison_fig <- fal_and_syn_comparison_fig +
  scale_y_continuous(expand = c(0, 0)) + # Forces the y-axis to start at zero
  theme_minimal_hgrid(12) # Applies a theme from the 'cowplot' package

fal_and_syn_comparison_fig <- fal_and_syn_comparison_fig +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        # Adjusts x-axis tick labels
        axis.title.x = element_blank()) + # Removes x-axis title
  labs(fill = "Dataset") #legend is named "Dataset"

fal_and_syn_comparison_fig #reports the figure


##--TESTING THE SYNTHESIZED DATA--##################################################################################

syn_data_t <- t.test(syn_data_df$pre_values,
                     syn_data_df$fal_post_values,
                     paired = TRUE)
syn_data_t

syn_data_d <- cohens_d(syn_data_df$pre_values,
                       syn_data_df$fal_post_values,
                       paired = TRUE)
syn_data_d
