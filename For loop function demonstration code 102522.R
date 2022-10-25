# For loop & function demonstration code
# Code Club Oct 27 2022
# SJC & LAB


# This exercise is pretty much simulating a telemetry data set for a population 
# where you have individual information but not any more telemetry 
# your funding got cut, sorry, now you have to do a simulation

# Individual data

#set.seed(999) # makes it so everyone gets the same numbers

# *********************** STEP 1: FAKE BANDING DATA ************************************
id <- 1:200 # individual id, sequenced 1-200
sex <- c(rep("m", 100), rep("f", 100)) # repeats "m" 100 times and "f" 100 times to represent sex
age <- sample(x=c("hy", "ahy"), size=200, prob=c(0.3, 0.6), replace=T) # samples "hy" (atch year) and "ahy" (after hatch year) with a 30% chance of hy and a 60% chance of ahy
tarsus <- rnorm(200, 50, 10) # tarsus length, from normal distribution
bill <- rnorm(200, 70, 15) # bill length, from normal distribution
site <- sample(x=c('Maine', 'Middle Earth', 'Australia', 'Tatooine', "Missouri"), size=200, prob=c(0.2, 0.3, 0.1, 0.1, 0.3), replace=T) # samples site, same way as age

bird_data <- data.frame(id=id, # make into data frame
                        sex=sex,
                        age=age,
                        tarsus=tarsus,
                        bill=bill,
                        site=site)
head(bird_data) # look at data


# ********************* STEP 2: GET MORE INFORMATION FROM DATA *************************
# new column: fake body condition index 
bird_data$condition <- bird_data$bill + bird_data$tarsus # add bill length + tarsus to get body condition
quantile(bird_data$condition, probs=0.25) # find worst 25% of body condition #106 (save for later)

# calculate mean body condition and number of hy individuals at each site
site_data <- data.frame(matrix(NA, nrow=length(unique(bird_data$site)), ncol=4)) # empty data frame for site data
names(site_data) <- c('site', 'mean_condition', 'count', 'hy_count') # add column names
site_data$site <- unique(bird_data$site) # fill in site names in site column

# fill in site data 
# ** FOR LOOP **
for (i in 1:length(site_data$site)){ # i = site
  site_data$mean_condition[i] <- mean(bird_data$condition[which(bird_data$site==site_data$site[i])])
  site_data$count[i] <- length(which(bird_data$site==site_data$site[i]))
  site_data$hy_count[i] <- length(which(bird_data$age[which(bird_data$site==site_data$site[i])]=="hy"))
}
site_data # look at data

# add survival information here.....
# we are going to pretend that we know some survival rates based on a previous study
#.... you would never actually know all this but whatever it's all fake anyway
#     1. baseline survival rate for an adult bird is 0.7
#     2. But for males it's 0.1 less than females
#     3. If their body condition is less than 107 survival drops by 0.3
#     4. And survival is 0.2 less in middle earth than the baseline
# small male birds from middle earth are fucked

# So we're going to add a column for expected survival and figure out what it will be for each
bird_data$ex_surv <- 0.7 # baseline survival column

# ** FOR LOOP **
# each individual is different so we're looping through individuals
for (i in 1:nrow(bird_data)){ # i = individual bird
  if (bird_data$sex[i]=='m'){
    bird_data$ex_surv[i] <- bird_data$ex_surv[i]-0.1
  }
  if (bird_data$condition[i]<106){
    bird_data$ex_surv[i] <- bird_data$ex_surv[i]-0.3
  }
  if(bird_data$site[i]=='Middle Earth'){
    bird_data$ex_surv[i] <- bird_data$ex_surv[i]-0.2
  }
}
hist(bird_data$ex_surv)


# **** PART 3 : SOMETHING YOU NEED A FOR LOOP FOR *****
# simulate telemetry data (or something else with survival over time. Gonna get back to this later)


# **** PART 4: SOMETHING YOU NEED A FUNCTION FOR ****
# something about landcover

# ****** PART 5: PUT TOGETHER SOME KIND OF DATA SET, I GUESS *****
# put together simulated capture recapture data sets with a few different parameter situations

# 