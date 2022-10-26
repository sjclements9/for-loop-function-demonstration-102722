# For loop & function demonstration code
# Code Club Oct 27 2022
# SJC & LAB


# I still need to add better instructions/commentary and get rid of uneccesary things but this is mostly there

#  ******************* STEP 0: Basics of How to Make a For Loop and a Function **********************


# I am not sure what to write about this but I'll figure it out later
letters <- c('a', 'b', 'c', 'd', 'e') 
numbers <- 1:10
colors <- c('blue', 'red', 'green')
mat <- matrix(NA, nrow=length(letters), ncol=length(numbers)) # 5 x 10 matrix
arr <- array(NA, dim=c(length(letters), length(numbers), length(colors))) #5 x 10 x 3 array

# *** For Loop ***
# this is what you're referencing when you go to make a for loop...
length(letters)
1:length(letters)
q <- 1
letters[q]

# 3 examples to show how indexing works"
# 1. print each of the 5 letters in the letters vector
for (i in 1:length(letters)){ 
  print(letters[i])
}

# 2. Print each letter and each number in a matrix
for (i in 1:length(letters)){ # for each row in mat (1:5)
  for (j in 1:length(numbers)){ # for each column in mat (1:10)
    mat[i,j] <- paste(letters[i], numbers[j]) # fill mat row i column j with letter i and number j
  }
}
mat

# 3. print each letter, number, and color in an array
for (i in 1:length(letters)){
  for (j in 1:length(numbers)){
    for (salsa_toast in 1:length(colors)){
      arr[i,j,salsa_toast] <- paste(letters[i], numbers[j], colors[salsa_toast]) # fill row i, column j, and slice salsa_toast with letter i number j and color salsa_toast
    }
  }
}
arr

# *** Functions ***



# **************************************************************************************************************************************

# This example is pretty much simulating a data set for a population that you used to be able to do 
# telemetry on but can't anymore.

# Individual data

#set.seed(999) # maybe

# *********************** STEP 1: FAKE BANDING DATA ************************************
id <- 1:200 # individual id, sequenced 1-200
sex <- c(rep("m", 100), rep("f", 100)) # repeats "m" 100 times and "f" 100 times to represent sex
age <- sample(x=c("hy", "ahy"), size=200, prob=c(0.3, 0.6), replace=T) # samples "hy" (atch year) and "ahy" (after hatch year) with a 30% chance of hy and a 60% chance of ahy
tarsus <- rnorm(200, 50, 10) # tarsus length, from normal distribution
bill <- rnorm(200, 70, 15) # bill length, from normal distribution
site <- sample(x=c('Maine', 'Middle Earth', 'Australia', 'Tatooine', "Missouri"), size=200, prob=c(0.2, 0.3, 0.1, 0.1, 0.3), replace=T) # samples site, same way as age

bird_data <- data.frame(id=id, sex=sex, age=age, tarsus=tarsus, bill=bill, site=site) # make data frame
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

# I think a matrix model might actually be better here... But that would take more time and we probably don't feel like it
# if we did a matrix model we would skip part 3, use part 4 to have something to do with productivity, then put it in a matrix model for part 5
# which would then cover the same thing as part 3. 
# but here is some simluated telemetry survival data
time <- 8 # (8 weeks... or could be years, I don't know)
ch <- matrix(0, nrow(bird_data), time) # capture history matrix with a row for each individual and a column for each time step
dim(ch) # should be 200 x 8
ch[,1] <- 1 # everyone is alive at the beginning of the study

for (i in 1:nrow(bird_data)){
  for (t in 2:time){ # note that this is 2:8 because we already know what is happening at t=1
    if (ch[i, t-1]==1){ # if the individual is alive at time step t-1
      ch[i,t] <- rbinom(1,1,prob=bird_data$ex_surv[i]) # then use the assigned survival probability for a bernoulli trial
    }
  }
}

# then if you want can add how many time steps they're expected to live
hist(rowSums(ch))
hist(rowSums(ch[which(bird_data$site=='Missouri'),]))
hist(rowSums(ch[which(bird_data$site=='Middle Earth'),]))






# **** PART 4: SOMETHING YOU NEED A FUNCTION FOR ****
# something about landcover

# ****** PART 5: PUT TOGETHER SOME KIND OF DATA SET, I GUESS *****
# put together simulated capture recapture data sets with a few different parameter situations
# or maybe not

# 