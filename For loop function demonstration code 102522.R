# For loop & function demonstration code
# Code Club Oct 27 2022
# SJC & LAB

library(dplyr)
library(furrr)
library(landscapeR)
library(landscapetools)
library(landscapemetrics)
library(raster)

# Table of contents:
# 0: Basics of How to Make a For Loop and a Function
# 1: Creating fake banding data
# 2: Exploring the data and adding new covariates
# 3: Simulating telemetry data using a for loop 
# 4: Using functions to process code faster
# 5: Using functions and for loops together

#  ******************* STEP 0: Basics of How to Make a For Loop and a Function **********************

# First, let's start by 
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

# 3. print each letter, number, and color in an array (a three-dimensional matrix)
for (i in 1:length(letters)){ # for each row i in arr (1:5)
  for (j in 1:length(numbers)){ # for each row j in arr (1:10)
    for (k in 1:length(colors)){ # for each slice k in arr (1:3)
      arr[i,j,k] <- paste(letters[i], numbers[j], colors[k]) # fill row i, column j, and slice k with letter i number j and color k
    }
  }
}
arr

# *** Functions ***
# Functions are an essential part of R, and chances are you're already familiar with them
mean(x = c(1,2,3)) #here's a function!

# We can tell that it's a function because it has parentheses after it
mean() # won't run, but note the parentheses after mean

# Functions take arguments, which are objects that we supply to the function by placing them inside the parentheses
# We can pull up the help page for the mean function to determine which arguments that the function will accept
?mean

# The mean function takes three arguments: x, trim, and na.rm
# we can either supply those arguments in order:
mean(c(1,2,3), 0.2, TRUE)

# Or we can supply them by name, in which case they're allowed to be out of order
mean(x = c(1,2,3), na.rm = TRUE, trim = 0.2)

# R and its packages will supply thousands of functions that you can use, but you can also build your own!

example_function <- function(x){ #list the arguments of the function inside the parentheses
  return(x + 1) # Note the return function- this tells the function which value is the output of the function
} #the code which forms the function goes inside the brackets

example_function(1)
example_function(3)
example_function(5)

# We can use functions to run an operation on many inputs at once

example_function(1:5) #this is what we call a vectorized function: vector in, vector out. 
#They're one of the fastest ways to process data in R

# However, not all functions can be easily vectorized. Maybe you need to use a list as an input, for example.
# In this case, base R supplies a series of apply functions (mainly lapply, sapply, and mapply)
lapply(list(1,2,3,4,5), example_function) # lapply returns values as a list
sapply(list(1,2,3,4,5), example_function) # sapply returns values as a vector if possible
mapply(x = c(1,2,3), y = c(4,5,6), z = c(7,8,9), #mapply takes multiple arguments
       FUN = function(x, y, z){x + y +z}) # notice how we provided this function without giving it a name first? 
#This is called an "anonymous function", and it is a very handy tool for making functions that you only intend to use once

# Functions can frequently be used interchangably with for loops
# for example, this function and this for loop do the same thing

t <- 1:5

for(i in 1:length(t)){
  t[i] <- t[i] + 1
}

t

example_function <- function(x){ 
  return(x + 1) 
}

t <- example_function(1:5)

t

# *********************** STEP 1: CREATING FAKE BANDING DATA ************************************
set.seed(999)

id <- 1:200 # individual id, sequenced 1-200
sex <- c(rep("m", 100), rep("f", 100)) # repeats "m" 100 times and "f" 100 times to represent sex
age <- sample(x=c("hy", "ahy"), size=200, prob=c(0.3, 0.6), replace=T) # samples "hy" (atch year) and "ahy" (after hatch year) with a 30% chance of hy and a 60% chance of ahy
tarsus <- rnorm(200, 50, 10) # tarsus length, from normal distribution with mean = 50 and sd = 10
bill <- rnorm(200, 70, 15) # bill length, from normal distribution with mean = 70 and sd = 15
site <- sample(x=c('Maine', 'Middle Earth', 'Australia', 'Tatooine', "Missouri"), size=200, prob=c(0.2, 0.3, 0.1, 0.1, 0.3), replace=T) # samples site, same way as age

bird_data <- data.frame(id=id, sex=sex, age=age, tarsus=tarsus, bill=bill, site=site) # make data frame
head(bird_data) # look at data
dim(bird_data)

# ********************* STEP 2: Exploring the data and adding new covariates *************************
# new column: fake body condition index 
bird_data$condition <- bird_data$bill + bird_data$tarsus # add bill length + tarsus to get body condition
quantile(bird_data$condition, probs=0.25) # 25% of birds have body conditions less than 106.7693 (save for later)

# calculate mean body condition and number of hy individuals at each site
site_data <- data.frame(matrix(NA, nrow=length(unique(bird_data$site)), ncol=4)) # empty data frame for site data
names(site_data) <- c('site', 'mean_condition', 'count', 'hy_count') # add column names
site_data$site <- unique(bird_data$site) # fill in site names in site column

# determine the mean condition score, total number of individuals, and number of hatch year birds at each site
# ** FOR LOOP **
for (i in 1:length(site_data$site)){ # i = site
  site_data$mean_condition[i] <- bird_data %>% # fill every space [i] in the mean_condition column of site_data with...
    filter(site == site_data$site[i]) %>% # get all data from site i from bird_data
    pull(condition) %>% # get all data in condition column
    mean() # get the mean of all data in condition column in site i
    
  site_data$count[i] <- bird_data %>% # fill every space [i] in the mean_condition column of site_data with...
    filter(site == site_data$site[i]) %>% # get all data from site i in bird_data
    nrow() # get number of rows in site i (which is the total number of birds in the site)

  site_data$hy_count[i] <- bird_data %>% # fill every space [i] in the hy_count column of site_data with...
    filter(site == site_data$site[i],
           age == "hy") %>% # get all data from site i with age hy
    nrow() # get number of rows in site i with age hy (total number of hatch year birds in the site)
}
site_data # look at data

## Adding expected survival rates

# we are going to pretend that we know some survival rates based on a previous study
#     1. baseline survival rate for an adult bird is 0.7
#     2. But for males it's 0.1 less than females
#     3. If their body condition is less than 107 survival drops by 0.3
#     4. And survival is 0.2 less in middle earth than the baseline


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


# ************************* PART 3 : Simulating telemetry data using a for loop *************************
# Now let's simulate some telemetry survival data using the expected survival rates we calculated above

time <- 8 # (8 weeks)
ch <- matrix(0, nrow(bird_data), time) # capture history matrix with a row for each individual and a column for each time step
dim(ch) # should be 200 x 8
ch[,1] <- 1 # everyone is alive at the beginning of the study

for (i in 1:nrow(bird_data)){
  for (t in 2:time){ # note that this is 2:8 because we already know what is happening at t=1
    if (ch[i, t-1]==1){ # if the individual is alive at time step t-1
      ch[i,t] <- rbinom(1, 1, prob=bird_data$ex_surv[i]) # then use the assigned survival probability for a bernoulli trial (weighted coin flip)
    }
  }
}

# then if you want can add up how many time steps they're expected to live
hist(rowSums(ch)) #Most birds don't make to week 2
hist(rowSums(ch[which(bird_data$site=='Missouri'),]))
hist(rowSums(ch[which(bird_data$site=='Middle Earth'),]))

# ************************* PART 4: Using functions to process code faster *************************
# In certain circumstances, your code may take a prohibitively long time to run. This is especially true of landscape analyses
# We can make our code run faster using a unique application of functions called parallel processing

#Let's demonstrate this by analyzing the landscapes in which our birds live. 
#We'll begin by simulating a landscape raster for each bird. The proportion of forest will depend on the site

create_forest_maps <- function(x){
  percentage_forest <- x %>% 
    recode('Maine' = 0.8, #almost all forest in Maine
           'Middle Earth' = 0.5, 
           'Australia' = 0.15, 
           'Tatooine' = 0.01, #almost no forest on Tatooine
           "Missouri" = 0.3)
  
  m <- matrix(0, 150, 150)
  r <- raster(m)
  rr <- makePatch(r, size=nrow(m)*ncol(m)*percentage_forest, rast=TRUE)
  
  return(rr)
}
#we could create these landscapes one at a time

bird_data$landscapes <- lapply(bird_data$site, create_forest_maps)

#alternatively, let's use a parallel processing package (furrr) to do it quickly
plan(multisession) #run this if you're using a PC
# plan(multicore) #run this if you're using a Mac

bird_data$landscapes <- future_map(bird_data$site, create_forest_maps, 
            .options = furrr_options(seed = TRUE)) #tells the machine to generate maps randomly for each instance

#The parallel code ran 6.08 times faster on Liam's PC
plan(sequential) #run this to close parallel processing

#let's take a look at what one of these simulated landscapes looks like. Remember, 1 is forest and 0 is everything else
show_landscape(bird_data$landscapes[[1]])

# Now, to determine how these landscapes might affect our birds. Let's calculate a metric of fragmentation 
#(aggregation index) for each landscape using functions.

bird_data$aggregation_index <- lapply(bird_data$landscapes, function(x){ #this code executes quickly, so parallel processing isn't necessary
  lsm_c_ai(x) %>% 
    filter(class == 1) %>% 
    pull(value) %>% 
    return()
})

# ^ this last step could have been done with a for loop as well. Frequently, for loops vs. functions comes down to personal preference
# However, functions tend to be more computationally efficient when dealing with large datasets,
# and are the only way to access parallel processing in R



# ************************* PART 5: Using functions and for loops together *************************
# How would the encounter histories look if we changed the base survival rate?

capture_history_generator <- function(base_survival, bird_data){
  
  # So we're going to add a column for expected survival and figure out what it will be for each
  bird_data$ex_surv <- base_survival # baseline survival column
  
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
    if(bird_data$ex_surv[i] < 0){ # survival can't be less than 0
      bird_data$ex_surv[i] <- 0
    }
  }

  time <- 8 # (8 weeks)
  ch <- matrix(0, nrow(bird_data), time) # capture history matrix with a row for each individual and a column for each time step
  ch[,1] <- 1 # everyone is alive at the beginning of the study
  
  for (i in 1:nrow(bird_data)){
    for (t in 2:time){ # note that this is 2:8 because we already know what is happening at t=1
      if (ch[i, t-1]==1){ # if the individual is alive at time step t-1
        ch[i,t] <- rbinom(1, 1, prob=bird_data$ex_surv[i]) # then use the assigned survival probability for a bernoulli trial (weighted coin flip)
      }
    }
  }
  return(ch)
}

# We can now generate capture histories while changing the base survival rate as desired, 
# and watch how the resulting capture histories change
ch_100pct <- capture_history_generator(base_survival = 1, bird_data = bird_data); hist(rowSums(ch_100pct))
ch_50pct <- capture_history_generator(base_survival = 0.5, bird_data = bird_data); hist(rowSums(ch_50pct))
ch_30pct <- capture_history_generator(base_survival = 0.3, bird_data = bird_data); hist(rowSums(ch_30pct))

# This modularity is one of the major advantages of functions
