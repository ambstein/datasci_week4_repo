# What packages do we need?
library(tidyverse)

# Character data (strings)

# Just reading/cleaning data for the example...we come back to pivoting later
ds <- read_csv('data_cleaned/vocab_annoying_headers.csv') %>% 
  mutate(id = "Jonah", dob = as.Date("2017-08-30"), .before = "age_12")
ds <- ds %>% pivot_longer(-(id:dob), names_to = "age", values_to = "word", values_drop_na = T, names_prefix = "age_") %>%
  select(-id, -dob)

# Jonah's first words

ds$word

# Length, containing strings, counting strings

ds %>% mutate(word_length = str_length(word), 
              has_b = str_detect(word, "b"), 
              num_b = str_count(word, "b"))

# Matching patterns

ds %>% mutate(contains_punct = str_detect(word, "[:punct:]")) 
ds %>% mutate(word_cleaned = str_remove_all(word, "[:punct:]")) 
ds %>% mutate(word_cleaned = str_replace_all(word, "[:punct:]"," ")) 
ds %>% mutate(vowel_count = str_count(word, "[aeiou]")) 

fnames <- list.files(path="data_raw", full.names = T)
str_split(fnames, "/") 
str_replace_all(fnames, "data_raw/|.csv|vocab","") 
str_extract_all(fnames, "\\d{2}.\\d{1}|\\d{2}", simplify = T) 
str_extract_all(fnames, "\\.[:alpha:]{1,4}", simplify = T) 
str_extract_all(ds$word, boundary("word"), simplify = T) 

# Combining strings

age <- seq(from = 12, to = 24, by = .5) 
paste0("data_raw/vocab", age, ".csv") 
str_glue("data_raw/vocab{age}.csv") 

# Logicals from text

contains_b <- str_detect(ds$word, "b") # TRUE if the word contains b
contains_b 


typeof(contains_b) #returns type of variable for each word/row that contains b

sum(contains_b) # sum num of rows/vwords containing b
mean(contains_b) 
!contains_b 
ds$word[contains_b] 
ds %>% mutate(contains_b = contains_b) 
ds %>% filter(contains_b) 
ds %>% mutate(b_and_vowels = contains_b & str_detect(word, "[aeiou]"))
ds <- ds %>% mutate(b_or_startswithd = contains_b | str_starts(word, "d"))  

# Vectors

v <- c(1, 2, 3, 4, 5)
v
typeof(v)
length(v)
v[1] # Use brackets to access a vector element
v[3]
v <- c(v, 6, 7, 8) # use c to add items
v <- c(1, 2, 3, "4", "5", "6") 
v # You cannot mix types in a vector
v <- c(x = 1, y = 2, z = 3) # Named vector
v

v['y'] #show us the element in vector v that's labeled 'y'

files <- list.files(path = "data_raw")

#returns the very first name in the file
files[1] # Character vector
files[2]

#how many files
length(files) 

# Lists

lt <- list(x = 1, y = "3", z = 1:5)
lt
lt$z # Access named list elements with $
lt[[1]] # Use double brackets to get list elements "give me 'x'"
lt[[3]] <- 5:7 # create the 3rd element of the list as being 5-7

# List of lists
lt <- list(lt1 = lt, lt2 = lt, lt3 = lt)
lt$lt1 # pull out all 3 pieces of info from the original list lt1
lt$lt2 # will print all 3 elements within the list

# if we ran glm in star wars data -- results are stored in lists
#  can use $ to pull data from data analysis outputs (like output of lm())
# all tibbles are lists; dataframes are not necessarily lists

model <- lm(height ~ mass, data = starwars)
typeof(model)
model$coefficients


# Working with dates and times

dates <- tibble(dates1 = c("2017-09-01", "2016-09-01", "2015-09-01", "2014-09-01"),
                dates2 = c("9/01/2018", "09/01/2017", "9/1/2016", "9/1/2015"),
                dates3 = c("March 1, 2019", "May 1, 2018", "January 20, 2017", "February 4, 2020"))

# Parsing dates

dates
dates %>% mutate(dates1_parsed = ymd(dates1)) #please recognize this as a date that's stored as ymd (year month date)
dates %>% mutate(dates2_parsed = mdy(dates2)) #recognize this as a data that's stored as myd (but will always display as ymd)
dates %>% mutate(dates3_parsed = parse_date(dates3, format = "%B %d, %Y")) #parse this as a datetime but format is "month day, year"
# %B means month in regular expression / internal language
?parse_date #will give info about how to specify things

# Time is weird

time_length(mdy("05-27-2022")-mdy("05-27-1983"), "years") #give us the length of time for datetime that's mdy format, subtract another mdy piece of info and give us that time in years
time_length(as.period(interval(mdy("05-27-1983"), mdy("05-27-2022"))), "years") # gives output in calendar time rather than physical time
time_length(as.period(interval(mdy("05-27-1983"), mdy("05-28-2022"))), "years") 

# Age math

birth_date <- ymd("2001-09-01", "2000-09-01") 
test_date <- ymd("2020-09-01", "2019-09-01") # these should be exactly 19 years apart
(test_date-birth_date)/365.25 #should give us time difference in days ... but its different because there was a leap year in 2020
as.period(interval(start = birth_date, end = test_date), unit = "years") #if we look at time diff as a calendar period, then it'll give us 19 years exactly (no leap year mess)


test_date2 <- test_date - years(1) + days(7) #gives us the same piece of info as above code (exact correct year & day)
test_date2
(test_date2-birth_date)/365.25
as.period(interval(start = birth_date, end = test_date2), unit = "years")

# Time Zones

t1 <- as_datetime("2022-01-27 10:39:28", tz = "America/Los_Angeles") #PST
t1
tz(t1)
t2 <- as_datetime("2022-01-27 13:39:28", tz = "America/New_York") #EST
t2-t1 # returns a 0 because these times are aligned accounting for time zone
t3 <- as_datetime("2021-10-15 10:39:28", tz = "America/Los_Angeles") 
t3
t4 <- as_datetime(now())
t4 
as.period(interval(start = t1, end = t4), unit = "days") # look at interval btw t1 and t4, using PST?
with_tz(t4, "America/New_York") 
force_tz(t4, "America/New_York") #this will force the time zone to be exactly what you want it to be (basically just changing the time zone, not calculating diff btw time zone)

# Factors represent categories

x <- c(1, 1, 2, 3, 1, 4) 
x <- factor(x, levels = c(1,2,3), labels = c("rarely", "neutral", "frequently")) #create a factor from vector x. the levels are 1, 2, and 3; anything outside of 1, 2, 3 will be cat as missing
# viewing new factor
x #restrict possible values to whatever the set levels are
#can treat as continuous var using as.numeric()

# Factors

ds <- starwars
unique(ds$eye_color) # 15 unique eye colors 
ds <- ds %>% mutate(eye_f = factor(eye_color)) 
fct_count(ds$eye_f, sort = T) %>% slice_head(n = 5) #create a factor from eye_color variable; sort = T -- will give frequency table where highest/most common is at the top
fct_count(ds$eye_f, prop = T) %>% slice_head(n = 5) #slice_head() being used to see only 5 rows of data for demo -- prop = T -- will give N and proportion of frequency

# Setting factors levels

ds <- ds %>% select(name, eye_f) #viewing 2 vars from dataset using select()
ds 
ds %>% mutate(eye_f2 = factor(eye_f, levels = c("blue", "brown"))) # creating factor eye_f2 with levels of blue and brown only
ds %>% mutate(eye_f2 = factor(eye_f, levels = c("blue", "brown"), labels = c("bl","br"))) # doing same as above but including labels bl and br in the output
ds %>% mutate(eye_f2 = fct_lump(eye_f, n = 3)) #fct_lump() in this code is combining less fqtly seen levels into one category called "other"
ds %>% mutate(eye_type = fct_collapse(eye_f, human = c("blue", "brown"), nonhuman = c("yellow", "red"))) # creating a new variable eye_type from fct_collapse() function
#  where certain levels get assigned a new category

# Reordering factors levels

fct_count(ds$eye_f) #ordering in order in which they're being observed
fct_count(fct_rev(ds$eye_f)) # reversing above
fct_count(fct_infreq(ds$eye_f)) #sorted in descending order from most to least fqtly occurring
fct_count(factor(ds$eye_f, levels = c("red", "brown", "blue"))) # creating a new factor where we only include levels red, brown, & blue (everything else = missing)


### Tidying Data 
## to make it maximally *machine readable*

# Data Transformations

# Multiple Columns -> One Variable 

ds <- read_csv('data_cleaned/vocab_annoying_headers.csv') %>% 
  mutate(id = "Jonah", dob = as.Date("2017-08-30"), .before = "age_12") #creating a data frame structured by age, in wide format
ds %>% print(n = 25) 
ds %>% pivot_longer(-(id:dob), names_to = "age", values_to = "word") #transforms data into long format [-(id:dob) says to keep those columns as they are]
ds %>% pivot_longer(-(id:dob), names_to = "age", 
                    names_prefix = "age_", values_to = "word") # this tells R to give just the age, without "age" (so just 12.5)
ds %>% pivot_longer(-(id:dob), names_to = "age", 
                    values_to = "word", values_drop_na = T, names_prefix = "age_") # not sorted by age but has words and age in long form

# One Column -> Multiple variables

ds <- read_delim(list.files(path = "data_headers", full.names = T),
                 col_names = c("field", "value"), col_types = c("cc"), # only reading in the first chunk of info containing participant info
                 delim = " ", n_max = 8, id = "filename") # if there's a space btw two info (age male --> field = age, value = male)
print(ds)

ds <- ds %>% pivot_wider(id_cols = "filename",
                         names_from = "field", 
                         values_from = "value") 
ds

# One Variable -> Multiple Variables

ds %>% separate_wider_delim(filename, delim = "/", names = c("dir", "file")) #separate each piece of info in the file name variable, the delimited is "/". 
# first column will be directory, second column will be file
print(ds)

# Multiple Variables -> One Variable

ds %>% unite(col = "block_speed_condition", Block, speed, condition, sep = "_") # telling R that we want Block AND speed AND condition info in one variable, dropping the original columns/vars

# Binding Columns

star_wars_numeric <- starwars %>% select(where(is.numeric)) #87x3 # selecting numeric data
star_wars_numeric
star_wars_char <- starwars %>% select(where(is.character)) #87x8 #selecting character data
star_wars_char
star_wars_combined <- bind_cols(star_wars_numeric, star_wars_char) #87x11 #then combining the two together
star_wars_combined # data do have to be the same number of columns

# Binding Rows

s1 <- starwars %>% slice_sample(n = 2) #2x14  #helpful for longitudinal data, where too many participants would make visualization difficult, so choose only a subset
s1
s2 <- starwars %>% slice_sample(n = 2) #2x14
s2
s3 <- starwars %>% slice_sample(n = 2) #2x14
s3
star_wars_combined <- bind_rows(s1, s2, s3) #6x14
star_wars_combined

# Joining by key value

fnames <- list.files(path = "data_headers", full.names = T) 
header <- read_delim(fnames, col_names = c("field", "value"), col_types = c("cc"), delim = " ", n_max = 7, id = "filename") %>% pivot_wider(id_cols = "filename", names_from = "field", values_from = "value")
trials <- read_tsv(fnames, col_names = c("trial_num", "speed_actual", "speed_resp", "correct"), skip = 8, id = "filename")
header 
trials 
left_join(header, trials, by = "filename") 
left_join(slice_sample(header, n = 5), trials, by = "filename") 
left_join(slice_sample(trials, n = 5), header, by = "filename") 

