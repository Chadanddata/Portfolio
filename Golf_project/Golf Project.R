''' Golf Course Investment project
 An investment firm has aproached our consulting firm about identifying an area to open up a new golf course. 
They Want to find an area that has low competiton and greater market size. Aditionaly They want to identify an aproximate cost to start 
the course as well as forecast for when they can expect to see a return on investment.

We will deliver a recomendation of no less than three loacations for them to build at, including cost and income forecast, and local competition. 
To determine best locations we will need to identify an area that has a growing or consistent population, a population that matches golfer demographics,
amenities that can be offered that competitors dont possess. 

Data sourced from user seanconeys US_GOLF_COURSES repository on github. 
Golfer demographics <- https://www.fogolf.com/828298/golf-industry-statistics-by-demographics-and-facts-2024/
County population from United States Census Bureau "County Population by Characteristics: 2020-2024
'''


library(tidyverse)
library(data.table)
df <- read.csv('golf_courses.csv', row.names = NULL)
dim(df)                          # There are 17,417 courses with 36 attributes
head(df)



#Golf Data Frame
#Feature Selection and identifying data issues

golf <- df |> select('row.names', 'Street', 'City', 'Zip2', 'Description', 'Public.Private', 'Year.Built', 'Annual.Rounds',
                     'Superintendent', 'Dress.Code', 'Fee.Weekend', 'Fee.Weekday', 'Earliest.Tee.Time')

new.names <- c('Name', 'City', 'State', 'County', 'Public.Private', 'Year.Built', 'Annual.Rounds (estimated)', 'Season', 'Guest.Policy',
               'Fee.Weekend', 'Fee.Weekday', 'Tee.Time.Reservations', 'Holes')

colnames(golf) <- new.names
golfdf <- data.frame(golf)


# Remove (estimated) from Rounds per year
unique(golfdf$Annual.Rounds..estimated.)
golfdf <- golfdf |> mutate(value_clnd = str_replace_all(Annual.Rounds..estimated., "\\s*\\([^)]*\\)", ""),
                           value_clnd = str_trim(value_clnd),
                           value_clnd = str_replace(value_clnd, ",", ""),
                           Annual_rounds_estimate = as.numeric(value_clnd), 
                           Tee.Time.Reservations = word(Tee.Time.Reservations, 1)) |>
                     select(Name, City, State, County, Public.Private, Year.Built, Season, Guest.Policy, Fee.Weekend, Fee.Weekday, Tee.Time.Reservations,
                            Holes, Annual_rounds_estimate)
                    


# Change fees columns to numeric
golfdf$Fee.Weekend <- as.numeric(str_remove_all(golfdf$Fee.Weekend,"[$]"))
golfdf$Fee.Weekday <- as.numeric(str_remove_all(golfdf$Fee.Weekday,"[$]"))                                       


# Season has too many values

golfdf <- golfdf |> mutate(szn_clnd = str_replace_all(Season, "\\s*\\d+\\s*", " ")) |>
                    mutate(szn_clnd = str_squish(szn_clnd))
 





# County population CSV
''' Started with 1,811,040 cells of data, 18,864 rows, and 96 columns
Removed the years 2020-2024 to hide pandemic impacted growth, and so most recent year population change
Removed unneccessary columns 
Aggregated data instead of 20+ Columns for age groups increasing by 5 created three groups (children, Young adult, retirees)
New data contains 3,144 Rows of 11 columns
'''
county_pop <- read.csv('cc-est2024-agesex-all.csv')
county_df <- county_pop |> filter(YEAR %in% c(5,6)) |>
                        mutate(children = UNDER5_TOT + AGE513_TOT + AGE1417_TOT,
                               Young_adults = AGE1824_TOT + AGE2529_TOT,
                               Middleage_adults = AGE3034_TOT + AGE3539_TOT + AGE4044_TOT+ AGE4564_TOT) |>
                        select(STNAME, CTYNAME, YEAR, POPESTIMATE, children, Young_adults, Middleage_adults, AGE65PLUS_TOT, MEDIAN_AGE_TOT)
                        
          


# Golf and Population DFs
course_count <- golfdf |> group_by(State, County) |> count(County)
course_count

county_df <- county_df |> mutate(County = word(CTYNAME, 1))
course_count <- course_count |> mutate(County = word(County, 1))
county_df <-county_df |> rename(State = STNAME)

course_count$State <- state.name[match(course_count$State, state.abb)]
golfdf$State <- state.name[match(golfdf$State, state.abb)]

course_population <- left_join(county_df, course_count, by = c("State", "County"))

all_data <- full_join(county_df, golfdf, by = c("State", "County") )



# Data Analysis

# Number of Courses by County / State
course_count

# Smallest size county that has a golfcourse
smallest_county <- course_population |> filter(n >= 1) |> select(POPESTIMATE, State, County, n)
min(smallest_county$POPESTIMATE)    
smallest_county |> filter(POPESTIMATE == 698)
# Hooker County Nebraska has 2 golf courses and only 698 people 


# Average number of courses per capita
options(scipen = 9999)
crse_pop_ratio = (smallest_county$POPESTIMATE / smallest_county$n)
mean(crse_pop_ratio)
#On Average there is 1 golf course for every 19,684 people


# Find average weekday and weekend fees for private vs public, 9 vs 18 holes, State
private_courses <- golfdf |> filter(Public.Private == 'Private')
public_courses <- golfdf |> filter(Public.Private == 'Public')

mean(private_courses$Fee.Weekday, na.rm = TRUE)  # $47.41
mean(public_courses$Fee.Weekday, na.rm = TRUE)   # $24.42
mean(private_courses$Fee.Weekend, na.rm = TRUE)  # $52.21
mean(public_courses$Fee.Weekend, na.rm = TRUE)   # $28.32

mean(private_courses$Fee.Weekday[private_courses$Holes == 9], na.rm = TRUE)     # $18.08 Private club, weekday, 9 holes
mean(private_courses$Fee.Weekday[private_courses$Holes == 18], na.rm = TRUE)    # $50.56 Private club, weekday, 18 holes
mean(private_courses$Fee.Weekday[private_courses$Holes > 18], na.rm = TRUE)     # $59.40 Private club, weekday, more than 18 holes
mean(private_courses$Fee.Weekend[private_courses$Holes == 9], na.rm = TRUE)     # $20.79 Private club, weekend, 9 holes
mean(private_courses$Fee.Weekend[private_courses$Holes == 18], na.rm = TRUE)    # $55.79 Private club, weekend, 18 holes
mean(private_courses$Fee.Weekend[private_courses$Holes > 18], na.rm = TRUE)     # $64.40 Private CLub, Weekend, more than 18 holes

mean(public_courses$Fee.Weekday[public_courses$Holes == 9], na.rm = TRUE)       # $13.90 Public club, weekday, 9 holes
mean(public_courses$Fee.Weekday[public_courses$Holes == 18], na.rm = TRUE)      # $27.65 Public club, weekday, 18 holes
mean(public_courses$Fee.Weekday[public_courses$Holes > 18], na.rm = TRUE)       # $31.48 Public club, weekday, more than 18 holes
mean(public_courses$Fee.Weekend[public_courses$Holes == 9], na.rm = TRUE)       # $15.67 Public club, weekend, 9 holes
mean(public_courses$Fee.Weekend[public_courses$Holes == 18], na.rm = TRUE)      # $32.40 Public Club, weekend, 18 holes
mean(public_courses$Fee.Weekend[public_courses$Holes > 18], na.rm = TRUE)       # $36.24 Public club, weekend, more than 18 holes

state_avg_weekday_fee <- golfdf |> group_by(State) |> summarise(avg_fee = mean(Fee.Weekday, na.rm = TRUE))
state_avg_weekend_fee <- golfdf |> group_by(State) |> summarise(avg_fee = mean(Fee.Weekend, na.rm = TRUE))

# Find average rounds by private/public, State, Guest Policy, Tee Time reservations
private_courses |> filter(Holes == 9) |>
                   mutate(Annual_rounds_estimate = ifelse(is.na(Annual_rounds_estimate), mean(Annual_rounds_estimate, na.rm = TRUE), Annual_rounds_estimate)) |>
                   summarise(avg_9_rnds = mean(Annual_rounds_estimate))

private_courses |> filter(Holes == 18) |>
  mutate(Annual_rounds_estimate = ifelse(is.na(Annual_rounds_estimate), mean(Annual_rounds_estimate, na.rm = TRUE), Annual_rounds_estimate)) |>
  summarise(avg_9_rnds = mean(Annual_rounds_estimate))

private_courses |> filter(Holes > 18) |>
  mutate(Annual_rounds_estimate = ifelse(is.na(Annual_rounds_estimate), mean(Annual_rounds_estimate, na.rm = TRUE), Annual_rounds_estimate)) |>
  summarise(avg_9_rnds = mean(Annual_rounds_estimate))

public_courses |> filter(Holes == 9) |>
  mutate(Annual_rounds_estimate = ifelse(is.na(Annual_rounds_estimate), mean(Annual_rounds_estimate, na.rm = TRUE), Annual_rounds_estimate)) |>
  summarise(avg_9_rnds = mean(Annual_rounds_estimate))

public_courses |> filter(Holes == 18) |>
  mutate(Annual_rounds_estimate = ifelse(is.na(Annual_rounds_estimate), mean(Annual_rounds_estimate, na.rm = TRUE), Annual_rounds_estimate)) |>
  summarise(avg_9_rnds = mean(Annual_rounds_estimate))

public_courses |> filter(Holes > 18) |>
  mutate(Annual_rounds_estimate = ifelse(is.na(Annual_rounds_estimate), mean(Annual_rounds_estimate, na.rm = TRUE), Annual_rounds_estimate)) |>
  summarise(avg_9_rnds = mean(Annual_rounds_estimate))

golfdf |> filter(Guest.Policy != 'Closed') |> 
          mutate(Annual_rounds_estimate = ifelse(is.na(Annual_rounds_estimate), mean(Annual_rounds_estimate, na.rm = TRUE), Annual_rounds_estimate)) |>
          summarise(avg_9_rnds = mean(Annual_rounds_estimate),
                    avg_fee = mean(Fee.Weekend, na.rm = TRUE) + mean(Fee.Weekday, na.rm = TRUE) / 2)

golfdf |> filter(Guest.Policy == 'Closed') |> 
  mutate(Annual_rounds_estimate = ifelse(is.na(Annual_rounds_estimate), mean(Annual_rounds_estimate, na.rm = TRUE), Annual_rounds_estimate)) |>
  summarise(avg_9_rnds = mean(Annual_rounds_estimate),
            avg_fee = mean(Fee.Weekend, na.rm = TRUE) + mean(Fee.Weekday, na.rm = TRUE) / 2)

golfdf |> filter(Tee.Time.Reservations == 'Accepted') |> 
  mutate(Annual_rounds_estimate = ifelse(is.na(Annual_rounds_estimate), mean(Annual_rounds_estimate, na.rm = TRUE), Annual_rounds_estimate)) |>
  summarise(avg_9_rnds = mean(Annual_rounds_estimate),
            avg_fee = mean(Fee.Weekend, na.rm = TRUE) + mean(Fee.Weekday, na.rm = TRUE) / 2)

golfdf |> filter(Tee.Time.Reservations != 'Accepted') |> 
  mutate(Annual_rounds_estimate = ifelse(is.na(Annual_rounds_estimate), mean(Annual_rounds_estimate, na.rm = TRUE), Annual_rounds_estimate)) |>
  summarise(avg_9_rnds = mean(Annual_rounds_estimate),
            avg_fee = mean(Fee.Weekend, na.rm = TRUE) + mean(Fee.Weekday, na.rm = TRUE) / 2)

# Private course, 9 holes, average 19,271 rounds a year                
# Private course, 18 holes, average 27,655 rounds a year
# Private course, more than 18 holes, average 60,166 rounds a year
# Public course, 9 holes, average 24,776 rounds a year
# Public course, 18 holes, average 39,278 rounds a year
# Public course, more than 18 holes, 64,404 rounds a year
# Average rounds at courses with open or reciprocal guest policies 39,280 with average fees of 52.05
# Average rounds at courses with closed guest policies 38,167 with average fees of 78.43
# Average rounds at courses that reserve Tee Times 42,124 with average fees of 61.03
# Average rounds at courses that dont reserve Tee Times 25,908 average fees of 38.55


''' Assuming that 2/3 of rounds are played on weekdays and 1/3 of rounds are played on weekends we can determine a general 
    income range for the different categories of courses. The formula is ...
   ((rounds per year * (1/3)) * Weekend  rate) + ((rounds per year * (2/3)) * weekday rate
'''
public_9_income <- ((19271 * .33) * 18.08) + ((19271 * .66)* 15.67)  # $314,283
public_18_income <- ((39278 * .33) * 32.40) + ((39278 * .66)* 27.65) # $1,136.744
public_27_income <- ((64404 * .33) * 36.24) + ((64404 * .66)* 31.48) # $2,108,329
private_9_income <- ((19271 * .33) * 20.79) + ((19271 * .66)* 18.08) # $362,169
private_18_income <- ((27655 * .33) * 55.79) + ((27655 * .66)* 50.56)# $1,431,984
private_27_income <- ((60166 * .33) * 64.40) + ((60166 * .66)* 59.40)# $3,637,395


''' Ideas for sites
 Idea Number 1: Find an area for a private 18+ hole golf course 
     - What is the average population of counties that have these courses          245,176
     - How many other courses are available in counties that have these courses    11 coursers on average
     - What is the ratio of courses to people in these counties                    1 course for every 20,750 people on average
     
 Idea number 2: Find a county that has grown and has not had a proprtionate amount of courses put in yet. 
    
'''

# Idea 1
Private_27_courses <- course_population |> filter(County %in% (private_courses$County[private_courses$Holes > 18]))
Private_27_courses$n <- as.numeric(Private_27_courses$n)
county_df <- as.numeric((county_df$n))

Private_27_courses |> filter(n > 0) |> summarise(mean(POPESTIMATE))
Private_27_courses |> filter(n > 0) |> summarise(mean(n))
Private_27_courses |> filter(n > 0) |> summarise(mean(POPESTIMATE / n))

Idea_1_recomendations <- course_population |> filter(POPESTIMATE > 175000, n < 5, pop_growth_rate > 0)

#Idea 2 
 course_population$ratio <- course_population |> summarise(ratio = n / POPESTIMATE)
 
 mod <- lm(n~ POPESTIMATE, data = course_population)
 predictions <- predict(mod, newdata = course_population)
 course_population$predicted_n <- predictions
 
 ggplot(course_population, mapping = aes(x = n, y = POPESTIMATE)) + 
       geom_point() + 
       geom_smooth(method = 'lm') +
       labs(Title = 'County population vs Number of Courses', 
            x = 'Number of courses', 
            y = 'County Population')
# We are looking for counties above the line, meaning counties with a higher population but lower than expected number of golf courses       

course_population$course_diff <- course_population |> summarise(course_diff = predicted_n - n)       
Idea_2_recomendation <- course_population |>
                        filter(pop_growth_rate > 0, n >= 1, course_diff > 0, POPESTIMATE > 14000) |>
                        select(State, CTYNAME, POPESTIMATE, n, pop_growth_rate, predicted_n, course_diff) |>
                        arrange(desc(course_diff))
                        
# Webb county TX, Jefferson Parish LA, Tuscaloosa Count AL. Have disproportionate Course to population ratio and may
# be candidates for a Private 18+ hole course

Webb_county <- golfdf |> filter(State == 'Texas', County == 'Webb')
Jefferson_parish <- golfdf |> filter(Name == 'Chateau Golf & Country Club')
Jefferson_parish
Tuscaloosa_county <- golfdf |> filter(State == 'Maasachustes', County == 'Tuscaloosa')
Tuscaloosa_county
Spotsylvania_county <- golfdf |> filter(State == 'Virginia', County == 'Spotsylvania')
Spotsylvania_county
