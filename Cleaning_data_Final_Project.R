
setwd("~/Desktop/Tidy_Data_project")
monster_com_job_sample <- read.csv('monster_com-job_sample.csv', header = TRUE)
#view(monster_com_job_sample)

# All: remove (country, country_code, has_expired, job_board, page_url, uniq_id). Since all of the jobs are in United States of America with the same country code. They are also all published in "jobs.monster.com" and haven't expired.
# All: for date_added: the blank observations will be changed to unknown and the filled ones will be changed to have the same format, "mm/dd/yyyy"
# Myamin: Unite "organization" and "sector" in one columns. Since the sector was sometimes in the organization column and other times in the sector column. Then clean the united column. 
# Wed: clean job_title by removing any describtion or extra words. change the "salary" column to be three columns "lower_limit", "upper_limit", "per duration".
# Ibtisam: for job_type: we will seperate the column into two. The first one "job_period" will have three categories: (full time, part time, per diem). The other column"job_type" will have (employee, temporary, intern, seasonal)
# Alomayri: 5- change the location to be "city, state zip_code", if the zip_code wasn't available, then "city, state". 
#--------------------------------------------------------------------------------------------------------------

# install and run libraries
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages('maps')
#install.packages("viridis")
#install.packages("ggplot2") 
#install.packages("plotly") 

library(maps)
library(viridis)
library(plotly)
library(tidyverse)
library(lubridate)
#install.packages("tm")           
library(tm)
library(dbplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(RColorBrewer)

# 1- Removing undesired columns


monster2 <- monster_com_job_sample %>% select(-country, -country_code, -has_expired, -job_board, -page_url, -uniq_id)
colnames(monster2)


#--------------------------------------------------------------------------------------------------------------

# 2- adjusting dates 

date <- monster2 %>% select (date_added) %>% filter(!grepl("^\\s*$", date_added))

monster2$date_added[7993] <- "5/9/2016" 

#strftime(as.Date(inp, format="%d/%m/%Y"), format="%d/%m/%Y")


 

    for (x in 1:nrow(monster2)) 
    {  if (grepl("^\\s*$", monster2$date_added [x])) {
    monster2$date_added[x] = "Unknown"}
    }
  

#view(monster2)

#--------------------------------------------------------------------------------------------------------------
# Import dataset

#view(monster_com_job_sample)


#Here we got rid of the undesired columns that either have the same information in the entire column or repetitive to other columns

# 3- In this part of the code, I wanted to unite "organization" and "sector" in one column. Since the sector was sometimes in the organization column and other times in the sector column. 
# Then clean the data in the united column.

# Uniting the columns into one column

monster2 <- monster2 %>% unite( sector, organization, sep = " ")

#view(monster2)

# Looking at the sector column, it has a lot of white space cells that aren't Null. Almost no two rows in the same format. Sometimes it has address; others it has job type such as full time. 
# It also has a lot of rows with merged words.


# In order to know what are the available sectors in the data:
# First, I put some sectors randomly, and set a condition to show the cells that don't have these words on them
# This step was done gradually, every time I'll add a couple and view the potential_underired_data_in_sector, to see what other sectors are available in the data
# The list decreased gradually and shrink to include undesired data only


potential_undesired_data_in_sector <- monster2 %>% select (sector) %>% filter(!grepl("Management|Aerospace|bruker|Metals|Minerals|Sports|Shiloh|Music|Administration|Marketing|Advertising|Media|Support|Specialist|Communications|Developing|Media|Sales|sales|Accounting|utilities|Utilities|Research|Education|Academic|Computer|software|web|software|IT|I T|Network|Support|Specialist|Technical|Application|Information|Art|art|actor|Architect|Office|office|residential|construction|Constructions|Construction|Travel|Traveling|Telecommunication|Professional|Admissions|Technology|Teaching|Tutoring|Engineering|Engineer|engineer|Health|health|Medical|medical|Doctor|doctor|Psychology|Nursing|Social|Sociology|Service|service|Customer Service|technologyTourism|Insurance|Security|Government|Banking|bancking|Mfg|Electronics|Employment|Employee|Agency|Agencies|agencies|Retail|Printing|banking|non-profit|Nonprofit|nonprofit|Organization|Industry|Industries|Materials|Eport|Import|Hotels|Chemicals|law|Law|Food|Customes|Industrial|Supply|Chain|Agricultural|Agriculture|Customer|Manufacturing|Entertainment|Theaters|Sporrts|Clothing|Textiles", sector))
view (potential_undesired_data_in_sector)

# Looking at the undesired data, it can be seen that there are cells that has address sometimes city, STATE zipcode, City, zip code.
# There wasn't a specific format for the address
# Hence, I filtered the column to show data that has "," to get rid of all cities, states, and zip codes In the next step. 

potential_undesired_data_in_sector2<- monster2 %>% select (sector) %>% filter(grepl(",", sector))
view (potential_undesired_data_in_sector2)

# Here I removed all the address related data in sector

monster2 <- monster2 %>%
  mutate(sector = str_remove(sector, ".*AL|.*AK|.*AZ|.*AR|.*CT|.*DE|.*HI|.*ID|.*IN|.*IA|.*KS|.*KY|.*LA|.*ME|.*MD|.*MA|.*MI|.*NE|.*NC|.*NV|.*NH|.*NY|.*NM|.*ND|.*PA|.*RI|.*SC|.*SD|.*TN|.*UT|.*VA|.*WV|.*WI|.*WY|.*TX|.*GA|.*FL|.*CA|.*IL|.*PA|.*MN|.*OH|.*MA|.*DE|.*WA|.*NE|.*MI|.*CO|.*NJ|.*WI|.*OR|.*DC| *OK|Sacramento,|.*Sunnyvale,|.*Folsom,|.*Roseville,.*|Carlsbad,.*|.*Lake Aluma,.*|.*Fremont,.*|.*Oklahoma City.*|.*Milpitas,.*|.*_.*|[0-9]+"))

# Then removed words that aren't sectors and were in sector column based on the results of potential_undesired_data_in_sector

monster2 <- monster2 %>%
  mutate(sector = str_remove(sector, "Experienced|.*Other|.*Otherother|.*OtherOther|Experienced level|(Non-Manager)|career level|career|All|all|Entery|Office|General/Others:|High level|.*Level|.*required|¬†‚Ä¢ ¬† |/|student|Full Time Employee|/ "))

# Since some words were merged into each others, I seperated them based on the condition: if there is a capital letter, put space before it. 
monster2 <- monster2 %>%
  mutate(sector = gsub("(?!^)(?=[[:upper:]])", " ", sector, perl=T))

# Putting space based on capital letters casused some extra spacing before the words that already had a space before
# I removed the extra spaces here

library(stringr)
monster2 <- monster2 %>%
  mutate(sector = str_replace(gsub("\\s+", " ", str_trim(sector)), "B", "b"))


# Since all the cells now are arranged correctly with a format that is somewhat similar, all the cells that didn't have a specific sector was assigned to "Not Classified"
# I used for loop for that

for (x in 1:nrow(monster2)) 
{  if (!grepl(" ", monster2$sector [x])) {
  monster2$sector[x] = "Not Classified"}}

# In order to make sure on the work that i have done, I created a dataframe that has all the classified sectors

classified_sectors <- monster2 %>% select (sector) %>% filter(!grepl("Not Classified", sector))
view(classified_sectors)

# dataframe for Not Classified

Not_classified_sectors <- monster2 %>% select (sector) %>% filter(grepl("Not Classified", sector))
view(Not_classified_sectors)


# sum of the rows in the two dataframes is 22000, which means all rows in sector column in monster data have value on them. 
sum(nrow(Not_classified_sectors)+nrow(classified_sectors))

view(monster2)

Management <- sum(str_count(monster2$sector, "Management|Executive|Direction|Supervisoion|Administration"))
Health <- sum(str_count(monster2$sector,"Health|health|Medical|medical|Doctor|doctor|Psychology|Nurse|Nursing|nursing|Paramedic|Physician|Social Worker|social worker"))
Military <- sum(str_count(monster2$sector, "Military|military|Surveillance"))
Travel_Tourism<- sum(str_count(monster2$sector,"Travel|Tourism|Transportation|travel|tourism|transportation")) 
Industry <- sum(str_count(monster2$sector,"Industry|Industrial|Make|make|Making|Automotive|automotive"))
Technology <- sum(str_count(monster2$sector,"Technology|technology|biotechnology|Developing|Software|web|software|Web|Hardware|IT|I T"))
Food <- sum(str_count(monster2$sector,"Food|food|Beverage|beverage|Restaurant|Restaurants"))

plot_sector <- c("Management","Health","Military","Travel_Tourism","Industry","Technology","Food")
sectors_count <- c(Management,Health,Military,Travel_Tourism,Industry,Technology,Food)

# Create a matrix for the Plot_sectors
Plot_sectors <- matrix( nrow = length(sectors_count), ncol = 2) 
Plot_sectors[,1] <- plot_sector
Plot_sectors[,2] <- sectors_count
# Building a data frame for the Plot_sectors
Plot_sectors <- as.data.frame(Plot_sectors) 
co_headings <- c('Sector','Sector_Count') # To add headings for the data frame
names(Plot_sectors) <- co_headings
view(Plot_sectors)


ggplot(Plot_sectors, aes(x="", y=sectors_count, fill=plot_sector)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_brewer(palette="Set3")




#___________________________________________________________________________


# 4 Clean job_title by removing any description or extra words. 

# string_to_remove is a vector that includes unwanted strings 
string_to_remove <- c("Job.*|Job in.*|Full.*|Entry Level|[**$].*|[(-)].*|[*]| - .*| – .*|/ .*|[()].*")

# Iterate through the job_title column and remove the strings that are in string_to_remove vector.
for (x in 1:nrow(monster2)){
  monster2$job_title[x] = str_remove(monster2$job_title[x],string_to_remove)
  if (grepl("^\\s*$|Please apply only if you are qualified|Up to $", monster2$job_title[x])){ 
    monster2$job_title[x] = "Not specified yet"
  } 
}

#view(monster2)


# Create job categories and Sum the count of the keywords that represent each category in the job_title column 

management <- sum(str_count(monster2$job_title, "Management|Manager|manager|Executive|Director|Supervisor|Administration|Administrator|Admin"))
marketing <- sum(str_count(monster2$job_title, "Marketing|Advertising Assistant|Advertising Coordinator|Advertising Specialist|Assistant Media Planner|Client Support Specialist|Communications Coordinator|Developer|Media"))
sales <- sum(str_count(monster2$job_title, "Sales|sales|Account Coordinator|Account Representative|Account Specialist|Researcher"))
education <- sum(str_count(monster2$job_title, "Education|Academic|Professor|Admissions Representative|Teach|Adviser|Instructor|Teacher|Coach|school|Preschool|Tutor"))
engineering <- sum(str_count(monster2$job_title,"Engineering|Engineer|engineer")) 
health <- sum(str_count(monster2$job_title,"Health|health|Medical|medical|Doctor|doctor|Psychologist|Nurse|Paramedic|Physician|Social Worker|social worker"))
service_industry <- sum(str_count(monster2$job_title,"Service|service|Call Center|Bank Teller"))
technology <- sum(str_count(monster2$job_title,"Technology|technology|Developer|developer|programmer|Programmer|Back End|Front End|Computer|software|web|software|Web|Hardware|IT|Cloud|Network|Help Desk|Support Specialist|Technical|Database|Data|Application|Information|Security Specialist|Java|.NET|Analyst|System Architect|.net|Python"))
art <- sum(str_count(monster2$job_title,"Art|art|actor|Architect"))

# Create vectors for the job categories and the jobs count  
job_category<- c("Management","Marketing","Sales","Education","Engineering","Health","Service Industry","Technology","Art")
jobs_count <- c(management,marketing,sales,education,engineering,health,service_industry,technology,art)

# Create a matrix for the job_categories
job_categories <- matrix( nrow = length(jobs_count), ncol = 2) 
job_categories[,1] <- job_category
job_categories[,2] <- jobs_count  

# Building a data frame for the job_categories
job_categories <- as.data.frame(job_categories) 
co_headings <- c('Job Categories','Jobs count') # To add headings for the data frame
names(job_categories) <- co_headings
view(job_categories)

#Bar Chart showing comparison of job categories advertised in monster.com (in USA)
ggplot(job_categories, aes(job_category, jobs_count, fill = job_category)) +
  geom_bar(stat = 'identity', orientation = "x")+
  labs(title = "Comparison of Job Categories advertised in monster.com (in USA)", x = "Job Categories", y = "Jobs count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#--------------------------------------------------------------------------------------------------------------

# 5- for job_type: we will separate the column into two. The first one "job_period" will have three categories: (full time, part time, per diem). The other column"job_type" will have (employee, temporary, intern, seasonal)
# The first one "job_period" will have three categories: (full time, part time, per diem).
# The other column"job_type" will have (employee, temporary, intern, seasonal)



# Creat a new variable "job_period"

stopwords1 <- c("Employee","Temporary","Intern","Seasonal","Project","Contract","Job", "Type", "Exempt")  # List unwanted words
job_period  <- monster2$job_type        
job_period  <-  removeWords(job_period,stopwords1)     # Remove stopwords from job_period


monster2 <- monster2 %>% mutate(job_period)            # Insert "job_period" into "monster2" data frame

monster2$job_period <- (gsub(",","",monster2$job_period))            # To remove commas
monster2$job_period <- (gsub(">","",monster2$job_period))            # To remove  >
monster2$job_period <- (gsub("/","",monster2$job_period))            # To remove slashes

for (x in 1:nrow(monster2))                                          # State "Unknown" for empty observations
{  if (grepl("^\\s*$", monster2$job_period [x])) {
  monster2$job_period[x] = "Unknown"}
}

#_________

#Add a new variable "contract_type"
stopwords2 <- c("Per","Diem","Full","Part","Time","Project","Contract","Job", "Type","Exempt")   # List unwanted words
contract_type  <- monster2$job_type        
contract_type  <-  removeWords(contract_type,stopwords2)     # Remove stopwords from contract_type


monster2 <- monster2 %>%  mutate(contract_type)       # Insert "job_period" into "monster2" data frame

monster2$contract_type <- (gsub(",","",monster2$contract_type))        # To remove commas
monster2$contract_type <- (gsub(">","",monster2$contract_type))        # To remove  >
monster2$contract_type <- (gsub("/","",monster2$contract_type))        # To remove slashes

for (x in 1:nrow(monster2))                                            # State "Unknown" for empty observations
{  if (grepl("^\\s*$", monster2$contract_type [x])) {
  monster2$contract_type[x] = "Unknown"}
}

monster2 <- monster2 %>% select(-job_type)                             # Drop "job_type" variable
col_order <- c("date_added", "job_description", "job_title","job_period","contract_type",
               "location", "sector","salary")                          # Reorder the variables
monster2 <- monster2[ ,col_order]

View(monster_com_job_sample[,8])  # Before cleaning
View(monster2[,4:5])              # After cleaning


#Plot

par(mfrow=c(2,1))

# job_period: (full time, part time, per diem)
Full_Time<-  sum(str_count(monster2$job_period,"Full Time"))
Part_Time<-  sum(str_count(monster2$job_period,"Part Time"))
Per_Diem<-  sum(str_count(monster2$job_period,"Per Diem"))

# Contract_type: (employee, temporary, intern, seasonal)
employee<-  sum(str_count(monster2$contract_type,"Employee"))
temporary<-  sum(str_count(monster2$contract_type,"Temporary"))
intern<-  sum(str_count(monster2$contract_type,"Intern"))
seasonal<-  sum(str_count(monster2$contract_type,"Seasonal"))

par(mfrow=c(2,1))

#Pie Chart demonstrates the comparsion of job period categories
slices<- c(Full_Time,Part_Time,Per_Diem)  
pct <- ceiling(slices/sum(slices)*100)       # To find percentages
lbls= c("Full Time", "Part Time", "Per Diem")              # To add labels
lbls <- paste(lbls, "(",pct)               # To add percentages to labels
lbls <- paste(lbls,"% )",sep="")           # To add % sign to labels
pie(slices,labels = lbls ,                 # Pie cart for average Adults Vs. Children Visitors for Theater 1
    main="Comparsion of Job Period Categories " ,
    col=c("dark blue","light blue"))    

#Pie Chart demonstrates the comparison contract type categories
slices<- c(employee, temporary, seasonal)  
pct <- ceiling(slices/sum(slices)*100)       # To find percentages
lbls= c("Employee", "Temporary", "Seasonal")              # To add labels
lbls <- paste(lbls, "(",pct)               # To add percentages to labels
lbls <- paste(lbls,"% )",sep="")           # To add % sign to labels
pie(slices,labels = lbls ,                 # Pie cart for average Adults Vs. Children Visitors for Theater 1
    main="Comparsion of Contract Type Categories" ,
    col=c("dark blue","light blue"))    


#--------------------------------------------------------------------------------------------------------------

# 6- Change the "salary" column to be three columns "lower_limit_salary", "upper_limit_salary", "salary_paid_per".

limit  <- monster2$salary  
limit2<- gsub("[^[:digit:].-]", "",  limit)               # Only keep  (.)  (-) (digit)

lower_limit_salary<- sub('\\..*', '', limit2)             # Remove every thing after (.)
upper_limit_salary<- sub('.*\\-', '', limit2)             # Remove every thing before (-)
upper_limit_salary<- sub('\\..*', '',upper_limit_salary)  # Remove every thing after (.)

salary_paid_per<- gsub("[^[yearhour]", "",  monster2$salary) # # Remove every thing except year & hour


monster2 <- monster2 %>%  mutate(lower_limit_salary, upper_limit_salary, salary_paid_per)  # Insert the new variables into "monster2"

for (x in 1:nrow(monster2))         # State "Not specified" for empty observations
{  if (grepl("^\\s*$", monster2$lower_limit_salary [x])) {
  monster2$lower_limit_salary[x] = "Not specified"}
}
for (x in 1:nrow(monster2))         # State "Not specified" for empty observations and $
{  if (grepl("^\\s*$", monster2$upper_limit_salary [x])) {
  monster2$upper_limit_salary[x] = "Not specified"}
}


for (x in 1:nrow(monster2))         # State "Not specified" for empty typos
{  if ( nchar(monster2$salary_paid_per [x]) > 4) {
  monster2$salary_paid_per[x] = "Not specified"}
}
for (x in 1:nrow(monster2))         # State "Not specified" for empty typos
{  if ( nchar(monster2$salary_paid_per [x]) < 4) {
  monster2$salary_paid_per[x] = "Not specified"}
}
for (x in 1:nrow(monster2))         # State "Not specified" for empty typos and empty observations
{  if (!grepl("[year]|[hour]", monster2$salary_paid_per [x])) {
  monster2$salary_paid_per[x] = "Not specified"}
}


# State "Not specified" for empty typos
monster2$salary_paid_per[c(5130,71,1065,17734,17895,18250,21772,2087,3255,5101,13696,17589,21944,20032,9843,17785,
                           9191,11833,9382,14091,5729,8094,9541,12965,14161,5702,5890,18967,9734,17594,21168,13737,7941)]<- "Not specified"

monster2 <- monster2 %>% select(-salary)    # Drop "salary from "monster2"

View(monster_com_job_sample[,12])  # Before cleaning
View(monster2[,8:10])              # After cleaning


##########################



#_____________________________________________________________________-

# 7- location

#Cities_codes_dataframe <- read.csv('USA_Cities.csv', header = TRUE) #read csv file that contain all famous cities names of USA, it will take long time to run
Cities_codes_dataframe <- read.csv('top_300_cities.csv', header = TRUE) #read csv file that contain top famous cities names of USA
#Cities_codes_dataframe$?..City<-
  
 # Cities_codes_dataframe %>% 
 # rename(Cities_codes_dataframe$?..City=City)

view(Cities_codes_dataframe)

states<-paste(state.abb,collapse="|")   # all state names to use them in regular expression

#get all cities names to use them in regular expression
cities_names<-str_remove_all(Cities_codes_dataframe$City,"[ ](?=[ ])|[^-_,A-Za-z0-9 ]+")

cities_names<-paste( cities_names,collapse="|")



# this will add three column state_name, city_zip and city_name and will extract data from location column
monster2<- monster2 %>%
  mutate(
    
    
    state_name=ifelse(str_detect(location,states),str_extract(location,states),""),
    city_zip=ifelse(str_detect(location,"\\d{5}"),str_extract(location,"\\d{5}"),""),
    city_name=ifelse(str_detect(location,regex(cities_names, ignore_case = TRUE)),str_extract(location,regex(cities_names, ignore_case = TRUE)),"")
    
  ) %>%
  
  
  select(-location,state_name,city_name,city_zip)



#view(location_column)
view(monster2)
# state_list is dataframe with five columns State_name, state_code, state_lat, state_lon, Job_Count
state_list<-data.frame(State_name=state.name,state_code=state.abb,state_lat=state.center$x,state_lon=state.center$y,Job_Count=0) 


#fill job_count with number of jobs for every state
for(x in 1:nrow(state_list)){
  state_list[x,5]=sum(monster2[,10]==as.character(state_list[x,2]))
  
  
  
}


# Map Plot for jobs ads

ggplot(data=state_list, aes(x=state_lat,y= state_lon, size =Job_Count,color=Job_Count)) +
  labs(title = "Jobs",x="latitude",y="Longitude",colours="size")+
  
  
  borders("state",fill = "light blue") +
  geom_point() +
  coord_quickmap() +
  scale_colour_viridis()






# Interactive Map Plot for jobs ads

state_list$hover <- with(state_list, paste(
  "State Name :",state_list$State_name, '<br>',
  "State Code:",state_list$state_code, '<br>',
  "Number of Jobs Ads:",state_list$Job_Count, '<br>'
))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
#install.packages("tidyverse")
#install.packages("lubridate")
install.packages('maps')
install.packages("viridis")
#install.packages("ggplot2") 
install.packages("plotly") 

library(maps)
library(viridis)
library(plotly)
library(tidyverse)
library(lubridate)
library(dbplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library("RColorBrewer")

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig <- plot_geo(state_list, locationmode = 'USA-states')
fig <- fig %>% add_trace(
  z = state_list$Job_Count, text =state_list$hover , locations = state_list$state_code,
  color = state_list$Job_Count, colors = 'Purples'
)
fig <- fig %>% colorbar(title = "Jobs Ads in USA")
fig <- fig %>% layout(
  title = 'Number of Jobs Advertisment in USA',
  geo = g
)

fig

#--------------------------------------------------------------------------------------------------------------



