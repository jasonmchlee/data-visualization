###############################################################
# INSTALL AND LOAD PACKAGES
###############################################################

install.packages("readxl")
library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2)
library(plotly)
library(forcats)


###############################################################
# WE CLEANED THE EXCEL SHEETS BY REMOVING ROWS AND META DATA
###############################################################

###############################################################
# READ IN INDIVIDUAL EXCEL SHEETS
###############################################################

# read in specific sheets in the excel file
weekly_visits <- read_excel("Alloy Dataset.xls", sheet = 1)
financials <- read_excel("Alloy Dataset.xls", sheet = 2)
lbs_sold <- read_excel("Alloy Dataset.xls", sheet = 3)
daily_visits <- read_excel("Alloy Dataset.xls", sheet = 4)
traffic_sources <- read_excel("Alloy Dataset.xls", sheet = 5)
referring_sites <- read_excel("Alloy Dataset.xls", sheet = 6)
search_engine <- read_excel("Alloy Dataset.xls", sheet = 7)
geographic_sources <- read_excel("Alloy Dataset.xls", sheet = 8)
browsers_used <- read_excel("Alloy Dataset.xls", sheet = 9)
operating_system <- read_excel("Alloy Dataset.xls", sheet = 10)


###############################################################################
# WE DID EXTRA EXTERNAL RESEARCH TO ANALYZE GOOGLE TRENDS FOR THE TERM "ALLOY"
###############################################################################

# GOOGLE TRENDS - Read in excel file
search_trends <- read_excel("Google Trends.xlsx",sheet = 1)
region_trends <- read_excel("Google Trends.xlsx",sheet = 2)



#########################################################
# CREATE A COLUMN DEFINING A STAGE FOR EACH SHEET
#########################################################


# FINANCIALS SHEET
financials$stage <- c()

for (x in 1:nrow(financials)) {
  financials$stage[x] <- "Initial"} 

for (x in 15:nrow(financials)) {
  financials$stage[x] <- "Pre Promotion"} 

for (x in 36:nrow(financials)) {
  financials$stage[x] <- "Promotion"} 

for (x in 53:nrow(financials)) {
  financials$stage[x] <- "Post Promotion"} 
  

# WEEKLY VISITS SHEET
weekly_visits$stage <- c()

for (x in 1:nrow(weekly_visits)) {
  weekly_visits$stage[x] <- "Initial"} 

for (x in 15:nrow(weekly_visits)) {
  weekly_visits$stage[x] <- "Pre Promotion"} 

for (x in 36:nrow(weekly_visits)) {
  weekly_visits$stage[x] <- "Promotion"} 

for (x in 53:nrow(weekly_visits)) {
  weekly_visits$stage[x] <- "Post Promotion"} 


#############################################################
# CREATE A NEW COLUMN FOR WHICH WEEK  NUMBER IT IS
#############################################################

# Financial dataframe
financials$week_number <-c()

for(x in 1:nrow(financials)){
  financials$week_number[x] <- x
}  

# Weekly visit dataframe
weekly_visits$week_number <-c()

for(x in 1:nrow(weekly_visits)){
  weekly_visits$week_number[x] <- x
}  

##########################################################
# SCATTER PLOTS
##########################################################

#scatter plot - Unique Visits
ggplot(weekly_visits) +
  aes(x = avg_time_on_site, y = unique_visits, color = "red")+
  geom_point()+
  labs(title = "Weekly Unique Visits on the Site", x = "Average Time on Site", y = "Number of Unique Visit")+
  theme(panel.background = element_rect(fill="white"))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))


#scatter plot - Bounce Rate
ggplot(weekly_visits) +
  aes(x = bounce_rate, y = percent_new_visit, color = "red")+
  geom_point()+
  labs(title = "Bounce Rate for New Visits", x = "Bounce Rate", y = "New Visitors")+
  theme(panel.background = element_rect(fill="white"))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))


#scatter plot - REVENUE VS VISITS - NO RELATIONSHIP
ggplot(weekly_visits) +
  aes(x = visits, y = financials$revenue, color = "red")+
  geom_point()+
  labs(title = "Relationship between Visits and Revenue", x = "Visits", y = "Revenue")+
  theme(panel.background = element_rect(fill="white"))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))


# Facet wrap graph - each stage shows the weekly visit

financials$stage <- factor(financials$stage, 
                           levels = c("Initial", "Pre Promotion", "Promotion", "Post Promotion"))
ggplot(financials)+
  aes(x=inquiries, y= revenue, size = weekly_visits$visits)+
  geom_point(color = "red")+
  facet_wrap(~stage)+
  labs(title = "Traffic during each stage for revenue and inquries", x = "Inquiries", y = "Revenue", size = "Weekly Visits")+
  theme(panel.background = element_rect(fill="white"))+
  theme(plot.title = element_text(hjust = 0.5))


#####################################################################
# HISTORGRAM
#####################################################################

# histogram of lbs sold - normal distribution
ggplot(financials)+
  aes(x=lbs_sold, fill="blue")+
  geom_histogram()+
  labs(title = "Distribution of Lbs Sold", x = "Lbs Sold", y = "Frequency")+
  theme(panel.background = element_rect(fill="white"))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))

# Facet wrap histograms graph - each stage shows lbs sold

financials$stage <- factor(financials$stage, 
                           levels = c("Initial", "Pre Promotion", "Promotion", "Post Promotion"))
ggplot(financials)+
  aes(x=lbs_sold)+
  geom_histogram(fill = "blue", bins = 6)+
  facet_wrap(~stage)+
  labs(title = "Lbs Sold during each stage", x = "Stage", y = "Count")+
  theme(panel.background = element_rect(fill="white"))+
  theme(plot.title = element_text(hjust = 0.5))


#####################################################################
# PIE CHART
#####################################################################

# traffic source - HIGH TRAFFIC FROM REFFERING SITES ARE PRODUCING POOR RESULTS SINCE SALES HAVE NOT INCREASED, RESULT IS EXTRA COST
# Barplot
bp<- ggplot(traffic_sources, aes(x="", y=visits, fill=traffic_source))+
  geom_bar(width = 1, stat = "identity")
pie <- bp +coord_polar("y", start = 0)+
  labs(title = "Traffic Sources", x = "", y = "Total Visits", fill = "Channels")+
  theme(panel.background = element_rect(fill="white"))+
  theme(plot.title = element_text(hjust = 0.5))
pie


##########################################################
# CALCULATE PERCENTAGE CHANGE BETWEEN WEEKS
##########################################################

# NEW COLUMN REVENUE CHANGE
financials$revenue_change <-c()

for(x in 1:nrow(financials)){
  financials$revenue_change[x] <- ((financials$revenue[x+1] / financials$revenue[x]) - 1) *100
}  

# PROFIT CHANGE IN PERCENTAGE
financials$profit_change <-c()

for(x in 1:nrow(financials)){
  financials$profit_change[x] <- ((financials$profit[x+1] / financials$profit[x]) - 1) *100
}  

# LBS CHANGE IN PERCENTAGE
financials$lbs_change <-c()

for(x in 1:nrow(financials)){
  financials$lbs_change[x] <- ((financials$lbs_sold[x+1] / financials$lbs_sold[x]) - 1) *100
}  


###########################################################
# LINE GRAPHS FOR PERCENT CHANGE
##########################################################

#filter for only promotion stage
promotion_stage <- financials %>%
  filter(stage == "Promotion") %>%
  select(week_number, revenue_change, profit_change)


######## REVENUE VS PROFIT
ggplot(promotion_stage)+
  geom_line(aes(x = week_number, y = revenue_change), color = 'blue', size = 1)+
  geom_line(aes(x = week_number, y = profit_change), color = 'red', size = 1)+
  scale_x_discrete(labels=c(""))+
  labs(title = "Percent Change (Promotion)", x = "Week Duration", y = "Percent Change")+
  theme(panel.background = element_rect(fill="white"))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = 35)


#filter for only PRE stage
pre_promotion_stage <- financials %>%
  filter(stage == "Pre Promotion") %>%
  select(week_number, revenue_change, profit_change)


# REVENUE VS PROFIT
ggplot(pre_promotion_stage)+
  geom_line(aes(x = week_number, y = revenue_change), color = 'blue', size = 1)+
  geom_line(aes(x = week_number, y = profit_change), color = 'red', size = 1)+
  scale_x_discrete(labels=c(""))+
  labs(title = "Percent Change (Pre Promotion)", x = "Week Duration", y = "Percent Change")+
  theme(panel.background = element_rect(fill="white"))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = 35)


# Plot revenue change over time
ggplot(financials)+
  geom_line(aes(x = week_number, y = revenue_change), color = 'blue', size = 1)+
  scale_x_discrete(labels=c(""))+
  labs(title = "Percent Change in Revenue", x = "Week Duration", y = "Percent Change")+
  theme(panel.background = element_rect(fill="white"))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = 14)+
  geom_vline(xintercept =35)+
  geom_vline(xintercept = 52)


############################################################
# GOOGLE SEARCH TRENDS
############################################################

# search trends
ggplot(search_trends)+
  geom_line(aes(x = week_number, y = alloys_interest), color = 'red', size = 1)+
  scale_x_discrete(labels=c(""))+
  labs(title = 'Google Search for "Alloys"', x = "Week Duration", y = "Search Trending")+
  theme(panel.background = element_rect(fill="white"))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = 14)+
  geom_vline(xintercept =35)+
  geom_vline(xintercept = 52)

# total visits
ggplot(weekly_visits)+
  geom_line(aes(x = week_number, y = visits), color = 'blue', size = 1)+
  scale_x_discrete(labels=c(""))+
  labs(title = 'Weekly Visits"', x = "Week Duration", y = "Visitor Count")+
  theme(panel.background = element_rect(fill="white"))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = 14)+
  geom_vline(xintercept =35)+
  geom_vline(xintercept = 52)

# Create a choropleth map of the change in voter turnout from 2014 to 2018
region_trends %>%
  plot_geo(locationmode = 'USA-states') %>%
  add_trace(z = ~region_interest, locations = ~state) %>%
  layout(geo = list(scope="usa"))+
  labs(title = "Interest in Alloy by State", fill = "Interest")



###############################################################
# CORRELATION MATRIX
###############################################################

#coorelaton - a lot of page views are new visitors but they do not convert into inquries or orders
data <- data.frame(financials$revenue, financials$inquiries, weekly_visits$percent_new_visit, weekly_visits$visits)
correlated <- round(cor(data),2)
melted_correlated <- melt(correlated)
melted_correlated
ggplot(data = melted_correlated, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color='white', size=2)+
  labs(x='',y='')+
  scale_x_discrete(labels=c("financials.revenue" = "Revenue", "financials.inquiries" = "Inquries",
                                "weekly_visits.percent_new_visit" = "New Visitors", "weekly_visits.visits" ="Total Visits"))+
  scale_y_discrete(labels=c("financials.revenue" = "Revenue", "financials.inquiries" = "Inquries",
                            "weekly_visits.percent_new_visit" = "New Visitors", "weekly_visits.visits" ="Total Visits"))


#coorelaton - a lot of page views are new visitors but they do not convert into inquries or orders
data <- data.frame(search_trends$alloys_interest, weekly_visits$page_views, weekly_visits$percent_new_visit, weekly_visits$visits)
correlated <- round(cor(data),2)
melted_correlated <- melt(correlated)
melted_correlated
ggplot(data = melted_correlated, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color='white', size=2)+
  labs(x='',y='')+
  scale_x_discrete(labels=c("search_trends.alloys_interest" = "Google Trends", "weekly_visits.page_views" = "Inquries",
                            "weekly_visits.percent_new_visit" = "New Visitors", "weekly_visits.visits" ="Total Visits"))+
  scale_y_discrete(labels=c("search_trends.alloys_interest" = "Google Trends", "weekly_visits.page_views" = "Inquries",
                            "weekly_visits.percent_new_visit" = "New Visitors", "weekly_visits.visits" ="Total Visits"))


################################################################
#DESCRIPTIVE STATISTICS
################################################################

# Summary on financials
summary(weekly_visits$visits)
summary(weekly_visits$unique_visits)
summary(financials$revenue)
summary(financials$profit)
summary(financials$lbs_sold)
summary(financials$inquiries)

#standard devations
sd(weekly_visits$visits)
sd(weekly_visits$unique_visits)
sd(financials$revenue)
sd(financials$profit)
sd(financials$lbs_sold)
sd(financials$inquiries)


###################################################################
# BOX PLOTS
###################################################################

# Setting factor order for graph
financials$stage <- factor(financials$stage, 
                       levels = c("Initial", "Pre Promotion", "Promotion", "Post Promotion"))


# BOX PLOT OR STAGES IN LB SOLD
ggplot(financials)+
  aes(x=stage, y = lbs_sold)+
  geom_boxplot(color="darkred")+
  labs(title = "Lbs Sold in Each Stage", x = "Stage", y = "Lbs Sold")+
  theme(panel.background = element_rect(fill="white"))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))

#box plot for profits
ggplot(financials)+
  aes(x=stage, y = profit)+
  geom_boxplot(color="blue")+
  labs(title = "Profit in Each Stage", x = "Stage", y = "Profit")+
  theme(panel.background = element_rect(fill="white"))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))



######################################################################
# LOGISTIC REGRESSION MODEL
######################################################################

log_data <- cbind(financials,weekly_visits)
for (i in 1:nrow(log_data)) {
  if (log_data$inquiries[i] >= 6){
    log_data$success_inquiry[i] <- 1
  }
  else if (log_data$inquiries[i] < 6){
    log_data$success_inquiry[i] <- 0
  }
  else {log_data$success_inquiry[i] <- NA}
}

log_data <- log_data[order(log_data$success_inquiry),]
rownames(log_data) <- 1:nrow(log_data)

#######################################################################
# Training and Testing Data
#######################################################################

library(caTools) 
train_index <- sample.split(log_data$inquiries, SplitRatio = 0.6)
train <- log_data[train_index,]
test <- log_data[!train_index,]

# Model: 
###temp --> page_views0.4,page_visit0.08,avg_time_on_site0.22,bounce_rate0.122,percent_new_visit0.3

# Model 1: About 70-80% Predictive Power
my_mod <- glm(success_inquiry ~ page_views+avg_time_on_site+bounce_rate,data = log_data, family = "binomial")
summary(my_mod)
predict(my_mod, test, type="response") 

# Model 2: About 70-80% Predictive Power
my_mod <- glm(success_inquiry ~ page_views+bounce_rate,data = log_data, family = "binomial")
summary(my_mod)
predict(my_mod, test, type="response")






############################################################
# CASE SPECIFIC QUESTIONS
############################################################

#Q1. How many people visit the website? 
  
  ## Average Weekly Visits
w_initial_shakedown <- weekly_visits[1:14,]
w_pre_promotion <- weekly_visits[15:35,]
w_promotion <- weekly_visits[36:52,]
w_post_promotion <- weekly_visits[53:66,]

# Average of Weekly visits during Initial Shakedown Period
w_visits1 <- mean(w_initial_shakedown$unique_visits)
w_visits1

# Average of Weekly visits during Pre-promotion Period
w_visits2 <- mean(w_pre_promotion$unique_visits)
w_visits2

# Average of Weekly visits during promotion Period
w_visits3 <- mean(w_promotion$unique_visits)
w_visits3

# Average of Weekly visits during Post-promotion Period
w_visits4 <- mean(w_post_promotion$unique_visits)
w_visits4


## Average Daily visits 
d_initial_shakedown <- daily_visits[1:98,]
d_pre_promotion <- daily_visits[99:245,]
d_promotion <- daily_visits[246:364,]
d_post_promotion <- daily_visits[365:462,]

# Average of Weekly visits during Initial Shakedown Period
d_visits1 <- mean(d_initial_shakedown$visits)
d_visits1

# Average of Weekly visits during Pre-promotion Period
d_visits2 <- mean(d_pre_promotion$visits)
d_visits2

# Average of Weekly visits during promotion Period
d_visits3 <- mean(d_promotion$visits)
d_visits3

# Average of Weekly visits during Post-promotion Period
d_visits4 <- mean(d_post_promotion$visits)
d_visits4

#######################################################################################

# Q2. Is the website generating interest, and does this interest yield actual sales?

# Financials

f_initial_shakedown <- financials[1:14,]
f_pre_promotion <- financials[15:35,]
f_promotion <- financials[36:52,]
f_post_promotion <- financials[53:66,]

## Total Inquiries and Average of Weekly Inquiries made in each period respectively

# Total inquiries and Average of Weekly inquiry during Initial Shakedown Period
inquiry1 <- c(sum(f_initial_shakedown$inquiries),mean(f_initial_shakedown$inquiries))
inquiry1

# Total inquiries and Average of Weekly inquiry during Pre-promotion Period
inquiry2 <- c(sum(f_pre_promotion$inquiries),mean(f_pre_promotion$inquiries))
inquiry2

# Total inquiries and Average of Weekly inquiry during promotion Period
inquiry3 <- c(sum(f_promotion$inquiries),mean(f_promotion$inquiries))
inquiry3

# Total inquiries and Average of Weekly inquiry during Post-promotion Period
inquiry4 <- c(sum(f_post_promotion$inquiries),mean(f_post_promotion$inquiries))
inquiry4


## Total Revenue and Average of Weekly Revenues made in each period respectively

# Total Revenue and Average of Weekly Revenues during Initial Shakedown Period
revenue1 <- c(sum(f_initial_shakedown$revenue),mean(f_initial_shakedown$revenue))
revenue1

# Total Revenue and Average of Weekly Revenues during Pre-promotion Period
revenue2 <- c(sum(f_pre_promotion$revenue),mean(f_pre_promotion$revenue))
revenue2

# Total Revenue and Average of Weekly Revenues during promotion Period
revenue3 <- c(sum(f_promotion$revenue),mean(f_promotion$revenue))
revenue3

# Total Revenue and Average of Weekly Revenues during Post-promotion Period
revenue4 <- c(sum(f_post_promotion$revenue),mean(f_post_promotion$revenue))
revenue4


## Total Profit and Average of Weekly Profit made in each period respectively

# Total Profit and Average of Weekly Profit during Initial Shakedown Period
profit1 <- c(sum(f_initial_shakedown$profit),mean(f_initial_shakedown$profit))
profit1

# Total Profit and Average of Weekly Profit during Pre-promotion Period
profit2 <- c(sum(f_pre_promotion$profit),mean(f_pre_promotion$profit))
profit2

# Total Profit and Average of Weekly Profit during promotion Period
profit3 <- c(sum(f_promotion$profit),mean(f_promotion$profit))
profit3

# Total Profit and Average of Weekly Profit during Post-promotion Period
profit4 <- c(sum(f_post_promotion$profit),mean(f_post_promotion$profit))
profit4

## Total lbs_old and Average of Weekly lbs_sold made in each period respectively

# Total lbs_old and Average of Weekly lbs_sold during Initial Shakedown Period
lbs_sold1 <- c(sum(f_initial_shakedown$lbs_sold),mean(f_initial_shakedown$lbs_sold))
lbs_sold1

# Total lbs_old and Average of Weekly lbs_sold during Pre-promotion Period
lbs_sold2 <- c(sum(f_pre_promotion$lbs_sold),mean(f_pre_promotion$lbs_sold))
lbs_sold2

# Total lbs_old and Average of Weekly lbs_sold during promotion Period
lbs_sold3 <- c(sum(f_promotion$lbs_sold),mean(f_promotion$lbs_sold))
lbs_sold3

# Total lbs_old and Average of Weekly lbs_sold during Post-promotion Period
lbs_sold4 <- c(sum(f_post_promotion$lbs_sold),mean(f_post_promotion$lbs_sold))
lbs_sold4

###############################################################
# Bar charts
###############################################################

w_var <- round(x = c(w_visits1, w_visits2, w_visits3, w_visits4), digits = 0)
d_var <- round(x = c(d_visits1, d_visits2, d_visits3, d_visits4), digits = 0)
inq_var <- round(x = c(inquiry1[1], inquiry2[1], inquiry3[1], inquiry4[1]), digits = 0)
rev_var <- round(x = c(revenue1[1]/1000000, revenue2[1]/1000000, revenue3[1]/1000000, revenue4[1]/1000000), digits = 1)
pr_var <- round(x = c(profit1[1]/1000000, profit2[1]/1000000, profit3[1]/1000000, profit4[1]/1000000), digits = 1)
lbs_var <- round(x = c(lbs_sold1[1]/1000, lbs_sold2[1]/1000, lbs_sold3[1]/1000, lbs_sold4[1]/1000), digits = 0)
promo_periods <- c("p1","p2", "p3","p4")

bar_data <- as.data.frame(matrix(nrow = 4, ncol = 1))
bar_data$promo_periods <- promo_periods
bar_data$w_var <- w_var
bar_data$d_var <- d_var
bar_data$inq_var <- inq_var
bar_data$rev_var <- rev_var
bar_data$pr_var <- pr_var
bar_data$lbs_var <- lbs_var
bar_data$V1 <- NULL

# Bar Chart 1: Avg.Weekly Visits
library(ggplot2)
bar1 <- ggplot(bar_data, aes(promo_periods, w_var))+
  geom_bar(stat = "identity",fill = "steelblue", width =0.5) +
  labs(x = "Timeline of Campaign", y = "Average Weekly Visits") + 
  geom_text(aes(label=w_var), vjust=-0.3, color="black", size=3.5) +
  ggtitle("Increased Avg.Weekly Visits during Promotion\n") +
  theme_classic()
bar1

# Bar Chart 2: Avg.Daily Visits
bar2 <- ggplot(bar_data, aes(promo_periods, d_var))+
  geom_bar(stat = "identity",fill = "steelblue", width =0.5) +
  labs(x = "Timeline of Campaign", y = "Average Daily Visits") + 
  geom_text(aes(label=d_var), vjust=-0.3, color="black", size=3.5) +
  ggtitle("Increased Avg.Daily Visits during Promotion\n") +
  theme_classic()
bar2

# Bar Chart 3: Total Inquiries
bar3 <- ggplot(bar_data, aes(promo_periods, inq_var))+
  geom_bar(stat = "identity",fill = "steelblue", width =0.5) +
  labs(x = "Timeline of Campaign", y = "Total Inquiries") + 
  geom_text(aes(label=inq_var), vjust=-0.3, color="black", size=3.5) +
  ggtitle("Decreased Total Inquiries during Promotion\n & Post-Promotion\n") +
  theme_classic()
bar3

# Bar Chart 4: Total Revenue
bar4 <- ggplot(bar_data, aes(promo_periods, rev_var))+
  geom_bar(stat = "identity",fill = "steelblue", width =0.5) +
  labs(x = "Timeline of Campaign", y = "Total Revenue (in million dollars)") + 
  geom_text(aes(label=rev_var), vjust=-0.3, color="black", size=3.5) +
  ggtitle("Decreased Total Revenue during Promotion\n & Post-Promotion\n") +
  theme_classic()
bar4

# Bar Chart 5: Total Profit
bar5 <- ggplot(bar_data, aes(promo_periods, pr_var))+
  geom_bar(stat = "identity",fill = "steelblue", width =0.5) +
  labs(x = "Timeline of Campaign", y = "Total Profit (in million dollars)") + 
  geom_text(aes(label=pr_var), vjust=-0.3, color="black", size=3.5) +
  ggtitle("Decreased Total Profit during Promotion\n & Post-Promotion\n") +
  theme_classic()
bar5

# Bar Chart 6: Total lbs. Sold
bar6 <- ggplot(bar_data, aes(promo_periods, lbs_var))+
  geom_bar(stat = "identity",fill = "steelblue", width =0.5) +
  labs(x = "Timeline of Campaign", y = "Total lbs. Sold (in thousands)") + 
  geom_text(aes(label=lbs_var), vjust=-0.3, color="black", size=3.5) +
  ggtitle("Decreased Total lbs. Sold during Promotion\n & Post-Promotion\n") +
  theme_classic()
bar6


