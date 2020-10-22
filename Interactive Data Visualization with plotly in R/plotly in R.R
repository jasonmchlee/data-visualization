vgsales <- read.csv("vgsales.csv")

# load libraries
install.packages("plotly")
library(ggplot2)
library(plotly)
library(dplyr)

# Store the scatterplot of Critic_Score vs. NA_Sales sales in 2016
scatter <- vgsales  %>%
  filter(Year == 2016) %>%
  ggplot(aes(x = NA_Sales, y = Critic_Score)) +
  geom_point(alpha = 0.3)

# Convert the scatterplot to a plotly graphic
ggplotly(scatter)


vgsales2016 <- vgsales %>%
  filter(Year == 2016)

vgsales2016 %>%
  plot_ly(x = ~Critic_Score ,y = ~User_Score, color = ~log(User_Count))%>%
  add_markers()

vgsales %>% 
  plot_ly(x = ~Critic_Score, y = ~User_Score) %>%
  add_markers()


# Fit the regression model of User_Score on Critic_Score
m <- lm(User_Score ~ Critic_Score, data = vgsales2016)

# Create the scatterplot with smoother
vgsales2016 %>%
  select(User_Score, Critic_Score) %>%
  na.omit() %>%
  plot_ly(x = ~Critic_Score, y = ~User_Score) %>%
  add_markers(showlegend = FALSE) %>%
  add_lines(y = fitted(m))

activision <- vgsales %>%
  filter(Publisher == "Activision")
ea <- vgsales %>%
  filter(Publisher == "EA")
nintendo <- vgsales %>%
  filter(Publisher == "Nintendo")

# Compute density curves
d.a <- density(activision$Critic_Score, na.rm = TRUE)
d.n <- density(nintendo$Critic_Score, na.rm = TRUE)

# Overlay density plots
plot_ly() %>%
  add_lines(x = ~d.a$x, y = ~d.a$y, name = "Activision", fill = 'tozeroy') %>%
  add_lines(x = ~d.n$x, y = ~d.n$y, name = "Nintendo", fill = 'tozeroy') %>%
  layout(xaxis = list(title = 'Critic Score'),
         yaxis = list(title = 'Density'))