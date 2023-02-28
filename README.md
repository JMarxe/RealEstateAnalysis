# Real Estate Data Analysis (R)

## Introduction

#### This Dataset contains Real Estate statistics in the US broken by State and zip code. Data was collected via web scraping using python libraries, extracted from Realtor.com with a Temporal Coverage Start Date of 02/28/2022 and an End Date of 03/16/2022. This CSV file contains the following variables: status, price, bed, bath, acre_lot, full address, street, city, state, zip_code, house_size, and sold_date. With this Dataset we would like to predict the housing prices based on this data given, find which location has the highest prices, the correlation between house prices and other attributes, and the trend behind the housing prices. 

```{r echo=T, results= 'hide', error=FALSE, warning=FALSE, message=FALSE  }

library(tidyverse)
library(scales)
library(lemon)
library(plotly)
library(lubridate)

options(sciphen = 999)
real_estate<- read.csv("realtor-data.csv")
view(real_estate)
```


## Average Price of House Per State

```{r}
real_estate%>%
  drop_na(price)%>%
  select(price, state)%>%
  group_by(state)%>%
  summarize(average_price = mean(price))%>%
  arrange(desc(average_price))%>%
  mutate(state = fct_reorder(state, average_price))%>%
  ggplot(aes(x = average_price, y = state, fill = state))+
  geom_col(show.legend = FALSE)+
  scale_x_continuous(labels= dollar_format())+
  labs(title = "Average price of house per State", x = "Average_Price", y = "State")
```

<p align="center">
<img src="https://user-images.githubusercontent.com/114123232/221841691-8056ad52-74be-484e-ba91-ad7d72758ab0.png" height="80%" width="80%"/>
<br />

#### This bar chart above shows the average price of house per State. It is also in descending order, which teaches us that New York has the highest price per house while South Carolina has the lowest. From New Jersey to Vermont the average is close to $500,000.

## Relationship Between Bed and House Size

```{r}
real_estate%>%
  na.omit()%>%
  filter(state == "New York")%>%
  ggplot(aes(x = bed, y = house_size))+
  geom_point(color = "#CC33CC", alpha = 0.2)+
  labs(title = "House Size Compared to Number of Beds",subtitle = "In New York", x = " Bed", y = "House_Size")
```

#### This shows the relationship between the quantity of beds and house size in New York. Most Houses are between 0-20 beds but close to 30,000 sq/feet. This shows that more beds do not necessarily equal to bigger houses. We can also see fewer but more distant outliers in the house size area than in the numbers of bed area.

## Price and House Size in Puerto Rico, San Juan
```{r}
real_estate %>%
  filter(state == "Puerto Rico" & city == "San Juan")%>%
  arrange(desc(price)) %>%
  distinct(full_address, .keep_all = TRUE) %>%
  ggplot() +
  aes(x = house_size, y = price, ) +
  geom_point(color = "#0000FF", shape = 6, alpha = 0.4, size = 3) +
  scale_y_continuous(labels = dollar_format()) +
  labs(x = "House Size", y = "Price", title = "Relationship between House Size and Price in San Juan, Puerto Rico", caption = "Majority of house sizes in San Juan are less than 10000 with the price being extremely low, close to zero.")
```

#### This scatter plot gives me an understanding of the relationship between house sizes and prices in a specific location. My intention with making this visual is for me to get a better understanding of how much it would cost to live in San Juan, Puerto Rico and what I found was that prices were extremely low with house sizes less than 10000 sq feet reaching close to 0$. The outliers being the 2 house sizes nearing 30000 sq feet. This was useful in our research as Puerto Rico was one of the states with significant data and we wanted to determine if greater house sizes would result in greater prices. While small house sizes seem to correlate with low prices it fluctuates significantly as it appears that small house sizes can have big prices than big house sizes and vice versa. 

## Price and House sizes of 3 bed 2 bath in Woonsocket, Rhode Island
```{r}
real_estate %>%
  filter(state == "Rhode Island" & city == "Woonsocket")%>%
  filter(bed == 3 & bath == 2) %>%
  select(house_size, price) %>%
  arrange(desc(price)) %>%
  distinct(house_size, .keep_all = TRUE) %>%
  ggplot() +
  aes(y = house_size, x = price, color = price) +
  geom_boxplot() +
  scale_x_continuous(labels = dollar_format()) +
  labs(y = "House Size", x = "Price", title = "Boxplot for House Sizes and Prices in Woonsocket, Rhode Island", subtitle = "3 bed and 2 bath houses", caption = "House sizes between 1200-1800 seem to vary in cost from $190,000-$400,000 with the two outliers being the house sizes of 2500+." )
  
```

#### This boxplot provides a better representation of the relationship between house sizes and prices in Woonsocket, Rhode Island as we were looking for how different it would be if we selected specifically 3 bed and 2 bath houses. In this visual we can see that there is a cluster and countable outliers as the majority of house sizes between 1200-1800 sq ft have a price range from $190,000-$400,000. This displays to use that a bigger house size doesn't correlate with a greater price as for example a house size of 1560 sq ft has a price of about $300,000 while a house size of 1250 sq ft has a price of about $400,000. This was useful to our research because it implies that other variables such as location and state play a big factor in determining price. 


## Price Distribution in Ponce

```{r echo=T, results = 'hide', error=FALSE, warning=FALSE, message=FALSE}

real_estate %>% 
  filter(city == "Ponce") %>% 
  ggplot() +
  aes(x = price, y = city, fill = price) + 
  geom_boxplot() +
  coord_flip() + 
  scale_x_continuous(labels = dollar_format()) + 
  labs (x = "Price", y = "City in Puerto Rico") +
  labs (title = "House Prices in Puerto Rico" , subtitle = "In one of the cities in PR, Ponce") 


```

#### In the boxplot above, it shows the house prices in Ponce, Puerto Rico. You can see that the average price of houses in the city ranges from about $100,000 to $250,000. While that's considered the average, there are some houses that are more expensive then that, and the most expensive house in the city is almost $2 million dollars. 

## Houses For Sale in the States
```{r echo=T, results = 'hide', error=FALSE, warning=FALSE, message=FALSE}

real_estate %>%
  count(state, sort = TRUE) %>%
  mutate(name = fct_reorder(state, n)) %>%
  head(10) %>%
  ggplot(aes(x = state, y = n, fill = name)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1)) +
  labs(x = "State" , y = "Count") +
  labs(title = "States with the most houses for sale", subtitle = "Top 10")

```

#### A bar graph was created to show the top 10 states with the most houses for sale. You can see that New Jersey has the most, while New York almost has the same amount of houses for sale as New Jersey. You can also see that Puerto Rico is on the top 10 list because they have around 25,000 houses for sale. 


## Conclusion

#### The purpose of our visuals was to display relationship between price and house size. We wanted to think as a real estate agent and pose questions we would need to answer if we were asked to conduct research on a certain area for houses and determine prices for clients. It was interesting because with this information we can make several conclusions about these two variables based on the areas we researched. We can conclude that New Jersey had the most houses available for sale, and they had about 25,000 houses available. You can also conclude that houses in Ponce weren't cheap, with the most expensive one being almost $2 million dollars. In this data set, the majority of the data didn't have have dates. The only date that was listed in the data set was when the house was sold, which the majority of them weren't at the time. We wanted to observe some key states that we wish were included with dense populations such as California or Florida. It would’ve been interesting to compare with the smaller states we researched just to see the varying prices as we are certain the would be exponentially higher than in these other states regardless of house size. Notable findings have been the outliers in our graphs that we have found as it was interesting to discover fluctuations in our data. Real estate agencies could use our information to make better inferences and decisions when selling a home or trying to help purchase one for a client. This type of data is useful for target market research.
