# KPIs

library(tidyverse)
library(readxl)

df <- read_excel("Data/retail.xlsx") %>%
  mutate(TotalAmount = Quantity * UnitPrice)

# n customers, n purchases
n_distinct(df$CustomerID)
n_distinct(df$InvoiceNo)

# monthly revenue
df %>%
  group_by(Month = lubridate::month(InvoiceDate)) %>%
  summarise(Revenue =sum(TotalAmount)) %>%
  ggplot(aes(Month , Revenue)) +
  geom_col() +
  scale_y_continuous(labels = function(n) {
    trans = n / 1000
    paste0(trans, "K")
  })

# n new customers by month
df %>%
  group_by(CustomerID) %>%
  summarise(Month = min(lubridate::month(InvoiceDate))) %>%
  count(Month) %>%
  ggplot(aes(Month, n)) +
  geom_col()

# arpu
df %>% 
  group_by(CustomerID) %>%
  summarise(Revenue = sum(TotalAmount)) %>%
  summarise(mean = mean(Revenue),
            median = median(Revenue))

# arpu by country
df %>% 
  group_by(Country,CustomerID) %>%
  summarise(Revenue = sum(TotalAmount)) %>%
  summarise(mean = mean(Revenue),
            median = median(Revenue),
            n = n()) %>%
  arrange(desc(median))
  
# Revenue per purchase (invoice)
df %>%
  summarise(mean = mean(TotalAmount),
            median = median(TotalAmount))

df %>%
  mutate(mean = mean(TotalAmount),
         sd = sd(TotalAmount)) %>%
  filter(TotalAmount < mean + sd*2,
         TotalAmount > mean - sd*2) %>%
  ggplot(aes(TotalAmount)) +
  geom_density()
