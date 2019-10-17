# KPIs

library(tidyverse)
library(readxl)
library(ggridges)

df <- read_excel("Data/retail.xlsx") %>%
  mutate(TotalAmount = Quantity * UnitPrice,
         Month = lubridate::month(InvoiceDate))

# n customers, n purchases
n_distinct(df$CustomerID)
n_distinct(df$InvoiceNo)

# monthly revenue
df %>%
  group_by(Month) %>%
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
  summarise(Min_month = min(Month)) %>%
  count(Min_month) %>%
  ggplot(aes(Min_month,n)) +
  geom_col()

# growth rate comparing present with previous month
df %>%
  group_by(CustomerID) %>%
  summarise(Min_month = min(Month)) %>%
  count(Min_month) %>%
  mutate(Growth_Rate = (n - lag(n))/ lag(n) * 100) %>%
  ggplot(aes(Min_month,Growth_Rate)) +
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
  group_by(Month) %>%
  summarise(mean = mean(TotalAmount),
            median = median(TotalAmount))

df %>%
  mutate(mean = mean(TotalAmount),
         sd = sd(TotalAmount)) %>%
  filter(TotalAmount < mean + sd*2,
         TotalAmount > mean - sd*2) %>%
  ggplot(aes(TotalAmount)) +
  geom_density()

# avg order size
df %>%
  group_by(InvoiceNo) %>%
  count() %>%
  ungroup() %>%
  summarise(mean = mean(n),
            median = median(n))
df %>%
  group_by(Month,InvoiceNo) %>%
  count() %>%
  ungroup() %>%
  group_by(Month) %>%
  summarise(mean = mean(n),
            median = median(n))

df %>%
  group_by(Month,InvoiceNo) %>%
  count() %>%
  ungroup() %>%
  mutate(Month = factor(Month, levels = rev(1:12))) %>%
  filter(n<80) %>%
  ggplot(aes(x=n,y=Month)) +
  geom_density_ridges()
