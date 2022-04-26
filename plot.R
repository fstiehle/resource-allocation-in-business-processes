library(readr)
library(dplyr)
library(tidyr)
library(igraph)
library(graphlayouts)
library(ggraph)
library(ggplot2)
library(scales)
library(stringr)

data <- read_csv("Literature_review_collection.xlsx - DataExtraction.csv", 
  col_types = cols(Title = col_character(), 
  Year = col_date(format = "%Y")),
  skip = 2)

colors = c("#8CCCEC", "#392C87", "#61A899", "#36683C", "#DACC82", "#999944")
colors_rating = c("#D86933", "#E49845", "#EEC35A", "#F6F171", "#CEDFB0", "#9FC094")

data <- data %>% 
  filter(Country != "no")

# Publication trends
by_country <- data %>%
  group_by(Country) %>%
  summarise(n=n()) %>%
  arrange(n, desc(Country))

other <- by_country %>%
  filter(n<=1)
by_country <- by_country %>%
  filter(! Country %in% other$Country)
  
rbind(by_country, data.frame(Country="Other (<2)", n=count(other)))
by_country$ID <- seq.int(nrow(by_country))
by_country$Country <- factor(by_country$ID, 
                             labels=by_country$Country,
                             levels=by_country$ID,
                             ordered = TRUE)

ggplot(by_country, aes(x=n, y=Country)) +
  geom_bar(stat="identity", fill=colors[1]) +
  ylab("Country") +
  xlab("Number of Publications") + 
  scale_x_continuous(n.breaks=max(by_country$n), minor_breaks = NULL)
ggsave("countries.PDF", width = 4, height = 2)

# By year
ggplot(data, aes(x=Year)) +
  geom_bar(fill=colors[1]) +
  ylab("Publication Year") +
  xlab("Number of Publications") + 
  scale_x_date(date_labels = "%Y", breaks = "3 years", minor_breaks = NULL) +
  scale_y_continuous(n.breaks=max(by_country$n), minor_breaks = NULL)
ggsave("years.PDF", width = 4, height = 2)

# Optimization goals
goals <- data %>%
  group_by(Goal, `Goal Type`) %>%
  summarise(n=n()) %>%
  arrange(`Goal Type`, n)

goals$ID = seq.int(nrow(goals))
goals$Goal <- factor(goals$ID, 
  labels=goals$Goal,
  levels=goals$ID,
  ordered = TRUE)

ggplot(goals, aes(y=Goal, x=n, fill=`Goal Type`)) +
  geom_bar(stat='Identity') +
  scale_fill_manual(name = element_blank(), values = colors) +
  scale_x_continuous(breaks=pretty_breaks(n=floor(max(goals$n)/2)), minor_breaks = NULL) +
  xlab("Number of Publications")
ggsave("goals.PDF", width = 6, height = 2)

# Allocation Capabilities
ggplot(data, aes(x=`Allocation capability`)) +
  geom_bar(fill=colors[4]) +
  ylab("Number of Publications") + 
  scale_y_continuous(breaks=pretty_breaks(n=7), minor_breaks = NULL) + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
ggsave("allocation_small.PDF", width = 3, height = 2)

# Solution techniques grouped by their allocation goal
sol <- select(data, Goal, `Resource allocation technique type`)
sol$t <- sol$`Resource allocation technique type`
sol$t[sol$t == "Rule" | sol$t == "Logic programming"] <- "Rule or Logic programming" 
sol$t[sol$t == "Machine Learning" | sol$t == "Genetic algorithm" | sol$t == "Trained rule"] <- "ML, Genetic or Trained" 

# we do this for ordering
sol <- sol %>%
  group_by(Goal) %>%
  mutate(n = n()) %>%
  arrange(n)

sol$ID = seq.int(nrow(sol))
sol$Goal <- factor(sol$ID, 
                     labels=sol$Goal,
                     levels=sol$ID,
                     ordered = TRUE)

ggplot(sol, aes(y=Goal, fill=t)) +
  geom_bar() +
  scale_fill_manual(name = element_blank(), values = colors) +
  scale_x_continuous(breaks=pretty_breaks(n=floor(max(sol$n)/2)), minor_breaks = NULL) +
  xlab("Number of Publications")
ggsave("solution_technique.PDF", width = 8, height = 2)

# Maturity levels
# we do this for ordering
mat <- select(data, `Evaluation Type`, `Prototypical implementation type`)
mat <- mat %>%
  group_by(`Evaluation Type`) %>%
  mutate(n = n()) %>%
  arrange(n)

mat$ID = seq.int(nrow(mat))
mat$`Evaluation Type` <- factor(mat$ID, 
                   labels=mat$`Evaluation Type`,
                   levels=mat$ID,
                   ordered = TRUE)

ggplot(mat, aes(y=`Evaluation Type`, fill=`Prototypical implementation type`)) +
  geom_bar() +
  scale_fill_manual(name = "Prototype", values = c("available"= colors_rating[5],
                                                       "not acc., pseudoc." = colors_rating[4],
                                                       "pseudocode" = colors_rating[3],
                                                       "not accessible" = colors_rating[2],
                                                       "no" = colors_rating[1])) +
  scale_x_continuous(breaks=pretty_breaks(n=floor(max(mat$n)/2)), minor_breaks = NULL) +
  xlab("Number of Publications")
ggsave("evaluation.PDF", width = 8, height = 2)

ggplot(data, aes(x=str_to_title(Maturity), fill=str_to_title(Maturity))) +
  geom_bar() +
  scale_fill_manual(name = element_blank(), values = rev(tail(colors_rating, -1))) +
  xlab("Maturity Level") +
  ylab("Number of Publications") +
  theme(legend.position="none") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "#353839")
ggsave("maturity.PDF", width = 3, height = 2)