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
  skip = 1)

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
  
by_country <- rbind(data.frame(Country="Other (<2)", n=count(other)), by_country)
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
ggsave("countries.PDF", width = 5, height = 3)

# By year
ggplot(data, aes(x=Year)) +
  geom_bar(fill=colors[1]) +
  xlab("Publication Year") +
  ylab("Number of Publications") + 
  scale_x_date(date_labels = "%Y", breaks = "3 years", minor_breaks = NULL) +
  scale_y_continuous(n.breaks=max(by_country$n), minor_breaks = NULL) 
ggsave("years.PDF", width = 5, height = 3)

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
  geom_text(aes(label = n), stat = "identity", hjust = 2, colour = "white" ,  size=2) +
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
sol <- select(data, `Resource allocation technique type`)
sol$t <- sol$`Resource allocation technique type`

# Count occurrences
sol_counts <- table(sol$t)

# Create a data frame for plotting
sol_df <- data.frame(t = names(sol_counts), n = as.numeric(sol_counts))

# Sort the data frame by count in descending order
sol_df <- sol_df[order(sol_df$n, decreasing = TRUE), ]

# Plotting using ggplot
ggplot(sol_df, aes(x = n, y = reorder(t, n), fill = t)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  scale_fill_manual(name = element_blank(), values = c("grey",colors[5],colors[3],colors[1],colors[2],colors[4],"#5D8A7D","#5D8A7D",colors[1])) +
  scale_x_continuous(breaks = pretty_breaks(n = floor(max(sol_df$n) / 2)), minor_breaks = seq(length.out = max(sol_df$n), from = 0, by = 1)) +
  geom_text(aes(label = n), stat = "identity", hjust = 2, colour = "white" ,  size=2) +
  xlab("Number of Publications") +
  ylab("Solution Technique")
 ggsave("solution_technique.PDF", width = 5, height = 2)

# Maturity levels
# we do this for ordering
mat <- select(data, `Evaluation Type`, `Prototypical implementation type`)
mat <- mat %>%
  group_by(`Evaluation Type`, `Prototypical implementation type`) %>%
  mutate(n = n()) %>%
  arrange(n)

mat$`Prototypical implementation type` = str_to_title(mat$`Prototypical implementation type`)
mat$`Evaluation Type` = str_to_title(mat$`Evaluation Type`)

order_y = str_to_title(c(
  "No evaluation",
  "Toy example",
  "Case study",
  "(Comp.) sim. experiments",
  "(Comp.) experiments",
  "Experiments + Case study"
))
order_x = str_to_title(c(
  "no implementation",
  "not accessible",
  "pseudocode",
  "not acc., pseudoc.",
  "available"
))

mat$ID = seq.int(nrow(mat))
mat$`Evaluation Type` <- factor(mat$ID, 
                   labels=mat$`Evaluation Type`,
                   levels=mat$ID,
                   ordered = TRUE)

ggplot(mat, aes(y=factor(mat$`Evaluation Type`, level=order_y), x=factor(mat$`Prototypical implementation type`, level=order_x), size=n)) +
  geom_point(colour=colors[1], fill=colors[2], alpha=0.8, shape=21) + 
  scale_size(range = c(5, 15)) +
  guides(size="none") +
  geom_text(aes(label = n), colour = "white", size=mat$n*0.5 + 2, check_overlap = T) +
  xlab("Implementation") +
  ylab("Evaluation Type") +
  theme(axis.text.x = element_text(angle =25, hjust=1)) +
  theme(axis.title.x = element_text(vjust=-1)) 
ggsave("evaluation.PDF", width = 5, height = 3, dpi = 300)

#ggplot(data, aes(x=str_to_title(Maturity), fill=str_to_title(Maturity))) +
#  geom_bar() +
#  scale_fill_manual(name = element_blank(), values = rev(tail(colors_rating, -1))) +
#  xlab("Maturity Level") +
#  ylab("Number of Publications") +
#  theme(legend.position="none") + 
#  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "#353839")
#ggsave("maturity.PDF", width = 3, height = 2)