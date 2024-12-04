# Clear the environment
rm(list=ls())

# Load initial libs.
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(ggdark)
library(naniar)

# Load ds
full <- read.csv('Merged_redux.csv')
# Set blanks to 'NA':
fult <- full %>% replace_with_na(replace = list(Armament.category = 0)) 
fult2 <- fult %>% replace_with_na(replace = list(Armament.category = ""))
# Rename cols.
names(fult2)[names(fult2) == 'Partner.Name'] <- 'Partner'
names(fult2)[names(fult2) == 'Armament.category'] <- 'Armament'
# Parse df by exports and imports
exp <- subset(fult2, (Trade.Flow == 'Export'))
imp <- subset(fult2, (Trade.Flow == 'Import'))

# Export and Import Levels each year
h <- ggplot(fult2,aes(x=Year, y=Amount, colour=Partner)) +
  geom_point() + 
  dark_theme_light() + 
  theme(text = element_text(family = "Optima")) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x="Year",
       y = "Amount (Thousand $)",
       title = "Trade Flows via 'Country'",
       subtitle = "Overall Imp/Exp Levels",
       caption = "Sources: WITS") +
  theme(legend.position = "bottom") +
  facet_wrap(Country~Trade.Flow, ncol =2)
invert_geom_defaults()
h

# Trade Flows Breakdown
Paired2 <- fult2 %>% filter(Year != "2022")
Paired22 <- Paired2 %>% filter(Year >= "1996")

set.seed(123)
sampled_trade <- Paired22 %>%
  sample_n(500)  # Sample 500 observations for a clearer plot

aggregated_trade <- Paired22 %>% 
  group_by(Year, Trade.Flow) %>%
  summarise(Mean_Value = mean(Amount, na.rm = TRUE), .groups = 'drop')

ggplot(aggregated_trade, aes(x = Year, y = Mean_Value, color = Trade.Flow)) +
  geom_line(size = 1) +
  scale_y_continuous(labels=scales::dollar) +
  geom_point() +
  labs(
    title = "Trade Trends Over Time",
    x = "Year",
    y = "Mean Trade Value (Thousand)",
    subtitle = "An Overall View",
    caption = "Sources: WITS",
    color = "Trade Flow"
  ) +
  theme_few()

# By country imp/exps
ggplot(data = fult2, mapping = aes(x = Year, y = Amount, color = Country)) +
  geom_point() + 
  dark_theme_dark() +
  theme(text = element_text(family = "Optima")) +
  scale_y_continuous(labels=scales::dollar) +
  labs(x="Year",
       y = "Amount (Thousand $)",
       title = "Overall Import/Export Levels",
       subtitle = "Trade Over Time",
       caption = "Sources: WITS") +
  #coord_flip() +
  theme(legend.position = "right") +
  facet_wrap(~Trade.Flow)

# Trade per Country, per Partner
p2 <- ggplot(data = fult2, mapping = aes(x = Year, y = Amount, color = Country))
p2 + geom_line(color="gray70", aes(group = Partner))+
  geom_smooth(linewidth = 1.1, method = "loess", se = FALSE)+
  scale_y_log10(labels=scales::dollar)+
  facet_wrap(~ Trade.Flow, ncol = 5)+
  theme_economist() +
  theme(text = element_text(family = "Optima"),
        #axis.title.x = element_text(hjust=1), 
        #axis.title.y = element_text(hjust=1),
        axis.title.y = element_text(vjust = 4),
        axis.title.x = element_text(vjust = -2)) +
  labs(x = "Year",
       y = "Trade Level (Thousands, Logged)",
       title = "Directional Trade Flows",
       subtitle = "Partner Nations are in Grey",
       caption = "Source: WITS")

# Plot TII Values by country
p3 <- ggplot(data = fult2, mapping = aes(x = Year, y = TII.Score, color = Partner)) +
  geom_point(size = 2) +
  dark_theme_gray() +
  theme(text = element_text(family = "Optima")) +
  labs(x = "Year",
       y = "TII Rank",
       title = "Trade Flows",
       subtitle = "How Trade Varies by Bilateral Realtionship",
       caption = "Sources: WITS") +
  theme(legend.position = "top") +
  facet_wrap(~Country)
p3
  
# TII each year
p1 <- ggplot(data = fult2,
             mapping = aes(x= reorder(Partner, TII.Score, na.rm = TRUE),
                           y = TII.Score, color = Country)) +
      geom_point() +
      dark_theme_gray() +
      theme(text = element_text(family = "Optima")) +
      labs(x=NULL,
        y = "TII Score",
        title = "Trade Flows via 'Partner'",
        subtitle = "How Trade Varies by Bilateral Relationship",
        caption = "Sources: WITS") +
      coord_flip() +
      theme(legend.position = "top") +
      facet_wrap(~Year)
p1

# Bat-chart of weapons, US makes up most
W22 <- fult2 %>% filter(Armament != 'NA')
ggplot(data = W22, mapping = aes(x = Armament, fill = Country)) +
  geom_bar() +
  theme_economist() +
  theme(text = element_text(family = "Optima"),
        #axis.title.x = element_text(hjust=1), 
        #axis.title.y = element_text(hjust=1),
        axis.title.y = element_text(vjust = 4),
        axis.title.x = element_text(vjust = -2)) +
  labs(x = "Armament Type",
       y = "Freq. (#)",
       title = "Types of Arm",
       subtitle = "Weapons Type Supplied via 'Country'",
       caption = "Sources: SIPRI") +
  theme(legend.position = "right") 

# Arms over Time
Paired <- fult2 %>% filter(Country == "United States" | Country == "Russia" | Country == "China")

set.seed(123456)
sampled_weapons <- Paired %>%
  sample_n(100)

top_weaponstrade <- Paired %>%
  top_n(50, TIV.delivery.values)

ggplot(sampled_weapons, aes(x = Delivery.year, y = Recipient, color = Country)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Arms Transfer Trends Over Time",
    subtitle = "A Subset of Top Fifty Trades by TIV",
    caption = "Sources: SIPRI",
    x = "Year",
    y = "Recipient Nation",
    color = "Supplier"
  ) +
  theme(text = element_text(family = "Optima")) +
  dark_theme_gray()

# Scatter of TIVs
W22 <- fult2 %>% filter(Armament != 'NA')
ggplot(data = W22, mapping = aes(x = Year, y = TIV.delivery.values, color = Country)) +
  geom_point() +
  dark_theme_bw() +
  theme(text = element_text(family = "Optima"),
        #axis.title.x = element_text(hjust=1), 
        #axis.title.y = element_text(hjust=1),
        axis.title.y = element_text(vjust = 4),
        axis.title.x = element_text(vjust = -2)) +
  labs(x = "Armament Type",
       y = "TIV Value",
       title = "Types of Arm",
       subtitle = "Weapons Type By Type",
       caption = "Sources: SIPRI") +
  theme(legend.position = "right") +
  facet_wrap(~Armament)

# !UNUSED!Variable Width-bar of SIPRI of missiles, aircraft and armored vech.
Wepns <- fult2[,c(13,23)]
Wepns2 <- Wepns %>%
  group_by(Armament) %>%
  mutate(Arm_count = n())
W2 <- Wepns2 %>% filter(Armament == 'Missiles' |Armament == 'Aircraft' |Armament == 'Armoured vehicles')
W2$right <- cumsum(W2$Arm_count) + 5*c(0:(nrow(W2)-1))
W2$left <- W2$right - W2$Arm_count 
# Plot
h1 <- ggplot(W2, aes(ymin = 0)) + 
  geom_rect(aes(xmin = left, xmax = right, ymax = TIV.delivery.values, colour = Armament, fill = Armament)) +
  #scale_y_continuous() +
  scale_x_comma() +
  labs(x = "# Of Arms Type", 
       y = "TIV", 
       title = "Estimated TIV of Top Three Arms",
       subtitle = "Weapons via 'Type'",
       caption = "Sources: SIPRI") +
  theme_solarized() +
  theme(text = element_text(family = "Optima")) +
  theme(legend.position="right") 
h1

# Boxplots
library(patchwork)
W2 <- Wepns2 %>% filter(Armament == 'Missiles' |Armament == 'Aircraft' |Armament == 'Armoured vehicles')
W22 <- fult2 %>% filter(Armament != 'NA')
p5 <- ggplot(W22, aes(x=Armament, y=TIV.delivery.values, fill = Armament)) + 
  geom_boxplot(outlier.colour="navy", outlier.shape=8,
               outlier.size=1) +
  labs(x = "Arms Type", 
       y = "TIV", 
       title = "Estimated TIV of Arms",
       subtitle = "Weapons via 'Type'",
       caption = "Sources: SIPRI") +
  theme_solarized() +
  theme(text = element_text(family = "Optima")) +
  theme(legend.position="none") 
p5

p6 <- ggplot(W22, aes(x=Armament, y=TII.Score, fill = Armament)) + 
  geom_boxplot(outlier.colour="navy", outlier.shape=8,
               outlier.size=1) +
  labs(x = "Arms Type", 
       y = "TII Rank", 
       title = "Estimated TII of Arms",
       subtitle = "Weapons via 'Type'",
       caption = "Sources: SIPRI and WITS") +
  theme_solarized() +
  theme(text = element_text(family = "Optima")) +
  theme(legend.position="right") 
p6

combined_plot <- p5 + p6 + plot_layout(ncol = 2)
combined_plot

# Top 3 arms and TII Beeswarm
W222 <- fult2 %>% filter(Armament == 'Missiles' |Armament == 'Aircraft' |Armament == 'Armoured vehicles')
library(ggbeeswarm)
f <- ggplot(W222, aes(x = Armament, y = TIV.delivery.values, colour = Partner)) +
  geom_beeswarm(corral = "gutter", cex = 3) +
  #theme(text = element_text(family = "Optima")) +
  labs(x = "Armament", 
       y = "TIV Value", 
       title = "TIVs of Top Three Arms via Country",
       subtitle = "Who Gives The Most?",
       caption = "Source: SIPRI") +
  theme(legend.position = "right") +
  theme_clean() +
  geom_hline(yintercept=1.67, linetype="dashed", color = "red", linewidth=2) + 
  facet_wrap(~Country)
f

# !UNUSED!Ridge plots of TIV and TII values
# Missing is explained!
# Artifact (Unused)! ---> facyr <- as.factor(imp$Year)
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
ggplot(fult2, aes(x = TIV.delivery.values, y = Partner, fill = stat(x))) +
  geom_density_ridges_gradient(rel_min_height = 0.005) +
  scale_fill_viridis_c(name = "Range Amount", option = "C") +
  theme_ridges() +
  theme(text = element_text(family = "Optima")) +
  coord_cartesian(clip = "off") +
  labs(x = "TIV Values", y = "Partner Country",
       title = "TIV Distro (Totals)",
       subtitle = "Missing data: Own Country or\nNo Bilateral Weapons Trade\nData Available",
       caption = "Sources: SIPRI and WITS") +
  #theme(legend.position="bottom") +
  facet_wrap(~Country)

ggplot(fult2, aes(x = TII.Score, y = Partner, fill = stat(x))) +
  geom_density_ridges_gradient() +
  scale_fill_viridis_c(name = "Range Amount", option = "C") +
  theme_ridges() +
  theme(text = element_text(family = "Optima")) +
  coord_cartesian(clip = "off") +
  labs(x = "TII Values", y = "Partner Country",
       title = "TII Distro (Totals)",
       subtitle = "Missing data: Own Country",
       caption = "Sources: SIPRI and WITS") +
  facet_wrap(~Country)

# TIV and TII compare
ggplot(fult2, aes(x=TII.Score, y=TIV.delivery.values) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  labs(x = "TII Rank", 
       y = "TIV Value", 
       title = "TIV vs. TII",
       subtitle = "A Prelimanary Examination",
       caption = "Sources: SIPRI and WITS") +
  dark_theme_bw() +
  theme(text = element_text(family = "Optima")) +
  theme(legend.position="right") +
  scale_fill_continuous(name="Correlation\nValue")

# Density Plots
wpns3 <- c("Aircraft", "Sensors", "Armoured vehicles",
           "Air defense systems", "Artillery",
           "Engines", "Missles",
           "Naval weapons", "Ships",
           "Other")
ggplot(data = subset(fult2, subset = Armament %in% wpns3),
       mapping = aes(x = TIV.delivery.values, fill = Armament, color = Armament)) +
  theme(text = element_text(family = "Optima")) +
  labs(x = "TIV Values", y = "Density Estimate",
       title = "Distro of TIVs",
       subtitle = "Getting an Sense of Composition (Upto ~ 300)",
       caption = "Sources: SIPRI") +
  geom_vline(aes(xintercept=59),
             color="blue", linetype="dashed", linewidth=0.5) +
  theme_few() +
  xlim(0, 300) +
  geom_density(alpha = 0.3)

mean(fult$TIV.delivery.values,na.rm = TRUE)

ggplot(data = (fult2),
       mapping = aes(x = TII.Score, fill = Country, color = Country)) +
  theme(text = element_text(family = "Optima")) +
  labs(x = "TII Rank", y = "Proportional Density Estimate",
       title = "Distro of TIIs",
       subtitle = "Getting an Sense of Composition (Upto ~ 10)",
       caption = "Sources: WITS") +
  geom_vline(aes(xintercept=1.67),
             color="red", linetype="dashed", linewidth=0.5) +
  theme_few() +
  xlim(0, 10) +
  geom_density(alpha = 0.3, mapping = aes(y = ..scaled..))

# Trade and Weapn. Vals.
library(hrbrthemes)

fult22 <- fult2 %>% filter(Armament != 'NA' )
fult222 <- fult22 %>% filter(Armament != 'Other')
f1 <- fult222 %>%
      arrange(desc(SIPRI.estimate)) %>%
       #mutate(country = factor(country, country)) %>%
       ggplot(aes(x=Amount, y=TII.Score, size=TIV.delivery.values, color=Armament)) +
       geom_point(alpha=0.5) +
  scale_colour_ft() +
       scale_x_log10(labels=scales::dollar) +
       scale_size(range = c(.1, 24), name="TIV Value Range") +
       dark_theme_light() +
       theme(text = element_text(family = "Optima")) +
       labs(x = "Amount per Year (Logged, Thousand $)", y = "TII Rank",
       title = "Trade Flows and Weapons",
       subtitle = "How Trade Levels Relate to\nWeapons Transfered (sans 'Other')",
       caption = "Sources: SIPRI and WITS") +
      geom_hline(aes(yintercept=1.67),
             color="red", linetype="dashed", linewidth=0.5) +
       facet_wrap(~Trade.Flow, ncol = 2)
f1

f2 <- fult222 %>%
  arrange(desc(SIPRI.estimate)) %>%
  #mutate(country = factor(country, country)) %>%
  ggplot(aes(x=Amount, y=TII.Score, size=TIV.delivery.values, color=Armament)) +
  geom_point(alpha=0.5) +
  scale_colour_ft() +
  scale_x_log10(labels=scales::dollar) +
  scale_size(range = c(.1, 24), name="TIV Value Range") +
  dark_theme_light() +
  theme(text = element_text(family = "Optima")) +
  labs(x = "Overall Trade Amount per Year (Logged, Thousand $)", y = "TII Rank",
       title = "Trade Flows and Weapons by 'Supplier'",
       subtitle = "How Trade Levels Relate to\nWeapons Transfered (sans 'Other')",
       caption = "Sources: SIPRI and WITS") +
  geom_hline(aes(yintercept=1.67),
             color="red", linetype="dashed", linewidth=0.5) +
  facet_wrap(~Country, ncol = 2)
f2

# Make Sankley Graphics
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(networkD3)
# Way 1
nodes <- data.frame(name=c(as.character(fult2$Country), as.character(fult2$Partner)) %>% unique())
fult2$IDsource=match(fult2$Country, nodes$name)-1
fult2$IDtarget=match(fult2$Partner, nodes$name)-1
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

sankeyNetwork(Links = fult2, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "Amount", NodeID = "name",
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)

sankeyNetwork(Links = fult2, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "TIV.delivery.values", NodeID = "name",
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)

# Way 2
# Trade
links = data.frame(data.frame(
                     source = fult2$Country, 
                      target = fult2$Partner,
                      value = fult2$Amount))
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

p8 <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   nodeWidth=35, fontSize=23, 
                   nodePadding=200,
                   height = 800,
                   width = 400,
                   iterations = 32, 
                   sinksRight=FALSE)
p8
# TIVs
links = data.frame(data.frame(
  source = fult2$Country, 
  target = fult2$Partner,
  value = fult2$TIV.delivery.values))
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

p9 <- sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "IDsource", Target = "IDtarget",
                    Value = "value", NodeID = "name", 
                    nodeWidth=40, fontSize=23, 
                    nodePadding=30,
                    height = 800,
                    width = 800,
                    sinksRight=FALSE)
p9

# Network graphs
library(igraph)
# Trade
myvars <- c("Country", "Partner", "Amount")
all <- fult2[myvars]
g <- graph_from_data_frame(all, directed = FALSE)
h <- plot(g)
# Weapons
myvars2 <- c("Country", "Partner", "TIV.delivery.values")
all1 <- fult2[myvars2]
g1 <- graph_from_data_frame(all, directed = FALSE)
h1 <- plot(g1)

combined_plot2 <- h + h1
combined_plot2

# Assoc. Rules
library(gtools)
library(arules)
library(arulesViz)
library(knitr)
asc <- read.csv('arc copy.csv', header = T, colClasses = "factor")
#asc <- transform(asc,trans_id = as.factor(trans_id))
#asc <- transform(asc,Armament.category = as.factor(Armament.category))
trans <- as(asc, "transactions")
dim(trans)
itemLabels(trans)
summary(trans)
image(trans)
itemFrequencyPlot(trans, topN=10,  cex.names=1)
#Min Support, confidence
rules <- apriori(trans, 
                 parameter = list(supp=0.1, conf=0.1, 
                                  maxlen=10, 
                                  target= "rules"))
rules <- apriori(asc)
summary(rules)
inspect(rules)
# Reduce number of rules
rules2 <- apriori(asc, parameter = list(minlen = 2, maxlen = 5, supp = .7))
inspect(rules2)
# Check for most popular 'item'
summary(asc)
itemFrequencyPlot(asc, topN=10,  cex.names=1)
# It's 'Aircraft'
# Focus on rules where LHS and RHS are 'True' matches.
rules4 <- apriori(asc, parameter= list(minlen = 2, maxlen = 8, supp = .05, conf = .05),
                  appearance = list(rhs=c("Aircraft=TRUE"),lhs=c("Air.defence.systems=TRUE",
                                                                 "Armoured.vehicles=TRUE",
                                                                 "Artillery=TRUE",
                                                                 "Engines=TRUE",
                                                                 "Missiles=TRUE",
                                                                 "Naval.weapons=TRUE",
                                                                 "Satellites=TRUE",
                                                                 "Sensors=TRUE",
                                                                 "Ships=TRUE",
                                                                 "Other=TRUE"), default="none"))
inspect(rules4)
summary(rules4)
plot(rules4, method = "grouped")
plot(rules4, method = "graph", control=list(type="items"))

# ARC and TII
devtools::install_github('Mikata-Project/ggthemr')
library(ggthemr)
ggthemr_reset()

# Make new df
W22222 <- fult2 %>% filter(Armament == 'Sensors' |Armament == 'Engines' |Armament == 'Armoured vehicles'|Armament == 'Missiles'|Armament == 'Aircraft')
# Check
mean(W22222$TIV.delivery.values)
median(W22222$TII.Score)
# Plot attempt
ah <- ggplot(data = W22222, mapping = aes(x = Armament, y = TII.Score, color = Country)) +
  geom_point() + 
  #dark_theme_bw() +
  theme(text = element_text(family = "Optima")) +
  scale_y_continuous() +
  labs(x="Arm",
       y = "TII Rank",
       title = "TII vs. ARCs",
       subtitle = "Any Correlation?",
       caption = "Sources: WITS and SIPRI") +
  #coord_flip() +
  geom_hline(yintercept=1.67, linetype="dashed", color = "red") +
  theme(legend.position = "right") 
  
  #facet_wrap(~Trade.Flow)
ggthemr('flat dark')
ah
detach("package:ggthemr", unload=TRUE)

# TII and LHS compare?
ba <- ggplot(W22222, aes(x=TIV.delivery.values, y=TII.Score) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  labs(x = "TIV Score", 
       y = "TII Rank", 
       title = "What About via TIVs?",
       subtitle = "Not Striking...",
       caption = "Sources: SIPRI and WITS") +
  dark_theme_bw() +
  theme(text = element_text(family = "Optima")) +
  theme(legend.position="right") +
  scale_fill_continuous(name="Correlation\nValue") +
  geom_hline(yintercept=1.67, linetype="dashed", color = "red") +
  facet_wrap(~Armament, ncol = 2)
ba

# Try again...
library(ggthemr)
ba2 <- ggplot(W22222, aes(x = Armament, y = TII.Score, fill = Country)) + 
  geom_boxplot(outlier.colour="darkgoldenrod", outlier.shape=6,
               outlier.size=1) +
  labs(x = "Arm Type", 
       y = "TII Rank", 
       title = "TII vs. ARC Components",
       subtitle = "Any Correlation?",
       caption = "Sources: WITS and SIPRI") +
  #theme_minimal() +
  geom_hline(yintercept=1.67, linetype="dashed", color = "red") +
  theme(text = element_text(family = "Optima")) +
  theme(legend.position="right") 
ggthemr('flat dark')
ba2

# Test strip chart:
ggplot(W22222, aes(x=Armament, y=TII.Score, shape = Country, color = Armament)) + 
  geom_jitter(position=position_jitter(0.2)) +
  stat_summary(fun.data="mean_sdl", mult=1, 
                 geom="crossbar", width=0.5)

# Conclusion Plot - Final
ba3 <- ggplot(W22222, aes(x=Armament, y=TII.Score, shape = Country, color = Country)) + 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept=1.67, linetype="dashed", color = "red") +
  geom_jitter(position=position_jitter(0.2)) +
  labs(x = "Arm Type", 
       y = "TII Rank", 
       title = "TII vs. ARC Components",
       subtitle = "Any Correlation?",
       caption = "Sources: WITS and SIPRI") +
  theme(text = element_text(family = "Optima"))
ggthemr('flat dark')
ba3

# Zoom In
ba4 <- ggplot(W22222, aes(x=Armament, y=TII.Score, shape = Country, color = Country)) + 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept=1.67, linetype="dashed", color = "red") +
  geom_jitter(position=position_jitter(0.2)) +
  ylim(0, 10) +
  labs(x = "Arm Type", 
       y = "TII Rank", 
       title = "TII vs. ARC Components",
       subtitle = "Zooming In...",
       caption = "Sources: WITS and SIPRI") +
  theme(text = element_text(family = "Optima"))
ggthemr('flat dark')
ba4

# Just Violins
ba5 <- ggplot(W22222, aes(x=Armament, y=TII.Score, shape = Country, color = Country)) + 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept=1.67, linetype="dashed", color = "red") +
ylim(0, 10) +
  labs(x = "Arm Type", 
       y = "TII Rank", 
       title = "TII vs. ARC Components",
       subtitle = "Just Distros",
       caption = "Sources: WITS and SIPRI") +
  theme(text = element_text(family = "Optima"))
ggthemr('flat dark')
ba5

