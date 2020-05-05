setwd("D:/BA classes/machine learning dataset of R/unsupervised learning/rules association(market basket ananlysis)")
getwd()

# Load packages
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
library(plotly)
library(arules)
library(arulesViz)
library(visNetwork)
library(igraph)
library(kableExtra)

Telco = read.transactions("Telecom.csv", format="single",cols=c(1,2))
itemFrequencyPlot(Telco,topN=20,type="absolute")

# Implementing Apriori Algorithm
rules <- apriori(Telco, parameter = list(support = 0.005, confidence = 0.25))

# Remove redundant rule    
rules <- rules[!is.redundant(rules)]
rules_dt <- data.table( lhs = labels( lhs(rules) ), 
                        rhs = labels( rhs(rules) ), 
                        quality(rules) )[ order(-lift), ]

library("RColorBrewer")
arules::itemFrequencyPlot(Telco,
                          topN=20,
                          col=brewer.pal(8,'Pastel2'),
                          main='Relative Item Frequency Plot',
                          type="relative",
                          ylab="Item Frequency (Relative)") 

plotly_arules(rules)

sel <- plot(rules, measure=c("support", "lift"), 
            shading = "confidence",
            interactive = TRUE)

subrules2 <- head(sort(rules, by="confidence"),20)
ig <- plot( subrules2, method="graph", control=list(type="items") )
ig_df <- get.data.frame( ig, what = "both" )
nodesv %>%
  visNodes(size = 10) %>%
  visLegend() %>%
  visEdges(smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visEdges(arrows = 'from') %>%
  visPhysics(
    solver = "barnesHut",
    maxVelocity = 35,
    forceAtlas2Based = list(gravitationalConstant = -6000)
  )