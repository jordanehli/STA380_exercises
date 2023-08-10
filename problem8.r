# Problem 8: Association rule mining

library(tidyverse)
library(igraph)
library(arules) 
library(arulesViz)
library(data.table)
library(rlist)

# Read in groceries, setting the head as flase
groceries_raw = read.csv("./data/groceries.txt", head=FALSE)

# View our information of groceries transactions
str(groceries_raw)
summary(groceries_raw)
View(groceries_raw)

# Convert purchases into  pair of products
# The idea is taking all purchases and converting it with pairs of all
# the other items that were bought in the same purchase
# from ["Prod1","Prod2","Prod3"]
# to a combination of
# (Prod1, Prod2), (Prod1, Prod3), (Prod2, Prod3)
#
#

df1 = groceries_raw
comb = data.frame(matrix(ncol = 2, nrow = 0))
for(i in 1:nrow(df1)){
  items_comb = combn(df1[i,], 2, simplify = FALSE)
  
  for (j in 1:length(items_comb)){
    if (items_comb[[j]][[1]] != "" & items_comb[[j]][[2]] != ""){
      print('Adding pair')
      comb[nrow(comb)+1,] = c(items_comb[[j]][[1]], items_comb[[j]][[2]])
    }
  }
}
comb

groceries_c = split(x=comb$X2, f=comb$X1)
typeof(groceries_c)

groceries_c = lapply(groceries_c, unique)


gc_trans = as(groceries_c, "transactions")
summary(gc_trans)
gc_trans

gcrules = apriori(gc_trans, parameter=list(support=.03, confidence=.1, maxlen=2))
inspect(gcrules)
plot(gcrules)
# tried with all rules, but it seemed that many of the single products had a high
# single rule so decided to look to all rules that were between 2 nodes at least

gcrules_2 = apriori(gc_trans, parameter=list(support=.03, confidence=.1, maxlen=2, minlen=2))
inspect(gcrules_2)
plot(gcrules_2)


# Save the network so we can view it using Gephi
grocieries_graph = associations2igraph(subset(gcrules_2, lift>4), associationsAsNodes = FALSE)
igraph::write_graph(grocieries_graph, file='groceries.graphml', format = "graphml")


# We are adding the picture of graphs with support 0.005, 0.03, 0.05, and 0.06
# Photos are found under the graph_photo folder
# - For 0.005 (g_005.png): We see that there is a big network on food items.
# But on the outer edges we can see other kind of products like cleaning.
#
# - We wanted to look at 0.03 (g_03_1.png, g_03_2.png) to have more insight
# of this product relationships we see two networks. One with food products,
# where we can see the most common items that were bought and the outliers.
# These represent a good opportunity for placing this not so common items
# next to the ones that were regularly bought. On the other network we observe
# a unusual relationship between alcohol products, and chocolate.
#
# - For networks with 0.05 and 0.06 (g_05.png, g_06.png), we can see
# more of the specific relationships between the top products. This
# could be consider our baseline products.
