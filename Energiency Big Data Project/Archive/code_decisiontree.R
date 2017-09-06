#############  decision  tree

# Make big tree
tree.1 <- rpart(y2 ~ ., as.data.frame(x),control=rpart.control(minsplit=20,cp=0))

fancyRpartPlot(tree.1)


#-------------------------------------------------------------------
tree.2 <- rpart(y2 ~ ., as.data.frame(x), method = "class")			
                               									
fancyRpartPlot(tree.2)			


