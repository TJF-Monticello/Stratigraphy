# Filename:    HarrisMatrix_URCode.R
# Purpose:     Sections 1-5 pull Stratigraphic Information from the Context tables and 
# gets them into a form that works for ArchEd. There are several data integrity
# checks. There is an option to replace individual
# contexts with their SG.
# Section 6 uses Rgraphviz package to visualize relationships in a different way.  Serves as 
# a check for ArchEd, which does not tell you where errors are only that they exist.
#   																							
#By:          FDN 11.26.2013
#Last Update: FDN 01.14.2018
#Section 6 by DTW 12.09.2019 Updated Rgraphviz package

#############################################################################
#############################################################################
############# do this once and then comment out or delete  ####  ############
############# Installs Rgraphviz package - not available   ###  #############
############# on CRAN                                      ##  ##############
 if (!requireNamespace("BiocManager", quietly = TRUE))   #               ##
   install.packages("BiocManager")                     ##  ##############
 BiocManager::install(c("Rgraphviz"))                    ###  #############
##                                                         ####  ############
#############################################################################
#############################################################################

# libraries needed
library(DBI)
library(RPostgreSQL)
library("Rgraphviz")
library(glue)
library(tidyr)
library(dplyr)


# Establish a DBI connection to DAACS PostgreSQL database and submit SQL queries
#Link to file with database password
source("credentials.R")


# get the contexts, SGs, features and their related contexts 
# use the "WHERE" clause to choose the Project and contexts
# FYI: correlation names do NOT need to be double quoted
csr <- dbGetQuery(DRCcon,' 
SELECT  
  		a."ProjectID",
			a."Context" as "context",
      a."DAACSStratigraphicGroup" as "SG",
			a."FeatureNumber",
			c."StratRelType" as "relationship",
			b."Context" as "relatedContext"
FROM  "tblContext"                    a 
      join "tblContextStratRel"	      b         
				on a."ContextAutoID"=b."ContextAutoID"
			join "tblContextStratRelType" 	c  				
				on b."StratRelTypeID"=c."StratRelTypeID"
WHERE ( a."ProjectID" = \'100\'
                  
)
	ORDER BY a."Context"
')


## 1.Recode the relationships to Arch-Ed .lst format ########
# Note that the recoding takes into account the differences between conventions used 
# in the DAACS backend vs. Arch-ED .lst files. In DAACS When A has a relationship of "Seals" to B, 
# it implies that A is "above" B. In the Arch-ED .lst file format this is represented as
# "A Below:B", which causes Arch-ED to draw B below A. So to get Arch-Ed to draw DAACS
# relationships correctly, we have to "reverse" the DAACS relationship 

csr$archEdRel[csr$relationship=="Sealed By"]<-"above"
csr$archEdRel[csr$relationship=="Seals"]<-"below"
csr$archEdRel[csr$relationship=="Intruded By"]<-"above"
csr$archEdRel[csr$relationship=="Intrudes"]<-"below"
csr$archEdRel[csr$relationship=="Contained By"]<-"above"
csr$archEdRel[csr$relationship=="Within"]<-"below"
csr$archEdRel[csr$relationship=="Correlates With"]<-"equal to"
csr$archEdRel[csr$relationship=="Contains"]<-"above"
csr$archEdRel[csr$relationship=="Contemporary W"]<-"equal to"


## 1.1  Do a bit more recoding so we do not get tripped up later by mixtures of NAs and '' in the
# SG and FeatureNumber fields
# When a Context lacks an SG or Feature Number, sometimes those fields are NULL, sometimes they have 
# blanks. In the former case, an NA is returned in R. So we make sure all the NAs are set to blanks
csr$SG[is.na(csr$SG)]    <-''
sum(is.na(csr$SG))
csr$FeatureNumber[is.na(csr$FeatureNumber)]    <-''
sum(is.na(csr$FeatureNumber))

# 1.2 Write out an Excel file with the DAACS data. Useful for trouble shooting
# The file is written to the current working directory.
write.csv (csr, file="contextStratigraphicRelationhips.csv")


## 2.This section does some data consistency checks. #####
## 2.1 Check for context values that appear in the context field but not the related context field and v.v.#####
# These need to fixed in the database BEFORE going further.
uniqueCxt<-unique(csr$context)
uniqueRelCxt<-unique(csr$relatedContext)
cxtOrphans<-uniqueCxt[!(uniqueCxt %in% uniqueRelCxt)]
paste("the following contexts do not appear as relatedContexts:",cxtOrphans)
relCxtOrphans<-uniqueRelCxt[!(uniqueRelCxt %in% uniqueCxt)]
paste("the following relatedContexts do not appear as contexts:",relCxtOrphans)

# 2.2  The cxtNoSG file may contain contexts that you REALLY do not want in the analysis,
# for example, if you are only analyzing a subset of contexts for a given project.
# To get rid fo them, run the following line 
csr <-subset(csr, !(csr$relatedContext   %in% relCxtOrphans))

# 2.3 Find "equal to" context pairs that have no SG assignments.
# If there are any, fix them BEFORE going further. ALL "correlated with" contexts
# need to belong to the same SG. 
cxtNoSG<-csr$context[(csr$archEdRel=="equal to") & (csr$SG == "")]
paste("the following Contexts have 'equal to' relationships but have no SG assigned:"
      ,cxtNoSG)

## 2.4 Find any "equal to" pairs of contexts and related contexts that have DIFFERENT SG assignments.######
# If there are any, these need to be fixed (e.g. by making the SGs the same) BEFORE going further.
# First we have to assign SG's to related contexts...
#   get a list of unique contexts and their SGs and put them in a new dataframe 
relatedCxtSG<- unique(data.frame(csr$context,csr$SG,stringsAsFactors=F))
#   rename the SG variable and the Context variable in the new dataframe   
names(relatedCxtSG)[names(relatedCxtSG)=="csr.SG"] <- "relatedSG"
names(relatedCxtSG)[names(relatedCxtSG)=="csr.context"] <- "relatedContext"
# merge the new related context and related SG data frame with the orignal context and SG dataframe 
# we match merge records on the common RelatedContext field and keep everything in the orginal context table 
csr1<-merge(csr,relatedCxtSG, by="relatedContext",all.x=T)  
#   sort the result on SG, relatedSG, archEdRel 
sortedCsr1 <- csr1[order(csr1$SG, csr1$relatedSG, csr1$archEdRel),] 
#   reorder the cols for convenience
sortedCsr1 <- sortedCsr1[c(2,3,4,5,6,7,1,8)]  
# Now we look for contexts and related contexts that are "equal to" each other by have different SGs 
diffSGVec<-(sortedCsr1$archEdRel=="equal to") & (sortedCsr1$SG != sortedCsr1$relatedSG)
differentSGs <- sortedCsr1[diffSGVec,]
paste("the following Contexts have 'equal to' relationships but have Different SG assignments:")
differentSGs


## 2.5 Context/RelatedContext and SG/RelatedSG stratigraphic consistency check ########
# Check to make sure the above and below relationships among contexts that 
# belong to different SGs are consistent: contexts that belong to the
# a given SG should all have the same relationships to related contexts that all 
# belong to a second SG. This code chunk finds non-matching relationships. The steps are:
#   - Loop through the sorted data to find the cases where relationships do not match.   
#   - Note that we exclude contexts that have been assigned to the same SG
#     on the assumption that SG assignment is correct. We checked that in the previous step. 
badRelationTF<- rep(F,nrow(sortedCsr1))
for (i in 1:(nrow(sortedCsr1)-1)) {
  # only worry about this if BOTH context and related cxt have SG assignments
  # orginal code had a bug here:' ', not  ''
  if ((sortedCsr1$SG[i] != '') & (sortedCsr1$relatedSG[i] !='')) {
    #  are the SGs at row (i) and row (i+1) the same?
      badRelationTF[i] <- (sortedCsr1$SG[i] ==  sortedCsr1$SG[i+1]) & 
      # are the related SGs the same?
        (sortedCsr1$relatedSG[i] == sortedCsr1$relatedSG[i+1]) &
      # are the archEd relations different?
       (sortedCsr1$archEdRel[i] != sortedCsr1$archEdRel[i+1]) &
        # this is the bit that excludes contexts assigned to the same SG      
       (sortedCsr1$SG[i] !=  sortedCsr1$relatedSG[i])
  }
}
badRelationTF[(which(badRelationTF == T)+1)]<-T
paste(table(badRelationTF)[2], 
"There are contradictory relationhips among contexts belonging to different SGs. Check the exported file 'badRelations.csv' for details")      
badRelation <- sortedCsr1[badRelationTF,]
badRelation

write.csv(badRelation, file="badRelation.csv")


## 3. This section preps the data in the format required by ArchEd  ########
## 3.1  Set up a Dataframe to store the results. Its rows are all possible combinations of Contexts and relatedContexts 
allCxt <- unique( c(uniqueCxt,uniqueRelCxt))
HMData <- expand.grid(allCxt,allCxt,stringsAsFactors=F)
colnames(HMData)<-c("cxt","relCxt")
HMData$archEdRel<-"NA"


## 3.2 Assign the reciprocal relationships (e.g. A>B, B<A) 
for (i  in 1: nrow(csr)) {
    # identify the context and its related context in the data from DAACS
    thisCxt<-csr$context[i]
    thisRelCxt<-csr$relatedContext[i]
    # find the two locations in HMData
    loc1 <- which(HMData$cxt==thisCxt &  HMData$relCxt== thisRelCxt)
    loc2 <- which(HMData$cxt==thisRelCxt & HMData$relCxt== thisCxt)
     
    #assign the relationships
    HMData$archEdRel[loc1]<-csr$archEdRel[i]
    if (csr$archEdRel[i]=="above") {HMData$archEdRel[loc2]<-"below"}
    if (csr$archEdRel[i]=="below") {HMData$archEdRel[loc2]<-"above"}
    if (csr$archEdRel[i]=="equal to") {HMData$archEdRel[loc2]<-"equal to"}
    }    


# check on the results
table(HMData$archEdRel)


## 3.3 If you want to set the Contexts that belong to the same SG as "equal to" run this bit
allSG<- unique(csr$SG)
allSG<- allSG[!allSG==""]
allCxtSG <- unique(data.frame(csr$context, csr$SG, stringsAsFactors=F))
for (i  in 1: length(allSG)){
    thisSG<-allSG[i]
    cxtForThisSG <- allCxtSG$csr.context[which(allCxtSG$csr.SG==thisSG)]
    equalToIndex <- (HMData$cxt %in% cxtForThisSG) & (HMData$relCxt %in% cxtForThisSG)
    HMData$archEdRel[equalToIndex]<-"equal to"                 
}

# check on the results
table(HMData$archEdRel)


## 3.4 get rid of context pairs in the HM data file without relationships  #########
HMData<-HMData[!(HMData$archEdRel=="NA"),]
# get rid of context pairs that are the same -- keeping them cause Arch Ed to blow up.
HMData<-HMData[!(HMData$cxt==HMData$relCxt),]


# merge the SGs into the HM data file by Cxt
HMData<-merge(HMData,allCxtSG, by.x="cxt",by.y="csr.context",all.x=T)  
# sort the HM data on the SG, contexts, and relationship. Needed to write the output. 
sortedHMData<-with(HMData,
                     HMData[order(csr.SG,cxt,archEdRel,relCxt),])  



## 3.5 Run this next block _IF_ you want to to replace the contexts with their SGs #########
HMData1<-merge(HMData,relatedCxtSG, by.x="relCxt", by.y="relatedContext", all.x=T)  
 for (i in 1:nrow(HMData1)){
   if (HMData1$csr.SG[i] != "") {
     HMData1$cxt[i]<-HMData1$csr.SG[i]
   }
   if (HMData1$relatedSG[i] != "") {
     HMData1$relCxt[i]<-HMData1$relatedSG[i]
   }
 }


HMData1 <- subset(HMData1, select=c(cxt,relCxt,archEdRel))
# keep only records where ctx and relCtx are not the same value
## DTW 3/22/2018 - this also gets rid of DAACS data entry errors where a Context that is not part of an SG is above/below itself
HMData1 <- HMData1[HMData1$cxt != HMData1$relCxt,]
# get rid of redundant records
HMData1<-unique(HMData1)
sortedHMData<-HMData1[order(HMData1$cxt, HMData1$relCxt, HMData1$archEdRel),] 



# 5. This section defines functions and then uses them to write the data out in ArchEd .lst format ######
# define functions that will help with writing the output file

first.case<-function(myVec){
# locates the first occurrences of each value in a sorted vector
#
# Args:
#   myVec: the sorted vector
# Returns: a logical vector with T at the first occurrences
  result<-rep(F,length(myVec))
  for (i in (1:length(myVec))){
      if (i==1) {result[1]<-T}
      else {result[i]<- (myVec[i] != myVec[i-1])}
  }
  return(result)
}

last.case<-function(myVec){
# locates the last occurences of each value in a sorted vector
#
# Args:
#   myVec: the sorted vector
# Returns: a logical vector with T at the last occurrences
  result<-rep(F,length(myVec))
  for (i in (1:length(myVec))){
    if (i==length(myVec)) {result[length(myVec)]<-T}
    else {result[i]<- (myVec[i] != myVec[i+1])}
  }
  return(result)
}



firstSG<-first.case(sortedHMData$csr.SG)
firstCxt<-first.case(sortedHMData$cxt)
firstArchEdRel<-first.case(sortedHMData$archEdRel)
lastArchEdRel<-last.case(sortedHMData$archEdRel)
lastCxt<-last.case(sortedHMData$cxt)

firstSG<-first.case(sortedHMData$csr.SG)
firstCxt<-first.case(sortedHMData$cxt)
firstArchEdRel<-first.case(sortedHMData$archEdRel)
lastArchEdRel<-last.case(sortedHMData$archEdRel)
lastCxt<-last.case(sortedHMData$cxt)

##  write the output file to the current working directory

file.create("output.lst")
for (i in 1:nrow(sortedHMData)) {
  if (firstCxt[i] == T) {
    cat(paste(sortedHMData$cxt[i]),"\n", file="output.lst",append=TRUE)
  }
  if ((firstCxt[i] == T) | (firstArchEdRel[i]==T))  { 
    cat (paste("            ", sortedHMData$archEdRel[i]), ": ", sep="", 
         file="output.lst", append=TRUE)
  }
  cat(paste(sortedHMData$relCxt[i]), sep="", file="output.lst", append=TRUE)
  if  ((lastArchEdRel[i] == F)  & (lastCxt[i]==F)) {
    cat(", ", sep="", file="output.lst", append=TRUE)
  }
  if ((lastArchEdRel[i] == T) | (lastCxt[i]==T)) {
    cat("\n", file="output.lst",append=TRUE)
  }              
}    


if  (lastCxt[i]==F) {
  cat(" ,", file="output.lst",append=TRUE)
}



##### 6 - Plotting HM data via Graphviz and RGraphviz - by DTW 03.16.2018 #####
## assign global node shape to ellipse and allow the ellipse to resize itself to surround the node's text
attrs <- list(node=list(shape="ellipse", fixedsize=FALSE, graphssize = "34, 22"))

## Secton 6.1 - Saving HM data in Graphviz format

## Standalone Graphviz program requires double quotes around each context. Put double quote into a variable:
doublequote <- '"'

## Add a new column to csr1 - it will be the 'Above' Context
## for loop will put the relatedContext field into the 'Above' column when the relationship is 'above'
## it will also put the context field into the 'Above' column when the relationship is 'below'
for (i in 1:nrow(csr1)){ 
  if(csr1$archEdRel[i]=='above')  {
    csr1$GraphVizAbove[i] <- paste(doublequote, csr1$relatedContext[i], doublequote, sep="")
    
  } else if(csr1$archEdRel[i]=='below'){
    csr1$GraphVizAbove[i] <- paste(doublequote, csr1$context[i], doublequote, sep="")
  } else {csr1$GraphVizAbove[i] <- NA} #Graphviz can't handle 'equals to' realtionships so put in an empty string
}
## Graphviz requires a "->" to denote one context is above another.  Add this to a new column
csr1$GraphVizArrow <- '->'

## Add a new column to csr1 - it will be the 'Below' context
## This section is similar to the For statement above
## it will put the context field into the 'Below' column when the relationship is 'above'
## it will also put the relatedContext into the 'Below' column when the relationship is 'below'
for (i in 1:nrow(csr1)){ 
  if(csr1$archEdRel[i]=='above')  {
    csr1$GraphVizBelow[i] <- paste(doublequote, csr1$context[i], doublequote, sep="")
  } else if(csr1$archEdRel[i]=='below'){
    csr1$GraphVizBelow[i] <- paste(doublequote, csr1$relatedContext[i], doublequote, sep="")
  } else {csr1$GraphVizBelow[i] <- NA}
}

## Create new data frame that has just 3 columns - GraphvizAbove, GraphvizArrow and GraphvizBelow
GraphVizTable <- data.frame(csr1$GraphVizAbove, csr1$GraphVizArrow, csr1$GraphVizBelow)

## Remove rows that don't have values in GraphvizAbove or GraphvizBelow (rows where archEdRel = 'equals to')
GraphVizTable <- na.omit(GraphVizTable)

## write out the GraphViz dot file.  This is the file that you will open in Graphviz standalone program
## comment out if you don't have Graphviz isntalled on your computer
## probably is worth it as the graphs look MUCH better (as of 2/28/2018)!

## Create the Graphviz formatted text vector
GraphvizOutput <- "digraph G {\n node [shape=ellipse, fixedsize=FALSE]; \n"
for (i in 1:nrow(GraphVizTable)){
  GraphvizOutput <- paste(GraphvizOutput, GraphVizTable$csr1.GraphVizAbove[i], GraphVizTable$csr1.GraphVizArrow[i], GraphVizTable$csr1.GraphVizBelow[i], "\n", sep = " ")
}
GraphvizOutput <- paste(GraphvizOutput, "}")

## Write the Graphviz file
fileConn<-file("Graphviz_Ctx.gv")
writeLines(GraphvizOutput, fileConn)
close(fileConn)


## Section 6.2 - Plot data using Rgraphviz

## 6.2 may be commented out as this section produces a graph without arrows.
## However, it does print every relationship - i.e., if both A is above B and B is below A are listed
## then two lines will be drawn between A and B.  The graph drawn in 6.3 only draws one line, but has arrows.
## It appears that the 'agread' command creates a graph object whereas a graphNEL object is needed to 
## print a directed graph (contains arrows showing the direction of the relationship)

## read the text file back into R and print out using Rgraphviz
graphNew <- agread("Graphviz_Ctx.gv", layoutType = "dot", layout = TRUE)
plot(graphNew)

## Save the output as a pdf file
pdf("HM_Ctx_output0.pdf", width = 34, height = 22)
plot(graphNew)
dev.off()

## Section 6.3 - Plot data using Rgraphviz - This section prints out data as a graphNEL object
## This gives more options on print layout

## This method does not allow for duplicate relationships or 'edges'
## i.e., A above B can not be listed twice

## Remove duplicate relationships
unique_GraphvizTable <- unique(GraphVizTable)

## Put the Above contexts into a vector
edges_Above <- unique_GraphvizTable$csr1.GraphVizAbove

## The above command thinks the text is a variable type of 'factor' (due to pulling it from a data frame)
## Convert the vector so that the values are 'text' variables
edges_Above <- as.character(edges_Above)
## Remove the trailing double quote
edges_Above <- substr(edges_Above, 1, nchar(edges_Above)-1)
## Remove the leading double quote
edges_Above <- substr(edges_Above, 2, nchar(edges_Above))

## Put the Below contexts into a vector
edges_Below <- unique_GraphvizTable$csr1.GraphVizBelow

## The above command thinks the text is a variable type of 'factor' (due to pulling it from a data frame)
## Convert the vector so that the values are 'text' variables
edges_Below <- as.character(edges_Below)
## Remove the trailing double quote
edges_Below <- substr(edges_Below, 1, nchar(edges_Below)-1)
## Remove the leading double quote
edges_Below <- substr(edges_Below, 2, nchar(edges_Below))

## Combine the 'Above' and 'Below' vectors into a matrix
edges_GraphvizTable <- cbind(edges_Above, edges_Below)

## Convert the matrix into a graphNEL object
## graphNEL objects can be directed graphs (i.e., graph with arrows showing direction of relationship)
mygraphNEL <- ftM2graphNEL(edges_GraphvizTable, edgemode="directed")

## plot the graphNEL object in the plot window
## the recipEdges="distinct" command will draw an arrow for each relationship in the graph
## i.e., if there's a cycle (A above B and B above A), the default is to draw a single line with an arrow head
## at each end.  This command will force the graph to draw two arrows, one point in each direction
## this is a valid command, but does not produce two arrows...
plot(mygraphNEL, recipEdges="distinct")

## Save the output as a pdf file
pdf("HM_Ctx_output1.pdf", width = 34, height = 22) #height and width is paper size
plot(mygraphNEL, recipEdges="distinct")
dev.off()


## section 6.3
## This section replicates the previous pdf/plot created in section 6.2 and applies colors and shapes 
## to each node (Context) in an SG.  Each Context in a SG gets a specific color/shape combination
testGraph <- layoutGraph(mygraphNEL)

## nodes (contexts) can have four distinct shapes around the context name (box, circle, ellipse and triangle)
## the "plaintext" option removes all shapes (default is the circle shape) around the contexts
## we will apply shapes to contexts that are part of an SG.  All contexts not in an SG will not have a shape
## start by applying the plaintext shape (no shape) as the default
nodeRenderInfo(testGraph) <- list(shape="plaintext")

##Section 6.3a
## this section reuses Fraser's code for looping through the contexts in each SG (Section 3.3)
## the next three lines were already run in section 3.3 so variables still exist
#allSG<- unique(csr$SG)
#allSG<- allSG[!allSG==""]
#allCxtSG <- unique(data.frame(csr$context, csr$SG, stringsAsFactors=F))

## PUt the four different Rgraphviz node types into a character vector
graph_Node_Shapes <- c("box", "ellipse", "triangle", "circle")
## Repeat the 4 node shapes as many times as the number of SG's in allSG
graph_Node_Shapes <- rep(graph_Node_Shapes, (length(allSG)+4)/4)
## Put 17 different collors that Rgraphviz recognizes into a character vector
graph_Node_Colors <- c("red", "blue", "green", "orange", "pink", "cyan", "purple", "transparent", "brown", "lightyellow", "gray", "lightblue", "gold", "darkgreen", "magenta", "yellow", "lightgreen")
## Repeat the 17 node colors as many times as the number of SG's in allSG
graph_Node_Colors <- rep(graph_Node_Colors, (length(allSG)+17)/17)

## Loop through all the SG's in allSg
## Select the Contexts for the current SG
## Assign a node shape and color for the contexts in the current SG
for (i  in 1:length(allSG)){
  thisSG <- allSG[i]
  
  ## Put all the Contexts in the current SG into the vector cxtForThisSG
  cxtForThisSG <- allCxtSG$csr.context[which(allCxtSG$csr.SG==thisSG)]
  ## Put contents of cxtForThisSG into a new vector called nodefillcolor
  nodefillcolor <- cxtForThisSG
  ## Construct a matrix containing the Context and the node fill color for the ith SG
  nodefillcolor <- rep(graph_Node_Colors[i], length(cxtForThisSG))
  names(nodefillcolor) <- cxtForThisSG
  ## Change the node fill color for contexts in ith SG
  nodeRenderInfo(testGraph) <- list(fill=nodefillcolor)
  
  ## Put contents of cxtForThisSG into a new vector called nodeshape
  nodeshape <- cxtForThisSG
  ## Construct a matrix containing the Context and the node shape for the ith SG
  nodeshape <- rep(graph_Node_Shapes[i], length(cxtForThisSG))
  names(nodeshape) <- cxtForThisSG
  ## Change the node shape for contexts in the ith SG
  nodeRenderInfo(testGraph) <- list(shape=nodeshape)
}

#graph.par(list(edges=list(col="lightblue", lty="solid", lwd=1)))
#renderGraph(testGraph)
#graph.par(list(nodes=list(col="darkgreen", lty="dotted", lwd=2, fontsize=10)))
#renderGraph(testGraph)

#layoutGraph(testGraph)
#graph.par(list(nodes=list(fixedsize=FALSE, col="darkgreen", lty="solid", lwd=1, fontsize=30)))
#nodeRenderInfo(testGraph) <- list(shape=(fixedsize=FALSE))

#nodeRenderInfo(testGraph) <- list(shape=c( "2581A" = "rectangle"))
#nodeRenderInfo(testGraph) <- list(skew=c( "2581A" = -100))
#graph.par(list(nodes=list(fill="yellow", textCol="blue", fontsize = 20, font = "Times New Roman")))
#graph.par(list(edges=list(col="lightblue", lty="solid", lwd=1)))
#nodeRenderInfo(testGraph) <- list(fill=c("2581A"="", "2582A"="darkred", "2583A" = "red"))
#testGraph <- layoutGraph(testGraph)
#nodeRenderInfo(testGraph) <- list(arrowhead=c("dot"))
#layoutGraph(testGraph)

renderGraph(testGraph, recipEdges="distinct")
## Save the output as a pdf file
pdf("HM_Ctx_output2.pdf", width = 48, height = 36) #height and width is paper size
renderGraph(testGraph, recipEdges="distinct")
dev.off()


## 6.4 Plot SGs and Contexts HM graph
## This section uses HMData1 as the source of Ctx and SG relationships

## put HMData1 into a new table 
GraphViz_HM_Feat_SG_Ctx <- HMData1

## tidy R way to remove rows where archEdRel = 'equal to'
#GraphViz_HM_Feat_SG_Ctx <- GraphViz_HM_Feat_SG_Ctx[(GraphViz_HM_Feat_SG_Ctx$archEdRel!="equal to"),]
## dplyr  way to remove rows where archEdRel = 'equal to'
GraphViz_HM_Feat_SG_Ctx <- dplyr::filter(GraphViz_HM_Feat_SG_Ctx, archEdRel != "equal to")

## tidy R and dplyr - add new column "GraphvizAbove" and put in value of "relCtx" column if "archEdRel" = 'above'. If not true, put in value of "ctx"
GraphViz_HM_Feat_SG_Ctx <- dplyr::mutate(GraphViz_HM_Feat_SG_Ctx, GraphvizAbove = ifelse(GraphViz_HM_Feat_SG_Ctx$archEdRel == 'above', GraphViz_HM_Feat_SG_Ctx$relCxt, GraphViz_HM_Feat_SG_Ctx$cxt))
## tidy R and dplyr - add new column "GraphvizBelow" and put in value of "Ctx" column if "archEdRel" = 'above'. If not true, put in value of "relCtx"
GraphViz_HM_Feat_SG_Ctx <- dplyr::mutate(GraphViz_HM_Feat_SG_Ctx, GraphvizBelow = ifelse(GraphViz_HM_Feat_SG_Ctx$archEdRel == 'above', GraphViz_HM_Feat_SG_Ctx$cxt, GraphViz_HM_Feat_SG_Ctx$relCxt))

## Create new data frame that has just 2 columns - GraphvizAbove and GraphvizBelow
GraphVizTable_HM_Feat_SG_Ctx <- data.frame(GraphViz_HM_Feat_SG_Ctx$GraphvizAbove,  GraphViz_HM_Feat_SG_Ctx$GraphvizBelow)

## Remove duplicate relationships
unique_GraphVizTable_HM_Feat_SG_Ctx <- unique(GraphVizTable_HM_Feat_SG_Ctx)

## Put the Above contexts into a vector
edges_Above_HM_Feat_SG_Ctx <- unique_GraphVizTable_HM_Feat_SG_Ctx$GraphViz_HM_Feat_SG_Ctx.GraphvizAbove

## The above command thinks the text is a variable type of 'factor' (due to pulling it from a data frame)
## Convert the vector so that the values are 'text' variables
edges_Above_HM_Feat_SG_Ctx <- as.character(edges_Above_HM_Feat_SG_Ctx)

## Put the Below contexts into a vector
edges_Below_HM_Feat_SG_Ctx <- unique_GraphVizTable_HM_Feat_SG_Ctx$GraphViz_HM_Feat_SG_Ctx.GraphvizBelow

## The above command thinks the text is a variable type of 'factor' (due to pulling it from a data frame)
## Convert the vector so that the values are 'text' variables
edges_Below_HM_Feat_SG_Ctx <- as.character(edges_Below_HM_Feat_SG_Ctx)


## Combine the 'Above' and 'Below' vectors into a matrix
edges_HM_Feat_SG_Ctx_GraphvizTable <- cbind(edges_Above_HM_Feat_SG_Ctx, edges_Below_HM_Feat_SG_Ctx)

## Convert the matrix into a graphNEL object
## graphNEL objects can be directed graphs (i.e., graph with arrows showing direction of relationship)
mygraphNEL_HM_Feat_SG_Ctx <- ftM2graphNEL(edges_HM_Feat_SG_Ctx_GraphvizTable, edgemode="directed")

## plot the graphNEL object in the plot window
## the recipEdges="distinct" command will draw an arrow for each relationship in the graph
## i.e., if there's a cycle (A above B and B above A), the default is to draw a single line with an arrow head
## at each end.  This command will force the graph to draw two arrows, one point in each direction
plot(mygraphNEL_HM_Feat_SG_Ctx, recipEdges="distinct", attrs=attrs)

## Save the output as a pdf file
pdf("HM_output1_HM_Feat_SG_Ctx.pdf", width = 34, height = 22) #height and width is paper size
plot(mygraphNEL_HM_Feat_SG_Ctx, recipEdges="distinct", attrs=attrs)
dev.off()



##### Section 6.5 #####
## Produce a HM directed graph usings SGs.  However, also append to the SG label all the contexts that make up the SG.

## Add two new columns to csr1 
csr1$SG_CTX_Above <- NA
csr1$SG_CTX_Below <- NA

## loop through csr1 and put contexts into SG_CTX_Above when ArchEdRel is 'above' or 'below'
for (i in 1:nrow(csr1)){ 
  if(csr1$archEdRel[i]=='above')  {
    csr1$SG_CTX_Above[i] <- csr1$relatedContext[i]
    
  } else if(csr1$archEdRel[i]=='below'){
    csr1$SG_CTX_Above[i] <- csr1$context[i]
  } 
}

for (i in 1:nrow(csr1)){ 
  if(csr1$archEdRel[i]=='above')  {
    csr1$SG_CTX_Below[i] <- csr1$context[i]
  } else if(csr1$archEdRel[i]=='below'){
    csr1$SG_CTX_Below[i] <- csr1$relatedContext[i]
  }
}


## need the glue library for the collapse command inside the for loop

## loop through allSG vector (contains all the SGs in csr1) and create a new string containing
## the SG name followed by all the Contexts that make up the SG (separtated by spaces).
## The internal For loops through csr1 and replaces the contexts within the SG_CTX_Above and SG_CTX_Below fields
## with the concatenated SG and Context names.
for (i  in 1:length(allSG)){
  ## at beginning of each loop set SG_CTX_Combo = to an empty string ("")
  SG_CTX_Combo <- ""
  
  thisSG<-allSG[i]
  cxtForThisSG_Combo <- allCxtSG$csr.context[which(allCxtSG$csr.SG==thisSG)]
  ## paste together thisSG and CxtForThisSG_Combo into a single variable SG_CTX_Combo (vector of length of 1)
  SG_CTX_Combo <- paste(thisSG, ": ", glue_collapse(cxtForThisSG_Combo, sep = " ", width = Inf, last = ""), sep = "")
  
  ## put into column SG_CTX_Above the value of SG_CTX_Combo (SG followed by list of Contexts)  
  for (j in 1:nrow(csr1)) {
    if(csr1$SG[j]==thisSG)  {
      if(csr1$archEdRel[j]=='above')  {
        csr1$SG_CTX_Below[j] <- SG_CTX_Combo
      } else if(csr1$archEdRel[j]=='below'){
        csr1$SG_CTX_Above[j] <- SG_CTX_Combo
      }
    }
  }
  ## put into column SG_CTX_Below the value of SG_CTX_Combo (SG followed by list of Contexts)
  for (j in 1:nrow(csr1)) {
    if(csr1$relatedSG[j]==thisSG)  {
      if(csr1$archEdRel[j]=='above')  {
        csr1$SG_CTX_Above[j] <- SG_CTX_Combo
      } else if(csr1$archEdRel[j]=='below'){
        csr1$SG_CTX_Below[j] <- SG_CTX_Combo
      }
    }
  }
}



## Create new data frame that has just 2 columns - SG_CTX_Above, SG_CTX_Below
GraphVizTable_SG_Ctx <- data.frame(csr1$SG_CTX_Above, csr1$SG_CTX_Below)

## Remove rows that don't have values in GraphvizAbove or GraphvizBelow (rows where archEdRel = 'equals to')
GraphVizTable_SG_Ctx <- na.omit(GraphVizTable_SG_Ctx)

## write out the GraphViz dot file.  This is the file that you will open in Graphviz standalone program
## comment out if you don't have Graphviz installed on your computer
## probably is worth it as the graphs look MUCH better (as of 2/28/2018)!

## Create the Graphviz formatted text vector
GraphvizOutput_SG_Ctx <- "digraph G {\n"
for (i in 1:nrow(GraphVizTable_SG_Ctx)){
  GraphvizOutput_SG_Ctx <- paste(GraphvizOutput_SG_Ctx, doublequote, GraphVizTable_SG_Ctx$csr1.SG_CTX_Above[i], doublequote, "->", doublequote, GraphVizTable_SG_Ctx$csr1.SG_CTX_Below[i], doublequote, "\n", sep = " ")
}
GraphvizOutput_SG_Ctx <- paste(GraphvizOutput_SG_Ctx, "}")

## Write the Graphviz file
fileConn<-file("Graphviz_SG_Ctx.gv")
writeLines(GraphvizOutput_SG_Ctx, fileConn)
close(fileConn)

## Remove duplicate relationships
unique_GraphvizTable_SG_Ctx <- unique(GraphVizTable_SG_Ctx)


## Put the Above contexts into a vector
edges_Above_SG_Ctx <- unique_GraphvizTable_SG_Ctx$csr1.SG_CTX_Above

## The above command thinks the text is a variable type of 'factor' (due to pulling it from a data frame)
## Convert the vector so that the values are 'text' variables
edges_Above_SG_Ctx <- as.character(edges_Above_SG_Ctx)

## Put the Below contexts into a vector
edges_Below_SG_Ctx <- unique_GraphvizTable_SG_Ctx$csr1.SG_CTX_Below

## The above command thinks the text is a variable type of 'factor' (due to pulling it from a data frame)
## Convert the vector so that the values are 'text' variables
edges_Below_SG_Ctx <- as.character(edges_Below_SG_Ctx)
#write.table(edges_Below_SG_Ctx, "mydata.txt", sep="\t")


## Combine the 'Above' and 'Below' vectors into a matrix
edges_GraphvizTable_SG_Ctx <- cbind(edges_Above_SG_Ctx, edges_Below_SG_Ctx)

## Convert the matrix into a graphNEL object
## graphNEL objects can be directed graphs (i.e., graph with arrows showing direction of relationship)
mygraphNEL_SG_Ctx <- ftM2graphNEL(edges_GraphvizTable_SG_Ctx, edgemode="directed")

## plot the graphNEL object in the plot window
## the recipEdges="distinct" command will draw an arrow for each relationship in the graph
## i.e., if there's a cycle (A above B and B above A), the default is to draw a single line with an arrow head
## at each end.  This command will force the graph to draw two arrows, one point in each direction
plot(mygraphNEL_SG_Ctx, recipEdges="distinct", attrs=attrs)

## Save the output as a pdf file
pdf("HM_SG_Ctx_output1.pdf", width = 34, height = 22) #height and width is paper size
plot(mygraphNEL_SG_Ctx, recipEdges="distinct", attrs=attrs)
dev.off()

