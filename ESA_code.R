rm(list = ls()) 

#the code of this R file finds the matches and mismatches between pairs of datasets (excel spreadsheets) for different variables, saving the results in a folder called "ESA" located in the Desktop.

#to create the directory where the data is going to be contained and where we will be working
path_to_desktop <- file.path(Sys.getenv("USERPROFILE"),"Desktop") #this finds the path where the Desktop is (different for Windows and Mac users)
dir.create(file.path(sprintf("~%s/ESA",path_to_desktop)), recursive = TRUE) #to create the folder where we will work
setwd(sprintf("~%s/ESA",path_to_desktop)) #to set the working directory to the ESA folder.
#to download the data
download.file(url = "https://github.com/alextunascorzon/ESA/archive/master.zip", destfile = "ESA-master.zip") 
unzip(zipfile = "ESA-master.zip")
file.rename("ESA-master","Data")

#the following nested loops create the directories where the results will go (within the ESA folder, a series of folders will be created, which will then contain the results after running the entire code).

for (j in c("Scorer1_Scorer2", "Scorer3_Scorer4", "Scorer5_Scorer6", "Scorer7_Scorer8", "Scorer9_Scorer10")) {
  for (i in c("Action","Action_focus","Action_subaction","Action_subaction_focus_subfocus","Actionable","Focus","Focus_subfocus","Subaction","Subfocus")) {
    mkfldr <- sprintf("Results/%s/Graphs/%s", j, i) #sprintf is a command that allows to include variables within the name of the file. In this case, the variable ("%s") is the name os the student. %s is replaced by j and i, which in this case corresponds with the pair names and the combinations of categories.
    dir.create(file.path(mkfldr), recursive = TRUE)
  }
}


#to install the packages that are not already installed
package_load <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}
package_load(c("openxlsx", "rlist","matrixStats", "tidyr","car"))
#openxlsx is needed to open the excel files.
#rlist contains the function list.names, which allows us to get the names out of lists
#matrixStats is needed for a command that is later used to get the standard deviation.
#tidyr contains tools for changing the shape and hierarchy of data.
#car is needed for the testing of the ANOVAs.

#the following are empty vectors that will be populated with values from the different pairs later.
proportion_subaction_to_action <- proportion_subfocus_to_focus <- actions <- diff_actions <- focuses <- diff_focuses <- actions_subactions <- diff_actions_subactions <- focuses_subfocuses <- diff_focuses_subfocuses <- actions_focuses <- diff_actions_focuses <- all_combinations <- diff_all_combinations <- proportion_research_action <- proportion_research_subaction <- proportion_research_focus <- proportion_research_subfocus <- absolute_actions <- absolute_actions_subactions <- absolute_focuses <- absolute_focuses_subfocuses <- absolute_actions_focuses <- absolute_all <- absolute_diff_actions <- absolute_diff_actions <- absolute_diff_actions_subactions <-  absolute_diff_focuses <- absolute_diff_focuses_subfocuses <- absolute_diff_actions_focuses <- absolute_diff_all <- NULL 

#the following are empty lists that will be populated with values from the different pairs later.
action_type_across_pairs <- action_type_across_pairs_proportions <- action_type_across_scorers_proportions <- diff_action_type_across_pairs <- focus_type_across_pairs <- diff_focus_type_across_pairs <- action_subaction_type_across_pairs <- diff_action_subaction_type_across_pairs <- focus_subfocus_type_across_pairs <- diff_focus_subfocus_type_across_pairs <- vector("list") 

#to make a list with the five pairs of scorers
pairs <- list(c("Scorer1", "Scorer2"), c("Scorer3", "Scorer4"), c("Scorer5", "Scorer6"), c("Scorer7", "Scorer8"), c("Scorer9", "Scorer10")) 

#to loop over those five pairs.
for (student in pairs) { 
  try({ #this is to ignore and skip errors. This is needed because, by the end of each for loop, there is an error that pops up for every of the three first pairs. This is because there is a line of code that needs the information of the five pairs in order to work, meaning that it will not work unless the corresponding dataframe becomes populated with the data from the five pairs (i.e. until the code of the last pair is run). If this command ("try") is not added, the code would crash after the first pair is run.
    
    #to assign a letter to every student in the pair, so that their corresponding datasets can be called and read into R.
    a <- student[1] #student 1 of every pair
    b <- student[2] #student 2 of every pair
    
    #to read the datasets.
    student1 <- read.xlsx(sprintf("Data/%s Actions.xlsx", a)) 
    student2 <- read.xlsx(sprintf("Data/%s Actions.xlsx", b)) 
    
    ##to convert the "vagues" to "Unclear"
    student1[student1=="Vague"] <- "Unclear"
    student2[student2=="Vague"] <- "Unclear"
    #I saw one case where "Vague" appeared as lower case (probably because the scorer types "vague" instead of choosing from the selection panel), so:
    student1[student1=="vague"] <- "Unclear" 
    student2[student2=="vague"] <- "Unclear"
    
    ####### MATCHING #######
    
    ##the following code is to find the matches and differences of actions, sub-actions, focuses and sub-focuses individually (i.e. one by one):
    
    matches <- vector("list") #this is an empty list to which we will append vectors.
    #loop to find the matches of each column.
    for (i in which(colnames(student1)=="Action"):which(colnames(student1)=="Actionable?")) {#this is equivalent to say: for the values of i that go from the column called "Action" to the column called "Actionable?"
      matches <- c(matches, list(which(student1[,i] == student2[,i]))) #this is to populate the empty list "matches" with vectors containing the matching values (the rows where there was a match) between both datasets for every of the previously specified columns.
    } #the index (i) represents the column numbers where we have the actions, subactions, focuses, subfocuses and actionable categories (in this case, from column 7 (Action) to 11 (Actionable)). 
    
    #after the loop is run, "matches" will be a list of 5 vectors, each containing the matching actions, sub-actions, focuses, sub-focuses and actionable categories (in that order). That is, matches[[1]] gives us the rows that contain matching actions, matches[[2]] gives us the rows that contain matching sub-actions, and so on.
    
    ##the rows containing the matchings.
    action_rows <- matches[[1]] 
    subaction_rows <- matches[[2]] 
    focus_rows <- matches[[3]] 
    subfocus_rows <- matches[[4]]
    actionable_rows <- matches[[5]]
    
    #the following is to obtain the data associated with the rows where there was a match or mismatch, for the given category. 
    
    #the actions that are equal.
    matching_action <- student1[action_rows,]
    #the actions that are different.
    different_action <- student1[-action_rows,]
    different_action_2 <- student2[-action_rows,]
    #the sub-actions that are equal.
    matching_subaction <- student1[subaction_rows,]
    #the sub-actions that are different.
    different_subaction <- student1[-subaction_rows,] 
    different_subaction_2 <- student2[-subaction_rows,] 
    #the focuses that are equal.
    matching_focus <- student1[focus_rows,]
    #the focuses that are different.
    different_focus <- student1[-focus_rows,]
    different_focus_2 <- student2[-focus_rows,]
    #the sub-focuses that are equal.
    matching_subfocus <- student1[subfocus_rows,]
    #the sub-focuses that are different.
    different_subfocus <- student1[-subfocus_rows,]
    different_subfocus_2 <- student2[-subfocus_rows,]
    #the actionable items that are equal.
    matching_actionable <- student1[actionable_rows,]
    #the actionable items that are different.
    different_actionable <- student1[-actionable_rows,]
    different_actionable_2 <- student2[-actionable_rows,]
    
    ##matches and differences of actions and subactions (both at the same time).
    
    matches[[1]] %in% matches[[2]] #this gives us the rows that contain both matching actions and subactions (it would read something like: tell me if the rows that contain matching actions also contain matching subactions).
    
    action_subaction_rows <- matches[[1]][which(matches[[1]] %in% matches[[2]])] #by incorporating the previous piece into this, we are able to get the actual row numbers that contain matching actions and matching subactions at the same time.
    
    #the following is to obtain the data associated to the rows where there was a match or mismatch, for the given category.
    
    #the actions and sub-actions that are equal.
    matching_action_subaction <- student1[action_subaction_rows,]
    #the actions and sub-actions that are different.
    different_action_subaction <- student1[-action_subaction_rows,]
    different_action_subaction_2 <- student2[-action_subaction_rows,]
    
    
    ##matches and differences of focuses and sub-focuses (both at the same time).
    
    matches[[3]] %in% matches[[4]] #this tells us the rows that contain both matching focuses and sub-focuses (it would read something like: tell me if the rows that contain matching focuses also contain matching sub-focuses).
    
    focus_subfocus_rows <- matches[[3]][which(matches[[3]] %in% matches[[4]])] #by incorporating the previous piece into this, we are able to get the actual row numbers that contain matching focuses and matching sub-focuses at the same time.
    
    #the following is to obtain the data associated to the rows where there was a match or mismatch, for the given category.
    
    #the actions and sub-actions that are equal.
    matching_focus_subfocus <- student1[focus_subfocus_rows,]
    #the actions and sub-actions that are different.
    different_focus_subfocus <- student1[-focus_subfocus_rows,]
    different_focus_subfocus_2 <- student2[-focus_subfocus_rows,]
    
    ##matches and differences of actions and focuses (both at the same time).
    
    matches[[1]] %in% matches[[3]] #this tells us the rows that contain both matching actions and focuses (it would read something like: tell me if the rows that contain matching actions also contain matching focuses).
    
    action_focus_rows <- matches[[1]][which(matches[[1]] %in% matches[[3]])] #by incorporating the previous piece into this, we are able to get the actual row numbers that contain matching actions and matching focuses at the same time.
    
    #the following is to obtain the data associated to the rows where there was a match or mismatch, for the given category. 
    
    #the actions and sub-actions that are equal.
    matching_action_focus <- student1[action_focus_rows,]
    #the actions and sub-actions that are different.
    different_action_focus <- student1[-action_focus_rows,]
    different_action_focus_2 <- student2[-action_focus_rows,]
    
    
    ##matches and differences of actions, sub-actions, focuses and sub-focuses (all at the same time).
    
    action_subaction_rows %in% focus_subfocus_rows #this tells us the rows that contain matching actions, sub-actions, focuses and sub-focuses (it would read something like: tell me if the rows that contain matching actions and sub-actions also contain matching focuses and sub-focuses).
    
    action_subaction_focus_subfocus <- action_subaction_rows[which(action_subaction_rows %in% focus_subfocus_rows)] #by incorporating the previous piece into this, we are able to get the actual row numbers that contain all of the matchings.
    
    #the following is to obtain the data associated to the rows where there was a match or mismatch, for the given category. 
    
    #the actions and sub-actions that are equal.
    matching_all <- student1[action_subaction_focus_subfocus,]
    #the actions and sub-actions that are different.
    different_all <- student1[-action_subaction_focus_subfocus,]
    different_all_2 <- student2[-action_subaction_focus_subfocus,]
    
    
    ##the following is a function (add_mismatch) that will be used later to get the total mismatches for the different combinations of categories.
    
    add_mismatch <- function(a){ #a will be a list containing two vectors, one with the actions mismatched of student 1 and the other with the mismatched actions of student 2
      diff_names_bypair <- unique(unlist(lapply(a, list.names)))#this gets the names of the actions mismatched by the pair, then gets the unrepeated ones.
      #this could also be done without the list.names function, like this:
      #unique(c(names(diff_action_names_bypair[[1]]), names(diff_action_names_bypair[[2]])))
      data <- setNames(as.data.frame(diff_names_bypair, stringsAsFactors=FALSE), "Names") #to make a data frame and call "Names" to the first column.
      #to make a dataframe.
      data_freq <- lapply(a, as.data.frame)
      
      for (j in 1:length(data_freq)) { #every loop corresponds to a pair.
        data$mismatches <- lapply(data[,1], function(x) as.numeric(data_freq[[j]]$Freq[which(data_freq[[j]]$Var1%in%x)])) #to add the proportions of each pair to the dataframe data, assigining such values (in subsequent columns) to their corresponding element from the first column. This can be more easily understood by breaking this complex line code in parts:
        #which(data_freq[[j]])$Var1%in%x) indicates the elements of data[,1] (represented by the "x" due to the function and the preceding lapply command) that are contained in the dataframe data_freq. It gives the result as the position (a number) that those shared elements occupy in data_freq.
        #Example to see why this works:
        
        #data_freq[[1]]$Freq[7] would give us the 7th ("[7]") frequency ("Freq") of the pair 1 ("data_freq[[1]]"); then data_freq[[j]]$Freq[which(data_freq[[j]]$Var1%in%x) gives us the "[which(data_freq[[j]]$Var1%in%x)]" frequency ("Freq") of the jth pair ("[[j]]"), where [which(data_freq[[j]]$Var1%in%x) indicates the values of the data_freq that correspond to actions present in data (the values are matched to the elements in data[,1])
        
        names(data)[names(data) == "mismatches"] <- paste0("student", sep = "_", j) #to change the name of every column that is being created.
      }
      #to convert the columns to numeric
      data[,c(2,3)] <- as.data.frame(lapply(data[,c(2,3)], as.numeric))
      #to sum the values of each student and add it to a new column
      data$Total <- rowSums(data[,c(2,3)], na.rm = TRUE)
      return(data) #this is needed to work with data afterwards
    }
    
    
    
    ####### RESULTS BY PAIR ########
    
    ###the following code is to get the results for the individual pairs.
    
    ##action
    #to get the total number of matches.
    count_action <- length(matching_action[,"Action"]) 
    #to get the total number of mismatches.
    count_diff_action <- length(different_action[,"Action"])
    #to get the number of matches by the type of category
    action_type <- table(matching_action[, "Action"])
    action_type_across_pairs[[""]] <- setNames(as.data.frame(action_type, stringsAsFactors=FALSE), c("Names", "Total")) #to make a data frame and call "Names" and "Total" to the columns.
    #to get the number of mismatches by the type of category
    diff_action_type_1 <- diff_action_type <- table(different_action[,"Action"])
    diff_action_type_2 <- table(different_action_2[,"Action"])
    diff_action_bypair <- list(diff_action_type_1, diff_action_type_2) #to create a list.
    diff_action_type_across_pairs[[""]] <- as.data.frame(add_mismatch(diff_action_bypair)[,c("Names", "Total")])
    #to save the plot of total matches and differences.
    jpeg(sprintf("Results/%s_%s/Graphs/Action/action.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,1.2,1,1))
    barplot(c(count_action, count_diff_action), main = "Actions", names.arg = c("matches", "differences"), ylab = "count", col = c("gold", "firebrick"), ylim = c(0, 150), space = 0.2, width = c(0.2,0.2), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    #to save the plot of matches by type.
    jpeg(sprintf("Results/%s_%s/Graphs/Action/action_types.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,7,1,1))
    barplot(sort(action_type, decreasing = FALSE), main = "Action types", names.arg = names(sort(action_type, decreasing = FALSE)), xlab = "count", horiz = TRUE, col = "gold", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    #to get the total differences of both pairs.
    data <- add_mismatch(diff_action_bypair)
    #to save the plot of mismatches by type.
    jpeg(sprintf("Results/%s_%s/Graphs/Action/diff_action_types.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,9,1,1))
    barplot(data[order(data$Total),"Total"], main = "Action types", names.arg = data[order(data$Total),"Names"], xlab = "count", horiz = TRUE, col = "firebrick", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    
    
    
    ##subaction
    #to get the total number of matches.
    count_subaction <- length(matching_subaction[,"Sub-Action"])
    #to get the total number of mismatches.
    count_diff_subaction <- length(different_subaction[,"Sub-Action"])
    #to get the number of matches by the type of category
    subaction_type <- table(matching_subaction[,"Sub-Action"])
    #to get the number of mismatches by the type of category
    diff_subaction_type_1 <- diff_subaction_type <- table(different_subaction[,"Sub-Action"])
    diff_subaction_type_2 <- table(different_subaction_2[,"Sub-Action"])
    diff_subaction_bypair <- list(diff_subaction_type_1, diff_subaction_type_2)
    #to create a list.
    #to save the plot of total matches and differences.
    jpeg(sprintf("Results/%s_%s/Graphs/Subaction/subaction.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,1.2,1,1))
    barplot(c(count_subaction, count_diff_subaction), main = "Sub-actions", names.arg = c("matches", "differences"), ylab = "count", col = c("gold", "firebrick"), ylim = c(0, 150), space = 0.2,width = c(0.2,0.2), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    #to save the plot of matches by type.
    jpeg(sprintf("Results/%s_%s/Graphs/Subaction/subaction_types.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,7,1,1))
    barplot(sort(subaction_type, decreasing = FALSE), main = "Sub-action types", names.arg = names(sort(subaction_type, decreasing = FALSE)), xlab = "count", horiz = TRUE, col = "gold", axis.lty=1, las=1, xlim = c(0, 100),cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    #to get the total differences of both pairs.
    data <- add_mismatch(diff_subaction_bypair)
    #to save the plot of mismatches by type.
    jpeg(sprintf("Results/%s_%s/Graphs/Subaction/diff_subaction_types.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,9,1,1))
    barplot(data[order(data$Total),"Total"], main = "Action types", names.arg = data[order(data$Total),"Names"], xlab = "count", horiz = TRUE, col = "firebrick", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    
    
    
    ##focus
    #to get the total number of matches.
    count_focus <- length(matching_focus[,"Primary.Focus"])
    #to get the total number of mismatches.
    count_diff_focus <- length(different_focus[,"Primary.Focus"])
    #to get the number of matches by the type of category
    focus_type <- table(matching_focus[,"Primary.Focus"])
    focus_type_across_pairs[[""]] <- setNames(as.data.frame(focus_type, stringsAsFactors=FALSE), c("Names", "Total")) #to make a data frame and call "Names" and "Total" to the columns.
    #to get the number of mismatches by the type of category
    diff_focus_type_1 <- diff_focus_type <- table(different_focus[,"Primary.Focus"])
    diff_focus_type_2 <- table(different_focus_2[,"Primary.Focus"])
    diff_focus_bypair <- list(diff_focus_type_1, diff_focus_type_2) #to create a list.
    diff_focus_type_across_pairs[[""]] <- as.data.frame(add_mismatch(diff_focus_bypair)[,c("Names", "Total")])
    #to save the plot of total matches and differences.
    jpeg(sprintf("Results/%s_%s/Graphs/Focus/focus.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,1.2,1,1))
    barplot(c(count_focus, count_diff_focus), main = "Focus", names.arg = c("matches", "differences"), ylab = "count", col = c("gold", "firebrick"), ylim = c(0, 150), space = 0.2,width = c(0.2,0.2),cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    #to save the plot of matches by type.
    jpeg(sprintf("Results/%s_%s/Graphs/Focus/focus_types.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,7,1,1))
    barplot(sort(focus_type, decreasing = FALSE), main = "Focus types", names.arg = names(sort(focus_type, decreasing = FALSE)), xlab = "count", horiz = TRUE, col = "gold", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    #to get the total differences of both pairs.
    data <- add_mismatch(diff_focus_bypair)
    #to save the plot of mismatches by type.
    jpeg(sprintf("Results/%s_%s/Graphs/Focus/diff_focus_types.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,9,1,1))
    barplot(data[order(data$Total),"Total"], main = "Action types", names.arg = data[order(data$Total),"Names"], xlab = "count", horiz = TRUE, col = "firebrick", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    
    
    
    ##subfocus
    #to get the total number of matches.
    count_subfocus <-length(matching_subfocus[,"Primary.Sub-Focus"])
    #to get the total number of mismatches.
    count_diff_subfocus <-length(different_subfocus[,"Primary.Sub-Focus"])
    #to get the number of matches by the type of category
    subfocus_type <- table(matching_subfocus[,"Primary.Sub-Focus"])
    #to get the number of mismatches by the type of category
    diff_subfocus_type_1 <- diff_subfocus_type <- table(different_subfocus[,"Primary.Sub-Focus"])
    diff_subfocus_type_2 <- table(different_subfocus_2[,"Primary.Sub-Focus"])
    diff_subfocus_bypair <- list(diff_subfocus_type_1, diff_subfocus_type_2) #to create a list.
    #to save the plot of total matches and differences.
    jpeg(sprintf("Results/%s_%s/Graphs/Subfocus/subfocus.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,1.2,1,1))
    barplot(c(count_subfocus, count_diff_subfocus), main = "Sub-focus", names.arg = c("matches", "differences"), ylab = "count", col = c("gold", "firebrick"), ylim = c(0, 150), space = 0.2,width = c(0.2,0.2), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    #to save the plot of matches by type.
    jpeg(sprintf("Results/%s_%s/Graphs/Subfocus/subfocus_types.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,7,1,1))
    barplot(sort(subfocus_type, decreasing = FALSE), main = "Sub-focus types", names.arg = names(sort(subfocus_type, decreasing = FALSE)), xlab = "count", horiz = TRUE, col = "gold", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    #to get the total differences of both pairs.
    data <- add_mismatch(diff_subfocus_bypair)
    #to save the plot of mismatches by type.
    jpeg(sprintf("Results/%s_%s/Graphs/Subfocus/diff_subfocus_types.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,9,1,1))
    barplot(data[order(data$Total),"Total"], main = "Action types", names.arg = data[order(data$Total),"Names"], xlab = "count", horiz = TRUE, col = "firebrick", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    
    
    
    ##action & subaction
    #to get the total number of matches.
    count_action_subaction <- length(matching_action_subaction[,"Action"]) #I indicated "Action" but I could have indicated "Sub-Action", or even any other column name, as what counts is the number of rows.
    #to get the total number of mismatches.
    count_diff_action_subaction <- length(different_action_subaction[,"Action"])
    #to get the number of matches by the type of category
    action_subaction_type <- lapply(matching_action_subaction[,c("Action", "Sub-Action")], table)
    action_subaction_type_across_pairs[[""]] <- setNames(as.data.frame(action_subaction_type$`Sub-Action`, stringsAsFactors=FALSE), c("Names", "Total")) #to make a data frame and call "Names" and "Total" to the columns.
    #to get the number of mismatches by the type of category
    diff_action_subaction_type_1 <- diff_action_subaction_type <- lapply(different_action_subaction[,c("Action", "Sub-Action")], table) #for scorer 1
    diff_action_subaction_type_2 <- lapply(different_action_subaction_2[,c("Action", "Sub-Action")], table) #for scorer 2
    diff_action_subaction_bypair_action <- list(diff_action_subaction_type_1[[1]], diff_action_subaction_type_2[[1]]) #to create a list.
    diff_action_subaction_bypair_subaction <- list(diff_action_subaction_type_1[[2]], diff_action_subaction_type_2[[2]]) #to create a list.
    diff_action_subaction_type_across_pairs[[""]] <- as.data.frame(add_mismatch(diff_action_subaction_bypair_subaction)[,c("Names", "Total")])
    #to save the plot of total matches and differences.
    jpeg(sprintf("Results/%s_%s/Graphs/Action_subaction/action_subaction.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,1.2,1,1))
    barplot(c(count_action_subaction, count_diff_action_subaction), main = "Action & Sub-action", names.arg = c("matches", "differences"), ylab = "count", col = c("gold", "firebrick"), ylim = c(0, 150), space = 0.2,width = c(0.2,0.2), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    #to save the plot of matches by type.
    jpeg(sprintf("Results/%s_%s/Graphs/Action_subaction/action_subaction_types.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,7,1,1), mfrow=c(1,2))
    barplot(sort(action_subaction_type$Action, decreasing = FALSE), main = "Action", names.arg = names(sort(action_subaction_type$Action, decreasing = FALSE)), xlab = "count", horiz = TRUE, col = "gold", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    barplot(sort(action_subaction_type$`Sub-Action`, decreasing = FALSE), main = "Sub-action", names.arg = names(sort(action_subaction_type$`Sub-Action`, decreasing = FALSE)), xlab = "count", horiz = TRUE, col = "gold", axis.lty=1, las=1, xlim = c(0, 60), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    #to save the plot of total matches for action and for action + subaction
    jpeg(sprintf("Results/%s_%s/Graphs/Action_subaction/action_and_actionsubaction.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,1.2,1,1))
    barplot(c(count_action, count_action_subaction), main = "Matches", names.arg = c("Action", "Action and Subaction"), ylab = "count", col = c("gold", "gold"), ylim = c(0, 150), space = 0.2,width = c(0.2,0.2), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    #to get the total differences of both pairs.
    data_1 <- add_mismatch(diff_action_subaction_bypair_action)
    data_2 <- add_mismatch(diff_action_subaction_bypair_subaction)
    #to save the plot of mismatches by type.
    jpeg(sprintf("Results/%s_%s/Graphs/Action_subaction/diff_action_subaction_types.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,9,1,1), mfrow=c(1,2))
    barplot(data_1[order(data_1$Total),"Total"], main = "Action types", names.arg = data[order(data_1$Total),"Names"], xlab = "count", horiz = TRUE, col = "firebrick", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    barplot(data_2[order(data_2$Total),"Total"], main = "Sub-Action types", names.arg = data[order(data_2$Total),"Names"], xlab = "count", horiz = TRUE, col = "firebrick", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    
    
    
    ##focus & subfocus
    #to get the total number of matches.
    count_focus_subfocus <- length(matching_focus_subfocus[,"Primary.Focus"])
    #to get the total number of mismatches.
    count_diff_focus_subfocus <- length(different_focus_subfocus[,"Primary.Focus"])
    #to get the number of matches by the type of category
    focus_subfocus_type <- lapply(matching_focus_subfocus[,c("Primary.Focus", "Primary.Sub-Focus")], table)
    focus_subfocus_type_across_pairs[[""]] <- setNames(as.data.frame(focus_subfocus_type$`Primary.Sub-Focus`, stringsAsFactors=FALSE), c("Names", "Total")) #to make a data frame and call "Names" and "Total" to the columns.
    #to get the number of mismatches by the type of category
    diff_focus_subfocus_type_1 <- diff_focus_subfocus_type <- lapply(different_focus_subfocus[,c("Primary.Focus", "Primary.Sub-Focus")], table) #for scorer 1
    diff_focus_subfocus_type_2 <- lapply(different_focus_subfocus_2[,c("Primary.Focus", "Primary.Sub-Focus")], table) #for scorer 2
    diff_focus_subfocus_bypair_focus <- list(diff_focus_subfocus_type_1[[1]], diff_focus_subfocus_type_2[[1]]) #to create a list.
    diff_focus_subfocus_bypair_subfocus <- list(diff_focus_subfocus_type_1[[2]], diff_focus_subfocus_type_2[[2]]) #to create a list.
    diff_focus_subfocus_type_across_pairs[[""]] <- as.data.frame(add_mismatch(diff_focus_subfocus_bypair_subfocus)[,c("Names", "Total")])
    #to save the plot of total matches and  differences.
    jpeg(sprintf("Results/%s_%s/Graphs/Focus_subfocus/focus_subfocus.jpg", a, b), width = 1500, height = 1200)
    barplot(c(count_focus_subfocus, count_diff_focus_subfocus), main = "Focus & Sub-focus", names.arg = c("matches", "differences"), ylab = "count", col = c("gold", "firebrick"), ylim = c(0, 150), space = 0.2,width = c(0.2,0.2), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    #to save the plot of matches by type.
    jpeg(sprintf("Results/%s_%s/Graphs/Focus_subfocus/focus_subfocus_types.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,7,1,1), mfrow=c(1,2))
    barplot(sort(focus_subfocus_type$Primary.Focus, decreasing = FALSE), main = "Focus", names.arg = names(sort(focus_subfocus_type$Primary.Focus, decreasing = FALSE)), xlab = "count", horiz = TRUE, col = "gold", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    barplot(sort(focus_subfocus_type$`Primary.Sub-Focus`, decreasing = FALSE), main = "Sub-focus", names.arg = names(sort(focus_subfocus_type$`Primary.Sub-Focus`, decreasing = FALSE)), xlab = "count", horiz = TRUE, col = "gold", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    #to save the plot of total matches for focus and for focus + subfocus
    jpeg(sprintf("Results/%s_%s/Graphs/Focus_subfocus/focus_and_focussubfocus.jpg", a, b), width = 1500, height = 1200)
    barplot(c(count_focus, count_focus_subfocus), main = "Matches", names.arg = c("Focus", "Focus and Subfocus"), ylab = "count", col = c("gold", "gold"), ylim = c(0, 150), space = 0.2,width = c(0.2,0.2), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    #to get the total differences of both pairs.
    data_1 <- add_mismatch(diff_focus_subfocus_bypair_focus)
    data_2 <- add_mismatch(diff_focus_subfocus_bypair_subfocus)
    #to save the plot of mismatches by type.
    jpeg(sprintf("Results/%s_%s/Graphs/Focus_subfocus/diff_focus_subfocus_types.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,9,1,1), mfrow=c(1,2))
    barplot(data_1[order(data_1$Total),"Total"], main = "Focus types", names.arg = data[order(data_1$Total),"Names"], xlab = "count", horiz = TRUE, col = "firebrick", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    barplot(data_2[order(data_2$Total),"Total"], main = "Sub-Focus types", names.arg = data[order(data_2$Total),"Names"], xlab = "count", horiz = TRUE, col = "firebrick", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    
    
    
    ##action & focus
    #to get the total number of matches.
    count_action_focus <- length(matching_action_focus[,"Action"])
    #to get the total number of mismatches.
    count_diff_action_focus <- length(different_action_focus[,"Action"])
    #to get the number of matches by the type of category
    action_focus_type <- lapply(matching_action_focus[,c("Action", "Primary.Focus")], table)
    #to get the number of mismatches by the type of category
    diff_action_focus_type_1 <- diff_action_focus_type <- lapply(different_action_focus[,c("Action", "Primary.Focus")], table) #for scorer 1
    diff_action_focus_type_2 <- lapply(different_action_focus_2[,c("Action", "Primary.Focus")], table) #for scorer 2
    diff_action_focus_bypair_action <- list(diff_action_focus_type_1[[1]], diff_action_focus_type_2[[1]]) #to create a list.
    diff_action_focus_bypair_focus <- list(diff_action_focus_type_1[[2]], diff_action_focus_type_2[[2]]) #to create a list.
    #to save the plot of total matches and differences.
    jpeg(sprintf("Results/%s_%s/Graphs/Action_focus/action_focus.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,1.2,1,1))
    barplot(c(count_action_focus, count_diff_action_focus), main = "Action & Focus", names.arg = c("matches", "differences"), ylab = "count", col = c("gold", "firebrick"), ylim = c(0, 150), space = 0.2,width = c(0.2,0.2), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    #to save the plot of matches by type.
    jpeg(sprintf("Results/%s_%s/Graphs/Action_focus/action_focus_types.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,7,1,1), mfrow=c(1,2))
    barplot(sort(action_focus_type$Action, decreasing = FALSE), main = "Action", names.arg = names(sort(action_focus_type$Action, decreasing = FALSE)), xlab = "count", horiz = TRUE, col = "gold", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    barplot(sort(action_focus_type$Primary.Focus, decreasing = FALSE), main = "Focus", names.arg = names(sort(action_focus_type$Primary.Focus, decreasing = FALSE)), xlab = "count", horiz = TRUE, col = "gold", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    #to get the total differences of both pairs.
    data_1 <- add_mismatch(diff_action_focus_bypair_action)
    data_2 <- add_mismatch(diff_action_focus_bypair_focus)
    #to save the plot of mismatches by type.
    jpeg(sprintf("Results/%s_%s/Graphs/Action_focus/diff_action_focus_types.jpg", a, b), width = 3000, height = 1200)
    par(mai=c(1,7,1,1), mfrow=c(1,2))
    barplot(data_1[order(data_1$Total),"Total"], main = "Action types", names.arg = data_1[order(data_1$Total),"Names"], xlab = "count", horiz = TRUE, col = "firebrick", axis.lty=1, las=1, xlim = c(0, 250), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    barplot(data_2[order(data_2$Total),"Total"], main = "Focus types", names.arg = data_2[order(data_2$Total),"Names"], xlab = "count", horiz = TRUE, col = "firebrick", axis.lty=1, las=1, xlim = c(0, 250), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    
    
    
    ##action, subaction, focus & subfocus
    #to get the total number of matches.
    count_all <- length(matching_all[,"Action"])
    #to get the number of matches by the type of category
    all_type <- lapply(matching_all[,c("Action", "Sub-Action", "Primary.Focus", "Primary.Sub-Focus")], table)
    #to get the total number of mismatches.
    count_diff_all <- length(different_all[,"Action"])
    #to get the number of mismatches by the type of category
    diff_all_type <- lapply(different_all[,c("Action", "Sub-Action", "Primary.Focus", "Primary.Sub-Focus")], table)
    
    #to get the number of mismatches by the type of category
    diff_all_type_1 <- diff_all_type <- lapply(different_all[,c("Action", "Sub-Action", "Primary.Focus", "Primary.Sub-Focus")], table) #for scorer 1
    diff_all_type_2 <- lapply(different_all_2[,c("Action", "Sub-Action", "Primary.Focus", "Primary.Sub-Focus")], table) #for scorer 2
    diff_all_bypair_action <- list(diff_all_type_1[[1]], diff_all_type_2[[1]]) #to create a list.
    diff_all_bypair_subaction <- list(diff_all_type_1[[2]], diff_all_type_2[[2]]) #to create a list.
    diff_all_bypair_focus <- list(diff_all_type_1[[3]], diff_all_type_2[[3]]) #to create a list.
    diff_all_bypair_subfocus <- list(diff_all_type_1[[4]], diff_all_type_2[[4]]) #to create a list.
    #to save the plot of total matches and differences.
    jpeg(sprintf("Results/%s_%s/Graphs/Action_subaction_focus_subfocus/all.jpg", a, b), width = 1500, height = 1200)
    barplot(c(count_all, count_diff_all), main = "Action, Sub-action, Focus & Sub-focus", names.arg = c("matches", "differences"), ylab = "count", col = c("gold", "firebrick"), ylim = c(0, 150), space = 0.2,width = c(0.2,0.2), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    #to save the plot of matches by type.
    jpeg(sprintf("Results/%s_%s/Graphs/Action_subaction_focus_subfocus/all_types.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,7,1,1), mfrow=c(2,2))
    barplot(sort(all_type$Action, decreasing = FALSE), main = "Action", names.arg = names(sort(all_type$Action, decreasing = FALSE)), xlab = "count", horiz = TRUE, col = "gold", axis.lty=1, las=1, xlim = c(0, 10), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    barplot(sort(all_type$`Sub-Action`, decreasing = FALSE), main = "Sub-action", names.arg = names(sort(all_type$`Sub-Action`, decreasing = FALSE)), xlab = "count", horiz = TRUE, col = "gold", axis.lty=1, las=1, xlim = c(0, 10), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    barplot(sort(all_type$Primary.Focus, decreasing = FALSE), main = "Focus", names.arg = names(sort(all_type$Primary.Focus, decreasing = FALSE)), xlab = "count", horiz = TRUE, col = "gold", axis.lty=1, las=1, xlim = c(0, 10), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    barplot(sort(all_type$`Primary.Sub-Focus`, decreasing = FALSE), main = "Sub-focus", names.arg = names(sort(all_type$`Primary.Sub-Focus`, decreasing = FALSE)), xlab = "count", horiz = TRUE, col = "gold", axis.lty=1, las=1, xlim = c(0, 10), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    
    #to get the total differences of both pairs.
    data_1 <- add_mismatch(diff_all_bypair_action)
    data_2 <- add_mismatch(diff_all_bypair_subaction)
    data_3 <- add_mismatch(diff_all_bypair_focus)
    data_4 <- add_mismatch(diff_all_bypair_subfocus)
    #to save the plot of mismatches by type.
    jpeg(sprintf("Results/%s_%s/Graphs/Action_subaction_focus_subfocus/diff_all_types.jpg", a, b), width = 1500, height = 1200)
    par(mai=c(1,9,1,1), mfrow=c(2,2))
    barplot(data_1[order(data_1$Total),"Total"], main = "Action types", names.arg = data[order(data_1$Total),"Names"], xlab = "count", horiz = TRUE, col = "firebrick", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    barplot(data_2[order(data_2$Total),"Total"], main = "Subaction types", names.arg = data_2[order(data_2$Total),"Names"], xlab = "count", horiz = TRUE, col = "firebrick", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    barplot(data_3[order(data_3$Total),"Total"], main = "Focus types", names.arg = data[order(data_3$Total),"Names"], xlab = "count", horiz = TRUE, col = "firebrick", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    barplot(data_4[order(data_4$Total),"Total"], main = "Subfocus types", names.arg = data[order(data_4$Total),"Names"], xlab = "count", horiz = TRUE, col = "firebrick", axis.lty=1, las=1, xlim = c(0, 100), cex.axis = 2, cex.names = 2, cex.main= 2, cex.lab= 2)
    dev.off()
    
    
    ##actionable items
    #to get the total number of matches.
    count_actionable <- length(matching_actionable[,"Actionable?"])
    #to get the number of matches by the type of category
    actionable_type <- table(matching_actionable[, "Actionable?"])
    #to get the total number of mismatches.
    count_diff_actionable <- length(different_actionable[,"Actionable?"])
    #to get the number of mismatches by the type of category
    diff_actionable_type_1  <- diff_actionable_type <- table(different_actionable[,"Actionable?"])
    diff_actionable_type_2  <- table(different_actionable_2[,"Actionable?"])
    diff_actionable_bypair <- list(diff_actionable_type_1, diff_actionable_type_2) #to create a list.
    
    #to save the plot of total matches and differences.
    jpeg(sprintf("Results/%s_%s/Graphs/Actionable/actionable.jpg", a, b), width = 1000, height = 700)
    barplot(c(count_actionable, count_diff_actionable), main = "Actionable?", names.arg = c("matches", "differences"), ylab = "count", col = c("gold", "firebrick"), ylim = c(0, 150), space = 0.2,width = c(0.2,0.2))
    dev.off()
    #to save the plot of matches by type.
    jpeg(sprintf("Results/%s_%s/Graphs/Actionable/actionable_types.jpg", a, b), width = 1000, height = 700)
    par(mai=c(1,3.5,1,1))
    barplot(sort(actionable_type, decreasing = FALSE), main = "Actionable types", names.arg = names(sort(actionable_type, decreasing = FALSE)), xlab = "count", horiz = TRUE, col = "gold", axis.lty=1, las=1, xlim = c(0, 200))
    dev.off()
    
    #to get the total differences of both pairs.
    data <- add_mismatch(diff_actionable_bypair)
    #to save the plot of mismatches by type.
    jpeg(sprintf("Results/%s_%s/Graphs/Actionable/diff_actionable_types.jpg", a, b), width = 1000, height = 700)
    par(mai=c(1,4,1,1))
    barplot(data[order(data$Total),"Total"], main = "Actionable types", names.arg = data[order(data$Total),"Names"], xlab = "count", horiz = TRUE, col = "firebrick", axis.lty=1, las=1, xlim = c(0, 100))
    dev.off()
    
    
    
    
    ####### RESULTS ACROSS PAIRS ########
    
    ###the following code is to get the results compiled across pairs.
    
    ##proportions
    
    #to collect the proportion of rows containing subaction and action matchings
    proportion_subaction_to_action <- c(proportion_subaction_to_action, count_action_subaction/count_action)
    #to collect the proportion of rows containing subfocus and focus matchings
    proportion_subfocus_to_focus <- c(proportion_subfocus_to_focus, count_focus_subfocus/count_focus)
    
    #mean and variance of these proportions
    mean(proportion_subaction_to_action)
    var(proportion_subaction_to_action)
    mean(proportion_subfocus_to_focus)
    var(proportion_subfocus_to_focus)
    
    #vectors to collect the absolute and relative amounts of matchings by category for the different pairs.
    actions <- c(actions, count_action/length(student1$Action))
    absolute_actions <- c(absolute_actions, count_action)
    diff_actions <- c(diff_actions, count_diff_action/length(student1$Action))
    absolute_diff_actions <- c(absolute_diff_actions, count_diff_action)
    focuses <- c(focuses, count_focus/length(student1$Action))
    absolute_focuses <- c(absolute_focuses, count_focus)
    diff_focuses <- c(diff_focuses, count_diff_focus/length(student1$Action))
    absolute_diff_focuses <- c(absolute_diff_focuses, count_diff_focus)
    actions_subactions <- c(actions_subactions, count_action_subaction/length(student1$Action))
    absolute_actions_subactions <- c(absolute_actions_subactions, count_action_subaction)
    diff_actions_subactions <-c(diff_actions_subactions, count_diff_action_subaction/length(student1$Action))
    absolute_diff_actions_subactions <- c(absolute_diff_actions_subactions, count_diff_action_subaction)
    focuses_subfocuses <- c(focuses_subfocuses, count_focus_subfocus/length(student1$Action))
    absolute_focuses_subfocuses <- c(absolute_focuses_subfocuses, count_focus_subfocus)
    diff_focuses_subfocuses <- c(diff_focuses_subfocuses, count_diff_focus_subfocus/length(student1$Action))
    absolute_diff_focuses_subfocuses <- c(absolute_diff_focuses_subfocuses, count_diff_focus_subfocus)
    actions_focuses <- c(actions_focuses, count_action_focus/length(student1$Action))
    absolute_actions_focuses <- c(absolute_actions_focuses, count_action_focus)
    diff_actions_focuses <- c(diff_actions_focuses, count_diff_action_focus/length(student1$Action))
    absolute_diff_actions_focuses <- c(absolute_diff_actions_focuses, count_diff_action_focus)
    all_combinations <- c(all_combinations, count_all/length(student1$Action))
    absolute_all <- c(absolute_all, count_all)
    diff_all_combinations <- c(diff_all_combinations, count_diff_all/length(student1$Action))
    absolute_diff_all <- c(absolute_diff_all, count_diff_all)
    
    #the proportions of action matches for every pair.
    proportion_research_action[length(proportion_research_action)+1] <- length(matching_action[matching_action$Action %in% "Research & Monitoring","Action"])/length(matching_action[, "Action"]) #this saves the proportion of matches of every pair into a vector. Such vector at the beginning has length 0 and, as it becomes populated with data from each pair, the length increases by one
    #to get the average proportion
    mean(proportion_research_action)
    #to get the sd of the average proportion
    sd(proportion_research_action)
    action_type_across_pairs_proportions[[""]] <- setNames(as.data.frame(action_type/length(matching_action[, "Action"]), stringsAsFactors=FALSE), c("Names", "Total"))
    
    #the proportions of action matches for every scorer.
      
    action_type_across_scorers_proportions <- c(action_type_across_scorers_proportions, list(table(student1$Action)/length(student1$Action), table(student2$Action)/length(student2$Action)))
    
    proportion_research_subaction[length(proportion_research_subaction)+1] <- length(matching_action_subaction[matching_action_subaction$`Sub-Action` %in% "Basic Research & Status Monitoring","Sub-Action"])/length(matching_action_subaction[, "Sub-Action"]) #this saves the proportion of matches of every pair into a vector. Such vector at the beginning has length 0 and, as it becomes populated with data from each pair, the length increases by one.
    #to get the average proportion
    mean(proportion_research_subaction)
    #to get the sd of the average proportion
    sd(proportion_research_subaction)
    
    proportion_research_focus[length(proportion_research_focus)+1] <- length(matching_focus[matching_focus$Primary.Focus %in% "Research & Monitoring","Primary.Focus"])/length(matching_focus[, "Primary.Focus"]) #this saves the proportion of matches of every pair into a vector. Such vector at the beginning has length 0 and, as it becomes populated with data from each pair, the length increases by one.
    #to get the average proportion
    mean(proportion_research_focus)
    #to get the sd of the average proportion
    sd(proportion_research_focus)
    proportion_research_subfocus[length(proportion_research_subfocus)+1] <- length(matching_focus_subfocus[matching_focus_subfocus$`Primary.Sub-Focus` %in% "Basic Research & Status Monitoring","Primary.Sub-Focus"])/length(matching_focus_subfocus[, "Primary.Sub-Focus"]) #this saves the proportion of matches of every pair into a vector. Such vector at the beginning has length 0 and, as it becomes populated with data from each pair, the length increases by one.
    #to get the average proportion
    mean(proportion_research_subfocus)
    #to get the sd of the average proportion
    sd(proportion_research_subfocus)
    
    
    ##the following is to create excel files containing the matches and mismatches for each pair:
    
    
    
    ####### EXPORTING #######
    
    action <- setNames(as.data.frame(matching_action[,c("Action.Description","Action")], stringsAsFactors=FALSE), c("Description","Scorer_1_and_2"))
    write.xlsx(action, sprintf("Results/%s_%s/action.xlsx", student[1], student[2]))
    
    diff_action <- setNames(as.data.frame(different_action[,c("Action.Description","Action")], stringsAsFactors=FALSE), c("Description","Scorer_1"))
    diff_action[,3] <- setNames(as.data.frame(different_action_2[,"Action"], stringsAsFactors=FALSE), "Scorer_2")
    diff_action[is.na(diff_action)] <- "NA" #to get NAs into the excel files.  
    write.xlsx(diff_action, sprintf("Results/%s_%s/diff_action.xlsx", student[1], student[2]))
    
    
    
    subaction <- setNames(as.data.frame(matching_subaction[,c("Action.Description","Sub-Action")], stringsAsFactors=FALSE), c("Description","Scorer_1_and_2"))
    write.xlsx(subaction, sprintf("Results/%s_%s/subaction.xlsx", student[1], student[2]))
    
    diff_subaction <- setNames(as.data.frame(different_subaction[,c("Action.Description","Sub-Action")], stringsAsFactors=FALSE), c("Description","Scorer_1"))
    diff_subaction[,3] <- setNames(as.data.frame(different_subaction_2[,"Sub-Action"], stringsAsFactors=FALSE), "Scorer_2")
    diff_subaction[is.na(diff_subaction)] <- "NA" #to get NAs into the excel files.  
    write.xlsx(diff_subaction, sprintf("Results/%s_%s/diff_subaction.xlsx", student[1], student[2]))
    
    
    
    focus <- setNames(as.data.frame(matching_focus[,c("Action.Description","Primary.Focus")], stringsAsFactors=FALSE), c("Description","Scorer_1_and_2"))
    write.xlsx(focus, sprintf("Results/%s_%s/focus.xlsx", student[1], student[2]))
    
    diff_focus <- setNames(as.data.frame(different_focus[,c("Action.Description","Primary.Focus")], stringsAsFactors=FALSE), c("Description","Scorer_1"))
    diff_focus[,3] <- setNames(as.data.frame(different_focus_2[,"Primary.Focus"], stringsAsFactors=FALSE), "Scorer_2")
    diff_focus[is.na(diff_focus)] <- "NA" #to get NAs into the excel files.  
    write.xlsx(diff_focus, sprintf("Results/%s_%s/diff_focus.xlsx", student[1], student[2]))
    
    
    
    subfocus <- setNames(as.data.frame(matching_subfocus[,c("Action.Description","Primary.Sub-Focus")], stringsAsFactors=FALSE), c("Description","Scorer_1_and_2"))
    write.xlsx(subfocus, sprintf("Results/%s_%s/subfocus.xlsx", student[1], student[2]))
    
    diff_subfocus <- setNames(as.data.frame(different_subfocus[,c("Action.Description","Primary.Sub-Focus")], stringsAsFactors=FALSE), c("Description","Scorer_1"))
    diff_subfocus[,3] <- setNames(as.data.frame(different_subfocus_2[,"Primary.Sub-Focus"], stringsAsFactors=FALSE), "Scorer_2")
    diff_subfocus[is.na(diff_subfocus)] <- "NA" #to get NAs into the excel files.  
    write.xlsx(diff_subfocus, sprintf("Results/%s_%s/diff_subfocus.xlsx", student[1], student[2]))
    
    
    
    action_subaction <- setNames(as.data.frame(matching_action_subaction[,c("Action.Description","Action", "Sub-Action")], stringsAsFactors=FALSE), c("Description","Action_Scorer_1_and_2","Sub-Action_Scorer_1_and_2"))
    write.xlsx(action_subaction, sprintf("Results/%s_%s/action_subaction.xlsx", student[1], student[2]))
    
    diff_action_subaction <- setNames(as.data.frame(different_action_subaction[,c("Action.Description","Action", "Sub-Action")], stringsAsFactors=FALSE), c("Description","Action_Scorer_1","Sub-Action_Scorer_1"))
    diff_action_subaction[,c(4,5)] <- setNames(as.data.frame(different_action_subaction_2[,c("Action", "Sub-Action")], stringsAsFactors=FALSE), c("Action_Scorer_2","Sub-Action_Scorer_2"))
    diff_action_subaction[is.na(diff_action_subaction)] <- "NA" #to get NAs into the excel files.  
    write.xlsx(diff_action_subaction, sprintf("Results/%s_%s/diff_action_subaction.xlsx", student[1], student[2]))
    
    
    
    focus_subfocus <- setNames(as.data.frame(matching_focus_subfocus[,c("Action.Description","Primary.Focus", "Primary.Sub-Focus")], stringsAsFactors=FALSE), c("Description","Focus_Scorer_1_and_2", "Sub-Focus_Scorer_1_and_2"))
    write.xlsx(focus_subfocus, sprintf("Results/%s_%s/focus_subfocus.xlsx", student[1], student[2]))
    
    diff_focus_subfocus <- setNames(as.data.frame(different_focus_subfocus[,c("Action.Description","Primary.Focus", "Primary.Sub-Focus")], stringsAsFactors=FALSE), c("Description","Focus_Scorer_1", "Sub-Focus_Scorer_1"))
    diff_focus_subfocus[,c(4,5)] <- setNames(as.data.frame(different_focus_subfocus_2[,c("Primary.Focus", "Primary.Sub-Focus")], stringsAsFactors=FALSE), c("Focus_Scorer_2", "Sub-Focus_Scorer_2"))
    diff_focus_subfocus[is.na(diff_focus_subfocus)] <- "NA" #to get NAs into the excel files.  
    write.xlsx(diff_focus_subfocus, sprintf("Results/%s_%s/diff_focus_subfocus.xlsx", student[1], student[2]))
    
    
    
    action_focus <- setNames(as.data.frame(matching_action_focus[,c("Action.Description","Action", "Primary.Focus")], stringsAsFactors=FALSE), c("Description","Action_Scorer_1_and_2", "Focus_Scorer_1_and_2"))
    write.xlsx(action_focus, sprintf("Results/%s_%s/action_focus.xlsx", student[1], student[2]))
    
    diff_action_focus <- setNames(as.data.frame(different_action_focus[,c("Action.Description","Action", "Primary.Focus")], stringsAsFactors=FALSE), c("Description","Action_Scorer_1", "Focus_Scorer_1"))
    diff_action_focus[,c(4,5)] <- setNames(as.data.frame(different_action_focus_2[,c("Action", "Primary.Focus")], stringsAsFactors=FALSE), c("Action_Scorer_2", "Focus_Scorer_2"))
    diff_action_focus[is.na(diff_action_focus)] <- "NA" #to get NAs into the excel files.  
    write.xlsx(diff_action_focus, sprintf("Results/%s_%s/diff_action_focus.xlsx", student[1], student[2]))
    
    
    
    all <- setNames(as.data.frame(matching_all[,c("Action.Description","Action", "Sub-Action", "Primary.Focus", "Primary.Sub-Focus")], stringsAsFactors=FALSE), c("Description","Action_Scorer_1_and_2", "Subaction_Scorer_1_and_2","Focus_Scorer_1_and_2","Sub-Focus_Scorer_1_and_2"))
    write.xlsx(all, sprintf("Results/%s_%s/all.xlsx", student[1], student[2]))
    
    diff_all <- setNames(as.data.frame(different_all[,c("Action.Description","Action", "Sub-Action", "Primary.Focus", "Primary.Sub-Focus")], stringsAsFactors=FALSE), c("Description","Action_Scorer_1", "Subaction_Scorer_1","Focus_Scorer_1","Sub-Focus_Scorer_1"))
    diff_all[,c(6:9)] <- setNames(as.data.frame(different_all_2[,c("Action", "Sub-Action", "Primary.Focus", "Primary.Sub-Focus")], stringsAsFactors=FALSE), c("Action_Scorer_2", "Sub-Action_Scorer_2", "Focus_Scorer_2", "Sub-Focus_Scorer_2"))
    diff_all[is.na(diff_all)] <- "NA" #to get NAs into the excel files.  
    write.xlsx(diff_all, sprintf("Results/%s_%s/diff_all.xlsx", student[1], student[2]))
    
  }) 
}


#to get a table with the absolute values for matches.
absolute_matches <- as.data.frame(rbind(absolute_actions,absolute_focuses,absolute_actions_subactions,absolute_focuses_subfocuses,absolute_actions_focuses,absolute_all))
absolute_matches$Total <- rowSums(absolute_matches)
colnames(absolute_matches)[1:5] <- c("Pair 1","Pair 2","Pair 3","Pair 4","Pair 5")
rownames(absolute_matches) <- c("Action", "Focus", "Action & Sub-Action", "Focus & Sub-Focus", "Action & Focus", "All")
absolute_matches <- absolute_matches[order(absolute_matches$Total, decreasing = TRUE),]
absolute_matches <- cbind(rownames(absolute_matches),absolute_matches)
rownames(absolute_matches) <- NULL
colnames(absolute_matches)[1] <- ""

#to get a table with the absolute values for mismatches.
absolute_mismatches <- as.data.frame(rbind(absolute_diff_actions,absolute_diff_focuses,absolute_diff_actions_subactions,absolute_diff_focuses_subfocuses,absolute_diff_actions_focuses,absolute_diff_all))
absolute_mismatches$Total <- rowSums(absolute_mismatches)
colnames(absolute_mismatches)[1:5] <- c("Pair 1","Pair 2","Pair 3","Pair 4","Pair 5")
rownames(absolute_mismatches) <- c("Action", "Focus", "Action & Sub-Action", "Focus & Sub-Focus", "Action & Focus", "All")
absolute_mismatches <- absolute_mismatches[order(absolute_mismatches$Total, decreasing = FALSE),]
absolute_mismatches <- cbind(rownames(absolute_mismatches),absolute_mismatches)
rownames(absolute_mismatches) <- NULL
colnames(absolute_mismatches)[1] <- ""

#to get a table with the relative values for matches.
relative_matches <- setNames(as.data.frame(rbind(actions, focuses, actions_subactions,focuses_subfocuses,actions_focuses,all_combinations)), c("Pair 1", "Pair 2", "Pair 3","Pair 4", "Pair 5"))
relative_matches$Average <- rowMeans(relative_matches)
relative_matches$SD <- rowSds(data.matrix(relative_matches[,1:5]))
relative_matches <- relative_matches[order(relative_matches$Average, decreasing = TRUE),]
rownames(relative_matches) <- c("Action", "Focus", "Action & Sub-Action", "Focus & Sub-Focus", "Action & Focus", "All")
relative_matches <- cbind(rownames(relative_matches),relative_matches)
relative_matches[,2:ncol(relative_matches)] <- round(relative_matches[,2:ncol(relative_matches)], digits = 3)
rownames(relative_matches) <- NULL
colnames(relative_matches)[1] <- ""


#to get a table with the relative values for mismatches.
relative_mismatches <- setNames(as.data.frame(rbind(diff_actions, diff_focuses, diff_actions_subactions, diff_focuses_subfocuses, diff_actions_focuses, diff_all_combinations)), c("Pair 1", "Pair 2", "Pair 3","Pair 4", "Pair 5"))
relative_mismatches$Average <- rowMeans(relative_mismatches)
relative_mismatches$SD <- rowSds(data.matrix(relative_mismatches[,1:5]))
relative_mismatches <- relative_mismatches[order(relative_mismatches$Average, decreasing = TRUE),]
rownames(relative_mismatches) <- c("Action", "Focus", "Action & Sub-Action", "Focus & Sub-Focus", "Action & Focus", "All")
relative_mismatches <- cbind(rownames(relative_mismatches),relative_mismatches)
relative_mismatches[,2:ncol(relative_mismatches)] <- round(relative_mismatches[,2:ncol(relative_mismatches)], digits = 3)
rownames(relative_mismatches) <- NULL
colnames(relative_mismatches)[1] <- ""

#to save the tables
write.xlsx(absolute_matches, "Results/total_matches.xlsx")
write.xlsx(absolute_mismatches, "Results/total_mismatches.xlsx")
write.xlsx(relative_matches, "Results/relative_matches.xlsx")
write.xlsx(relative_mismatches, "Results/relative_mismatches.xlsx")

##Distribution of actions and focuses across pairs.

#the following function can be used later to get the results for different combinations of matchings/mismatchings and categories.
across_pairs <- function(matching=TRUE, category="Action") {
  if (matching==TRUE & category=="Action") {
    categ <- as.name("action") #the as.name function tells R to consider the word between quotation marks as a name.
    matches_or_mismatches <- as.name("matches")
  } else if (matching==TRUE & category=="Sub-Action") {
    categ <- as.name("action_subaction")
    matches_or_mismatches <- as.name("matches")
  } else if (matching==TRUE & category=="Focus") {
    categ <- as.name("focus")
    matches_or_mismatches <- as.name("matches")
  } else if (matching==TRUE & category=="Sub-Focus") {
    categ <- as.name("focus_subfocus")
    matches_or_mismatches <- as.name("matches")
  } else if (matching==FALSE & category=="Action") {
    categ <- as.name("diff_action")
    matches_or_mismatches <- as.name("mismatches")
  } else if (matching==FALSE & category=="Sub-Action") {
    categ <- as.name("diff_action_subaction")
    matches_or_mismatches <- as.name("mismatches")
  } else if (matching==FALSE & category=="Focus") {
    categ <- as.name("diff_focus")
    matches_or_mismatches <- as.name("mismatches")
  } else if (matching==FALSE & category=="Sub-Focus") {
    categ <- as.name("diff_focus_subfocus")
    matches_or_mismatches <- as.name("mismatches")
  }
  #the following vector will contain five vectors, each with the number of matches or mismatches by element for every pair.
  data_list <- eval(as.name(paste(categ, "_type_across_pairs", sep=""))) #eval is needed for R to be able to tell that as.name(paste(x, "_type_across_pairs", sep="")) is the name of an existing a variable.
  
  a <- vector() ##an empty vector
  for (i in 1:length(data_list)) { #every loop corresponds to a pair
    a <- c(a, data_list[[i]][["Names"]]) #to get a vector with the names of all of the elements that were used by the five pairs, all together. Some of the names will be repeated because different pairs used the same elements.
    data <- as.data.frame(unique(a)) #to filter out the elements that are repeated and then create a dataframe containing the names of the elements that were used by the five pairs.
    colnames(data) <- category #to change the name of the column with the elements used by the five pairs.
  }
  for (j in 1:length(data_list)) { #every loop corresponds to a pair
    data$mat_or_mismat <- lapply(data[,1], function(x) as.numeric(data_list[[j]]$Total[which(data_list[[j]]$Names%in%x)])) #to add the counts of each pair to the previous dataframe (data), assigining such values (in subsequent columns) to their corresponding element from the first column. This can be more easily understood by breaking this complex line code in parts:
    #1. data_list[[j]]$Total gives the values of the Total column.
    #2. which(data_list[[j]]$Names%in%x) indicates the elements of data[,1] (represented by the "x") that are contained in the data_list dataframe.
    #3. When [which(data_list[[j]]$Names%in%x)] (from point 2) is added to data_list[[j]]$Total (from point 1), the values of the elements in data_list[[j]] are matched to the elements shared between data[,1] and data_list so that each Total value is assigned to its corresponding Name.
    names(data)[names(data) == "mat_or_mismat"] <- paste0(matches_or_mismatches, sep = "_", j) #to change the name of every column that is being created.
  }
  #to convert the newly created columns into numeric.
  data[,2:ncol(data)] <- lapply(data[,2:ncol(data)], as.numeric) 
  #to make the sum of the rows and include the result as a new column
  data$Total <- rowSums(data[,2:ncol(data)], na.rm = TRUE) #na.rm must be set to TRUE (otherwise, it will not make the sum of those rows containing NAs)
  #to order the results based on the number of matches (from higher to lower)
  data <- data[order(data$Total, decreasing = TRUE),]
  #to convert columns into rows and rows into columns (avoiding column number 1).
  data_2 <- data.frame(t(data[-1]))
  #to get the names of the columns.
  colnames(data_2) <- data[, 1] #this is getting the names from the previous dataframe.
  #to convert the dataframe into a matrix
  data_2 <- as.matrix(data_2) #this is necessary for the posterior plotting.
  #to get rid of the last row (total)
  data_2 <- data_2[1:5,] 
  #to convert NAs into 0.
  data_2[is.na(data_2)] <- 0 #if we want R to plot the actions with NAs, we have to convert these into ceros.
  #plot across the pairs
  barplot(data_2, beside = FALSE, main = paste("Distribution of", gsub(".*diff_","",categ), matches_or_mismatches, sep=" "), names.arg = colnames(data_2),  col=c("cadetblue3","gold1", "orchid1", "gray0", "firebrick2"), las=2, cex.axis = 2,cex.names = 2, ylim = c(0,300),space=1, cex.main=2)
  title(ylab="Count of matches", line=5,  cex.lab = 2)
  legend("topright", legend = c("pair 1", "pair 2", "pair 3", "pair 4", "pair 5"), fill=c("cadetblue3","gold1", "orchid1", "gray0", "firebrick2"),  inset=c(0.035,0), cex = 2)
  return(data)
}


##PLOTS

#to get the different combination of results into our corresponding Results folder:

jpeg("Results/action_across.jpg", width = 1500, height = 1100)
par(mar=c(27,4,1,4)+4, xpd=TRUE)
across_pairs(matching=TRUE, category = "Action")
dev.off()
jpeg("Results/diff_action_across.jpg", width = 1500, height = 1100)
par(mar=c(31,4,1,4)+4, xpd=TRUE)
across_pairs(matching=FALSE, category = "Action")
dev.off()
jpeg("Results/action_subaction_across.jpg", width = 1500, height = 1100)
par(mar=c(37,4,1,4)+4, xpd=TRUE)
across_pairs(matching=TRUE, category = "Sub-Action")
dev.off()
jpeg("Results/diff_action_subaction_across.jpg", width = 1500, height = 1100)
par(mar=c(37,4,1,4)+4, xpd=TRUE)
across_pairs(matching=FALSE, category = "Sub-Action")
dev.off()
jpeg("Results/focus_across.jpg", width = 1500, height = 1100)
par(mar=c(27,4,1,4)+4, xpd=TRUE)
across_pairs(matching=TRUE, category = "Focus")
dev.off()
jpeg("Results/diff_focus_across.jpg", width = 1500, height = 1100)
par(mar=c(31,4,1,4)+4, xpd=TRUE)
across_pairs(matching=FALSE, category = "Focus")
dev.off()
jpeg("Results/focus_subfocus_across.jpg", width = 1500, height = 1100) 
par(mar=c(37,4,1,4)+4, xpd=TRUE)
across_pairs(matching=TRUE, category = "Sub-Focus")
dev.off()
jpeg("Results/diff_focus_subfocus_across.jpg", width = 1500, height = 1100)
par(mar=c(37,4,1,4)+4, xpd=TRUE)
across_pairs(matching=FALSE, category = "Sub-Focus")
dev.off()

###to get the total matches and mismatches for different categories.

##actions
action_total_matches <- across_pairs(matching=TRUE, category = "Action")
action_total_mismatches <- across_pairs(matching=FALSE, category = "Action")
action_total <- setNames(as.data.frame(unique(c(as.character(action_total_matches$Action), as.character(action_total_mismatches$Action))), stringsAsFactors=FALSE), "Action")
data_list <- list(action_total_matches[c(1,ncol(action_total_matches))], action_total_mismatches[c(1,ncol(action_total_mismatches))])

for (j in 1:length(data_list)) { #every loop corresponds to a pair
  action_total$Total <- lapply(action_total[,1], function(x) as.numeric(data_list[[j]]$Total[which(data_list[[j]]$Action%in%x)]))
  
  names(action_total)[names(action_total) == "Total"] <- paste0("Total", sep = "_", if (j==1){"matches"} else {"mismatches"}) #to change the name of every column that is being created.
}
action_total[c(2,3)] <- lapply(action_total[c(2,3)], as.numeric)
action_total$Total <- rowSums(action_total[,2:3], na.rm = TRUE)
action_total <- action_total[order(action_total$Total, decreasing = TRUE),]
action_total <- rbind(action_total, c("Total", colSums(action_total[,c(2:4)], na.rm = TRUE)))
action_total[is.na(action_total)] <- "NA"
write.xlsx(action_total, "Results/action_total.xlsx")
action_total_matches_proportions <- as.data.frame(c(action_total_matches[1],lapply(action_total_matches[,2:as.numeric(ncol(action_total_matches)-1)], function(x) {x/sum(x, na.rm=TRUE)})))

##subactions
subaction_total_matches <- across_pairs(matching=TRUE, category = "Sub-Action")
subaction_total_mismatches <- across_pairs(matching=FALSE, category = "Sub-Action")
subaction_total <- setNames(as.data.frame(unique(c(as.character(subaction_total_matches$`Sub-Action`), as.character(subaction_total_mismatches$`Sub-Action`))), stringsAsFactors=FALSE), "Names")
data_list <- list(subaction_total_matches[c(1,ncol(subaction_total_matches))], subaction_total_mismatches[c(1,ncol(subaction_total_mismatches))])

for (j in 1:length(data_list)) { #every loop corresponds to a pair
  subaction_total$Total <- lapply(subaction_total[,1], function(x) as.numeric(data_list[[j]]$Total[which(data_list[[j]]$`Sub-Action`%in%x)]))
  
  names(subaction_total)[names(subaction_total) == "Total"] <- paste0("Total", sep = "_", if (j==1){"matches"} else {"mismatches"}) #to change the name of every column that is being created.
}
subaction_total[c(2,3)] <- lapply(subaction_total[c(2,3)], as.numeric)
subaction_total$Total <- rowSums(subaction_total[,2:3], na.rm = TRUE)
subaction_total <- subaction_total[order(subaction_total$Total, decreasing = TRUE),]
subaction_total <- rbind(subaction_total, c("Total", colSums(subaction_total[,c(2:4)], na.rm = TRUE)))
subaction_total[is.na(subaction_total)] <- "NA"
write.xlsx(subaction_total, "Results/subaction_total.xlsx")


##focus
focus_total_matches <- across_pairs(matching=TRUE, category = "Focus")
focus_total_mismatches <- across_pairs(matching=FALSE, category = "Focus")
focus_total <- setNames(as.data.frame(unique(c(as.character(focus_total_matches$Focus), as.character(focus_total_mismatches$Focus))), stringsAsFactors=FALSE), "Names")

data_list <- list(focus_total_matches[c(1,ncol(focus_total_matches))], focus_total_mismatches[c(1,ncol(focus_total_matches))])

for (j in 1:length(data_list)) { #every loop corresponds to a pair
  focus_total$Total <- lapply(focus_total[,1], function(x) as.numeric(data_list[[j]]$Total[which(data_list[[j]]$Focus%in%x)]))
  
  names(focus_total)[names(focus_total) == "Total"] <- paste0("Total", sep = "_", if (j==1){"matches"} else {"mismatches"}) #to change the name of every column that is being created.
}
focus_total[c(2,3)] <- lapply(focus_total[c(2,3)], as.numeric)
focus_total$Total <- rowSums(focus_total[,2:3], na.rm = TRUE)
focus_total <- focus_total[order(focus_total$Total, decreasing = TRUE),]
focus_total <- rbind(focus_total, c("Total", colSums(focus_total[,c(2:4)], na.rm = TRUE)))
focus_total[is.na(focus_total)] <- "NA"
write.xlsx(focus_total, "Results/focus_total.xlsx")


#subfocus
subfocus_total_matches <- across_pairs(matching=TRUE, category = "Sub-Focus")
subfocus_total_mismatches <- across_pairs(matching=FALSE, category = "Sub-Focus")
subfocus_total <- setNames(as.data.frame(unique(c(as.character(subfocus_total_matches$`Sub-Focus`), as.character(subfocus_total_mismatches$`Sub-Focus`))), stringsAsFactors=FALSE), "Names")

data_list <- list(subfocus_total_matches[c(1,ncol(subfocus_total_matches))], subfocus_total_mismatches[c(1,ncol(subfocus_total_matches))])

for (j in 1:length(data_list)) { #every loop corresponds to a pair
  subfocus_total$Total <- lapply(subfocus_total[,1], function(x) as.numeric(data_list[[j]]$Total[which(data_list[[j]]$`Sub-Focus`%in%x)]))
  
  names(subfocus_total)[names(subfocus_total) == "Total"] <- paste0("Total", sep = "_", if (j==1){"matches"} else {"mismatches"}) #to change the name of every column that is being created.
}
subfocus_total[c(2,3)] <- lapply(subfocus_total[c(2,3)], as.numeric)
subfocus_total$Total <- rowSums(subfocus_total[,2:3], na.rm = TRUE)
subfocus_total <- subfocus_total[order(subfocus_total$Total, decreasing = TRUE),]
subfocus_total <- rbind(subfocus_total, c("Total", colSums(subfocus_total[,c(2:4)], na.rm = TRUE)))
subfocus_total[is.na(subfocus_total)] <- "NA"
write.xlsx(subfocus_total, "Results/subfocus_total.xlsx")


##plotting matches and mismatches separately (i.e. with individual plotbars for every pair and category)

#plot of matches and mismatches for actions
a <- rbind(matches=actions, mismatches=diff_actions) #on the left hand side of the argument we put the name of the matrix rows.
jpeg("Results/actions.jpg", width = 1000, height = 700)
par(mar=c(5,5,4,19), xpd=TRUE)
barplot(a, beside = FALSE, main = "Action", names.arg = c("pair 1", "pair 2", "pair 3", "pair 4", "pair 5"), col=c("gold","firebrick"), las=1, ylim = c(0,1),cex.names = 2, cex.main= 2, cex.lab= 2, cex.axis = 2)
legend(6.5,1, legend = c("Matches","Mismatches"), col=c("gold","firebrick"), lty=1, lwd = 6, inset=c(-0.3,0), cex = 2)
dev.off()

#plot of matches and mismatches for focuses
b <- rbind(matches=focuses, mismatches=diff_focuses) #on the left hand side of the argument we put the name of the matrix rows.
jpeg("Results/focuses.jpg", width = 1000, height = 700)
par(mar=c(5,5,4,19), xpd=TRUE)
barplot(b, beside = FALSE, main = "Focus", names.arg = c("pair 1", "pair 2", "pair 3", "pair 4", "pair 5"),  col=c("gold","firebrick"), las=1, ylim = c(0,1), cex.names = 2, cex.main= 2, cex.lab= 2, cex.axis = 2)
legend(6.5,1, legend = c("Matches","Mismatches"), col=c("gold","firebrick"), lty=1, lwd = 6, inset=c(-0.3,0), cex = 2)
dev.off()

#plot of matches and mismatches for action and subaction
c <- rbind(matches=actions_subactions, mismatches=diff_actions_subactions) #on the left hand side of the argument we put the name of the matrix rows.
jpeg("Results/action_subaction.jpg", width = 1000, height = 700)
par(mar=c(5,5,4,19), xpd=TRUE)
barplot(c, beside = FALSE, main = "Action & Subaction", names.arg = c("pair 1", "pair 2", "pair 3", "pair 4", "pair 5"),  col=c("gold","firebrick"), las=1, ylim = c(0,1), cex.names = 2, cex.main= 2, cex.lab= 2, cex.axis = 2)
legend(6.5,1, legend = c("Matches","Mismatches"), col=c("gold","firebrick"), lty=1, lwd = 6, inset=c(-0.3,0), cex = 2)
dev.off()

#plot of matches and mismatches for focus and subfocus
d <- rbind(matches=focuses_subfocuses, mismatches=diff_focuses_subfocuses) #on the left hand side of the argument we put the name of the matrix rows.
jpeg("Results/focus_subfocus.jpg", width = 1000, height = 700)
par(mar=c(5,5,4,19), xpd=TRUE)
barplot(d, beside = FALSE, main = "Focus & Subfocus", names.arg = c("pair 1", "pair 2", "pair 3", "pair 4", "pair 5"),  col=c("gold","firebrick"), las=1, ylim = c(0,1), cex.names = 2, cex.main= 2, cex.lab= 2, cex.axis = 2)
legend(6.5,1, legend = c("Matches","Mismatches"), col=c("gold","firebrick"), lty=1, lwd = 6, inset=c(-0.3,0), cex = 2)
dev.off()

#plot of matches and mismatches for action and focus
e <- rbind(matches=actions_focuses, mismatches=diff_actions_focuses) #on the left hand side of the argument we put the name of the matrix rows.
jpeg("Results/actions_focuses.jpg", width = 1000, height = 700)
par(mar=c(5,5,4,19), xpd=TRUE)
barplot(e, beside = FALSE, main = "Action & Focus", names.arg = c("pair 1", "pair 2", "pair 3", "pair 4", "pair 5"),  col=c("gold","firebrick"), las=1, ylim = c(0,1), cex.names = 2, cex.main= 2, cex.lab= 2, cex.axis = 2)
legend(6.5,1, legend = c("Matches","Mismatches"), col=c("gold","firebrick"), lty=1, lwd = 6, inset=c(-0.3,0), cex = 2)
dev.off()


#plot of matches and mismatches for all
f <- rbind(matches=all_combinations, mismatches=diff_all_combinations) #on the left hand side of the argument we put the name of the matrix rows.
jpeg("Results/all.jpg", width = 1000, height = 700)
par(mar=c(5,5,4,19), xpd=TRUE)
barplot(f, beside = FALSE, main = "Action, Subaction, Focus & Subfocus", names.arg = c("pair 1", "pair 2", "pair 3", "pair 4", "pair 5"),  col=c("gold","firebrick"), las=1, ylim = c(0,1),cex.names = 2, cex.main= 2, cex.lab= 2, cex.axis = 2)
legend(6.5,1, legend = c("Matches","Mismatches"), col=c("gold","firebrick"), lty=1, lwd = 6, inset=c(-0.3,0), cex = 2)
dev.off()


#plot of proportions of subaction to actions and subfocuses to focuses
x <- rbind(action=proportion_subaction_to_action,focus=proportion_subfocus_to_focus) #on the left hand side of the argument we put the name of the matrix rows.
jpeg("Results/proportions.jpg", width = 1000, height = 700)
par(mar=c(5,8,5,20),  xpd=TRUE)
barplot(x, beside = TRUE, names.arg = c("pair 1", "pair 2", "pair 3", "pair 4", "pair 5"), las=1, col=c("dimgrey","gainsboro"), ylim = c(0,1), cex.axis = 2, cex.names = 2, cex.main= 2)
title(ylab="Ratio of proportions", line=5, cex.lab= 2)
legend(15,1, legend = c("subaction/action","subfocus/focus"), col=c("dimgrey","gainsboro"), bty="n", lty=1, lwd = 10, cex = 2)
dev.off()


##ploting matches and mismatches grouping the different pairs into unique barplots with error bars showing the standard deviation.

#function for error bars
error.bar <- function(x, y, upper, lower=rep(0, length(x)), length=0.1,...){ #here I changed lower=upper by lower=rep(0, length(x)), because I only wanted the upper tails.
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}


d <- setNames(as.data.frame(c(rep("actions",5), rep("actions_subactions",5), rep("focuses",5), rep("focuses_subfocuses",5), rep("actions_focuses",5), rep("all_combined",5)),stringsAsFactors=FALSE), "Group")
d[,"Proportion"] <- as.data.frame(c(actions, actions_subactions, focuses, focuses_subfocuses, actions_focuses, all_combinations))


##to test if the differences are statistically significant.

#ANOVA
anova <- aov(Proportion ~ Group, data=d)
summary(anova) #we reject the null. 
write.xlsx(anova, "Results/anova_across_categories.xlsx")

#now, to see which ones are statistically significant different, we will apply a Tukey test.
TukeyHSD(anova)
#to test the normality assumption of the ANOVA
residuals <- split(anova$residuals,ceiling(seq_along(anova$residuals)/5)) #to get the residuals
par(mfrow=c(2,length(residuals)/2))
for (i in 1:length(residuals)){
  qqnorm(residuals[[i]]) #to get qqplots
  qqline(residuals[[i]]) #to fit the line
  print(shapiro.test(residuals[[i]])) #shapiro-wilk test (alpha = 0.05) 
}
#to test the homoscedasticity assumption of the ANOVA
leveneTest(d$Proportion~d$Group) #levene test (alpha = 0.05) 


#to plot the means and standard deviations.

jpeg("Results/mean_proportions_across_categories.jpg", width = 1000, height = 800)
par(mar=c(18,8,4,2) ,bty="l")
barx <- barplot(c(mean(actions),mean(actions_subactions),mean(focuses),mean(focuses_subfocuses),mean(actions_focuses),mean(all_combinations)),
                ylim=c(0,1),
                xlab="",
                ylab="",
                las=2,
                col="dimgrey",
                names.arg = c("action", "action & subaction", "focus", "focus & subfocus", "action & focus", "all combined"),
                cex.axis = 2,
                cex.names = 2,
                cex.main= 2)
error.bar(barx, c(mean(actions),mean(actions_subactions),mean(focuses),mean(focuses_subfocuses),mean(actions_focuses),mean(all_combinations)), c(sd(actions), sd(actions_subactions), sd(focuses), sd(focuses_subfocuses), sd(actions_focuses), sd(all_combinations)))
title(ylab="Mean proportion of matches", line=5,  cex.lab = 2)
text(barx, y=c(unlist(lapply(split(d$Proportion,ceiling(seq_along(d$Proportion)/5)), mean))[1:6] + unlist(lapply(split(d$Proportion,ceiling(seq_along(d$Proportion)/5)), sd))[1:6] + 0.03),labels = c("a","ab","bc","c","c","c"), cex = 2)
dev.off()


##to get plots for mean proportions of matches for basic research and basic research and monitoring.

d <- setNames(as.data.frame(c(rep("research_action",5), rep("research_subaction",5), rep("research_focus",5), rep("research_subfocus",5)),stringsAsFactors=FALSE), "Group")
d[,"Proportion"] <- as.data.frame(c(proportion_research_action, proportion_research_subaction, proportion_research_focus, proportion_research_subfocus))

#ANOVA
anova <- aov(Proportion ~ Group, data=d)
summary(anova) #we reject the null. 
write.xlsx(anova, "Results/anova_research.xlsx")

#now, to see which ones are statistically significant different, we will apply a Tukey test.
TukeyHSD(anova)
#to test the normality assumption of the ANOVA
residuals <- split(anova$residuals,ceiling(seq_along(anova$residuals)/5)) #to get the residuals
par(mfrow=c(2,length(residuals)/2))
for (i in 1:length(residuals)){
  qqnorm(residuals[[i]]) #to get qqplots
  qqline(residuals[[i]]) #to fit the line
  print(shapiro.test(residuals[[i]])) #shapiro-wilk test (alpha = 0.05) 
}
#to test the homoscedasticity assumption of the ANOVA
leveneTest(d$Proportion~d$Group) #levene test (alpha = 0.05) 

#to plot the means and standard deviations.
jpeg("Results/mean_proportions_research.jpg", width = 1000, height = 700)
par(mfrow=c(1,2), mar=c(5,8,4,4))
barx <- barplot(c(mean(proportion_research_action), mean(proportion_research_focus)),
                ylim=c(0,1),
                xlab="",
                ylab="",
                main="Research & Monitoring",
                las=1,
                col="dimgrey",
                names.arg = c("action", "focus"),
                cex.axis = 2,
                cex.names = 2,
                cex.main= 2)
error.bar(barx, c(mean(proportion_research_action), mean(proportion_research_focus)), c(sd(proportion_research_action), sd(proportion_research_focus)))
title(ylab="Mean proportion of matches", line=5,  cex.lab = 2)
text(barx, y=c(mean(proportion_research_action) + sd(proportion_research_action) + 0.02, mean(proportion_research_focus + sd(proportion_research_focus) + 0.02)),labels = c("a","b"), cex = 2)

barx <- barplot(c(mean(proportion_research_subaction), mean(proportion_research_subfocus)),
                ylim=c(0,1),
                xlab="",
                ylab="",
                main="Basic research & status monitoring",
                las=1,
                col="dimgrey",
                names.arg = c("subaction", "subfocus"),
                cex.axis = 2,
                cex.names = 2,
                cex.main= 2)
error.bar(barx, c(mean(proportion_research_subaction), mean(proportion_research_subfocus)), c(sd(proportion_research_subaction), sd(proportion_research_subfocus)))
title(ylab="Mean proportion of matches", line=5,  cex.lab = 2)
text(barx, y=c(mean(proportion_research_subaction) + sd(proportion_research_subaction) + 0.02, mean(proportion_research_subfocus + sd(proportion_research_subfocus) + 0.02)),labels = c("a","b"), cex = 2)
dev.off()


##to get the proportions of different action choices for the different scorers.

#to convert the action proportions into a dataframe.
data_proportions <- lapply(action_type_across_scorers_proportions, as.data.frame)

a <- vector() ##an empty vector
for (i in 1:length(data_proportions)) { #every loop corresponds to a pair
  a <- c(a, as.character(data_proportions[[i]][[1]])) #to get a vector with the names of all of the elements that were used by the scorers, all together. Some of the names will be repeated because different scorers used the same elements.
  data <- as.data.frame(unique(a)) #to filter out the elements that are repeated and then create a dataframe containing the names of the elements that were used by the five pairs.
  colnames(data) <- "Action" #to change the name of the column with the elements used by the five pairs.
}

for (j in 1:length(data_proportions)) { #every loop corresponds to a pair.
  data$scorer <- lapply(data[,1], function(x) as.numeric(data_proportions[[j]]$Freq[which(data_proportions[[j]]$Var1%in%x)])) #to add the proportions of each pair to the dataframe data, assigining such values (in subsequent columns) to their corresponding element from the first column.
  
  names(data)[names(data) == "scorer"] <- paste0("Scorer", sep = " ", j) #to change the name of every column that is being created.
}
#to convert the newly created columns into numeric.
data[,2:ncol(data)] <- lapply(data[,2:ncol(data)], as.numeric) 
#to order the results based on the mean number of matches (from higher to lower), and create a new column.
data$Average <- rowMeans(data[,2:ncol(data)], na.rm = TRUE) #na.rm must be set to TRUE (otherwise, it will not make the sum of those rows containing NAs)
#to order the results based on the number of average matches (from higher to lower)
data <- data[order(data$Average, decreasing = TRUE),]
#to calculate the standard deviation and create a new column, I am using the rowSds command from the matrixStats package. For that, I need to convert the dataframe to a matrix.
data$SD <- rowSds(data.matrix(data[,2:ncol(data)]), na.rm = TRUE) 
data[,2:ncol(data)] <- round(data[,2:ncol(data)], digits = 3)
data[is.na(data)] <- "NA"
write.xlsx(data, "Results/action_total_proportions_scorers.xlsx")




##to get plots for mean proportions of matches for the different actions.

#to convert the action proportions into a dataframe.
data_proportions <- lapply(action_type_across_pairs_proportions, as.data.frame)

a <- vector() ##an empty vector
for (i in 1:length(data_proportions)) { #every loop corresponds to a pair
  a <- c(a, data_proportions[[i]][[1]]) #to get a vector with the names of all of the elements that were used by the five pairs, all together. Some of the names will be repeated because different pairs used the same elements.
  data <- as.data.frame(unique(a)) #to filter out the elements that are repeated and then create a dataframe containing the names of the elements that were used by the five pairs.
  colnames(data) <- "Action" #to change the name of the column with the elements used by the five pairs.
}

for (j in 1:length(data_proportions)) { #every loop corresponds to a pair.
  data$matches <- lapply(data[,1], function(x) as.numeric(data_proportions[[j]]$Total[which(data_proportions[[j]]$Names%in%x)])) #to add the proportions of each pair to the dataframe data, assigining such values (in subsequent columns) to their corresponding element from the first column.
  
  names(data)[names(data) == "matches"] <- paste0("Pair", sep = " ", j) #to change the name of every column that is being created.
}
#to convert the newly created columns into numeric.
data[,2:ncol(data)] <- lapply(data[,2:ncol(data)], as.numeric) 
#to order the results based on the mean number of matches (from higher to lower), and create a new column.
data$Average <- rowMeans(data[,2:ncol(data)], na.rm = TRUE) #na.rm must be set to TRUE (otherwise, it will not make the sum of those rows containing NAs)
#to order the results based on the number of average matches (from higher to lower)
data <- data[order(data$Average, decreasing = TRUE),]
#to calculate the standard deviation and create a new column, I am using the rowSds command from the matrixStats package. For that, I need to convert the dataframe to a matrix.
data$SD <- rowSds(data.matrix(data[,2:ncol(data)]), na.rm = TRUE) 
data[,2:ncol(data)] <- round(data[,2:ncol(data)], digits = 3)
data[is.na(data)] <- "NA"
write.xlsx(data, "Results/action_total_proportions.xlsx")

#ANOVA
d <- gather(action_total_matches_proportions, Pair, Proportion, matches_1:matches_5, na.rm = TRUE)
d <- d[,c(1,3)]
colnames(d)[1] <- "Group"
anova <- aov(Proportion ~ Group, data=d)
summary(anova) #we reject the null. 
write.xlsx(anova, "Results/anova_across_actions.xlsx")

#now, to see which ones are statistically significant different, we will apply a Tukey test.
TukeyHSD(anova)
#if the confidence intervals overlaps with 0, then we will know that there is not a significant difference.
#to test the normality assumption of the ANOVA
residuals <- split(anova$residuals,ceiling(seq_along(anova$residuals)/5)) #to get the residuals
par(mfrow=c(2,ceiling(length(residuals)/2)))
for (i in 1:length(residuals)){
  qqnorm(residuals[[i]]) #to get qqplots
  qqline(residuals[[i]]) #to fit the line
  print(shapiro.test(residuals[[i]])) #shapiro-wilk test (alpha = 0.05) 
}
#to test the homoscedasticity assumption of the ANOVA
leveneTest(d$Proportion~d$Group) #levene test (alpha = 0.05) 

#to plot the means and standard deviations.
  jpeg("Results/mean_proportions_actions.jpg", width = 1000, height = 1200)
  par(mar=c(35,5,1,1)+4, xpd=TRUE)
  barx <- barplot(data$Average,
                  ylim=c(0,1),
                  xlab="",
                  ylab="",
                  las=2,
                  col="dimgrey",
                  cex.axis = 2.5,
                  cex.names = 2.5,
                  cex.main= 2.5,
                  names.arg = c(as.character(data$Action)))
  title(ylab="Mean proportion of matches", line=5.5,  cex.lab = 2.5)
  error.bar(barx, data$Average, data$SD)
  text(barx, y=data$Average+data$SD+0.03,labels = c("a", rep("b",9)), cex = 2.5)
  dev.off()


##to plot the relative proportions of matches between subaction/action and subfocus/focus

d <- setNames(as.data.frame(c(rep("subaction_to_action",5), rep("subfocus_to_focus",5)),stringsAsFactors=FALSE), "Group")
d[,"Proportion"] <- as.data.frame(c(proportion_subaction_to_action, proportion_subfocus_to_focus))

#ANOVA
anova <- aov(Proportion ~ Group, data=d)
summary(anova) #we reject the null. 
write.xlsx(anova, "Results/anova_ratios.xlsx")

#to test the normality assumption of the ANOVA
residuals <- split(anova$residuals,ceiling(seq_along(anova$residuals)/5)) #to get the residuals
par(mfrow=c(2,ceiling(length(residuals)/2)))
for (i in 1:length(residuals)){
  qqnorm(residuals[[i]]) #to get qqplots
  qqline(residuals[[i]]) #to fit the line
  print(shapiro.test(residuals[[i]])) #shapiro-wilk test (alpha = 0.05) 
}
#to test the homoscedasticity assumption of the ANOVA
leveneTest(d$Proportion~d$Group) #levene test (alpha = 0.05) 

#to plot the means and standard deviations.
jpeg("Results/relative_mean_proportions.jpg", width = 700, height = 900)
par(mar=c(5,7,5,4),bty="l")
barx <- barplot(c(mean(d[1:5,2]), mean(d[6:10,2])),
                ylim=c(0,1),
                xlab="",
                ylab="",
                las=1,
                col=c("dimgrey","gainsboro"),
                names.arg = c("subaction/action", "subfocus/focus"),
                cex.axis = 2,
                cex.names = 2,
                cex.main= 2)
error.bar(barx, c(mean(d[1:5,2]), mean(d[6:10,2])), c(sd(d[1:5,2]), sd(d[6:10,2])))
title(ylab="Mean ratio", line=5,  cex.lab = 2)
text(barx, y=c(mean(d[1:5,2])+sd(d[1:5,2])+0.02, mean(d[6:10,2])+sd(d[6:10,2])+0.02),labels = c("a","b"), cex = 2)
dev.off()




