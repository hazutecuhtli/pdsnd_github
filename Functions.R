
#*************************************************************************************************
#Functions
#*************************************************************************************************

#This file contains functions for the implementation of the processing and analysis related to
#Udacity data science with R programming. To be more specific, the functions change the format
#of the provided data, remove undefined values, create more informative features, and transform
#non-numerical categorical variables. Furthermore, it plots important features that help us
#to understand the behaviour of the bike-share systems, related to the provided data. 

#________________________________________________________________________________________________

PrePro1 <- function(df){
  
  #----------------------------------------------------------------------------------------------
  #Functio to give format to dataframes from the bike share system
  
  #Inputs -> Dataframe with dataframe bike sharing system data
  
  #Outputs-> Datframe with the wanted format
  #----------------------------------------------------------------------------------------------
  
  df <- DateFunc(df)
  Dictionaries <- StationsFunc(df)
  names(df) <- c('Index', 'City', 'Month', 'Day', 'Hour', 'Duration_Sec', 
                 'Origin', 'Destination', 'User', 'Gender', 'Birth_Year')  
  df$Origin <- ConvertingStations(Dictionaries$Stations_Dict, df, "Origin")
  df$Destination <- ConvertingStations(Dictionaries$Stations_Dict, df, "Destination")
  Days = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  Months = c("January", "February", "March", "April", "May", "June", "July", "August", 
             "September", "October", "November", "December")
  df$Day <- Encoding(df$Day, Days)
  df$Month <- Encoding(df$Month, Months)
  df <- dummy_cols(df, select_columns = "Gender")
  df <- dummy_cols(df, select_columns = "User")
  #df$User <- Encoding(df$User, Users)
  
  if (length(df[names(df)[(names(df) %in% c('Gender_Female', 'Gender_Male'))]])<1){
    df['Gender_Female'] <- rep(NA, nrow(df))
    df['Gender_Male'] <- rep(NA, nrow(df))
  }

  columns = c('Index', 'City', 'Month', 'Day', 'Hour', 'Duration_Sec', 'Origin', 'Destination',
              'User_Customer', 'User_Subscriber', 'Gender_Female', 'Gender_Male', 'Birth_Year')
  
  df <- df[names(df)[(names(df) %in% columns)]]

  df <- df[columns[columns %in% (names(df))]]

  names(df) = c('Index', 'City', 'Month', 'Day', 'Hour', 'Duration_Sec', 'Origin', 'Destination', 
                'Customer', 'Subscriber', 'Female', 'Male', 'Birth_Year')

  df[df['Female']==0 & df['Male']==0 & !is.na(df['Male'] | df['Female']), c('Female', 'Male')] <-NA
  df[df['Customer']==0 & df['Subscriber']==0 & !is.na(df['Customer'] | df['Subscriber']), c('Customer', 'Subscriber')] <-NA
  
  return(df)
}

#________________________________________________________________________________________________

PrePro2 <- function(df, cols){
  
  #----------------------------------------------------------------------------------------------
  #Function to remove undefined values for the dataframe
  
  #Inputs:
  #df -> Bike sharing system dataframe
  #cols -> Cols to remove the undefined values
  
  #Outputs:
  #df -> Modified dataframe
  #----------------------------------------------------------------------------------------------
  
  df <- ReplaceStrMissVals(df)
  df <- Func2Numeric(df, cols)
  df <- DropNANRowVals(df, names(df)[seq(3,length(names(df))-3)])
  
  return(df)
}

#________________________________________________________________________________________________

PrePro3 <- function(df, TimeRate1, TimeRate2, TimeRate3, TimeRate4){
  
  #----------------------------------------------------------------------------------------------
  #Functio to enconde the duration of the trips made by the users
  
  #Inputs:
  #df -> Bike sharing system dataframe
  #TimeRate1 -> First time limit to encode the used time
  #TimeRate2 -> Second time limit to encode the used time
  #TimeRate3 -> Third time limit to encode the used time
  #TimeRate4 -> Fourth time limit to encode the used time
  
  #Outputs:
  #df -> Modified dataframe
  #----------------------------------------------------------------------------------------------
  
  df['Duration_Min'] <- as.integer(round(df$Duration_Sec/60, digits = 0))
  df['Travel_Type'] <- as.integer(as.character(TravelTypeFunc(df, TimeRate1, TimeRate2, TimeRate3, TimeRate4)))
  
  columns = c('Index', 'City', 'Month', 'Day', 'Hour', 'Duration_Sec', 'Travel_Type', 
              names(df)[seq(7,length(names(df))-2,1)])
  
  df <- df[columns]
  
  return(df)
}

#________________________________________________________________________________________________

DateFunc <- function(df){
  
  #----------------------------------------------------------------------------------------------
  #Function to create new features based on a timestamp characteristic
  
  #Inputs:
  #df -> Bike sharing system dataframe

  #Outputs:
  #df -> Modified dataframe
  #----------------------------------------------------------------------------------------------
  
  df['Start.Month'] <- format(strptime(df$Start.Time, '%Y-%m-%d'), '%B')
  df['Start.Day'] <- format(strptime(df$Start.Time, '%Y-%m-%d'), '%A')
  df['Start.Hour'] <- format(strptime(df$Start.Time, '%Y-%m-%d %H:%M:%S'), '%H')
  df$X <- NULL
  df$Index <- seq.int(nrow(df))
  columns = c('Index', 'City', 'Start.Month', 'Start.Day', 'Start.Hour', 
              names(df)[seq(3,length(names(df))-5,1)])
  df <- df[columns]
  return(df)  
}

#________________________________________________________________________________________________

StationsFunc <- function(df){
  
  #----------------------------------------------------------------------------------------------
  #Function to change the format of dataframes
  
  #Inputs:
  #df -> Bike sharing system dataframe
  
  #Outputs:
  #df -> Modified dataframe
  #----------------------------------------------------------------------------------------------
  
  Stations_Names = unique(c(as.character(df$Start.Station), as.character(df$End.Station)))
  Stations_Dict <- list()
  Stations_InvDict <- list()
  for(i in 1:length(Stations_Names)) {
    Stations_Dict[as.character(Stations_Names[i])] <- i
    Stations_InvDict[i] <- Stations_Names[i]
  }
  ReturnList <- list("Stations_Dict" = Stations_Dict, "Stations_InvDict" = Stations_InvDict)
  return(ReturnList)
}

#________________________________________________________________________________________________

ConvertingStations <- function(Dict, df, column){
  
  #----------------------------------------------------------------------------------------------
  #Function to enconde the stations of the bike share system
  
  #Inputs:
  #df -> Bike sharing system dataframe
  #Dict ->Dictionary, list that emulate a dictionary, used to encode the column of interest
  #column -> Column  to enconde
  
  #Outputs:
  #ConvertedStations -> Vector contained the values enconded for the selected column
  #----------------------------------------------------------------------------------------------
  
  ConvertedStations <- list()
  for(i in 1:nrow(df)) {
    ConvertedStations[i] <- Dict[as.character(df[i,column])]
  }
  return(ConvertedStations)
}

#________________________________________________________________________________________________

Encoding <- function(data, levels){

  #----------------------------------------------------------------------------------------------
  #Function to enconde features of a dataframe
  
  #Inputs:
  #data -> Bike sharing system dataframe column as a vector
  #levels -> Values to enconde the column
  
  #Outputs:
  #Output -> Vector contained the values enconded for the selected column
  #----------------------------------------------------------------------------------------------  
  
    temp <- factor(data, levels = levels, ordered = TRUE)
  return(as.integer(temp))
}

#________________________________________________________________________________________________

ReplaceStrMissVals <- function(df){
  
  #----------------------------------------------------------------------------------------------
  #Function to change undefined values to nan values
  
  #Inputs:
  #df -> Bike sharing system dataframe

  #Outputs:
  #df -> Modified dataframe
  #----------------------------------------------------------------------------------------------    
  
  cols <- names(df)
  for (col in cols[seq(2,length(cols))]){
    indexs <- df[!(is.na(df[,col]) | is.null(df[,col])) & 
                   (df[,col]=="NULL" | df[,col]=="NA"),'Index']
    if (length(indexs) > 0){
      df[indexs, col] <- NA
    }
  }
  return(df)
}

#________________________________________________________________________________________________

Func2Numeric <- function(df, cols){
  
  #----------------------------------------------------------------------------------------------
  #Function to change the format of the dataframe columns
  
  #Inputs:
  #df -> Bike sharing system dataframe
  #cols -> cols of the dataframe to be changed
  
  #Outputs:
  #df -> Modified dataframe
  #----------------------------------------------------------------------------------------------    
  
  for (col in cols){
    df[col] <- as.integer(df[,col])
  }
  return(df)
}

#________________________________________________________________________________________________

DropNANRowVals <- function(df, cols){
  
  #----------------------------------------------------------------------------------------------
  #Function to remove undefined values
  
  #Inputs:
  #df -> Bike sharing system dataframe
  #cols -> cols of the dataframe look and remove nan values
  
  #Outputs:
  #df -> Modified dataframe
  #----------------------------------------------------------------------------------------------    
  
  for (col in cols){
    df <- df[!is.na(df[col]),]
  }
  return(df)
}

#________________________________________________________________________________________________

TravelTypeFunc <- function(df, TimeRate1, TimeRate2, TimeRate3, TimeRate4){
  
  #----------------------------------------------------------------------------------------------
  #Function to enconde the use time of the bikes
  
  #Inputs:
  #df -> Bike sharing system dataframe
  #TimeRate1 -> First time limit to encode the used time
  #TimeRate2 -> Second time limit to encode the used time
  #TimeRate3 -> Third time limit to encode the used time
  #TimeRate4 -> Fourth time limit to encode the used time
  
  #Outputs:
  #Used_Time -> Vector with the enconded values
  #----------------------------------------------------------------------------------------------   
  
  Used_Time <- list()
  for (i in 1:nrow(df)){
    if (df[i,'Duration_Min'] <= TimeRate1){
      Used_Time[[i]] <- 1
    } else if (df[i,'Duration_Min'] > TimeRate1 & df[i,'Duration_Min'] <=TimeRate2){
      Used_Time[[i]] <- 2
    } else if (df[i,'Duration_Min'] > TimeRate2 & df[i,'Duration_Min'] <=TimeRate3){
      Used_Time[[i]] <- 3          
    } else if (df[i,'Duration_Min'] > TimeRate3){
      Used_Time[[i]] <- 4
    }
    
  }
  return(Used_Time)
}

#________________________________________________________________________________________________

PlotDist <- function(df1, df2, df3, w=8, h=5){

  #----------------------------------------------------------------------------------------------
  #Function to plot the distribution for the use of bikes, in terms of using time
  
  #Inputs:
  #df1 -> Bike sharing system dataframe related with a first different city
  #df2 -> Bike sharing system dataframe related with a senond different city
  #df3 -> Bike sharing system dataframe related with a thrid different city
  #w -> Width of the figure 
  #h -> Heigth of the figure
  #----------------------------------------------------------------------------------------------   
  
  options(repr.plot.width=w, repr.plot.height=h)
  p1 <- ggplot(aes(x = Duration_Sec, y = ..density..), data = df1) +
    geom_histogram(binwidth = 100) +
    scale_x_continuous(limits=c(0,4000))
  
  p2 <- ggplot(aes(x = Duration_Sec, y = ..density..), data = df2) +
    geom_histogram(binwidth = 100) +
    scale_x_continuous(limits=c(0,4000))
  
  p3 <- ggplot(aes(x = Duration_Sec, y = ..density..), data = df3) +
    geom_histogram(binwidth = 100) +
    scale_x_continuous(limits=c(0,4000))
  
  grid.arrange(p1, p2, p3, ncol = 2)
}

#________________________________________________________________________________________________

PlotDuration <- function(df1, max, w=8, h=2){
  
  #----------------------------------------------------------------------------------------------
  #Function to plot the use of bikes
  
  #Inputs:
  #df1 -> Bike sharing system dataframe
  #max -> Maximun limit for the y axis of the plot
  #w -> Width of the figure 
  #h -> Heigth of the figure
  #----------------------------------------------------------------------------------------------  
  
  options(repr.plot.width=w, repr.plot.height=h)
  p1 <- ggplot( aes(x = Day, y = Duration_Sec, group = 1), data = df1 ) +
    geom_point(alpha=1/10, position = position_jitter(h=0), color='orange') +
    #coord_trans(y='sqrt') +
    #coord_cartesian(ylim = c(0, max)) + 
    geom_line(stat = 'summary', fun.y=mean) 
  
  p2 <- ggplot( aes(x = Month, y = Duration_Sec, group = 1), data = df1 ) +
    geom_point(alpha=1/10, position = position_jitter(h=0), color='orange') +
    #coord_trans(y='sqrt') +
    #coord_cartesian(ylim = c(0, max)) + 
    geom_line(stat = 'summary', fun.y=mean) 
  
  p3 <- ggplot( aes(x = Hour, y = Duration_Sec, group = 1), data = df1 ) +
    geom_point(alpha=1/10, position = position_jitter(h=0), color='orange') +
    #coord_trans(y='sqrt') +
    #coord_cartesian(ylim = c(0, max)) + 
    geom_line(stat = 'summary', fun.y=mean) 
  
  grid.arrange(p1, p2, p3, ncol = 3)
  
}

#________________________________________________________________________________________________

Rem_Outiers <- function(df, col){
  
  #----------------------------------------------------------------------------------------------
  #Function to remove outliers
  
  #Inputs:
  #df -> Bike sharing system dataframe
  #col -> Column of the dataframe to remove the outliers
  
  #Outoput:
  #df -> Modified dataframe
  
  #----------------------------------------------------------------------------------------------   
  
  Qrtls <- quantile(df[,col], probs = c(.25, .75))
  IQran <- IQR(df[,col])
  In_fence  = Qrtls[1]-1.5*IQran
  Out_fence = Qrtls[2]+1.5*IQran
  return(df[df[col]>In_fence & df[col]<Out_fence,])
}

#________________________________________________________________________________________________

PlotBoxplots <- function(df){
  
  #----------------------------------------------------------------------------------------------
  #Function to plot statisdistics in the form of boxplots
  
  #Inputs:
  #df -> Bike sharing system dataframe

  #----------------------------------------------------------------------------------------------     
  
  # make labels and margins smaller
  par(cex=0.7, mai=c(0.1,0.1,0.2,0.1))
  
  # define area for the histogram
  par(fig=c(0.1,0.55,0.6, 1))
  means <- tapply(df$Duration_Sec,df$Day,mean)
  boxplot(Duration_Sec~Day,data=df, main="Boxplots (Rental Days Vs. Time of use)",
          xlab="Day", ylab="Duration[s]", col=cm.colors(7)) 
  points(means,col="blue",pch=18)
  mtext("Seconds", side = 2, line = 3, cex = .7, font = 3) 
  mtext("Day", side = 1, line = 2.5, cex = .7, font = 3)
  par(fig=c(0.6,1,0.6, 1), new=TRUE)
  means <- tapply(df$Duration_Sec,df$Month,mean)
  boxplot(Duration_Sec~Month,data=df, main="Boxplots (Rental Months Vs. Time of use)",
          xlab="Month", ylab="Duration[s]", col=heat.colors(6)) 
  points(means,col="blue",pch=18, xlab="Hour", ylab="Duration[s]")
  mtext("Month", side = 1, line = 2.5, cex = .7, font = 3)
  # define area for the boxplot
  par(fig=c(0.1,1,0.1,.5), new=TRUE)
  means <- tapply(df$Duration_Sec,df$Hour,mean)
  boxplot(Duration_Sec~Hour,data=df, main="Boxplots (Rental Hours Vs. Time of use)",
          xlab="Hour", ylab="Duration[s]", col=colorRampPalette(rev(brewer.pal(9,"Blues")))(40)) 
  points(means,col="green",pch=18)
  mtext("Hour", side = 1, line = 2.5, cex = .7, font = 3)
  mtext("Seconds", side = 2, line = 3, cex = .7, font = 3) 
  
}

#________________________________________________________________________________________________

GroupCount2 <- function(df, col1, col2){
  
  #----------------------------------------------------------------------------------------------
  #Function to count the number of vaues in speficic features
  
  #Inputs:
  #df -> Bike sharing system dataframe
  #col1 -> Column of the dataframe to count unique values
  #col2 -> Column of the dataframe to count unique values
  
  #Outoput:
  #outpur -> List containing two lists, with the number of unique values in the 
  #          given columns
  
  #---------------------------------------------------------------------------------------------- 
  
  i <-1
  Count <- list()
  Metric <- list()
  for (col in sort(unique(df[,col1]))){
    
    Metric[i] <- col
    Count[i] <- sum(df[df[col1]==col & !is.na(df[col2]),col2])
    i <- i+1
  }
  return(list("Metric" = as.numeric(Metric), "Count" = as.numeric(Count)))
}

#________________________________________________________________________________________________

FunCatVar <- function(df, name, col1, col2, val1, val2){

  #----------------------------------------------------------------------------------------------
  #Function to replace undefined values
  
  #Inputs:
  #df -> Bike sharing system dataframe
  #name -> name of the columns to return
  #col1 -> Column of the dataframe to count unique values
  #col2 -> Column of the dataframe to count unique values
  #val1 -> Values to replace nan values for col1
  #val2 -> CValues to replace nan values for col2
  
  #Outoput:
  #df -> dataframe with replaced nan values containing the name columns
  
  #----------------------------------------------------------------------------------------------   
  
  df[name] <- NA
  df[df[col1]==1 & !is.na(df[col1]),name] <- val1
  df[df[col2]==1 & !is.na(df[col1]),name] <- val2
  
  return(df[, name])
}

#________________________________________________________________________________________________

GroupCount <- function(df){
  
  #----------------------------------------------------------------------------------------------
  #Function to count each of the genders of the dataframes
  
  #Inputs:
  #df -> Bike sharing system dataframe

    #Outoput:
  #df -> dataframe containing the column Gender with the count for male and female genders
  
  #----------------------------------------------------------------------------------------------    
  
  df['Gender'] <- FunCatVar(df, 'Gender', 'Female', 'Male', 'Female', 'Male')
  
  return(df)
}

#________________________________________________________________________________________________

PlotUseFVsM <- function(df, Var1, Var2, Const1, Const2, title, loc, step){
  
  #----------------------------------------------------------------------------------------------
  #Function to plot genders uses of bikes
  
  #Inputs:
  #df -> Bike sharing system dataframe
  #var1 -> Column containing one of the genders
  #var2 -> Column containing one of the genders
  #const1 -> Const values to idenfity the gender of var1
  #const2 -> Const values to idenfity the gender of var2
  #title -> Title of the plot
  #loc -> location for the lengends

  #----------------------------------------------------------------------------------------------    
  
  df.Temp <- GroupCount(df)
  plotting_df <- df.Temp %>% group_by('Metric' = df.Temp[,Var1], 'Cases' = df.Temp[,Var2]) %>% summarise(Freq = n()) %>% 
    mutate(Freq = if_else(Cases == Const1, -Freq, Freq))
  the_order <- unique(plotting_df[,'Metric'])
  
  MaxM = min(plotting_df[plotting_df['Cases']=='Male' & !is.na(plotting_df['Cases']), 'Freq'])
  MaxF = max(plotting_df[plotting_df['Cases']=='Female' & !is.na(plotting_df['Cases']), 'Freq'])
  MaxM <- -20^ceiling(log10(-MaxM))/10
  
  plotting_df[!is.na(plotting_df['Cases']),] %>% 
    ggplot(aes(x = Metric, y = Freq, group = Cases, fill = Cases)) +
    geom_bar(stat = "identity", width = 0.75) +
    coord_flip() +
    
    # another trick!
    scale_y_continuous(breaks = seq(MaxM, MaxF, step), labels = abs(seq(MaxM, MaxF, step))) +
    labs(x = Var1, y = "Count", title = title) +
    theme(legend.position = loc, legend.title = element_blank(), plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill =  "grey90")) +
    # reverse the order of items in legend
    # guides(fill = guide_legend(reverse = TRUE)) +
    # change the default colors of bars
    scale_fill_manual(values=c("coral2", "aquamarine3"), name="", 
                      breaks=c(Const1, Const2), labels=c(Const1, Const2))  
  
}

#________________________________________________________________________________________________

PlotBYVsGender <- function(df, Var1, title){
  
  #----------------------------------------------------------------------------------------------
  #Function to plot distribution of year_births
  
  #Inputs:
  #df -> Bike sharing system dataframe
  #var1 -> Column containing the birth years of the system users
  #title -> Title for the plot

  #----------------------------------------------------------------------------------------------    
  
  df.Temp <- GroupCount(df[df['Subscriber']==1 & !is.na(df['Birth_Year']),])
  ggplot(na.omit(df.Temp), aes(x=Birth_Year, fill=Gender, color=Gender)) +
    geom_histogram(position="identity", alpha=.2) +
    labs(x = Var1, y = "Count", title = title)+
    theme(legend.position = 'right', legend.title = element_blank(), plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill =  "grey90"))
  
}

#________________________________________________________________________________________________

ElbowFunct <- function(df, num_scores, tries, iter_max, width, height, n_start){
  
  #----------------------------------------------------------------------------------------------
  #Function to impement the elbow functions
  
  #Inputs:
  #df -> Bike sharing system dataframe
  #num_scores -> Number of scores, centers, to evaluate
  #tries -> Number of tries per score
  #iter_max -> maximun number of iteration for each k-mean
  #width -> Figure width
  #height -> Figure height
  #n_start -> n value for the kmean algorithm
  
  #----------------------------------------------------------------------------------------------     
  
  avg.totw.ss <-integer(length(tries)) 
  for(v in 2:num_scores){
    score.totw.ss <-integer(tries) 
    for(i in 1:tries){
      k_score <-kmeans(df,centers=v, iter.max = iter_max, nstart = n_start)
      score.totw.ss[i] <-k_score$tot.withinss
    }
    avg.totw.ss[v-1] <-mean(score.totw.ss) 
  }
  
  options(repr.plot.width=width, repr.plot.height=height)
  plot(2:num_scores,avg.totw.ss,type="b", main="K-means Scores",
       ylab="Average Scores Within Sum of Squares", xlab="Centers")
  
}

#________________________________________________________________________________________________
#Function group found clusters

PlotGroups <- function(df, n_groups, iter_max, n_start, title){

  #----------------------------------------------------------------------------------------------
  #Function to impement the elbow functions
  
  #Inputs:
  #df -> Bike sharing system dataframe
  #num_groups -> number of groups to plot
  #iter_max -> maximun iteration for the kmean algorithm
  #n_start -> n value for the kmeans algorithm
  #title -> title for the plot
  
  #----------------------------------------------------------------------------------------------     
  
  
  options(repr.plot.width=8, repr.plot.height=3)
  n = n_groups
  km <- kmeans(df, n, iter.max = iter_max, nstart = n_start)
  
  with(df, pairs(df[,c('Origin', 'Duration_Sec',  'Destination')], 
                 col=rainbow(n, s=1, v=1, alpha=1)[km$cluster], 
                 main = title, cex.main=.8, cex.labels = 1))     
 
}

#*************************************************************************************************
#END
#*************************************************************************************************