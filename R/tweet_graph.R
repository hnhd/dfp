#' Pivot the Twitter data. 
#' @param issue_terms Search term or terms. Takes input as a vector. 
#' @param tweet_database Tweet database. Must have the columns "last_name", "party", "state" and "tweet".
#' @param scale_by_total Scale your graph by all tweets by the Senator. Defaults to FALSE, which outputs tweet counts.

pivot_twitter_data <- function(tweet_database, issue_terms, scale_by_total=FALSE){
  
  # add a flag to mark if the tweet exists (used to scale by total)
  tweet_database$tweet_exists <- TRUE
  
  # identify tweets that contain the key term and flag them
  tweet_database$issue_flag <- tweet_database$tweet %>%
    tolower %>%
    str_detect(paste0(issue_terms, collapse = "|"))
  
  # pivot the data to show counts by senator, party & state
  issue_data <- tweet_database %>%
    dcast(last_name+party+state~"total_mentions", value.var = "issue_flag", fun.aggregate = sum)
  
  # recode the parties
  issue_data$party <- 
    ifelse(issue_data$party == "Democrat", "D", 
    ifelse(issue_data$party == "Republican", "R",
    ifelse(issue_data$party == "Independent", "I", "")))
  
  # if scaling by total, add a scale mentions column 
  # pivot the data to show all tweets
  tweet_counts <- tweet_database %>%
    dcast(last_name+party~"total_tweets", value.var = "tweet_exists", fun.aggregate = sum)
  
  if(scale_by_total) {
    # replace total mentions with the mentions per number of tweet
    issue_data$total_tweets <- tweet_counts$total_tweets
    issue_data$scaled_mentions <- issue_data$total_mentions/tweet_counts$total_tweets
  }
  # return the pivoted dataframe
  return(issue_data)
}

#' Plot informative analytic data on the Twitter search.
#' @param issue_terms Search term or terms. Takes input as a vector. 
#' @param tweet_database Tweet database. Must have the columns "last_name", "party", "state" and "tweet".
##' @export

issue_analysis <- function(tweet_database, issue_terms){
  issue_data <- pivot_twitter_data(issue_terms, tweet_database, scale_by_total=TRUE)
  issue_data <- issue_data[order(issue_data$scaled_mentions, decreasing = TRUE),]
  
  party_pivot <- join(
    dcast(issue_data, party ~ "total_mentions", value.var="total_mentions", fun.aggregate=sum),
    dcast(issue_data, party ~ "total_tweets", value.var="total_tweets", fun.aggregate=sum)
  )
  party_pivot$pct_mentions <- paste0(round(party_pivot$total_mentions/party_pivot$total_tweets*100,1),"%")
  
  message(paste0("There were a total of ", sum(party_pivot$total_mentions), " mentions of the term(s)"))
  message(paste0("- Democrats mentioned these terms ", party_pivot[party_pivot$party == "D", ]$total_mentions, " times, ", party_pivot[party_pivot$party == "D", ]$pct_mentions," of all tweets from a Democrata."))
  message(paste0("- Republicans mentioned these terms ", party_pivot[party_pivot$party == "R", ]$total_mentions, " times, ", party_pivot[party_pivot$party == "R", ]$pct_mentions," of all tweets from a Republicans."))
  message(paste0("- Independents Sanders & King mentioned these terms ", party_pivot[party_pivot$party == "I", ]$total_mentions, " times, ", party_pivot[party_pivot$party == "I", ]$pct_mentions," of all tweets from Independents."))
  message(paste0("- Senators, in order of support: \n\n", paste0(paste0(issue_data$last_name, " (", issue_data$total_mentions, " - ", round(issue_data$scaled_mentions*100,1), "%)"), collapse = ", ")))
}

#' Plot Data for Progress specified tweet graphs.
#' @param issue_terms Search term or terms. Takes input as a vector. 
#' @param tweet_database Tweet database. Must have the columns "last_name", "party", "state" and "tweet".
#' @param scale_by_total Scale your graph by all tweets by the Senator. Defaults to FALSE, which outputs tweet counts.
#' @param save_local Save the graph to your working directory. Defaults to FALSE.
#' @export

issue_bargraph <- function(tweet_database, issue_terms, scale_by_total=FALSE, save_local=FALSE) { 
  
  issue_data <- pivot_twitter_data(issue_terms, tweet_database, scale_by_total)
  
  # change the last names 
  issue_data$last_name <- paste0(toupper(issue_data$last_name), " (", issue_data$party, "-", issue_data$state, ")")
  issue_data$last_name <- issue_data$last_name %>%
    factor(levels = issue_data$last_name[order(issue_data$party, if(scale_by_total){issue_data$scaled_mentions}else{issue_data$total_mentions})])
  
  # save the tweet graph
  tweet_graph <- ggplot(data = issue_data, aes(x=last_name, y=if(scale_by_total){issue_data$scaled_mentions}else{issue_data$total_mentions}, fill=party)) +
    geom_bar(stat="identity", width = 1)+ 
    coord_flip() +
    labs(
      title = toupper(paste0(if(scale_by_total){"Scaled "},"Senator Mentions of Terms")), 
      subtitle = paste0("'",paste0(issue_terms, collapse = "', '"), "'"), 
      x = "Senator", 
      y = if(scale_by_total){"Share of Term Mentions To All Tweets"}else{"Count of Term Mentions"}
    ) +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0),labels=if(scale_by_total){percent}else{comma}) +
    scale_fill_manual(
      name = "Party",
      values = c("#0000cc", "#595959", "#cc0000"),
      breaks = c("Democrat", "Independent", "Republican"),
      labels = c("Democrat", "Independent", "Republican"),
      guide=FALSE
    ) + 
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 13),
      panel.background = element_blank(),
      text = element_text(family = "Montserrat"),
      axis.text.x = element_text(size = 9, face = "bold"),
      axis.text.y = element_text(size = 7),
      axis.ticks.length=unit(1, "mm"),
      axis.line.y = element_line(color="black", size = 0.5),
      axis.line.x = element_line(color="black", size = 0.5)
    )
  
  # print the barplot
  print(tweet_graph)
  
  # if the set to save local, save the file to the correct dimension
  if(save_local){
    ggsave(
      filename = paste0(if(scale_by_total){"scaled_"},str_replace_all(paste0(issue_terms, collapse = ""), " ", "_"), ".png"),
      width = 160, height = 280, units = "mm",
      scale = 1, dpi = 320
    )
  }
}
