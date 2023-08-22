remove_comma <- function(z){
  # Transform a number string with comma delimiters into numeric
  return(as.numeric(stringr::str_remove_all(z,',')))
}

transform_entry <- function(z){
  # Transform a death toll record string into numeric
  require(stringr)
  # Remove possible citation indices within square brackets
  z <- str_remove_all(z,'\\[.+\\]')
  if(grepl('–',z)){
    # The record is a range
    bounds <- str_split(z,'–')[[1]] # Find the upper/lower bound
    bounds <- sapply(bounds, function(x)
      remove_comma(str_remove_all(x,'\\+'))) # Change bounds into numeric
    # "+" suffix is removed from both bounds
    return(mean(bounds))
  }
  # The record is a single lower bound
  if(grepl('\\+',z)) return(remove_comma(str_remove_all(z,'\\+')))
  # The record is an exact number
  return(remove_comma(z))
}

url <- "https://en.wikipedia.org/wiki/List_of_natural_disasters_by_death_toll"
library(rvest)
library(stringr)
library(ggplot2)
library(scales)
# Read webpage and extract tables
page <- read_html(url)
nodes <- html_nodes(page,'.wikitable')
table20 <- as.data.frame(html_table(nodes[2],fill=TRUE)[[1]])
table21 <- as.data.frame(html_table(nodes[3],fill=TRUE)[[1]])
table20$DeathToll <- sapply(table20$`Death toll`,transform_entry)
table21$DeathToll <- sapply(table21$`Death toll`,transform_entry)
table_combined <- rbind(table20,table21)
table_combined$Type <- as.factor(table_combined$Type)
type_name <- levels(table_combined$Type)
colors <- c("black","blue","lightblue","darkgreen","red","brown",
            "orange","yellow","green","pink")
names(colors) <- type_name
# Death toll is plotted in log10 scale for more clarity
pdf("AllCauseDeathToll.pdf",width=14,height=7)
print(ggplot(table_combined) + theme_classic() +
        geom_point(aes(x=Year,y=DeathToll,color=Type),size=2) +
        scale_y_log10(labels=label_comma(),
                      breaks=10^(2:7)) +
        annotation_logticks() + scale_color_manual(values=colors) +
        xlab("Year") + ylab("Death Toll") +
        ggtitle("Highest single-event death toll from natural disaster by year") +
        theme(plot.title=element_text(hjust=0.5)))
dev.off()