# read the JSON data
library(jsonlite)
json_file <- "D:\\Personal\\D_Drive\\Palash\\Inter_Projects\\Plabro\\shouts.json"
dat <- fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=",")))
text_field <- dat$text
id_field <- dat$id

library(tm)
text_field.corp <- Corpus(VectorSource(text_field))
	# Cleaning the dataset
text_field.corp <- tm_map(text_field.corp, removePunctuation)

	# when working with emails, you can remove special characters.
for(j in seq(text_field.corp)){   
     text_field.corp[[j]] <- gsub("/", " ", text_field.corp[[j]])   
     text_field.corp[[j]] <- gsub("@", " ", text_field.corp[[j]])   
     text_field.corp[[j]] <- gsub("\\|", " ", text_field.corp[[j]])   }

text_field.corp <- tm_map(text_field.corp, content_transformer(tolower))		# use this 'content_transformer' for newer versions of 'tm_map'
text_field.corp <- tm_map(text_field.corp, removeNumbers)
text_field.corp <- tm_map(text_field.corp, stripWhitespace)
text_field.corp <- tm_map(text_field.corp, removeWords, stopwords())
text_field.corp <- tm_map(text_field.corp, removeWords, c("lolll", "stylish"))				# Removing particular words:	lolll, stylish
	

#	COMBINING WORDS that should stay together :
#	If you wish to preserve a concept is only apparent as a collection of two or more words, then you can combine them or reduce them to a meaningful acronym before you begin the analysis. Here, I am using examples that are particular to qualitative data analysis.

for (j in seq(text_field.corp)){
text_field.corp[[j]] <- gsub("qualitative research", "QDA", text_field.corp[[j]])
text_field.corp[[j]] <- gsub("qualitative studies", "QDA", text_field.corp[[j]])
text_field.corp[[j]] <- gsub("qualitative analysis", "QDA", text_field.corp[[j]])
text_field.corp[[j]] <- gsub("research methods", "research_methods", text_field.corp[[j]])	}	
	
	
	# Stemming the dataset 			# Try also without stemming	
library(SnowballC)
text_field.corp <- tm_map(text_field.corp, stemDocument)
text_field.corp <- tm_map(text_field.corp, stripWhitespace)			# Stripping unnecesary whitespace from your documents:
text_field.tdm <- TermDocumentMatrix(text_field.corp, control=list(tokenize=scan_tokenizer,stopwords=TRUE))
#	inspect(text_field.tdm)			# Shows the frequency matrix for each term in the 100 json objects (not cleaned!)

	# MUST  :  This tells R to treat your preprocessed documents as text documents.
text_field.corp <- tm_map(text_field.corp, PlainTextDocument)   


# Removing the special characters from the 'text_field.corp' and Making a data frame with the values in it
text_mat <- as.data.frame(matrix(nrow=length(text_field.corp), ncol=1))
for( i in 1:length(text_field.corp)){
	text_mat[i,1] <- gsub("[^0-9A-Za-z///' ]", "", text_field.corp[[i]])
#	text_mat[i,1] <- gsub("[^[:alnum:]///' ]", "", text_field.corp[[i]])
}
names(text_mat) <- "text"
text_mat <- text_mat$text
tt_mat.corp <- Corpus(VectorSource(text_mat))


tt_mat.tdm <- TermDocumentMatrix(tt_mat.corp)
#	inspect(tt_mat.tdm)		#	Shows the frequency matrix for each term in the 100 json objects (Cleaned!)

tt_mat.dtm <- DocumentTermMatrix(tt_mat.corp)

freq <- colSums(as.matrix(tt_mat.dtm))
words <- names(freq)			# This will give all the unique words occurring in the doc.
		# Either select/feed another list to shortlist the favorable occurring words in this list.
# write.csv("words.csv",x=words, row.names=FALSE)
		
# Relevant words present in the doc : 
#	1. Sale :  buyer, purchas, sale, saler, sell, seller
# 	2. Rent :  rent, leas, tenant		
sell_words <- c("sale", "saler", "sell", "seller","buyer", "purchas")
rent_words <- c("rent", "leas", "tenant")
# Giving weightages to each of the above words
sell_wght <- c(1,0.75,1,0.75,0.5,0.3)			# assigning weightages randomly, or on the basis of the data which we have for sell of property
rent_wght <- c(1,0.8,0.75)
# Give the JSON object ID for the following relevant words
# Words related with money : purchas, deposit, interest, payment, budget, amount, advanc
#rent, rental, renting, lease, hire, charter, contract, tenant, leaseholder, lesse, lessor, sublet, sublease


dtm_matrix <- data.frame(as.matrix(tt_mat.dtm))
# select the columns with the names of the above written words, and then select the docs/rows with '1' in them, and multiply the columns by their weightages and find the rank of each of the docs/rows
dtm_sell <- dtm_matrix[sell_words]
dtm_rent <- dtm_matrix[rent_words]

for(i in 1:length(sell_wght)){	dtm_sell[,i] <- dtm_sell[,i]*sell_wght[i]	}
for(i in 1:length(rent_wght)){	dtm_rent[,i] <- dtm_rent[,i]*rent_wght[i]	}

rent_field <- id_field[which(rowSums(dtm_rent)!=0)]
sell_field <- id_field[which(rowSums(dtm_sell)!=0)]
rent_rank <- data.frame(json_row_no=names(which(rowSums(dtm_rent)!=0)), json_id=rent_field,	values=rowSums(dtm_rent[which(rowSums(dtm_rent)!=0),]) )
sell_rank <- data.frame(json_row_no=names(which(rowSums(dtm_sell)!=0)), json_id=sell_field,	values=rowSums(dtm_sell[which(rowSums(dtm_sell)!=0),]) )
rent_rank <- rent_rank[with(rent_rank, order(-values)), ]
rent_rank$rank <- c(1:length(rent_field))
sell_rank <- sell_rank[with(sell_rank, order(-values)), ]
sell_rank$rank <- c(1:length(sell_field))
rownames(rent_rank) <- NULL
rownames(sell_rank) <- NULL
rent_rank <- rent_rank[c("rank","json_row_no","json_id","values")]
sell_rank <- sell_rank[c("rank","json_row_no","json_id","values")]

write.csv("rent_rank.csv",x=rent_rank, row.names=FALSE)
write.csv("sell_rank.csv",x=sell_rank, row.names=FALSE)

findFreqTerms(tt_mat.dtm, lowfreq=20)
dtms <- removeSparseTerms(tt_mat.dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
# 	inspect(dtms)
freq <- colSums(as.matrix(dtms)) 

	Hierarchal Clustering
#	First calculate distance between words & then cluster them according to similarity.
library(cluster)   
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward")   
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters

	K-means clustering
#	The k-means clustering method will attempt to cluster words into a specified number of groups (in this case 2), such that the sum of squared distances between individual words and one of the group centers. You can change the number of groups you seek by changing the number specified within the kmeans() command.
library(fpc)   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)


TERM CORRELATIONS :
#	If you have a term in mind that you have found to be particularly meaningful to your analysis, then you may find it helpful to identify the words that most highly correlate with that term.

# Doing the association, and fetching the related words with these
sell_df <- data.frame(findAssocs(tt_mat.dtm, c("sell"), corlimit=0.5))
write.csv("sell_df.csv",x=sell_df)
sale_df <- data.frame(findAssocs(tt_mat.dtm, c("sale"), corlimit=0.3))
write.csv("sale_df.csv",x=sale_df)
rent_df <- data.frame(findAssocs(tt_mat.dtm, c("rent"), corlimit=0.3))
write.csv("rent_df.csv",x=rent_df)
leas_df <- data.frame(findAssocs(tt_mat.dtm, c("leas"), corlimit=0.3))
write.csv("leas_df.csv",x=leas_df)
# findAssocs(tt_mat.dtm, c("sell" , "sale", "rent" , "leas"), corlimit=c(0.5,0.3,0.3,0.3))


library(wordcloud)
wordcloud(names(freq), freq, min.freq=20, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

