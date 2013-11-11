#! /usr/bin/env Rscript

#
# Generate wordcloud from a LaTeX index file.
#

# extract index terms if nothing has been done
if (!file.exists("terms.txt"))
  system("sh extract_keywords.sh")

# load and process data
library(wordcloud)
library(RColorBrewer)
key <- scan("terms.txt", what="character")
key.df <- as.data.frame(table(key))

# draw and save wordcloud
cols <- brewer.pal(9,"Pastel1")
pdf("wcindex.pdf")
with(key.df, wordcloud(key, Freq, min.freq=2, colors=cols, random.order=FALSE))
dev.off()

# crop the final PDF image
system("pdfcrop wcindex.pdf wcindex.pdf")
