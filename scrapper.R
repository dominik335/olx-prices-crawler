library(rvest)
library(stringr)
url <- "https://www.olx.pl/nieruchomosci/stancje-pokoje/warszawa/"
threshold = 800

if(str_count(url, '\\?') == 0) url = paste(url,"?", sep = "")

page <- read_html(url)

tekst = html_text(html_node(page, ":nth-child(16) .lheight24 span")) #jeśli w stronach jest ....
howmanypages <- as.numeric(tekst)

if(is.na(howmanypages) ) {  #jeśli nie wyszło, to inaczej
  tekst = html_text(html_node(page, ".pager , .lheight24 span")) 
  tekst = gsub("[\t\n]", " ", tekst)
  howmanypages = max(na.omit((as.numeric(unique(unlist(strsplit(tekst, " ")))))))
}

prices <- list()
currpagen = matrix(nrow = 10)

for (i in 1:howmanypages)
{
  suffix <- paste("&page=",i, sep = "") 
  print(suffix)
  currpage= html_text(html_nodes (read_html(paste(url,suffix, sep = "")), ".price strong" ))
  for(j in 1:length(currpage))  {
    currpagen[j] = as.numeric( gsub("[^0-9]", "", currpage[j]))
  }
 prices[length(prices)+1] <- list(currpagen)
}

print ( paste( "droższych niż threshold jest: " , length(which(prices>threshold))/length(prices) *100 , "procent" )  )

prices= unlist(prices)
summary(prices)
plot(table(prices))
hist(prices, breaks = 15)
