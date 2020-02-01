#importing libraries
library(tidyverse)
library(tidyr)
library(tidytext)
library(knitr)

clothes <- read_csv(("Womens_Clothing_Reviews.csv"))
clothes <- clothes[-1]
colnames(clothes) <-  c('ID', 'Age', 'Title', 'Review', 'Rating', 'Recommend', 'Liked', 'Division', 'Dept', 'Class')


glimpse(clothes)
print("no of rows")
print(nrow(clothes))

#clothes1 for nlp tasks , Just for fun :D
clothes1<-clothes%>%select('ID','Age','Title','Review', 'Rating', 'Recommend', 'Dept')
clothes<-clothes%>%select('ID','Age','Rating', 'Recommend', 'Dept')
##total no of na values
clothes%>% summarize_all(list(~sum(is.na(.))))

clothes<-clothes%>%drop_na()
clothes1<-clothes1%>%drop_na()
print(nrow(clothes))
## we can see that rows of new df decreases by removing NA values

unique(clothes["Dept"])


str(clothes)
head(clothes)


p <-ggplot(clothes, aes(Rating))
p +geom_bar(color = "black", fill = "light blue")
#bar plot of rating dist

summary(clothes$Rating)

Rating_dist <- clothes %>% 
  group_by(Rating) %>% 
  summarise(
    n = n()
  )
Rating_dist%>%mutate(proportion=n/sum(n)  )



p <-ggplot(clothes, aes(Recommend))
p +geom_bar(color = "black", fill = "yellow")
#bar plot of recommend dist

Recommend_dist <- clothes %>% 
  group_by(Recommend) %>% 
  summarise( n = n())
Recommend_dist%>%mutate(proportion=n/sum(n))


p <-ggplot(clothes, aes(x=Age)) 
p +geom_histogram(aes(y=..density..),color = "black", fill = "orange")+geom_density() + ggtitle("Age Distribuition")

ages <- clothes %>% select(ID, Age, Rating) %>%
mutate(Age_group = ifelse(Age<=25, '25 and under',ifelse(Age >= 26&Age <= 35, '26-35', ifelse(Age >= 36&Age <= 45, '36-45', ifelse(Age >= 46&Age <= 64, '46-64','65+' ))))) 
#ages
summary(ages$Age)
head(ages%>%group_by(Age_group)%>%  summarise( n = n() )%>%mutate(proportion=n/sum(n)) )%>%kable(.)

cor(clothes[, which(sapply(clothes, is.numeric))],
    use = "pairwise.complete.obs")


clothes%>%select(Rating,Recommend)%>%table(.)


library(ggmosaic)
ggplot(clothes%>%mutate(Rating=factor(Rating), Recommend=factor(Recommend))) + 
geom_mosaic(aes(x=product(Rating, Recommend),fill=Recommend))+xlab("Recommended")+ylab("Rating")+coord_flip()


prop.table(table(clothes$Dept))

ggplot(data.frame(prop.table(table(clothes$Dept))), aes(x=Var1, y = Freq*100,fill=Var1)) + geom_bar(stat = 'identity')+
xlab('Department Name') + ylab('Percentage of Reviews/Ratings (%)') + 
geom_text(aes(label=round(Freq*100,2)), vjust=-0.25) + 
ggtitle('Percentage of product By Department')


##making age groups as told in question 2 
ages <- clothes %>% 
mutate(Age_group = ifelse(Age<=25, '>=25',ifelse(Age >= 26&Age <= 35, '26-35', ifelse(Age >= 36&Age <= 45, '36-45', ifelse(Age >= 46&Age <= 64, '46-64','65+' ))))) 
#ages
ages <- ages %>% mutate(Age_group = factor(Age_group),Rating=factor(Rating))



#proportion of agegroup and ratings
ages_mod <- clothes %>% select(ID, Age, Rating) %>%
mutate(Age_group = ifelse(Age<=25, '>=25',ifelse(Age >= 26&Age <= 35, '26-35', ifelse(Age >= 36&Age <= 45, '36-45', ifelse(Age >= 46&Age <= 64, '46-64','65+' ))))) 
ages_mod<-ages%>%group_by(Rating,Age_group)%>%count()%>%ungroup() %>%mutate(prop=n/sum(n))%>%arrange(desc(n))
head(ages_mod)


ages_mod%>%ggplot(aes(x=Age_group, fill =Age_group )) + 
geom_bar(stat='identity',aes(y=prop)) + facet_wrap(~Rating, scales = 'free' ) +  
ylab('proportion of ratings') 

#facet wrap by age and look at Dept distribution in each
ages <- clothes %>% select(ID, Age, Dept) %>%
mutate(Age_group = ifelse(Age<=25, '>=25',ifelse(Age >= 26&Age <= 35, '26-35', ifelse(Age >= 36&Age <= 45, '36-45', ifelse(Age >= 46&Age <= 64, '46-64','65+' ))))) 

ages <- ages %>% mutate(Age_group = factor(Age_group), Dept = factor(Dept, levels = rev(c('Trend','Tops', 'Dresses', 'Bottoms', 'Intimate', 'Jackets'))))
ages %>% group_by(Age_group) %>% count(Dept) %>% ggplot(aes(Dept, n, fill = Age_group)) + 
geom_bar(stat='identity', show.legend = FALSE) + facet_wrap(~Age_group, scales = 'free') + xlab('Department') + 
ylab('Number of Reviews') + geom_text(aes(label = n), hjust = 1) + scale_y_continuous(expand = c(.1, 0)) + coord_flip() 


#ratings percentage by Department
 x<- clothes  %>% mutate(Dept = factor(Dept)) %>% group_by(Dept) %>% count(Rating) %>% mutate(prop = n/sum(n))
x%>% ggplot(aes(x=Rating, y = prop*100, fill = Dept)) + geom_bar(stat = 'identity', show.legend = FALSE) + facet_wrap(~Dept) + ylab('Percentage of ratings %') + geom_text(aes(label=round(prop*100,2)), vjust = -.2) + scale_y_continuous(limits = c(0,65))

clothes%>%
group_by(Age_group) %>%
summarise(Avg_Rating = mean(Rating),
Median_Rating= median(Rating))

## studying reviews using most frequent bigrams 
clothesr <- clothes1 
#clothesr
notitle <- clothesr %>% select(-Title)
#notitle
wtitle <- clothesr %>%unite(Review, c(Title, Review), sep = ' ')
#wtitle
##combing them
main <- bind_rows(notitle, wtitle)
#main

# removing unnecessary stop words because they are junk and spoil our analyses
# finding the most frequently occuring bigrams, 2words (excluding stopwords)
bigramming <- function(data){
  cbigram <- data %>% unnest_tokens(bigram, Review, token = 'ngrams', n = 2)
    print(cbigram)
  cbigram_sep <- cbigram %>% separate(bigram, c('first', 'second'), sep = ' ')
        print(cbigram_sep)

  cbigram2 <- cbigram_sep %>% 
    filter(!first %in% stop_words$word, !second %in% stop_words$word, !str_detect(first,      '\\d'), !str_detect(second, '\\d')) %>%
    unite(bigram, c(first, second), sep = ' ') 
    print(cbigram2)
  return(cbigram2)
}


top_bigrams <- bigramming(main) %>% mutate(Rating = factor(Rating, levels <- c(5:1))) %>% mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% group_by(Rating) %>% count(bigram, sort=TRUE) %>% top_n(10, n) %>% ungroup() 

top_bigrams  %>% ggplot(aes(bigram, n, fill = Rating)) + geom_col(show.legend = FALSE) + facet_wrap(~Rating, ncol = 3, scales = 'free') + labs(x=NULL, y = 'frequency') + ggtitle('Most Common Bigrams (By Ratings)') + coord_flip()

prod_high_rating <- clothes %>% group_by(ID,Dept) %>% summarise(count=n(),Avg_rating = mean(Rating),tot_no_of_reviews=sum(count),
                                                                prop_positive_recommendations=sum(Recommend)/tot_no_of_reviews)%>% arrange(desc(Avg_rating))

head(prod_high_rating,10)

prod_high_rating_1 <- clothes %>% group_by(ID) %>% summarise(count=n(),Avg_rating = mean(Rating),tot_no_of_reviews=sum(count),
                                                                prop_positive_recommendations=sum(Recommend)/tot_no_of_reviews)%>% arrange(desc(prop_positive_recommendations))


head(prod_high_rating_1,10)

a <- function(count){((1.96)^2)/count}
b <-  function(prop_positive_recommendations,count){(((prop_positive_recommendations)*(1-prop_positive_recommendations)))/count}
c_fun <- function(count,a){a/(2*count)}
WCL <- function(prop_positive_recommendations,a,b,c){(prop_positive_recommendations+a - 1.96*sqrt(b+c))/(1+2*a)}                  

a<-a(prod_high_rating$count)
b<-b(prod_high_rating$prop_positive_recommendations,prod_high_rating$count)
c<-c_fun(prod_high_rating$count,a)


WLCL<-WCL(prod_high_rating$prop_positive_recommendations,a,b,c)

prod_high_rating<-prod_high_rating%>%ungroup()%>% mutate(WLCL=WCL(prod_high_rating$prop_positive_recommendations,a,b,c)) %>% arrange(desc(WLCL))
head(prod_high_rating,10)
