#importing dataset
library(readr)

#BX_Book_Ratings
BX_Book_Ratings <- read_csv("~/R/good_reads/good_reads/BX-Book-Ratings.csv")
View(BX_Book_Ratings)

#BX_Books
BX_Books <- read_csv("~/R/good_reads/good_reads/BX-Books.csv")
View(BX_Books)

#Bx-Users
BX_Users <- read_csv("~/R/good_reads/good_reads/BX-Users.csv")
View(BX_Users)

#Creating data frames
df<-as.data.frame(BX_Book_Ratings)

df2<-as.data.frame(BX_Users)

df3<-as.data.frame(BX_Books)

#Analysis
summary(df)
summary(df2)
summary(df3)


hist(df$`Book-Rating`)
hist(df2$Age,xlim = 60)
hist(df3$`Year-Of-Publication`,xlim = c(1850,2010),ylim = c(0,500000))

#imputation library for further analysis
require(mice)
#VIM plots(Visualization and Imputation of Missing Value)
require(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(df3), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

md.pattern(df)
md.pattern(df2)
md.pattern(df3)

#pairs of all the missing data r-available m-unavailable
md.pairs (df)
md.pairs (df2)
md.pairs (df3)

#Outlier Analysis Whiskers Plots
boxplot(df$`Book-Rating`)
boxplot(df2$Age)
boxplot(df3$`Year-Of-Publication`)

pbox(df,pos=1,int=FALSE,cex=0.7)
pbox(df2,pos=1,int=FALSE,cex=0.7)
pbox(df3,pos=1,int=FALSE,cex=0.7)

imp <- mice(df)
imp2<-mice (df2)
imp3<-mice (df3)

miss1<-imp$imp$`Book-Rating`
miss2<-imp2$imp$Age
miss3<-imp3$imp$`Year-Of-Publication`

fd<-complete(imp)
fd2<-complete(imp2)
fd3<-complete(imp3)

View(fd)

#manipulation of col name in fd2
colnames(fd2)[1] <-"User-ID"

#inner join
total <- merge(fd, fd3,by="ISBN")
total <- merge(total,fd2,by="User-ID")

#Assosciation Rules
library (arules)
data.raw<-NULL
data.raw<-total
rules.all <- apriori(data.raw)
fdr.raw <- as.data.frame(fd)
rules.all
inspect(rules.all)
