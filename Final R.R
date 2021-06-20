library(plyr)
library(purrr)
library(pastecs)
library(dplyr)
library(psych)
library(purrr)
library(sqldf)
library(ggplot2)
library(GGally)
library(igraph)
library(MatchIt)
library(gridExtra)
library(tableone)
library(corrplot)          
library(ggExtra)
library(car)
library(pROC)

# Import data
# Set working directory for HighNote Data Midterm
setwd("C:/Data PSA/UCI/Winter 2021/BANA 277 customer analytics/midterm")
data <- read.csv("HighNote Data Midterm.csv")

# checking headers and dimensions of data
head(data)
print('Dimension of data is')
print(paste('Row Count:',dim(data)[1]))
print(paste('Column Count:',dim(data)[2]))

# check if any nil/NA values
sum(is.na(data))

# Descriptive statistics
abc<- data[data$adopter==1,c(2:16)] 
paid<-psych::describe(abc,skew=FALSE)
abc<- data[data$adopter==0,c(2:16)] 
free<-psych::describe(abc,skew=FALSE)
print("For Adopter =1")
paid
print("For Adopter =0")
free
#data[data$adopter==0,-1] %>% psych::describe(,skew=FALSE,mat=TRUE)

#Analyze the differences in the mean values of the variables, comparing the paid and free users

lapply(data[,c('age','male' , 'friend_cnt' , 'avg_friend_male' ,'avg_friend_age', 
                   'friend_country_cnt'  , 'songsListened' , 'lovedTracks' , 
                   'posts' , 'playlists' ,'shouts' , 'tenure' ,'good_country', 'subscriber_friend_cnt')], function(i) t.test(i ~ data$adopter))

#assign ID attribute to NULL, it is an identity and will not be used for further analysis or modelling 
data$ID<- NULL

# assign a new data frame for further computation, we will be factorising variables
data_desc<- data

#Make adopter variable a String Factor to help visualize how adopters and non-adopters differ from each other.
data_desc$paid <- ifelse(data_desc$adopter==1,'Paid','Free')
table(data_desc$paid)

#2. Data Visualization (i) demographics
#age
data_desc$Adopter <- factor(data_desc$adopter, levels = c(0, 1))
means <- ddply(data_desc, "Adopter", summarise, age.mean = mean(age))
means

ggplot(data_desc, aes(x = age, group = Adopter, fill = Adopter)) +
  geom_density(alpha = .3) + 
  geom_vline(data = means, aes(xintercept = age.mean, colour = Adopter),
             linetype = "longdash", size=1)

#male
data_desc$Male <- factor(data_desc$male, levels = c(0, 1))
data_desc %>% group_by(Adopter, Male) %>% tally() %>%
  ggplot(aes(Male, n, group = Adopter, fill = Adopter)) + geom_bar(position = "dodge", stat = "identity") + labs(x = 'male', y = 'count', title = 'Male Count by Adopter')

#good country
data_desc$Good_country <- factor(data_desc$good_country, levels = c(0, 1))
data_desc %>% group_by(Adopter, Good_country) %>% tally() %>%
  ggplot(aes(Good_country, n, group = as.factor(Adopter), fill = Adopter)) + geom_bar(position = "dodge", stat = "identity") + labs(x = 'good_country', y = 'count', title = 'Count of Good Country by Adopter')

# Box Plots for Demographics 
data_desc$paid=as.factor(data_desc$paid)
data_desc$male=as.factor(data_desc$male)
data_desc$good_country=as.factor(data_desc$good_country)
print("Box Plot for Demographic understanding to help visualize how adopters and non-adopters perform")
require(gridExtra)
xyz1 <- ggplot(data_desc, aes(x=paid, y=age)) + geom_boxplot(aes(fill=paid),outlier.colour="blue", outlier.shape=8,outlier.size=1)
xyz1 <- xyz1 + scale_fill_manual(values=c("#FF4500", "#32CD32")) +theme(legend.position="none")+theme(axis.title.x=element_blank())
xyz2<- ggplot(data_desc, aes(x=age)) + geom_bar()
xyz3<- ggplot(data_desc, aes(x=age)) + geom_density(kernel='gaussian')

data1<-data_desc[data_desc$male==0,]
xyz4 <- ggplot(data1, aes(x=paid)) +labs(title = "Female Users")+ geom_bar(aes(fill=paid) )+theme(axis.title.x=element_blank())+ scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")
data2<-data_desc[data_desc$male==1,]
xyz5 <- ggplot(data2, aes(x=paid)) +labs(title = "Male Users")+ geom_bar(aes(fill=paid) )+theme(axis.title.x=element_blank())+ scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none") #geom_bar(stat="identity") #geom_histogram()
xyz6 <- ggplot(data_desc, aes(paid))
xyz6 <- abc6  + geom_bar(aes(fill = male))+ scale_fill_manual(values=c("#808080", "#FFD700"))+theme(axis.title.x=element_blank())


data1<-data_desc[data_desc$good_country==0,]
xyz7 <- ggplot(data1, aes(x=paid)) +labs(title = "Others")+ geom_bar(aes(fill=paid) )+theme(axis.title.x=element_blank())+ scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")
data2<-data_desc[data_desc$good_country==1,]
xyz8 <- ggplot(data2, aes(x=paid)) +labs(title = "Good Country")+ geom_bar(aes(fill=paid) ) +theme(axis.title.x=element_blank())+ scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")#geom_bar(stat="identity") #geom_histogram()
data_descx=mutate(data_desc,country=good_country)
xyz9 <- ggplot(data_descx, aes(paid))
xyz9 <- xyz9  + geom_bar(aes(fill = country))+ scale_fill_manual(values=c("#808080", "#FFD700"))+theme(axis.title.x=element_blank())

grid.arrange(xyz1,xyz2,xyz3,xyz4,xyz5,xyz6,xyz7,xyz8,xyz9, ncol=3)

#Age and friend count details by country type
labs <- paste("Country Type:", c("Good", "Others"))
labs2 <- paste(c("Paid User", "Free User"))
data %>% mutate(good_country = ifelse(good_country == 1, labs[1], labs[2]))%>%
  mutate(adopter = ifelse(adopter == 1, labs2[1], labs2[2]))%>%
  ggplot(aes(x=age, y=friend_cnt,shape=adopter, color=adopter)) + geom_point(size=2)+
  facet_wrap(~good_country)+ylim(0,2500)

# Box plot for peer influence
xyz2 <- ggplot(data_desc, aes(x=paid, y=friend_cnt)) + geom_boxplot(aes(fill=paid),outlier.colour="blue", outlier.shape=8,outlier.size=1)
xyz2 <- xyz2 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$friend_cnt, c(0.1, 0.9)))+theme(axis.title.x=element_blank())
xyz3 <- ggplot(data_desc, aes(x=paid, y=avg_friend_age)) + geom_boxplot(aes(fill=paid),outlier.colour="blue", outlier.shape=8,outlier.size=1)
xyz3 <- xyz3 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$avg_friend_age, c(0.1, 0.9)))+theme(axis.title.x=element_blank())
xyz4 <- ggplot(data_desc, aes(x=paid, y=avg_friend_male)) + geom_boxplot(aes(fill=paid),outlier.colour="blue", outlier.shape=8,outlier.size=1)
xyz4 <- xyz4 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$avg_friend_male, c(0.1, 0.9)))+theme(axis.title.x=element_blank())
xyz5 <- ggplot(data_desc, aes(x=paid, y=friend_country_cnt)) + geom_boxplot(aes(fill=paid),outlier.colour="blue", outlier.shape=8,outlier.size=1)
xyz5 <- xyz5 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$friend_country_cnt, c(0.1, 0.9)))+theme(axis.title.x=element_blank())
xyz6 <- ggplot(data_desc, aes(x=paid, y=subscriber_friend_cnt)) + geom_boxplot(aes(fill=paid),outlier.colour="blue", outlier.shape=8,outlier.size=1)
xyz6 <- xyz6 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$subscriber_friend_cnt, c(0.1, 0.9)))+theme(axis.title.x=element_blank())
grid.arrange(xyz2,xyz3,xyz4,xyz5,xyz6, ncol=3)

labs <- paste("Gender:", c("Male", "Female"))
labs2 <- paste(c("Paid User", "Free User"))
data %>% mutate(male = ifelse(male == 1, labs[1], labs[2]))%>%
  mutate(adopter = ifelse(adopter == 1, labs2[1], labs2[2]))%>%
  ggplot(aes(x=songsListened, y=lovedTracks,shape=adopter, color=adopter)) + geom_point(size=2)+
  facet_wrap(~male)#+ylim(0,100)

labs <- paste("Gender:", c("Male", "Female"))
labs2 <- paste(c("Paid User", "Free User"))
data %>% mutate(male = ifelse(male == 1, labs[1], labs[2]))%>%
  mutate(adopter = ifelse(adopter == 1, labs2[1], labs2[2]))%>%
  ggplot(aes(x=songsListened, y=playlists,shape=adopter, color=adopter)) + geom_point(size=2)+
  facet_wrap(~male) #+ylim(0,150)

print("Box Plot for User Engagement Influence understanding to help visualize how adopters and non-adopters perform")
xyz7 <- ggplot(data_desc, aes(x=paid, y=songsListened)) + geom_boxplot(aes(fill=paid),outlier.colour="blue", outlier.shape=8,outlier.size=1)
xyz7 <- xyz7 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$songsListened, c(0.1, 0.9)))+theme(axis.title.x=element_blank())
xyz8 <- ggplot(data_desc, aes(x=paid, y=lovedTracks)) + geom_boxplot(aes(fill=paid),outlier.colour="blue", outlier.shape=8,outlier.size=1)
xyz8 <- xyz8 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$lovedTracks, c(0.1, 0.9)))+theme(axis.title.x=element_blank())
xyz9 <- ggplot(data_desc, aes(x=paid, y=posts)) + geom_boxplot(aes(fill=paid),outlier.colour="blue", outlier.shape=8,outlier.size=1)
xyz9 <- xyz9 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$posts, c(0.1, 0.9)))+theme(axis.title.x=element_blank())
xyz10 <- ggplot(data_desc, aes(x=paid, y=playlists)) + geom_boxplot(aes(fill=paid),outlier.colour="blue", outlier.shape=8,outlier.size=1)
xyz10 <- xyz10 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$playlists, c(0, 0.95)))
xyz11 <- ggplot(data_desc, aes(x=paid, y=shouts)) + geom_boxplot(aes(fill=paid),outlier.colour="blue", outlier.shape=8,outlier.size=1)
xyz11 <- xyz11 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$shouts, c(0.1, 0.9)))
xyz12 <- ggplot(data_desc, aes(x=paid, y=tenure)) + geom_boxplot(aes(fill=paid),outlier.colour="blue", outlier.shape=8,outlier.size=1)
xyz12 <- xyz12 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$tenure, c(0.1, 0.9)))
grid.arrange(xyz7,xyz8,xyz9,xyz10,xyz11,xyz12, ncol=3)

# Peer influence
#mean value of peer influence variables
col <- c(colnames(data_desc)[4:8], 'adopter')
peer_mean<- data_desc %>%
  group_by(adopter) %>%
  select(one_of(col)) %>%
  summarise_all(funs(mean))

#distribution 
ggpairs(data_desc[4:8])


# Plot variation on parameters in user engagement
pairs(data_desc[,c(8:12,14)])

data <- mutate(data,treatment=ifelse(data$subscriber_friend_cnt >=1,1,0))
data %>%
  group_by(adopter) %>% summarise(mean_treatment = mean(treatment),users=n())

with(data, t.test(treatment ~ adopter))

#Propensity Score Matching (PSM)to test whether having subscriber friends affects the likelihood of becoming an adopter (i.e. paid customer).
#split treatment and control group
data_desc <- read.csv("HighNote Data Midterm.csv")
data_desc<- mutate(data_desc, treatment = ifelse(subscriber_friend_cnt>=1, 1, 0))
data_desc %>%
  group_by(adopter) %>% summarise(mean_treatment = mean(treatment),users=n())

#run t-test before PSM
with(data_desc, t.test(adopter ~ treatment)) #c-t: 0.052 - 0.178
# Significance for all covariants
data_desc %>%
  group_by(treatment) %>%
  summarise_all(funs(mean(., na.rm = T)))

# Calculate t-test for each covariate by treatment status
labels <- c('age','male','friend_cnt','avg_friend_age','avg_friend_male','friend_country_cnt','songsListened','lovedTracks','posts','playlists','shouts','tenure','good_country')

lapply(labels, function(v) {
  t.test(data_desc[, v] ~ data_desc$treatment)
})


#logistic regression for PSM 
logit_highnote_data <- glm(treatment ~log(age) + male +log(friend_cnt+1) + log(avg_friend_age+1)  + log(avg_friend_male+1) +
              log(friend_country_cnt+1) + log(songsListened+1) + log(lovedTracks+1) + log(posts+1) +
              log(playlists+1) + log(shouts+1) + log(tenure+1) + good_country, family = binomial(), data = data_desc)
summary(logit_highnote_data)

#predicted propensity score
prs_highnote_data <- data.frame(pr_score = predict(logit_highnote_data, type = "response"),
                                treatment = logit_highnote_data$model$treatment)
head(prs_highnote_data)

#plot histogram of treatment status to examine the region of common support
labs <- paste("Actual treatment type:", c("Treatment w/ subscriber friends", "Control w/n subscriber friends"))
prs_highnote_data %>%
  mutate(treatment = ifelse(treatment == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~treatment) +
  xlab("Probability of getting subscriber friends") +
  theme_bw()

#prematching table
xvars <- colnames(data_desc[!colnames(data_desc) %in% c('ID', 'subscriber_friend_cnt', 'adopter', 'treatment')])
table1 <- CreateTableOne(vars = xvars, strata = "treatment", data = data_desc, test = FALSE)
print(table1, smd = TRUE)

#check null value and omit
sum(is.na(data_desc)) #no null value

# Since no null value, now we do matchit
mod_match <- matchit(treatment ~ log(age) + male +log(friend_cnt+1) + log(avg_friend_age+1)  + log(avg_friend_male+1) + log(friend_country_cnt+1) + log(songsListened+1) + log(lovedTracks+1) + log(posts+1) + log(playlists+1) + log(shouts+1) + log(tenure+1) + good_country, method = 'nearest', data = data_desc)
summary(mod_match, covariates = T)

#plot(mod_match)
plot(mod_match)

# Create a dataframe to save match data using match.data function  
dta_m <- match.data(mod_match)
print("Dimensions of the matched data frame")
dim(dta_m)
print("Table of Treatment distriution in the matched data frame")
table(dta_m$treatment)

#create a TableOne to see Standardized Mean Difference
match_table <- CreateTableOne(vars = xvars, strata = "treatment", data = dta_m, test = FALSE)
print(match_table, smd = TRUE)

#Method to see mean difference
df_colnames <- names(dta_m)[c(-1, -8, -14, -17, -18, -19)]
dta_m %>%
  group_by(treatment) %>%
  select(one_of(df_colnames)) %>%
  summarise_all(funs(mean))

#t=test
output = NULL
for (i in 1:13){
  name = df_colnames[i]
  p_value = t.test(dta_m[, df_colnames[i]] ~ dta_m$treatment)$p.value
  output <- rbind(output, data.frame(name, p_value))
}
print(output) #all significant

# Estimating treatment effects
with(dta_m, t.test(adopter ~ treatment))

# Visual inspection to examine covariance balance in matched sample
fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  dta$treatment <- as.factor(dta$treatment)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = treatment)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

grid.arrange(
  fn_bal(dta_m, "age"),
  fn_bal(dta_m, "male") + theme(legend.position = "none"),
  fn_bal(dta_m, "friend_cnt"),
  fn_bal(dta_m, "avg_friend_age") + theme(legend.position = "none"),
  fn_bal(dta_m, "avg_friend_male"),
  fn_bal(dta_m, "songsListened") + theme(legend.position = "none"),
  fn_bal(dta_m, "lovedTracks"),
  fn_bal(dta_m, "posts") + theme(legend.position = "none"),
  fn_bal(dta_m, "playlists"),  
  fn_bal(dta_m, "shouts") + theme(legend.position = "none"),
  fn_bal(dta_m, "tenure"),
  fn_bal(dta_m, "good_country") + theme(legend.position = "none"),
  nrow = 7)

#check null value and remove
sum(is.na(data_desc)) #no null value

#subclass matching approach using subclass method
mod_match2 <- matchit(treatment ~ log(age) + male +log(friend_cnt+1) + log(avg_friend_age+1)  + log(avg_friend_male+1) + log(friend_country_cnt+1) + log(songsListened+1) + log(lovedTracks+1) + log(posts+1) + log(playlists+1) + log(shouts+1) + log(tenure+1) + good_country, method = 'subclass', data = data_desc)
mod_match2
summary(mod_match2, covariates = T)

#plot(mod_match2)

dta_m2 <- match.data(mod_match2)
dta_m2 <- filter(dta_m2, subclass == 2)

#create a TableOne to see Standardized Mean Difference
match_table <- CreateTableOne(vars = xvars, strata = "treatment", data = dta_m2, test = FALSE)
print(match_table, smd = TRUE)

#method to see mean difference

df_colnames <- names(dta_m2)[c(-1, -8, -14, -17, -18, -19)]
dta_m2 %>% 
  group_by(treatment) %>%
  select(one_of(df_colnames)) %>%
  summarise_all(funs(mean))

#t=test
output = NULL
for (i in 1:13){
  name = df_colnames[i]
  p_value = t.test(dta_m2[, df_colnames[i]] ~ dta_m2$treatment)$p.value
  output <- rbind(output, data.frame(name, p_value))
}
print(output) 

# Visual inspection to examine covariance balance in matched sample
fn_bal <- function(dta_m2, variable) {
  dta_m2$variable <- dta_m2[, variable]
  dta_m2$treatment <- as.factor(dta_m2$treatment)
  support <- c(min(dta_m2$variable), max(dta_m2$variable))
  ggplot(dta_m2, aes(x = distance, y = variable, color = treatment)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

grid.arrange(
  fn_bal(dta_m2, "age"),
  fn_bal(dta_m2, "male") + theme(legend.position = "none"),
  fn_bal(dta_m2, "friend_cnt"),
  fn_bal(dta_m2, "avg_friend_age") + theme(legend.position = "none"),
  fn_bal(dta_m2, "avg_friend_male"),
  fn_bal(dta_m2, "friend_country_cnt") + theme(legend.position = "none"),
  fn_bal(dta_m2, "subscriber_friend_cnt"),
  fn_bal(dta_m2, "songsListened") + theme(legend.position = "none"),
  nrow = 4, widths = c(10, 8)
)

fn_bal <- function(dta_m2, variable) {
  dta_m2$variable <- dta_m2[, variable]
  dta_m2$treatment <- as.factor(dta_m2$treatment)
  support <- c(min(dta_m2$variable), max(dta_m2$variable))
  ggplot(dta_m2, aes(x = distance, y = variable, color = treatment)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

grid.arrange(
  fn_bal(dta_m2, "lovedTracks"),
  fn_bal(dta_m2, "posts") + theme(legend.position = "none"),
  fn_bal(dta_m2, "playlists"),
  fn_bal(dta_m2, "shouts") + theme(legend.position = "none"),
  fn_bal(dta_m2, "adopter"),
  fn_bal(dta_m2, "tenure") + theme(legend.position = "none"),
  fn_bal(dta_m2, "good_country"),
  nrow = 4, widths = c(10, 8)
)

#putting log data back to data

dta_m = mutate(dta_m, age_log = log(dta_m[,'age']+1))
dta_m = mutate(dta_m, male_new = dta_m[,'male'])
dta_m = mutate(dta_m, friend_cnt_log = log(dta_m[,'friend_cnt'])+1)
dta_m = mutate(dta_m, avg_friend_age_log = log(dta_m[,'avg_friend_age']+1))
dta_m = mutate(dta_m, avg_friend_male_log = log(dta_m[,'avg_friend_male']+1))
dta_m = mutate(dta_m, friend_country_cnt_log = log(dta_m[,'friend_country_cnt']+1))
dta_m = mutate(dta_m, songsListened_log = log(dta_m[,'songsListened']+1))
dta_m = mutate(dta_m, lovedTracks_log = log(dta_m[,'lovedTracks']+1))
dta_m = mutate(dta_m, posts_log = log(dta_m[,'posts']+1))
dta_m = mutate(dta_m, playlists_log = log(dta_m[,'playlists']+1))
dta_m = mutate(dta_m, shouts_log = log(dta_m[,'shouts']+1))
dta_m = mutate(dta_m, tenure_log = log(dta_m[,'tenure']+1))
dta_m = mutate(dta_m, good_country_new = dta_m[,'good_country'])


dta_m2 = mutate(dta_m2, age_log = log(dta_m2[,'age']+1))
dta_m2 = mutate(dta_m2, male_new = dta_m2[,'male'])
dta_m2 = mutate(dta_m2, friend_cnt_log = log(dta_m2[,'friend_cnt'])+1)
dta_m2 = mutate(dta_m2, avg_friend_age_log = log(dta_m2[,'avg_friend_age']+1))
dta_m2 = mutate(dta_m2, avg_friend_male_log = log(dta_m2[,'avg_friend_male']+1))
dta_m2 = mutate(dta_m2, friend_country_cnt_log = log(dta_m2[,'friend_country_cnt']+1))
dta_m2 = mutate(dta_m2, songsListened_log = log(dta_m2[,'songsListened']+1))
dta_m2 = mutate(dta_m2, lovedTracks_log = log(dta_m2[,'lovedTracks']+1))
dta_m2 = mutate(dta_m2, posts_log = log(dta_m2[,'posts']+1))
dta_m2 = mutate(dta_m2, playlists_log = log(dta_m2[,'playlists']+1))
dta_m2 = mutate(dta_m2, shouts_log = log(dta_m2[,'shouts']+1))
dta_m2 = mutate(dta_m2, tenure_log = log(dta_m2[,'tenure']+1))
dta_m2 = mutate(dta_m2, good_country_new = dta_m2[,'good_country'])


#mean differences of two matching methods 
cols <- colnames(dta_m)[20:32]

#1
match_table <- CreateTableOne(vars = cols, strata = "treatment", data = dta_m, test = FALSE)
print(match_table, smd = TRUE)

#
#dta_m %>%
#  group_by(treatment) %>%
#  select(one_of(cols)) %>%, #  summarise_all(funs(mean))
#2
match_table <- CreateTableOne(vars = cols, strata = "treatment", data = dta_m2, test = FALSE)
print(match_table, smd = TRUE)

#4th part
# Logistic regression approach to test which variables are significant for explaining the likelihood of becoming an adopter. 
model_test1 <- glm(adopter ~ treatment, data = dta_m2,family='binomial')
summary(model_test1)
exp(coef(model_test1))


#Build a glm model with the treatment variable and other covariates to predict the adopter status
model_test2<- lm(adopter~  log(age) + male +log(friend_cnt+1) + log(avg_friend_age+1)  + log(avg_friend_male+1) +
           log(friend_country_cnt+1) + log(subscriber_friend_cnt + 1) + log(songsListened+1) + log(lovedTracks+1) + log(posts+1) +log(playlists+1) + log(shouts+1) + log(tenure+1) + good_country, data = dta_m2, family = binomial())
summary(model_test2)
exp(coef(model_test2))

# Build a glm model only with the significant predictors
model_test3<-glm(adopter~ log(age) + male + log(avg_friend_age+1)  + log(subscriber_friend_cnt + 1) + log(songsListened+1) + log(lovedTracks+1) + log(posts+1) +log(playlists+1) + log(shouts+1) + log(tenure+1) + good_country, data = dta_m2, family = binomial())
summary(model_test3)
exp(coef(model_test3))


# Build a glm model with the same significant predictors from glm_model_3 but with the original dataset which has not been matched by propensity score
model_test4<-glm(adopter~ log(age) + male + log(avg_friend_age+1)  + log(subscriber_friend_cnt + 1) + log(songsListened+1) + log(lovedTracks+1) + log(posts+1) +log(playlists+1) + log(shouts+1) + log(tenure+1) + good_country, data = data_desc, family = binomial())
summary(model_test4)
exp(model_test4$coefficients)



