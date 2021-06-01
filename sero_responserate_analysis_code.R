# load data csv file

library("RCurl")

csvfile <- getURL("https://raw.githubusercontent.com/harlanhappydog/COVID_sero_responserates/main/Anschutz_Response_Rate_View.csv")
RR_data <- read.csv(text = csvfile)

RR_data <- RR_data[c(match(unique(RR_data[,"URL"]), RR_data[,"URL"])),]


RR_data[,"url1"] <- unlist(((apply(cbind(1:length(RR_data[,"URL"])) , 1, function(z) {
	urlvec<-rep("NA",3)
	ll <- unlist(strsplit(as.character(RR_data[z,"URL"]), split=", "))
	urlvec[1:length(ll)]<-sort(ll)
	return(urlvec[1])
	}))))

# restrict to unique studies as identified by url:

RR_data <- RR_data[c(match(unique(RR_data[,"url1"]), RR_data[,"url1"])),]
dim(RR_data)

# restrict to only those studies for which a response rate makes sense:

RR_data1 <- RR_data[!RR_data[,"JBI.9"]%in%c("N/a", "N/A"),]
dim(RR_data1)

# record start date for each study

RR_data1[,"start_date"] <- as.Date(RR_data1[,"Sampling.Start.Date"], "%d-%b-%y")
RR_data1[is.na(RR_data1[,"start_date"]),]

# redefine levels for region

RR_data1[,"region"] <- "Other"

RR_data1[RR_data1[,"Country"] %in%c("The United Kingdom", "Switzerland", "Sweden", "Spain", "Russian Federation", "Romania", "Portugal", "Poland", "Norway", "Netherlands", "Luxembourg", "Liechtenstein", "Jersey", "Italy", "Ireland", "Iceland", "Hungary", "Greece", "Germany", "Georgia", "France", "Finland", "Faroe Islands", "Estonia", "Denmark", "Czechia", "Croatia", "Bulgaria", "Belgium", "Austria", "Armenia", "Andorra") ,"region"] <- "Europe"


RR_data1[RR_data1[,"Country"] %in%c("United States of America") ,"region"] <- "US"


# record the 95% uncertainty interval for the prevalence:

RR_data1[,"IR_lower"] <- apply(cbind(1:length(RR_data1[,"Serum.pos.prevalence..95pct.CI.Lower"])),1, function(x) {as.numeric(unlist(strsplit(as.character(RR_data1[x,"Serum.pos.prevalence..95pct.CI.Lower"]), "%")))[1]})

RR_data1[,"IR_upper"] <- apply(cbind(1:length(RR_data1[,"Serum.pos.prevalence..95pct.CI.Upper"])),1, function(x) {as.numeric(unlist(strsplit(as.character(RR_data1[x,"Serum.pos.prevalence..95pct.CI.Upper"]), "%")))[1]})

# redefine levels for each study

levels(RR_data1[,"Source.Type"])[levels(RR_data1[,"Source.Type"])%in% 
c("Institutional Report", "Institutional Report, Institutional Report", "Institutional Report, News and Media, News and Media", "News and Media", "News and Media, Institutional Report", "News and Media, News and Media", "News and Media, News and Media, News and Media", "News and Media, News and Media, News and Media, News and Media")] <- "News or institutional report"

levels(RR_data1[,"Source.Type"])[levels(RR_data1[,"Source.Type"])%in% 
c("Journal Article (Peer-Reviewed)", "Journal Article (Peer-Reviewed), Journal Article (Peer-Reviewed)", "Journal Article (Peer-Reviewed), Preprint")] <- "Journal article"

levels(RR_data1[,"Source.Type"])[levels(RR_data1[,"Source.Type"])%in% 
c("Preprint", "Preprint, Institutional Report", "Preprint, Journal Article (Peer-Reviewed)")] <- "Preprint"

# record response rate for each study

RR_data1[,"RR"] <- apply(cbind(1:length(RR_data1[,"Response.Rate.."])),1, function(x) {as.numeric(unlist(strsplit(as.character(RR_data1[x,"Response.Rate.."]), "%")))[1]})

## “Paper reports that "The response rate in different strata ranged from 86.9 to 95.9 per cent." We calculate Response rate based on the average of numbers reported in Table 1. “

RR_data1[,"Response.Rate.."][RR_data1[,"Response.Rate.."]  %in% c("86.9 to 95.9")]
RR_data1[,"RR"][RR_data1[,"Response.Rate.."]  %in% c("86.9 to 95.9")]<-92.38

# record sampling method (redefine levels):

RR_data1[,"prob_sampling"] <- RR_data1[,"Sampling.Method"]%in%c("Simplified probability", "Stratified probability")


RR_data1[RR_data1[,"Sampling.Method"]%in%c("Unclear"),"prob_sampling"]<-NA

# record sample size:

RR_data1[,"sample_size"] <- as.numeric(as.character(RR_data1[,"Denominator.Value"]))

# record RR reported TRUE/FALSE

RR_data1[,"reported"]<-!RR_data1[,"JBI.9"]%in%c("Unclear")

### CONSORT DIAGRAM NUMBERS ####:

dim(RR_data1)

# restrict to exclude news reports:

RR0 <- RR_data1[!RR_data1[,"Source.Type"]%in%c("","News or institutional report"),]
dim(RR0)
sum(RR_data1[,"Source.Type"]%in%c("","News or institutional report"))

# restrict to only include Household and community samples:

RR0a <- RR0[RR0[,"Sample.Frame..groups.of.interest."]%in%c("Household and community samples"),]
dim(RR0a)
sum(!RR0[,"Sample.Frame..groups.of.interest."]%in%c("Household and community samples"))

RR1 <- RR0a


# restrict to only that report RR:

RR1a <- RR1[!(RR1[,"JBI.9"]%in%c("Unclear") | is.na(RR1[,"RR"])),]
dim(RR1a)
sum((RR1[,"JBI.9"]%in%c("Unclear") | is.na(RR1[,"RR"])))


# restrict to only studies for which we have RR:

RR2 <- RR1[!is.na(RR1[,"RR"]),]
dim(RR2)

RR_final <- RR2

#write.csv(RR1, "RR1.csv")
#write.csv(RR_final, "RR_final.csv")

#### RESULTS ####


length(unique(RR1[,"Country"]))
table(RR1[,"region"])

table(RR1[,"prob_sampling"])

table(RR1[,"prob_sampling"],RR1[,"reported"])

range(RR_final[RR_final[,"JBI.9"]=="Unclear","RR"])
mean(RR_final[RR_final[,"JBI.9"]=="Unclear","RR"])

range(RR_final[,"RR"])
mean(RR_final[,"RR"])

sum(RR_final[,"RR"]>60)
mean(RR_final[,"RR"]>60)

range(RR_final[RR_final[,"prob_sampling"]==FALSE,"RR"], na.rm=TRUE)
mean(RR_final[RR_final[,"prob_sampling"]==FALSE,"RR"], na.rm=TRUE)

range(RR_final[RR_final[,"prob_sampling"]==TRUE,"RR"], na.rm=TRUE)
mean(RR_final[RR_final[,"prob_sampling"]==TRUE,"RR"], na.rm=TRUE)

RR_final$region<-as.factor(RR_final$region)
RR_final$region<-relevel(RR_final$region, ref="Other")
summary(  lm(RR ~ sample_size+prob_sampling+region, data=RR_final))




### FIGURES ###

RR_final[,"prob_sampling1"]<-NA
RR_final[RR_final[,"prob_sampling"]%in%TRUE,"prob_sampling1"]<-"Probability based sample"
RR_final[RR_final[,"prob_sampling"]%in%FALSE,"prob_sampling1"]<-"Convinience sample"

RR_final[,"sample_size"]

ggplot(data=RR_final, aes(x=RR, y=region, size=sample_size))+ geom_point(aes(size =sample_size, col=prob_sampling1), alpha = 0.5)+  scale_size(range = c(1, 10))+  xlim(0,100) + xlab("Response Rate (%)") + ylab("")+ theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "lightgrey"),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.key = element_rect(colour = NA, fill = NA),
    plot.margin = unit(c(1,0.1,1,0.1), "cm"), legend.position="right")+  labs(title = "",  color = "Sampling Method\n", size="Sample Size") + guides(size = guide_legend(override.aes = list(colour = list("grey","grey","grey"))))+  scale_size_continuous(breaks = c(500,1000,10000))

(na.omit(data.frame(RR_final[,c("RR","IR_lower","IR_upper")])))


manski1 <- cbind(
(RR_final[,"IR_lower"]/100)*(RR_final[,"RR"]/100) + (1-(RR_final[,"RR"]/100))*1,
(RR_final[,"IR_lower"]/100)*(RR_final[,"RR"]/100) + (1-(RR_final[,"RR"]/100))*0,
(RR_final[,"IR_upper"]/100)*(RR_final[,"RR"]/100) + (1-(RR_final[,"RR"]/100))*1,
(RR_final[,"IR_upper"]/100)*(RR_final[,"RR"]/100) + (1-(RR_final[,"RR"]/100))*0
)
RR_final[,"Manski_lower"] <- apply(manski1, 1,function(x) if(!is.na(x)) {min(x)} else{NA})
RR_final[,"Manski_upper"] <- apply(manski1, 1,function(x) if(!is.na(x)) {max(x)} else{NA})




RR_final_ord <- RR_final[order(RR_final[,"RR"]),]
RR_final_ord[,"ID"]<- 1:dim(RR_final_ord)[1]
ggplot(data= RR_final_ord,  aes(y=ID, x= Manski_lower, xmin= Manski_lower, xmax= Manski_upper))+ geom_pointrange(size=0.5, pch="|", alpha=0.35)+  xlim(0,1)+ geom_pointrange(size=0.5, pch="|", alpha=0.75, aes(y=ID, x= IR_lower/100, xmin= IR_lower/100, xmax= IR_upper/100)) + geom_point(aes(y=ID, x= IR_upper/100),  pch="|", size=4, alpha=0.75)+ geom_point(aes(y=ID, x= Manski_upper),  pch="|", size=4, alpha=0.5)+ geom_point(aes(y=ID, x= IR_lower/100),  pch="|", size=4, alpha=0.75)+ geom_point(aes(y=ID, x= Manski_lower),  pch="|", size=4, alpha=0.5)+
xlab("Prevalence (%)") + ylab("Studies in order of increasing RR")+ theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_line(colour = NA),
    panel.grid.minor = element_line(colour = NA),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.key = element_rect(colour = NA, fill = NA))+geom_vline(xintercept=c(0,0.5,1), col="grey")


