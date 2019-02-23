# **************************
# WES cost, payment, remit code  and  duration analysis using Oracle table 1 (WES_BASE_KVN_1_FNL_V22) and table 3 (WES_BASE_KVN_3_V2)
# David Chen, 2018.7.30
# **************************
library(RJDBC)
library(xlsx)
library(tidyr)
library("dplyr")
library("ggpubr")
library("car")
library(VCA)
library(MASS)
library(reshape2)
library(ggplot2)
library(gridExtra)

options(java.parameters = "-Xmx16g")
# set work directory
setwd("")

drv = JDBC("oracle.jdbc.OracleDriver", classPath = "~/ExternalJars/ojdbc7.jar", " ")
# set up connection parameters
con = dbConnect(
  drv,
  "",
  "",
  ""
)

## Section 1: define inclusion set using table 1.
query <- "dummy query string to get data"

full.df = dbGetQuery(con, query)
full.df$HSP_ACCOUNT_ID <- as.character(full.df$HSP_ACCOUNT_ID)
full.df$BUCKET_ID <- as.character(full.df$BUCKET_ID)
full.df$CPT <- as.factor(full.df$CPT)
full.df$FINANCIAL_CLASS_NAME <- as.factor(full.df$FINANCIAL_CLASS_NAME)

str(full.df)

# count the number of different cpts on the patient level

## Section: identify procedural groups on the pat level
cpts.by.pat.df <- as.data.frame(full.df %>% 
                                 group_by(PAT_ID) %>% 
                                 summarise(cpt.cnt = n_distinct(CPT), cpts = paste0(sort(unique(CPT)), collapse = ",")))

table(cpts.by.pat.df$cpts)
# 3860
# remove coexistence of 15/16 and 79
cpts.by.pat.df <- cpts.by.pat.df[!grepl(",81479", cpts.by.pat.df$cpts), ]
table(cpts.by.pat.df$cpts)
str(cpts.by.pat.df)

# add group label
cpts.by.pat.df$group[cpts.by.pat.df$cpts=="81479"] <- "non-wes"
cpts.by.pat.df$group[cpts.by.pat.df$cpts!="81479"] <- "wes"
cpts.by.pat.df$group<- as.factor(cpts.by.pat.df$group)
cpts.by.pat.df$cpts<- as.factor(cpts.by.pat.df$cpts)
master.df <- merge(cpts.by.pat.df, full.df, by = "PAT_ID")
str(master.df)

## Section 2: cost, payment, denial and remit code analysis.
query <- "select distinct HSP_ACCOUNT_ID, BUCKET_ID, TX_ID, PROCEDURE_DESC, TX_AMOUNT, FINANCIAL_CLASS_NAME
from CLAIMSDATA.WES_BASE_KVN_3_V2
where (liability_bucket_type in ('Primary Claim', 'Secondary Claim') AND LIABILITY_BUCKET_STATUS = 'Closed') OR procedure_desc = 'PATIENT - PAYMENT'"

full.df = dbGetQuery(con, query)
str(full.df)
full.df$HSP_ACCOUNT_ID <- as.character(full.df$HSP_ACCOUNT_ID)
full.df$BUCKET_ID <- as.character(full.df$BUCKET_ID)
full.df$TX_ID <- as.character(full.df$TX_ID)

length(unique(full.df$HSP_ACCOUNT_ID))
length(unique(full.df$BUCKET_ID))
length(unique(full.df$TX_ID))

# construct transaction level table using included hsps
hsp.df <- unique(master.df[, c('HSP_ACCOUNT_ID', 'PAT_ID', 'group')])
length(unique(hsp.df$HSP_ACCOUNT_ID))
# 3980
length(unique(hsp.df$PAT_ID))
# 3730
str(hsp.df)
tx.df <- merge(hsp.df, full.df, by = 'HSP_ACCOUNT_ID')
str(tx.df)

# calculate amt based on tx_amount column

amt.df <- tx.df %>% 
              group_by(HSP_ACCOUNT_ID, PAT_ID, group, PROCEDURE_DESC, FINANCIAL_CLASS_NAME) %>% 
              summarise(total.amt = sum(TX_AMOUNT, na.rm=TRUE))

amt.df.wide <- dcast(amt.df, HSP_ACCOUNT_ID + PAT_ID + group + FINANCIAL_CLASS_NAME  ~ PROCEDURE_DESC, value.var = "total.amt")
str(amt.df.wide)

# 1. cost analysis on the patient level

# cost by group
cost.df <- amt.df.wide %>% 
            group_by(PAT_ID, group) %>% 
            summarise(total.cost = sum(`SYSTEM DEBIT`, na.rm=TRUE))
cost.df <- cost.df[cost.df$total.cost<10000 & cost.df$total.cost>0,]
cost.df.stats <- as.data.frame(cost.df %>% group_by(group) %>%
                                  summarise(
                                    count = n()
                                  ))
# reorder by count desc
cost.df.stats <- cost.df.stats[order(-cost.df.stats$count), ]
cost.df <- merge(cost.df, cost.df.stats, by = 'group')
# reorder the fact by count desc
cost.df$group <- reorder(cost.df$group, -cost.df$count)
# create x lables
xlabel <- with(cost.df.stats, paste0(group,"\nn=",count))
ggplot(data = cost.df, aes(
  x=group, y=total.cost)) + 
  scale_x_discrete(labels = xlabel)+
  xlab("group") +
  ylab("cost") +
  geom_boxplot(aes(fill=group))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
        axis.text.y = element_text(size=10), 
        plot.title = element_text(hjust = 0.5),
        legend.position="none")+
  ggtitle("Total Cost by Procedural Group (WES vs non-WES)")+
  coord_flip()

# Use semi-transparent fill
p<-ggplot(cost.df, aes(x=total.cost, fill=group, color=group)) +
  geom_histogram(position="identity", alpha=0.5) +theme(legend.position="bottom")
p
# Add mean lines
mu <- cost.df %>%
  group_by(group) %>%
  summarise(grp.mean=mean(total.cost, na.rm = TRUE))
p+geom_vline(data=mu, aes(xintercept=grp.mean, color=group),
             linetype="dashed")

kruskal.test(total.cost ~ group, data = cost.df)
# Multiple pairwise-comparison between groups
pwt <- as.data.frame(pairwise.wilcox.test(cost.df$total.cost, cost.df$group,
                     p.adjust.method = "BH")$p.value)
pwt <- round(pwt,2 )
pwt[pwt>0.05] <- NA
pwt

# charge to financial class
cost.fc.df <- amt.df.wide %>% 
  group_by(PAT_ID, group, FINANCIAL_CLASS_NAME) %>% 
  summarise(total.cost = sum(`SYSTEM DEBIT`, na.rm=TRUE))
cost.fc.df <- cost.fc.df[cost.fc.df$total.cost<10000 & cost.fc.df$total.cost>0,]

cost.fc.df.stats <- as.data.frame(cost.fc.df %>% group_by(FINANCIAL_CLASS_NAME) %>%
                                 summarise(
                                   count = n()
                                 ))
cost.fc.df.stats <- cost.fc.df.stats[order(-cost.fc.df.stats$count), ]

cost.fc.df.both <- merge(cost.fc.df, cost.fc.df.stats, by = 'FINANCIAL_CLASS_NAME')
cost.fc.df.both$FINANCIAL_CLASS_NAME <- reorder(cost.fc.df.both$FINANCIAL_CLASS_NAME, -cost.fc.df.both$count)
xlabel <- with(cost.fc.df.stats, paste0(FINANCIAL_CLASS_NAME,"\nn=",count))
p1 <- ggplot(data = cost.fc.df.both, aes(
  x=FINANCIAL_CLASS_NAME, y=total.cost)) + 
  scale_x_discrete(labels = xlabel)+
  xlab("financial class") +
  ylab("cost") +
  geom_boxplot(aes(fill=FINANCIAL_CLASS_NAME))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
        axis.text.y = element_text(size=10), 
        plot.title = element_text(hjust = 0.5),
        legend.position="none")+
  ggtitle("WES and non-WES")+
  coord_flip()


cost.fc.df.non.wes <- cost.fc.df[cost.fc.df$group=='non-wes', ]
cost.fc.df.non.wes.stats <- as.data.frame(cost.fc.df.non.wes %>% 
                              group_by(FINANCIAL_CLASS_NAME) %>%
                                    summarise(
                                      count = n()
                                    ))
cost.fc.df.non.wes.stats <- cost.fc.df.non.wes.stats[order(-cost.fc.df.non.wes.stats$count), ]

cost.fc.df.non.wes <- merge(cost.fc.df.non.wes, cost.fc.df.non.wes.stats, by = 'FINANCIAL_CLASS_NAME')
cost.fc.df.non.wes$FINANCIAL_CLASS_NAME <- reorder(cost.fc.df.non.wes$FINANCIAL_CLASS_NAME, -cost.fc.df.non.wes$count)
xlabel <- with(cost.fc.df.non.wes.stats, paste0(FINANCIAL_CLASS_NAME,"\nn=",count))
p2 <- ggplot(data = cost.fc.df.non.wes, aes(
  x=FINANCIAL_CLASS_NAME, y=total.cost)) + 
  scale_x_discrete(labels = xlabel)+
  xlab("financial class") +
  ylab("cost") +
  geom_boxplot(aes(fill=FINANCIAL_CLASS_NAME))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
        axis.text.y = element_text(size=10), 
        plot.title = element_text(hjust = 0.5),
        legend.position="none")+
  ggtitle("non-WES")+
  coord_flip()

cost.fc.df.wes <- cost.fc.df[cost.fc.df$group=='wes', ]
cost.fc.df.wes.stats <- as.data.frame(cost.fc.df.wes %>% 
                                            group_by(FINANCIAL_CLASS_NAME) %>%
                                            summarise(
                                              count = n()
                                            ))
cost.fc.df.wes.stats <- cost.fc.df.wes.stats[order(-cost.fc.df.wes.stats$count), ]

cost.fc.df.wes <- merge(cost.fc.df.wes, cost.fc.df.wes.stats, by = 'FINANCIAL_CLASS_NAME')
cost.fc.df.wes$FINANCIAL_CLASS_NAME <- reorder(cost.fc.df.wes$FINANCIAL_CLASS_NAME, -cost.fc.df.wes$count)
xlabel <- with(cost.fc.df.wes.stats, paste0(FINANCIAL_CLASS_NAME,"\nn=",count))
p3 <- ggplot(data = cost.fc.df.wes, aes(
  x=FINANCIAL_CLASS_NAME, y=total.cost)) + 
  scale_x_discrete(labels = xlabel) +
  xlab("financial class") +
  ylab("cost") +
  geom_boxplot(aes(fill=FINANCIAL_CLASS_NAME))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
        axis.text.y = element_text(size=10), 
        plot.title = element_text(hjust = 0.5),
        legend.position="none")+
  ggtitle("WES")+
  coord_flip()

grid.arrange(p1, p2, p3, nrow = 1)

sort(table(cost.fc.df.wes$FINANCIAL_CLASS_NAME))
cost.fc.df.wes <- cost.fc.df.wes[!(cost.fc.df.wes$FINANCIAL_CLASS_NAME %in% c("Other","Medicare")), ]
cost.fc.df.wes$FINANCIAL_CLASS_NAME <- as.factor(as.character(cost.fc.df.wes$FINANCIAL_CLASS_NAME))
kruskal.test(total.cost ~ FINANCIAL_CLASS_NAME, data = cost.fc.df.wes)
pwt <- as.data.frame(pairwise.wilcox.test(cost.fc.df.wes$total.cost, cost.fc.df.wes$FINANCIAL_CLASS_NAME, p.adjust.method = "bonferroni")$p.value)
pwt <- round(pwt,2 )
pwt[pwt>0.05] <- NA
pwt


# cost analysis stratified by CCSs

query <- "select distinct tb1.PAT_ID, ccs9.CCS_CATEGORY_DESC
from CLAIMSDATA.WES_BASE_KVN_1_FNL_V22 tb1, CLAIMSDATA.WES_CCS_DX_ICD9 ccs9
where tb1.PRIM_ENC_CSN_ID is not null and (tb1.RESTRICTED_YN is null or tb1.RESTRICTED_YN = 'N') and tb1.DX_TYPE = 'Admit Dx' and REPLACE(tb1.ICD9, '.', '') = ccs9.ICD9_CM_CODE"

full.df = dbGetQuery(con, query)
full.df$PAT_ID <- as.character(full.df$PAT_ID)
str(full.df)
amt.df.ccs <- merge(amt.df.wide, full.df, by = 'PAT_ID')
str(amt.df.ccs)

# -----continue here... the pattern of analyzing ccs is similar to analyzing cost by financial class
# charge to CCS
cost.ccs.df <- amt.df.ccs %>% 
  group_by(PAT_ID, group, CCS_CATEGORY_DESC) %>% 
  summarise(total.cost = sum(`SYSTEM DEBIT`, na.rm=TRUE))

# keep two ccs names shorter
cost.ccs.df$CCS_CATEGORY_DESC[cost.ccs.df$CCS_CATEGORY_DESC=='Disorders usually diagnosed in infancy/ch']='Dis diag in infancy'
cost.ccs.df$CCS_CATEGORY_DESC[cost.ccs.df$CCS_CATEGORY_DESC=='Developmental disorders']='Dev dis'
cost.ccs.df$CCS_CATEGORY_DESC[cost.ccs.df$CCS_CATEGORY_DESC=='Attention-deficit/conduct/disruptive beha']='Att-def/con/dis beha'

amt.threshold <- 10000
cost.ccs.df <- cost.ccs.df[cost.ccs.df$total.cost<amt.threshold & cost.ccs.df$total.cost>0,]

cost.ccs.df.stats <- as.data.frame(cost.ccs.df %>% 
                                  group_by(CCS_CATEGORY_DESC) %>%
                                    summarise(
                                      count = n()
                                    ))
cost.ccs.df.stats <- cost.ccs.df.stats[order(-cost.ccs.df.stats$count), ]
top.n <- 20
cost.ccs.df.stats <- cost.ccs.df.stats[1:top.n,]
cost.ccs.df.both <- merge(cost.ccs.df, cost.ccs.df.stats, by = 'CCS_CATEGORY_DESC')
cost.ccs.df.both$CCS_CATEGORY_DESC <- reorder(cost.ccs.df.both$CCS_CATEGORY_DESC, -cost.ccs.df.both$count)
xlabel <- with(cost.ccs.df.stats, paste0(CCS_CATEGORY_DESC,"\nn=",count))
p1 <- ggplot(data = cost.ccs.df.both, aes(
  x=CCS_CATEGORY_DESC, y=total.cost)) + 
  scale_x_discrete(labels = xlabel)+
  xlab("ccs") +
  ylab("cost") +
  geom_boxplot(aes(fill=CCS_CATEGORY_DESC))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
        axis.text.y = element_text(size=10), 
        plot.title = element_text(hjust = 0.5),
        legend.position="none")+
  ggtitle("WES and non-WES")+
  coord_flip(ylim = c(0, amt.threshold))

# test the difference of cost by ccs for both groups
# remove Unclassified
cost.ccs.df.both <- cost.ccs.df.both[!(cost.ccs.df.both$CCS_CATEGORY_DESC %in% c("Unclassified")), ]
kruskal.test(total.cost ~ CCS_CATEGORY_DESC, data = cost.ccs.df.both)
pwt <- as.data.frame(pairwise.wilcox.test(cost.ccs.df.both$total.cost, 
                                          cost.ccs.df.both$CCS_CATEGORY_DESC, 
                                          p.adjust.method = "bonferroni")$p.value)
pwt <- round(pwt,2 )
pwt[pwt>0.05] <- NA
pwt

cost.ccs.df.non.wes <- cost.ccs.df[cost.ccs.df$group=='non-wes', ]
cost.ccs.df.non.wes.stats <- as.data.frame(cost.ccs.df.non.wes %>% 
                                            group_by(CCS_CATEGORY_DESC) %>%
                                            summarise(
                                              count = n()
                                            ))
cost.ccs.df.non.wes.stats <- cost.ccs.df.non.wes.stats[order(-cost.ccs.df.non.wes.stats$count), ]
cost.ccs.df.non.wes.stats <- cost.ccs.df.non.wes.stats[1:top.n,]
cost.ccs.df.non.wes <- merge(cost.ccs.df.non.wes, cost.ccs.df.non.wes.stats, by = 'CCS_CATEGORY_DESC')
cost.ccs.df.non.wes$CCS_CATEGORY_DESC <- reorder(cost.ccs.df.non.wes$CCS_CATEGORY_DESC, -cost.ccs.df.non.wes$count)
xlabel <- with(cost.ccs.df.non.wes.stats, paste0(CCS_CATEGORY_DESC,"\nn=",count))
p2 <- ggplot(data = cost.ccs.df.non.wes, aes(
  x=CCS_CATEGORY_DESC, y=total.cost)) + 
  scale_x_discrete(labels = xlabel)+
  xlab("ccs") +
  ylab("cost") +
  geom_boxplot(aes(fill=CCS_CATEGORY_DESC))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
        axis.text.y = element_text(size=10), 
        plot.title = element_text(hjust = 0.5),
        legend.position="none")+
  ggtitle("non-WES")+
  coord_flip(ylim = c(0, amt.threshold))

cost.ccs.df.wes <- cost.ccs.df[cost.ccs.df$group=='wes', ]
cost.ccs.df.wes.stats <- as.data.frame(cost.ccs.df.wes %>% 
                                        group_by(CCS_CATEGORY_DESC) %>%
                                        summarise(
                                          count = n()
                                        ))
cost.ccs.df.wes.stats <- cost.ccs.df.wes.stats[order(-cost.ccs.df.wes.stats$count), ]
cost.ccs.df.wes.stats <- cost.ccs.df.wes.stats[1:top.n,]
cost.ccs.df.wes <- merge(cost.ccs.df.wes, cost.ccs.df.wes.stats, by = 'CCS_CATEGORY_DESC')
cost.ccs.df.wes$CCS_CATEGORY_DESC <- reorder(cost.ccs.df.wes$CCS_CATEGORY_DESC, -cost.ccs.df.wes$count)
xlabel <- with(cost.ccs.df.wes.stats, paste0(CCS_CATEGORY_DESC,"\nn=",count))
p3 <- ggplot(data = cost.ccs.df.wes, aes(
  x=CCS_CATEGORY_DESC, y=total.cost)) + 
  scale_x_discrete(labels = xlabel) +
  xlab("ccs") +
  ylab("cost") +
  geom_boxplot(aes(fill=CCS_CATEGORY_DESC))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
        axis.text.y = element_text(size=10), 
        plot.title = element_text(hjust = 0.5),
        legend.position="none")+
  ggtitle("WES")+
  coord_flip(ylim = c(0, amt.threshold) )

# grid.arrange(p1, p2, p3, nrow = 1)
grid.arrange(p2, p3, nrow = 1)



## Section: percentage of payment analysis

# amt.df.wide is the base

Validate.Range <- function(x){
  x[is.nan(x)] <- NA
  x[is.infinite(x)] <- NA
  x[x > 1] <- 1
  x[x < 0] <- 0
  return(x)
}

pay.df <- amt.df.wide
# pay.df[is.na(pay.df)] <- 0 
pay.df <- pay.df[, c("HSP_ACCOUNT_ID", "group","FINANCIAL_CLASS_NAME", 
          "INSURANCE - PAYMENT", "REFUND-INSURANCE/OTHER", "SYSTEM DEBIT", "PATIENT - PAYMENT")]
pay.df$`REFUND-INSURANCE/OTHER`[is.na(pay.df$`REFUND-INSURANCE/OTHER`)] <- 0 
pay.df$`INSURANCE - PAYMENT`[is.na(pay.df$`INSURANCE - PAYMENT`)] <- 0 
pay.df$ins.percent.pay <- NA
for(i in 1:length(pay.df$ins.percent.pay)){
  if(!is.na(pay.df$`SYSTEM DEBIT`[i])){
    pay.df$ins.percent.pay[i] <- round(-(pay.df$`INSURANCE - PAYMENT`[i] + pay.df$`REFUND-INSURANCE/OTHER`[i]) / pay.df$`SYSTEM DEBIT`[i], 2)
  }
}
pay.df$ins.percent.pay <- Validate.Range(pay.df$ins.percent.pay)

pay.df.ins <- pay.df[!is.na(pay.df$FINANCIAL_CLASS_NAME), ]

# merge transplant network with Other
pay.df.ins$FINANCIAL_CLASS_NAME[pay.df.ins$FINANCIAL_CLASS_NAME=='Transplant Network'] <- 'Other'

# percentage of payment by group
max.val <- 1
ggplot(data = pay.df.ins, aes(
  x=group, y=ins.percent.pay)) + 
  # scale_x_discrete(labels = xlabel)+
  scale_y_continuous(breaks = seq(0, max.val, 0.1))+
  xlab("group") +
  ylab("percentage") +
  geom_boxplot(aes(fill=group))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
        axis.text.y = element_text(size=10), 
        plot.title = element_text(hjust = 0.5),
        legend.position="none")+
  ggtitle("Percentage of Payment by Insurance")+
  coord_flip(ylim = c(0,max.val))

# Use semi-transparent fill
p<-ggplot(pay.df.ins, aes(x=ins.percent.pay, fill=group, color=group)) +
  geom_histogram(position="identity", alpha=0.5) +theme(legend.position="bottom")
p
# Add mean lines
mu <- pay.df.ins %>%
  group_by(group) %>%
  summarise(grp.mean=mean(ins.percent.pay, na.rm = TRUE))
p+geom_vline(data=mu, aes(xintercept=grp.mean, color=group),
             linetype="dashed")

# percentage of pay by fc
pay.df.ins.stats <- as.data.frame(pay.df.ins %>% 
                                    group_by(FINANCIAL_CLASS_NAME) %>%
                                    summarise(
                                      count = n()
                                    ))
pay.df.ins.stats <- pay.df.ins.stats[order(-pay.df.ins.stats$count), ]
pay.df.ins.both <- merge(pay.df.ins, pay.df.ins.stats, by = 'FINANCIAL_CLASS_NAME')
pay.df.ins.both$FINANCIAL_CLASS_NAME <- reorder(pay.df.ins.both$FINANCIAL_CLASS_NAME, -pay.df.ins.both$count)
xlabel <- with(pay.df.ins.stats, paste0(FINANCIAL_CLASS_NAME,"\nn=",count))
p1 <- ggplot(data = pay.df.ins.both, aes(
  x=FINANCIAL_CLASS_NAME, y=ins.percent.pay)) + 
  scale_x_discrete(labels = xlabel)+
  scale_y_continuous(breaks = seq(0, max.val, 0.1))+
  xlab("financial class") +
  ylab("percentage") +
  geom_boxplot(aes(fill=FINANCIAL_CLASS_NAME))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
        axis.text.y = element_text(size=10), 
        plot.title = element_text(hjust = 0.5),
        legend.position="none")+
  ggtitle("Both Groups")+
  coord_flip(ylim = c(0,max.val))

pay.df.ins.non.wes <- pay.df.ins[pay.df.ins$group=='non-wes', ]
pay.df.ins.stats.non.wes <- as.data.frame(pay.df.ins.non.wes %>% 
                                    group_by(FINANCIAL_CLASS_NAME) %>%
                                    summarise(
                                      count = n()
                                    ))
pay.df.ins.stats.non.wes <- pay.df.ins.stats.non.wes[order(-pay.df.ins.stats.non.wes$count), ]
pay.df.ins.non.wes <- merge(pay.df.ins.non.wes, pay.df.ins.stats.non.wes, by = 'FINANCIAL_CLASS_NAME')
pay.df.ins.non.wes$FINANCIAL_CLASS_NAME <- reorder(pay.df.ins.non.wes$FINANCIAL_CLASS_NAME, -pay.df.ins.non.wes$count)
xlabel <- with(pay.df.ins.stats.non.wes, paste0(FINANCIAL_CLASS_NAME,"\nn=",count))
p2 <- ggplot(data = pay.df.ins.non.wes, aes(
  x=FINANCIAL_CLASS_NAME, y=ins.percent.pay)) + 
  scale_x_discrete(labels = xlabel)+
  scale_y_continuous(breaks = seq(0, max.val, 0.1))+
  xlab("financial class") +
  ylab("percentage") +
  geom_boxplot(aes(fill=FINANCIAL_CLASS_NAME))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
        axis.text.y = element_text(size=10), 
        plot.title = element_text(hjust = 0.5),
        legend.position="none")+
  ggtitle("non-WES")+
  coord_flip(ylim = c(0,max.val))

pay.df.ins.wes <- pay.df.ins[pay.df.ins$group=='wes', ]
pay.df.ins.stats.wes <- as.data.frame(pay.df.ins.wes %>% 
                                            group_by(FINANCIAL_CLASS_NAME) %>%
                                            summarise(
                                              count = n()
                                            ))
pay.df.ins.wes <- merge(pay.df.ins.wes, pay.df.ins.stats.wes, by = 'FINANCIAL_CLASS_NAME')
# use non-wes count to order wes fc
non.wes.count.df <- pay.df.ins.non.wes[, c('FINANCIAL_CLASS_NAME', 'count')]
non.wes.count.df <- unique(non.wes.count.df)
colnames(non.wes.count.df)[colnames(non.wes.count.df)=="count"] <- "non.wes.count"
pay.df.ins.wes<- merge(pay.df.ins.wes, non.wes.count.df, by = 'FINANCIAL_CLASS_NAME', all.x = TRUE)
pay.df.ins.wes$FINANCIAL_CLASS_NAME <- reorder(pay.df.ins.wes$FINANCIAL_CLASS_NAME, -pay.df.ins.wes$non.wes.count)
pay.df.ins.stats.wes <- merge(pay.df.ins.stats.wes, non.wes.count.df, by = 'FINANCIAL_CLASS_NAME', all.x = TRUE)
pay.df.ins.stats.wes <- pay.df.ins.stats.wes[order(-pay.df.ins.stats.wes$non.wes.count), ]
xlabel <- with(pay.df.ins.stats.wes, paste0(FINANCIAL_CLASS_NAME,"\nn=",count))
p3 <- ggplot(data = pay.df.ins.wes, aes(
  # ggplot(data = na.omit(amt.df.final), aes(
  x=FINANCIAL_CLASS_NAME, y=ins.percent.pay)) + 
  scale_x_discrete(labels = xlabel)+
  scale_y_continuous(breaks = seq(0, max.val, 0.1))+
  xlab("financial class") +
  ylab("percentage") +
  geom_boxplot(aes(fill=FINANCIAL_CLASS_NAME))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
        axis.text.y = element_text(size=10), 
        plot.title = element_text(hjust = 0.5),
        legend.position="none")+
  ggtitle("WES")+
  coord_flip(ylim = c(0,max.val))

# grid.arrange(p1, p2, p3, nrow = 1)
grid.arrange(p2, p3, nrow = 1)


pay.df.ins$denial.status <- NA
pay.thredhold <- 0.01
pay.df.ins$denial.status[pay.df.ins$ins.percent.pay > pay.thredhold] <- "Payment"
pay.df.ins$denial.status[pay.df.ins$ins.percent.pay <= pay.thredhold] <- "Denial"
table(pay.df.ins$denial.status, pay.df.ins$group)


# calculate percentage of patient payment
# carry over total charge (system debit) from insurance level to patient level
pay.df <- as.data.frame(pay.df %>%
              group_by(HSP_ACCOUNT_ID) %>%
              arrange(`SYSTEM DEBIT`) %>%
              mutate(`SYSTEM DEBIT` = sum(`SYSTEM DEBIT`, na.rm = TRUE)))
pay.df.self <- pay.df[is.na(pay.df$FINANCIAL_CLASS_NAME), ]

# pay.df.self$`PATIENT - PAYMENT`[is.na(pay.df.self$`PATIENT - PAYMENT`)] <- 0 
pay.df.self$pat.percent.pay <- NA
for(i in 1:length(pay.df.self$pat.percent.pay)){
  if(!is.na(pay.df.self$`SYSTEM DEBIT`[i]) && pay.df.self$`SYSTEM DEBIT`[i]>0){
    pay.df.self$pat.percent.pay[i] <- round(-pay.df.self$`PATIENT - PAYMENT`[i] / pay.df.self$`SYSTEM DEBIT`[i], 2)
  }
}
pay.df.self$pat.percent.pay <- Validate.Range(pay.df.self$pat.percent.pay)
# percentage of payment by patient
max.val <- 1
ggplot(data = pay.df.self, aes(
  x=group, y=pat.percent.pay)) + 
  # scale_x_discrete(labels = xlabel)+
  scale_y_continuous(breaks = seq(0, max.val, 0.1))+
  xlab("group") +
  ylab("percentage") +
  geom_boxplot(aes(fill=group))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
        axis.text.y = element_text(size=10), 
        plot.title = element_text(hjust = 0.5),
        legend.position="none")+
  ggtitle("Percentage of Payment by Patient")+
  coord_flip(ylim = c(0,max.val))

# Use semi-transparent fill
p<-ggplot(pay.df.self, aes(x=pat.percent.pay, fill=group, color=group)) +
  geom_histogram(position="identity", alpha=0.5) +theme(legend.position="bottom")
p
# Add mean lines
mu <- pay.df.self %>%
    group_by(group) %>%
  summarise(grp.mean=mean(pat.percent.pay, na.rm = TRUE))
p+geom_vline(data=mu, aes(xintercept=grp.mean, color=group),
             linetype="dashed")

# calculate patient payment in $
# carry over total charge (system debit) from insurance level to patient level
pay.df <- as.data.frame(pay.df %>%
                          group_by(HSP_ACCOUNT_ID) %>%
                          arrange(`SYSTEM DEBIT`) %>%
                          mutate(`SYSTEM DEBIT` = sum(`SYSTEM DEBIT`, na.rm = TRUE)))
pay.df.self <- pay.df[is.na(pay.df$FINANCIAL_CLASS_NAME), ]

pay.df.self$self.pay <- -pay.df.self$`PATIENT - PAYMENT`
max.val <- 2000
pay.df.self$self.pay[pay.df.self$self.pay<0 | pay.df.self$self.pay>max.val] <- NA
# pay.df.self$`PATIENT - PAYMENT`[is.na(pay.df.self$`PATIENT - PAYMENT`)] <- 0 
# pay.df.self$pat.percent.pay <- NA
# for(i in 1:length(pay.df.self$pat.percent.pay)){
#   if(!is.na(pay.df.self$`SYSTEM DEBIT`[i]) && pay.df.self$`SYSTEM DEBIT`[i]>0){
#     pay.df.self$pat.percent.pay[i] <- round(-pay.df.self$`PATIENT - PAYMENT`[i] / pay.df.self$`SYSTEM DEBIT`[i], 2)
#   }
# }


ggplot(data = pay.df.self, aes(
  x=group, y=self.pay)) + 
  scale_y_continuous(breaks = seq(0, max.val, 1000))+
  xlab("group") +
  ylab("amount") +
  geom_boxplot(aes(fill=group))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
        axis.text.y = element_text(size=10), 
        plot.title = element_text(hjust = 0.5),
        legend.position="none")+
  ggtitle("Payment by Patient")+
  coord_flip(ylim = c(0,max.val))

# Use semi-transparent fill
p<-ggplot(pay.df.self, aes(x=self.pay, fill=group, color=group)) +
  geom_histogram(position="identity", alpha=0.5) +theme(legend.position="bottom")
p
# Add mean lines
mu <- pay.df.self %>%
  group_by(group) %>%
  summarise(grp.mean=mean(self.pay, na.rm = TRUE))
p+geom_vline(data=mu, aes(xintercept=grp.mean, color=group),
             linetype="dashed")

## Section: remit code analysis

query <- "select HSP_ACCOUNT_ID, BUCKET_ID, BDC_ID, RMT_CODE_EXT, TX_REMIT_GROUP_CODE_LIST, DENIAL_TYPE
from CLAIMSDATA.WES_BASE_KVN_3_V2
where (liability_bucket_type in ('Primary Claim', 'Secondary Claim') AND LIABILITY_BUCKET_STATUS = 'Closed') OR procedure_desc = 'PATIENT - PAYMENT'"

full.df = dbGetQuery(con, query)
full.df <- unique(full.df)
str(full.df)
# full.df$HSP_ACCOUNT_ID <- as.character(full.df$HSP_ACCOUNT_ID)
# [1] 67906     6
# length(unique(full.df$BUCKET_ID))
# 4838 closed buckets and 7690 total buckets

remit.df <- merge(hsp.df, full.df, by = 'HSP_ACCOUNT_ID')
table(remit.df$TX_REMIT_GROUP_CODE_LIST, remit.df$group)

## Section: hsp level claim duration analysis
query <- "SELECT distinct DEID_HSP_ACCOUNT_ID, BUCKET_ID, CONTACT_DATE, CRD_CONTACT_TYP
FROM CLAIMSDATA.WES_REC_CLM_OT"

full.df = dbGetQuery(con, query)
full.df <- unique(full.df)
full.df$BUCKET_ID <- as.character(full.df$BUCKET_ID)
str(full.df)
# full.df$HSP_ACCOUNT_ID <- as.character(full.df$HSP_ACCOUNT_ID)
# [1] 67906     6
length(unique(full.df$BUCKET_ID))
# 4838 closed buckets and 7690 total buckets

duration.df <- merge(hsp.df, full.df, by.y = "DEID_HSP_ACCOUNT_ID",  by.x = 'HSP_ACCOUNT_ID')
duration.df$BUCKET_ID <- as.character(duration.df$BUCKET_ID)
duration.df <- unique(duration.df)
duration.df <- duration.df[order(duration.df$BUCKET_ID, duration.df$CONTACT_DATE), ]
duration.df$CONTACT_DATE <- as.Date(duration.df$CONTACT_DATE)
str(duration.df)
duration.df <- duration.df[duration.df$CRD_CONTACT_TYP %in% c("Claim Run Accepted", "Claim Closed"), ]
table(duration.df$CRD_CONTACT_TYP)
# length(unique(full.df$BUCKET_ID))

last.closed.df <- duration.df[duration.df$CRD_CONTACT_TYP=='Claim Closed', ] %>% 
            group_by(HSP_ACCOUNT_ID) %>% 
            summarise(last.closed = max(CONTACT_DATE, na.rm=TRUE))

first.accepted.df <- duration.df[duration.df$CRD_CONTACT_TYP=='Claim Run Accepted', ] %>% 
            group_by(HSP_ACCOUNT_ID) %>% 
            summarise(first.accepted = min(CONTACT_DATE, na.rm=TRUE))

hsp.duration.df <- duration.df[, c("HSP_ACCOUNT_ID", "group")]
hsp.duration.df <- unique(hsp.duration.df)
str(duration.df)
hsp.duration.df <- merge(hsp.duration.df, first.accepted.df, by = "HSP_ACCOUNT_ID")
str(duration.df)
hsp.duration.df <- merge(hsp.duration.df, last.closed.df, by = "HSP_ACCOUNT_ID")
str(duration.df)

hsp.duration.df$claims.duration <- as.numeric(hsp.duration.df$last.closed - hsp.duration.df$first.accepted)

# visualize claims duration 
max.val <- 365
hsp.duration.df <- hsp.duration.df[hsp.duration.df$claims.duration<=max.val, ]
ggplot(data = hsp.duration.df, aes(
  x=group, y=claims.duration)) + 
  scale_y_continuous(breaks = seq(0, max.val, 30))+
  xlab("group") +
  ylab("claims duration") +
  geom_boxplot(aes(fill=group))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
        axis.text.y = element_text(size=10), 
        plot.title = element_text(hjust = 0.5),
        legend.position="none")+
  ggtitle("Claims Duration by Group")+
  coord_flip(ylim = c(0, max.val))

# Use semi-transparent fill
p<-ggplot(hsp.duration.df, aes(x=claims.duration, fill=group, color=group)) +
  geom_histogram(position="identity", alpha=0.5) +theme(legend.position="bottom") + scale_x_continuous(limits = c(0, max.val), breaks = seq(0, max.val, 30))
p
# Add mean lines
mu <- hsp.duration.df %>%
  group_by(group) %>%
  summarise(grp.mean=mean(claims.duration, na.rm = TRUE))
p+geom_vline(data=mu, aes(xintercept=grp.mean, color=group),
             linetype="dashed")

## Section: derive 25 cases for each group for payment calculation
query <- "SELECT distinct DEID_HSP_ACCOUNT_ID, HSP_ACCOUNT_ID as REAL_HSP_ACCOUNT_ID
FROM CLAIMSDATA.WES_REC_CLM_OT"
hsp.ids.df = dbGetQuery(con, query)
hsp.ids.df <- unique(hsp.ids.df)
validation.df <- merge(pay.df, hsp.ids.df, by.x = 'HSP_ACCOUNT_ID', by.y = "DEID_HSP_ACCOUNT_ID")
# write.xlsx(validation.df[validation.df$group=='wes',][1:25,], "Results/wes-25.xlsx", col.names=TRUE, row.names=FALSE, showNA=FALSE)
# write.xlsx(validation.df[validation.df$group=='non-wes',][1:25,], "Results/non-wes-25.xlsx", col.names=TRUE, row.names=FALSE, showNA=FALSE)

dbDisconnect(con)
