########## ESCC-tm

### Topic models

setwd("m://users//stefange//OneDrive - NTNU//2-data//crisis") # Your working directory

library(stm)
library(car)
library(reshape2)

load("wDTM.RData") # you get this through ESCC-pre

### We drop all terms that occur less than 100 times in the corpus. That can lead to elimination of very specific terms that may help identify individual events or crises. For identifying single crises and automatically labelling them, rather use the full set of terms.

tdtm <- dfm_trim(dtm, min_docfreq = 100, docfreq_type = "count") 
out.selected <- convert(tdtm, to = "stm")

### Alternatively, we drop all terms that occur less than 1000 times in the corpus. That can lead to elimination of very specific terms that may help identify individual events or crises. For identifying single crises and automatically labelling them, rather use the full set of terms.

tdtm2 <- dfm_trim(dtm, min_docfreq = 1000, docfreq_type = "count") 
out.selected2 <- convert(tdtm2, to = "stm")

### For searching the optimal number of topics, we draw a 2% sample. Random numbers between 0 and 100

tdtm2@docvars$subsetter <- runif(min=0,max=100,n=129103)
tdtm2.02 <- dfm_subset(tdtm2,subsetter>98)
out.selected2.02 <- convert(tdtm2.02,to="stm")

### The searchK functions take very long to complete. It is therefore executed with only 2 percent of the documents. We try out 20, 40, 60, 80, 100, 120, 140, 160 topic models.

sK2.01 <- searchK(out.selected2.02$documents,out.selected2.02$vocab,K=c(20,40,60,80),init.type="Spectral",cores=1)								  
sK2.02 <- searchK(out.selected2.02$documents,out.selected2.02$vocab,K=c(100,120,140,160),init.type="Spectral",cores=1)								  
sK2 <- rbind(sK2.01$results,sK2.02$results)
sKK2 <- melt(sK2,id.vars=c("K"))

ggplot(sKK2,aes(x=K,y=value,color=variable))+geom_point()+geom_line()+facet_wrap(~variable,scales="free")

### It looks like extracting 120 topics is reasonable. Residuals are lowest.

### We run the STM with 120 topics for the two reduced DTMs (f>100:STMr and f>1000:STM)

STMr <- stm(documents=out.selected2$documents,vocab <- out.selected2$vocab, K=120, data=out.selected2$meta,
			init.type="Spectral", verbose=TRUE)
STM <- stm(documents=out.selected$documents,vocab <- out.selected$vocab, K=120, data=out.selected$meta,
			init.type="Spectral", verbose=TRUE)
save(STM,file="stmf120.RData")
save(STMr,file="stmr120.RData")

### We create a data frame that gives us the proportion of/likelihood of topics for each text in the wide format (i.e. topics are represented as variables). 
wide.STM <- data.frame(STMr$theta,out.selected2$meta)
metavars <- names(out.selected2$meta)

### We create a long version of wide.STM (i.e. topics are represented as additional cases, and each article shows up 120 times (once per topic)).
long.STM <- melt(wide.STM,id.vars=metavars)
names(long.STM)[17:18] <- c("topic","pr")

### labelling the topics based on their keywords
summary(STMr)
long.STM$issue <- Recode(long.STM$topic,"'X1'='Australia';'X2'='?New York City?';'X3'='Holiday/Tourism';'X4'='Worker/Unite/Die/Teacher';'X5'='Stocks/Companies';'X6'='?Dates?';'X7'='US Administration';'X8'='Profits/Sales/Recession';'X9'='Army/Soldiers/Military';
'X10'='Epidemic/Disease';'X11'='Failure/Breakdown';'X12'='?Numbers?';'X13'='?1970s/Health?';'X14'='?1990s?';'X15'='Mobile Technology';'X16'='Election/Coalition/Government/Parties';'X17'='Near-East/Telegram';'X18'='Space/Flight/Launch/Survival';'X19'='Influenza/Seasonal/Numbers';
'X20'='Britain/Wales/Countries';'X21'='?Activity?';'X22'='Sell/Buy/Competition/Property/Auction';'X23'='Company/Merger/Takeover';'X24'='?Numbers2?';'X25'='Economic Policy/Effective';'X26'='Boat/Steamship/Captain';'X27'='?2000s/Billions?';'X28'='Theatre/Art/Exhibition';'X29'='Public Sector/Social Service';
'X30'='Earthquake/Killed/Area';'X31'='Crude Oil Price';'X32'='Crisis Meeting/Committee/Conference';'X33'='Teachers/Schools/Education/Students/University';'X34'='Wool/Trade/Prices';'X35'='Despair/Country/England/Peace';'X36'='Train/Railway/Traffic';'X37'='Investment/Investors/Hedge Funds';'X38'='East/West/Middle/Region';'X39'='?OCR errors?';
'X40'='?Appeals/Pleas?';'X41'='Football/Team/Club/Players/Fans';'X42'='Coal Pit/Colliery/Mine/Explosion';'X43'='Newspaper/Article/Publish';'X44'='Israel/Iraq/Lebanon/Syria/Arab';'X45'='GDP/Recession/Inflation/Growth/Forecasts';'X46'='Steel/Industry/Manufacturing/Output';'X47'='Virus/Research/Drug/Science/Fly';'X48'='Pakistan/Government/Democracy/Corrupion';'X49'='?Glory/Lord/Memorial?';
'X50'='Cars/Motor/Sales';'X51'='Oxford/Chester/Wellington/Liverpool/Glasgow';'X52'='Beef/Cattle/Meat/Animals/Fish';'X53'='Fever/Public/Persons/Present';'X54'='Nuclear/Gas/Power/Plant/Electricity';'X55'='ID/Music/Jews/Radio/Concert';'X56'='?Speak/Not?';'X57'='Bonds/Securities/Prices';'X58'='Female/Children/Women/Family/Aged/Wife';'X59'='Bridge/Building/Construction/Collapse';
'X60'='Famine/Aid/Relief/Fund/Refugee/Charity';'X61'='Church/Catholic/Bishop/Christian';'X62'='Percent/Rate/Increase/Rise';'X63'='Race/White/Black/Kennedy';'X64'='Fuel/Petrol/Supply/Shortage';'X65'='?Mr/Dr/Teeth/Heath?';'X66'='?Old-fashioned personal pronouns?';'X67'='Airlines/Aircraft/Airline/Airport/Air travel/Aviation';'X68'='Local/Council/Association';'X69'='Irish/Ireland/Unionist/Ulster';
'X70'='Coffee/America/Brazil/Mexico';'X71'='Submarine/Ships/Admiral/Navy';'X72'='University/Training/Computer/Technology/Research/Management';'X73'='Firefighters/Fire/Explosion/Flames/Injured/Smoke';'X74'='Insurers/Insurance/Companies';'X75'='Russia/Austria/Emperor/Petersburg';'X76'='Dam/Water/River/Flood/Rain';'X77'='Tin Market/Brokers/Stock Exchange';'X78'='Cotton/Employers/Wages/Federation/Strike';'X79'='Coroner/Inquiry/Witness/Jury/Evidence';
'X80'='Ferry Disaster/Survivors/Killed/Safety';'X81'='Cabinet/Chamber/Deputies/Assembly/Constitution/Senate';'X82'='Rubber/Expenditure/1930s/Taxation/Revenue';'X83'='Europe/EEC/EU/Britain/Brussels';'X84'='BBC/Radio/Television/Media/Ads';'X85'='Dehli/India/Governor/Provinces';'X86'='Banks/Loans/Lending/Mortgage';'X87'='Royalty/Queen/Prince/Majesty';'X88'='Pension/Tax/Budget/Deficit/Income/Taxes';'X89'='Misery/Perhaps/Might/Familiar';
'X90'='Percent/Firms/Big/Less/Gap/Even';'X91'='Sugar/Wheat/Ton/Crop/Iron/Silver';'X92'='Downing Street/Spokesperson';'X93'='France/Switzerland/Italy/Paris';'X94'='Societ/Russia/Communist/Moskow/Poland/Solidarity';'X95'='Police/Prison/Officer/Murder/Arrest/Violence/Jail';'X96'='Parliament/Lords/Commons/Debate/Amendment/Cheers';'X97'='Cabinet/Minister/Sinister/Grime/Resignation/Lisbon';'X98'='Egyptian/Negotiation/British/Treaty/Agreement/Settlement';'X99'='Job/Worker/Redundancy/Unemployment/Wage';
'X100'='Homeless/Homes/Housing/Population/Families';'X101'='Wine/Fashion/Sales/Retailers/Customers/Recession';'X102'='Nurses/Hospital/Patients/Medical/Health/Doctors';'X103'='Military/War/Defense/NATO/Peace/Weapons/Nations';'X104'='Germany/Berlin/Bonn';'X105'='?Print/May/Next/Already?';'X106'='Korea/Japan/China/Africa/Tokyo';'X107'='Athens/Greece/Turkish/Greek';'X108'='Milk/Food/Farmers/Agriculture/Farm/Wheat';'X109'='?Month?';
'X110'='Romania/USA/Canada/Kingdom/Sweden';'X111'='Fraud/Court/Legal/Law/Justice/Lawyers';'X112'='Gold/Dollar/Currency/Sterling/Monetary/Exchange';'X113'='Chinese/Troops/Enemy/Artillery/Expedition';'X114'='Comedy/Film/Drama/Star/Story';'X115'='Unions/Strike/Workers/Dispute/Talks';'X116'='?single letters?';'X117'='FTSE/Shares/Market/Prices/Index';'X118'='?Drug?';'X119'='IMF/Debt/Global/Euro';
'X120'='Copper/Ton/Metal/Ore/Lead'")

### assigning the topics to areas based on their keywords.
long.STM$area <- Recode(long.STM$topic,"'X1'='Location';'X2'='?????';'X3'='Leisure';'X4'='Workers';'X5'='Stock markets';'X6'='?????';'X7'='Government';'X8'='Economy';'X9'='Military';
'X10'='Epidemic';'X11'='Accident';'X12'='?????';'X13'='Health';'X14'='1990s';'X15'='Technology';'X16'='Government';'X17'='Geopolitical';'X18'='Technology';'X19'='Epidemic';
'X20'='Location';'X21'='?????';'X22'='Economy';'X23'='Economy';'X24'='?????';'X25'='Economic Policy';'X26'='Transport';'X27'='2000s/Billions';'X28'='Culture';'X29'='Welfare';
'X30'='Disaster';'X31'='Resources';'X32'='Negotiation';'X33'='Education';'X34'='Economy';'X35'='Geopolitical';'X36'='Transport';'X37'='Stock markets';'X38'='Location';'X39'='?????';
'X40'='?????';'X41'='Leisure';'X42'='Accident';'X43'='Media';'X44'='Geopolitical';'X45'='Economy';'X46'='Manufacturing';'X47'='Epidemic';'X48'='Geopolitical';'X49'='?????';
'X50'='Economy';'X51'='Location';'X52'='Agriculture';'X53'='Epidemic';'X54'='Energy';'X55'='Culture';'X56'='?????';'X57'='Stock markets';'X58'='Family/Gender';'X59'='Accident';
'X60'='Humanitarian Aid';'X61'='Religion';'X62'='Economy';'X63'='Race relations';'X64'='Resources';'X65'='?????';'X66'='?????';'X67'='Transport';'X68'='Associations';'X69'='Ireland';
'X70'='Geopolitical';'X71'='Military';'X72'='Education';'X73'='Disaster';'X74'='Economy';'X75'='Geopolitical';'X76'='Disaster';'X77'='Resources';'X78'='Manufacturing';'X79'='Crime';
'X80'='Disaster';'X81'='Government';'X82'='Resources';'X83'='Europe';'X84'='Media';'X85'='Geopolitical';'X86'='Banks';'X87'='Royals';'X88'='Pension/Budget';'X89'='Welfare';
'X90'='Economy';'X91'='Resources';'X92'='Government';'X93'='Geopolitical';'X94'='Geopolitical';'X95'='Crime';'X96'='Government';'X97'='Government';'X98'='Geopolitical';'X99'='Workers';
'X100'='Welfare';'X101'='Agriculture';'X102'='Health';'X103'='Military';'X104'='Geopolitical';'X105'='?????';'X106'='Geopolitical';'X107'='Geopolitical';'X108'='Agriculture';'X109'='?????';
'X110'='Geopolitical';'X111'='Crime';'X112'='Currency';'X113'='Geopolitical';'X114'='Leisure';'X115'='Workers';'X116'='?????';'X117'='Stock markets';'X118'='?????';'X119'='Currency';
'X120'='Resources'")

### First exploration of issues and areas by decade
# Create a variable for the decade in which the article was published.
long.STM$decade <- 10*floor(as.numeric(long.STM$year)/10) 

# Create a dataset of the number of articles of each topic in the respective decade, by newspaper.
decade.STM <- aggregate(long.STM$pr,by=list(long.STM$decade,long.STM$issue,long.STM$newspaper),FUN="sum")
names(decade.STM) <- c("decade","issue","newspaper","articles")

# Create a dataset of the number of articles of each area in the respective decade, by newspaper.
decade.STM_area <- aggregate(long.STM$pr,by=list(long.STM$decade,long.STM$area,long.STM$newspaper),FUN="sum")
names(decade.STM_area) <- c("decade","area","newspaper","articles")

# Create a dataset of the total volume of crisis coverage in the respective decade, by newspaper.
decade.VOL <- aggregate(long.STM$pr,by=list(long.STM$decade,long.STM$newspaper),FUN="sum")
names(decade.VOL) <- c("decade","newspaper","articles")

# Create volume, share, and percentage variables.
decade.STM$volume <- c(rep(decade.VOL$articles[1:18],times=120),rep(decade.VOL$articles[19:42],times=120))
decade.STM$share <- decade.STM$articles/decade.STM$volume
decade.STM$percentage <- round(100*decade.STM$articles/decade.STM$volume,1)

# Create volume, share, and percentage variables.
decade.STM_area$volume <- c(rep(decade.VOL$articles[1:18],times=39),rep(decade.VOL$articles[19:42],times=39))
decade.STM_area$share <- decade.STM_area$articles/decade.STM_area$volume
decade.STM_area$percentage <- round(100*decade.STM_area$articles/decade.STM_area$volume,1)

# Plots the development of each issue over decades, by newspaper
ggplot(decade.STM,aes(y=percentage,x=decade,linetype=newspaper))+geom_smooth()+facet_wrap(~issue,scales="free_y")

# Plots the development of each area over decades, by newspaper
ggplot(decade.STM_area,aes(y=percentage,x=decade,linetype=newspaper))+geom_smooth()+facet_wrap(~area,scales="free_y")+geom_hline(yintercept=0,color="red")+theme_light()

# Sort areas by their initial frequency before 1900, The Times only.
decade.STM_area$area <- Recode(decade.STM_area$area,"'?????'=NA")
with(subset(decade.STM_area,newspaper=="The Times."),tapply(share,area,FUN="mean"))
decade.STM_area$area_f <- factor(decade.STM_area$area,ordered=TRUE,levels=c("Geopolitical","Epidemic","Government","Disaster","Transport","Welfare","Crime","Resources","Military","Royals",
	"Stock markets","Media","Family/Gender","Accident","Negotiation","Workers","Economy","Culture","Location",
	"Leisure","Manufacturing","Economic Policy","Religion","Agriculture","Humanitarian Aid","Ireland","Health",
	"Associations","Banks","Education","Race relations","Currency","Pension/Budget","Technology","Europe","Energy","1990s","2000s/Billions"))

ggplot(subset(decade.STM_area,newspaper=="The Times."),aes(y=percentage,x=decade,fill=area_f))+geom_area(color="white")+
	scale_fill_viridis_d(na.value="grey80")+theme_light()+ggplot2::annotate("text",hjust=0,x=rep(2011,times=38),y=100-labeller,label=levels(decade.STM_area$area_f))

matcher <- match(levels(decade.STM_area$area_f),subset(decade.STM_area,newspaper=="The Times." & decade=="2010")$area_f)

labeller <- cumsum(subset(decade.STM_area,newspaper=="The Times." & decade=="2010")[matcher,"percentage"])-subset(decade.STM_area,newspaper=="The Times." & decade=="2010")[matcher,"percentage"]/2

round(100*prop.table(table(long.STM$decade,long.STM$issue),2),1)

