library(RColorBrewer)
library(readxl)
library(ggplot2)
library(stringr)
#pdfs of figures are saved in the working directory (C:/Documents)
cyanide <- read_excel(path = "C:/Users/Taylor.Dodrill/Downloads/draft_ecotox_tool_correct_allcriteria.xlsx", sheet='Screened', guess_max = 10000)
cyanide <- read.csv("C:/Users/Taylor.Dodrill/Downloads/Wa_draft_ecotox_tool_Cyanide_screened.csv")
cyanide <- subset(cyanide, cyanide$'Use?'=='yes')
#make sure to use the right excel spreadsheet - the one in google drive has broken links - use the first path (from Pat's email)

#RQs using fresh water cyanide criteria for Washington
FWAcuteCN <- subset(cyanide, cyanide$'Acute or Chronic?'=='Acute'&cyanide$'Media Type'=='Fresh water') 
FWAcuteCN$'Taxon'=factor(FWAcuteCN$'Taxon', levels=c('Amphibians', 'Salmonid','Sturgeon','Other Fish','Invertebrates','Algae&Plants')) #sort order for taxa
FWAcuteCN$'Effect'=factor(FWAcuteCN$'Effect',levels=c('Behavior','Reproduction','Growth & Development','Mortality','Population','Ecosystem process')) #sort order for effects (Y axis)
FWAcuteCN$'Endpoint'=factor(FWAcuteCN$'Endpoint', levels = c('No effect','NOEC','NR-ZERO','LC0','Decrease','Change','Increase','LETC','LOEC','MATC',
                                                             'EC01','EC05','EC10','EC15','EC20','EC25','EC40','EC50','ET50','IC50','EC60','EC75','EC80',
                                                             'EC85','EC90','EC95','EC99','LC01','LC05','LC10','LT10','LC100','LC15','LC20','LC50','LT50',
                                                             'LC85','LC90','LC95','LC99','NR-LETH','BCF')) #sort order for endpoints (LCxx, etc)
FWAcuteCN$'Response Magnitude'=factor(FWAcuteCN$'Response Magnitude', levels = c('No effect','NOEC','NR-ZERO','Decrease','Change','Increase','LETC',
                                                                                 'LOEC','MATC','0','1','5','10','15','20','25','40','50','60','75','80','85',
                                                                                 '90','95','99','100','NR-LETH','BCF'))

#plotting
# pFWAcuteCN <- ggplot(FWAcuteCN,aes(x='Washington RQ', y='Endpoint',color='Effects',shape='Effects'))+
#   geom_point(position = position_dodge(0.6),cex=1.5)+
#   scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8))+
#   scale_x_continuous(transform = 'log10')+
#   scale_color_brewer(palette = 'Dark2')+
#   geom_vline(xintercept=1, color='black')+
#   facet_grid(~'Taxon', labeller=labeller('Taxon'=label_wrap_gen(10)))+
#   theme(axis.text.x = element_text(size = 7, angle = 45))+
#   labs(colour='Effects',x='Risk Quotient (Applicable Criterion/Effects Concentration)',y='Endpoints for Acute Cyanide Exposures in Fresh Water')

mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 6))


#pFWAcuteCN<-ggplot(FWAcuteCN,aes(x=`Washington RQ`,y=`Endpoint`,color=`Effects`,shape=`Effects`))+geom_point(position=position_dodge(0.6),cex=1.5)+scale_shape_manual(values=c(0,1,2,3,4,5,6,8))+scale_x_continuous(trans='log10')+scale_color_brewer(palette='Dark2')+geom_vline(xintercept=1, color='black')+facet_grid(~`Taxon`,labeller = labeller(`Taxon` = label_wrap_gen(10)))+theme(axis.text.x = element_text(size = 7,angle=45))+labs(colour = 'Effects',x='Risk Quotient (Applicable Criterion/Effects Concentration)',y='Endpoints for Acute Cyanide Exposures in Fresh Water')
pFWAcuteCN<-ggplot(FWAcuteCN,aes(x=`Washington RQ`,y=`Endpoint`,color=`Effects`,shape=`Effects`))+geom_point(position=position_dodge(0.6),cex=1.5)+scale_shape_manual(values=c(0,1,2,3,4,5,6,8))+scale_x_continuous(trans='log10')+scale_color_manual(values = mycolors)+geom_vline(xintercept=1, color='black')+facet_grid(~`Taxon`,labeller = labeller(`Taxon` = label_wrap_gen(10)))+theme(axis.text.x = element_text(size = 7,angle=45))+labs(colour = 'Effects',x='Risk Quotient (Applicable Criterion/Effects Concentration)',y='Endpoints for Acute Cyanide Exposures in Fresh Water')
#left off here - couldn't get the graph to load. The FW Acute CN dataframe has values in it
pdf('Acute, Freshwater Cyanide RQ Plot for Washington State Criteria 26-Feb-2025')
print(pFWAcuteCN)
dev.off()


# FW Chronic Plot
FWChronicCN <- subset(cyanide,cyanide$'Acute or Chronic?'=='Chronic'&cyanide$'Media Type'=='Fresh water')
FWChronicCN$'Taxon' = factor(FWChronicCN$'Taxon', levels=c('Amphibians','Salmonid','Sturgeon','Other Fish','Invertebrates','Algae&Plants'))
FWChronicCN$'Effect' = factor(FWChronicCN$'Effect', levels=c('Behavior','Reproduction','Growth & Development','Mortality','Population','Ecosystem process'))
FWChronicCN$'Endpoint' = factor(FWChronicCN$'Endpoint', levels=c('No effect','NOEC','NR-ZERO','LC0','Decrease','Change','Increase','LETC','LOEC','MATC','EC01','EC05','EC10','EC15','EC20','EC25','EC40','EC50','ET50','IC50','EC60','EC75','EC80','EC85','EC90','EC95','EC99','LC01','LC05','LC10','LT10','LC100','LC15','LC20','LC50','LT50','LC85','LC90','LC95','LC99','NR-LETH','BCF'))
FWChronicCN$'Response Magnitude' = factor(FWChronicCN$'Response Magnitude', levels=c('No effect','NOEC','NR-ZERO','Decrease','Change','Increase','LETC','LOEC','MATC','0','1','5','10','15','20','25','40','50','60','75','80','85','90','95','99','100','NR-LETH','BCF'))

pFWChronicCN<-ggplot(FWChronicCN,aes(x=`Washington RQ`,y=`Endpoint`,color=`Effects`,shape=`Effects`))+geom_point(position=position_dodge(0.6),cex=1.5)+scale_shape_manual(values=c(0,1,2,3,4,5,6,8))+scale_x_continuous(trans='log10')+scale_color_brewer(palette='Dark2')+geom_vline(xintercept=1, color='black')+facet_grid(~`Taxon`,labeller = labeller(`Taxon` = label_wrap_gen(10)))+theme(axis.text.x = element_text(size = 7,angle=45))+labs(colour = 'Effects',x='Risk Quotient (Applicable Criterion/Effects Concentration)',y='Endpoints for Chronic Cyanide Exposures in Fresh Water')
pdf('Chronic, Freshwater Cyanide RQ Plot for Washington State Criteria 28-Feb-2025')
print(pFWChronicCN)
dev.off()

#SW Acute Plot
SWAcuteCN <- subset(cyanide, cyanide$'Acute or Chronic?'=='Acute'&cyanide$'Media Type'=='Salt water')
SWAcuteCN$'Taxon' = factor(SWAcuteCN$'Taxon', levels=c('Amphibians','Salmonid','Sturgeon','Other Fish','Invertebrates','Algae&Plants'))
SWAcuteCN$'Effect' = factor(SWAcuteCN$'Effect', levels=c('Behavior','Reproduction','Growth & Development','Mortality','Population','Ecosystem process'))
SWAcuteCN$'Endpoint' = factor(SWAcuteCN$'Endpoint', levels=c('No effect','NOEC','NR-ZERO','LC0','Decrease','Change','Increase','LETC','LOEC','MATC','EC01','EC05','EC10','EC15','EC20','EC25','EC40','EC50','ET50','IC50','EC60','EC75','EC80','EC85','EC90','EC95','EC99','LC01','LC05','LC10','LT10','LC100','LC15','LC20','LC50','LT50','LC85','LC90','LC95','LC99','NR-LETH','BCF'))
SWAcuteCN$'Response Magnitude' = factor(SWAcuteCN$'Response Magnitude', levels=c('No effect','NOEC','NR-ZERO','Decrease','Change','Increase','LETC','LOEC','MATC','0','1','5','10','15','20','25','40','50','60','75','80','85','90','95','99','100','NR-LETH','BCF'))

pSWAcuteCN<-ggplot(SWAcuteCN,aes(x=`Washington RQ`,y=`Endpoint`,color=`Effects`,shape=`Effects`))+geom_point(position=position_dodge(0.6),cex=1.5)+scale_shape_manual(values=c(0,1,2,3,4,5,6,8))+scale_x_continuous(trans='log10')+scale_color_brewer(palette='Dark2')+geom_vline(xintercept=1, color='black')+facet_grid(~`Taxon`,labeller = labeller(`Taxon` = label_wrap_gen(10)))+theme(axis.text.x = element_text(size = 7,angle=45))+labs(colour = 'Effects',x='Risk Quotient (Applicable Criterion/Effects Concentration)',y='Endpoints for Acute Cyanide Exposures in Salt Water')
pdf('Acute, Saltwater Cyanide RQ Plot for Washington State Criteria 28-Feb-2025')
print(pSWAcuteCN)
dev.off()

#SW Chronic Plot
SWChronicCN <- subset(cyanide, cyanide$'Acute or Chronic?'=='Chronic'&cyanide$'Media Type'=='Salt water')
SWChronicCN$'Taxon' = factor(SWChronicCN$'Taxon', levels=c('Amphibians','Salmonid','Sturgeon','Other Fish','Invertebrates','Algae&Plants'))
SWChronicCN$'Effect' = factor(SWChronicCN$'Effect', levels=c('Behavior','Reproduction','Growth & Development','Mortality','Population','Ecosystem process'))
SWChronicCN$'Endpoint' = factor(SWChronicCN$'Endpoint', levels=c('No effect','NOEC','NR-ZERO','LC0','Decrease','Change','Increase','LETC','LOEC','MATC','EC01','EC05','EC10','EC15','EC20','EC25','EC40','EC50','ET50','IC50','EC60','EC75','EC80','EC85','EC90','EC95','EC99','LC01','LC05','LC10','LT10','LC100','LC15','LC20','LC50','LT50','LC85','LC90','LC95','LC99','NR-LETH','BCF'))
SWChronicCN$'Response Magnitude' = factor(SWChronicCN$'Response Magnitude', levels=c('No effect','NOEC','NR-ZERO','Decrease','Change','Increase','LETC','LOEC','MATC','0','1','5','10','15','20','25','40','50','60','75','80','85','90','95','99','100','NR-LETH','BCF'))

pSWChronicCN<-ggplot(SWChronicCN,aes(x=`Washington RQ`,y=`Endpoint`,color=`Effects`,shape=`Effects`))+geom_point(position=position_dodge(0.6),cex=1.5)+scale_shape_manual(values=c(0,1,2,3,4,5,6,8))+scale_x_continuous(trans='log10')+scale_color_brewer(palette='Dark2')+geom_vline(xintercept=1, color='black')+facet_grid(~`Taxon`,labeller = labeller(`Taxon` = label_wrap_gen(10)))+theme(axis.text.x = element_text(size = 7,angle=45))+labs(colour = 'Effects',x='Risk Quotient (Applicable Criterion/Effects Concentration)',y='Endpoints for Chronic Cyanide Exposures in Salt Water')
pdf('Chronic, Saltwater Cyanide RQ Plot for Washington State Criteria 28-Feb-2025')
print(pSWChronicCN)
dev.off()

#Puget Sound
pPSAcuteCN<-ggplot(SWAcuteCN,aes(x=`Puget Sound RQ`,y=`Endpoint`,color=`Effects`,shape=`Effects`))+geom_point(position=position_dodge(0.6),cex=1.5)+scale_shape_manual(values=c(0,1,2,3,4,5,6,8))+scale_x_continuous(trans='log10')+scale_color_brewer(palette='Dark2')+geom_vline(xintercept=1, color='black')+facet_grid(~`Taxon`,labeller = labeller(`Taxon` = label_wrap_gen(10)))+theme(axis.text.x = element_text(size = 7,angle=45))+labs(colour = 'Effects',x='Risk Quotient (Applicable Criterion/Effects Concentration)',y='Endpoints for Acute Cyanide Exposures in Puget Sound')
pdf('Acute, Puget Sound Cyanide RQ Plot for Washington State Criteria 28-Feb-2025')
print(pPSAcuteCN)
dev.off()

pPSChronicCN<-ggplot(SWChronicCN,aes(x=`Puget Sound RQ`,y=`Endpoint`,color=`Effects`,shape=`Effects`))+geom_point(position=position_dodge(0.6),cex=1.5)+scale_shape_manual(values=c(0,1,2,3,4,5,6,8))+scale_x_continuous(trans='log10')+scale_color_brewer(palette='Dark2')+geom_vline(xintercept=1, color='black')+facet_grid(~`Taxon`,labeller = labeller(`Taxon` = label_wrap_gen(10)))+theme(axis.text.x = element_text(size = 7,angle=45))+labs(colour = 'Effects',x='Risk Quotient (Applicable Criterion/Effects Concentration)',y='Endpoints for Chronic Cyanide Exposures in Puget Sound')
pdf('Chronic, Puget Sound Cyanide RQ Plot for Washington State Criteria 28-Feb-2025')
print(pPSChronicCN)
dev.off()

#try it as a plotly 
install.packages("plotly")
library(plotly)
pPSChronicCN<-ggplot(SWChronicCN,aes(x=`Puget Sound RQ`,y=`Endpoint`,color=`Effects`,shape=`Effects`))+geom_point(position=position_dodge(0.6),cex=1.5)+scale_shape_manual(values=c(0,1,2,3,4,5,6,8))+scale_x_continuous(trans='log10')+scale_color_brewer(palette='Dark2')+geom_vline(xintercept=1, color='black')+facet_grid(~`Taxon`,labeller = labeller(`Taxon` = label_wrap_gen(10)))+theme(axis.text.x = element_text(size = 7,angle=45))+labs(colour = 'Effects',x='Risk Quotient (Applicable Criterion/Effects Concentration)',y='Endpoints for Chronic Cyanide Exposures in Puget Sound')
plPSChronicCN <- ggplotly(pPSChronicCN) # i don't think this way will work to get the different labels unfortunately

#seems like the best way is to rebuild the ggplot as a plotly and then alter hover text, possibly using add_trace? 
#Will need to find out how to have color and shape attributes in plotly
p1 <- plot_ly(data = SWChronicCN,
              x = ~'Puget Sound RQ',
              y = ~'Endpoint',
              color = ~'Effects',
              shape = ~'Effects')

p2 <- plPSChronicCN %>%
  add_trace(
    hovertext = paste(SWChronicCN$Label
                      )
  )

plotly_obj <- add_trace(plPSChronicCN,
                        x = SWChronicCN$`Puget Sound RQ`,
                        y = SWChronicCN$Endpoint,
                        color = SWChronicCN$Effects,
                        shape = SWChronicCN$Effects,
                        type = "bar",
                        hovertext = paste(SWChronicCN$Label))
library(htmlwidgets)


htmlwidgets::saveWidget(
  
  widget = plPSChronicCN, #the plotly object
  
  file = 'Interactive Chronic Puget Sound Cyanide Endpoint Figure.html',
  
  selfcontained = TRUE #creates a single html file
  
)


