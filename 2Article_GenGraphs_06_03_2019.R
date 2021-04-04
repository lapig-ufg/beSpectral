source("file:///D:/SPEC/DATA/2Article_09_09_2019.R")
library(robustbase)
library(maptools)

FAZ_LENS8_specs = noiseFiltering(RAW_LENS8, method="sgolay", n=25,p=4)
FAZ_PROBE_specs = noiseFiltering(RAW_PROBE, method="sgolay", n=25,p=4)

#teste = merge(RAW_LENS8,RAW_PROBE)

#teste2 = subset(teste, Site =='ALP' &  Target =='Other')

#FAZ_PROBE_specs = subset(FAZ_PROBE_specs, Target =='Brachiaria brizantha')
#FAZ_PROBE_specs = spectral.resampling(FAZ_PROBE_specs,'Landsat8')

FAZ_LENS8_specs = subset(FAZ_LENS8_specs, Target =='Brachiaria brizantha')
FAZ_PROBE_specs = subset(FAZ_PROBE_specs, Target =='Brachiaria brizantha')

mask(FAZ_LENS8_specs) = wave2Mask
mask(FAZ_PROBE_specs) = wave2Mask

s2 = spectral.resampling(FAZ_LENS8_specs,'Sentinel2')
l8 = spectral.resampling(FAZ_LENS8_specs,'Landsat8')
l5 = spectral.resampling(FAZ_LENS8_specs,'Landsat5')
l7 = spectral.resampling(FAZ_LENS8_specs,'Landsat7')
MODIS = spectral.resampling(FAZ_LENS8_specs,'MODIS')

fileList = SI(l8)$Filename
ndvi_field = vegindex(FAZ_LENS8_specs, "(R842-R665)/(R842+R665)")
ndvi_s2 = vegindex(s2, "(R842-R665)/(R842+R665)")
ndvi_l5 = vegindex(l5, "(R830-R660)/(R830+R660)")
ndvi_l7 = vegindex(l7, "(R840-R660)/(R840+R660)")
ndvi_l8 = vegindex(l8, "(R863.5-R655.0)/(R863.5+R655.0)")
ndvi_MODIS = vegindex(MODIS, "(R858-R645)/(R858+R645)")


date = SI(s2)$FileDate
site = SI(s2)$Site

df2Exp = data.frame(fileList,date,site,ndvi_field,ndvi_l5,ndvi_l7,ndvi_l8,ndvi_s2,ndvi_MODIS)

write.table(df2Exp, "D:\\SPEC\\2Process\\BHRV\\ALL\\CanopyField_resampled_data_BHRV_w_MODIS_2.csv", sep=";")

#mask(FAZ_LENS8_specs) = c(1372,1376)


BRACH_LENS8_wv = FAZ_LENS8_specs@wavelength
BRACH_PROBE_wv = FAZ_PROBE_specs@wavelength

BRACH_LENS8 = data.frame(FAZ_LENS8_specs@spectra@spectra_ma)
BRACH_PROBE = data.frame(FAZ_PROBE_specs@spectra@spectra_ma)
colnames(BRACH_PROBE) = BRACH_PROBE_wv
colnames(BRACH_LENS8) = BRACH_LENS8_wv

BRACH_LENS8 = cbind(BRACH_LENS8,FAZ_LENS8_specs@SI@SI_data$V1)
BRACH_PROBE = cbind(BRACH_PROBE,FAZ_PROBE_specs@SI@SI_data$V1)

names(BRACH_LENS8)[names(BRACH_LENS8) == "FAZ_LENS8_specs@SI@SI_data$V1"] <- "Class"
names(BRACH_PROBE)[names(BRACH_PROBE) == "FAZ_PROBE_specs@SI@SI_data$V1"] <- "Class"

BRACH_LENS8$Class = as.character(BRACH_LENS8$Class)
BRACH_PROBE$Class = as.character(BRACH_PROBE$Class)
#BRACH_PROBE$Class[BRACH_PROBE$Class =="Brachiaria brizantha"] = "Pastagem"
#BRACH_PROBE$Class[BRACH_PROBE$Class !="Brachiaria brizantha"] = "NaoPastagem"

#names(BRACH_PROBE) = BRACH_PROBE$Class

specLib = FAZ_LENS8_specs
#specLib@SI@SI_data[["V1"]][is.na(specLib@SI@SI_data[["V1"]])] = '2019-01-17'
#specLib@SI@SI_data[["V1"]][is.na(specLib@SI@SI_data[["V1"]])] = '2018-11-15'

specLib@SI@SI_data[["V1"]] = as.Date(specLib@SI@SI_data[["V1"]])

#AllData = merge(FAZ_LENS8_specs,FAZ_PROBE_specs)

wave2MaskNumata = c(1100, 1250,1850,2218)

wave2Mask = c(1100, 1250,1800,2000,2300,2600)
mask(specLib) = wave2Mask

#specLib = subset(specLib,Site =='MTH')

abril_2018 = subset(specLib,V1 >= '2018-04-01' & V1 <= '2018-04-30' & Time <= 173000 & Target == 'Brachiaria brizantha')
maio_2018 = subset(specLib,V1 >= '2018-05-01' & V1 <= '2018-05-30' & Time <= 173000 & Target == 'Brachiaria brizantha')
agosto_2018 = subset(specLib,V1 >= '2018-08-01' & V1 <= '2018-08-31' & Time <= 173000 & Target == 'Brachiaria brizantha')
outubro_2018 = subset(specLib,V1 >= '2018-10-01' & V1 <= '2018-10-31' & Time <= 143000 & Target == 'Brachiaria brizantha')
novembro_2018 = subset(specLib,V1 >= '2018-11-01' & V1 <= '2018-11-30' & Time <= 173000 & Target == 'Brachiaria brizantha')
dezembro_2018 = subset(specLib,V1 >= '2018-12-01' & V1 <= '2018-12-31' & Time <= 173000 & Target == 'Brachiaria brizantha')
janeiro_2019 = subset(specLib,V1 >= '2019-01-01' & V1 <= '2019-01-31' & Time <= 173000 & Target == 'Brachiaria brizantha')
fevereiro_2019 = subset(specLib,V1 >= '2019-02-01' & V1 <= '2019-02-28' & Time <= 173000 & Target == 'Brachiaria brizantha')
março_2019 = subset(specLib,V1 >= '2019-03-01' & V1 <= '2019-03-31' & Time <= 173000 & Target == 'Brachiaria brizantha')
abril_2019 = subset(specLib,V1 >= '2019-04-01' & V1 <= '2019-04-30' & Time <= 173000 & Target == 'Brachiaria brizantha')

mes = subset(specLib,Target == 'Brachiaria brizantha')

plot(mes,FUN = mean,col=mes@wavelength,xaxt='n',
     ylim = c(0,70),xlim = c(350,2300),col="red",new = TRUE)

lines(x = mes@wavelength , y = apply(mes@spectra@spectra_ma,2,mean), col=c('forestgreen'))
#points(x = mes@wavelength , y = colMedians(mes@spectra@spectra_ma), col=c('red'),pch = 19)

lines(x = mes@wavelength ,apply(mes@spectra@spectra_ma,2,quantile,probs = c(0.05)),col=c('blue'))
lines(x = mes@wavelength ,apply(mes@spectra@spectra_ma,2,quantile,probs = c(0.95)),col=c('red'))

#lines(x = mes@wavelength , y = colMeans(mes@spectra@spectra_ma),col=c('darkgreen'),lwd=5)
#points(mes[,spliBands]@wavelength,colMedians(mes[,spliBands]@spectra@spectra_ma),col=c("blue"),pch = 19)

axis(side = 1, at=seq(300,2300,100),labels = seq(300,2300,100))
plot_regions(spectras_LENS8, regions = default_spec_regions(), add = TRUE)
grid()

legend("topleft",legend=c("Percentile - 95%", "Mean","Percentile - 5%"), col=c("red", "forestgreen","blue"),
       lty=c(19,19,19), cex=0.75,lwd=2,bg =c('white','blue'))

#BEstBandsNamesL8 = c('Coastal','Blue','Green','Red','NIR','Cirrus','SWIR 1','SWIR 2')
#BEstBandsNamesS2 = c('Coastal','Blue','Green','Red','RedEdge 1','RedEdge 2','RedEdge 3',
#                     'NIR','RedEdge 4','WV','Cirrus','SWIR 1','SWIR 2')
#
#pointLabel(x = mes[,spliBands]@wavelength, y = colMedians(mes[,spliBands]@spectra@spectra_ma),
#           labels = as.character(Best_LENS8_Saz), cex =0.75,offset =0.5 ,
#           allowSmallOverlap = FALSE,doPlot = TRUE,trace = TRUE, method = 'GA')
#
#text(x = mes@wavelength , y = colMedians(mes@spectra@spectra_ma),
#     labels = BEstBandsNamesS2, pos = 2,cex = 0.75)

for (i in 2:nspectra(FAZ_LENS8_specs)){
  lines(x = FAZ_LENS8_specs@wavelength , y = FAZ_LENS8_specs@spectra@spectra_ma[i,], col=c('cyan'))
}

plot(subset(mes,Site == '2B'),FUN = median,col=c(''),xaxt='n',
     ylim = c(0,50),xlim = c(350,2300))

#LOOPS
for (i in 2:nspectra(subset(mes,Site == '2B'))){
  lines(x = subset(mes,Site == '2B')@wavelength , y = subset(mes,Site == '2B')@spectra@spectra_ma[i,], col=c('darkgreen'))
}
for (i in 2:nspectra(subset(mes,Site == 'MDC'))){
  lines(x = subset(mes,Site == 'MDC')@wavelength , y = subset(mes,Site == 'MDC')@spectra@spectra_ma[i,], col=c('royalblue'))
}
for (i in 2:nspectra(subset(mes,Site == 'MTH'))){
  lines(x = subset(mes,Site == 'MTH')@wavelength , y = subset(mes,Site == 'MTH')@spectra@spectra_ma[i,], col=c('red'))
}
for (i in 2:nspectra(subset(mes,Site == 'ALP'))){
  lines(x = subset(mes,Site == 'ALP')@wavelength , y = subset(mes,Site == 'ALP')@spectra@spectra_ma[i,], col=c('magenta'))
}
for (i in 2:nspectra(subset(mes,Site == 'MRD'))){
  lines(x = subset(mes,Site == 'MRD')@wavelength , y = subset(mes,Site == 'MRD')@spectra@spectra_ma[i,], col=c('cyan'))
}

#MEANS
lines(x = subset(mes,Site == '2B' )@wavelength , y = colMeans(subset(mes,Site == '2B')@spectra@spectra_ma),col=c('darkgreen'), lwd=2)
lines(x = subset(mes,Site == 'MDC')@wavelength , y = colMeans(subset(mes,Site == 'MDC')@spectra@spectra_ma),col=c('royalblue'),lwd=2)
lines(x = subset(mes,Site == 'MTH')@wavelength , y = colMeans(subset(mes,Site == 'MTH')@spectra@spectra_ma),col=c('red'),      lwd=2)
lines(x = subset(mes,Site == 'ALP')@wavelength , y = colMeans(subset(mes,Site == 'ALP')@spectra@spectra_ma),col=c('magenta'),  lwd=2)
lines(x = subset(mes,Site == 'MRD')@wavelength , y = colMeans(subset(mes,Site == 'MRD')@spectra@spectra_ma),col=c('cyan'),     lwd=2)

#OTHERS
axis(side = 1, at=seq(300,2300,100),labels = seq(300,2300,100))
plot_regions(spectras_LENS8, regions = default_spec_regions(), add = TRUE)
grid()
legend("topleft",legend=c("2B", "MDC",'MTH','ALP','MRD'), col=c("darkgreen", "royalblue",'red','magenta','cyan'),lty=1:1, cex=0.75,lwd=2)

NDVI = "(R780-R680)/(R780+R680)"
#

#RED = "(R680)"
#NIR = "(R780)"
#
#NDVI = RED
#
faz = c('2B','MDC','MTH','ALP','MRD')
#
#meses = c('04/2018','05/2018','08/2018','10/2018','11/2018','12/2018','01/2019')
#ndvi_media = data.frame(Date=as.character.Date(meses))
#ndvi_null = NULL
#
#ndvi_media = data.frame()
#
#for (i in 1:length(faz)){
#  print(faz[i])
#  print(mean(vegindex(subset(abril_2019,Site == faz[i]), NDVI)))
#}
#
fazNmb = 5
#

colorList = c('red','green','royalblue',"black","aquamarine2")
dateList = c("04/2018","05/2018","08/2018","10/2018","11/2018","12/2018","01/2019","02/2019","03/2019","04/2019")
for (i in 1:length(faz)){

  fazNmb = i
  
red_abril_red =  mean(vegindex(subset(abril_2018,Site == faz[fazNmb]), RED))
red_maio =  mean(vegindex(subset(maio_2018,Site == faz[fazNmb]), RED))
red_agosto =  mean(vegindex(subset(agosto_2018,Site == faz[fazNmb]), RED))
red_outubro =  mean(vegindex(subset(outubro_2018,Site == faz[fazNmb]), RED))
red_novembro =  mean(vegindex(subset(novembro_2018,Site == faz[fazNmb]), RED))
red_dezembro =  mean(vegindex(subset(dezembro_2018,Site == faz[fazNmb]), RED))
red_janeiro =  mean(vegindex(subset(janeiro_2019,Site == faz[fazNmb]), RED))
red_fevereiro =  mean(vegindex(subset(fevereiro_2019,Site == faz[fazNmb]), RED))
red_março =  mean(vegindex(subset(março_2019,Site == faz[fazNmb]), RED))
red_abril =  mean(vegindex(subset(abril_2019,Site == faz[fazNmb]), RED))
#
nir_abril_nir = mean(vegindex(subset(abril_2018,Site == faz[fazNmb]), NIR))
nir_maio = mean(vegindex(subset(maio_2018,Site == faz[fazNmb]), NIR))
nir_agosto = mean(vegindex(subset(agosto_2018,Site == faz[fazNmb]), NIR))
nir_outubro = mean(vegindex(subset(outubro_2018,Site == faz[fazNmb]), NIR))
nir_novembro = mean(vegindex(subset(novembro_2018,Site == faz[fazNmb]), NIR))
nir_dezembro = mean(vegindex(subset(dezembro_2018,Site == faz[fazNmb]), NIR))
nir_janeiro = mean(vegindex(subset(janeiro_2019,Site == faz[fazNmb]), NIR))
nir_fevereiro =  mean(vegindex(subset(fevereiro_2019,Site == faz[fazNmb]),NIR))
nir_março =  mean(vegindex(subset(março_2019,Site == faz[fazNmb]),NIR))
nir_abril =  mean(vegindex(subset(abril_2019,Site == faz[fazNmb]),NIR))

red = c(red_abril_red,red_maio,red_agosto,red_outubro,red_novembro,red_dezembro,red_janeiro,red_fevereiro,red_março,red_abril)
nir = c(nir_abril_nir,nir_maio,nir_agosto,nir_outubro,nir_novembro,nir_dezembro,nir_janeiro,nir_fevereiro,nir_março,nir_abril)

if (i== 1){
  plot(red,nir,col=c(colorList[i]),ylim = c(0,60),xlim = c(0,35),
       xlab='RED (%)',ylab = 'NIR (%)',
       #main = 'Fazenda Mata da Chuva - Britânia/GO',
       pch = 19)
  #text(x = red , y = nir, labels = dateList, pos = 3)
  #lines(x = red, y = nir,col=c(colorList[i]),lwd=2)
}
else {
  points(red,nir,col=c(colorList[i]),pch = 19)
  #text(x = red , y = nir, labels = dateList, pos = 3)
  #lines(x = red, y = nir,col=c(colorList[i]),lwd=2)
}

}

grid()
legend("bottomleft",legend=c("2B", "MDC",'MTH','ALP','MRD'), col=colorList,pch = 19, cex=0.75)


#lines(x = red, y = nir,col=c('green'),lwd=2)

points(red,nir,col=c('royalblue'))
#lines(x = red, y = nir,col=c('royalblue'),lwd=2)

points(red,nir,col=c("black"))

points(red,nir,col=c("aquamarine2"))

plot(red_abril_red,nir_abril_nir,col=c('red'),ylim = c(0,100),xlim = c(0,100),
     xlab='RED (%)',ylab = 'NIR (%)',main = 'Fazenda Mata da Chuva - Britânia/GO')
points(red_maio,nir_maio,col=c('green'))
points(red_agosto,nir_agosto,col=c('cyan'))
points(red_outubro,nir_outubro,col=c('magenta'))
points(red_novembro,nir_novembro,col=c('royalblue'))
points(red_dezembro,nir_dezembro,col=c("darkgreen"))
points(red_janeiro,nir_janeiro,col=c("black"))
points(red_fevereiro,nir_fevereiro,col=c("darkochid"))
points(red_março,nir_março,col=c("darked"))
points(red_abril,nir_abril,col=c("aquamarine2"))

grid()
legend("topright",
       legend=c("Abril - 2018", "Maio - 2018",'Agosto - 2018','Outubro - 2018',
                'Novembro - 2018','Dezembro - 2018','Janeiro - 2019'),
       col=c('red','green','cyan','magenta','royalblue',"darkgreen","black"),
       lty=1:1, cex=0.75,lwd=2)

janeiro = subset(FAZ_LENS8_specs,V1 >= '2019-01-01' & V1 <= '2019-01-30')

mth_janeiro = subset(janeiro,Site =='2B')
mth_janeiro_s2 = spectral.resampling(subset(janeiro,Site =='2B'),'Sentinel2')
mth_janeiro_l8 = spectral.resampling(subset(janeiro,Site == '2B'),'Landsat8')
mth_janeiro_MODIS = spectral.resampling(subset(janeiro,Site == '2B'),'MODIS')

plot(mth_janeiro_s2,FUN = median,col=c(''),xaxt='n',
     ylim = c(0,50),xlim = c(350,2300))
plot_regions(spectras_LENS8, regions = default_spec_regions(), add = TRUE)


lines(x = mth_janeiro@wavelength , y = colMeans(mth_janeiro@spectra@spectra_ma), col=c('red'),lwd=3)

#lines(x = mth_janeiro_s2@wavelength , y = colMeans(mth_janeiro_s2@spectra@spectra_ma), col=c('red'))
points(x = mth_janeiro_s2@wavelength , y = colMeans(mth_janeiro_s2@spectra@spectra_ma), col=c('blue'),bg =c('blue'),pch = 21)

#lines(x = mth_janeiro_l8@wavelength , y = colMeans(mth_janeiro_l8@spectra@spectra_ma), col=c('red'))
points(x = mth_janeiro_l8@wavelength , y = colMeans(mth_janeiro_l8@spectra@spectra_ma), col=c('darkgreen'),bg =c('darkgreen'),pch = 22)

points(x = mth_janeiro_MODIS@wavelength , y = colMeans(mth_janeiro_MODIS@spectra@spectra_ma), col=c('magenta'),bg =c('magenta'),pch = 23)

axis(side = 1, at=seq(300,2300,100),labels = seq(300,2300,100))
grid()
legend("topleft",legend=c("Raw", "S2",'L8'), col=c("red", "blue",'darkgreen'),
       lty=c(1,0,0),pch = c(0,1,0), cex=0.75,lwd=2,bg =c('white','blue','darkgreen'))

labesl_here = c('S2 - B1','S2 - B2','S2 - B3','S2 - B4','S2 - B5','S2 - B6','S2 - B7',
                'S2 - B8','S2 - B8A', 'S2 - B9','S2 - B10', 'S2 - B11', 'S2 - B12')

pointLabel(x = mth_janeiro_s2@wavelength , y = colMeans(mth_janeiro_s2@spectra@spectra_ma),
           labels = as.character(labesl_here), cex =0.75,offset =0.5 ,
           allowSmallOverlap = FALSE,doPlot = TRUE,trace = TRUE, method = 'GA')

pointLabel(x = mth_janeiro_l8@wavelength , y = colMeans(mth_janeiro_l8@spectra@spectra_ma),
           labels = as.character(c('L8 - B1','L8 - B2','L8 - B3','L8 - B4','L8 - B5','L8 - B9','L8 - B6','L8 - B7')), cex =0.75,offset =0.5 ,
           allowSmallOverlap = FALSE,doPlot = TRUE,trace = TRUE, method = 'GA')


text(x = mth_janeiro_s2@wavelength , y = colMeans(mth_janeiro_s2@spectra@spectra_ma),
     labels = c('S2 - B1','S2 - B2','S2 - B3','S2 - B4','S2 - B5','S2 - B6','S2 - B7',
                'S2 - B8','S2 - B8A', 'S2 - B9','S2 - B10', 'S2 - B11', 'S2 - B12'), pos = 2)

text(x = mth_janeiro_l8@wavelength , y = colMeans(mth_janeiro_l8@spectra@spectra_ma),
     labels = c('L8 - B1','L8 - B2','L8 - B3','L8 - B4','L8 - B5','L8 - B9','L8 - B6','L8 - B7'), pos = 4)

#plot(ndvi_media[1:2],)


#####################################################
nspectra(FAZ_PROBE_specs)

#ABRIL
FAZ_MRD_fill_abril= subset(FAZ_MRD_fill,V1 >= '2018-04-01' & V1 <= '2018-04-30' & Time <= 143000 & Target == 'Brachiaria brizantha')
FAZ_02B_fill_abril= subset(FAZ_02b_fill,V1 >= '2018-04-01' & V1 <= '2018-04-30' & Time <= 173000 & Target == 'Brachiaria brizantha')
FAZ_MDC_fill_abril= subset(FAZ_MDC_fill,V1 >= '2018-04-01' & V1 <= '2018-04-30' & Time <= 143000 & Target == 'Brachiaria brizantha')
FAZ_MTH_fill_abril= subset(FAZ_MTH_fill,V1 >= '2018-04-01' & V1 <= '2018-04-30' & Time <= 143000 & Target == 'Brachiaria brizantha')
FAZ_ALP_fill_abril= subset(FAZ_ALP_fill,V1 >= '2018-04-01' & V1 <= '2018-04-30' & Time <= 163000 & Target == 'Brachiaria brizantha')

FAZ_MRD_fill_agosto = subset(FAZ_MRD_fill,V1 >= '2018-08-01' & V1 <= '2018-08-31' & Time <= 143000 & Target == 'Brachiaria brizantha')
FAZ_02B_fill_agosto = subset(FAZ_02b_fill,V1 >= '2018-08-01' & V1 <= '2018-08-31' & Time <= 143000 & Target == 'Brachiaria brizantha')
FAZ_MDC_fill_agosto = subset(FAZ_MDC_fill,V1 >= '2018-08-01' & V1 <= '2018-08-31' & Time <= 143000 & Target == 'Brachiaria brizantha')
FAZ_MTH_fill_agosto = subset(FAZ_MTH_fill,V1 >= '2018-08-01' & V1 <= '2018-08-31' & Time <= 143000 & Target == 'Brachiaria brizantha')
FAZ_ALP_fill_agosto = subset(FAZ_ALP_fill,V1 >= '2018-08-01' & V1 <= '2018-08-31' & Time <= 163000 & Target == 'Brachiaria brizantha')

#MTH
FAZ_MTH_fill_maio= subset(FAZ_MTH_fill,V1 >= '2018-05-01' & V1 <= '2018-05-31' & Time <= 143000 & Target == 'Brachiaria brizantha')
FAZ_MTH_fill_agosto= subset(FAZ_MTH_fill,V1 >= '2018-08-01' & V1 <= '2018-08-30' & Time <= 143000 & Target == 'Brachiaria brizantha')

FAZ_MTH_fill_abril_S2 = spectral.resampling(FAZ_MTH_fill_abril,'Sentinel2')
FAZ_MTH_fill_maio_S2 = spectral.resampling(FAZ_MTH_fill_maio,'Sentinel2')
FAZ_MTH_fill_agosto_S2 = spectral.resampling(FAZ_MTH_fill_agosto,'Sentinel2')

FAZ_MDC_fill_abril= subset(FAZ_MDC_fill,V1 >= '2018-04-01' & V1 <= '2018-04-30' & Time <= 143000 & Target == 'Brachiaria brizantha')
FAZ_MDC_fill_maio= subset(FAZ_MDC_fill,V1 >= '2018-05-01' & V1 <= '2018-05-31' & Time <= 143000 & Target == 'Brachiaria brizantha')
FAZ_MDC_fill_agosto= subset(FAZ_MDC_fill,V1 >= '2018-08-01' & V1 <= '2018-08-31' & Time <= 143000 & Target == 'Brachiaria brizantha')

NDVI = "(R780-R680)/(R780+R680)"

mean(vegindex(FAZ_MTH_fill_abril, NDVI))
mean(vegindex(FAZ_MTH_fill_abril_S2, NDVI))

mean(vegindex(FAZ_MTH_fill_maio, NDVI))
mean(vegindex(FAZ_MTH_fill_maio_S2, NDVI))

mean(vegindex(FAZ_MTH_fill_agosto, NDVI))
mean(vegindex(FAZ_MTH_fill_agosto_S2, NDVI))

nspectra(FAZ_MDC_fill_abril)
nspectra(FAZ_MDC_fill_maio)
nspectra(FAZ_MDC_fill_agosto)

#plot(FAZ_MTH_fill_abril,FUN = 1,ylim = c(0,70),xlim = c(350,2300),
#     main = 'Valores espectrais médios de Brachiaria brizantha \n Fazenda Matinha - Jussara/GO - 2018',xaxt='n',col=c('red'))
plot(FAZ_MDC_fill_abril,FUN = mean,ylim = c(0,70),xlim = c(350,2300),
    ,xaxt='n',col=c('forestgreen'))

#LOOPS
for (i in 2:nspectra(FAZ_MDC_fill_abril)){
  lines(x = FAZ_MTH_fill_abril@wavelength , y = FAZ_MDC_fill_abril@spectra@spectra_ma[i,], col=c('forestgreen'))
}
for (i in 1:nspectra(FAZ_MTH_fill_abril)){
  lines(x = FAZ_MDC_fill_maio@wavelength , y = FAZ_MDC_fill_maio@spectra@spectra_ma[i,], col=c('royalblue'))
}
for (i in 1:nspectra(FAZ_MTH_fill_agosto)){
  lines(x = FAZ_MDC_fill_agosto@wavelength , y = FAZ_MDC_fill_agosto@spectra@spectra_ma[i,], col=c('darkgreen'))
}

#MEANS
lines(x = FAZ_MDC_fill_agosto@wavelength , y = colMeans(FAZ_MDC_fill_agosto@spectra@spectra_ma),col=c('darkgreen'),lwd=5)
lines(x = FAZ_MDC_fill_maio@wavelength , y = colMeans(FAZ_MDC_fill_maio@spectra@spectra_ma),col=c('royalblue'),lwd=5)
lines(x = FAZ_MDC_fill_abril@wavelength , y = colMeans(FAZ_MDC_fill_abril@spectra@spectra_ma),col=c('red'),lwd=5)
#OTHERS
axis(side = 1, at=seq(300,2300,100),labels = seq(300,2300,100))
plot_regions(spectras_LENS8, regions = default_spec_regions(), add = TRUE)
grid()
legend("topleft",legend=c("Abril - 14 espectros", "Maio - 24 espectros",'Agosto - 20 espectros'), col=c("red", "royalblue",'darkgreen'),lty=1:1, cex=1,lwd=2)
####################################################################

plot(FAZ_02B_fill_abril,FUN = mean,ylim = c(0,100),xlim = c(350,2300),
     ,xaxt='n',col=c('forestgreen'))

#LOOPS
for (i in 2:nspectra(FAZ_02B_fill_abril)){
  lines(x = FAZ_02B_fill_abril@wavelength , y = FAZ_02B_fill_abril@spectra@spectra_ma[i,], col=c('forestgreen'))
}

axis(side = 1, at=seq(300,2300,100),labels = seq(300,2300,100))
grid()

plot(FAZ_02B_fill_agosto,FUN = mean,ylim = c(0,100),xlim = c(350,2300),
     ,xaxt='n',col=c('red'))

#LOOPS
for (i in 2:nspectra(FAZ_02B_fill_agosto)){
  lines(x = FAZ_02B_fill_agosto@wavelength , y = FAZ_02B_fill_agosto@spectra@spectra_ma[i,], col=c('red'))
}

axis(side = 1, at=seq(300,2300,100),labels = seq(300,2300,100))
grid()