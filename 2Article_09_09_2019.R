library(lubridate)
library(Hmisc)
library(spectrolab)
library(hsdar)
library(ggplot2)

makeSpectras = function(filePath){
  spectrums = filePath
  spectras = NULL
  
  for (i in 1:length(spectrums)){
    if (i == 1){
      spectras = read_spectra(path = spectrums[[i]], format = "sed")
    }
    else{
      spectras = combine(spectras,read_spectra(path = spectrums[[i]], format = "sed"))
    }
  }
  return(spectras)
}

Faz = read.table(file = 'file:///D:/SPEC/2Process/BHRV/ALL/spectrums_bhrv_vvm.csv',
                    sep = ';',header = T)

#gdBaseSpectra = as.data.frame(read.table(file = 'file:///C:/Users/Vinicius-DKTP/Desktop/SBSR_2019_ARTIGO/Resultados/DATA/spectrums_bhrv_vvm.csv',
#                 sep = ';', dec = '.',header = T))

#FazMRD = read.table(file = 'file:///C:/Users/Vinicius-DKTP/Desktop/SBSR_2019_ARTIGO/Resultados/DATA/FAZ_MORADOR/spectrums_bhrv_FAZ_MORADOR.csv',
#                                  sep = ';',header = T)
#Faz2B = read.table(file = 'file:///C:/Users/Vinicius-DKTP/Desktop/SBSR_2019_ARTIGO/Resultados/DATA/FAZ_2B/spectrums_bhrv_FAZ_2B.csv',
#                                 sep = ';',header = T)
#FazMDC = read.table(file = 'file:///C:/Users/Vinicius-DKTP/Desktop/SBSR_2019_ARTIGO/Resultados/DATA/FAZ_MATA_DA_CHUVA/spectrums_bhrv_FAZ_MATA_DA_CHUVA.csv',
#                                  sep = ';',header = T)
#FazMTH = read.table(file = 'file:///C:/Users/Vinicius-DKTP/Desktop/SBSR_2019_ARTIGO/Resultados/DATA/FAZ_MATINHA/spectrums_bhrv_FAZ_MATINHA.csv',
#                                  sep = ';',header = T)
#FazALP = read.table(file = 'file:///C:/Users/Vinicius-DKTP/Desktop/SBSR_2019_ARTIGO/Resultados/DATA/FAZ_AGUA_LIMPA/spectrums_bhrv_FAZ_AGUA_LIMPA.csv',
#                                  sep = ';',header = T)

dateFormat = "%m/%d/%Y"

Faz$Date <- as.Date(Faz$Date, dateFormat)
#Faz2B$Date <- as.Date(Faz2B$Date, dateFormat)
#FazMDC$Date <- as.Date(FazMDC$Date, dateFormat)
#FazMTH$Date <- as.Date(FazMTH$Date, dateFormat)
#FazALP$Date <- as.Date(FazALP$Date, dateFormat)

Faz$Time <- as.numeric(gsub(':','',Faz$Time))
#Faz2B$Time <-  as.numeric(gsub(':','',Faz2B$Time))
#FazMDC$Time <- as.numeric(gsub(':','',FazMDC$Time))
#FazMTH$Time <- as.numeric(gsub(':','',FazMTH$Time))
#FazALP$Time <- as.numeric(gsub(':','',FazALP$Time))

Fazsub_LENS8 = droplevels.data.frame(subset(Faz, Foreoptic=="LENS8"))
Fazsub_PROBE = droplevels.data.frame(subset(Faz, Foreoptic=="PROBE"))
#Faz2Bsub = droplevels.data.frame(subset(Faz2B, Foreoptic=="LENS8"))
#FazMDCsub = droplevels.data.frame(subset(FazMDC, Foreoptic=="LENS8"))
#FazMTHsub = droplevels.data.frame(subset(FazMTH, Foreoptic=="LENS8"))
#FazALPsub = droplevels.data.frame(subset(FazALP, Foreoptic=="LENS8"))

list_LENS8 = as.character(Fazsub_LENS8$Filename)
list_PROBE = as.character(Fazsub_PROBE$Filename)

#list2B = as.character(Faz2Bsub$Filename)
#listMDC = as.character(FazMDCsub$Filename)
#listMTH = as.character(FazMTHsub$Filename)
#listALP = as.character(FazALPsub$Filename)

spectras_LENS8 = makeSpectras(list_LENS8)
spectras_PROBE = makeSpectras(list_PROBE)
#spectras_2B  = makeSpectras(list2B)
#spectras_MDC = makeSpectras(listMDC)
#spectras_MTH = makeSpectras(listMTH)
#spectras_ALP = makeSpectras(listALP)

##########################################################################

RAW_LENS8 = speclib(spectra = reflectance(spectras_LENS8)*100,
                            wavelength= wavelengths(spectras_LENS8),
                            SI=c(Fazsub_LENS8$Date))
SI(RAW_LENS8)$Time = Fazsub_LENS8$Time
SI(RAW_LENS8)$Site = as.character(Fazsub_LENS8$Site)
SI(RAW_LENS8)$Target = as.character(Fazsub_LENS8$Target)
SI(RAW_LENS8)$Characteristic = as.character(Fazsub_LENS8$Characteristic)
SI(RAW_LENS8)$Specific = as.character(Fazsub_LENS8$Specific)
SI(RAW_LENS8)$Foreoptic = as.character(Fazsub_LENS8$Foreoptic)
SI(RAW_LENS8)$Filename = as.character(apply(as.data.frame(Fazsub_LENS8$Filename),1,basename))
SI(RAW_LENS8)$FileDate = as.Date(apply(as.data.frame(Fazsub_LENS8$Filename),1,function(x) gsub('_','-',substr(strsplit(as.character(x),split="\\\\")[[1]][7], 0, 10))),"%d-%m-%Y")



RAW_PROBE = speclib(spectra = reflectance(spectras_PROBE)*100,
                    wavelength= wavelengths(spectras_PROBE),
                    SI=c(as.character(Fazsub_PROBE$Date)))
SI(RAW_PROBE)$Time = Fazsub_PROBE$Time
SI(RAW_PROBE)$Site = as.character(Fazsub_PROBE$Site)
SI(RAW_PROBE)$Target = as.character(Fazsub_PROBE$Target)
SI(RAW_PROBE)$Characteristic = as.character(Fazsub_PROBE$Characteristic)
SI(RAW_PROBE)$Specific = as.character(Fazsub_PROBE$Specific)
SI(RAW_PROBE)$Foreoptic = as.character(Fazsub_PROBE$Foreoptic)
SI(RAW_PROBE)$Filename = as.character(apply(as.data.frame(Fazsub_PROBE$Filename),1,basename))
SI(RAW_PROBE)$FileDate = as.Date(apply(as.data.frame(Fazsub_PROBE$Filename),1,function(x) gsub('_','-',substr(strsplit(as.character(x),split="\\\\")[[1]][7], 0, 10))),"%d-%m-%Y")





#RAW_2B = speclib(spectra = reflectance(spectras_2B)*100,
#                 wavelength= wavelengths(spectras_2B),
#                 SI=c(Faz2Bsub$Date))
#SI(RAW_2B)$Time = Faz2Bsub$Time
#SI(RAW_2B)$Site = as.character(Faz2Bsub$Site)
#SI(RAW_2B)$Target = as.character(Faz2Bsub$Target)
#SI(RAW_2B)$Characteristic = as.character(Faz2Bsub$Characteristic)
#SI(RAW_2B)$Specific = as.character(Faz2Bsub$Specific)
#SI(RAW_2B)$Foreoptic = as.character(Faz2Bsub$Foreoptic)
#
#RAW_MDC = speclib(spectra = reflectance(spectras_MDC)*100,
#                  wavelength= wavelengths(spectras_MDC),
#                  SI=c(FazMDCsub$Date))
#SI(RAW_MDC)$Time = FazMDCsub$Time
#SI(RAW_MDC)$Site = as.character(FazMDCsub$Site)
#SI(RAW_MDC)$Target = as.character(FazMDCsub$Target)
#SI(RAW_MDC)$Characteristic = as.character(FazMDCsub$Characteristic)
#SI(RAW_MDC)$Specific = as.character(FazMDCsub$Specific)
#SI(RAW_MDC)$Foreoptic = as.character(FazMDCsub$Foreoptic)
#
#RAW_MTH = speclib(spectra = reflectance(spectras_MTH)*100,
#                  wavelength= wavelengths(spectras_MTH),
#                  SI=c(FazMTHsub$Date))
#SI(RAW_MTH)$Time = FazMTHsub$Time
#SI(RAW_MTH)$Site = as.character(FazMTHsub$Site)
#SI(RAW_MTH)$Target = as.character(FazMTHsub$Target)
#SI(RAW_MTH)$Characteristic = as.character(FazMTHsub$Characteristic)
#SI(RAW_MTH)$Specific = as.character(FazMTHsub$Specific)
#SI(RAW_MTH)$Foreoptic = as.character(FazMTHsub$Foreoptic)
#
#RAW_ALP = speclib(spectra = reflectance(spectras_ALP)*100,
#                  wavelength= wavelengths(spectras_ALP),
#                  SI=c(FazALPsub$Date))
#SI(RAW_ALP)$Time = FazALPsub$Time
#SI(RAW_ALP)$Site = as.character(FazALPsub$Site)
#SI(RAW_ALP)$Target = as.character(FazALPsub$Target)
#SI(RAW_ALP)$Characteristic = as.character(FazALPsub$Characteristic)
#SI(RAW_ALP)$Specific = as.character(FazALPsub$Specific)
#SI(RAW_ALP)$Foreoptic = as.character(FazALPsub$Foreoptic)

#######################################################################

rm(dateFormat)

rm(list_LENS8 , Fazsub_LENS8 , Faz,list_PROBE , Fazsub_PROBE)
#rm(spectras_2B  , list2B  , Faz2Bsub  , Faz2B )
#rm(spectras_MDC , listMDC , FazMDCsub , FazMDC)
#rm(spectras_MTH , listMTH , FazMTHsub , FazMTH)
#rm(spectras_ALP , listALP , FazALPsub , FazALP)


