deg2rad <- function(deg) {(deg * pi) / (180)}

eq = function(ang,dist){pi*(tan(deg2rad(ang)/2)*dist)^2}
plot(eq(8,seq(0,150,5)), type='p',xlab='distancia (cm)',ylab = 'Area (cm²)')
print(eq(8,100))


library(Hmisc)
library(spectrolab)
library(hsdar)

#filePath = "C://Users//vinicius.mesquita//Downloads//BHRV_Spectrum//Spectral Evolution//"
#filePath = 'I://SPECTRAL_DATA//BHRV_vinicius_spectros//Maio_2018//Campo_14_05_2018_BHRV//Faz_morador//LENS8//Brachiaria//'
#filePath =  'I://SPECTRAL_DATA//AgroTools//Faz_Eldorado_02_02_2018//Organized//ALL//'

filePath = 'I://SPECTRAL_DATA//BHRV_vinicius_spectros//Maio_2018//Campo_14_05_2018_BHRV//Faz_morador//LENS8//Brachiaria//'

spectrums = list.files(path = filePath, pattern = ".sed")

spectras = NULL

for (i in 1:length(spectrums)){
  if (i == 1){
    spectras = read_spectra(path = paste0(filePath,spectrums[i]), format = "sed")
  }
  else{
    spectras = combine(spectras,read_spectra(path = paste0(filePath,spectrums[i]), format = "sed"))
  }
}

spec_new <- speclib(reflectance(spectras)*100, wavelengths(spectras))

sgolay <- noiseFiltering(spec_new, method="sgolay", n=29,p=4)

spec_new <- noiseFiltering(sgolay, method="spline", 
                           n=round(nbands(spec_new)/7,0))

mask(spec_new) = c(1800,1950,1350,1450,2400,2600)

#spec_new = greenProbe

FDR = derivative.speclib(spec_new, m = 1, method = "sgolay", n=7,p=3)

maxInfo = c()
for (i in 1:length(spec_new@spectra[1,])){
  maxInfo[i] = max(spec_new@spectra[,i])
}

#axis(1, at=seq(350, 2350, 100))
#axis(2, at=seq(0, 20, 5))

plot(spec_new,FUN=1:18, lwd = 0.75,type="l",
     ylim = c(0,100),xlim = c(350,2300),
     lty = 1,xlab = 'Wavelenght (nm)', ylab = 'Reflectance (%)',
     panel.first=grid(),main = "Brachiaria brizantha - Raw",
     xaxt='n')

plot_regions(spectras, regions = default_spec_regions(), add = TRUE)
axis(side = 1, at=seq(300,2300,150),labels = seq(300,2300,150))
grid()

data_RE <- spectralResampling(spec_new, 'Landsat8',response_function = TRUE)

maxInfo = c()
for (i in 1:length(data_RE@spectra[1,])){
  maxInfo[i] = max(data_RE@spectra[,i])
}

plot(data_RE,FUN=1:18, lwd = 0.75,type="b",
     ylim = c(0,100),xlim = c(350,2300),
     lty = 1,xlab = 'Wavelenght (nm)', ylab = 'Reflectance (%)',
     col=c('orange','darkgreen','darkgreen','darkgreen','darkgreen','darkgreen'),
     panel.first=grid(),main = "Brachiaria brizantha - Landsat 8",
     xaxt='n')

plot_regions(spectras, regions = default_spec_regions(), add = TRUE)
axis(side = 1, at=seq(300,2300,150),labels = seq(300,2300,150))
text(maxInfo ~data_RE@wavelength, labels=c('CB','B','G','R','N','SC','S1','S2'))
grid()
#text(x=seq(300,2300,150), y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]),
#     labels=seq(300,2300,150), srt=90, adj=0.5, xpd=TRUE)

data_RE <- spectralResampling(spec_new, "Sentinel2",response_function = TRUE)

maxInfo = c()
for (i in 1:length(data_RE@spectra[1,])){
  maxInfo[i] = max(data_RE@spectra[,i])
}

plot(data_RE, lwd = 0.75,type="b",
     ylim = c(0,100),xlim = c(350,2300),
     lty = 1,xlab = 'Wavelenght (nm)', ylab = 'Reflectance (%)',
     col=c('orange','darkgreen','darkgreen','darkgreen','darkgreen','darkgreen'),
     panel.first=grid(),main = "Brachiaria brizantha - Sentinel 2",
     xaxt='n')

plot_regions(spectras, regions = default_spec_regions(), add = TRUE)
axis(side = 1, at=seq(300,2300,150),labels = seq(300,2300,150))
text(maxInfo ~data_RE@wavelength, labels=c('CB','B','G','R','RE1','RE2','RE3','N','RE4','WV','SC','S1','S2'))
grid()

data_RE <- spectralResampling(spec_new, "MODIS",response_function = TRUE)

maxInfo = c()
for (i in 1:length(data_RE@spectra[1,])){
  maxInfo[i] = max(data_RE@spectra[,i])
}

plot(data_RE, lwd = 0.75,type="b",
     ylim = c(0,100),xlim = c(350,2300),
     lty = 1,xlab = 'Wavelenght (nm)', ylab = 'Reflectance (%)',
     col=c('orange','darkgreen','darkgreen','darkgreen','darkgreen','darkgreen'),
     panel.first=grid(),main = "Brachiaria brizantha - MODIS",
     xaxt='n')

plot_regions(spectras, regions = default_spec_regions(), add = TRUE)
axis(side = 1, at=seq(300,2300,150),labels = seq(300,2300,150))
text(maxInfo ~data_RE@wavelength, labels=c('B','G','R','N','S0','S1','S2'))
grid()

#text(maxInfo ~data_RE@wavelength, labels=c('CB','B','G','R','RE1','RE2','RE3','N','RE4','WV','SC','S1','S2'))

par(mfrow = c(2, 2))
#
#plot(spec_new[1:10], lwd = 0.75, lty = 1, col = "grey25", main = "All Spectra")


#file = "C://Users//vinicius.mesquita//Downloads//BHRV_Spectrum//Spectral Evolution//1418085_00098.sed"
#
#dir_path = system.file(file, package = "spectrolab")
#
#acer_spectra = read_spectra(path = file, format = "sed")
#
#n = names(acer_spectra)
#w = wavelengths(acer_spectra)
#r = reflectance(acer_spectra)
#m = meta(acer_spectra, "ssp", simplify = TRUE)
#
#spec_sub_vis = acer_spectra[ , wavelengths(acer_spectra, 350, 2400) ]
#spec = resample(acer_spectra, new_wvls = seq(350, 2400, 0.5), parallel = FALSE)
#
#plot(c(spec_sub_vis,spec)))

par(mfrow = c(1, 3))

data_RE <- spectralResampling(spec_new, "Landsat8",response_function = TRUE)
maxInfo = c()
for (i in 1:length(data_RE@spectra[1,])){
  maxInfo[i] = max(data_RE@spectra[,i])
}

plot(data_RE, lwd = 0.75,type="b",
     ylim = c(0,1),xlim = c(350,2300),
     lty = 1,xlab = 'Wavelenght (nm)', ylab = 'Reflectance (10^-2%)',
     col=c('orange','darkgreen','darkgreen','darkgreen','darkgreen','darkgreen'),
     panel.first=grid(),main = "Spline",
     xaxt='n')
plot_regions(spectras, regions = default_spec_regions(), add = TRUE)
axis(side = 1, at=seq(300,2300,150),labels = seq(300,2300,150))
text(maxInfo ~data_RE@wavelength, labels=c('CB','B','G','R','N','SC','S1','S2'))

data_RE <- spectralResampling(sgolay, "Landsat8",response_function = TRUE)
maxInfo = c()
for (i in 1:length(data_RE@spectra[1,])){
  maxInfo[i] = max(data_RE@spectra[,i])
}

plot(data_RE, lwd = 0.75,type="b",
     ylim = c(0,1),xlim = c(350,2300),
     lty = 1,xlab = 'Wavelenght (nm)', ylab = 'Reflectance (10^-2%)',
     col=c('orange','darkgreen','darkgreen','darkgreen','darkgreen','darkgreen'),
     panel.first=grid(),main = "SaVgol",
     xaxt='n')
plot_regions(spectras, regions = default_spec_regions(), add = TRUE)
axis(side = 1, at=seq(300,2300,150),labels = seq(300,2300,150))
text(maxInfo ~data_RE@wavelength, labels=c('CB','B','G','R','N','SC','S1','S2'))


file= 'C://Users//Vinicius_Note//Downloads//Maio_2018-20180815T174203Z-001//Maio_2018//Campo_14_05_2018_BHRV//Faz_morador//PROBE//Brachiaria//Verde//1418085_00299.sed'

greenProbe = read_spectra(file, format = "sed")
greenProbe <- speclib(reflectance(greenProbe), wavelengths(greenProbe))
greenProbe <- noiseFiltering(greenProbe, method="spline", 
                           n=round(nbands(greenProbe)/10,0))

data_RE <- spectralResampling(greenProbe, "Landsat8",response_function = TRUE)

maxInfo = c()
for (i in 1:length(data_RE@spectra[1,])){
  maxInfo[i] = max(data_RE@spectra[,i])
}

plot(data_RE, lwd = 0.75,type="b",
     ylim = c(0,1),xlim = c(350,2300),
     lty = 1,xlab = 'Wavelenght (nm)', ylab = 'Reflectance (10^-2%)',
     col=c('orange','darkgreen','darkgreen','darkgreen','darkgreen','darkgreen'),
     panel.first=grid(),main = "Probe Green Leaf",
     xaxt='n')
plot_regions(spectras, regions = default_spec_regions(), add = TRUE)
axis(side = 1, at=seq(300,2300,150),labels = seq(300,2300,150))
text(maxInfo ~data_RE@wavelength, labels=c('CB','B','G','R','N','SC','S1','S2'))

SplineD <- spectralResampling(spec_new, "Landsat8",response_function = TRUE)
SavgolD <- spectralResampling(sgolay, "Landsat8",response_function = TRUE)
ProbeD <- spectralResampling(greenProbe, "Landsat8",response_function = TRUE)

vegindex(SplineD[1], "0.5*(R2000+R2194.5)-R2100")
vegindex(SavgolD[1], "(R800-R680) / (R800+R680)")
vegindex(ProbeD[1], "(R800-R680) / (R800+R680)")

((R678-R500)/R750)
