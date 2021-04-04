import os
import sys
import glob
import csv
import re
import pathlib

#dirSpectras = '/data/DADOS02/SPECTRAL_DATA/AgroTools/Faz_Eldorado_02_02_2018/Spectrums/'
#dirSpectrasList = ['/data/DADOS02/BACKUP_12/SPECTRAL_DATA/BHRV_vinicius_spectros/Agosto_2018/DATA/',
#				'/data/DADOS02/BACKUP_12/SPECTRAL_DATA/BHRV_vinicius_spectros/Abril_2018/DATA/',
#				'/data/DADOS02/BACKUP_12/SPECTRAL_DATA/BHRV_vinicius_spectros/Maio_2018/DATA/',
#				'/data/DADOS02/BACKUP_12/SPECTRAL_DATA/BHRV_vinicius_spectros/Outubro_2018/DATA/',
#				'/data/DADOS02/BACKUP_12/SPECTRAL_DATA/BHRV_vinicius_spectros/Novembro_2018/DATA/',
#				'/data/DADOS02/BACKUP_12/SPECTRAL_DATA/BHRV_vinicius_spectros/Dezembro_2018/DATA/',
#dirSpectrasList =['/data/DADOS02/BACKUP_12/SPECTRAL_DATA/BHRV_vinicius_spectros/Janeiro_2019/DATA/']
#dirSpectrasList =['/data/PASTAGEM/Campo/SPECTRAL_DATA/BHRV/BHRV_C10/DATA/']

dirSpectras ="D:\\SPEC\\2Process\\BHRV\\ALL\\"

fileSpectras = list(pathlib.Path(dirSpectras).glob('**/*.sed'))
	
infoSpectras = []
	
header = ['Filename','Site','Target','Characteristic','Specific','Instrument','Detectors','Measurement','Date','Time','Temperature (C)','Battery Voltage',
'Averages','Integration','Dark Mode','Foreoptic','Radiometric Calibration','Units',
'Wavelength Range','Latitude','Longitude','Altitude','GPS Time','Satellites']
	
print(len(fileSpectras))
	
def dms2dd(degrees, minutes, seconds, direction):
    dd = float(degrees) + float(minutes)/60.0 + float(seconds)/(60.0*60.0);
    if direction == 'W' or direction == 'S':
        dd *= -1
    return dd;	

for spec in range(0,len(fileSpectras)):
	openSpectra = open(fileSpectras[spec],'r').readlines()[3:22] 
	data = []
	#nameSplit = (re.split('Spectrums.',fileSpectras[spec])[1]).split('\\')
	data.append(fileSpectras[spec])

	site = os.path.dirname(fileSpectras[spec]).split('\\')[5]
	#print(site)

	data.append(site)

	file_name = str(fileSpectras[spec])

	if file_name.find('solo')>-1 or file_name.find('Solo')>-1:

		data.append('Solo')

		if site == '2B':
			data.append('Latossolo Vermelho')

		elif site == 'MTH' or site == 'ALP':
			data.append('Latossolo Amarelo')


		elif site == 'MDC':
			data.append('Neossolo Quartzarenico')


		elif site == 'MRD':
			data.append('Neossolo Litolico')

		else:
			data.append('None')

		if file_name.find('CUPINZEIRO')>-1 or file_name.find('cupinzeiro')>-1:
			data.append('Cupinzeiro')
		else:
			data.append('None')

	elif file_name.find('brachiaria')>-1 or file_name.find('Brachiaria')>-1:
			
		data.append('Brachiaria brizantha')

		if file_name.find('Verde') > -1 or file_name.find('VERDE')> -1:
			data.append('Verde')
		elif file_name.find('Seca') > -1 or file_name.find('SECA')> -1:
			data.append('Seca')
		else:
			data.append('None')

		data.append('None')

	else:
		data.append('Other')
		data.append('None')
		data.append('None')

	#data.append(nameSplit[2])
	#data.append(nameSplit[4])
	
	for i in range(0,len(openSpectra)):

		if i in [3,6,7,9]:
			 if str(openSpectra[i].split(':')[1][1:].split(',')[0]) == '10/01/2012':
			 	data.append('17/01/2019')
			 	continue
			 	
			 data.append(openSpectra[i].split(':')[1][1:].split(',')[0].replace('\n',''))

		elif i == 4:
			data.append(openSpectra[i].split(':',1)[1][1:].split(',',3)[1].replace('\n',''))
		elif i == 10:
			data.append(openSpectra[i].split(':')[1][1:].split(',')[0].replace('\n','').replace(' ',''))
		elif i == 5:
			data.append(openSpectra[i].split(':')[1][1:].split(',',3)[3].replace('\n',''))
		elif i in [14,15]:
			try:
				coords =  re.split('[\xc2\xb0\'\r""]+',openSpectra[i].split(':')[1][1:].replace('\n',''))
				data.append(dms2dd(coords[0],coords[1],coords[2],coords[3]))
			except:
				data.append(openSpectra[i].split(':')[1][1:].replace('\n',''))				
		elif i == 17:
			data.append(openSpectra[i].split(':',1)[1].replace('\n',''))
		else:
			data.append(openSpectra[i].split(':')[1][1:].replace('\n',''))

	#data.append(openSpectra[0].split(':')[1][1:].replace('\n',''))
	#data.append(openSpectra[1].split(':')[1][1:].replace('\n',''))


	#'M-3500_SN1418085 [3]', 'n/a', 'REFLECTANCE', '04/09/2018,04/09/2018', '15', '35.40,8.94,-4.50,35.64,8.94,-4.50',
	#'7.86,7.84', '25,25', '40,50,30,100,50,30', 'AUTO,AUTO', 'LENS8,LENS8', 'RADIANCE', 'W/m^2/sr/nm', '350,2500', 'n/a', 'n/a', 'n/a', 'n/a', 'n/a'
	#for i in range(0,len(openSpectra)):
	#		data.append(openSpectra[i].split(':')[1][1:].replace('\n',''))
	infoSpectras.append(data)
	data = None
		
outputFile = open(dirSpectras + 'spectrums_bhrv_vvm.csv' , 'w')
outputWriter = csv.writer(outputFile,delimiter=';')
outputWriter.writerow(header)

for i in range(0,len(infoSpectras),1):
		outputWriter.writerow((infoSpectras[i]))

infoSpectras = None
outputFile.close()