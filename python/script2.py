# -*- coding: utf-8 -*-
import urllib
import csv
import sys
import xml.etree.ElementTree as ET
import math

url = "http://wservice.viabicing.cat/getstations.php?v=1"
xml = urllib.urlopen(url).read()
urllib.urlopen(url).close()
root = ET.fromstring(xml);

file   = open('restaurants.csv', 'rb')
reader = iter(csv.reader(file, delimiter = ','))
reader.next()

def deg2rad(deg):
  return deg * (math.pi/180)

def radiOK(dist):
	return dist < 1

def distance(lat1,lon1,lat2,lon2):
	dLat = deg2rad(lat2-lat1)
	dLon = deg2rad(lon2-lon1)
	a = math.sin(dLat/2)*math.sin(dLat/2)+math.cos(deg2rad(lat1))*math.cos(deg2rad(lat2))*math.sin(dLon/2) * math.sin(dLon/2)
	c = 2 * math.atan2(math.sqrt(a), math.sqrt(1-a))
	return 6371*c

def getBikes(lat,lon):
	bikes_avl = []
	slots_avl = []
	for station in root.findall('station'):
		lat = station.find('lat').text
		lon = station.find('long').text
		dist = distance(float(lat),float(lon),float(lat),float(lon))
		if radiOK(dist):
			st   = (station,dist)
			if  int(station.find('bikes').text) > 0: bikes_avl.append(st)
			if  int(station.find('slots').text) > 0: slots_avl.append(st)
	return (sorted(bikes_avl, key=lambda station: station[1]),
			sorted(slots_avl, key=lambda station: station[1]))


def check(params, nom):
	if   isinstance(params, str)   : return (nom.find(params) != -1)
	elif isinstance(params, tuple) or isinstance(params, list):
		for i in params:
			if check(i, nom): return True
		return False

def table_design():
	return """<html>
		<head>
			<meta charset="utf-8"/>
			<title>Practica Python</title>
		</head>
		<body>
			<right><h3>Ricard Meyerhofer Parra - LP Q2 2015-2016 - </h3><h1>Practica Python</h1></right>
		  	<table border="1"> <center>
		    	<thead  bgcolor="orange">
		    		<th><h2>Nom Rest</h2></th>
		    		<th><h2>Dades Rest</h2></th>
		    		<th><h2>Slots disp</h2></th>
		    		<th><h2>Bicis disp</h2></th>
		    	</thead>"""
def main():
	if len(sys.argv) == 2:
		params  = eval(sys.argv[1])

		html = table_design()
		for row in reader:
			if(check(params, row[0])):
				html += "<tr><td>" + "<b>" + row[0] + "</b>"+ "</td>" + "<td>" +  row[3]  + "<br>" + row[6]
				html += "<br>" + row[9] + "<br>" + row[8] + "<br>"+ row[12] + "<br>" + "</td>" + "<td>"
				stations = getBikes(row[1],row[2])
				for station in stations[0]:
					html += station[0].find('street').text + "<br> "
				html += "</td><td>"
				for station in stations[1]:
					html += station[0].find('street').text + "<br> "
				html += "</td>"
		html += "</center></table></div></body></html>"
		doc = open('table.html','w')
		doc.write(html)
		doc.close()
	else: print ("Es requereixen dos parametres")

main()