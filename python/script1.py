#!/usr/bin/python
# -*- coding: utf-8 -*-
import sys
import csv

from HTMLParser import HTMLParser

allrest=[]

class restaurant: 

    def afegir_nom(self,nom):       self.nom = nom
    def afegir_adr(self,adr):       self.adr = adr
    def afegir_distr(self,distr):   self.distr = distr
    def afegir_barri(self,barri):   self.barri = barri
    def afegir_ciutat(self,ciutat): self.ciutat = ciutat
    def afegir_reg(self,reg):       self.reg = reg
    def afegir_pais(self,pais):     self.pais = pais
    def afegir_lat(self,lat):       self.lat = lat
    def afegir_lon(self, lon):      self.lon = lon
    def afegir_web(self,web):       self.web = web
    def afegir_mail(self,mail):     self.mail = mail
    def afegir_codi(self,codi):     self.codi = codi
    def afegir_tel(self,tel):
        if hasattr(self,'telefon'): self.tel.append(tel)
        else:
            self.tel = []
            self.tel.append(tel)

# creem una subclasse i sobreescribim el metodes del han
class MHTMLParser(HTMLParser):

    crest = restaurant()
    ctag = ""

    def handle_starttag(self, tag, attrs):
        self.ctag = tag
        if tag == 'v:vcard':
            self.crest = restaurant()
        elif tag == 'v:url':
            for nom, val in attrs:
                if nom == 'rdf:resource':       self.crest.afegir_web(val)
        elif tag == 'rdf:description':
            for nom, val in attrs:
                if nom == 'rdf:about':          self.crest.afegir_mail(val)

    def handle_endtag(self, tag):
        self.ctag = ""
        if tag == 'v:vcard':                    allrest.append(self.crest)

    def handle_data(self, data):
        if self.ctag == 'v:fn':                 self.crest.afegir_nom(data)
        elif self.ctag == 'v:locality':         self.crest.afegir_ciutat(data)
        elif self.ctag == 'xv:neighborhood':    self.crest.afegir_barri(data)
        elif self.ctag == 'xv:district':        self.crest.afegir_distr(data)
        elif self.ctag == 'v:latitude':         self.crest.afegir_lat(data)
        elif self.ctag == 'v:postal-code':      self.crest.afegir_codi(data)
        elif self.ctag == 'v:longitude':        self.crest.afegir_lon(data)
        elif self.ctag == 'v:region':           self.crest.afegir_reg(data)
        elif self.ctag == 'v:country-name':     self.crest.afegir_pais(data)
        elif self.ctag == 'v:street-address':   self.crest.afegir_adr(data)
        elif self.ctag == 'rdf:value' and data[0] == '+':         self.crest.afegir_tel(data)


f = open('restaurants.rdf', 'rb') # obre l'arxiu
rdfSource = f.read()                            
f.close()

parser = MHTMLParser()
parser.feed(rdfSource)
print len(allrest)

data = open('restaurants.csv','wb')
data2 = csv.writer(data)

head = ["Nom"]+["Telefon"]+["Direccio"]+["Districte"]+["Barri"]+["Ciutat"]+["Codi Postal"]+["Regio"]+["Pais"]+["Lat"]+["Lon"]+["Web"]+["Mail"]
data2.writerows([head])

for r in allrest:
    nom = [r.nom]
    if hasattr(r,'telefon'):
        telf =''
        count = 0
        for it in r.telefon:
            if count == 0:
                count = 1
                telf = telf + it 
            else:
                telf = telf+','+it

    else: telf = '-'

    if hasattr(r,'adress'): adress = [r.adr]
    else:                   adress = ['-----']

    if hasattr(r,'barri'):  barri = [r.barri]
    else:                   barri = ['-----']

    if hasattr(r,'codi'):   codi = [r.codi]
    else:                   codi = ['-----']

    if hasattr(r,'web'):    url = [r.web]
    else:                   url = ['-----']
    if hasattr(r,'mail'):   mail = [r.mail]
    else:                   mail = ['-----']

    if hasattr(r,'distr'):  distr = [r.distr]
    else:                   distr = ['-----']

    ciutat = [r.ciutat]
    regio = [r.reg]
    pais = [r.pais]
    lat = [r.lat]
    lon = [r.lon]

    output = nom + [telf] + adress + distr + barri + ciutat + codi + regio + pais + lat + lon + url + mail
    data2.writerows([output])

data.close()
 