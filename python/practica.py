#!/usr/bin/python
# -*- coding: utf-8 -*-
import sys

from HTMLParser import HTMLParser

allrest=[]

class restaurant: 

    def afegir_nom(self,nom):
        self.nom = nom
    def afegir_sect(self,sect):
        self.sect = sect

    #def afegir_adre(self,nom):
     #   self.nom = nom
    #def afegir_lat(self,):
    #def afegir_lon():
    #def afegir_telf():
    #def afegir_mail():
    #def afegir_web():
    #def afegir_barri():

        
# creem una subclasse i sobreescribim el metodes del han
class MHTMLParser(HTMLParser):

    crest = restaurant()
    ctag = ""

    def handle_starttag(self, tag, attrs):
        self.ctag = tag
        if tag == 'v:vcard':
            self.crest = restaurant()

    def handle_endtag(self, tag):
        self.ctag = ""
        if tag == 'v:vcard':
            allrest.append(self.crest)

    def handle_data(self, data):
        if self.ctag == 'v:fn':
            self.crest.afegir_nom(data)
        elif self.ctag == 'xv:section':
            self.crest.afegir_sect(data)

        #elif self.ctag == 'v:adapted':
        #elif self.ctag == 'v:adr':
        #elif self.ctag == 'v:geo':
        #elif self.ctag == 'v:tel':
        #elif self.ctag == 'xv:schedule':
        #elif self.ctag == 'dct:isPartOf':
        #elif self.ctag == 'v:category':
        #elif self.ctag == 'dct:created':
        #elif self.ctag == 'v:rev':'''


f = open('restaurants.rdf', 'rb') # obre l'arxiu
rdfSource = f.read()                            
f.close()

parser = MHTMLParser()
parser.feed(rdfSource)
print len(allrest)
for r in allrest:
    print r.nom
    print r.sect

