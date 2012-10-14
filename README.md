PVAR-JakRandr
=============

Proiect pentru PVAR -- platformă pentru simulări-automatizări + engine grafic + implementare

Componente
==========

* CoreUtils
    - Interfaţă pentru entităţi, clase abstracte
* GfxUtils
    - Engine grafic şi utilitare grafice
* JakRandr
    - Interfaţă grafică pentru engine

Misc
====

Engineul grafic foloseşte algoritmul lui Newell pentru sortare poligoanelor.

Funcţia care testează interioritatea unui punct e luată de-aici:
http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
Mulţumesc lui W. Randolph Franklin!

Fiecare entitate rulează independent pe propriul ceas, rendarea se face la ~60hz (+/-)

Arhitectură cât de cât MVC.

TODO
====

* CoreUtils
    - câteva exemple pentru a vedea ce cod rămâne comun
    - finalizat designul
* GfxUtils
    - finalizat sortarea poligoanelor
* JakRandr
    - implementat forma unde va fi rendat totul
* JakOmlette
    - implementat particularităţiile proiectului (adică scopul acestui proiect)
