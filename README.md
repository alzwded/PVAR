PVAR-JakRandr
=============

Proiect pentru cursul de PVAR, anul III -- platformă pentru simulări-automatizări + engine grafic + implementare

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

Acknowledgements
===============

Polygon sorting uses a twisted implementation of Newell's algorithm.

Point-in-polygon test function is a delphi port of prof. W. Randolph Franklin's implementation, taken from here
http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html

Licensing
=========

You are free to use any code from this project for personal use or educational purposes,
provided that you link back to this repository.

TODO
====

* CoreUtils
    - câteva exemple pentru a vedea ce cod rămâne comun
    - finalizat designul
* GfxUtils
    - finalizat sortarea poligoanelor
    - store coordinates in the form of (origVector, xrotation, yrotation, zrotation) because rotations are (very) lossy
* JakRandr
    - implementat forma unde va fi rendat totul
    - implement rendering on separate thread because lcl is stupid and timers are kinda executed serially and that's what I DON'T want
    - ^ although, that's what I want for state machines where timers need to be based on a global timer (like in the real world)
    - implement good scaling for TImage because it sucks (the engine knows how to scale itself vectorially)
* JakOmlette
    - implementat particularităţiile proiectului (adică scopul acestui proiect)
