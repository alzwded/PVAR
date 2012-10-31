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

Building
========

You need lazarus version 0.9.30.4 or newer with fpc 2.6.0 or newer.

Make sure you select the build target for your platform (currently win32 or linux 32). I.e. if you're on windows, select the win32 target from build options, if you're on linux, select either of the linux-i686 targets.

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
    - camera origin position thingy (rotation works, though)
    - sprite, sphere rendering / scaling
    - sorting decision when viewport is on the plane of the polygon :-D
* JakRandr
    - implement rendering on separate thread because lcl is stupid and timers are kinda executed serially and that's what I DON'T want
    - ^ although, that's what I want for state machines where timers need to be based on a global timer (like in the real world)
* JakOmlette
    - implementat particularităţiile proiectului (adică scopul acestui proiect)
