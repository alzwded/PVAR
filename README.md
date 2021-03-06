PVAR-JakRandr
=============

Proiect pentru cursul de PVAR, anul III -- platformă pentru simulări-automatizări + engine grafic + implementare

Componente
==========

* CoreUtils (model abstract)
    - Interfaţă pentru entităţi, clase abstracte
* GfxUtils (low level, renderer)
    - Engine grafic şi utilitare grafice
* JakRandr (view)
    - Interfaţă grafică pentru engine, ceas de rendare, manipulare cameră

Building
========

You need lazarus version 0.9.30.4 or newer with fpc 2.6.0 or newer.

You can get it from here:
http://sourceforge.net/projects/lazarus/files/
other information here:
http://www.lazarus.freepascal.org/

Make sure you select the build target for your platform (currently win32 or linux 32). I.e. if you're on windows, select the win32 target from build options, if you're on linux, select either of the linux-i686 targets.

Misc
====

Engineul grafic foloseşte algoritmul lui Newell pentru sortare poligoanelor.

Fiecare entitate rulează independent pe propriul ceas.

Frecvența rendării se reglează automat în funcție de încărcare (oarecum).

Arhitectură cât de cât MVC.

Using the camera
================

Camera manipulation is currently implemented dumbly. This will be fixed in a future version.

Right now rotation seems to be okay, it's just the panning that's weird. Panning currently works by assuming the camera is oriented on (0,0,1) which causes confusion, especially when Z is rotated.

Left mouse button:
* normal:	pan X and Y coordinates
* +control:	pan Z coordinate
* +shift:	change focal distance

Right mouse button:
* normal:	rotate X and Y axes
* +control:	rotate Z axis

Acknowledgements
================

Polygon sorting uses a twisted implementation of Newell's algorithm.

Point-in-polygon test function is a delphi port of prof. W. Randolph Franklin's implementation, taken from here
http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html

Built using lazarus/freepascal.

Licensing
=========

You are free to use any code from this project for personal use or educational purposes,
provided that you link back to this repository.

TODO
====

* CoreUtils
    - ~~câteva exemple pentru a vedea ce cod rămâne comun~~
    - ~~test new classes~~
    - ~~implement RotateAround/Rotate for all useful classes~~
    - ~~finalizat designul~~
* GfxUtils
    - ~~export nodes of polygons as pointers as well (useful for TSkin)~~ overruled
    - ~~test sprite, sphere rendering / scaling~~ actually, I don't think I'll use those
    - use constants/singletons where aplicable
    - improve performance of InOrderLinePolygon
* JakRandr
    - ~~implement rendering on separate thread because lcl is stupid and timers are kinda executed serially and that's what I DON'T want~~ no.
    - ^ although, that's what I want for state machines where timers need to be based on a global timer (like in the real world)
* JakOmlette
    - implementat particularităţiile proiectului (adică scopul acestui proiect)
        - ~~gravity and pits of death~~
        - ~~fliparms~~
        - bot parts
        - head crank
        - leg crank
        - death animation
        - syncing

What we've learnt from this project
===================================

(in no particular order:)
* That z-buffering is so widely used for a reason
* gpu-assisted maths is many, many times faster
* don't reinvent the wheel
* don't bother with algorithms which where invented 30 years ago yet nobody seems to remember them (this, too, happens for a reason)
* don't bother with things that are documented using the NODoc automated documentation system
* don't over-architect

Known issues
============

* polygon sorting uses some shortcuts to improve performance. This means that if you mix in polygons whose dimensions are of different orders of magnitude, you may discover that you can see the smaller one through the big one. This is fixable, but greatly decreases performance.
Maybe a compilation flag can toggle between performance and correctitude
