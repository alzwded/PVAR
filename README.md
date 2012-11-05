PVAR-JakRandr
=============

Proiect pentru cursul de PVAR, anul III -- platformă pentru simulări-automatizări + engine grafic + implementare

Componente
==========

* CoreUtils (model)
    - Interfaţă pentru entităţi, clase abstracte
* GfxUtils (low level)
    - Engine grafic şi utilitare grafice
* JakRandr (view/controller)
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

Fiecare entitate rulează independent pe propriul ceas, rendarea se face la ~30hz (+/-)

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
    - câteva exemple pentru a vedea ce cod rămâne comun
    - finalizat designul
* GfxUtils
    - test sprite, sphere rendering / scaling
    - sorting decision when viewport is on the plane of the polygon :-D
    - fix memory leaks related to IEntity3D (I know they're there, but no profiling was done yet)
    - use constants/singletons where aplicable
* JakRandr
    - implement rendering on separate thread because lcl is stupid and timers are kinda executed serially and that's what I DON'T want
    - ^ although, that's what I want for state machines where timers need to be based on a global timer (like in the real world)
    - Camera rotation based on currently rotated camera (yeah, THIS is where I need to compute rotations and displacements based on current rotation and displacement)
* JakOmlette
    - implementat particularităţiile proiectului (adică scopul acestui proiect)
