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

Engineul grafic foloseşte algoritmul lui Newell pentru sortare poligoanelor. (încă neimplementat complet)

Fiecare entitate rulează independent pe propriul ceas, rendarea se face la ~60hz (+/-)

Arhitectură cât de cât MVC.
