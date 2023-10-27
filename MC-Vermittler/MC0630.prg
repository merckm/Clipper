* Program.: MC0630.PRG
* Author..: Martin Merck
* Datum...: 9. Oktober 1987
* Notiz...:
* Bemerk..: Listen der Vermittler

*----------------------------------------------------------------------*
* Initialisierungen
*----------------------------------------------------------------------*

* Alle offenen Dateien schlie·en
CLOSE DATABASES

************************************************************************
*          B E G I N   D E S   H A U P T P R o G R A M M S             *
************************************************************************


*----------------------------------------------------------------------*
* Maske auf dem Bildschirm ausgeben
*----------------------------------------------------------------------*

PNAME = "MC0630"

c_error_t    = " "
c_sort_name  = " "
c_sort_vermi = " "

n_map = 5

FOR i = 3 TO 27
   STORE SPACE(30) TO f_keytext[i]
NEXT

* Funktionstasten belegen

f_key[2] = "ZURöCK "
f_key[3] = "       "
f_key[4] = "       "
f_key[5] = "       "
f_key[6] = "       "
f_key[7] = "       "
f_key[8] = "       "
f_key[10] = " HILFE "

f_keytext[5] = " ZurÅck zum Hauptmenue !"
f_keytext[29] = " Bedeutung der Funktionstasten"

* Maske ausgeben

CLEAR

IF .NOT. l_map[n_map] = .T.

   DO RAHMEN WITH "Listen der Vermittler"

   @ 8,5  SAY "SORTIERUNG:"
   @10,10 SAY "Name           : "
   @11,10 SAY "Vermittler Nr. :"

   SAVE SCREEN TO m_map[n_map]
   l_map[n_map] = .T.

ELSE

   RESTORE SCREEN FROM m_map[n_map]

ENDIF


DO WHILE .T.

   n_map = 10

   FOR i = 3 TO 27
      STORE SPACE(30) TO f_keytext[i]
   NEXT

* Funktionstasten belegen
   
   f_key[2] = "ZURöCK "
   f_key[3] = "       "
   f_key[4] = "       "
   f_key[5] = "       "
   f_key[6] = "       "
   f_key[7] = "       "
   f_key[8] = "       "
   f_key[9] = "       "
   f_key[10] = " HILFE "
   
   f_keytext[5] = " ZurÅck zum Hauptmenue !"
   f_keytext[29] = " Bedeutung der Funktionstasten"
   
   RESTORE SCREEN FROM m_map[n_map]
   
   n_int_key = 0
   @ 3,71 SAY TIME()
   @10,27 GET c_sort_name  PICTURE "!"
   @11,27 GET c_sort_vermi PICTURE "!"

   @ 0,2 SAY c_error_t
   c_error_t = " "

   READ

   @ 0,0 CLEAR TO 0,79
   
* Funktionstasten auswerten

      FOR i = 2 TO 9
         IF ( n_int_key = i .AND. LEN(TRIM(f_key[i])) = 0)
            c_error_t = "FEHLER: Funktionstaste wird nicht unterstÅtzt"
            LOOP
         ENDIF
      NEXT
   
      IF n_int_key = 2
         RETURN
      ENDIF
   
      IF n_int_key = 10
         LOOP
      ENDIF

*----------------------------------------------------------------------*
* PrÅfung der Eingabe
*----------------------------------------------------------------------*

   @ 0,0 CLEAR TO 0,79
   @ 0,2 SAY "PrÅfung der Eingabe"

   IF ( c_sort_name = " " .AND. c_sort_vermi = " " )
      c_error_t = "*** Bitte ein Sortierkriterium angeben  ***"
      LOOP
   ENDIF

   IF ( c_sort_name # " " .AND. c_sort_vermi # " " )
      c_error_t = "*** Nur ein Sortierkriterium angeben  ***"
      LOOP
   ENDIF

   EXIT

ENDDO

IF c_sort_name # " "
   c_sort_text = "Name"
ELSE
   c_sort_text = "Vermittler-Nr"
ENDIF

@ 0,0 CLEAR TO 0,79
@ 0,2 SAY "*** Drucker einschalten und beliebige Taste betÑtigen ***"
INKEY(0)

DO liste WITH c_sort_text

RETURN


PROCEDURE LISTE
***********************************************************************
* Das Unterprogramm LISTE gibt eine Liste der Vermittler aus.         *
***********************************************************************
PARAMETER c_sort_text

USE
IF c_sort_text = "Name"
   USE MDVERMI INDEX MDSUP02
ELSE
   USE MDVERMI INDEX MDSUP01
ENDIF

@ 0,0 CLEAR TO 0,79
@ 0,2 SAY "*** Liste wird auf dem Drucker gedruckte ***"

GO TOP
SET DEVICE TO PRINTER
seite = 1
zeile = 1

DO WHILE !EOF()
   IF vermittler = 0
      SKIP 1
      LOOP
   ENDIF

   @ 2,1    SAY "----- ("+PNAME+")"
   @ 2,24   SAY "LISTE DER VERMITTLER-ADRESSEN"
   @ 2,104  SAY "DATUM "+DTOC(DATE())+" SEITE "+STR(seite,2)
   @ 3,1    SAY "USER: "+c_nachname
   @ 3,104  SAY "START "+TIME()
   @ 4,1    SAY REPLICATE("*",131)
   @ 6,1    SAY "SORTIERKRITERIEN:"
   @ 6,20   SAY c_sort_text
   @ 8,1    SAY "ZWST"
   @ 8,5    SAY " VER-"
   @ 8,15   SAY "NAME"
   @ 8,29   SAY "VORNAME"
   @ 8,40   SAY "FIRMA"
   @ 8,51   SAY "STRASSE"
   @ 8,72   SAY "PLZ"
   @ 8,77   SAY "ORT"
   @ 8,99   SAY "TELEFON"
   @ 8,115  SAY "TELEFON"
   @ 9,2    SAY "AKT"
   @ 9,5    SAY "MITTLER"
   @ 9,99   SAY "(privat)"
   @ 9,115  SAY "(Firma)"
   @10,1    SAY REPLICATE(".",131)
   zeile = 11
   DO WHILE zeile < 63
      IF status # "L" .AND. status # "l" .AND. vermittler # 0
         @zeile,1   SAY STR(zwst_akt,3)
         @zeile,5   SAY STR(vermittler,9)
         @zeile,15  SAY SUBSTR(vm_name,1,13)
         @zeile,29  SAY SUBSTR(vm_vorname,1,10)
         @zeile,49  SAY SUBSTR(vm_firma,1,10)
         @zeile,51  SAY SUBSTR(vm_strasse,1,20)
         @zeile,72  SAY STR(vm_plz,4)
         @zeile,77  SAY SUBSTR(vm_ort,1,12)
         @zeile,99  SAY telpvw+"/"+telpdw
         @zeile,115 SAY telfvw+"/"+telfdw
      ELSE
         zeile = zeile - 1
      ENDIF
      SKIP 1
      IF EOF()
         IF zeile < 60
            @ zeile+5,50 SAY "***** ENDE DER AUSGABE *****"
         ELSE
            @ 64,50 SAY "***** ENDE DER AUSGABE *****"
         ENDIF
         EXIT
      ENDIF
      zeile = zeile + 1
   ENDDO
   @ 66,1 SAY REPLICATE("*",131)
   @ 67,1 SAY "----- ("+PNAME+")"
   @ 67,24 SAY "LISTE DER VERMITTLER-ADRESSEN"
   @ 67,104 SAY "DATUM "+DTOC(DATE())+" SEITE "+STR(seite,2)
   @ 68,1 SAY "USER: "+c_nachname
   @ 68,104 SAY "START "+TIME()
   EJECT
   seite = seite + 1

ENDDO

SET DEVICE TO SCREEN
USE

RETURN

* EOF : MC0630.PRG