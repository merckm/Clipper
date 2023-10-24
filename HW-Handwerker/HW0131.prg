* Program.: HW0131.PRG
* Author..: Martin Merck
* Datum...: 24. September 1987
* Notiz...:
* Bemerck..: Unterprogramme zur Handerker Profil-Auswahl

PROCEDURE anzeigen
***********************************************************************
* Das Unterprogramm ANZEIGEN dient zum Anzeigen der durch die         *
* Profil-Auswahl selektierten Handwerkweradressen                     * 
***********************************************************************

PNAME = "HW0131"

DECLARE auswahl[11]

c_error_t = " "

SELECT 2
IF FILE("TEMP02.NTX")
   USE TEMP2 INDEX TEMP02
ELSE
   USE TEMP2
ENDIF

DO WHILE .T.

*----------------------------------------------------------------------*
* Maske aus dem Bildschirm ausgeben
*----------------------------------------------------------------------*

   n_map = 15

   FOR i = 3 TO 27
      STORE SPACE(30) TO f_keytext[i]
   NEXT

* Funktionstasten belegen
   
   f_key[2] = "ZURöCK "
   f_key[3] = "       "
   f_key[4] = "       "
   f_key[5] = " Pg Dn "
   f_key[6] = " Pg Up "
   f_key[7] = "       "
   f_key[8] = "       "
   f_key[9] = "GEWERK "
   f_key[10] = " HILFE "
   
   f_keytext[5] = " ZurÅck zum Hauptmenue !"
   f_keytext[17] = " Weitere Adressen zeigen"
   f_keytext[19] = " Vorherige Adressen zeigen"
   f_keytext[26] = " ZulÑssige Gewerke anzeigen "
   f_keytext[29] = " Bedeutung der Funktionstasten"
   
* Maske ausgeben
   
   CLEAR

   IF .NOT. l_map[n_map] = .T.
   
      DO RAHMEN WITH "Anzeigen Handwerker-Adressen"
      @5,2   SAY "Auswahl:  Adressen-Nr : "
      @5,34  SAY "bis "
      @5,47  SAY "Gewerk: "
      @5,59  SAY "bis "
      @6,2   SAY "Post-   :  von "
      @6,26  SAY "von "
      @6,39  SAY "von "
      @6,52  SAY "von "
      @6,65  SAY "von "
      @7,2   SAY "leitzahl   bis "
      @7,26  SAY "bis "
      @7,39  SAY "bis "
      @7,52  SAY "bis "
      @7,65  SAY "bis "
      @9,5   SAY "Nachname"
      @9,24  SAY "Vorname"
      @9,32  SAY "PLZ"
      @9,37  SAY "Ort"
      @9,58  SAY "Telefon"

      SAVE SCREEN TO m_map[n_map]
      l_map[n_map] = .T.

   ELSE

      RESTORE SCREEN FROM m_map[n_map]
 
   ENDIF
 
   
   DO WHILE .T.

      n_map = 15

      FOR i = 3 TO 27
         STORE SPACE(30) TO f_keytext[i]
      NEXT
   
* Funktionstasten belegen
      
      f_key[2] = "ZURöCK "
      f_key[3] = "       "
      f_key[4] = "       "
      f_key[5] = " Pg Dn "
      f_key[6] = " Pg Up "
      f_key[7] = "       "
      f_key[8] = "       "
      f_key[9] = "GEWERK "
      f_key[10] = " HILFE "
      
      f_keytext[5] = " ZurÅck zum Hauptmenue !"
      f_keytext[17] = " Weitere Adressen zeigen"
      f_keytext[19] = " Vorherige Adressen zeigen"
      f_keytext[26] = " ZulÑssige Gewerke anzeigen "
      f_keytext[29] = " Bedeutung der Funktionstasten"
      
      RESTORE SCREEN FROM m_map[n_map]
      
      FOR i = 1 TO 11
         auswahl[i] = " "
      NEXT

      n_int_key = 0
      @10,2 CLEAR TO 20,78
      @ 3,71 SAY TIME()
      @ 5,26 SAY n_adr_von PICTURE "9999999"
      @ 5,38 SAY n_adr_bis PICTURE "9999999"
      @ 5,55 SAY n_gew_von PICTURE "999"
      @ 5,63 SAY n_gew_bis PICTURE "999"
      @ 6,17 SAY n_plz_von[1] PICTURE "9999"
      @ 6,30 SAY n_plz_von[2] PICTURE "9999"
      @ 6,43 SAY n_plz_von[3] PICTURE "9999"
      @ 6,56 SAY n_plz_von[4] PICTURE "9999"
      @ 6,69 SAY n_plz_von[5] PICTURE "9999"
      @ 7,17 SAY n_plz_bis[1] PICTURE "9999"
      @ 7,30 SAY n_plz_bis[2] PICTURE "9999"
      @ 7,43 SAY n_plz_bis[3] PICTURE "9999"
      @ 7,56 SAY n_plz_bis[4] PICTURE "9999"
      @ 7,69 SAY n_plz_bis[5] PICTURE "9999"
      SELECT 2
      FOR i = 1 TO 11
         @ 9+i,2  GET auswahl[i] PICTURE "!"
         @ 9+i,5  SAY SUBSTR(name,1,18)
         @ 9+i,24 SAY SUBSTR(name,1,7)
         @ 9+i,32 SAY plz PICTURE "9999"
         @ 9+i,37 SAY SUBSTR(ort,1,20)
         @ 9+i,58 SAY telvw PICTURE "!!!!!"
         @ 9+i,64 SAY "/"
         @ 9+i,66 SAY teldw PICTURE "!!!!!!!!!"
         SKIP 1
         IF EOF()
            i = 12
         ENDIF
      NEXT
      SKIP -1

      @ 0,0
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

      IF n_int_key = 5 .OR. LASTKEY() = 3
         FOR i = 1 TO 10
            IF EOF()
               c_error_t = "Sie sind am Ende der Daten"
               i = 11
               SKIP -1
            ELSE
               SKIP 1
            ENDIF
         NEXT
         SKIP -10
         LOOP
      ENDIF

      IF n_int_key = 6 .OR. LASTKEY() = 18
         FOR i = 1 TO 20
            IF BOF()
               c_error_t = "Sie sind am Anfang der Daten"
               i = 21
            ELSE
               SKIP -1
            ENDIF
         NEXT
         LOOP
      ENDIF

      IF n_int_key = 9
         DO zeige_gewerk
         FOR i = 1 TO 10
            IF BOF()
               i = 11
            ELSE
               SKIP -1
            ENDIF
         NEXT
         LOOP
      ENDIF

      IF n_int_key = 10
         FOR i = 1 TO 10
            IF BOF()
               i = 11
            ELSE
               SKIP -1
            ENDIF
         NEXT
         LOOP
      ENDIF

      n_k = 0
      FOR i = 1 TO 11
         IF LEN(TRIM(auswahl[i])) > 0
            n_index = i
            n_k = n_k + 1
         ENDIF
      NEXT

      IF n_k > 1
         c_error_t = "*** Bitte immer nur einen Satz ankreuzen ***"
         LOOP
      ENDIF

      IF n_k = 1
         EXIT
      ENDIF

   ENDDO

*----------------------------------------------------------------------*
* Auswertung der Auswahl
*----------------------------------------------------------------------*

   SKIP -(11-n_index)

   n_map = 16
   FOR i = 3 TO 27
      STORE SPACE(30) TO f_keytext[i]
   NEXT
   f_key[2] = "ZURöCK "
   f_key[3] = "       "
   f_key[4] = "       "
   f_key[5] = "       "
   f_key[6] = "       "
   f_key[9] = "GEWERK "
   f_key[10] = " HELP   "
   f_keytext[5] = " ZurÅck zum Hauptmenue !"
   f_keytext[26] = " ZulÑssige Gewerke anzeigen "
   f_keytext[29] = " Bedeutung der Funktionstasten"
   
   IF .NOT. l_map[n_map]
      CLEAR
      DO RAHMEN WITH "Handwerker-Anzeigen"
      @ 5,3 SAY "ADRESSEN-NR :"
      @ 5,30 SAY "STATUS    :"
      @ 6,3  SAY "KREDITOREN-NR:"
      @ 6,30 SAY "GEWERK 1 bis 3 :"
      @ 7,1 TO 7,78

      @ 8,3  SAY "ANREDE:"
      @ 9,3  SAY "TITEL:"
      @ 10,3 SAY "FIRMEN-NAME:"
      @ 11,3 SAY "NAME:"
      @ 12,3 SAY "VORNAME:"
      @ 13,3 SAY "STRASSE:"
      @ 14,3 SAY "PLZ:"
      @ 15,3 SAY "ORT:"
      @ 16,3 SAY "TELEFON:"
      @ 16,30 SAY "/"
      @ 18,3 SAY "INFORMATION:"
      SAVE SCREEN TO m_map[n_map]
      l_map[n_map] = .T.
   ELSE
      RESTORE SCREEN FROM m_map[n_map]
   ENDIF

   DO WHILE .T.

      n_map = 16
      FOR i = 3 TO 27
         STORE SPACE(30) TO f_keytext[i]
      NEXT
      f_key[2] = "ZURöCK "
      f_key[3] = "       "
      f_key[4] = "       "
      f_key[5] = "       "
      f_key[6] = "       "
      f_key[9] = "GEWERK "
      f_key[10] = " HELP   "
      f_keytext[5] = " ZurÅck zum Hauptmenue !"
      f_keytext[26] = " ZulÑssige Gewerke anzeigen "
      f_keytext[29] = " Bedeutung der Funktionstasten"

      RESTORE SCREEN FROM m_map[n_map]

      SELECT 2

      @ 3,71 SAY TIME()
      @ 5,18 SAY ADRNR
      @ 5,48 SAY STATUS
      @ 6,18 SAY KREDNR
      @ 6,47 SAY GEWERK1
      @ 6,57 SAY GEWERK2
      @ 6,67 SAY GEWERK3
      @ 8,18  SAY ANREDE
      @ 9,18  SAY TITEL
      @ 10,18 SAY FIRMA
      @ 11,18 SAY NAME
      @ 12,18 SAY VORNAME
      @ 13,18 SAY STRASSE
      @ 14,18 SAY PLZ
      @ 15,18 SAY ORT
      @ 16,18 SAY TELVW
      @ 16,32 SAY TELDW
      @ 18,18 SAY INFO
   
      n_int_key = 0
      @ 3,71 SAY TIME()
   
      @ 0,0
      @ 0,2 SAY c_error_t
      c_error_t = "ENTER = ZurÅck zur Einzeladressauswahl!"

      WAIT " "
   
      FOR i = 2 TO 9
         IF ( n_int_key = i .AND. LEN(TRIM(f_key[i])) = 0)
            c_error_t = "FEHLER: Funktionstaste wird nicht unterstÅtzt"
            LOOP
         ENDIF
      NEXT
   
      IF n_int_key = 2
         RETURN
      ENDIF
      IF n_int_key = 9
         DO zeige_gewerk
         LOOP
      ENDIF
      IF n_int_key = 10
         LOOP
      ENDIF
      EXIT
   ENDDO

   SKIP -(n_index-1)

ENDDO

USE

RETURN


PROCEDURE liste
**********************************************************************
* Das Unterprogramm LISTE gibt eine Liste aller durch die            *
* Profil-Auswahl selektierten Handwerkeradressen aus.                *
**********************************************************************
PARAMETER c_sort_text,n_anzahl

PNAME = "HW0131"

SELECT 2
IF FILE("TEMP02.NTX")
   USE TEMP2 INDEX TEMP02
ELSE
   USE TEMP2
ENDIF

GO TOP
SET DEVICE TO PRINTER
seite = 1
zeile = 1

DO WHILE !EOF()
   @ 2,1   SAY "----- ("+PNAME+")"
   @ 2,20  SAY "PROFIL-AUSWAHL FöR HANDWERKER-ADRESSEN"
   @ 2,104 SAY "DATUM "+DTOC(DATE())+" SEITE "+STR(seite,2)
   @ 3,1   SAY "USER: "+c_nachname
   @ 3,104 SAY "START "+TIME()
   @ 4,1   SAY REPLICATE("*",131)
   @ 6,1   SAY "SORTIERKRITERIEN:"
   @ 6,20  SAY c_sort_text
   @ 8,1   SAY "AUSWAHLKRITERIEN:"
   @ 8,20  SAY "ADRESS-NR   :"
   @ 8,34  SAY STR(n_adr_von,7)
   @ 8,42  SAY "bis "
   @ 8,46  SAY STR(n_adr_bis,7)
   @ 9,20  SAY "GEWERK      :"
   @ 9,38  SAY STR(n_gew_von,3)
   @ 9,42  SAY "bis "
   @ 9,46  SAY STR(n_gew_bis,3)
   @10,20  SAY "POSTLEITZAHL:"
   @10,37  SAY STR(n_plz_von[1],4)
   @10,42  SAY "bis "
   @10,46  SAY STR(n_plz_bis[1],4)
   @11,37  SAY STR(n_plz_von[2],4)
   @11,42  SAY "bis "
   @11,46  SAY STR(n_plz_bis[2],4)
   @12,37  SAY STR(n_plz_von[3],4)
   @12,42  SAY "bis "
   @12,46  SAY STR(n_plz_bis[3],4)
   @13,37  SAY STR(n_plz_von[4],4)
   @13,42  SAY "bis "
   @13,46  SAY STR(n_plz_bis[4],4)
   @14,37  SAY STR(n_plz_von[5],4)
   @14,42  SAY "bis "
   @14,46  SAY STR(n_plz_bis[5],4)
   @16,1   SAY "ANZAHL DER HANDWERKER-ADRESSEN: "
   @16,PCOL() SAY STR(n_anzahl)
   @17,1   SAY CHR(15)
   @18,1   SAY "ADRESS-"
   @18,9   SAY "GEWERK"
   @18,21  SAY "NAME"
   @18,52  SAY "VORNAME"
   @18,68  SAY "STRASSE"
   @18,99  SAY "PLZ"
   @18,104 SAY "ORT"
   @18,139 SAY "TELEFON"
   @18,155 SAY "KRED-"
   @18,161 SAY "INFORMATION"
   @19,3   SAY "NR"
   @19,10  SAY "1   2   3"
   @19,157 SAY "NR"
   @20,1   SAY REPLICATE(".",207)
   zeile = 21
   DO WHILE zeile < 63
      @zeile,1   SAY STR(adrnr,7)
      @zeile,9   SAY STR(gewerk1,3)
      IF gewerk2 > 0
         @zeile,13  SAY STR(gewerk2,3)
      ENDIF
      IF gewerk3 > 0
         @zeile,17  SAY STR(gewerk3,3)
      ENDIF
      @zeile,21  SAY name
      @zeile,52  SAY vorname
      @zeile,68  SAY strasse
      @zeile,99  SAY STR(plz,4)
      @zeile,104 SAY ort
      @zeile,139 SAY telvw+"/"+teldw
      @zeile,155 SAY STR(krednr,5)
      @zeile,161 SAY info
      SKIP 1
      IF EOF()
         @ PROW(),PCOL() SAY CHR(18)
         IF zeile < 60
            @ zeile+5,50 SAY "***** ENDE DER AUSGABE *****"
         ELSE
            @ 64,50 SAY "***** ENDE DER AUSGABE *****"
         ENDIF
         EXIT
      ENDIF
      zeile = zeile + 1
   ENDDO
   @ PROW(),PCOL() SAY CHR(18)
   @ 66,1 SAY REPLICATE("*",131)
   @ 67,1 SAY "----- ("+PNAME+")"
   @ 67,20 SAY "PROFIL-AUSWAHL FöR HANDWERKER-ADRESSEN"
   @ 67,104 SAY "DATUM "+DTOC(DATE())+" SEITE "+STR(seite,2)
   @ 68,1 SAY "USER: "+c_nachname
   @ 68,104 SAY "START "+TIME()
   EJECT
   seite = seite + 1

ENDDO
SET DEVICE TO SCREEN
USE

RETURN


PROCEDURE aufkleber
**********************************************************************
* Das Unterprogramm AUFKLEBER druckt Aufkleber der durch die         *
* Profil-Auswahl selektierten Handwerkeradressen aus.                *
**********************************************************************
  
PNAME = "HW0131"

SELECT 2
IF FILE("TEMP02.NTX")
   USE TEMP2 INDEX TEMP02
ELSE
   USE TEMP2
ENDIF

GO TOP
@ 0,2
@ 0,2 SAY "ACHTUNG: Bitte Adre·aufkleber in den Drucker einlegen !"
INKEY(0)
@ 0,2
antwort = " "
@ 0,2 SAY "Soll ein Probeausdruck zum justieren erfolgen (j/n) "
DO WHILE .T.
   @ 0,COL() GET antwort PICTURE "!"
   READ
   IF antwort $ "JN"
      EXIT
   ENDIF
ENDDO
IF antwort = "J"
   LABEL FORM hwlabel SAMPLE TO PRINT
ENDIF
LABEL FORM hwlabel TO PRINT

USE

RETURN


PROCEDURE brief
**********************************************************************
* Das Unterprogramm BRIEF erzeugt eine Textdateimit den durch die    *
* Profil-Auswahl selektierten Handwerkeradressen fÅr die Textverarb. *
**********************************************************************
PARAMETER c_dateiname

PNAME = "HW0131"

SELECT 2
IF FILE("TEMP02.NTX")
   USE TEMP2 INDEX TEMP02
ELSE
   USE TEMP2
ENDIF

GO TOP
COPY TO &c_dateiname ;
FIELDS adrnr,krednr,anrede1,anrede2,titel1,firma,name,vorname,strasse,plz,ort ;
DELIMITED

USE 

return

* EOF : HW0131.PRG