* Program.: MC0610.PRG
* Author..: Martin Merck
* Datum...: 09. Oktober 1987
* Notiz...:
* Bemerck..: Erfassen der Vermittler

*----------------------------------------------------------------------*
* Initialisierungen
*----------------------------------------------------------------------*

CLEAR

PNAME = "MC0610"

*----------------------------------------------------------------------*
* Letzte Vermittlernummer fÅr eine Zweigstelle suchen
* Diese wird in dem Satz mit der Vermittlernummer 0
*----------------------------------------------------------------------*

USE MDVERMI INDEX MDSUP01,MDSUP02,MDSUP03    // Adaption for Harbour

n_vermi_dummy = n_zwstnr * 1000000
n_vermi_dummy = n_vermi_dummy+VAL(SUBSTR(STR(YEAR(DATE()),4),3,2))*10000

SEEK STR(n_zwstnr,3)+STR(n_vermi_dummy,9)

IF EOF()
   APPEND BLANK
   REPLACE zwst_akt WITH n_zwstnr
   REPLACE vermittler WITH n_vermi_dummy
   REPLACE telpdw WITH STR(n_vermi_dummy,9)
   n_vermittler = n_vermi_dummy
   n_isn        = RECNO()
ELSE
   n_vermittler = VAL(telpdw)
   n_isn        = RECNO()
ENDIF

USE

n_vermittler = n_vermittler + 1

************************************************************************
*          B E G I N   D E S   H A U P T P R o G R A M M S             *
************************************************************************

DO WHILE .T.

*----------------------------------------------------------------------*
* Variablen Initialisieren
*----------------------------------------------------------------------*

   n_zwst_akt = n_zwstnr
   c_status = " "
   c_typ = "  "
   c_anrede = " "
   c_titel = " "
   c_name = "                    "
   c_vorname = "              "
   c_firma = "                  "
   c_strasse = "                          "
   n_plz = 0
   c_ort = "                     "
   c_telpvw = "     "
   c_telpdw = "         "
   c_telfvw = "     "
   c_telfdw = "         "
   c_errortext = " "

*--------------------------------------------------------------------------*
* Maske auf dem Bildschirm ausgeben
*--------------------------------------------------------------------------*

   n_map = 1

   FOR n_i = 3 TO 27
      STORE SPACE(30) TO f_keytext[n_i] 
   NEXT

* Maske ausgeben

   IF .NOT. l_map[n_map] = .T.

* Funktionstasten belegen

      f_key[2] = "ZURöCK "
      f_key[3] = "  CLS  "
      f_key[4] = "       "
      f_key[5] = "       "
      f_key[6] = "       "
      f_key[7] = "ANREDEN"
      f_key[8] = " TITEL "
      f_key[9] = ""
      f_key[10] = " HILFE "

      CLEAR
      DO RAHMEN WITH "Erfassen der Vermittler"
      @ 6,5  SAY "Vermittler-Nummer : "
      @ 7,5  SAY "Aktuelle Zweigstelle : "
      @ 7,40 SAY "Status : "
      @ 7,60 SAY "Typ : "
      @ 9,0  SAY CHR(199)+REPLICATE(CHR(196),78)+CHR(182)
      @11,5  SAY "Anrede :    Titel : "
      @12,5  SAY "Name              : "
      @13,5  SAY "Vorname           : "
      @14,5  SAY "Firma             : "
      @16,5  SAY "Stra·e            : "
      @17,5  SAY "PLZ :         Ort : "
      @18,5  SAY "Telefon (privat)  : "
      @18,30 SAY "/"
      @19,5  SAY "Telefon (dienst.) : "
      @19,30 SAY "/"

      SAVE SCREEN TO m_map[n_map]
      l_map[n_map] = .T.


   ENDIF


   DO WHILE .T.

      n_map = 1
      n_int_key = 0

      RESTORE SCREEN FROM m_map[n_map]

* Funktionstasten belegen

      f_key[2] = "ZURöCK "
      f_key[3] = "  CLS  "
      f_key[4] = "       "
      f_key[5] = "       "
      f_key[6] = "       "
      f_key[7] = "ANREDEN"
      f_key[8] = " TITEL "
      f_key[9] = ""
      f_key[10] = " HILFE "
      f_keytext[5] = " ZurÅck zum Hauptmenue !"
      f_keytext[5] = " Alle Eingabefelder lîschen"
      f_keytext[20] = " AbkÅrzungen aller zulÑssigen Anreden"
      f_keytext[23] = " AbkÅrzungen aller zulÑssigen Titel"
      f_keytext[29] = " Bedeutung der Funktionstasten"

      @ 6,25 SAY n_vermittler
      @ 7,28 SAY n_zwst_akt PICTURE "999"
      @ 7,49 GET c_status PICTURE "!"
      @ 7,66 GET c_typ PICTURE "!!"
      @11,14 GET c_anrede PICTURE "!"
      @11,25 GET c_titel PICTURE "!"
      @12,25 GET c_name PICTURE "!XXXXXXXXXXXXXXXXXXX"
      @13,25 GET c_vorname PICTURE "!XXXXXXXXXXXXX"
      @14,25 GET c_firma PICTURE "!XXXXXXXXXXXXXXXXX"
      @16,25 GET c_strasse PICTURE "!XXXXXXXXXXXXXXXXXXXXXXXXX"
      @17,11 GET n_plz PICTURE "99999"
      @17,25 GET c_ort PICTURE "!XXXXXXXXXXXXXXXXXXXX"
      @18,25 GET c_telpvw PICTURE "XXXXX"
      @18,31 GET c_telpdw PICTURE "XXXXXXXXX"
      @19,25 GET c_telfvw PICTURE "XXXXX"
      @19,31 GET c_telfdw PICTURE "XXXXXXXXX"
   
      @ 3,71 SAY TIME()
      @ 0,2 SAY c_errortext
      c_errortext = " "
   
      READ

      @ 0,02 CLEAR TO 0,79

* Funktionstasten auswerten

      FOR n_i = 2 TO 9
         IF ( n_int_key = n_i .AND. LEN(TRIM(f_key[n_i])) = 0)
            c_errortext = "FEHLER: Funktionstaste wird nicht unterstÅtzt"
            LOOP
         ENDIF
      NEXT

      IF n_int_key = 2
         RETURN
      ENDIF

      IF n_int_key = 3
         EXIT
      ENDIF

      IF n_int_key = 7
         DO zeige_anreden
         LOOP
      ENDIF

      IF n_int_key = 8
         DO zeige_titel
         LOOP
      ENDIF

      IF n_int_key = 10
         LOOP
      ENDIF

      @ 0,02 SAY "PrÅfung der Eingaben"

      IF c_status <> " " .AND. c_status <> "L"
         c_errortext = "Status nur ' ' fÅr aktiv, oder 'L' fÅr lîschen!"
         KEYBOARD(REPLICATE(CHR(13),0))
         LOOP
      ENDIF

      IF PRUEF_ANREDE(c_anrede) = 0
         c_errortext = "*** Anrede ist falsch ! ***"
         KEYBOARD(REPLICATE(CHR(13),2))
         LOOP
      ENDIF

      IF PRUEF_TITEL(c_titel) = 0
         c_errortext = "*** Titel ist falsch ! ***"
         KEYBOARD(REPLICATE(CHR(13),3))
         LOOP
      ENDIF

      IF LEN(TRIM(c_name)) = 0
         c_errortext = "*** Bitte Namen eingeben ***"
         KEYBOARD(REPLICATE(CHR(13),4))
         LOOP
      ENDIF

      IF LEN(TRIM(c_strasse)) = 0
         c_errortext = "*** Bitte Stra·e eingeben ***"
         KEYBOARD(REPLICATE(CHR(13),7))
         LOOP
      ENDIF

      IF n_plz = 0
         c_errortext = "*** Bitte PLZ eingeben ***"
         KEYBOARD(REPLICATE(CHR(13),8))
         LOOP
      ENDIF
      n_plz = PRUEF_PLZ(n_plz)

      IF LEN(TRIM(c_ort)) = 0
         c_errortext = "*** Bitte ORT eingeben ***"
         KEYBOARD(REPLICATE(CHR(13),9))
         LOOP
      ENDIF

      DO PRUEF_TEL
      IF LEN(TRIM(c_errortext)) # 0
         LOOP
      ENDIF

      IF BESTAETIGEN() = 1
         DO abspeichern
         EXIT
      ELSE
         LOOP
      ENDIF

   ENDDO

ENDDO

RETURN

************************************************************************
*           E n d e   d e s   H a u p t p r o g r a m m s              *
************************************************************************


PROCEDURE ZEIGE_ANREDEN
************************************************************************
* Die Prozedur ZEIGE_ANREDEN zeigt die Anreden aus AA-CODE an.         * 
************************************************************************
   
   n_map = 3
   
   IF .NOT. l_map[n_map]
   
      USE aacode INDEX aasup01            // Changed for Harbour compatibility
   
      DECLARE c_an_abk[32],c_an_txt[32]
   
      FOR n_i = 1 TO 32
         c_an_abk[n_i] = " "
         c_an_txt[n_i] = " "
      NEXT
   
      SEEK "ANREDE    "
   
      n_i = 0
      DO WHILE funktion = "ANREDE    "
         n_i = n_i + 1
         c_an_abk[n_i] = SUBSTR(code1,1)
         c_an_txt[n_i] = kurztext
         IF n_i = 32
            EXIT
         ENDIF
         SKIP
      ENDDO

      USE
   
      FOR n_i = 3 TO 27
         STORE SPACE(30) TO f_keytext[n_i] 
      NEXT
   
   * Funktionstasten belegen
   
      f_key[2] = "       "
      f_key[3] = "       "
      f_key[4] = "       "
      f_key[5] = "       "
      f_key[6] = "       "
      f_key[7] = "       "
      f_key[8] = "       "
      f_key[9] = "       "
      f_key[10] = "       "
   
   * Maske ausgeben
   
      CLEAR
   
      DO RAHMEN WITH "ZulÑssige Anreden"
      @ 5,5  SAY c_an_abk[1]+" "+c_an_txt[1]
      @ 5,42 SAY c_an_abk[2]+" "+c_an_txt[2]
      @ 6,5  SAY c_an_abk[3]+" "+c_an_txt[3]
      @ 6,42 SAY c_an_abk[4]+" "+c_an_txt[4]
      @ 7,5  SAY c_an_abk[5]+" "+c_an_txt[5]
      @ 7,42 SAY c_an_abk[6]+" "+c_an_txt[6]
      @ 8,5  SAY c_an_abk[7]+" "+c_an_txt[7]
      @ 8,42 SAY c_an_abk[8]+" "+c_an_txt[8]
      @ 9,5  SAY c_an_abk[9]+" "+c_an_txt[9]
      @ 9,42 SAY c_an_abk[10]+" "+c_an_txt[10]
      @10,5  SAY c_an_abk[11]+" "+c_an_txt[11]
      @10,42 SAY c_an_abk[12]+" "+c_an_txt[12]
      @11,5  SAY c_an_abk[13]+" "+c_an_txt[13]
      @11,42 SAY c_an_abk[14]+" "+c_an_txt[14]
      @12,5  SAY c_an_abk[15]+" "+c_an_txt[15]
      @12,42 SAY c_an_abk[16]+" "+c_an_txt[16]
      @13,5  SAY c_an_abk[17]+" "+c_an_txt[17]
      @13,42 SAY c_an_abk[18]+" "+c_an_txt[18]
      @14,5  SAY c_an_abk[19]+" "+c_an_txt[19]
      @14,42 SAY c_an_abk[20]+" "+c_an_txt[20]
      @15,5  SAY c_an_abk[21]+" "+c_an_txt[21]
      @15,42 SAY c_an_abk[22]+" "+c_an_txt[22]
      @16,5  SAY c_an_abk[23]+" "+c_an_txt[23]
      @16,42 SAY c_an_abk[24]+" "+c_an_txt[24]
      @17,5  SAY c_an_abk[25]+" "+c_an_txt[25]
      @17,42 SAY c_an_abk[26]+" "+c_an_txt[26]
      @18,5  SAY c_an_abk[27]+" "+c_an_txt[27]
      @18,42 SAY c_an_abk[28]+" "+c_an_txt[28]
      @19,5  SAY c_an_abk[29]+" "+c_an_txt[29]
      @19,42 SAY c_an_abk[30]+" "+c_an_txt[30]
      @20,5  SAY c_an_abk[31]+" "+c_an_txt[31]
      @20,42 SAY c_an_abk[32]+" "+c_an_txt[32]
   
      SAVE SCREEN TO m_map[n_map]
      l_map[n_map] = .T.
   
   ELSE
   
      RESTORE SCREEN FROM m_map[n_map]
   
   ENDIF
   
   INKEY(0)
   
   IF LASTKEY() = 28
      DO ENDE
   ENDIF
   
   RETURN
   
   
   PROCEDURE ZEIGE_TITEL
   ************************************************************************
   * Die Prozedur ZEIGE_TITEL zeigt die Titel aus AA-CODE an.             * 
   ************************************************************************
   
   n_map = 4
   
   IF .NOT. l_map[n_map]
   
      USE aacode INDEX aasup01            // Changed for Harbour compatibility
   
      DECLARE c_ti_abk[32],c_ti_txt[32]
   
      FOR n_i = 1 TO 32
         c_ti_abk[n_i] = " "
         c_ti_txt[n_i] = " "
      NEXT
   
      SEEK "TITEL     "
   
      n_i = 0
      DO WHILE funktion = "TITEL     "
         n_i = n_i + 1
         c_ti_abk[n_i] = SUBSTR(code1,1)
         c_ti_txt[n_i] = kurztext
         IF n_i = 32
            EXIT
         ENDIF
         SKIP
      ENDDO

      USE
      
      FOR n_i = 3 TO 27
         STORE SPACE(30) TO f_keytext[n_i] 
      NEXT
   
   * Funktionstasten belegen
   
      f_key[2] = "       "
      f_key[3] = "       "
      f_key[4] = "       "
      f_key[5] = "       "
      f_key[6] = "       "
      f_key[7] = "       "
      f_key[8] = "       "
      f_key[9] = "       "
      f_key[10] = "       "
   
   * Maske ausgeben
   
      CLEAR
   
      DO RAHMEN WITH "ZulÑssige Titel"
      @ 5,5  SAY c_ti_abk[1]+" "+c_ti_txt[1]
      @ 5,42 SAY c_ti_abk[2]+" "+c_ti_txt[2]
      @ 6,5  SAY c_ti_abk[3]+" "+c_ti_txt[3]
      @ 6,42 SAY c_ti_abk[4]+" "+c_ti_txt[4]
      @ 7,5  SAY c_ti_abk[5]+" "+c_ti_txt[5]
      @ 7,42 SAY c_ti_abk[6]+" "+c_ti_txt[6]
      @ 8,5  SAY c_ti_abk[7]+" "+c_ti_txt[7]
      @ 8,42 SAY c_ti_abk[8]+" "+c_ti_txt[8]
      @ 9,5  SAY c_ti_abk[9]+" "+c_ti_txt[9]
      @ 9,42 SAY c_ti_abk[10]+" "+c_ti_txt[10]
      @10,5  SAY c_ti_abk[11]+" "+c_ti_txt[11]
      @10,42 SAY c_ti_abk[12]+" "+c_ti_txt[12]
      @11,5  SAY c_ti_abk[13]+" "+c_ti_txt[13]
      @11,42 SAY c_ti_abk[14]+" "+c_ti_txt[14]
      @12,5  SAY c_ti_abk[15]+" "+c_ti_txt[15]
      @12,42 SAY c_ti_abk[16]+" "+c_ti_txt[16]
      @13,5  SAY c_ti_abk[17]+" "+c_ti_txt[17]
      @13,42 SAY c_ti_abk[18]+" "+c_ti_txt[18]
      @14,5  SAY c_ti_abk[19]+" "+c_ti_txt[19]
      @14,42 SAY c_ti_abk[20]+" "+c_ti_txt[20]
      @15,5  SAY c_ti_abk[21]+" "+c_ti_txt[21]
      @15,42 SAY c_ti_abk[22]+" "+c_ti_txt[22]
      @16,5  SAY c_ti_abk[23]+" "+c_ti_txt[23]
      @16,42 SAY c_ti_abk[24]+" "+c_ti_txt[24]
      @17,5  SAY c_ti_abk[25]+" "+c_ti_txt[25]
      @17,42 SAY c_ti_abk[26]+" "+c_ti_txt[26]
      @18,5  SAY c_ti_abk[27]+" "+c_ti_txt[27]
      @18,42 SAY c_ti_abk[28]+" "+c_ti_txt[28]
      @19,5  SAY c_ti_abk[29]+" "+c_ti_txt[29]
      @19,42 SAY c_ti_abk[30]+" "+c_ti_txt[30]
      @20,5  SAY c_ti_abk[31]+" "+c_ti_txt[31]
      @20,42 SAY c_ti_abk[32]+" "+c_ti_txt[32]
   
      SAVE SCREEN TO m_map[n_map]
      l_map[n_map] = .T.
   
   ELSE
   
      RESTORE SCREEN FROM m_map[n_map]
   
   ENDIF
   
   INKEY(0)
   
   IF LASTKEY() = 28
      DO ENDE
   ENDIF
   
RETURN
   
   
FUNCTION PRUEF_ANREDE
************************************************************************
* Die Funktion PRUEF_ANREDE prÅft ob c_anrede in AA-CODE vorhanden ist.* 
************************************************************************
PARAMETER c_anrede1

IF LEN(TRIM(c_anrede1)) # 0

   SELECT 10
   USE aacode INDEX aasup01            // For Harbour compatibility

   GO TOP
   SEEK "ANREDE    "+c_anrede1
   IF EOF()
      RETURN(0)
   ENDIF

   USE
   SELECT 1

ENDIF

RETURN(1)


FUNCTION PRUEF_TITEL
************************************************************************
* Die Funktion PRUEF_TITEL prÅft ob c_titel in AA-CODE vorhanden ist.  * 
************************************************************************
PARAMETER c_titel1

IF LEN(TRIM(c_titel1)) # 0

   SELECT 10
   USE aacode INDEX aasup01            // For Harbour compatibility

   GO TOP
   SEEK "TITEL     "+c_titel1
   IF EOF()
      RETURN(0)
   ENDIF

   USE
   SELECT 1

ENDIF

RETURN(1)


FUNCTION PRUEF_PLZ
************************************************************************
* Die Funktion PRUEF_PLZ prÅft die PLZ.                                * 
************************************************************************
PARAMETER n_plz1

IF n_plz1 < 10
   n_plz1 = n_plz1 * 1000
ENDIF
IF n_plz1 < 100
   n_plz1 = n_plz1 * 100
ENDIF
IF n_plz1 < 1000
   n_plz1 = n_plz1 * 10
ENDIF
RETURN(n_plz1)


PROCEDURE PRUEF_TEL
************************************************************************
* Die Prozedur PRUEF_TEL prÅft die Telelfonnummern.                                * 
************************************************************************

n_telpvw = VAL(c_telpvw)
n_telpdw = VAL(c_telpdw)
n_telfvw = VAL(c_telfvw)
n_telfdw = VAL(c_telfdw)

IF LEN(TRIM(c_telpdw)) #0 .AND. LEN(TRIM(c_telpvw)) = 0
   c_errortest = "*** Bitte auch VORWAHL eingeben ! ***"
   KEYBOARD(REPLICATE(CHR(13),10))
   RETURN
ENDIF

IF n_telpvw > 0 .AND. n_telpvw < 10
   c_errortest = "*** VORWAHL falsch ! ***"
   KEYBOARD(REPLICATE(CHR(13),10))
   RETURN
ENDIF

IF LEN(TRIM(c_telpvw)) #0 .AND. LEN(TRIM(c_telpdw)) = 0
   c_errortest = "*** Bitte auch DURCHWAHL eingeben ! ***"
   KEYBOARD(REPLICATE(CHR(13),11))
   RETURN
ENDIF

IF n_telpdw > 0 .AND. n_telpdw < 10
   c_errortest = "*** DURCHWAHL falsch ! ***"
   KEYBOARD(REPLICATE(CHR(13),11))
   RETURN
ENDIF

IF LEN(TRIM(c_telfdw)) #0 .AND. LEN(TRIM(c_telfvw)) = 0
   c_errortest = "*** Bitte auch VORWAHL eingeben ! ***"
   KEYBOARD(REPLICATE(CHR(13),12))
   RETURN
ENDIF

IF n_telfvw > 0 .AND. n_telfvw < 10
   c_errortest = "*** VORWAHL falsch ! ***"
   KEYBOARD(REPLICATE(CHR(13),12))
   RETURN
ENDIF

IF LEN(TRIM(c_telfvw)) #0 .AND. LEN(TRIM(c_telfdw)) = 0
   c_errortest = "*** Bitte auch DURCHWAHL eingeben ! ***"
   KEYBOARD(REPLICATE(CHR(13),13))
   RETURN
ENDIF

IF n_telfdw > 0 .AND. n_telfdw < 10
   c_errortest = "*** DURCHWAHL falsch ! ***"
   KEYBOARD(REPLICATE(CHR(13),13))
   RETURN
ENDIF

RETURN


FUNCTION BESTAETIGEN
***************************************************************************
* Die Funktion BESTAETIGEN zeigt all Daten zum BestÑtigen noch einmal an. * 
***************************************************************************

*--------------------------------------------------------------------------*
* Maske auf dem Bildschirm ausgeben
*--------------------------------------------------------------------------*

n_map = 2

FOR n_i = 3 TO 27
   STORE SPACE(30) TO f_keytext[n_i] 
NEXT

* Maske ausgeben

IF .NOT. l_map[n_map] = .T.

   CLEAR

* Funktionstasten belegen

   f_key[2] = "ZURöCK "
   f_key[3] = "       "
   f_key[4] = "       "
   f_key[5] = "       "
   f_key[6] = "       "
   f_key[7] = "       "
   f_key[8] = "       "
   f_key[9] = ""
   f_key[10] = " HILFE "

   DO RAHMEN WITH "BestÑtigen der Daten mit ENTER!"
   @ 6,5  SAY "Vermittler-Nummer : "
   @ 7,5  SAY "Aktuelle Zweigstelle : "
   @ 7,40 SAY "Status : "
   @ 7,60 SAY "Typ : "
   @ 9,0  SAY CHR(199)+REPLICATE(CHR(196),78)+CHR(182)
   @11,5  SAY "Anrede :    Titel : "
   @12,5  SAY "Name              : "
   @13,5  SAY "Vorname           : "
   @14,5  SAY "Firma             : "
   @16,5  SAY "Stra·e            : "
   @17,5  SAY "PLZ :         Ort : "
   @18,5  SAY "Telefon (privat)  : "
   @18,30 SAY "/"
   @19,5  SAY "Telefon (dienst.) : "
   @19,30 SAY "/"

   SAVE SCREEN TO m_map[n_map]
   l_map[n_map] = .T.

ENDIF


DO WHILE .T.

   n_int_key = 0
   n_map = 2

   RESTORE SCREEN FROM m_map[n_map]

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

   @ 6,25 SAY n_vermittler
   @ 7,28 SAY n_zwst_akt PICTURE "999"
   @ 7,49 SAY c_status PICTURE "!"
   @ 7,66 SAY c_typ PICTURE "!!"
   @11,14 SAY c_anrede PICTURE "!"
   @11,25 SAY c_titel PICTURE "!"
   @12,25 SAY c_name PICTURE "!XXXXXXXXXXXXXXXXXXX"
   @13,25 SAY c_vorname PICTURE "!XXXXXXXXXXXXX"
   @14,25 SAY c_firma PICTURE "!XXXXXXXXXXXXXXXXX"
   @16,25 SAY c_strasse PICTURE "!XXXXXXXXXXXXXXXXXXXXXXXXX"
   @17,11 SAY n_plz PICTURE "99999"
   @17,25 SAY c_ort PICTURE "!XXXXXXXXXXXXXXXXXXXX"
   @18,25 SAY c_telpvw PICTURE "XXXXX"
   @18,31 SAY c_telpdw PICTURE "XXXXXXXXX"
   @19,25 SAY c_telfvw PICTURE "XXXXX"
   @19,31 SAY c_telfdw PICTURE "XXXXXXXXX"

   @ 3,71 SAY TIME()
   @ 0,2 SAY c_errortext
   c_errortext = " "

   WAIT " "

   @ 0,02

* Funktionstasten auswerten

   FOR n_i = 2 TO 9
      IF ( n_int_key = n_i .AND. LEN(TRIM(f_key[n_i])) = 0)
         c_errortext = "FEHLER: Funktionstaste wird nicht unterstÅtzt"
         LOOP
      ENDIF
   NEXT

   IF n_int_key = 2
      RETURN(0)
   ENDIF

   IF n_int_key = 10
      LOOP
   ENDIF

   EXIT

ENDDO

RETURN(1)


PROCEDURE ABSPEICHERN
************************************************************************
* Die Prozedur ABSPEICHERN speichert die Daten in der Datei MD-VERMI   * 
************************************************************************

@ 0,02 CLEAR TO 0,79
@ 0,02 SAY "Abspeichern der Daten"

USE MDVERMI INDEX MDSUP01,MDSUP02,MDSUP03    // Adaption for Harbour
APPEND BLANK
REPLACE zwst_akt   WITH n_zwst_akt
REPLACE vermittler WITH n_vermittler
REPLACE status     WITH c_status
REPLACE anlagedat  WITH DATE()
REPLACE vm_name    WITH c_name
REPLACE vm_vorname WITH c_vorname
REPLACE vm_firma   WITH c_firma
REPLACE vm_anrede  WITH c_anrede
REPLACE vm_titel   WITH c_titel
REPLACE vm_strasse WITH c_strasse
REPLACE vm_plz     WITH n_plz
REPLACE vm_ort     WITH c_ort
REPLACE telpvw     WITH c_telpvw
REPLACE telpdw     WITH c_telpdw
REPLACE telfvw     WITH c_telfvw
REPLACE telfdw     WITH c_telfdw
REPLACE vm_typ     WITH c_typ
REPLACE kz_ueber   WITH "N"

GO n_isn
REPLACE telpdw WITH STR(n_vermittler,9)
n_vermittler = n_vermittler + 1

USE

@ 0,02 CLEAR TO 0,79

RETURN


* EOF : MC0610.PRG