* Program.: HW0110.PRG
* Author..: ULRICH SCHWEIER
***************************************************************************
* Erstellt am : 25. August 1987
* GeÑndert am : 99. XXXXXX 9999    durch :
***************************************************************************
* Notiz...:
***************************************************************************
* Bemerk..:
* Bemerk..:                        HANDWERKER - ADRESSEN
* Bemerk..:
***************************************************************************
* Bemerk..: NEUANLEGEN HANDWERKER-ADRESSEN
***************************************************************************
* Bemerk..: VERWENDETE DBASE DBF(DBT) : AA-CODE .DBF
* Bemerk..:                             HW-ADRES.DBF
***************************************************************************


PROCEDURE HW0110()
*--------------------------------------------------------------------------*
* Initialisierungen
*--------------------------------------------------------------------------*

* Alle offenen Dateien schlie·wn

PNAME = "HW0110"

STORE 0000000   TO n_adrnr
STORE SPACE(1)  TO c_status
STORE 00000     TO n_krednr
STORE 000       TO n_gk1
STORE 000       TO n_ggk1
STORE 000       TO n_gk2
STORE 000       TO n_ggk2
STORE 000       TO n_gk3
STORE 000       TO n_ggk3
STORE SPACE(1)  TO c_anrede
STORE SPACE(1)  TO c_titel
STORE SPACE(30) TO c_firma
STORE SPACE(30) TO c_firma1
STORE SPACE(30) TO c_name
STORE SPACE(30) TO c_name1
STORE SPACE(15) TO c_vorname
STORE SPACE(15) TO c_vorname1
STORE SPACE(30) TO c_strasse
STORE SPACE(30) TO c_strasse1
STORE 0000      TO n_plz
STORE SPACE(34) TO c_ort
STORE SPACE(34) TO c_ort1
STORE 00000     TO n_telvw
STORE SPACE(5)  TO c_telvw
STORE 000000000 TO n_teldw
STORE SPACE(9)  TO c_teldw
STORE SPACE(49) TO c_info
STORE SPACE(12) TO c_sortname

STORE 00000000 TO n_datn
STORE 00       TO n_i
STORE 00       TO n_k
STORE 00       TO n_k0
STORE 00       TO n_k1
STORE SPACE(1) TO c_char
STORE SPACE(11) TO c_rest
STORE SPACE(11) TO c_begin
STORE SPACE(11) TO c_beginelse
STORE 0000     TO n_zaehler
STORE 0        TO n_neu
STORE 00       TO n_mark
STORE 00       TO n_markerror
STORE SPACE(79) TO c_errortext
STORE SPACE(10) TO c_aasub01
STORE SPACE(2)  TO c_b
STORE 999      TO n_betrag
STORE 0        TO n_exiter
STORE 0        TO n_zeigegewerk
STORE 000      TO n_vergleicher
STORE 0        TO n_kleinster
STORE 0        TO n_isteiner

SELECT 1
   USE HWADRES SHARED

SELECT 2
   USE aacode INDEX aasup01

*--------------------------------------------------------------------------*
* Programm z.B.  : Menue auf dem Bildschirm ausgeben
*--------------------------------------------------------------------------*

n_map = 1

FOR n_i = 3 TO 27
   STORE SPACE(30) TO f_keytext[n_i] 
NEXT

* Funktionstasten belegen

f_key[2] = "ZURöCK "
f_key[3] = "       "
f_key[4] = "       "
f_key[5] = "       "
f_key[6] = "       "
f_key[7] = " TITEL "
f_key[8] = "ANREDE "
f_key[9] = "GEWERKE"
f_key[10] = " HELP  "

f_keytext[2] = " Beendet das Programm !"
f_keytext[5] = " ZurÅck zum Hauptmenue !"
f_keytext[20] = " Zeigt zulÑssige Titel!"
f_keytext[23] = " Zeigt zulÑssige Anreden!"
f_keytext[26] = " ZulÑssige Gewerke anzeigen!"

IF .NOT. l_map[n_map] = .T.

   CLEAR

   DO RAHMEN WITH "ERFASSEN - HANDWERKER"
   @ 5,30 SAY "STATUS :     ('L'=LôSCHEN; ' '=AKTIV;)"
   @ 6,3  SAY "KREDITOREN-NR:"
   @ 6,30 SAY "GEWERK 1 bis 3 :"
   @ 7,1 TO 7,78
   @ 9,3  SAY "ANREDE:"
   @ 9,20 SAY "('D'=FIRMA)"
   @ 9,33 SAY "TITEL:"
   @ 10,3 SAY "FIRMEN-NAME:"
   @ 11,3 SAY "NAME:"
   @ 12,3 SAY "VORNAME:"
   @ 13,3 SAY "STRASSE:"
   @ 14,3 SAY "PLZ:"
   @ 15,3 SAY "ORT:"
   @ 16,3 SAY "TELEFON:"
   @ 16,24 SAY "/"
   @ 18,3 SAY "INFORMATION:"
   SAVE SCREEN TO m_map[n_map]
   l_map[n_map] = .T.
ELSE
   RESTORE SCREEN FROM m_map[n_map]
ENDIF

DO WHILE .T.
   n_map = 1
   FOR n_i = 3 TO 27
      STORE SPACE(30) TO f_keytext[n_i] 
   NEXT
   
   * Funktionstasten belegen
   f_key[2] = "ZURöCK "
   f_key[3] = "       "
   f_key[4] = "       "
   f_key[5] = "       "
   f_key[6] = "       "
   f_key[7] = " TITEL "
   f_key[8] = "ANREDE "
   f_key[9] = "GEWERKE"
   f_key[10] = " HELP  "
   
   f_keytext[2] = " Beendet das Programm !"
   f_keytext[5] = " ZurÅck zum Hauptmenue !"
   f_keytext[20] = " Zeigt zulÑssige Titel!"
   f_keytext[23] = " Zeigt zulÑssige Anreden!"
   f_keytext[26] = " ZulÑssige Gewerke anzeigen!"
   
   RESTORE SCREEN FROM m_map[n_map]

   @ 5,40 GET c_status PICTURE "!"
   @ 6,18 GET n_krednr PICTURE "99999"
   @ 6,46 GET n_gk1 PICTURE "999"
   @ 6,50 GET n_gk2 PICTURE "999"
   @ 6,54 GET n_gk3 PICTURE "999"
   @ 9,18 GET c_anrede PICTURE "!"
   @ 9,40 GET c_titel PICTURE "!"
   @ 10,18 GET c_firma PICTURE "!XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
   @ 11,18 GET c_name PICTURE "!XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
   @ 12,18 GET c_vorname PICTURE "!XXXXXXXXXXXXXX"
   @ 13,18 GET c_strasse PICTURE "!XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
   @ 14,18 GET n_plz PICTURE "99999"
   @ 15,18 GET c_ort PICTURE "!XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
   @ 16,18 GET c_telvw PICTURE "99999"
   @ 16,26 GET c_teldw PICTURE "999999999"
   @ 18,18 GET c_info

   IF c_errortext <> " "
      @ 0,0
      @ 0,2 SAY c_errortext
   ENDIF

   c_errortext = " "
   n_int_key = 0
   @ 3,71 SAY TIME()

   READ

   @ 0,0 CLEAR TO 0,79

   FOR n_i = 2 TO 9
      IF ( n_int_key = n_i .AND. LEN(TRIM(f_key[n_i])) = 0)
         c_errortext = "FEHLER: Funktionstaste wird nicht unterstÅ?tzt"
         n_markerror = 1
         LOOP
      ENDIF
   NEXT

   IF n_int_key = 2
      RETURN
   ENDIF

   IF n_int_key = 7
      DO ZEIGETITEL
      LOOP
   ENDIF

   IF n_int_key = 8
      DO ZEIGEANREDE
      LOOP
   ENDIF

   IF n_int_key = 9
      DO ZEIGEGEWERK
      LOOP
   ENDIF

   IF n_int_key = 10
      LOOP
   ENDIF

   DO WHILE .T.
      c_firma1   = UPPER(c_firma)
      c_name1    = UPPER(c_name)
      c_vorname1 = UPPER(c_vorname)
      c_strasse1 = UPPER(c_strasse)
      c_ort1     = UPPER(c_ort)

      c_firma    = c_firma1
      c_name     = c_name1
      c_vorname  = c_vorname1
      c_strasse  = c_strasse1
      c_ort      = c_ort1

      IF n_zeigegewerk = 1
         n_zeigegewerk = 0
         EXIT
      ELSE
         *               ---- PlausibilitÑts-PrÅfungen ----

         @ 0,2
         @ 0,2 SAY  "Es erfolgt die PlausibilitÑtsprÅfung"

         DO STATUSPRUEFUNG
         IF n_markerror <> 0
            EXIT
         ENDIF

         n_mode = 0
         DO KREDNRPRUEFUNG WITH n_mode
         IF n_markerror <> 0
            EXIT
         ENDIF

         DO GEWERKPRUEFUNG
         IF n_markerror <> 0
            EXIT
         ENDIF

         DO NAMENSPRUEFUNG
         IF n_markerror <> 0
            EXIT
         ENDIF

         DO ADRESSPRUEFUNG
         IF n_markerror <> 0
            EXIT
         ENDIF

         DO TELEFONPRUEFUNG
         IF n_markerror <> 0
            EXIT
         ENDIF


         *               ---- Aufbau SORTNAME ----

         c_sortname = c_name
         n_i = 0
         DO WHILE AT(" ",(TRIM(c_sortname))) <> 0 .OR. n_i = 0
            FOR n_i = 1 TO 20
               c_char = SUBSTR(c_sortname,n_i,1)
               c_rest = SUBSTR(c_sortname,n_i+1)
               c_begin = SUBSTR(c_sortname,1,n_i)
               c_beginelse = SUBSTR(c_sortname,1,n_i-1)
               IF c_char$" éôö"
                  IF c_char$"é"
                     c_sortname = c_beginelse+"AE"+c_rest
                  ENDIF
                  IF c_char$"ô"
                     c_sortname = c_beginelse+"OE"+c_rest
                  ENDIF
                  IF c_char$"ö"
                     c_sortname = c_beginelse+"UE"+c_rest
                  ENDIF
                  IF c_char$" "
                     c_sortname = c_beginelse+c_rest
                     n_i = n_i - 1
                  ENDIF
               ELSE
                  c_sortname = c_begin+c_rest
               ENDIF
            NEXT
         END DO

         *               ---- PLAUSIBILIT?TSPR?FUNG (2) ----

         n_neu = 0
         n_markerror = 0
         SELECT 1
         GO TOP
         LOCATE FOR TRIM(SORTNAME) = TRIM(c_sortname)
         IF FOUND()
            DO WHILE .T.
               IF EOF()
                  EXIT
               ENDIF
               IF TRIM(SORTNAME) = TRIM(c_sortname)
                  IF PLZ = n_plz .AND. UPPER(STRASSE) = UPPER(c_strasse)
                     n_markerror = 1
                     c_errortext = "Adresse vorhanden,"+;
                        " mit Gewerknummern:"+;
                        STR(GEWERK1)+c_b+;
                        STR(GEWERK2)+c_b+;
                        STR(GEWERK3)+c_b
                     KEYBOARD(REPLICATE(CHR(13),8))
                     n_neu    = 1
                     n_exiter = 1
                     EXIT
                  ELSE
                     CONTINUE
                  ENDIF
               ELSE
                  EXIT
               ENDIF
            ENDDO
         ENDIF
         IF n_exiter = 1
            n_exiter = 0
            EXIT
         ENDIF

         *               ---- LETZTE ADRESS-NUMMER IN HW-ADRES ----

         SELECT 1
         GO TOP
         LOCATE FOR ADRNR = 0
         IF FOUND() = .F.
            GO BOTTOM
            n_adrnr = RECNO() + 1
            APPEND BLANK
            IF (!LOCK())                           // Harbour addition
               c_errortext = "Der Satz ist aktuell gesperrt,"+;
                             " bitte spÑter probieren"
               EXIT                                // Harbour addition
            ENDIF                                  // Harbour addition
            REPLACE ADRNR     WITH 0
            REPLACE BERTADRNR WITH n_adrnr
            EXIT
         ELSE
            n_adrnr = BERTADRNR + 1
            n_bertadrnr = BERTADRNR + 1
            DO BESTAETIGEN
            IF n_exiter = 1
               n_exiter = 0
               n_markerror = 0
               c_errortext = " "
               EXIT
            ENDIF
            IF (!LOCK())                           // Harbour addition
               c_errortext = "Der Satz ist aktuell gesperrt,"+;
                             " bitte spÑter probieren"
               EXIT                                // Harbour addition
            ENDIF                                  // Harbour addition
            IF UPDATED()
               REPLACE BERTADRNR WITH n_bertadrnr
            ENDIF
         ENDIF

         *               ---- ABSPEICHERN ----

         CLEAR


         @ 10,30 SAY "Satznummer"
         @ 10,41 SAY n_adrnr
         @ 12,39 SAY "Abgespeichert !"
         SELECT 1
         SET INDEX TO HWSUP01,HWSUP02                 // Harbour change
         APPEND BLANK

         REPLACE ADRNR        WITH n_adrnr
         REPLACE BERTADRNR    WITH n_bertadrnr
         REPLACE KREDNR       WITH n_krednr
         REPLACE GEWERK1      WITH n_gk1
         REPLACE GEWERK2      WITH n_gk2
         REPLACE GEWERK3      WITH n_gk3
         REPLACE STATUS       WITH " "
         REPLACE AENDDAT      WITH DATE()
         REPLACE ANREDE       WITH c_anrede
         REPLACE TITEL        WITH c_titel
         REPLACE FIRMA        WITH c_firma
         REPLACE NAME         WITH c_name
         REPLACE SORTNAME     WITH c_sortname
         REPLACE VORNAME      WITH c_vorname
         REPLACE STRASSE      WITH c_strasse
         REPLACE PLZ          WITH n_plz
         REPLACE ORT          WITH c_ort
         REPLACE TELVW        WITH c_telvw
         REPLACE TELDW        WITH c_teldw
         REPLACE INFO         WITH c_info
         REPLACE KZUEBER      WITH "N"
         SET INDEX TO

         UNLOCK                                 // Harbour addition

         STORE 0000000   TO n_adrnr
         STORE SPACE(1)  TO c_status
         STORE 00000     TO n_krednr
         STORE 000       TO n_gk1
         STORE 000       TO n_ggk1
         STORE 000       TO n_gk2
         STORE 000       TO n_ggk2
         STORE 000       TO n_gk3
         STORE 000       TO n_ggk3
         STORE " "       TO c_anrede
         STORE SPACE(1)  TO c_titel
         STORE SPACE(30) TO c_firma
         STORE SPACE(30) TO c_firma1
         STORE SPACE(30) TO c_name
         STORE SPACE(30) TO c_name1
         STORE SPACE(15) TO c_vorname
         STORE SPACE(15) TO c_vorname1
         STORE SPACE(30) TO c_strasse
         STORE SPACE(30) TO c_strasse1
         STORE 0000      TO n_plz
         STORE SPACE(34) TO c_ort
         STORE SPACE(34) TO c_ort1
         STORE 00000     TO n_telvw
         STORE SPACE(5)  TO c_telvw
         STORE 000000000 TO n_teldw
         STORE SPACE(9)  TO c_teldw
         STORE SPACE(49) TO c_info
         STORE SPACE(12) TO c_sortname
         STORE 00000000 TO n_datn
         STORE 00       TO n_i
         STORE 00       TO n_k
         STORE 00       TO n_k0
         STORE 00       TO n_k1
         STORE SPACE(1) TO c_char
         STORE SPACE(11) TO c_rest
         STORE SPACE(11) TO c_begin
         STORE SPACE(11) TO c_beginelse
         STORE 0000     TO n_zaehler
         STORE 0        TO n_neu
         STORE 00       TO n_mark
         STORE 00       TO n_markerror
         STORE SPACE(79) TO c_errortext
         STORE SPACE(10) TO c_aasub01
         STORE 0        TO n_kleinster

         INKEY(2)

         CLEAR
         EXIT

      ENDIF

   END DO




END DO
   
RETURN

PROCEDURE ZEIGEGEWERK
**********************************************************************
* Das Unterprogramm ZEIGE_GEWERK zeigt die Gewerke aus der Datei     *
* AA-CODE                                                            *
**********************************************************************

   n_map = 3

   SELECT 2

   IF .NOT. l_map[n_map]


      DECLARE c_gewerk[32],c_gew_te[32]

      FOR n_i = 1 TO 32
         c_gewerk[n_i] = "   "
         c_gew_te[n_i] = "                             "
      NEXT

      SEEK "GEWERK   "

      n_i = 0
      DO WHILE funktion = "GEWERK"
         n_i = n_i + 1
         c_gewerk[n_i] = SUBSTR(code1,1,3)
         c_gew_te[n_i] = kurztext
         IF n_i = 32
            EXIT
         ENDIF
         SKIP
      ENDDO


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

      DO RAHMEN WITH "   ZulÑssige Gewerknummern   "

      @5,4   SAY c_gewerk[1]+" "+c_gew_te[1]
      @5,42  SAY c_gewerk[2]+" "+c_gew_te[2]
      @6,4   SAY c_gewerk[3]+" "+c_gew_te[3]
      @6,42  SAY c_gewerk[4]+" "+c_gew_te[4]
      @7,4   SAY c_gewerk[5]+" "+c_gew_te[5]
      @7,42  SAY c_gewerk[6]+" "+c_gew_te[6]
      @8,4   SAY c_gewerk[7]+" "+c_gew_te[7]
      @8,42  SAY c_gewerk[8]+" "+c_gew_te[8]
      @9,4   SAY c_gewerk[9]+" "+c_gew_te[9]
      @9,42  SAY c_gewerk[10]+" "+c_gew_te[10]
      @10,4   SAY c_gewerk[11]+" "+c_gew_te[11]
      @10,42  SAY c_gewerk[12]+" "+c_gew_te[12]
      @11,4   SAY c_gewerk[13]+" "+c_gew_te[13]
      @11,42  SAY c_gewerk[14]+" "+c_gew_te[14]
      @12,4   SAY c_gewerk[15]+" "+c_gew_te[15]
      @12,42  SAY c_gewerk[16]+" "+c_gew_te[16]
      @13,4   SAY c_gewerk[17]+" "+c_gew_te[17]
      @13,42  SAY c_gewerk[18]+" "+c_gew_te[18]
      @14,4   SAY c_gewerk[19]+" "+c_gew_te[19]
      @14,42  SAY c_gewerk[20]+" "+c_gew_te[20]
      @15,4   SAY c_gewerk[21]+" "+c_gew_te[21]
      @15,42  SAY c_gewerk[22]+" "+c_gew_te[22]
      @16,4   SAY c_gewerk[23]+" "+c_gew_te[23]
      @16,42  SAY c_gewerk[24]+" "+c_gew_te[24]
      @17,4   SAY c_gewerk[25]+" "+c_gew_te[25]
      @17,42  SAY c_gewerk[26]+" "+c_gew_te[26]
      @18,4   SAY c_gewerk[27]+" "+c_gew_te[27]
      @18,42  SAY c_gewerk[28]+" "+c_gew_te[28]
      @19,4   SAY c_gewerk[29]+" "+c_gew_te[29]
      @19,42  SAY c_gewerk[30]+" "+c_gew_te[30]
      @20,4   SAY c_gewerk[31]+" "+c_gew_te[31]
      @20,42  SAY c_gewerk[32]+" "+c_gew_te[32]

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


*************************** ENDE ZEIGEGEWERK *************************


PROCEDURE STATUSPRUEFUNG
**********************************************************************
* Das Unterprogramm STATUSPRUEFUNG prÅft den STATUS                  *
**********************************************************************

n_markerror = 0

IF c_status <> " " .AND. c_status <> "L"
   c_errortext = "STATUS kann nur 'L' oder ' ' sein !               "
   n_markerror = 1
   KEYBOARD(REPLICATE(CHR(13),0))
ENDIF

*************************** ENDE STATUSPRUEFUNG **********************


PROCEDURE KREDNRPRUEFUNG
************************************************************************
* Das Unterprogramm KREDNRPRUEFUNG prÅft die KREDITNR                  *
* Der Parameter n_mode ist 0 falls es sich um einen neuen Satz handelt *
* Falls es sich um eine énderung eines schon existierenden Satzes      *
* handelt, so enthÑlt n_mode die physosche Satznummer ( RECNO() )      *
* dieses Satzes. In diesem Falle darf ein gefundener Satz mit der      *
* selben Kreditorennummer exostoeren falls es physisch der selbe Satz  *
* ist                                                                  *
************************************************************************
PARAMETER n_mode

n_markerror = 0

c_errortext = " "
SELECT 1
GO TOP
IF n_krednr <> 0
   LOCATE FOR KREDNR = n_krednr
   IF FOUND() = .T.
      IF n_mode = 0 .OR. n_mode # RECNO()
         n_markerror = 1
         c_errortext = "Diese Kreditorennr ist schon vergeben !"
         KEYBOARD(REPLICATE(CHR(13),1))
      ENDIF
   ENDIF
ELSE
   n_markerror = 1
   c_errortext = "Kreditorennr darf nicht NULL sein !"
   KEYBOARD(REPLICATE(CHR(13),1))
ENDIF

*************************** ENDE KREDNRPRUEFUNG **********************
   

PROCEDURE GEWERKPRUEFUNG
**********************************************************************
* Das Unterprogramm GEWERKPRUEFUNG prÅft die GEWERLNR                  *
**********************************************************************
n_markerror = 0
n_kleinster = 0
n_betrag    = 999
n_ggk1      = 0
n_ggk2      = 0
n_ggk3      = 0

IF n_gk1 > 0 .OR. n_gk2 > 0 .OR. n_gk3 > 0
   FOR n_i = 1 TO 3
      FOR n_k = 1 TO 3
         DO CASE
            CASE n_k = 1
               n_vergleicher = n_gk1
            CASE n_k = 2
               n_vergleicher = n_gk2
            CASE n_k = 3
               n_vergleicher = n_gk3
         ENDCASE
         IF n_vergleicher # 0 .AND. n_vergleicher < n_betrag
            STORE n_k TO n_kleinster
            STORE n_vergleicher TO n_betrag
         ENDIF
      NEXT
      IF n_i = 1
         DO CASE
            CASE n_kleinster = 1
               STORE n_betrag  TO n_ggk1
               STORE 0         TO n_gk1
            CASE n_kleinster = 2
               STORE n_betrag  TO n_ggk1
               STORE 0         TO n_gk2
            CASE n_kleinster = 3
               STORE n_betrag  TO n_ggk1
               STORE 0         TO n_gk3
         ENDCASE
      ENDIF
      IF n_i = 2
         DO CASE
            CASE n_kleinster = 1
               STORE n_betrag  TO n_ggk2
               STORE 0         TO n_gk1
            CASE n_kleinster = 2
               STORE n_betrag  TO n_ggk2
               STORE 0         TO n_gk2
            CASE n_kleinster = 3
               STORE n_betrag  TO n_ggk2
               STORE 0         TO n_gk3
         ENDCASE
      ENDIF
      IF n_i = 3
         DO CASE
            CASE n_kleinster = 1
               STORE n_betrag  TO n_ggk3
               STORE 0         TO n_gk1
            CASE n_kleinster = 2
               STORE n_betrag  TO n_ggk3
               STORE 0         TO n_gk2
            CASE n_kleinster = 3
               STORE n_betrag  TO n_ggk3
               STORE 0         TO n_gk3
         ENDCASE
      ENDIF
      n_betrag = 999
      n_kleinster = 0
   NEXT
   n_gk1 = n_ggk1
   n_gk2 = n_ggk2
   n_gk3 = n_ggk3
   STORE 0 TO n_ggk1,n_ggk2,n_ggk3
    
   IF n_gk2 = 0 .AND. n_gk3 = 0
      n_i = 1
   ELSE
      IF n_gk1 = n_gk2 .OR.;
         n_gk1 = n_gk3 .OR.;
         n_gk2 = n_gk3
         n_markerror = 1
         c_errortext = "Nicht mehrmals die gleiche Gewerknr. eingeben! "
         KEYBOARD(REPLICATE(CHR(13),3))
      ENDIF
   ENDIF
ELSE
   n_markerror = 1
   c_errortext = "Mindestens ein Gewerk eingeben !"
   KEYBOARD(REPLICATE(CHR(13),2))
ENDIF

IF c_errortext = " "
   SELECT 2
   FOR n_i = 1 TO 3
      GO TOP
      DO CASE
         CASE n_i = 1
            n_ggk1 = n_gk1
            n_k = 1
         CASE n_i = 1
            n_ggk1 = n_gk2
            n_k = 2
         CASE n_i = 1
            n_ggk1 = n_gk3
            n_k = 3
      ENDCASE
      IF n_ggk1 > 0
         SEEK "GEWERK    "
         DO WHILE FUNKTION = "GEWERK"
            IF SUBSTR(TRIM(CODE1),1,3) = STR(n_ggk1,3)
               n_isteiner = 1
               EXIT
            ENDIF
            SKIP
         ENDDO
         IF n_isteiner = 0
            n_markerror = 1
            c_errortext = "Gewerk falsch ! ZulÑssige"+ ;
                          " Gewewrknummern eingeben!"
            KEYBOARD(REPLICATE(CHR(13),n_k+1))
            RETURN
         ELSE
            n_isteiner = 0
         ENDIF
      ENDIF
   NEXT
ENDIF

RETURN
*************************** ENDE GEWERKPRUEFUNG **********************
   


PROCEDURE NAMENSPRUEFUNG
**********************************************************************
* Das Unterprogramm NAMENSPRUEFUNG prÅft die ANREDRL,VORNAME,NAME    *
**********************************************************************

n_markerror = 0

SELECT 2
GO TOP

DO WHILE .T.

   IF c_name = " "
      c_errortext = "Bitte Name eingeben !                            "
      n_markerror = 1     
      KEYBOARD(REPLICATE(CHR(13),8))
      EXIT
   ENDIF

   c_anrede1 = " "
   c_anrede2 = " "
   IF LEN(TRIM(c_anrede)) # 0
      SEEK "ANREDE    "+c_anrede
      IF .NOT. FOUND()
         c_errortext = "ANREDE ungÅltig !                             "
         n_markerror = 1     
         c_anrede1 = " "
         KEYBOARD(REPLICATE(CHR(13),5))
         EXIT
      ENDIF
   ENDIF

   c_titel1 = " "
   IF LEN(TRIM(c_titel)) # 0
      SEEK "TITEL     "+c_titel
      IF .NOT. FOUND()
         c_errortext = "TITEL ungÅltig !                              "
         n_markerror = 1     
         KEYBOARD(REPLICATE(CHR(13),6))
         EXIT
      ENDIF
   ENDIF

   EXIT

ENDDO

RETURN
*************************** ENDE NAMENSPRUEFUNG **********************




PROCEDURE ADRESSPRUEFUNG
**********************************************************************
* Das Unterprogramm ADRESSPRUEFUNG prÅft die ADRESSE                 *
**********************************************************************
   
n_markerror = 0

DO WHILE .T.

   IF c_strasse = " "
      c_errortext = "Bitte Strasse eingeben !                     "
      n_markerror = 1
      KEYBOARD(REPLICATE(CHR(13),10))
      EXIT
   ENDIF

   IF n_plz = 0
      c_errortext = "Bitte Postleitzahl eingeben !                "
      n_markerror = 1
      KEYBOARD(REPLICATE(CHR(13),11))
      EXIT
   ELSE
      DO CASE
         CASE n_plz < 10
            n_plz = n_plz * 1000
         CASE n_plz < 100
            n_plz = n_plz * 100
         CASE n_plz < 1000
            n_plz = n_plz * 100
      ENDCASE
   ENDIF

   IF c_ort = " "
      c_errortext = "Bitte Ort eingeben !                         "
      n_markerror = 1
      KEYBOARD(REPLICATE(CHR(13),12))
      EXIT
   ENDIF

   EXIT

ENDDO

RETURN

*************************** ENDE ADRESSPRUEFUNG ******************



PROCEDURE TELEFONPRUEFUNG
**********************************************************************
* Das Unterprogramm TELEFONPRUEFUNG prÅft die Telefon-Felder         *
**********************************************************************
   
n_markerror = 0

DO WHILE .T.

   IF c_telvw <> " "
      n_telvw = VAL(c_telvw)
      IF n_telvw < 10
         c_errortext = "VORWAHL falsch !                                "
         n_markerror = 1
         KEYBOARD(REPLICATE(CHR(13),13))
         EXIT
      ENDIF
   ENDIF
   IF c_teldw <> " "
      n_teldw = VAL(c_teldw)
      IF n_teldw < 10
         c_errortext = "DURCHWAHL falsch !                                "
         n_markerror = 1
         KEYBOARD(REPLICATE(CHR(13),14))
         EXIT
      ENDIF
   ENDIF
   IF c_telvw <> " " .AND. c_teldw = " "
      c_errortext = "Bitte auch DURCHWAHL eingeben !                     "
      n_markerror = 1
      KEYBOARD(REPLICATE(CHR(13),14))
      EXIT
   ENDIF
   IF c_teldw <> " " .AND. c_telvw = " "
      c_errortext = "Bitte auch VORWAHL eingeben !                       "
      n_markerror = 1
      KEYBOARD(REPLICATE(CHR(13),13))
      EXIT
   ENDIF
   EXIT
ENDDO

*************************** ENDE TELEFONPRUEFUNG **********************
   


PROCEDURE BESTAETIGEN
***********************************************************************
* Das Unterprogramm BESTAETIGEN zeigt vor dem Abspeichern die Daten.  *
***********************************************************************

SET KEY -9 TO HELPMASK1

FOR n_i = 2 TO 27
   STORE SPACE(30) TO f_keytext[n_i]
NEXT

f_key[2] = "ZURöCK"
f_key[3] = "      "
f_key[4] = "      "
f_key[5] = "      "
f_key[6] = "      "
f_key[7] = " TITEL"
f_key[8] = "ANREDE"
f_key[9] = "GEWERK"
f_key[10] = " HELP "
f_keytext[2] = " Beender das Programm !"
f_keytext[5] = " ZurÅck zur Auswahl"
f_keytext[20] = " ZulÑssige Titel anzeigen"
f_keytext[23] = " ZulÑssige Anreden anzeigen"
f_keytext[26] = " ZulÑssige Gewerke anzeigen"

c_errortext = "ENTER = bestÑtigen und abspeichern"

n_map = 2

IF .NOT. l_map[n_map]
   CLEAR                                     // Bugfix
   @ 5,3 SAY "ADRESSEN-NR :"
   @ 5,30 SAY "STATUS      :"
   @ 5,50 SAY "( 'L'=LôSCHEN; ' '=AKTIV;)"
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
   n_map = 2
   FOR n_i = 2 TO 27
      STORE SPACE(30) TO f_keytext[n_i]
   NEXT
   
   f_key[2] = "ZURöCK"
   f_key[3] = "      "
   f_key[4] = "      "
   f_key[5] = "      "
   f_key[6] = "      "
   f_key[7] = " TITEL"
   f_key[8] = "ANREDE"
   f_key[9] = "GEWERK"
   f_key[10] = " HELP "
   f_keytext[2] = " Beender das Programm !"
   f_keytext[5] = " ZurÅck zur Auswahl"
   f_keytext[20] = " ZulÑssige Titel anzeigen"
   f_keytext[23] = " ZulÑssige Anreden anzeigen"
   f_keytext[26] = " ZulÑssige Gewerke anzeigen"
   
   RESTORE SCREEN FROM m_map[n_map]

   @ 5,18 SAY n_adrnr
   @ 5,47 SAY c_status
   @ 6,18 SAY n_krednr
   @ 6,47 SAY n_gk1
   @ 6,57 SAY n_gk2
   @ 6,67 SAY n_gk3
   @ 8,18 SAY c_anrede
   @ 9,18 SAY c_titel
   @ 10,18 SAY c_firma
   @ 11,18 SAY c_name
   @ 12,18 SAY c_vorname
   @ 13,18 SAY c_strasse
   @ 14,18 SAY n_plz
   @ 15,18 SAY c_ort
   @ 16,18 SAY c_telvw
   @ 16,32 SAY c_teldw
   @ 18,18 SAY c_info

   IF c_errortext <> " "
      @ 0,0
      @ 0,2 SAY c_errortext
   ENDIF

   n_int_key = 0
   @ 3,71 SAY TIME()

   WAIT ""

   c_errortext = "ENTER = bestÑtigen und abspeichern"

   FOR n_i = 2 TO 9
      IF (n_int_key = n_i .AND. LEN(TRIM(f_key[n_i])) = 0 )
         c_errortext = "FEHLER: Funktionstaste wird nicht unterstÅtzt"
         LOOP
      ENDIF
   NEXT

   IF n_int_key = 2
      n_exiter = 1
      SET KEY -9 TO HELPMASK
      EXIT
   ENDIF

   IF n_int_key = 7
      DO ZEIGETITEL
      LOOP
   ENDIF

   IF n_int_key = 8
      DO ZEIGEANREDE
      LOOP
   ENDIF

   IF n_int_key = 9
      DO ZEIGEGEWERK
      LOOP
   ENDIF

   IF n_int_key = 10
      LOOP
   ENDIF

   IF n_int_key <> 2 .AND. n_int_key <> 9
      IF LASTKEY() = 13
         SET KEY -9 TO HELPMASK
         EXIT
      ENDIF
   ENDIF
ENDDO

c_errortext = " "

RETURN

*************************** ENDE BESTAETIGEN **************************
   

PROCEDURE GEWERKSORT
***********************************************************************
* Das Unterprogramm GEWERKSORT sortiert die drei eingegebenen         *
* Gewerke nach ihrer Grîsse hin fallend.                              *
***********************************************************************
c_errortext = " "
STORE 0 TO n_ggk1,n_ggk2,n_ggk3,n_vergleicher,n_betrag,n_grosster
STORE 0 TO n_i,n_k

IF n_gk1 > 0 .OR. n_gk2 > 0 .OR. n_gk3 > 0
   FOR n_i = 1 TO 3
      FOR n_k = 1 TO 3
         DO CASE
            CASE n_k = 1
               n_vergleicher = n_gk1
            CASE n_k = 2
               n_vergleicher = n_gk2
            CASE n_k = 3
               n_vergleicher = n_gk3
         ENDCASE
         IF n_vergleicher > n_betrag
            STORE n_k TO n_grosster
            STORE n_vergleicher TO n_betrag
         ENDIF
      NEXT
      IF n_i = 1
         DO CASE
            CASE n_grosster = 1
               STORE n_betrag  TO n_ggk1
               STORE 0         TO n_gk1
            CASE n_grosster = 2
               STORE n_betrag  TO n_ggk1
               STORE 0         TO n_gk2
            CASE n_grosster = 3
               STORE n_betrag  TO n_ggk1
               STORE 0         TO n_gk3
         ENDCASE
      ENDIF
      IF n_i = 2
         DO CASE
            CASE n_grosster = 1
               STORE n_betrag  TO n_ggk2
               STORE 0         TO n_gk1
            CASE n_grosster = 2
               STORE n_betrag  TO n_ggk2
               STORE 0         TO n_gk2
            CASE n_grosster = 3
               STORE n_betrag  TO n_ggk2
               STORE 0         TO n_gk3
         ENDCASE
      ENDIF
      IF n_i = 3
         DO CASE
            CASE n_grosster = 1
               STORE n_betrag  TO n_ggk3
               STORE 0         TO n_gk1
            CASE n_grosster = 2
               STORE n_betrag  TO n_ggk3
               STORE 0         TO n_gk2
            CASE n_grosster = 3
               STORE n_betrag  TO n_ggk3
               STORE 0         TO n_gk3
         ENDCASE
      ENDIF
      n_betrag = 0
      n_grosster = 0
   NEXT
   n_gk1 = n_ggk1
   n_gk2 = n_ggk2
   n_gk3 = n_ggk3

   IF n_gk2 = 0 .AND. n_gk3 = 0
      n_i = 1
   ELSE
      IF n_gk1 = n_gk2 .OR.;
         n_gk1 = n_gk3 .OR.;
         n_gk2 = n_gk3c_errortext = "Nicht mehrmals die gleiche Gewerknr. eingeben!"
         KEYBOARD(REPLICATE(CHR(13),4))
      ENDIF
   ENDIF
ENDIF
   
*************************** ENDE GEWERKSORT ****************************

* EOF: HW0110.PRG
         