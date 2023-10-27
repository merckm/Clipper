* Program.: HW0130.PRG
* Author..: Martin Merck
* Datum...: 13. September 1987
* Notiz...:
* Bemerk..: Profilauswahl und PlausibilitÑtsprÅfungen

*----------------------------------------------------------------------*
* Initialisierungen
*----------------------------------------------------------------------*

* Alle offenen Dateien schlie·en
CLOSE DATABASES

PNAME = "HW0130"

*----------------------------------------------------------------------*
* TemporÑre Dateien lîschen falls noch vorhanden
*----------------------------------------------------------------------*

IF FILE ("TEMP2.DBF")
   ERASE TEMP2.DBF
ENDIF

IF FILE ("TEMP01.NTX")
   ERASE TEMP01.NTX
ENDIF

IF FILE ("TEMP02.NTX")
   ERASE TEMP02.NTX
ENDIF

************************************************************************
*          B E G I N   D E S   H A U P T P R o G R A M M S             *
************************************************************************

DO WHILE .T.

*----------------------------------------------------------------------*
* Maske auf dem Bildschirm ausgeben
*----------------------------------------------------------------------*

   DECLARE n_plz_von[5],n_plz_bis[5]

   c_error_t = " "
   n_adr_von = 0
   n_adr_bis = 0
   n_gew_von = 0
   n_gew_bis = 0
   n_plz_von[1] = 0
   n_plz_bis[1] = 0
   n_plz_von[2] = 0
   n_plz_bis[2] = 0
   n_plz_von[3] = 0
   n_plz_bis[3] = 0
   n_plz_von[4] = 0
   n_plz_bis[4] = 0
   n_plz_von[5] = 0
   n_plz_bis[5] = 0
   n_plz_k = 0
   n_name_k = 0
   n_gewerk_k = 0
   n_adrnr_k = 0
   c_aus_anzeigen  = " "
   c_aus_liste     = " "
   c_aus_aufkleber = " "
   c_aus_brief     = " "
   c_sort_k        = " "

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
   f_key[9] = "GEWERKE"
   f_key[10] = " HILFE "
   
   f_keytext[5] = " ZurÅck zum Hauptmenue !"
   f_keytext[26] = " ErklÑrung Gewerk"
   f_keytext[29] = " Bedeutung der Funktionstasten"
   
* Maske ausgeben
   
   CLEAR

   IF .NOT. l_map[n_map] = .T.
   
      DO RAHMEN WITH "PROFILAUSWAHL - HANDWERKER"

      @5,5   SAY "ADRESSEN-NR.:"
      @5,29  SAY "BIS"
      @6,5   SAY "GEWERK:"
      @6,29  SAY "BIS"
      @7,5   SAY "PLZ:"
      @7,29  SAY "BIS"
      @8,29  SAY "BIS"
      @9,29  SAY "BIS"
      @10,29 SAY "BIS"
      @11,29 SAY "BIS"
      @13,5  SAY "SORTIERUNG:  REIHENFOLGE MIT 1-2-3 ANGEBEN"
      @15,10 SAY "PLZ    NAME    GEWERK    ADRESSENNUMMER"
      @17,5 SAY "AUSGABE ANZEIGEN:"
      @18,14 SAY "LISTE:"
      @19,14 SAY "ADRESS-AUFKLEBER:"
      @20,14 SAY "BRIEF:"

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
      f_key[9] = "GEWERKE"
      f_key[10] = " HILFE "
      
      f_keytext[5] = " ZurÅck zum Hauptmenue !"
      f_keytext[26] = " ErklÑrung Gewerk"
      f_keytext[29] = " Bedeutung der Funktionstasten"
      
      RESTORE SCREEN FROM m_map[n_map]
      
      n_int_key = 0
      @ 3,71 SAY TIME()
      @ 5,21 GET n_adr_von PICTURE "9999999"
      @ 5,34 GET n_adr_bis PICTURE "9999999"
      @ 6,21 GET n_gew_von PICTURE "999"
      @ 6,34 GET n_gew_bis PICTURE "999"
      @ 7,21 GET n_plz_von[1] PICTURE "9999"
      @ 7,34 GET n_plz_bis[1] PICTURE "9999"
      @ 8,21 GET n_plz_von[2] PICTURE "9999"
      @ 8,34 GET n_plz_bis[2] PICTURE "9999"
      @ 9,21 GET n_plz_von[3] PICTURE "9999"
      @ 9,34 GET n_plz_bis[3] PICTURE "9999"
      @ 10,21 GET n_plz_von[4] PICTURE "9999"
      @ 10,34 GET n_plz_bis[4] PICTURE "9999"
      @ 11,21 GET n_plz_von[5] PICTURE "9999"
      @ 11,34 GET n_plz_bis[5] PICTURE "9999"
      @ 15,15 GET n_plz_k PICTURE "9"
      @ 15,23 GET n_name_k PICTURE "9"
      @ 15,33 GET n_gewerk_k PICTURE "9"
      @ 15,51 GET n_adrnr_k PICTURE "9"
      @ 17,33 GET c_aus_anzeigen  PICTURE "!"
      @ 18,33 GET c_aus_liste     PICTURE "!"
      @ 19,33 GET c_aus_aufkleber PICTURE "!"
      @ 20,33 GET c_aus_brief     PICTURE "!"

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
   
      IF n_int_key = 9
         DO zeige_gewerk
         LOOP
      ENDIF

      IF n_int_key = 10
         LOOP
      ENDIF

*----------------------------------------------------------------------*
* PrÅfung der Eingabe
*----------------------------------------------------------------------*

      @ 0,0
      @ 0,2 SAY c_error_t
      n_hz_index = 0

      IF (n_adr_von # 0 .OR. n_adr_bis # 0 )
         IF n_adr_von <= 0
            c_error_t = "*** Adress-Nr falsch ***"
            LOOP
         ENDIF
         IF n_adr_bis <= 0
            c_error_t = "*** Adress-Nr falsch ***"
            KEYBOARD(CHR(13))
            LOOP
         ENDIF
         IF n_adr_von > n_adr_bis
            c_error_t = "*** Intervall fÅr Adress-Nr ist falsch ***"
            LOOP
         ENDIF
         n_hz_index = 1

      ELSE
         n_adr_von = 1
         n_adr_bis = 9999999
      ENDIF

      IF (n_gew_von # 0 .OR. n_gew_bis # 0 )
         SELECT 10
         USE aacode INDEX aasup01 SHARED              // Added SHARED for Harbour
         GO TOP
         SEEK "GEWERK    "+STR(n_gew_von,3)
         IF EOF()
            c_error_t = "*** Gewerk ist falsch ***"
            KEYBOARD(REPLICATE(CHR(13),2))
            LOOP
         ENDIF
         GO TOP
         SEEK "GEWERK    "+STR(n_gew_bis,3)
         IF EOF()
            c_error_t = "*** Gewerk ist falsch ***"
            KEYBOARD(REPLICATE(CHR(13),3))
            LOOP
         ENDIF
         USE
         SELECT 1
         IF n_gew_von > n_gew_bis
            c_error_t = "*** Intervall fÅr Gewerk ist falsch ***"
            KEYBOARD(REPLICATE(CHR(13),3))
            LOOP
         ENDIF
         n_hz_index = 2

      ELSE
         n_gewerk_bis = 999
      ENDIF

      n_leer = 0
      FOR i = 1 TO 5

         IF (n_plz_von[i] # 0 .OR. n_plz_bis[i] # 0 )
            IF n_adr_von <= 0
               c_error_t = "*** Postleitzahl falsch ***"
               KEYBOARD(REPLICATE(CHR(13),2*(i-1)+4))
               LOOP
            ENDIF
            IF n_adr_bis <= 0
               c_error_t = "*** Postleitzahl falsch ***"
               KEYBOARD(REPLICATE(CHR(13),2*(i-1)+5))
               LOOP
            ENDIF
            n_plz = n_plz_von[i]
            DO norm_plz WITH n_plz
            n_plz_von[i] = n_plz
            n_plz = n_plz_bis[i]
            DO norm_plz WITH n_plz
            n_plz_bis[i] = n_plz
            DO norm_plz WITH n_plz_bis[i]
            IF n_plz_von[i] > n_plz_bis[i]
               c_error_t = "*** Intervall fÅr Postleitzahl ist falsch ***"
               KEYBOARD(REPLICATE(CHR(13),2*(i-1)+4))
               LOOP
            ENDIF
            n_hz_index = 3 + (i - 1) * 2
   
         ELSE
            n_leer = n_leer + 1
         ENDIF
 
      NEXT
      IF n_leer = 5
         n_plz_von[1] = 1000
         n_plz_bis[1] = 9999
      ENDIF
      
      IF n_hz_index = 0
         c_error_t = "*** Mindestens eine Selektions-Angabe machen ***"
         LOOP
      ENDIF

      if ( n_plz_k # 0 .AND. n_name_k # 0 .AND.    ;
           n_gewerk_k # 0 .AND. n_adrnr_k # 0 )
           c_error_t = "*** Nur maximal 3 Sortier-Kriterin ***"
         KEYBOARD(REPLICATE(CHR(13),14))
         LOOP
      ENDIF

      IF ( n_plz_k < 0 .AND. n_plz_k > 3 )
         c_error_t = "*** Nur 1 ... 3 eingeben ***"
         KEYBOARD(REPLICATE(CHR(13),14))
         LOOP
      ENDIF

      IF ( n_name_k < 0 .AND. n_name_k > 3 )
         c_error_t = "*** Nur 1 ... 3 eingeben ***"
         KEYBOARD(REPLICATE(CHR(13),15))
         LOOP
      ENDIF

      IF ( n_gewerk_k < 0 .AND. n_gewerk_k > 3 )
         c_error_t = "*** Nur 1 ... 3 eingeben ***"
         KEYBOARD(REPLICATE(CHR(13),16))
         LOOP
      ENDIF

      IF ( n_adrnr_k < 0 .AND. n_adrnr_k > 3 )
         c_error_t = "*** Nur 1 ... 3 eingeben ***"
         KEYBOARD(REPLICATE(CHR(13),17))
         LOOP
      ENDIF

      n_k1 = 0
      n_k2 = 0
      n_k3 = 0
      c_sort_k = " "
      c_text1 = " "
      c_text2 = " "
      c_text3 = " "
      c_s_text1 = " "
      c_s_text2 = " "
      c_s_text3 = " "
      DO CASE
         CASE n_plz_k = 1
            c_text1 = "STR(plz,4)"
            c_s_text1 = "POSTLEITZAHL   "
            n_k1 = n_k1 + 1
         CASE n_plz_k = 2
            c_text2 = "STR(plz,4)"
            c_s_text2 = "POSTLEITZAHL   "
            n_k2 = n_k2 + 1
         CASE n_plz_k = 3
            c_text3 = "STR(plz,4)"
            c_s_text3 = "POSTLEITZAHL   "
            n_k3 = n_k3 + 1
      ENDCASE
      DO CASE
         CASE n_name_k = 1
            c_text1 = "sortname"
            c_s_text1 = "NAME           "
            n_k1 = n_k1 + 1
         CASE n_name_k = 2
            c_text2 = "sortname"
            c_s_text2 = "NAME           "
            n_k2 = n_k2 + 1
         CASE n_name_k = 3
            c_text3 = "sortname"
            c_s_text3 = "NAME           "
            n_k3 = n_k3 + 1
      ENDCASE
      DO CASE
         CASE n_gewerk_k = 1
            c_text1 = "STR(gewerk1,3)"
            c_s_text1 = "GEWERK         "
            n_k1 = n_k1 + 1
         CASE n_gewerk_k = 2
            c_text2 = "STR(gewerk1,3)"
            c_s_text2 = "GEWERK         "
            n_k2 = n_k2 + 1
         CASE n_gewerk_k = 3
            c_text3 = "STR(gewerk1,3)"
            c_s_text3 = "GEWERK         "
            n_k3 = n_k3 + 1
      ENDCASE
      DO CASE
         CASE n_adrnr_k = 1
            c_text1 = "STR(adrnr,7)"
            c_s_text1 = "ADRESS_NUMMER  "
            n_k1 = n_k1 + 1
         CASE n_adrnr_k = 2
            c_text2 = "STR(adrnr,7)"
            c_s_text2 = "ADRESS_NUMMER  "
            n_k2 = n_k2 + 1
         CASE n_adrnr_k = 3
            c_text3 = "STR(adrnr,7)"
            c_s_text3 = "ADRESS_NUMMER  "
            n_k3 = n_k3 + 1
      ENDCASE

      c_sort_k     = c_text1+c_text2+c_text3
      c_sort_text  = c_s_text1+c_s_text2+c_s_text3

      IF ( n_k1 > 1 .OR. n_k2 > 1 .OR. n_k3 > 1 )
         c_error_t = "*** Sortierkriterien dÅrfen sich nicht wiederholen ***"
         KEYBOARD(REPLICATE(CHR(13),14))
         LOOP
      ENDIF

      IF ( n_k1 = 0 .AND. ( n_k2 # 0 .OR. n_k3 # 0 ) )
         c_error_t = "*** Kriterium 1 fÅr Sortierung eingeben ***"
         KEYBOARD(REPLICATE(CHR(13),14))
         LOOP
      ENDIF

      IF ( n_k2 = 0 .AND. n_k3 # 0 )
         c_error_t = "*** Kriterium 2 fÅr Sortierung eingeben ***"
         KEYBOARD(REPLICATE(CHR(13),14))
         LOOP
      ENDIF

      n_index1 = 0

      IF LEN(TRIM(c_aus_anzeigen)) > 0
         n_index1 = n_index1 + 1
      ENDIF
      IF LEN(TRIM(c_aus_liste)) > 0
         n_index1 = n_index1 + 1
      ENDIF
      IF LEN(TRIM(c_aus_aufkleber)) > 0
         n_index1 = n_index1 + 1
      ENDIF
      IF LEN(TRIM(c_aus_brief)) > 0
         n_index1 = n_index1 + 1
      ENDIF

      IF n_index1 = 0
         c_error_t = "*** Ausgabe auswÑhlen ***"
         KEYBOARD(REPLICATE(CHR(13),18))
         LOOP
      ENDIF

      IF n_index1 > 1
         c_error_t = "*** Nur eine Ausgabe auswÑhlen ***"
         KEYBOARD(REPLICATE(CHR(13),18))
         LOOP
      ENDIF


      EXIT

   ENDDO

*----------------------------------------------------------------------*
* Auswertung der Auswahl
*----------------------------------------------------------------------*

   IF ! FILE("TEMP2.DBF")
      USE HWADRES
      COPY TO TEMP3 STRUCTURE EXTENDED
      USE
      USE TEMP3
      APPEND BLANK
      REPLACE field_name WITH "ANREDE1"
      REPLACE field_type WITH "C"
      REPLACE field_len  WITH 25
      APPEND BLANK
      REPLACE field_name WITH "ANREDE2"
      REPLACE field_type WITH "C"
      REPLACE field_len  WITH 25
      APPEND BLANK
      REPLACE field_name WITH "TITEL1"
      REPLACE field_type WITH "C"
      REPLACE field_len  WITH 25
      CREATE TEMP2 FROM TEMP3
   ELSE
      IF ! FILE("TEMP02-NTX")
         USE TEMP2 INDEX TEMP01
      ELSE
         USE TEMP2 INDEX TEMP01,TEMP02
      ENDIF
      ZAP
      USE
   ENDIF

   SELECT 1
   USE HWADRES INDEX HWSUP02 ALIAS HW

   SELECT 2
   USE TEMP2
   INDEX ON adrnr TO TEMP01
   IF LEN(TRIM(c_sort_k)) > 0
      INDEX ON c_sort_k TO TEMP02
   ENDIF
   USE
   IF LEN(TRIM(c_sort_k)) > 0
      USE TEMP2 INDEX TEMP01,TEMP02 ALIAS TMP
   ELSE
      USE TEMP2 INDEX TEMP01 ALIAS TMP
   ENDIF
   SET ORDER TO 1

   SELECT 3
   USE AACODE INDEX AASUP01 ALIAS AA SHARED              // Added SHARED for Harbour

   SELECT 1

   @ 0,0
   @ 0,2 SAY "System sucht, und findet     Satz."
   n_i = 0

   FOR i = 1 TO 5

      IF n_plz_bis[i] > 0

         GO TOP
         LOCATE FOR adrnr >= n_adr_von .AND. plz >= n_plz_von[i]

         DO WHILE .T.

            IF EOF()
               EXIT
            ENDIF
            IF status $ "Ll"
               SKIP 1
               LOOP
            ENDIF
            IF adrnr > n_adr_bis
               SKIP 1
               LOOP
            ENDIF
            IF ((gewerk1 < n_gew_von .OR. gewerk1 > n_gew_bis ) .AND. ;
                (gewerk2 < n_gew_von .OR. gewerk2 > n_gew_bis ) .AND. ;
                (gewerk3 < n_gew_von .OR. gewerk3 > n_gew_bis ))
               SKIP 1
               LOOP
            ENDIF
            IF plz > n_plz_bis[i]
               EXIT
            ENDIF

            SELECT 2
            SEEK hw->adrnr
            IF .NOT. FOUND()
               n_i = n_i + 1
               IF n_i = 2
                  @ 0,2 SAY "System sucht, und findet     SÑtze."
               ENDIF
               @ 0,27 SAY n_i PICTURE "999"
               SELECT 3
               GO TOP
               c_anrede1 = " "
               c_anrede2 = " "
               c_titel1  = " "
               SEEK "ANREDE    "+hw->anrede
               IF FOUND()
                  c_anrede1 = kurztext
               ENDIF
               SEEK "ANREDE2   "+hw->anrede
               IF FOUND()
                  c_anrede2 = kurztext
               ENDIF
               SEEK "TITEL     "+hw->titel
               IF FOUND()
                  c_titel1 = kurztext
               ENDIF
               SELECT 2
               APPEND BLANK
               REPLACE adrnr     WITH hw->adrnr
               REPLACE bertadrnr WITH hw->bertadrnr
               REPLACE krednr    WITH hw->krednr
               REPLACE gewerk1   WITH hw->gewerk1
               REPLACE gewerk2   WITH hw->gewerk2
               REPLACE gewerk3   WITH hw->gewerk3
               REPLACE status    WITH hw->status
               REPLACE aenddat   WITH hw->aenddat
               REPLACE anrede    WITH hw->anrede
               REPLACE anrede1   WITH c_anrede1
               REPLACE anrede2   WITH c_anrede2
               REPLACE titel     WITH hw->titel
               REPLACE titel1    WITH c_titel1
               REPLACE firma     WITH hw->firma
               REPLACE name      WITH hw->name
               REPLACE sortname  WITH hw->sortname
               REPLACE vorname   WITH hw->vorname
               REPLACE strasse   WITH hw->strasse
               REPLACE plz       WITH hw->plz
               REPLACE ort       WITH hw->ort
               REPLACE telvw     WITH hw->telvw
               REPLACE teldw     WITH hw->teldw
               REPLACE info      WITH hw->info
            ENDIF

            SELECT 1
            SKIP 1
         ENDDO

      ENDIF

   NEXT

   SELECT 2
   SET ORDER TO 0
   GO BOTTOM
   IF EOF()
      n_anzahl = 0
   ELSE
      n_anzahl = recno()
   ENDIF
   SELECT 1

   IF n_anzahl  = 0
      n_map = 11

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
      
* Maske ausgeben
      
      IF .NOT. l_map[n_map]

      CLEAR
      
         DO RAHMEN WITH "Profilauswahl-Handwerker"
   
         @10,5     SAY "FÅr diese Profil-Auswahl wurden "
         SET COLOR TO N/W
         @10,37    SAY "keine"
         SET COLOR TO W/N
         @10,42    SAY " Adressen gefunden."
   
         SAVE SCREEN TO m_map[n_map]
         l_map[n_map] = .T.
   
      ENDIF

      DO WHILE .T.

         n_map = 11

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

         @ 0,0
         @ 0,2 SAY c_error_t
         c_error_t = " "
   
         WAIT
      
         @ 0,0 CLEAR TO 0,79
      
   * Funktionstasten auswerten
   
         FOR i = 2 TO 9
            IF ( n_int_key = i .AND. LEN(TRIM(f_key[i])) = 0)
               c_error_t = "FEHLER: Funktionstaste wird nicht unterstÅtzt"
               LOOP
            ENDIF
         NEXT
      
         IF n_int_key = 2
            EXIT
         ENDIF
   
         IF n_int_key = 10
            LOOP
         ENDIF
   
      ENDDO

      IF n_int_key = 2
         LOOP
      ENDIF

   ENDIF

   IF n_anzahl > 100 .AND. LEN(TRIM(c_aus_anzeigen)) > 0
      n_map = 12

      FOR i = 3 TO 27
         STORE SPACE(30) TO f_keytext[i]
      NEXT
   
* Funktionstasten belegen
      
      f_key[2] = "ZURöCK "
      f_key[3] = " LISTE "
      f_key[4] = "       "
      f_key[5] = "       "
      f_key[6] = "       "
      f_key[7] = "       "
      f_key[8] = "       "
      f_key[9] = "WEITER "
      f_key[10] = " HILFE "
      
      f_keytext[5] = " ZurÅck zum Hauptmenue !"
      f_keytext[8] = " Drucken einer Liste"
      f_keytext[26] = " Weiter mit Anzeigen"
      f_keytext[29] = " Bedeutung der Funktionstasten"
      
* Maske ausgeben
      

      IF .NOT. l_map[n_map]

      CLEAR
      
         DO RAHMEN WITH "Profilauswahl-Handwerker"
   
         @10,15    SAY "Sie haben "
         @10,31    SAY "Adressen ausgewÑhlt."
         @12,10    SAY "Die Anzeige zeigt davon nur die ersten "
         SET COLOR TO N/W
         @12,COL() SAY "100"
         SET COLOR TO W/N
         @12,COL() SAY " Stck."
         @14,5     SAY "Sie wÑhlen besser "
         SET COLOR TO N/W
         @14,COL() SAY "LISTE"
         SET COLOR TO W/N
         @14,COL() SAY " und drucken alle Adressen aus."
   
         SAVE SCREEN TO m_map[n_map]
         l_map[n_map] = .T.
   
      ENDIF

      DO WHILE .T.

         n_map = 12

         FOR i = 3 TO 27
            STORE SPACE(30) TO f_keytext[i]
         NEXT
      
* Funktionstasten belegen
         
         f_key[2] = "ZURöCK "
         f_key[3] = " LISTE "
         f_key[4] = "       "
         f_key[5] = "       "
         f_key[6] = "       "
         f_key[7] = "       "
         f_key[8] = "       "
         f_key[9] = "WEITER "
         f_key[10] = " HILFE "
         
         f_keytext[5] = " ZurÅck zum Hauptmenue !"
         f_keytext[8] = " Drucken einer Liste"
         f_keytext[26] = " Weiter mit Anzeigen"
         f_keytext[29] = " Bedeutung der Funktionstasten"
         
         RESTORE SCREEN FROM m_map[n_map]

         n_int_key = 0
         @ 3,71 SAY TIME()
         SET COLOR TO N/W
         @10,25 SAY STR(n_anzahl,5)
         SET COLOR TO W/N

         @ 0,0
         @ 0,2 SAY c_error_t
         c_error_t = " "
   
         WAIT
      
         @ 0,0 CLEAR TO 0,79

* Funktionstasten auswerten
   
         FOR i = 2 TO 9
            IF ( n_int_key = i .AND. LEN(TRIM(f_key[i])) = 0)
               c_error_t = "FEHLER: Funktionstaste wird nicht unterstÅtzt"
               LOOP
            ENDIF
         NEXT
      
         IF n_int_key = 2
            EXIT
         ENDIF
   
         IF n_int_key = 3
            c_aus_liste = "X"
            c_aus_anzeige = " "
         ENDIF

         IF n_int_key = 9
            EXIT
         ENDIF

         IF n_int_key = 10
            LOOP
         ENDIF
   
      ENDDO

      IF n_int_key = 2
         LOOP
      ENDIF

   ELSE
      n_map = 13

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
      f_key[9] = "WEITER "
      f_key[10] = " HILFE "
      
      f_keytext[5] = " ZurÅck zum Hauptmenue !"
      f_keytext[26] = " Weiter im Programm"
      f_keytext[29] = " Bedeutung der Funktionstasten"
      
* Maske ausgeben
      
      IF .NOT. l_map[n_map]

      CLEAR
      
         DO RAHMEN WITH "Profilauswahl-Handwerker"
   
         @10,15 SAY "Sie haben "
         @10,31 SAY " Adressen ausgewÑhlt."
         @14,10 SAY "Wenn ihnen das zu viel ist, Åberlegen Sie, ob Ihre"
         @15,15 SAY "Profil-Auswahl zu gro· war und wÑhlen Sie neu aus "
   
         SAVE SCREEN TO m_map[n_map]
         l_map[n_map] = .T.
   
      ENDIF

      DO WHILE .T.

         n_map = 13

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
         f_key[9] = "WEITER "
         f_key[10] = " HILFE "
         
         f_keytext[5] = " ZurÅck zum Hauptmenue !"
         f_keytext[26] = " Weiter im Programm"
         f_keytext[29] = " Bedeutung der Funktionstasten"
         
         RESTORE SCREEN FROM m_map[n_map]

         n_int_key = 0
         @ 3,71 SAY TIME()
         SET COLOR TO N/W
         @10,25 SAY STR(n_anzahl,5)
         SET COLOR TO W/N

         @ 0,0
         @ 0,2 SAY c_error_t
         c_error_t = " "
   
         WAIT
      
         @ 0,0 CLEAR TO 0,79
      
* Funktionstasten auswerten
   
         FOR i = 2 TO 9
            IF ( n_int_key = i .AND. LEN(TRIM(f_key[i])) = 0)
               c_error_t = "FEHLER: Funktionstaste wird nicht unterstÅtzt"
               LOOP
            ENDIF
         NEXT
      
         IF n_int_key = 2
            EXIT
         ENDIF
   
         IF n_int_key = 9
            EXIT
         ENDIF
   
         IF n_int_key = 10
            LOOP
         ENDIF
   
      ENDDO

      IF n_int_key = 2
         LOOP
      ENDIF

   ENDIF

   IF LEN(TRIM(c_aus_anzeigen)) > 0
      DO anzeigen
   ENDIF

   IF LEN(TRIM(c_aus_liste)) > 0
      DO liste WITH c_sort_text,n_anzahl
   ENDIF

   IF LEN(TRIM(c_aus_aufkleber)) > 0
      DO aufkleber
   ENDIF

   IF LEN(TRIM(c_aus_brief)) > 0
      c_dateiname = "HW-ADR  "
      @ 17,10 SAY "Geben Sie den Namen der Datei an, in welcher die"
      @ 18,10 SAY "Adressen fÅr die Briefe gespeichert werden sollen."
      @ 19,25 GET c_dateiname PICTURE "!!!!!!!!"
      READ

      DO brief with TRIM(c_dateiname)

      @ 17,10 CLEAR TO 19,60
   ENDIF

   PNAME = "HW0130"

ENDDO

RETURN


PROCEDURE NORM_PLZ
***********************************************************************
* Das Unterprogramm NORM_PLZ erweitert die AbkÅrzungen der PLZ auf    *
* den ganzen Wert  (z.B. 8 ---> 8000 )                                *
***********************************************************************
PARAMETER plz

IF n_plz < 10
   n_plz = n_plz * 1000
ENDIF
IF n_plz < 100
   n_plz = n_plz * 100
ENDIF
IF n_plz < 10
   n_plz = n_plz * 10
ENDIF

RETURN


PROCEDURE ZEIGE_GEWERK
**********************************************************************
* Das Unterprogramm ZEIGE_GEWERK zeigt die Gewerke aus der Datei     *
* AA-CODE an .                                                       *
**********************************************************************
   
n_map = 14

IF .NOT. l_map[n_map]

   SELECT 10
   USE aacode INDEX aasup01 SHARED              // Added SHARED for Harbour

   DECLARE c_gewerk[32],c_gew_te[32]

   FOR i = 1 TO 32
      c_gewerk[i] = "   "
      c_gew_te[i] = "                             "
   NEXT

   SEEK "GEWERK   "

   i = 0
   DO WHILE funktion = "GEWERK"
      i = i + 1
      c_gewerk[i] = SUBSTR(code1,1,3)
      c_gew_te[i] = kurztext
      IF i = 32
         EXIT
      ENDIF
      SKIP
   ENDDO

   USE
   SELECT 1
   
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
   
* EOF : HW0130.PRG
   

   