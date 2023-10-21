PROCEDURE HW0110()
****************************************************************************
* PARAMETER öBERNAHME AUS HW0100
PARAMETERS n_para
***************************************************************************
* Program.: HW0120.PRG
* Author..: ULRICH SCHWEIER
***************************************************************************
* Erstellt am : 31. August 1987
* GeÑndert am : 99. XXXXXX 9999    durch :
***************************************************************************
* Notiz...:
***************************************************************************
* Bemerk..:
* Bemerk..:                        HANDWERKER - ADRESSEN
* Bemerk..:
***************************************************************************
* Bemerk..: ANZEIGEN HANDWERKER-ADRESSEN (n_para = 1)
* Bemerk..: éNDERN   HANDWERKER-ADRESSEN (n_para = 2)
***************************************************************************
* Bemerk..: VERWENDETE DBASE DBF(DBT) : AA-CODE .DBF
* Bemerk..:                             HW-ADRES.DBF
* Bemerk..:                             TEMP1   .DBF
* Bemerk..:                             TEMP01  .NTX
***************************************************************************

* Neue Variablen initialisieren -> c_variablen

PNAME = "HW0110"

STORE SPACE(30)  TO c_hcnameausw
STORE SPACE(30)  TO c_hcnameausw1
STORE SPACE(12)  TO c_sortname
STORE SPACE(1)   TO c_char
STORE SPACE(11)  TO c_rest
STORE SPACE(11)  TO c_begin
STORE SPACE(11)  TO c_beginelse
STORE SPACE(79)  TO c_errortext

* Neue Variablen initialisieren -> n_variablen
STORE 0              TO n_locate
STORE 0000000        TO n_hcadrnrausw
STORE 0000           TO n_hcplzvonausw
STORE 0000           TO n_hcolzbisausw
STORE 000            TO n_hcgkav
STORE 000            TO n_hcgkab
STORE 00000          TO n_hckrednrausw
STORE 0000000        TO n_hcadrnrvonbis
STORE 00000000       TO n_isn
STORE 0000           TO n_plz
STORE 00000          TO n_i
STORE 00000          TO n_k
STORE 0000           TO n_index1
STORE 0000           TO n_index2
STORE 0000           TO n_zeile
STORE 0000           TO n_zeilevon
STORE 0000           TO n_zeilebis
STORE 0000           TO n_seite
STORE 0              TO n_exiter
STORE 0              TO n_einmal
STORE 0              TO n_ggk1
STORE 0              TO n_ggk2
STORE 0              TO n_ggk3

* ARRAY - AUFBAU

DECLARE c_aname[10],c_avorname[10],n_aplz[10]
DECLARE c_aort[10],c_agk1[10],c_agk2[10],c_agk3[10],c_aadrnr1[10]
DECLARE c_auswahl[10]
DECLARE n_array[500]

* LôSCHEN DER SPEZIAL-DATEIEN

SELECT 1
   USE HWADRES
SELECT 2
   USE AACODE INDEX AASUP01
SELECT 3
   USE HWADRES INDEX HWSUP01,HWSUP02

*--------------------------------------------------------------------------*
* Programm z.B.  : Menue auf dem Bildschirm ausgeben
*--------------------------------------------------------------------------*


DO WHILE .T.

   IF n_para = 1
      n_map = 4
   ENDIF
   IF n_para = 2
      n_map = 7
   ENDIF
   FOR n_i = 3 TO 27
      STORE SPACE(30) TO f_keytext[n_i] 
   NEXT
   f_key[1] = " ENDE  "
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
   f_keytext[29] = " Bedeutung der Funktionstasten"


   IF .NOT. l_map[n_map]
      CLEAR
      IF n_para = 1
         DO RAHMEN WITH "ANZEIGEN - HANDWERKER"
      ENDIF
      IF n_para = 2
         DO RAHMEN WITH "éNDERN - HANDWERKER"
      ENDIF
      @ 7,3   SAY "ADRESS-NUMMER:"
      @ 9,3   SAY "NAME.........:"
      @ 11,3  SAY "POSTLEITZAHL.:"
      @ 11,24 SAY "BIS "
      @ 13,3  SAY "GEWERK.......:"
      @ 13,24 SAY "BIS "
      @ 15,3  SAY "KREDITOREN-NR:"
      SAVE SCREEN TO m_map[n_map]
      l_map[n_map] = .T.
   ELSE
      RESTORE SCREEN FROM m_map[n_map]
   ENDIF

   DO WHILE .T.

      IF n_para = 1
         n_map = 4
      ENDIF
      IF n_para = 2
         n_map = 7
      ENDIF

      FOR n_i = 3 TO 27
         STORE SPACE(30) TO f_keytext[n_i] 
      NEXT
   
      f_key[1] = " ENDE  "
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
      f_keytext[29] = " Bedeutung der Funktionstasten"
   
      RESTORE SCREEN FROM m_map[n_map]

      @ 7,18  GET n_hcadrnrausw PICTURE "9999999"
      @ 9,18  GET c_hcnameausw PICTURE "AXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      @ 11,18 GET n_hcplzvonausw PICTURE "9999"
      @ 11,29 GET n_hcplzvonausw PICTURE "9999"
      @ 13,18 GET n_hcgkav PICTURE "999"
      @ 13,29 GET n_hcgkab PICTURE "999"
      @ 15,18 GET n_hckrednrausw PICTURE "99999"

      IF c_errortext <> " "
         @ 0,0
         @ 0,2 SAY c_errortext
         c_errortext = " "
      ENDIF

      n_int_key = 0
      @ 3,71 SAY TIME()

      READ

      @ 0,0

      FOR n_i = 2 TO 10
         IF ( n_int_key = n_i .AND. LEN(TRIM(f_key[n_i])) = 0)
            c_errortext = "FEHLER: Funktionstaste wird nicht unterst?tzt"
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

      n_hcadrnrausw1 = UPPER(n_hcadrnrausw)
      n_hcadrnrausw = n_hcadrnrausw1


*                    ---- PlausibilitÑts-PrÅfungen ----

      DO WHILE .T.
         c_errortext = " "
         @ 0,2
         @ 0,2 SAY  "Es erfolgt die PlausibilitÑtsprÅfung"
         n_index1 = 0

         IF n_hcadrnrausw > 0
            n_hcadrnrvonbis = n_hcadrnrausw
            n_index1 = 1
         ENDIF

         IF c_hcnameausw <> " "
            n_sortname = n_hcanameausw
            DO SORTNAME
            n_index1 = 2
         ENDIF

         IF n_hcplzvonausw > 0 .OR. n_hcplzbisausw > 0
            IF n_hcplzvonausw > 0 .AND. n_hcplzbisausw = 0
               c_errortext = "Bitte PLZ-BIS angeben !"
               KEBOARD(REPLICATE(CHR(13),3))
               EXIT
            ENDIF
            IF n_hcplzvonausw = 0 .AND. n_hcplzbisausw > 0
               c_errortext = "Bitte PLZ-VON angeben !"
               KEBOARD(REPLICATE(CHR(13),2))
               EXIT
            ENDIF
            IF n_hcplzvonausw > 0
               n_plz = n_hcplzvonausw
               DO PLZNORM
               n_hcplzvonausw = n_plz
            ENDIF
            IF n_hcplzbisausw > 0
               n_plz = n_hcplzbisausw
               DO PLZNORM
               n_hcplzbisausw = n_plz
            ENDIF
            IF n_hcplzvonausw <> 0 .AND. n_hcplzbisausw <> 0
               IF n_hcplzvonausw > n_hcplzbisausw
                  c_errortext = "PLZ-Intervall falsch !"
                  KEBOARD(REPLICATE(CHR(13),2))
                  EXIT
               ENDIF
            ENDIF
            n_index1 = 3
         ENDIF

         IF n_hcgkav > 0 .OR. n_hcgkab > 0
            IF n_hcgkav = 0
               c_errortext = "Bitte Gewerkvon eingeben !"
               KEBOARD(REPLICATE(CHR(13),4))
               EXIT
            ENDIF
            IF n_hcgkab = 0
               c_errortext = "Bitte Gewerkbis eingeben !"
               KEBOARD(REPLICATE(CHR(13),5))
               EXIT
            ENDIF
            IF n_hcgkav > n_hcgkab
               c_errortext = "Intervall falsch !"
               KEBOARD(REPLICATE(CHR(13),4))
               EXIT
            ENDIF
            n_index1 = 4
         ENDIF

         IF n_hckrednrausw <> 0
            n_index1 = 5
         ENDIF
         
         IF n_index1 = 0
            c_errortext = "Bitte mindestens eine Auswahl treffen !"
            EXIT
         ENDIF

         EXIT
      ENDDO

      IF c_errortext = " "
         @ 0,0
         @ 0,2 SAY "System sucht !"
         SELECT 3
         GO TOP
         n_isn = 0
         DO WHILE .T.

            IF EOF()
               EXIT
            ENDIF

            IF n_hcadrnrausw <> 0
               IF ADRNR <> n_hcadrnrausw
                  SKIP
                  LOOP
               ENDIF
            ENDIF
            IF c_hcnameausw <> " "
               IF SORTNAME <> c_sortname
                  SKIP
                  LOOP
               ENDIF
            ENDIF
            IF n_hcplzvonausw <> 0
               IF PLZ < n_hcplzvonausw .OR. PLZ > n_hcplzbisausw
                  SKIP
                  LOOP
               ENDIF
            ENDIF
            IF n_hcgkav <> 0
               n_k = 0
               IF GEWERK1 < n_kvgkav .OR. GEWERK1 > n_hcgwab
                  n_k = n_k + 1
               ENDIF
               IF GEWERK2 < n_kvgkav .OR. GEWERK2 > n_hcgwab
                  n_k = n_k + 1
               ENDIF
               IF GEWERK3 < n_kvgkav .OR. GEWERK3 > n_hcgwab
                  n_k = n_k + 1
               ENDIF
               IF n_k = 3
                  SKIP
                  LOOP
               ENDIF
            ENDIF
            if n_hckrednrausw <> 0
               IF KREDNR <> n_hckrednrausw
                  SKIP
                  LOOP
               ENDIF
            ENDIF

            n_isn = n_isn + 1

            IF n_isn <= 500
               n_array[n_isn] = RECNO()
            ENDIF

            IF n_num = 1
               @ 0,2 SAY "SYSTEM sucht, und findet     Satz !"
            ENDIF
            IF n_num = 2
               @ 0,2 SAY "SYSTEM sucht, und findet     SÑtze !"
            ENDIF
            IF n_num <= 500
               @ 0,27 SAY n_isn PICTURE "999"
            ENDIF
            IF n_num = 501
               @ 0,0
               @ 0,2 SAY "SYSTEM findet          SÑtze. Nur 500 SÑtze anzeigbar !"
            ENDIF
            IF n_num > 500
               @ 0,16 SAY n_isn PICTURE "99999999"
            ENDIF
            SKIP
            IF EOF()
               EXIT
            ENDIF
         ENDDO
         IF n_isn = 0
            c_errortext = "Keine SÑtze fÅr diese Auswahl gefunden!"
            DO RESET
            EXIT
         ENDIF

         IF n_isn = 1 .AND. n_para = 1
            DO EINZELANZEIGE WITH n_array[1]
         ENDIF
         IF n_isn = 1 .AND. n_para = 2
            DO AENDERUNG WITH n_array[1]
         ENDIF

         IF n_isn > 1

            n_seite = 1
            n_zeile = 1

            IF n_para = 1
               n_map = 5
            ENDIF
            IF n_para = 2
               n_map = 8
            ENDIF

            FOR n_i = 3 TO 27
               STORE SPACE(30) TO f_keytext[n_i] 
            NEXT

            f_key[2] = "ZURöCK "
            f_key[3] = "       "
            f_key[4] = "       "
            f_key[5] = " Pg Dn "
            f_key[6] = " Pg Up "
            f_key[7] = " TITEL "
            f_key[8] = "ANREDE "
            f_key[9] = "GEWERKE"
            f_key[10] = " HELP  "
            f_keytext[2] = " Beendet das Programm !"
            f_keytext[5] = " ZurÅck zum Hauptmenue !"
            f_keytext[14] = " VorwÑrtsblÑttern  "
            f_keytext[17] = " RÅckwÑrtsblÑttern "
            f_keytext[20] = " Zeigt zulÑssige Titel!"
            f_keytext[23] = " Zeigt zulÑssige Anreden!"
            f_keytext[26] = " ZulÑssige Gewerke anzeigen!"
            f_keytext[29] = " Bedeutung der Funktionstasten"
         
            IF .NOT. l_map[n_map]
               CLEAR
               IF n_para = 1
                  DO RAHMEN WITH "   Auswahl-Einzeladresse / Anzeigen "
               ENDIF
               IF n_para = 2
                  DO RAHMEN WITH "   Auswahl-Einzeladresse / éndern "
               ENDIF
               @ 5,60 SAY "SEITE:"
               @ 7,4  SAY "NACHNAME"
               @ 7,24 SAY "VORNAME"
               @ 7,33 SAY "PLZ"
               @ 7,38 SAY "ORT"
               @ 7,58 SAY "GEWERK 1-3"
               @ 7,70 SAY "ADRNR"
               SAVE SCREEN TO m_map[n_map]
               l_map[n_map] = .T.
            ELSE
               RESTORE SCREEN FROM m_map[n_map]
               @ 5,67 SAY n_seite
            ENDIF
            DO WHILE .T.
               IF n_para = 1
                  n_map = 5
               ENDIF
               IF n_para = 2
                  n_map = 8
               ENDIF
               SELECT 2
               GO TOP
               n_zeilevon = n_zeile
               n_zeilebis = n_zeilevon + 9
               DO RA2
               FOR n_i = 3 TO 27
                  STORE SPACE(30) TO f_keytext[n_i] 
               NEXT
               f_key[2] = "ZURöCK "
               f_key[3] = "       "
               f_key[4] = "       "
               f_key[5] = " Pg Dn "
               f_key[6] = " Pg Up "
               f_key[7] = " TITEL "
               f_key[8] = "ANREDE "
               f_key[9] = "GEWERKE"
               f_key[10] = " HELP  "
               f_keytext[2] = " Beendet das Programm !"
               f_keytext[5] = " ZurÅck zum Hauptmenue !"
               f_keytext[14] = " VorwÑrtsblÑttern  "
               f_keytext[17] = " RÅckwÑrtsblÑttern "
               f_keytext[20] = " Zeigt zulÑssige Titel!"
               f_keytext[23] = " Zeigt zulÑssige Anreden!"
               f_keytext[26] = " ZulÑssige Gewerke anzeigen!"
               f_keytext[29] = " Bedeutung der Funktionstasten"
               RESTORE SCREEN FROM m_map[n_map]

               @ 5,67 SAY n_seite

               FOR n_index1 = n_zeilevon TO n_zeilebis
                  n_index2 = n_index1 - n_zeilevon + 1
                  IF n_index <= m_isn
                     SELECT 3
                     n_goto = n_array[n_index1]
                     GO n_goto
                     c_aname[n_index2] = SUBSTR(NAME,1,18)
                     c_avorname[n_index2] = SUBSTR(VORNAME,1,7)
                     n_aplz[n_index2]  = PLZ
                     c_aort[n_index2]  = SUBSTR(ORT,1,20)
                     n_agk1[n_index2]  = GEWERK1
                     n_agk2[n_index2]  = GEWERK2
                     n_agk3[n_index2]  = GEWERK3
                     n_aadrnr[n_index2] = ADRNR
                  ELSE
                     c_aname[n_index2] = "------------------"
                     c_avorname[n_index2] = "-------"
                     n_aplz[n_index2]  = 0
                     c_aort[n_index2]  = "--------------------"
                     n_agk1[n_index2]  = 0
                     n_agk2[n_index2]  = 0
                     n_agk3[n_index2]  = 0
                     n_aadrnr[n_index2] = 0
                  ENDIF
               NEXT

               FOR n_k = 1 TO 10
                  n_z = 9 + n_k
                  @ n_z,4  SAY c_aname[n_k]
                  @ n_z,24 SAY c_avorname[n_k]
                  @ n_z,33 SAY n_aplz[n_k]
                  @ n_z,38 SAY c_aort[n_k]
                  @ n_z,58 SAY n_agk1[n_k]
                  @ n_z,62 SAY n_agk2[n_k]
                  @ n_z,66 SAY n_agk3[n_k]
                  @ n_z,70 SAY n_aadrnr[n_k]
               NEXT
            IF c_errortext <> " "
               @ 0,0
               @ 0,2 SAY c_errortext
               c_errortext = " "
            ENDIF
            FOR n_k = 1 TO 10
               n_z = 9 + nk
               @n_z,2 GET c_auswahl[n_k]
            NEXT
            n_int_key = 0
            @ 3,71 SAY TIME()
            READ
            FOR n_k = 1 TO 10
               IF c_auswahl[n_k] <> " "
                  n_index1 = n_k +n_zeilevon - 1
                  IF n_index1 > n_isn
                     @ 0,2 SAY "Bitte keine leere Reihe ankreuzen !"
                     INKEY(3)
                     LOOP
                  ENDIF
               ENDIF
            NEXT       
            FOR n_i = 2 TO 10
               IF ( n_int_key = n_i .AND. LEN(TRIM(f_key[n_i])) = 0)
                  c_errortext = "FEHLER: Funktionstaste wird nicht unterst?tzt"
                  LOOP
               ENDIF
            NEXT
            IF n_int_key = 2
               RETURN
               EXIT
            ENDIF
            IF n_int_key = 5 .OR. LASTKEY() = 3
               n_zeile = n_zeile + 10
               n_seite = n_seite + 1
               IF n_zeile > n_isn
                  n_zeile = n_isn
                  n_seite = n_seite - 1
               ENDIF
               LOOP
            ENDIF
            IF n_int_key = 6 .OR. LASTKEY() = 18
               n_zeile = n_zeile - 10
               n_seite = n_seite - 1
               IF n_zeile < 0
                  n_zeile = 1
                  n_seite = 1
               ENDIF
               LOOP
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

            IF n_para = 1
               DO EINZELANZEIGE WITH n_array[n_index1]
            ENDIF
            IF n_para = 2
               DO AENDERUNG WITH n_array[n_index1]
            ENDIF
   
            DO RESET

            IF n_exiter = 1
               n_exiter = 0
               EXIT
            ENDIF
         ENDDO
         DO RESET
      ENDIF
   ENDIF
ENDDO
ENDDO


PROCEDURE SORTNAME
************************************************************************
* Das Unterprogramm SORTNAME bereitet den Namen zum sortieren auf      * 
************************************************************************
   n_i = 0
   DO WHILE AT(" ",(TRIM(c_sortname))) <> 0 .OR. n_i = 0
      FOR n_i = 1 TO 20
         c_char  = SUBSTR(c_sortname,n_i,1)
         c_rest  = SUBSTR(c_sortname,n_i+1)
         c_begin = SUBSTR(c_sortname,1,n_i)
         c_beginelse = SUBSTR(c_sortname,1,n_i-1)
         IF c_char$" "
            IF c_char$"é"
               c_sorname = c_beginelse+"AE"+c_rest
            ENDIF
            IF c_char$"ô"
               c_sorname = c_beginelse+"OE"+c_rest
            ENDIF
            IF c_char$"ö"
               c_sorname = c_beginelse+"UE"+c_rest
            ENDIF
            IF c_char$" "
               c_sorname = c_beginelse+c_rest
               n_i = n_i - 1
            ENDIF
         ELSE
            c_sorname = c_begin+c_rest
         ENDIF
      NEXT
   ENDDO

************************* ENDE SORTNAME ********************************


PROCEDURE PLZNORM
************************************************************************
* Das Unterprogramm PLZNORM bereitet die PLZ auf.                      * 
************************************************************************
n_errortext = " "
   DO CASE
      CASE n_plz < 10
           n_plz = n_plz * 1000
      CASE n_plz < 100
           n_plz = n_plz * 100
      CASE n_plz < 1000
           n_plz = n_plz * 10
   ENDCASE
************************* ENDE PLZNORM ********************************



PROCEDURE RA2
************************************************************************
* Das Unterprogramm RA setzt die ARRAYS auf 0 bzw " " zurÅck.          * 
************************************************************************
FOR n_i = 1 TO 10
   STORE  0  TO n_agk1[n_i],n_agk2[n_i],n_agk3[n_i],n_aadrnr[n_i]
   STORE " " TO c_aort[n_i],c_auswahl[n_i]
NEXT
************************* ENDE PLZNORM ********************************



PROCEDURE RESET
************************************************************************
STORE SPACE(30)  TO c_hcnameausw
STORE SPACE(30)  TO c_hcnameausw1
STORE SPACE(12)  TO c_sortname
STORE SPACE(1)   TO c_char
STORE SPACE(11)  TO c_rest
STORE SPACE(11)  TO c_begin
STORE SPACE(11)  TO c_beginelse

* Neue Variablen initialisieren -> n_variablen
STORE 0              TO n_locate
STORE 0000000        TO n_hcadrnrausw
STORE 0000           TO n_hcplzvonausw
STORE 0000           TO n_hcolzbisausw
STORE 000            TO n_hcgkav
STORE 000            TO n_hcgkab
STORE 00000          TO n_hckrednrausw
STORE 0000000        TO n_hcadrnrvonbis
STORE 0000           TO n_plz
STORE 00000          TO n_i
STORE 00000          TO n_k
STORE 00000          TO n_k1
STORE 0000           TO n_index1
STORE 0000           TO n_index2
STORE 0000           TO n_zeilevon
STORE 0000           TO n_zeilebis
STORE 0              TO n_exiter
STORE 0              TO n_einmal
************************* ENDE RESET ********************************
 


PROCEDURE EINZELANZEIGE
************************************************************************
* Das Unterprogramm EINZELANZEIGE zeigt einen Satz an.                      * 
************************************************************************
PARAMETER n_locate
IF n_para = 1
   n_map = 6
ENDIF
FOR n_i = 1 TO 27
   STORE SPACE(30) TO f_keytext[n_i] 
NEXT
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
f_keytext[5] = " ZurÅck zur Auswahl"
f_keytext[20] = " Zeigt zulÑssige Titel!"
f_keytext[23] = " Zeigt zulÑssige Anreden!"
f_keytext[26] = " ZulÑssige Gewerke anzeigen!"
f_keytext[29] = " Bedeutung der Funktionstasten"

IF .NOT. l_map[n_map]
   CLEAR
   IF n_para = 1
      DO RAHMEN WITH "EINZELANZEIGE"
   ENDIF
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

   SELECT 1

   GO n_locate

   n_map = 6

   FOR n_i = 3 TO 27
      STORE SPACE(30) TO f_keytext[n_i] 
   NEXT
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
   f_keytext[5] = " ZurÅck zur Auswahl"
   f_keytext[20] = " Zeigt zulÑssige Titel!"
   f_keytext[23] = " Zeigt zulÑssige Anreden!"
   f_keytext[26] = " ZulÑssige Gewerke anzeigen!"
   f_keytext[29] = " Bedeutung der Funktionstasten"
   RESTORE SCREEN FROM m_map[n_map]
   IF c_errortext <> " "
      @ 0,0
      @ 0,2 SAY c_errortext
      c_errortext = " "
   ENDIF
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

   WAIT " "

   FOR n_i = 2 TO 10
      IF ( n_int_key = n_i .AND. LEN(TRIM(f_key[n_i])) = 0)
         c_errortext = "FEHLER: Funktionstaste wird nicht unterst?tzt"
         LOOP
      ENDIF
   NEXT

   IF n_int_key = 2
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
      DO ZEIGGEWERK
      LOOP
   ENDIF
   IF n_int_key = 10
      LOOP
   ENDIF

ENDDO
DO RESET
RETURN

************************* ENDE EINZELANZEIGE ********************************


PROCEDURE AENDERUNG
************************************************************************
* Das Unterprogramm AENDERUNG lÑsst die énderung eines Satzes zu.                      * 
************************************************************************
   PARAMETER n_locate
   n_map = 9
   FOR n_i = 1 TO 27
      STORE SPACE(30) TO f_keytext[n_i] 
   NEXT
   f_key[2] = "ZURöCK "
   f_key[3] = " CLS   "
   f_key[4] = "       "
   f_key[5] = "       "
   f_key[6] = "       "
   f_key[7] = " TITEL "
   f_key[8] = "ANREDE "
   f_key[9] = "GEWERKE"
   f_key[10] = " HELP  "
   f_keytext[2] = " Beendet das Programm !"
   f_keytext[5] = " ZurÅck zur Auswahl"
   f_keytext[8] = " énderung rÅckgÑngig machen "
   f_keytext[20] = " Zeigt zulÑssige Titel"
   f_keytext[23] = " Zeigt zulÑssige Anreden"
   f_keytext[26] = " ZulÑssige Gewerke anzeigen "
   f_keytext[29] = " Bedeutung der Funktionstasten"
   
   IF .NOT. l_map[n_map]
      CLEAR
      DO RAHMEN WITH "éNDERN EINZELADRESSE"
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
   
   SELECT 1
   GO n_locate

   n_adrnr = ADRNR
   c_status = STATUS
   n_krednr = KREDNR
   n_gk1     = GEWERK1
   n_gk2     = GEWERK2
   n_gk3     = GEWERK3
   c_anrede  = ANREDE
   c_titel   = TITEL
   c_firma   = FIRMA
   c_name    = NAME
   c_vorname = VORNAME
   c_strasse = STRASSE
   n_plz     = PLZ
   c_ort     = ORT
   c_telvw   = TELVW
   c_teldw   = TELDW
   c_info    = INFO

   DO WHILE .T.
      n_map = 9
      FOR n_i = 3 TO 27
         STORE SPACE(30) TO f_keytext[n_i] 
      NEXT
      f_key[2] = "ZURöCK "
      f_key[3] = " CLS   "
      f_key[4] = "       "
      f_key[5] = "       "
      f_key[6] = "       "
      f_key[7] = " TITEL "
      f_key[8] = "ANREDE "
      f_key[9] = "GEWERKE"
      f_key[10] = " HELP  "
      f_keytext[2]  = " Beendet das Programm !"
      f_keytext[5]  = " ZurÅck zur Auswahl"
      f_keytext[8]  = " énderung rÅckgÑngig machen "
      f_keytext[20] = " Zeigt zulÑssige Titel"
      f_keytext[23] = " Zeigt zulÑssige Anreden"
      f_keytext[26] = " ZulÑssige Gewerke anzeigen "
      f_keytext[29] = " Bedeutung der Funktionstasten"
      RESTORE SCREEN FROM m_map[n_map]
      IF c_errortext <> " "
         @ 0,0
         @ 0,2 SAY c_errortext
         c_errortext = " "
      ENDIF

      @ 5,18  SAY n_adrnr
      @ 5,48  GET c_status  PICTURE "!"
      @ 6,18  GET n_krednr  PICTURE "99999"
      @ 6,47  GET n_gk1     PICTURE "999"
      @ 6,57  GET n_gk2     PICTURE "999"
      @ 6,67  GET n_gk3     PICTURE "999"
      @ 8,18  GET c_anrede  PICTURE "!"
      @ 9,18  GET c_titel   PICTURE "!"
      @ 10,18 GET c_firma   PICTURE "!XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      @ 11,18 GET c_name    PICTURE "!XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      @ 12,18 GET c_vorname PICTURE "!XXXXXXXXXXXXXX"
      @ 13,18 GET c_strasse PICTURE "!XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      @ 14,18 GET n_plz     PICTURE "99999"
      @ 15,18 GET c_ort     PICTURE "!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      @ 16,18 GET c_telvw   PICTURE "99999"
      @ 16,32 GET c_teldw   PICTURE "999999999"
      @ 18,18 GET c_info
      n_int_key = 0
      @ 3,71 SAY TIME()
   
      READ
   
      FOR n_i = 2 TO 10
         IF ( n_int_key = n_i .AND. LEN(TRIM(f_key[n_i])) = 0)
            c_errortext = "FEHLER: Funktionstaste wird nicht unterst?tzt"
            LOOP
         ENDIF
      NEXT
   
      IF n_int_key = 2
         EXIT
      ENDIF
      IF n_int_key = 3
         SELECT 1
         GO n_locate
         n_adrnr = ADRNR
         c_status = STATUS
         n_krednr = KREDNR
         n_gk1     = GEWERK1
         n_gk2     = GEWERK2
         n_gk3     = GEWERK3
         c_anrede  = ANREDE
         c_titel   = TITEL
         c_firma   = FIRMA
         c_name    = NAME
         c_vorname = VORNAME
         c_strasse = STRASSE
         n_plz     = PLZ
         c_ort     = ORT
         c_telvw   = TELVW
         c_teldw   = TELDW
         c_info    = INFO
         LOOP
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
         DO ZEIGGEWERK
         LOOP
      ENDIF
      IF n_int_key = 10
         LOOP
      ENDIF

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
   
*                  ---- PlausibilitÑts-PrÅfungen ----

      @ 0,2
      @ 0,2 SAY "Es erfolgt die PlausibilitÑtsprÅfung"

      DO STATUSPRUEFUNG
      IF c_errortext <> " "
         LOOP
      ENDIF

      n_mode = n_locate
      DO KREDNRPRUEFUNG WITH n_mode
      IF c_errortext <> " "
         LOOP
      ENDIF

      DO GEWERKPRUEFUNG
      IF c_errortext <> " "
         LOOP
      ENDIF

      DO NAMENSPRUEFUNG
      IF c_errortext <> " "
         LOOP
      ENDIF

      DO ADRESSPRUEFUNG
      IF c_errortext <> " "
         LOOP
      ENDIF

      DO TELEFONPRUEFUNG
      IF c_errortext <> " "
         LOOP
      ENDIF

*            ---- Aufbaus SORTNAME ----

      c_sortname = c_name
      n_i = 0
      DO WHILE AT(" ",(TRIM(c_sortname))) <> 0 .OR. n_i = 0
         FOR n_i = 1 TO 20
            c_char  = SUBSTR(c_sortname,n_i,1)
            c_rest  = SUBSTR(c_sortname,n_i+1)
            c_begin = SUBSTR(c_sortname,1,n_i)
            c_beginelse = SUBSTR(c_sortname,1,n_i-1)
            IF c_char$" "
               IF c_char$"é"
                  c_sorname = c_beginelse+"AE"+c_rest
               ENDIF
               IF c_char$"ô"
                  c_sorname = c_beginelse+"OE"+c_rest
               ENDIF
               IF c_char$"ö"
                  c_sorname = c_beginelse+"UE"+c_rest
               ENDIF
               IF c_char$" "
                  c_sorname = c_beginelse+c_rest
                  n_i = n_i - 1
               ENDIF
            ELSE
               c_sorname = c_begin+c_rest
            ENDIF
         NEXT
      ENDDO

*            ---- PLAUSIBILITéTSPRUEFUNG (2) ----

      SELECT 1
      GO TOP
      LOCATE FOR TRIM(SORTNAME) = TRIM(c_sortname)
      IF FOUND()
         DO WHILE .T.
            IF EOF()
               EXIT
            ENDIF
            IF n_locate = RECNO()
               EXIT
            ENDIF
            IF TRIM(SORTNAME) = TRIM(c_sortname)
               IF PLZ = n_plz .AND. UPPER(STRASSE) = UPPER(c_strasse)
                  c_errortext = "Adresse vorhanden,"+;
                                " mit Gewerknummern:"+;
                                 STR(GEWERK1)+" "+;
                                 STR(GEWERK2)+" "+;
                                 STR(GEWERK3)
                  KEYBOARD(REPLICATE(CHR(13),8))
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
         LOOP
      ENDIF

      DO BESTAETIGEN

      IF n_exiter = 1
         n_exiter = 0
         LOOP
      ENDIF

      DO ABSPEICHERN

      EXIT

   ENDDO

   DO RESET
   
   RETURN
************************* ENDE EINZELANZEIGE ***************************
   

PROCEDURE ABSPEICHERN
************************************************************************
* Das Unterprogramm ABSPEICHERN speichert den geÑnderten Satz ab.      *
************************************************************************
SELECT 3

LOCATE FOR ADRNR = n_ardnr

IF ADRNR = n_adrnr

   CLEAR

   @ 10,30 SAY "Satznummer"
   @ 10,41 SAY n_adrnr
   @ 12,30 SAY "abgespeichert !"

   REPLACE ADRNR             WITH n_adrnr
   REPLACE BERTADRNR         WITH 0
   REPLACE KREDNR            WITH n_krednr
   REPLACE GEWERK1           WITH n_gk1
   REPLACE GEWERK2           WITH n_gk2
   REPLACE GEWERK3           WITH n_gk3
   REPLACE STATUS            WITH c_status
   REPLACE AENDDAT           WITH DATE()
   REPLACE ANREDE            WITH c_anrede
   REPLACE TITEL             WITH c_titel
   REPLACE FIRMA             WITH c_firma
   REPLACE NAME              WITH c_name
   REPLACE SORTNAME          WITH c_sortname
   REPLACE VORNAME           WITH c_vorname
   REPLACE STRASSE           WITH c_strasse
   REPLACE PLZ               WITH n_plz
   REPLACE ORT               WITH c_ort
   REPLACE TELVW             WITH c_telvw
   REPLACE TELDW             WITH c_teldw
   REPLACE INFO              WITH c_info
   IF c_status = " "
      REPLACE KZUEBER        WITH "U"
   ENDIF
   IF c_status = "L"
      REPLACE KZUEBER        WITH "D"
   ENDIF

ENDIF

RETURN

************************* ENDE ABSPEICHERN *****************************



PROCEDURE ZEIGEANREDE
************************************************************************
* Die Prozedur ZEIGEANREDE  zeigt die Anreden aus AA-CODE an.          * 
************************************************************************

n_map = 17

IF .NOT. l_map[n_map]

   SELECT 2

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


PROCEDURE ZEIGETITEL
************************************************************************
* Die Prozedur ZEIGETITEL zeigt die Anreden aus AA-CODE an.            * 
************************************************************************

n_map = 18

IF .NOT. l_map[n_map]

   SELECT 2

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

* EOF : HW0120.PRG