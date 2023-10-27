* PARAMETER öBERNAHME AUS MC0600
PARAMETERS n_para1
***************************************************************************
* Program.: MC0620.PRG
* Author..: ULRICH SCHWEIER
***************************************************************************
* Erstellt am : 08. OkTOber 1987
* GeÑndert am : 99. XXXXXX 9999    durch :
***************************************************************************
* Notiz...:
***************************************************************************
* Bemerk..:
* Bemerk..: n_para1 = 1 Anzeigen der Vermittlerdaten
* Bemerk..: n_para1 = 2 éndern/Lîschen der Vermittlerdaten
* Bemerk..:
***************************************************************************
* Bemerk..: VERWENDETE CLIPPER.DBF : MD-VERMI
* Bemerk..:                          AA-CODE 
* Bemerk..:                   .MEM : QZWST
* Bemerk..:                          USER
****************************************************************************
*----------------------------------------------------------------------*
* Initialisierungen
*----------------------------------------------------------------------*

* Alle offenen Dateien schlie·en
CLOSE DATABASES

* Variablen definieren

* ARRAY Erstellung
DECLARE n_isn[100],c_azwst[10],c_averminr[10],c_astatus[10],c_aname[10]
DECLARE c_avorname[10],c_afirma[10],c_aplz[10],c_aort[10]
DECLARE c_auswahl[10]

FOR n_k = 1 TO 100
   STORE 0 TO n_isn[n_k] 
NEXT
DO RA2

STORE "MC0620" TO PNAME

* Definieren von n_variablen
STORE 0 TO n_z
STORE 0 TO n_i
STORE 0 TO n_k
STORE 0 TO n_map
STORE 0 TO n_zwstakt
STORE 0 TO n_vermittler
STORE 0 TO n_vmplzv
STORE 0 TO n_vmplzb
STORE 0 TO n_plz
STORE 0 TO n_index1
STORE 0 TO n_index2
STORE 0 TO n_k1
STORE 0 TO n_zeile
STORE 0 TO n_seite

* Definieren von c_variablen
STORE SPACE(79) TO c_errortext
STORE SPACE(1)  TO c_status
STORE SPACE(20) TO c_vmname
STORE SPACE(14) TO c_vmvorname
STORE SPACE(18) TO c_vmfirma
STORE SPACE(1)  TO c_vmanrede
STORE SPACE(1)  TO c_vmtitel
STORE SPACE(26) TO c_vmstrasse
STORE SPACE(21) TO c_vmort
STORE SPACE(5)  TO c_teldvw
STORE SPACE(9)  TO c_telddw
STORE SPACE(5)  TO c_telpvw
STORE SPACE(9)  TO c_telpdw
STORE SPACE(2)  TO c_vmtyp
STORE "        " TO d_anlagedat

FOR n_k = 1 TO 10
   STORE SPACE(1) TO c_auswahl[n_k] 
NEXT

* Index Dateien aufbauen
DO AUFBAU

* LôSCHEN DER SPEZIAL-DATEIEN

IF FILE("TEMP00.NTX")
   DO LOESCH_NTX
ENDIF
IF FILE("TEMP1.DBF")
   DO LOESCH_DBF
ENDIF

* Arbeitsbereich wÑhlen
*--------------------------------------------------------------------------*
* Programm z.B.  : Menue auf dem Bildschirm ausgeben
*--------------------------------------------------------------------------*

IF n_para1 = 1
   n_map = 6
ENDIF
IF n_para1 = 2
   n_map = 9
ENDIF
FOR n_i = 3 TO 27
   STORE SPACE(30) TO f_keytext[n_i] 
NEXT

* Funktionstasten wÑhlen

f_key[1] = " ENDE  "
f_key[2] = "ZURöCK "
f_key[3] = "       "
f_key[4] = "       "
f_key[5] = "       "
f_key[6] = "       "
f_key[7] = "       "
f_key[8] = "       "
f_key[9] = "       "
f_key[10] = " HELP  "
f_keytext[2]  = " Beendet das Programm !"
f_keytext[5]  = "FÅhrt nach MC0600 zurÅck     "
f_keytext[29] = "Bedeutung der Funktionstasten"

IF n_para1 = 1
   n_map = 6
ENDIF
IF n_para1 = 2
   n_map = 9
ENDIF

@ 1,0

IF .NOT. l_map[n_map]
   CLEAR
   IF n_para1 = 1
      DO RAHMEN WITH "Anzeigen Vermittler / AUSWAHL"
   ENDIF
   IF n_para1 = 2
      DO RAHMEN WITH "éndern Vermittler / AUSWAHL"
   ENDIF
   @ 8,4  SAY "Suche nach der aktuelle Zweigstelle.."
   @10,4  SAY "           oder der Vermittlernummer.."
   @12,4  SAY "           oder dem Vermittlername...."
   @14,4  SAY "           oder nach Postleitzahlvon..       bis.."
   SAVE SCREEN TO m_map[n_map]
   l_map[n_map] = .T.
ELSE
   RESTORE SCREEN FROM m_map[n_map]
ENDIF
DO WHILE .T.
   IF n_para1 = 1
      n_map = 6
   ENDIF
   IF n_para1 = 2
      n_map = 9
   ENDIF

   FOR n_i = 3 TO 27
      STORE SPACE(30) TO f_keytext[n_i] 
   NEXT

* Funktionstasten wÑhlen
   f_key[1] = " ENDE  "
   f_key[2] = "ZURöCK "
   f_key[3] = "       "
   f_key[4] = "       "
   f_key[5] = "       "
   f_key[6] = "       "
   f_key[7] = "       "
   f_key[8] = "       "
   f_key[9] = "       "
   f_key[10] = " HELP  "
   f_keytext[2]  = " Beendet das Programm !"
   f_keytext[5]  = "FÅhrt nach MC0600 zurÅck     "
   f_keytext[29] = "Bedeutung der Funktionstasten"

   RESTORE SCREEN FROM m_map[n_map]

   @ 8,43  GET n_zwstakt PICTURE "999"
   @10,43  GET n_vermittler PICTURE "999999999"
   @12,43  GET c_vmname PICTURE "!XXXXXXXXXXXXXXXXXX"
   @14,43  GET n_vmplzv PICTURE "9999"
   @14,54  GET n_vmplzb PICTURE "9999"

   IF c_errortext <> " "
      @ 0,2 SAY c_errortext
   ENDIF

   c_errortext = " "
   n_int_key = 0
   @ 3,71 SAY TIME()
   READ
   @ 0,0 CLEAR TO 0,70

   FOR n_k = 2 TO 9
      IF ( n_int_key = n_k .AND. LEN(TRIM(f_key[n_k])) = 0)
         c_errortext = "FEHLER: Funktionstaste wird nicht unterstÅtzt"
         n_markerror = 1
         LOOP
      ENDIF
   NEXT

   IF n_int_key = 2
      RETURN
   ENDIF
   
   IF n_int_key = 10
      LOOP
   ENDIF

   *                    ---- PlausibilitÑts-PrÅfungen ----

   @ 0,5 SAY  "Es erfolgt die PlausibilitÑtsprÅfung. "
   DO WHILE .T.
      c_errortext = " "

      IF n_zwstakt = 0 .AND. n_vermittler = 0 .AND. ;
         c_vmname = " " .AND. n_vmplzv = 0 .AND. n_vmplzb = 0
         c_errortext = "Bitte eine Auswhl treffen ! "
         EXIT
      ENDIF
      IF n_zwstakt <> 0
         DO ZWSTPRUEF
         IF c_errortext <> " "
            EXIT
         ENDIF
      ENDIF
      IF n_vermittler <> 0
         DO VERMNRPRUEF
         IF c_errortext <> " "
            EXIT
         ENDIF
      ENDIF
      IF c_vmname <> " "
         DO VERMNAPRUEF
         IF c_errortext <> " "
            EXIT
         ENDIF
      ENDIF

      IF n_vmplzv <> 0 .OR. n_vmplzb > 0
         IF n_vmplzv > 0 .AND. n_vmplzb = 0
            c_errortext = "Bitte PLZ-BIS angeben !"
            KEYBOARD(REPLICATE(CHR(13),4))
            EXIT
         ENDIF
         IF n_vmplzv = 0 .AND. n_vmplzb > 0
            c_errortext = "Bitte PLZ-VON angeben !"
            KEYBOARD(REPLICATE(CHR(13),3))
            EXIT
         ENDIF
         IF n_vmplzv > 0
            n_vmplzv = PRUEF_PLZ(n_vmplzv)
         ENDIF
         IF n_vmplzb > 0
            n_vmplzb = PRUEF_PLZ(n_vmplzb)
         ENDIF
         IF n_vmplzv > n_vmplzb
            c_errortext = "PLZ-Intervall falsch !"
            KEYBOARD(REPLICATE(CHR(13),3))
            EXIT
         ENDIF
      ENDIF
      EXIT
   ENDDO

   IF c_errortext = " "
      @ 0,0
      @ 0,1 SAY "System sucht, und findet     Satz.  "
      n_i = 0
      USE MDVERMI
      GO TOP
      DO WHILE .T.
         IF EOF()
            EXIT
         ENDIF
         IF n_zwstakt <> 0
            IF ZWST_AKT <> n_zwstakt
               SKIP
               LOOP
            ENDIF
            IF VM_NAME = " "
               SKIP
               LOOP
            ENDIF
         ENDIF
         IF n_vermittler <> 0
            IF VERMITTLER <> n_vermittler
               SKIP
               LOOP
            ENDIF
         ENDIF
         IF c_vmname <> " "
            IF VM_NAME <> c_vmname
               SKIP
               LOOP
            ENDIF
         ENDIF
         IF n_vmplzv <> 0
            IF VM_PLZ < n_vmplzv .OR. VM_PLZ > n_vmplzb
               SKIP
               LOOP
            ENDIF
         ENDIF
         n_i = n_i + 1
         n_isn[n_i] = RECNO()
         IF n_i = 2
            @ 0,2 SAY "System sucht, und findet     SÑtze."
         ENDIF
         @ 0,27 SAY n_i PICTURE "999"
         IF n_i >= 100
            @ 0,0
            @ 0,1 SAY "Es werden maximal nur 100 SÑtze angezeigt !"
            EXIT
         ENDIF
         SKIP
         IF EOF()
            EXIT
         ENDIF
      ENDDO
      USE

      IF n_i > 0
         USE MDVERMI
         IF .NOT. FILE("TEMP1.DBF")
            COPY STRUCTURE TO TEMP1
         ENDIF
         USE

         FOR n_k = 1 TO 100
            n_recno = n_isn[n_k]
            IF n_recno > 0 .AND. n_k <= n_i
               USE MDVERMI
               GOTO n_recno
               n_zwstakt    = ZWST_AKT
               n_vermittler = VERMITTLER
               n_plz        = VM_PLZ
               c_status     = STATUS
               c_vmname     = VM_NAME
               c_vmvorname  = VM_VORNAME
               c_vmfirma    = VM_FIRMA
               c_vmanrede   = VM_ANREDE
               c_vmtitel    = VM_TITEL
               c_vmstrasse  = VM_STRASSE
               c_vmort      = VM_ORT
               c_teldvw     = TELFVW
               c_telddw     = TELFDW
               c_telpvw     = TELPVW
               c_telpdw     = TELPDW
               c_vmtyp       = VM_TYP
               USE
               DO NEUANLAGE
            ELSE
               n_k = 100
            ENDIF
         NEXT
         USE TEMP1
         INDEX ON STR(ZWST_AKT,3)+VM_NAME+STR(VM_PLZ,4)+;
                  STR(VERMITTLER,9) TO TEMP00.NTX
         USE

         IF n_i = 1
            IF n_para1 = 1
               DO MC0621 WITH n_index1
            ENDIF
            IF n_para1 = 2
               DO MC0622 WITH n_index1
            ENDIF
            DO RESET
            LOOP
         ENDIF

         n_seite = 1
         n_zeile = 1

         IF n_para1 = 1
            n_map = 7
         ENDIF
         IF n_para1 = 2
            n_map = 10
         ENDIF

         FOR n_i = 3 TO 27
            STORE SPACE(30) TO f_keytext[n_i] 
         NEXT
         f_key[1] = " ENDE  "
         f_key[2] = "ZURöCK "
         f_key[3] = "       "
         f_key[4] = "       "
         f_key[5] = " Pg Dn "
         f_key[6] = " Pg Up "
         f_key[7] = "       "
         f_key[8] = "       "
         f_key[9] = "       "
         f_key[10] = " HELP  "
         f_keytext[2] = "Programm beenden"
         f_keytext[5] = " FÅhrt zur Auswahl zurÅck "
         f_keytext[14] = " VorwÑrts blÑttern  "
         f_keytext[17] = " RÅckwÑrts blÑttern "
         f_keytext[29] = " Bedeutung der Funktionstasten"
         CLEAR
         
         IF .NOT. l_map[n_map]
            IF n_para1 = 1
               DO RAHMEN WITH "Anzeigen Vermittler / Wahl Anzeige"
            ENDIF
            IF n_para1 = 2
               DO RAHMEN WITH "éndern/Lîschen Vermittler / Wahl Anzeige"
            ENDIF
            @ 5,60 SAY "SEITE:"
            @ 6,3  SAY "Z"
            @ 7,3  SAY "W"
            @ 8,3  SAY "S"
            @ 9,3  SAY "T"
            @ 7,7  SAY "VERMIT-NR"
            @ 6,17 SAY "S"
            @ 7,17 SAY "T"
            @ 8,17 SAY "A"
            @ 9,17 SAY "T"
            @ 7,19 SAY "NAME"
            @ 7,32 SAY "VORNAME"
            @ 7,41 SAY "FIRMA"
            @ 7,52 SAY "PLZ"
            @ 7,57 SAY "ORT"
            SAVE SCREEN TO m_map[n_map]
            l_map[n_map] = .T.
         ENDIF

         DO WHILE .T.
            FOR n_k = 1 TO 10
               STORE SPACE(1) TO c_auswahl[n_k]
            NEXT
            
            IF n_para1 = 1
               n_map = 7
            ENDIF
            IF n_para1 = 2
               n_map = 10
            ENDIF

            USE TEMP1 INDEX TEMP00
            GO TOP

            n_zeilevon = n_zeile
            n_zeilebis = n_zeile + 9

            DO RA2

            FOR n_i = 3 TO 27
               STORE SPACE(30) TO f_keytext[n_i] 
            NEXT
            f_key[1] = " ENDE  "
            f_key[2] = "ZURöCK "
            f_key[3] = "       "
            f_key[4] = "       "
            f_key[5] = " Pg Dn "
            f_key[6] = " Pg Up "
            f_key[7] = "       "
            f_key[8] = "       "
            f_key[9] = "       "
            f_key[10] = " HELP  "
            f_keytext[2] = "Programm beenden"
            f_keytext[5] = " FÅhrt zur Auswahl zurÅck "
            f_keytext[14] = " VorwÑrts blÑttern  "
            f_keytext[17] = " RÅckwÑrts blÑttern "
            f_keytext[29] = " Bedeutung der Funktionstasten"
            RESTORE SCREEN FROM m_map[n_map]
            @ 5,67 SAY n_seite

            FOR n_index1 = n_zeilevon TO n_zeilebis
               IF n_index1 <= 100
                  n_index2 = n_index1 - n_zeilevon + 1

                  IF n_index1 <= n_i
                     GO TOP
                     SKIP n_index1 - 1
                     c_azwst[n_index2]     = STR(ZWST_AKT,3)
                     c_averminr[n_index2]  = STR(VERMITTLER,9)
                     c_astatus[n_index2]   = STATUS
                     c_aname[n_index2]     = SUBSTR(VM_NAME,1,12)
                     c_avorname[n_index2]  = SUBSTR(VM_VORNAME,1,8)
                     c_afirma[n_index2]    = SUBSTR(VM_FIRMA,1,10)
                     c_aplz[n_index2]      = STR(VM_PLZ,4)
                     c_aort[n_index2]      = SUBSTR(VM_ORT,1,20)
                  ELSE
                     c_azwst[n_index2]     = "---"
                     c_avermitnr[n_index2] = "---------"
                     c_status[n_index2] = "-"
                     c_aname[n_index2] = "------------"
                     c_avorname[n_index2] = "--------"
                     c_aplz[n_index2]  = "----"
                     c_aort[n_index2]  = "--------------------"
                  ENDIF
               ENDIF
            NEXT

            FOR n_k = 1 TO 10
               n_z = 10 + n_k
               @ n_z,3  SAY c_azwst[n_k]
               @ n_z,7  SAY c_averminr[n_k]
               @ n_z,17 SAY c_astatus[n_k]
               @ n_z,19 SAY c_aname[n_k]
               @ n_z,32 SAY c_avorname[n_k]
               @ n_z,41 SAY c_afirma[n_k]
               @ n_z,52 SAY c_aplz[n_k]
               @ n_z,57 SAY c_aort[n_k]
            NEXT

            IF c_errortext <> " "
               @ 0,2 SAY c_errortext
               c_errortext = " "
            ENDIF
            FOR n_k = 1 TO 10
               n_z = 10 + n_k
               @n_z,2 GET c_auswahl[n_k] PICTURE "X"
            NEXT
            n_int_key = 0
            @ 3,71 SAY TIME()
            READ

            FOR n_k = 1 TO 9
               IF ( n_int_key = n_k .AND. LEN(TRIM(f_key[n_k])) = 0)
                  c_errortext = "FEHLER: Funktionstaste wird nicht unterstÅtzt"
                  LOOP
               ENDIF
            NEXT

            IF n_int_key = 2
               EXIT
            ENDIF
            IF n_int_key = 5 .OR. LASTKEY() = 3
               n_zeile = n_zeile + 10
               n_seite = n_seite + 1
               IF n_zeile > n_i
                  n_zeile = n_i
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
            IF n_int_key = 10
               LOOP
            ENDIF

            n_k1 = 0
            FOR n_k = 1 TO 10
               IF c_auswahl[n_k] <> " "
                  IF c_aname[n_k] = "------------"
                     c_errotext = "Bitte richtig markieren!"
                     LOOP
                  ENDIF
                  n_index1 = n_k + n_zeilevon -1
                  n_k1 = n_k1 + 1
               ENDIF
            NEXT       
            IF n_k1 = 0
               c_errotext = "Bitte einen Satz markieren!"
               LOOP
            ENDIF
            IF n_k1 > 1
               c_errotext = "Bitte nur einen Satz markieren!"
               LOOP
            ENDIF
         
            IF n_k1 = 1
               USE
               IF n_para1 = 1
                  DO MC0621 WITH n_index1
               ENDIF
               IF n_para1 = 2
                  DO MC0622 WITH n_index1
               ENDIF
            ENDIF
            USE
         ENDDO
      ELSE
         c_errortext = "FÅr diese Auswahl wurden keine SÑtze gefunden!"
      ENDIF
      DO RESET
   ENDIF
ENDDO


PROCEDURE RESET
************************************************************************
* Das Unterprogramm RESET setzt all Variablen zurÅck                   * 
************************************************************************
* Definieren von n_variablen
STORE 0 TO n_z
STORE 0 TO n_i
STORE 0 TO n_k
STORE 0 TO n_map
STORE 0 TO n_zwstakt
STORE 0 TO n_vermittler
STORE 0 TO n_vmplzv
STORE 0 TO n_vmplzb
STORE 0 TO n_plz
STORE 0 TO n_index1
STORE 0 TO n_index2
STORE 0 TO n_k1
STORE 0 TO n_zeile
STORE 0 TO n_seite

* Definieren von n_variablen
STORE SPACE(79) TO c_errotext
STORE SPACE(1)  TO c_status
STORE SPACE(20) TO c_vmname
STORE SPACE(14) TO c_vmvorname
STORE SPACE(18) TO c_vmfirma
STORE SPACE(1)  TO c_vmanrede
STORE SPACE(1)  TO c_vmtitel
STORE SPACE(26) TO c_vmstrasse
STORE SPACE(21) TO c_vmort
STORE SPACE(5)  TO c_teldvw
STORE SPACE(9)  TO c_telddw
STORE SPACE(5)  TO c_telpvw
STORE SPACE(9)  TO c_telpdw
STORE SPACE(2)  TO c_vmtyp
STORE "        " TO d_anlagedat

IF FILE("TEMP0.NTX")
   USE TEMP1 INDEX TEMP00.NTX
      ZAP
      PACK
   USE
ENDIF
IF FILE("TEMP1.DBF")
   USE TEMP1
      ZAP
      PACK
   USE
ENDIF

************************* ENDE RESET ***********************************



PROCEDURE RA2
************************************************************************
* Das Unterprogramm RA setzt die ARRAYS auf 0 bzw " " zurÅck.          * 
************************************************************************
FOR n_k = 1 TO 10
   STORE SPACE(3)  TO c_azwst[n_k]
   STORE SPACE(9)  TO c_averminr[n_k]
   STORE SPACE(1)  TO c_astatus[n_k]
   STORE SPACE(12) TO c_aname[n_k]
   STORE SPACE(8)  TO c_avorname[n_k]
   STORE SPACE(10) TO c_afirma[n_k]
   STORE SPACE(4)  TO c_aplz[n_k]
   STORE SPACE(20) TO c_aort[n_k]
NEXT
************************* ENDE RA2 ********************************

PROCEDURE LOESCH_NTX
************************************************************************
* Das Unterprogramm LOESCH_NTX lîscht die Datei TEMP00.NTX 
************************************************************************
   ERASE TEMP00.NTX
************************* ENDE LOESCH_NTX ******************************

PROCEDURE LOESCH_DBF
************************************************************************
* Das Unterprogramm LOESCH_DBF lîscht die Datei TEMP1.DBF 
************************************************************************
   ERASE TEMP1.DBF
************************* ENDE LOESCH_DBF ******************************


PROCEDURE NEUANLAGE
************************************************************************
* Das Unterprogramm NEUANLAGE legt ausgewÑhlte SÑtze in TEMP1 an. 
************************************************************************
   USE TEMP1
   APPEND BLANK
   REPLACE ZWST_AKT   WITH n_zwstakt
   REPLACE VERMITTLER WITH n_vermittler
   REPLACE VM_PLZ     WITH n_plz
   REPLACE STATUS     WITH c_status
   REPLACE VM_NAME    WITH c_vmname
   REPLACE VM_VORNAME WITH c_vmvorname
   REPLACE VM_FIRMA   WITH c_vmfirma
   REPLACE VM_ANREDE  WITH c_vmanrede
   REPLACE VM_TITEL   WITH c_vmtitel
   REPLACE VM_STRASSE WITH c_vmstrasse
   REPLACE VM_ORT     WITH c_vmort
   REPLACE TELFVW     WITH c_teldvw
   REPLACE TELFDW     WITH c_telddw
   REPLACE TELPVW     WITH c_telpvw
   REPLACE TELPDW     WITH c_telpdw
   REPLACE VM_TYP     WITH c_vmtyp
   USE
************************* ENDE NEUANLAGE *******************************

   

PROCEDURE VERMNAPRUEF
************************************************************************
* Das Unterprogramm VERMNAPRUEF prÅft den Vermittlername.              * 
************************************************************************
c_errortext = " "
USE MDVERMI
   GO TOP
   LOCATE FOR VM_NAME = c_vmname
   IF .NOT. FOUND()
      c_errortext = "Vermittlername nicht vorhanden"
      KEYBOARD(REPLICATE(CHR(13),2))
   ENDIF
USE
************************* ENDE VERMNAPRUEF *******************************

PROCEDURE VERMNRPRUEF
************************************************************************
* Das Unterprogramm VERMNRPRUEF prÅft den Vermittlername.              * 
************************************************************************
c_errortext = " "
USE MDVERMI
   GO TOP
   LOCATE FOR VERMITTLER = n_vermittler
   IF .NOT. FOUND()
      c_errortext = "Vermittlernummer nicht vorhanden"
      KEYBOARD(REPLICATE(CHR(13),1))
   ENDIF
USE
************************* ENDE VERMNAPRUEF *******************************


PROCEDURE ZWSTPRUEF
************************************************************************
* Das Unterprogramm ZWSTPRUEF prÅft auf zulÑssige ZWST.                * 
************************************************************************
@ 0,0
************************* ENDE ZWSTPRUEF *********************************


PROCEDURE AUFBAU
************************************************************************
* Das Unterprogramm AUFBAU baut die Indexdateien auf.                  * 
************************************************************************
IF .NOT. FILE("MDSUP01.NTX")
   USE MDVERMI
   INDEX ON STR(ZWST_AKT,3)+STR(VERMITTLER,9) TO MDSUP01.NTX
   USE
ENDIF
IF .NOT. FILE("MDSUP02.NTX")
   USE MDVERMI
   INDEX ON SUBSTR(VM_NAME,12) TO MDSUP02.NTX
   USE
ENDIF
IF .NOT. FILE("MDSUP02.NTX")
   USE MDVERMI
   INDEX ON STR(ZWST_AKT,3)+STR(VM_PLZ,4)+SUBSTR(VM_FIRMA,10)+;
            SUBSTR(VM_NAME,12)+SUBSTR(VM_VORNAME,8) TO MDSUP03.NTX
   USE
ENDIF
************************* ENDE AUFBAU **********************************

* EOF: MC0620.PRG