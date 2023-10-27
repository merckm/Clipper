* PARAMETER öBERNAHME AUS MC0620

PARAMETERS n_para3
USE TEMP1 INDEX TEMP00
GO TOP
SKIP n_para3 -1

***************************************************************************
* Program.: MC0622.PRG
* Author..: ULRICH SCHWEIER
***************************************************************************
* Erstellt am : 10. Oktober 1987
* GeÑndert am : 99. XXXXXX 9999    durch :
***************************************************************************
* Notiz...: Dieses Programm wird von MC0620 aus aufgerufen
***************************************************************************
* Bemerk..:
* Bemerk..:                   Vermittler - éndern
* Bemerk..:
***************************************************************************


*----------------------------------------------------------------------*
* Initialisierungen
*----------------------------------------------------------------------*

STORE "MC0622" TO PNAME

* Definieren von n_variablen
STORE 0 TO n_map
STORE 0 TO n_zwst_akt
STORE 0 TO n_vermittler
STORE 0 TO n_vmplzv
STORE 0 TO n_vmplzb
STORE 0 TO n_plz

* Definieren von c_variablen
STORE SPACE(79) TO c_errortext
STORE SPACE(1)  TO c_status
STORE SPACE(20) TO c_name
STORE SPACE(14) TO c_vorname
STORE SPACE(18) TO c_firma
STORE SPACE(1)  TO c_anrede
STORE SPACE(1)  TO c_titel
STORE SPACE(26) TO c_strasse
STORE SPACE(21) TO c_ort
STORE SPACE(5)  TO c_telfvw
STORE SPACE(9)  TO c_telfdw
STORE SPACE(5)  TO c_telpvw
STORE SPACE(9)  TO c_telpdw
STORE SPACE(2)  TO c_typ
STORE "        " TO d_anlagedat

* Arbeitsbereich wÑhlen

*--------------------------------------------------------------------------*
* Programm z.B.  : Menue auf dem Bildschirm ausgeben
*--------------------------------------------------------------------------*

n_zwst_akt    = ZWST_AKT
n_vermittler = VERMITTLER
c_status     = STATUS
d_anlagedat  = ANLAGEDAT
c_name     = VM_NAME
c_vorname  = VM_VORNAME
c_firma    = VM_FIRMA
c_anrede   = VM_ANREDE
c_titel    = VM_TITEL
c_strasse  = VM_STRASSE
n_plz        = VM_PLZ
c_ort      = VM_ORT
c_telfvw     = TELFVW
c_telfdw     = TELFDW
c_telpvw     = TELPVW
c_telpdw     = TELPDW
c_typ       = VM_TYP
USE

n_map = 11
FOR n_i = 3 TO 30
   STORE SPACE(30) TO f_keytext[n_i] 
NEXT
f_key[1] = " ENDE  "
f_key[2] = "ZURöCK "
f_key[3] = "       "
f_key[4] = "       "
f_key[5] = "       "
f_key[6] = "       "
f_key[7] = "ANREDEN"
f_key[8] = " TITEL "
f_key[9] = "       "
f_key[10] = " HELP  "
f_keytext[5] = " ZurÅck zum Hauptmenue !"
f_keytext[20] = " AbkÅrzungen aller zulÑssigen Anreden"
f_keytext[23] = " AbkÅrzungen aller zulÑssigen Titel"
f_keytext[29] = "Bedeutung der Funktionstasten"

IF .NOT. l_map[n_map] = .T.
   CLEAR
   DO RAHMEN WITH "éndern/Lîschen Vermittler"
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
ELSE
   RESTORE SCREEN FROM m_map[n_map]
ENDIF
DO WHILE .T.
   n_map = 11
   FOR n_i = 3 TO 30
      STORE SPACE(30) TO f_keytext[n_i] 
   NEXT
   f_key[1] = " ENDE  "
   f_key[2] = "ZURöCK "
   f_key[3] = "       "
   f_key[4] = "       "
   f_key[5] = "       "
   f_key[6] = "       "
   f_key[7] = "ANREDEN"
   f_key[8] = " TITEL "
   f_key[9] = "       "
   f_key[10] = " HELP  "
   f_keytext[5] = " ZurÅck zum Hauptmenue !"
   f_keytext[20] = " AbkÅrzungen aller zulÑssigen Anreden"
   f_keytext[23] = " AbkÅrzungen aller zulÑssigen Titel"
   f_keytext[29] = "Bedeutung der Funktionstasten"

   RESTORE SCREEN FROM m_map[n_map]
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

   IF c_errortext <> " "
      @ 0,5 SAY c_errortext
      c_errortext = " "
   ENDIF

   n_int_key= 0
   @ 3,71 SAY TIME()
   
   READ

   FOR n_k = 2 TO 9
      IF ( n_int_key = n_k .AND. LEN(TRIM(f_key[n_k])) = 0)
         c_errortext = "FEHLER: Funktionstaste wird nicht unterstÅtzt"
         LOOP
      ENDIF
   NEXT

   IF n_int_key = 2
      RETURN
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

   @ 0,0
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
      DO SPEICERBOTH
      RETURN
   ELSE
      LOOP
   ENDIF

ENDDO

PROCEDURE SPEICERBOTH
************************************************************************
* Die Prozedur SPEICERBOTH speichert die Daten in der Datei MD-VERMI.DBF 
* Die Prozedur SPEICERBOTH speichert die Daten in der Datei TEMP1.DBF  * 
************************************************************************

@ 0,02
@ 0,02 SAY "Abspeichern der Daten"

USE MDVERMI INDEX MDSUP01,MDSUP02,MDSUP03    // Adaption for Harbour
SEEK STR(n_zwst_akt,3)+STR(n_vermittler,9)
REPLACE zwst_akt   WITH n_zwst_akt
REPLACE vermittler WITH n_vermittler
REPLACE status     WITH c_status
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
IF c_status # "Ll"
   REPLACE kz_ueber   WITH "D"
ELSE
   REPLACE kz_ueber   WITH "U"
ENDIF
USE

USE TEMP1 INDEX TEMP00              // Adaption for Harbour
SEEK STR(n_zwst_akt,3)+c_name+STR(n_plz,4)+STR(n_vermittler,9)
REPLACE zwst_akt   WITH n_zwst_akt
REPLACE vermittler WITH n_vermittler
REPLACE status     WITH c_status
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
IF c_status # "Ll"
   REPLACE kz_ueber   WITH "D"
ELSE
   REPLACE kz_ueber   WITH "U"
ENDIF
USE

@ 0,02 CLEAR TO 0,79

RETURN

****************************** SPEICHERBOTH ****************************

* EOF : MC0622.PRG