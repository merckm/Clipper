* PARAMETER öBERNAHME AUS MC0620

PARAMETERS n_para2
USE TEMP1 INDEX TEMP00
GO TOP
SKIP n_para2 -1

***************************************************************************
* Program.: MC0621.PRG
* Author..: ULRICH SCHWEIER
***************************************************************************
* Erstellt am : 10. Oktober 1987
* GeÑndert am : 99. XXXXXX 9999    durch :
***************************************************************************
* Notiz...: Dieses Programm wird von MC0620 aus aufgerufen
***************************************************************************
* Bemerk..:
* Bemerk..:                   Vermittler - Anzeigen
* Bemerk..:
***************************************************************************


*----------------------------------------------------------------------*
* Initialisierungen
*----------------------------------------------------------------------*

STORE "MC0621" TO PNAME

* Definieren von n_variablen
STORE 0 TO n_map
STORE 0 TO n_zwstakt
STORE 0 TO n_vermittler
STORE 0 TO n_vmplzv
STORE 0 TO n_vmplzb
STORE 0 TO n_plz

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

* Arbeitsbereich wÑhlen

*--------------------------------------------------------------------------*
* Programm z.B.  : Menue auf dem Bildschirm ausgeben
*--------------------------------------------------------------------------*

n_zwstakt    = ZWST_AKT
n_vermittler = VERMITTLER
c_status     = STATUS
d_anlagedat  = ANLAGEDAT
c_vmname     = VM_NAME
c_vmvorname  = VM_VORNAME
c_vmfirma    = VM_FIRMA
c_vmanrede   = VM_ANREDE
c_vmtitel    = VM_TITEL
c_vmstrasse  = VM_STRASSE
n_plz        = VM_PLZ
c_vmort      = VM_ORT
c_teldvw     = TELFVW
c_telddw     = TELFDW
c_telpvw     = TELPVW
c_telpdw     = TELPDW
c_vmtyp      = VM_TYP
USE

n_map = 8
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
   DO RAHMEN WITH "Einzelanzeige Vermittler"
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
   n_map = 8
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
   @ 7,28 SAY n_zwstakt
   @ 7,49 SAY c_status
   @ 7,66 SAY c_vmtyp
   @11,14 SAY c_vmanrede
   @11,25 SAY c_vmtitel
   @12,25 SAY c_vmname
   @13,25 SAY c_vmvorname
   @14,25 SAY c_vmfirma
   @16,25 SAY c_vmstrasse
   @17,11 SAY n_plz
   @17,25 SAY c_vmort
   @18,25 SAY c_telpvw
   @18,31 SAY c_telpdw
   @19,25 SAY c_teldvw
   @19,31 SAY c_telddw
   IF c_errortext < " "
      @ 0,5 SAY c_errortext
      c_errortext = " "
   ENDIF

   n_int_key= 0
   @ 3,71 SAY TIME()
   WAIT " "
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


ENDDO


* EOF : MC0621.PRG