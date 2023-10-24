*
************************************************************************
*
*  PROGRAMMNAME:                  M C 0 6 0 0
*
* ----------------------------------------------------------------------
*
*  VERWENDETE DATEIEN              --- KEINE ---
*
*-----------------------------------------------------------------------
*
*  AUFGABE:  ADV-MENUE
*
*-----------------------------------------------------------------------
*
*  BESONDERHEITEN:    ES WERDEN LOGISCHE VARIABLE ALS PUBLIC ERKLAERT,
*                     DAMIT DAS FOLGENDE AUSWAHLPROGRAMM FUER AENDERN,
*                     ANZEIGEN, LOESCHEN NUR EINMAL ERSTELLT WERDEN
*                     BRAUCHT UND JE NACH INHALT DIESER VARIABLEN DIE
*                     EIGENTLICHEN VERARBEITUNGSPROGRAMME AUFRUFEN KANN
*
*-----------------------------------------------------------------------
*
*    ABLAUF:    MAIN-LOOP BIS <F1>
*               AUFRUF DER VERARBEITUNGSPROGRAMME JE NACH AUSWAHL
*
*-----------------------------------------------------------------------
*
*   VERWENDETE MASKEN:        M C 0 6 0 0 F 1  (IM PROGRAMM)
*
*-----------------------------------------------------------------------
*
*   RUFT AUF :               
*
*  M C 0 6 1 0                           (ERFASSEN VERMITTLER)
*  M C 0 6 2 0  m_parameter = 1 (MC0621) (ANZEIGEN VERMITTLER)
*  M C 0 6 2 0  m_parameter = 2 (MC0622) (ŽNDERN /L™SCHEN VERMITTLER)
*  M C 0 6 3 0                           (LISTING))
*-----------------------------------------------------------------------
************************************************************************
*

*----------------------------------------------------------------------*
* Initialisierungen
*----------------------------------------------------------------------*
PROCEDURE MAIN()                    // Adaption for Harbour
CLEAR ALL
* Setzen der Arbeitsumgebung
SET DATE GERMAN
SET BELL OFF
SET MENU OFF
SET HEADING OFF
SET STATUS OFF
SET MESSAGE TO 0
SET PRINTER TO HW-ADR.prn              // For Harbour otherwise error on LPT1
SET DEVICE TO SCREEN

SET KEY 28 TO ENDE
SET KEY -1 TO F2_KEY
SET KEY -2 TO F3_KEY
SET KEY -3 TO F4_KEY
SET KEY -4 TO F5_KEY
SET KEY -5 TO F6_KEY
SET KEY -6 TO F7_KEY
SET KEY -7 TO F8_KEY
SET KEY -8 TO F9_KEY
SET KEY -9 TO HELPMASK

RESTORE FROM QZWST   ADDITIVE
RESTORE FROM USER    ADDITIVE

n_zwstnr = VAL(QZWSTNR)
c_zwstname = QZWSTNAME
n_psnr = UPSNR
c_nachname = UNACHNAME
FEHLER = SPACE(40)

*----------------------------------------------------------------------*
* Globale Variablen definieren + initialisieren
*----------------------------------------------------------------------*

DECLARE m_map[25],l_map[25],m_hmap[25],l_hmap[25]
DECLARE f_key[10],f_keytext[30]

PUBLIC m_map,l_map,m_hmap,l_hmap
PUBLIC m_rahmen,l_rahmen,m_help_map,l_help_map
PUBLIC f_key,f_keytext
PUBLIC n_int_key,n_map
PUBLIC LSHOW,LCHANGE,LDELETE,LRETMEN,LINTNR,LNAME
PUBLIC PNAME,c_nachname,n_zwstnr,c_zwstname,n_psnr

STORE .F. TO l_rahmen
STORE .F. TO l_help_map

FOR i = 1 TO 25
  STORE .F. TO l_map[i]
  STORE .F. TO l_hmap[i]
NEXT

FOR i = 1 TO 10
  STORE SPACE(7) TO f_key[i]
NEXT

FOR i = 1 TO 30
  STORE SPACE(30) TO f_keytext[i]
NEXT
f_keytext[2] = "Programm beenden"
f_keytext[29] = "Bedeutung der Funktionstasten"

*--------------------------------------------------------------------------*
* Indexdateien aufbauen, falls notig
*--------------------------------------------------------------------------*

IF .NOT. FILE("AASUP01.NTX")
   USE AACODE
   INDEX ON FUNKTION+CODE1 TO AASUP01
   USE
ENDIF

IF .NOT. FILE("MDSUP01.NTX")
   USE MDVERMI
   INDEX ON STR(ZWST_AKT,3)+STR(VERMITTLER,9) TO MDSUP01
   USE
ENDIF

IF .NOT. FILE("MDSUP02.NTX")
   USE MDVERMI
   INDEX ON SUBSTR(VM_NAME,1,12) TO MDSUP02
   USE
ENDIF

IF .NOT. FILE("MDSUP03.NTX")
   USE MDVERMI
   INDEX ON  STR(ZWST_AKT,3)+STR(VM_PLZ,4)+SUBSTR(VM_FIRMA,1,10)+  ;
             SUBSTR(VM_NAME,1,12)+SUBSTR(VM_VORNAME,1,8) TO MDSUP03
   USE
ENDIF

************************************************************************
*          B E G I N   D E S   H A U P T P R o G R A M M S             *
************************************************************************

CLEAR

DO WHILE .T.

   CLEAR
   PTITEL = "HAUPTMENU VERMITTLER-VERWALTUNG"
   PNAME = "MC0600"
   FUSS = '****************   W„hlen Sie bitte aus !    ******************** '
   RAHM(PTITEL,PNAME,FUSS)
   @05,16 SAY CHR(218)+REPLICATE(CHR(196),42)+CHR(191)
   @06,16 SAY CHR(179)+'                                          '+CHR(179)
   @07,16 SAY CHR(179)+'         VERMITTLER-VERWALTUNG            '+CHR(179)
   @08,16 SAY CHR(179)+'                                          '+CHR(179)
   @09,16 SAY CHR(192)+REPLICATE(CHR(196),42)+CHR(217)
   @12,21 PROMPT 'Neuanlage Vermittler Adressen'
   @13,21 PROMPT 'Anzeigen Vermittler Adressen'
   @14,21 PROMPT 'Žndern/L”schen Vermittler Adressen'
   @15,21 PROMPT 'Listen der Vermittler Adressen'
   @18,21 SAY '<F1> = Bearbeitung beenden '
   @0,02 SAY FEHLER
   FEHLER = SPACE(40)
   MENU TO WAHL

   LRETMEN = .F.
   LINTNR = .F.
   LNAME = .F.
   LSHOW = .F.
   LCHANGE = .F.
   LDELETE = .F.
   DO CASE
      CASE WAHL = 1
         @0,2
         @0,2 SAY "Bitte warten !"
         DO MC0610
      CASE WAHL = 2
         LSHOW = .T.
         @0,2
         @0,2 SAY "Bitte warten !"
         DO MC0620 WITH 1
      CASE WAHL = 3
         LCHANGE = .T.
         @0,2
         @0,2 SAY "Bitte warten !"
         DO MC0620 WITH 2
      CASE WAHL = 4
         LDELETE = .T.
         @0,2
         @0,2 SAY "Bitte warten !"
         DO MC0630
      OTHERWISE
         FEHLER = '**** FALSCHE AUSWAHL ******'
   ENDCASE

ENDDO

CLEAR
RETURN

****************************************************************************
* FUNKTION RAHM MALEN INCL TITEL                                           *
****************************************************************************
FUNCTION RAHM

PARAMETERS PTITEL,PNAME,FUSS

UEBER = QZWSTNAME
KASTEN = CHR(201)+CHR(205)+CHR(187)+CHR(186)+CHR(188)+CHR(205);
         +CHR(200)+CHR(186)
@1,0,24,79 BOX KASTEN
@4,0 SAY CHR(204)+REPLICATE(CHR(205),78)+CHR(185)
@2,2 SAY PNAME
ZWSTNR = QZWSTNR
@2,10 SAY ZWSTNR
@2,INT(40-LEN(TRIM(UEBER))/2) SAY UEBER
@2,71 SAY DTOC(DATE())
UNR = STR(UPSNR)
@3,4 SAY SUBSTR(UNR,7,4)
@3,10 SAY UNACHNAME
@3,INT(40-LEN(TRIM(PTITEL))/2) SAY PTITEL
@3,71 SAY TIME( )
@22,0 SAY CHR(204)+REPLICATE(CHR(205),78)+CHR(185)
@23,INT(40-LEN(TRIM(fuss))/2) SAY FUSS

RETURN (PTITEL)

* EOF : MC0600.PRG