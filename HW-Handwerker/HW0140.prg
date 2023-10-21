PROCEDURE RAHMEN
**********************************************************************
*                                                                    *
* Das Unterprogramm RAHMEN erstellt den Rahmen einer Maske           *
*                                                                    *
**********************************************************************
PARAMETERS c_titel

SET CONSOLE OFF
SET STATUS OFF
IF .NOT. l_rahmen
    CLEAR
    @ 1, 0 SAY CHR(201)
    @ 1, 1 SAY REPLICATE(CHR(205),78)
    @ 1,79 SAY CHR(187)
    @ 2, 0 SAY CHR(186)
    @ 2,79 SAY CHR(186)
    @ 3, 0 SAY CHR(186)
    @ 3,79 SAY CHR(186)
    @ 4, 0 SAY CHR(204)
    @ 4, 1 SAY REPLICATE(CHR(205),78)
    @ 4,79 SAY CHR(185)
    I = 5
    DO WHILE I<22
        @ I, 0 SAY CHR(186)
        @ I,79 SAY CHR(186)
        I = I + 1
    ENDDO
    @22,0 SAY CHR(204)
    @22, 1 SAY REPLICATE(CHR(205),6)+CHR(209)
    @22, 8 SAY REPLICATE(REPLICATE(CHR(205),7)+CHR(209),8)
    @22,72 SAY REPLICATE(CHR(205),7)
    @22,79 SAY CHR(185)
    @23,0 SAY CHR(186)
    I = 7
    DO WHILE I<73
        @23, I SAY CHR(179)
        I = I + 8
    ENDDO
    @23,79 SAY CHR(186)
    @24, 0 SAY CHR(200)
    @24, 1 SAY REPLICATE(CHR(205),6)+CHR(207)
    @24, 8 SAY REPLICATE(REPLICATE(CHR(205),7)+CHR(207),8)
    @24,72 SAY REPLICATE(CHR(205),7)
    @24,79 SAY CHR(188)

    @ 2,10 SAY STR(n_zwstnr,3) PICTURE "999"
    @ 2,INT(40-LEN(TRIM(c_zwstname))/2) SAY c_zwstname
    @ 2,71 SAY DTOC(DATE())
    @ 3,4 SAY SUBSTR(STR(n_psnr),7,4)
    @ 3,10 SAY c_nachname
    @ 3,71 SAY TIME( )
    
    SET COLOR TO N/W
    @22, 3 SAY "F1"
    I = 2
    DO WHILE I<10
        @22,(I-1)*8+3 SAY "F"+STR(I,1)
        I = I + 1
    ENDDO
    @22,74 SAY "F10"
    SET COLOR TO W/N
    @23, 2 SAY "ENDE"
    l_rahmen = .T.
    SAVE SCREEN TO m_rahmen
ELSE
    RESTORE SCREEN FROM m_rahmen
ENDIF

@ 2, 2 SAY PNAME
@ 3,INT(40-LEN(TRIM(c_titel))/2) SAY c_titel
I = 2
DO WHILE I<=10
    @23,(I-1)*8 SAY SUBSTR(f_key[I],1,7)
    I = I + 1
ENDDO

RETURN

PROCEDURE HELPMASK
**********************************************************************
*                                                                    *
* Das Unterprogramm HELPMASK zeigt an jeder Stelle im Programm immer *
* die Langtexte der Funktionstastenbedeutung an. Die Variable n_map  *
* gibt an, welche Help-Texte angezuzeigen sind                       *
*                                                                    *
**********************************************************************
PARAMETERS p_name,p_line,p_var
PUBLIC l_first_h

IF .NOT. l_hmap[n_map]
    IF .NOT. l_help_map
        l_first_h = .F.
        CLEAR
        @ 2, 0 SAY CHR(201)
        @ 2, 1 SAY REPLICATE(CHR(205),5)
        @ 2, 6 SAY CHR(209)
        @ 2, 7 SAY REPLICATE(CHR(205),32)
        @ 2,39 SAY CHR(203)
        @ 2,40 SAY REPLICATE(CHR(205),5)
        @ 2,45 SAY CHR(209)
        @ 2,46 SAY REPLICATE(CHR(205),32)
        @ 2,78 SAY CHR(187)
        @ 3, 0 SAY CHR(186)
        @ 3, 6 SAY CHR(179)
        @ 3,39 SAY CHR(186)
        @ 3,45 SAY CHR(179)
        @ 3,78 SAY CHR(186)
        @ 4, 0 SAY CHR(186)
        @ 4, 6 SAY CHR(179)
        @ 4,39 SAY CHR(186)
        @ 4,45 SAY CHR(179)
        @ 4,78 SAY CHR(186)
        @ 5, 0 SAY CHR(186)
        @ 5, 6 SAY CHR(179)
        @ 5,39 SAY CHR(186)
        @ 5,45 SAY CHR(179)
        @ 5,78 SAY CHR(186)
        @ 4, 3 SAY "F1"
        @ 4,42 SAY "F2"
*
        num = 3
        FOR i=6 TO 18 STEP 4
            @ i, 0 SAY CHR(204)
            @ i, 1 SAY REPLICATE(CHR(205),5)
            @ i, 6 SAY CHR(216)
            @ i, 7 SAY REPLICATE(CHR(205),32)
            @ i,39 SAY CHR(206)
            @ i,40 SAY REPLICATE(CHR(205),5)
            @ i,45 SAY CHR(216)
            @ i,46 SAY REPLICATE(CHR(205),32)
            @ i,78 SAY CHR(185)
            @ i+1, 0 SAY CHR(186)
            @ i+1, 6 SAY CHR(179)
            @ i+1,39 SAY CHR(186)
            @ i+1,45 SAY CHR(179)
            @ i+1,78 SAY CHR(186)
            @ i+2, 0 SAY CHR(186)
            @ i+2, 6 SAY CHR(179)
            @ i+2,39 SAY CHR(186)
            @ i+2,45 SAY CHR(179)
            @ i+2,78 SAY CHR(186)
            @ i+3, 0 SAY CHR(186)
            @ i+3, 6 SAY CHR(179)
            @ i+3,39 SAY CHR(186)
            @ i+3,45 SAY CHR(179)
            @ i+3,78 SAY CHR(186)
            @ i+2, 3 SAY "F"+STR(num,1)
            @ i+2,42 SAY "F"+STR(num+1,1)
            num = num + 2
        NEXT
        @ 20,41 SAY "F10"
        @ i, 0 SAY CHR(209)
        @ i, 1 SAY REPLICATE(CHR(205),5)
        @ i, 6 SAY CHR(207)
        @ i, 7 SAY REPLICATE(CHR(205),32)
        @ i,39 SAY CHR(202)
        @ i,40 SAY REPLICATE(CHR(205),5)
        @ i,45 SAY CHR(207)
        @ i,46 SAY REPLICATE(CHR(205),32)
        @ i,78 SAY CHR(188)
        SAVE SCREEN TO m_help_map
        l_help_map = .T.
    ELSE
        RESTORE SCREEN FROM m_help_map
    ENDIF
    i = 3
    p = 0
    DO WHILE i < 20
        @ i  ,8 SAY SUBSTR(f_keytext[p+1],1,30)
        @ i+1,8 SAY SUBSTR(f_keytext[p+2],1,30)
        @ i+2,8 SAY SUBSTR(f_keytext[p+3],1,30)
        i = i + 4
        p = p + 6
    ENDDO
    i = 3
    p = 3
    DO WHILE i < 20
        @ i  ,47 SAY SUBSTR(f_keytext[p+1],1,30)
        @ i+1,47 SAY SUBSTR(f_keytext[p+2],1,30)
        @ i+2,47 SAY SUBSTR(f_keytext[p+3],1,30)
        i = i + 4
        p = p + 6
    ENDDO
    SAVE SCREEN TO m_hmap[n_map]
    l_hmap[n_map] = .T.
    INKEY(0)
ELSE
    RESTORE SCREEN FROM m_hmap[n_map]
    INKEY(0)
ENDIF

RESTORE SCREEN FROM m_map[n_map]

IF l_first_h
    KEYBOARD(CHR(27))
ELSE
    l_first_h = .T.
ENDIF

n_int_key = 10

RETURN


PROCEDURE HELPMASK1
**********************************************************************
*                                                                    *
* Das Unterprogramm HELPMASK zeigt an jeder Stelle im Programm immer *
* die Langtexte der Funktionstastenbedeutung an. Die Variable n_map  *
* gibt an, welche Help-Texte angezuzeigen sind                       *
*                                                                    *
**********************************************************************
PARAMETERS p_name,p_line,p_var
IF .NOT. l_hmap[n_map]
    IF .NOT. l_help_map
        CLEAR
        @ 2, 0 SAY CHR(201)
        @ 2, 1 SAY REPLICATE(CHR(205),5)
        @ 2, 6 SAY CHR(209)
        @ 2, 7 SAY REPLICATE(CHR(205),32)
        @ 2,39 SAY CHR(203)
        @ 2,40 SAY REPLICATE(CHR(205),5)
        @ 2,45 SAY CHR(209)
        @ 2,46 SAY REPLICATE(CHR(205),32)
        @ 2,78 SAY CHR(187)
        @ 3, 0 SAY CHR(186)
        @ 3, 6 SAY CHR(179)
        @ 3,39 SAY CHR(186)
        @ 3,45 SAY CHR(179)
        @ 3,78 SAY CHR(186)
        @ 4, 0 SAY CHR(186)
        @ 4, 6 SAY CHR(179)
        @ 4,39 SAY CHR(186)
        @ 4,45 SAY CHR(179)
        @ 4,78 SAY CHR(186)
        @ 5, 0 SAY CHR(186)
        @ 5, 6 SAY CHR(179)
        @ 5,39 SAY CHR(186)
        @ 5,45 SAY CHR(179)
        @ 5,78 SAY CHR(186)
        @ 4, 3 SAY "F1"
        @ 4,42 SAY "F2"
*
        num = 3
        FOR i=6 TO 18 STEP 4
            @ i, 0 SAY CHR(204)
            @ i, 1 SAY REPLICATE(CHR(205),5)
            @ i, 6 SAY CHR(216)
            @ i, 7 SAY REPLICATE(CHR(205),32)
            @ i,39 SAY CHR(206)
            @ i,40 SAY REPLICATE(CHR(205),5)
            @ i,45 SAY CHR(216)
            @ i,46 SAY REPLICATE(CHR(205),32)
            @ i,78 SAY CHR(185)
            @ i+1, 0 SAY CHR(186)
            @ i+1, 6 SAY CHR(179)
            @ i+1,39 SAY CHR(186)
            @ i+1,45 SAY CHR(179)
            @ i+1,78 SAY CHR(186)
            @ i+2, 0 SAY CHR(186)
            @ i+2, 6 SAY CHR(179)
            @ i+2,39 SAY CHR(186)
            @ i+2,45 SAY CHR(179)
            @ i+2,78 SAY CHR(186)
            @ i+3, 0 SAY CHR(186)
            @ i+3, 6 SAY CHR(179)
            @ i+3,39 SAY CHR(186)
            @ i+3,45 SAY CHR(179)
            @ i+3,78 SAY CHR(186)
            @ i+2, 3 SAY "F"+STR(num,1)
            @ i+2,42 SAY "F"+STR(num+1,1)
            num = num + 2
        NEXT
        @ 20,41 SAY "F10"
        @ i, 0 SAY CHR(209)
        @ i, 1 SAY REPLICATE(CHR(205),5)
        @ i, 6 SAY CHR(207)
        @ i, 7 SAY REPLICATE(CHR(205),32)
        @ i,39 SAY CHR(202)
        @ i,40 SAY REPLICATE(CHR(205),5)
        @ i,45 SAY CHR(207)
        @ i,46 SAY REPLICATE(CHR(205),32)
        @ i,78 SAY CHR(188)
        SAVE SCREEN TO m_help_map
        l_help_map = .T.
    ELSE
        RESTORE SCREEN FROM m_help_map
    ENDIF
    i = 3
    p = 0
    DO WHILE i < 20
        @ i  ,8 SAY SUBSTR(f_keytext[p+1],1,30)
        @ i+1,8 SAY SUBSTR(f_keytext[p+2],1,30)
        @ i+2,8 SAY SUBSTR(f_keytext[p+3],1,30)
        i = i + 4
        p = p + 6
    ENDDO
    i = 3
    p = 3
    DO WHILE i < 20
        @ i  ,47 SAY SUBSTR(f_keytext[p+1],1,30)
        @ i+1,47 SAY SUBSTR(f_keytext[p+2],1,30)
        @ i+2,47 SAY SUBSTR(f_keytext[p+3],1,30)
        i = i + 4
        p = p + 6
    ENDDO
    SAVE SCREEN TO m_hmap[n_map]
    l_hmap[n_map] = .T.
    INKEY(0)
ELSE
    RESTORE SCREEN FROM m_hmap[n_map]
    INKEY(0)
ENDIF

RESTORE SCREEN FROM m_map[n_map]
n_int_key = 10
KEYBOARD(CHR(27))

RETURN


PROCEDURE F2_KEY
**********************************************************************
*                                                                    *
* Das Unterprogramm F2_KEY setzt die variable N_INI_KEY auf 2        *
* wenn eine Maske mit der F2-Taste beendet wird.                     *
*                                                                    *
**********************************************************************
PARAMETERS p_name,p_line,p_var

n_int_key = 2
KEYBOARD(CHR(27))
RETURN


PROCEDURE F3_KEY
**********************************************************************
*                                                                    *
* Das Unterprogramm F3_KEY setzt die variable N_INI_KEY auf 3        *
* wenn eine Maske mit der F3-Taste beendet wird.                     *
*                                                                    *
**********************************************************************
PARAMETERS p_name,p_line,p_var

n_int_key = 3
KEYBOARD(CHR(27))
RETURN


PROCEDURE F4_KEY
**********************************************************************
*                                                                    *
* Das Unterprogramm F4_KEY setzt die variable N_INI_KEY auf 4        *
* wenn eine Maske mit der F4-Taste beendet wird.                     *
*                                                                    *
**********************************************************************
PARAMETERS p_name,p_line,p_var

n_int_key = 4
KEYBOARD(CHR(27))
RETURN


PROCEDURE F5_KEY
**********************************************************************
*                                                                    *
* Das Unterprogramm F5_KEY setzt die variable N_INI_KEY auf 5        *
* wenn eine Maske mit der F5-Taste beendet wird.                     *
*                                                                    *
**********************************************************************
PARAMETERS p_name,p_line,p_var

n_int_key = 5
KEYBOARD(CHR(27))
RETURN


PROCEDURE F6_KEY
**********************************************************************
*                                                                    *
* Das Unterprogramm F6_KEY setzt die variable N_INI_KEY auf 6        *
* wenn eine Maske mit der F6-Taste beendet wird.                     *
*                                                                    *
**********************************************************************
PARAMETERS p_name,p_line,p_var

n_int_key = 6
KEYBOARD(CHR(27))
RETURN


PROCEDURE F7_KEY
**********************************************************************
*                                                                    *
* Das Unterprogramm F7_KEY setzt die variable N_INI_KEY auf 7        *
* wenn eine Maske mit der F7-Taste beendet wird.                     *
*                                                                    *
**********************************************************************
PARAMETERS p_name,p_line,p_var

n_int_key = 7
KEYBOARD(CHR(27))
RETURN


PROCEDURE F8_KEY
**********************************************************************
*                                                                    *
* Das Unterprogramm F8_KEY setzt die variable N_INI_KEY auf 8        *
* wenn eine Maske mit der F8-Taste beendet wird.                     *
*                                                                    *
**********************************************************************
PARAMETERS p_name,p_line,p_var

n_int_key = 8
KEYBOARD(CHR(27))
RETURN


PROCEDURE F9_KEY
**********************************************************************
*                                                                    *
* Das Unterprogramm F9_KEY setzt die variable N_INI_KEY auf 9        *
* wenn eine Maske mit der F9-Taste beendet wird.                     *
*                                                                    *
**********************************************************************
PARAMETERS p_name,p_line,p_var

n_int_key = 9
KEYBOARD(CHR(27))
RETURN


PROCEDURE ENDE
**********************************************************************
* Das Unterprogramm ENDE beendet die Verarbeitung                    *
**********************************************************************
PARAMETERS p_name,p_line,p_var

CLOSE DATABASES

    SET MENU ON
    SET HEADING ON
    SET STATUS ON

    CLEAR ALL
    CLEAR

    IF FILE("TEMP*.*")
        RUN ERASE TEMP*.*
    ENDIF

    QUIT

RETURN

* EOF : HW0140.PRG