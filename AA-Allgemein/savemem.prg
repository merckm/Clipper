STORE " 1"                    TO QFILNR
STORE "VERKAUFSHAUS M�NCHENH" TO QFILNAME
STORE "DESTOUCHESSTR. 35"     TO QFILSTR
STORE "8000 M�NCHEN"          TO QFILORT
STORE "089/3008220"           TO QFILTEL

SAVE TO QFILIALE

RELEASE ALL

STORE 1                    TO ZPSNR
STORE "KRIEGER"            TO ZNAME
STORE "08"                 TO ZTAETIG

SAVE TO ZBENUTZ

RELEASE ALL

RESTORE FROM QFILIALE
RESTORE FROM ZBENUTZ ADDITIVE
* DISPLAY MEMORY
