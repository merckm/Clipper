// CREATE a new structure extended file, append some records and
// then CREATE FROM this file a new database file

SET DATE German

CREATE template
APPEND BLANK
FIELD->FIELD_NAME := "ZWST_AKT"
FIELD->FIELD_TYPE := "N"
FIELD->FIELD_LEN  := 3
APPEND BLANK
FIELD->FIELD_NAME := "VERMITTLER"
FIELD->FIELD_TYPE := "N"
FIELD->FIELD_LEN  := 9
APPEND BLANK
FIELD->FIELD_NAME := "STATUS"
FIELD->FIELD_TYPE := "C"
FIELD->FIELD_LEN  := 1
APPEND BLANK
FIELD->FIELD_NAME := "ANLAGEDAT"
FIELD->FIELD_TYPE := "D"     
FIELD->FIELD_LEN  := 8    
APPEND BLANK
FIELD->FIELD_NAME := "VM_NAME"
FIELD->FIELD_TYPE := "C"
FIELD->FIELD_LEN  := 20
APPEND BLANK
FIELD->FIELD_NAME := "VM_VORNAME"
FIELD->FIELD_TYPE := "C"     
FIELD->FIELD_LEN  := 14      
APPEND BLANK
FIELD->FIELD_NAME := "VM_FIRMA"
FIELD->FIELD_TYPE := "C"
FIELD->FIELD_LEN  := 18
APPEND BLANK
FIELD->FIELD_NAME := "VM_ANREDE"
FIELD->FIELD_TYPE := "C"
FIELD->FIELD_LEN  := 1
APPEND BLANK
FIELD->FIELD_NAME := "VM_TITEL"
FIELD->FIELD_TYPE := "C"
FIELD->FIELD_LEN  := 1
APPEND BLANK
FIELD->FIELD_NAME := "VM_STRASSE"
FIELD->FIELD_TYPE := "C"     
FIELD->FIELD_LEN  := 26
APPEND BLANK
FIELD->FIELD_NAME := "VM_PLZ"
FIELD->FIELD_TYPE := "N"     
FIELD->FIELD_LEN  := 4      
APPEND BLANK
FIELD->FIELD_NAME := "VM_ORT"
FIELD->FIELD_TYPE := "C"     
FIELD->FIELD_LEN  := 21     
APPEND BLANK
FIELD->FIELD_NAME := "TELPVW"
FIELD->FIELD_TYPE := "C"     
FIELD->FIELD_LEN  := 5      
APPEND BLANK
FIELD->FIELD_NAME := "TELPDW"
FIELD->FIELD_TYPE := "C"     
FIELD->FIELD_LEN  := 9   
APPEND BLANK
FIELD->FIELD_NAME := "TELFVW"
FIELD->FIELD_TYPE := "C"     
FIELD->FIELD_LEN  := 5      
APPEND BLANK
FIELD->FIELD_NAME := "TELFDW"
FIELD->FIELD_TYPE := "C"     
FIELD->FIELD_LEN  := 9   
APPEND BLANK
FIELD->FIELD_NAME := "VM_TYP"
FIELD->FIELD_TYPE := "C"     
FIELD->FIELD_LEN  := 2
APPEND BLANK
FIELD->FIELD_NAME := "KZ_UEBER"
FIELD->FIELD_TYPE := "C"     
FIELD->FIELD_LEN  := 1   
CLOSE
CREATE MDVERMI FROM template

USE MDVERMI

CLOSE