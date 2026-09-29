       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSTSHR.
      *V13: SONDA DE COMPARTILHAMENTO DE ARQUIVO SOB MP/M.
      *RODAR EM DOIS TERMINAIS AO MESMO TEMPO (VER LOCK13.TXT).
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-PROD ASSIGN TO "B:PROD.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PR-CHAVE
               FILE STATUS IS FS-PRO.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-PROD RECORD CONTAINS 188 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REG-PROD.
           03 PR-CHAVE.
              05 PR-CODIGO           PIC 9(7).
           03 PR-RESTO               PIC X(181).
       WORKING-STORAGE SECTION.
       01  FS-PRO.
           03 FS-1                   PIC X VALUE SPACE.
           03 FS-2                   PIC X VALUE SPACE.
      *STATUS 9: O SEGUNDO BYTE E BINARIO (ERRO DO RTS).
       01  FS-NUM                    PIC 9(4) COMP VALUE ZERO.
       01  FS-NUM-R REDEFINES FS-NUM.
           03 FS-ALTO                PIC X.
           03 FS-BAIXO               PIC X.
       01  FS-ED                     PIC 999.
       01  FS-TXT                    PIC XXX.
       01  WS-MODO                   PIC X VALUE SPACE.
       01  WS-PAUSA                  PIC X VALUE SPACE.
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY "TSTSHR - TESTE DE COMPARTILHAMENTO SOB MP/M".
           DISPLAY "MODO: E=ENTRADA (INPUT)  A=ATUALIZACAO (I-O)".
           ACCEPT WS-MODO.
           IF WS-MODO = "A" OPEN I-O ARQ-PROD
           ELSE OPEN INPUT ARQ-PROD.
           PERFORM MOSTRA-FS THRU MOSTRA-FIM.
           DISPLAY "OPEN  STATUS: " FS-1 " / " FS-TXT.
           IF FS-PRO NOT = "00" GO TO FIM.
           MOVE 1001327 TO PR-CODIGO.
           READ ARQ-PROD INVALID KEY DISPLAY "CHAVE NAO ACHADA".
           PERFORM MOSTRA-FS THRU MOSTRA-FIM.
           DISPLAY "READ  STATUS: " FS-1 " / " FS-TXT.
           IF WS-MODO NOT = "A" GO TO SEGURA.
           REWRITE REG-PROD INVALID KEY DISPLAY "REWRITE FALHOU".
           PERFORM MOSTRA-FS THRU MOSTRA-FIM.
           DISPLAY "REWRITE STATUS: " FS-1 " / " FS-TXT.
       SEGURA.
           DISPLAY "ARQUIVO ABERTO. ENTER PARA FECHAR.".
           ACCEPT WS-PAUSA.
           CLOSE ARQ-PROD.
           PERFORM MOSTRA-FS THRU MOSTRA-FIM.
           DISPLAY "CLOSE STATUS: " FS-1 " / " FS-TXT.
       FIM.
           DISPLAY "FIM DO TESTE.".
           STOP RUN.
       MOSTRA-FS.
           MOVE SPACES TO FS-TXT.
           MOVE FS-2 TO FS-TXT.
           IF FS-1 NOT = "9" GO TO MOSTRA-FIM.
           MOVE ZERO TO FS-NUM.
           MOVE FS-2 TO FS-BAIXO.
           MOVE FS-NUM TO FS-ED.
           MOVE FS-ED TO FS-TXT.
       MOSTRA-FIM.
           EXIT.
