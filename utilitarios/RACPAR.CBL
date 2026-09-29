       IDENTIFICATION DIVISION.
       PROGRAM-ID. RACPAR.
       AUTHOR. JOSE LUIZ MARTINS - RETROLABS.
      *RETROLABS ACME - SISTEMA INTEGRADO COMERCIAL
      *ALVO: CIS COBOL 4.5 / MP/M / ALTOS 5-15AD
      *VERSAO DOS FONTES: 0.2 - DEMONSTRACAO 1984

      *V12: DADOS NO DRIVE B: / SEM LOCK (UM TERMINAL POR VEZ)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-PARAM ASSIGN TO "B:PARAM.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PA-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-PARAM RECORD CONTAINS 64 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REG-PARAM.
           03 PA-KEY.
              05 PA-CHAVE             PIC 99.
           03 PA-PROX-VENDA          PIC 9(8).
           03 PA-PROX-COMPRA         PIC 9(8).
           03 PA-PROX-MOV            PIC 9(6).
           03 PA-EMPRESA             PIC X(30).
           03 PA-ANO                 PIC 99.
           03 PA-VERSAO              PIC X(8).
       WORKING-STORAGE SECTION.
       01  WS-DATA-DRIVE          PIC XX VALUE "B:".
       01  WS-OPCAO                  PIC X.
       PROCEDURE DIVISION.
       INICIO.
           OPEN I-O ARQ-PARAM.
           MOVE 01 TO PA-CHAVE.
           READ ARQ-PARAM
              INVALID KEY
                 MOVE 00090001 TO PA-PROX-VENDA
                 MOVE 00050001 TO PA-PROX-COMPRA
                 MOVE 000001 TO PA-PROX-MOV
                 MOVE "RETROLABS ACME" TO PA-EMPRESA
                 MOVE 84 TO PA-ANO
                 MOVE "2.4" TO PA-VERSAO
                 WRITE REG-PARAM
                    INVALID KEY DISPLAY "ERRO CRIANDO PARAMETROS".
           PERFORM MOSTRA.
           DISPLAY "ALTERAR PARAMETROS (S/N)? ".
           ACCEPT WS-OPCAO.
           IF WS-OPCAO = "S"
              DISPLAY "PROXIMA VENDA: "
              ACCEPT PA-PROX-VENDA
              DISPLAY "PROXIMA COMPRA: "
              ACCEPT PA-PROX-COMPRA
              DISPLAY "PROXIMO MOVIMENTO: "
              ACCEPT PA-PROX-MOV
              REWRITE REG-PARAM
                 INVALID KEY DISPLAY "ERRO ATUALIZANDO PARAMETROS".
           CLOSE ARQ-PARAM.
           GO TO FIM-PROGRAMA.
       MOSTRA.
           DISPLAY SPACE.
           DISPLAY "RETROLABS ACME - PARAMETROS DO SISTEMA".
           DISPLAY "PROXIMA VENDA : " PA-PROX-VENDA.
           DISPLAY "PROXIMA COMPRA: " PA-PROX-COMPRA.
           DISPLAY "PROXIMO MOV.  : " PA-PROX-MOV.
           DISPLAY "EMPRESA       : " PA-EMPRESA.
           DISPLAY "ANO           : " PA-ANO.
           DISPLAY "VERSAO        : " PA-VERSAO.

       FIM-PROGRAMA.
           EXIT PROGRAM.
