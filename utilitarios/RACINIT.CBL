       IDENTIFICATION DIVISION.
       PROGRAM-ID. RACINIT.
       AUTHOR. JOSE LUIZ MARTINS - RETROLABS.
      *RETROLABS ACME - SISTEMA INTEGRADO COMERCIAL
      *ALVO: CIS COBOL 4.5 / MP/M / ALTOS 5-15AD
      *VERSAO DOS FONTES: 0.2 - DEMONSTRACAO 1984

      *V12: DADOS NO DRIVE B: / SEM LOCK (UM TERMINAL POR VEZ)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-CLI ASSIGN TO "B:CLI.DAT"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS CL-CHAVE.
           SELECT ARQ-FOR ASSIGN TO "B:FORN.DAT"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS FR-CHAVE.
           SELECT ARQ-PRO ASSIGN TO "B:PROD.DAT"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS PR-CHAVE.
           SELECT ARQ-PF ASSIGN TO "B:PRODF.DAT"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS PF-CHAVE.
           SELECT ARQ-VEN ASSIGN TO "B:VEN.DAT"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS VD-CHAVE.
           SELECT ARQ-IV ASSIGN TO "B:ITVEN.DAT"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS IV-CHAVE.
           SELECT ARQ-COM ASSIGN TO "B:COMP.DAT"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS CP-CHAVE.
           SELECT ARQ-IC ASSIGN TO "B:ITCOM.DAT"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS IC-CHAVE.
           SELECT ARQ-REC ASSIGN TO "B:REC.DAT"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS CR-CHAVE.
           SELECT ARQ-PAG ASSIGN TO "B:PAG.DAT"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS PG-CHAVE.
           SELECT ARQ-MOV ASSIGN TO "B:MOVEST.DAT"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS MV-CHAVE.
           SELECT ARQ-PAR ASSIGN TO "B:PARAM.DAT"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS PA-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-CLI RECORD CONTAINS 341 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-CLI.
          03 CL-CHAVE.
             05 CL-CODIGO              PIC 9(6).
          03 CL-RESTO                PIC X(335).
       FD  ARQ-FOR RECORD CONTAINS 330 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-FOR.
          03 FR-CHAVE.
             05 FR-CODIGO              PIC 9(5).
          03 FR-RESTO                PIC X(325).
       FD  ARQ-PRO RECORD CONTAINS 188 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-PRO.
          03 PR-CHAVE.
             05 PR-CODIGO              PIC 9(7).
          03 PR-RESTO                PIC X(181).
       FD  ARQ-PF RECORD CONTAINS 60 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-PF.
          03 PF-CHAVE.
             05 PF-PRODUTO           PIC 9(7).
             05 PF-FORNECEDOR        PIC 9(5).
          03 PF-RESTO                PIC X(48).
       FD  ARQ-VEN RECORD CONTAINS 77 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-VEN.
          03 VD-CHAVE.
             05 VD-NUMERO              PIC 9(8).
          03 VD-RESTO                PIC X(69).
       FD  ARQ-IV RECORD CONTAINS 45 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-IV.
          03 IV-CHAVE.
             05 IV-VENDA             PIC 9(8).
             05 IV-ITEM              PIC 9(3).
          03 IV-RESTO                PIC X(34).
       FD  ARQ-COM RECORD CONTAINS 43 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-COM.
          03 CP-CHAVE.
             05 CP-NUMERO              PIC 9(8).
          03 CP-RESTO                PIC X(35).
       FD  ARQ-IC RECORD CONTAINS 43 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-IC.
          03 IC-CHAVE.
             05 IC-COMPRA            PIC 9(8).
             05 IC-ITEM              PIC 9(3).
          03 IC-RESTO                PIC X(32).
       FD  ARQ-REC RECORD CONTAINS 102 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-REC.
          03 CR-CHAVE.
             05 CR-TITULO            PIC 9(8).
             05 CR-PARCELA           PIC 99.
          03 CR-RESTO                PIC X(92).
       FD  ARQ-PAG RECORD CONTAINS 65 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-PAG.
          03 PG-CHAVE.
             05 PG-TITULO            PIC 9(8).
             05 PG-PARCELA           PIC 99.
          03 PG-RESTO                PIC X(55).
       FD  ARQ-MOV RECORD CONTAINS 50 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-MOV.
          03 MV-CHAVE.
             05 MV-TIPO             PIC X.
             05 MV-DOCUMENTO        PIC 9(8).
             05 MV-ITEM             PIC 9(3).
          03 MV-RESTO               PIC X(38).
       FD  ARQ-PAR RECORD CONTAINS 64 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-PAR.
          03 PA-KEY.
             05 PA-CHAVE             PIC 99.
          03 PA-PROX-VENDA           PIC 9(8).
          03 PA-PROX-COMPRA          PIC 9(8).
          03 PA-PROX-MOV             PIC 9(6).
          03 PA-EMPRESA              PIC X(30).
          03 PA-ANO                  PIC 99.
          03 PA-VERSAO               PIC X(8).
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY SPACE.
           DISPLAY "RETROLABS ACME - INICIALIZACAO DOS ARQUIVOS".
           DISPLAY "ATENCAO: ESTE PROGRAMA RECRIA OS ARQUIVOS VAZIOS.".
           DISPLAY "USE SOMENTE EM INSTALACAO NOVA.".
           OPEN OUTPUT ARQ-CLI.
           CLOSE ARQ-CLI.
           OPEN OUTPUT ARQ-FOR.
           CLOSE ARQ-FOR.
           OPEN OUTPUT ARQ-PRO.
           CLOSE ARQ-PRO.
           OPEN OUTPUT ARQ-PF.
           CLOSE ARQ-PF.
           OPEN OUTPUT ARQ-VEN.
           CLOSE ARQ-VEN.
           OPEN OUTPUT ARQ-IV.
           CLOSE ARQ-IV.
           OPEN OUTPUT ARQ-COM.
           CLOSE ARQ-COM.
           OPEN OUTPUT ARQ-IC.
           CLOSE ARQ-IC.
           OPEN OUTPUT ARQ-REC.
           CLOSE ARQ-REC.
           OPEN OUTPUT ARQ-PAG.
           CLOSE ARQ-PAG.
           OPEN OUTPUT ARQ-MOV.
           CLOSE ARQ-MOV.
           OPEN OUTPUT ARQ-PAR.
           MOVE 01 TO PA-CHAVE.
           MOVE 00090001 TO PA-PROX-VENDA.
           MOVE 00050001 TO PA-PROX-COMPRA.
           MOVE 000001 TO PA-PROX-MOV.
           MOVE "RETROLABS ACME" TO PA-EMPRESA.
           MOVE 84 TO PA-ANO.
           MOVE "2.4" TO PA-VERSAO.
           WRITE REG-PAR
              INVALID KEY DISPLAY "ERRO GRAVANDO PARAM".
           CLOSE ARQ-PAR.
           DISPLAY "ARQUIVOS INICIALIZADOS.".
           STOP RUN.
