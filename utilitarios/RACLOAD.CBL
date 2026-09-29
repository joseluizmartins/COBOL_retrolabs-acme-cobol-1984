       IDENTIFICATION DIVISION.
       PROGRAM-ID. RACLOAD.
       AUTHOR. JOSE LUIZ MARTINS - RETROLABS.
      *RETROLABS ACME - CARGA DA BASE DEMONSTRATIVA 1984
      *V12: DADOS NO DRIVE B: / SEM LOCK (UM TERMINAL POR VEZ)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT I-CLI ASSIGN TO "A:CLI.TXT"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT O-CLI ASSIGN TO "B:CLI.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS WCL-CHAVE.
           SELECT I-FOR ASSIGN TO "A:FORN.TXT"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT O-FOR ASSIGN TO "B:FORN.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS WFR-CHAVE.
           SELECT I-PRO ASSIGN TO "A:PROD.TXT"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT O-PRO ASSIGN TO "B:PROD.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS WPR-CHAVE.
           SELECT I-PF ASSIGN TO "A:PRODF.TXT"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT O-PF ASSIGN TO "B:PRODF.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS WPF-CHAVE.
           SELECT I-VEN ASSIGN TO "A:VEN.TXT"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT O-VEN ASSIGN TO "B:VEN.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS WVD-CHAVE.
           SELECT I-IV ASSIGN TO "A:ITVEN.TXT"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT O-IV ASSIGN TO "B:ITVEN.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS WIV-CHAVE.
           SELECT I-COM ASSIGN TO "A:COMP.TXT"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT O-COM ASSIGN TO "B:COMP.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS WCP-CHAVE.
           SELECT I-IC ASSIGN TO "A:ITCOM.TXT"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT O-IC ASSIGN TO "B:ITCOM.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS WIC-CHAVE.
           SELECT I-REC ASSIGN TO "A:REC.TXT"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT O-REC ASSIGN TO "B:REC.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS WCR-CHAVE.
           SELECT I-PAG ASSIGN TO "A:PAG.TXT"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT O-PAG ASSIGN TO "B:PAG.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS WPG-CHAVE.
           SELECT I-MOV ASSIGN TO "A:MOVEST.TXT"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT O-MOV ASSIGN TO "B:MOVEST.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS WMV-CHAVE.
           SELECT I-PAR ASSIGN TO "A:PARAM.TXT"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT O-PAR ASSIGN TO "B:PARAM.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS WPA-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD I-CLI RECORD CONTAINS 341 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 R-CLI.
          03 CL-CHAVE.
             05 CL-CODIGO           PIC 9(6).
          03 CL-RESTO               PIC X(335).
       FD O-CLI RECORD CONTAINS 341 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 W-CLI.
          03 WCL-CHAVE.
             05 WCL-CODIGO          PIC 9(6).
          03 WCL-RESTO              PIC X(335).
       FD I-FOR RECORD CONTAINS 330 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 R-FOR.
          03 FR-CHAVE.
             05 FR-CODIGO           PIC 9(5).
          03 FR-RESTO               PIC X(325).
       FD O-FOR RECORD CONTAINS 330 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 W-FOR.
          03 WFR-CHAVE.
             05 WFR-CODIGO          PIC 9(5).
          03 WFR-RESTO              PIC X(325).
       FD I-PRO RECORD CONTAINS 188 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 R-PRO.
          03 PR-CHAVE.
             05 PR-CODIGO           PIC 9(7).
          03 PR-RESTO               PIC X(181).
       FD O-PRO RECORD CONTAINS 188 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 W-PRO.
          03 WPR-CHAVE.
             05 WPR-CODIGO          PIC 9(7).
          03 WPR-RESTO              PIC X(181).
       FD I-PF RECORD CONTAINS 60 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 R-PF.
          03 PF-CHAVE.
             05 PF-PRODUTO          PIC 9(7).
             05 PF-FORNECEDOR       PIC 9(5).
          03 PF-RESTO               PIC X(48).
       FD O-PF RECORD CONTAINS 60 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 W-PF.
          03 WPF-CHAVE.
             05 WPF-PRODUTO         PIC 9(7).
             05 WPF-FORNECEDOR      PIC 9(5).
          03 WPF-RESTO              PIC X(48).
       FD I-VEN RECORD CONTAINS 77 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 R-VEN.
          03 VD-CHAVE.
             05 VD-NUMERO           PIC 9(8).
          03 VD-RESTO               PIC X(69).
       FD O-VEN RECORD CONTAINS 77 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 W-VEN.
          03 WVD-CHAVE.
             05 WVD-NUMERO          PIC 9(8).
          03 WVD-RESTO              PIC X(69).
       FD I-IV RECORD CONTAINS 45 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 R-IV.
          03 IV-CHAVE.
             05 IV-VENDA            PIC 9(8).
             05 IV-ITEM             PIC 9(3).
          03 IV-RESTO               PIC X(34).
       FD O-IV RECORD CONTAINS 45 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 W-IV.
          03 WIV-CHAVE.
             05 WIV-VENDA           PIC 9(8).
             05 WIV-ITEM            PIC 9(3).
          03 WIV-RESTO              PIC X(34).
       FD I-COM RECORD CONTAINS 43 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 R-COM.
          03 CP-CHAVE.
             05 CP-NUMERO           PIC 9(8).
          03 CP-RESTO               PIC X(35).
       FD O-COM RECORD CONTAINS 43 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 W-COM.
          03 WCP-CHAVE.
             05 WCP-NUMERO          PIC 9(8).
          03 WCP-RESTO              PIC X(35).
       FD I-IC RECORD CONTAINS 43 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 R-IC.
          03 IC-CHAVE.
             05 IC-COMPRA           PIC 9(8).
             05 IC-ITEM             PIC 9(3).
          03 IC-RESTO               PIC X(32).
       FD O-IC RECORD CONTAINS 43 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 W-IC.
          03 WIC-CHAVE.
             05 WIC-COMPRA          PIC 9(8).
             05 WIC-ITEM            PIC 9(3).
          03 WIC-RESTO              PIC X(32).
       FD I-REC RECORD CONTAINS 102 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 R-REC.
          03 CR-CHAVE.
             05 CR-TITULO           PIC 9(8).
             05 CR-PARCELA          PIC 99.
          03 CR-RESTO               PIC X(92).
       FD O-REC RECORD CONTAINS 102 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 W-REC.
          03 WCR-CHAVE.
             05 WCR-TITULO          PIC 9(8).
             05 WCR-PARCELA         PIC 99.
          03 WCR-RESTO              PIC X(92).
       FD I-PAG RECORD CONTAINS 65 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 R-PAG.
          03 PG-CHAVE.
             05 PG-TITULO           PIC 9(8).
             05 PG-PARCELA          PIC 99.
          03 PG-RESTO               PIC X(55).
       FD O-PAG RECORD CONTAINS 65 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 W-PAG.
          03 WPG-CHAVE.
             05 WPG-TITULO          PIC 9(8).
             05 WPG-PARCELA         PIC 99.
          03 WPG-RESTO              PIC X(55).
       FD I-MOV RECORD CONTAINS 50 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 R-MOV.
          03 MV-CHAVE.
             05 MV-TIPO             PIC X.
             05 MV-DOCUMENTO        PIC 9(8).
             05 MV-ITEM             PIC 9(3).
          03 MV-RESTO               PIC X(38).
       FD O-MOV RECORD CONTAINS 50 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 W-MOV.
          03 WMV-CHAVE.
             05 WMV-TIPO            PIC X.
             05 WMV-DOCUMENTO       PIC 9(8).
             05 WMV-ITEM            PIC 9(3).
          03 WMV-RESTO              PIC X(38).
       FD I-PAR RECORD CONTAINS 64 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 R-PAR.
          03 PA-KEY.
             05 PA-CHAVE            PIC 99.
          03 PA-RESTO               PIC X(62).
       FD O-PAR RECORD CONTAINS 64 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 W-PAR.
          03 WPA-KEY.
             05 WPA-CHAVE           PIC 99.
          03 WPA-RESTO              PIC X(62).
       WORKING-STORAGE SECTION.
       01  WS-DATA-DRIVE          PIC XX VALUE "B:".
       01 WS-FIM                    PIC X.
       01 WS-CONTA                  PIC 9(6).
       01 WS-OPCAO                  PIC X VALUE SPACE.
       01 WS-OK                     PIC X VALUE "N".
       01 WS-REJ                    PIC 9(6) VALUE ZERO.
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY " ".
           DISPLAY "RETROLABS ACME - CARGA AUTOMATICA DA BASE 1984".
           DISPLAY "------------------------------------------------".
           DISPLAY "LE OS ARQUIVOS .TXT DO DRIVE A: E GRAVA A BASE".
           DISPLAY "(ARQUIVOS .DAT E .IDX) NO DRIVE B:.".
           DISPLAY "ATENCAO: A BASE ATUAL DO DRIVE B: SERA RECRIADA.".
           DISPLAY " ".
           MOVE "N" TO WS-OK.
           PERFORM CONFIRMA UNTIL WS-OK = "S".
           IF WS-OPCAO NOT = "S"
              DISPLAY "CARGA CANCELADA."
              STOP RUN.
           DISPLAY " ".
           DISPLAY " 1/12  PARAMETROS.................".
           PERFORM CARGA-PAR.
           DISPLAY " 2/12  CLIENTES...................".
           PERFORM CARGA-CLI.
           DISPLAY " 3/12  FORNECEDORES...............".
           PERFORM CARGA-FOR.
           DISPLAY " 4/12  PRODUTOS...................".
           PERFORM CARGA-PRO.
           DISPLAY " 5/12  PRODUTO X FORNECEDOR.......".
           PERFORM CARGA-PF.
           DISPLAY " 6/12  VENDAS.....................".
           PERFORM CARGA-VEN.
           DISPLAY " 7/12  ITENS DE VENDAS............".
           PERFORM CARGA-IV.
           DISPLAY " 8/12  COMPRAS....................".
           PERFORM CARGA-COM.
           DISPLAY " 9/12  ITENS DE COMPRAS...........".
           PERFORM CARGA-IC.
           DISPLAY "10/12  CONTAS A RECEBER...........".
           PERFORM CARGA-REC.
           DISPLAY "11/12  CONTAS A PAGAR.............".
           PERFORM CARGA-PAG.
           DISPLAY "12/12  MOVIMENTO DE ESTOQUE.......".
           PERFORM CARGA-MOV.
           DISPLAY " ".
           IF WS-REJ NOT = ZERO
              DISPLAY "ATENCAO - CHAVES DUPLICADAS REJEITADAS: " WS-REJ.
           DISPLAY "CARGA CONCLUIDA: 12 DE 12 ARQUIVOS.".
           DISPLAY "CONFIRA NO MENU, OPCAO C (AUDITORIA DA BASE).".
           STOP RUN.

      *A PRIMEIRA LEITURA DO CONSOLE PODE VIR VAZIA (PARAMETROS DA
      *LINHA DE COMANDO DO RUNA): REPETE ATE RESPOSTA S OU N.
       CONFIRMA.
           DISPLAY "CONFIRMA A CARGA (S/N)?".
           MOVE SPACE TO WS-OPCAO.
           ACCEPT WS-OPCAO.
           IF WS-OPCAO = "s" MOVE "S" TO WS-OPCAO.
           IF WS-OPCAO = "n" MOVE "N" TO WS-OPCAO.
           IF WS-OPCAO = "S" MOVE "S" TO WS-OK.
           IF WS-OPCAO = "N" MOVE "S" TO WS-OK.

       CARGA-CLI.
           OPEN INPUT I-CLI.
           OPEN OUTPUT O-CLI.
           MOVE "N" TO WS-FIM.
           MOVE ZERO TO WS-CONTA.
           PERFORM LER-CLI UNTIL WS-FIM = "S".
           CLOSE I-CLI O-CLI.
           DISPLAY "        REGISTROS GRAVADOS: " WS-CONTA.
       LER-CLI.
           READ I-CLI AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N"
              MOVE R-CLI TO W-CLI
              ADD 1 TO WS-CONTA
              WRITE W-CLI
                 INVALID KEY ADD 1 TO WS-REJ.

       CARGA-FOR.
           OPEN INPUT I-FOR.
           OPEN OUTPUT O-FOR.
           MOVE "N" TO WS-FIM.
           MOVE ZERO TO WS-CONTA.
           PERFORM LER-FOR UNTIL WS-FIM = "S".
           CLOSE I-FOR O-FOR.
           DISPLAY "        REGISTROS GRAVADOS: " WS-CONTA.
       LER-FOR.
           READ I-FOR AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N"
              MOVE R-FOR TO W-FOR
              ADD 1 TO WS-CONTA
              WRITE W-FOR
                 INVALID KEY ADD 1 TO WS-REJ.

       CARGA-PRO.
           OPEN INPUT I-PRO.
           OPEN OUTPUT O-PRO.
           MOVE "N" TO WS-FIM.
           MOVE ZERO TO WS-CONTA.
           PERFORM LER-PRO UNTIL WS-FIM = "S".
           CLOSE I-PRO O-PRO.
           DISPLAY "        REGISTROS GRAVADOS: " WS-CONTA.
       LER-PRO.
           READ I-PRO AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N"
              MOVE R-PRO TO W-PRO
              ADD 1 TO WS-CONTA
              WRITE W-PRO
                 INVALID KEY ADD 1 TO WS-REJ.

       CARGA-PF.
           OPEN INPUT I-PF.
           OPEN OUTPUT O-PF.
           MOVE "N" TO WS-FIM.
           MOVE ZERO TO WS-CONTA.
           PERFORM LER-PF UNTIL WS-FIM = "S".
           CLOSE I-PF O-PF.
           DISPLAY "        REGISTROS GRAVADOS: " WS-CONTA.
       LER-PF.
           READ I-PF AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N"
              MOVE R-PF TO W-PF
              ADD 1 TO WS-CONTA
              WRITE W-PF
                 INVALID KEY ADD 1 TO WS-REJ.

       CARGA-VEN.
           OPEN INPUT I-VEN.
           OPEN OUTPUT O-VEN.
           MOVE "N" TO WS-FIM.
           MOVE ZERO TO WS-CONTA.
           PERFORM LER-VEN UNTIL WS-FIM = "S".
           CLOSE I-VEN O-VEN.
           DISPLAY "        REGISTROS GRAVADOS: " WS-CONTA.
       LER-VEN.
           READ I-VEN AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N"
              MOVE R-VEN TO W-VEN
              ADD 1 TO WS-CONTA
              WRITE W-VEN
                 INVALID KEY ADD 1 TO WS-REJ.

       CARGA-IV.
           OPEN INPUT I-IV.
           OPEN OUTPUT O-IV.
           MOVE "N" TO WS-FIM.
           MOVE ZERO TO WS-CONTA.
           PERFORM LER-IV UNTIL WS-FIM = "S".
           CLOSE I-IV O-IV.
           DISPLAY "        REGISTROS GRAVADOS: " WS-CONTA.
       LER-IV.
           READ I-IV AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N"
              MOVE R-IV TO W-IV
              ADD 1 TO WS-CONTA
              WRITE W-IV
                 INVALID KEY ADD 1 TO WS-REJ.

       CARGA-COM.
           OPEN INPUT I-COM.
           OPEN OUTPUT O-COM.
           MOVE "N" TO WS-FIM.
           MOVE ZERO TO WS-CONTA.
           PERFORM LER-COM UNTIL WS-FIM = "S".
           CLOSE I-COM O-COM.
           DISPLAY "        REGISTROS GRAVADOS: " WS-CONTA.
       LER-COM.
           READ I-COM AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N"
              MOVE R-COM TO W-COM
              ADD 1 TO WS-CONTA
              WRITE W-COM
                 INVALID KEY ADD 1 TO WS-REJ.

       CARGA-IC.
           OPEN INPUT I-IC.
           OPEN OUTPUT O-IC.
           MOVE "N" TO WS-FIM.
           MOVE ZERO TO WS-CONTA.
           PERFORM LER-IC UNTIL WS-FIM = "S".
           CLOSE I-IC O-IC.
           DISPLAY "        REGISTROS GRAVADOS: " WS-CONTA.
       LER-IC.
           READ I-IC AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N"
              MOVE R-IC TO W-IC
              ADD 1 TO WS-CONTA
              WRITE W-IC
                 INVALID KEY ADD 1 TO WS-REJ.

       CARGA-REC.
           OPEN INPUT I-REC.
           OPEN OUTPUT O-REC.
           MOVE "N" TO WS-FIM.
           MOVE ZERO TO WS-CONTA.
           PERFORM LER-REC UNTIL WS-FIM = "S".
           CLOSE I-REC O-REC.
           DISPLAY "        REGISTROS GRAVADOS: " WS-CONTA.
       LER-REC.
           READ I-REC AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N"
              MOVE R-REC TO W-REC
              ADD 1 TO WS-CONTA
              WRITE W-REC
                 INVALID KEY ADD 1 TO WS-REJ.

       CARGA-PAG.
           OPEN INPUT I-PAG.
           OPEN OUTPUT O-PAG.
           MOVE "N" TO WS-FIM.
           MOVE ZERO TO WS-CONTA.
           PERFORM LER-PAG UNTIL WS-FIM = "S".
           CLOSE I-PAG O-PAG.
           DISPLAY "        REGISTROS GRAVADOS: " WS-CONTA.
       LER-PAG.
           READ I-PAG AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N"
              MOVE R-PAG TO W-PAG
              ADD 1 TO WS-CONTA
              WRITE W-PAG
                 INVALID KEY ADD 1 TO WS-REJ.

       CARGA-MOV.
           OPEN INPUT I-MOV.
           OPEN OUTPUT O-MOV.
           MOVE "N" TO WS-FIM.
           MOVE ZERO TO WS-CONTA.
           PERFORM LER-MOV UNTIL WS-FIM = "S".
           CLOSE I-MOV O-MOV.
           DISPLAY "        REGISTROS GRAVADOS: " WS-CONTA.
       LER-MOV.
           READ I-MOV AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N"
              MOVE R-MOV TO W-MOV
              ADD 1 TO WS-CONTA
              WRITE W-MOV
                 INVALID KEY ADD 1 TO WS-REJ.

       CARGA-PAR.
           OPEN INPUT I-PAR.
           OPEN OUTPUT O-PAR.
           MOVE "N" TO WS-FIM.
           MOVE ZERO TO WS-CONTA.
           PERFORM LER-PAR UNTIL WS-FIM = "S".
           CLOSE I-PAR O-PAR.
           DISPLAY "        REGISTROS GRAVADOS: " WS-CONTA.
       LER-PAR.
           READ I-PAR AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N"
              MOVE R-PAR TO W-PAR
              ADD 1 TO WS-CONTA
              WRITE W-PAR
                 INVALID KEY ADD 1 TO WS-REJ.
