       IDENTIFICATION DIVISION.
       PROGRAM-ID. RACCHK.
      *RETROLABS ACME - FONTES V23
       AUTHOR. JOSE LUIZ MARTINS - RETROLABS.
      *RETROLABS ACME - SISTEMA INTEGRADO COMERCIAL
      *ALVO: CIS COBOL 4.5 / MP/M / ALTOS 5-15AD
      *VERSAO DOS FONTES: 0.3 - DEMONSTRACAO 1984

      *V12: DADOS NO DRIVE B: / SEM LOCK (UM TERMINAL POR VEZ)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-CLI ASSIGN TO "B:CLI.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS CL-CHAVE.
           SELECT ARQ-FOR ASSIGN TO "B:FORN.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS FR-CHAVE.
           SELECT ARQ-PRO ASSIGN TO "B:PROD.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS PR-CHAVE.
           SELECT ARQ-VEN ASSIGN TO "B:VEN.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS VD-CHAVE.
           SELECT ARQ-IV ASSIGN TO "B:ITVEN.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS IV-CHAVE.
           SELECT ARQ-COM ASSIGN TO "B:COMP.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS CP-CHAVE.
           SELECT ARQ-IC ASSIGN TO "B:ITCOM.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS IC-CHAVE.
           SELECT ARQ-REC ASSIGN TO "B:REC.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS CR-CHAVE.
           SELECT ARQ-PAG ASSIGN TO "B:PAG.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS PG-CHAVE.
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
       FD  ARQ-VEN RECORD CONTAINS 77 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-VEN.
          03 VD-CHAVE.
             05 VD-NUMERO              PIC 9(8).
          03 VD-DATA                 PIC 9(6).
          03 VD-CLIENTE              PIC 9(6).
          03 VD-VENDEDOR             PIC 9(3).
          03 VD-OPERADOR             PIC X(8).
          03 VD-TERMINAL             PIC 99.
          03 VD-COND-PAGTO           PIC 9(3).
          03 VD-QTD-ITENS            PIC 9(3).
          03 VD-VLR-MERC             PIC 9(8)V99.
          03 VD-DESCONTO             PIC 9(7)V99.
          03 VD-FRETE                PIC 9(6)V99.
          03 VD-TOTAL                PIC 9(8)V99.
          03 VD-SITUACAO             PIC X.
       FD  ARQ-IV RECORD CONTAINS 45 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-IV.
          03 IV-CHAVE.
             05 IV-VENDA             PIC 9(8).
             05 IV-ITEM              PIC 9(3).
          03 IV-PRODUTO              PIC 9(7).
          03 IV-QUANTIDADE           PIC 9(6).
          03 IV-PRECO                PIC 9(6)V99.
          03 IV-DESCONTO             PIC 99V9.
          03 IV-TOTAL                PIC 9(8)V99.
       FD  ARQ-COM RECORD CONTAINS 43 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-COM.
          03 CP-CHAVE.
             05 CP-NUMERO              PIC 9(8).
          03 CP-DATA                 PIC 9(6).
          03 CP-FORNECEDOR           PIC 9(5).
          03 CP-OPERADOR             PIC X(8).
          03 CP-TERMINAL             PIC 99.
          03 CP-QTD-ITENS            PIC 9(3).
          03 CP-TOTAL                PIC 9(8)V99.
          03 CP-SITUACAO             PIC X.
       FD  ARQ-IC RECORD CONTAINS 43 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-IC.
          03 IC-CHAVE.
             05 IC-COMPRA            PIC 9(8).
             05 IC-ITEM              PIC 9(3).
          03 IC-PRODUTO              PIC 9(7).
          03 IC-QUANTIDADE           PIC 9(7).
          03 IC-CUSTO                PIC 9(6)V99.
          03 IC-TOTAL                PIC 9(8)V99.
       FD  ARQ-REC RECORD CONTAINS 102 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-REC.
          03 CR-CHAVE.
             05 CR-TITULO            PIC 9(8).
             05 CR-PARCELA           PIC 99.
          03 CR-CLIENTE              PIC 9(6).
          03 CR-VENDA                PIC 9(8).
          03 CR-FATURA               PIC 9(8).
          03 CR-EMISSAO              PIC 9(6).
          03 CR-VENCIMENTO           PIC 9(6).
          03 CR-VALOR                PIC 9(8)V99.
          03 CR-JUROS                PIC 9(6)V99.
          03 CR-DESCONTO             PIC 9(6)V99.
          03 CR-RECEBIDO             PIC 9(8)V99.
          03 CR-DT-PAGTO             PIC 9(6).
          03 CR-PORTADOR             PIC 9(3).
          03 CR-NOSSO-NUMERO         PIC X(12).
          03 CR-SITUACAO             PIC X.
       FD  ARQ-PAG RECORD CONTAINS 65 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-PAG.
          03 PG-CHAVE.
             05 PG-TITULO            PIC 9(8).
             05 PG-PARCELA           PIC 99.
          03 PG-FORNECEDOR           PIC 9(5).
          03 PG-COMPRA               PIC 9(8).
          03 PG-EMISSAO              PIC 9(6).
          03 PG-VENCIMENTO           PIC 9(6).
          03 PG-VALOR                PIC 9(8)V99.
          03 PG-PAGO                 PIC 9(8)V99.
          03 PG-DT-PAGTO             PIC 9(6).
          03 PG-PORTADOR             PIC 9(3).
          03 PG-SITUACAO             PIC X.
       WORKING-STORAGE SECTION.
       01  WS-DATA-DRIVE          PIC XX VALUE "B:".
       01 WS-FIM                     PIC X.
       01 WS-C-CLI                   PIC 9(6).
       01 WS-C-FOR                   PIC 9(6).
       01 WS-C-PRO                   PIC 9(6).
       01 WS-C-VEN                   PIC 9(6).
       01 WS-C-IV                    PIC 9(8).
       01 WS-C-COM                   PIC 9(6).
       01 WS-C-IC                    PIC 9(8).
       01 WS-C-REC                   PIC 9(8).
       01 WS-C-PAG                   PIC 9(8).
       01 WS-T-VEN                   PIC 9(12)V99.
       01 WS-T-COM                   PIC 9(12)V99.
       01 WS-T-REC                   PIC 9(12)V99.
       01 WS-T-PAG                   PIC 9(12)V99.
      *---- SEQUENCIAS VT100 (TERATERM EM MODO VT100)
       01  VT-LIMPA.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[2J".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XX VALUE "[H".
       01  VT-INV.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
       01  VT-PISCA.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[5m".
       01  VT-FORTE.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
       01  VT-NORMAL.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  VT-APAGA.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XX VALUE "[K".
       01  VT-HOME.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XX VALUE "[H".
       01  VT-POS.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC X VALUE "[".
           03 VT-LIN            PIC 99.
           03 FILLER            PIC X VALUE ";".
           03 VT-COL            PIC 99.
           03 FILLER            PIC X VALUE "H".
       01  TL-MSG               PIC X(76) VALUE SPACES.
       01  TL-MSG-R REDEFINES TL-MSG.
           03 TL-MSG-1          PIC X.
           03 FILLER            PIC X(75).
       01  TL-PROMPT            PIC X(40) VALUE SPACES.
       01  TL-TRACO             PIC X(79) VALUE ALL "-".
       01  TL-TITULO            PIC X(78) VALUE SPACES.
       01  TL-TIT-PADRAO.
           03 FILLER            PIC X(39) VALUE
              "AUDITORIA DA BASE                      ".
           03 FILLER            PIC X(39) VALUE
              "                       RETROLABS ACME  ".
       01  TL-RODAPE            PIC X(79) VALUE SPACES.
       01  TL-RODAPE-R REDEFINES TL-RODAPE.
           03 TL-ROD-A          PIC X(34).
           03 TL-ROD-B          PIC X(45).
       01  TL-ROD-LE.
           03 FILLER            PIC X(39) VALUE
              " AGUARDE - LENDO O DRIVE B:        CONF".
           03 FILLER            PIC X(39) VALUE
              "ERINDO OS 12 ARQUIVOS DA BASE          ".
           03 FILLER            PIC X(1) VALUE
              " ".
       01  TL-ROD-FIM.
           03 FILLER            PIC X(39) VALUE
              " RETURN = VOLTA AO MENU            CONF".
           03 FILLER            PIC X(39) VALUE
              "IRA OS TOTAIS COM A FOLHA DA DEMO      ".
           03 FILLER            PIC X(1) VALUE
              " ".
       01  TL-TEM               PIC X VALUE "N".
       01  TL-DTW               PIC 9(6).
       01  TL-DTW-R REDEFINES TL-DTW.
           03 TL-DTW-AA         PIC 99.
           03 TL-DTW-MM         PIC 99.
           03 TL-DTW-DD         PIC 99.
       01  TL-DT-ED.
           03 TL-DE-DD          PIC 99.
           03 FILLER            PIC X VALUE "/".
           03 TL-DE-MM          PIC 99.
           03 FILLER            PIC X VALUE "/".
           03 TL-DE-AA          PIC 99.
       01  TL-PAUSA             PIC X VALUE SPACE.
       01  TK-R02.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
           03 FILLER            PIC X(40) VALUE
              "    ARQUIVO                          REG".
           03 FILLER            PIC X(32) VALUE
              "ISTROS               VALOR TOTAL".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  TK-R03.
           03 FILLER            PIC X(40) VALUE
              "    -------                          ---".
           03 FILLER            PIC X(32) VALUE
              "------               -----------".
       01  TK-R04.
           03 FILLER            PIC X(34) VALUE
              "    CLIENTES.................     ".
           03 TK04-1               PIC ZZZ,ZZZ,ZZ9.
       01  TK-R05.
           03 FILLER            PIC X(34) VALUE
              "    FORNECEDORES.............     ".
           03 TK05-1               PIC ZZZ,ZZZ,ZZ9.
       01  TK-R06.
           03 FILLER            PIC X(34) VALUE
              "    PRODUTOS.................     ".
           03 TK06-1               PIC ZZZ,ZZZ,ZZ9.
       01  TK-R07.
           03 FILLER            PIC X(34) VALUE
              "    VENDAS...................     ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
           03 TK07-1               PIC ZZZ,ZZZ,ZZ9.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(6) VALUE
              "      ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 TK07-2               PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  TK-R08.
           03 FILLER            PIC X(34) VALUE
              "    ITENS DE VENDAS..........     ".
           03 TK08-1               PIC ZZZ,ZZZ,ZZ9.
       01  TK-R09.
           03 FILLER            PIC X(34) VALUE
              "    COMPRAS..................     ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
           03 TK09-1               PIC ZZZ,ZZZ,ZZ9.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(6) VALUE
              "      ".
           03 TK09-2               PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  TK-R10.
           03 FILLER            PIC X(34) VALUE
              "    ITENS DE COMPRAS.........     ".
           03 TK10-1               PIC ZZZ,ZZZ,ZZ9.
       01  TK-R11.
           03 FILLER            PIC X(34) VALUE
              "    CONTAS A RECEBER.........     ".
           03 TK11-1               PIC ZZZ,ZZZ,ZZ9.
           03 FILLER            PIC X(6) VALUE
              "      ".
           03 TK11-2               PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  TK-R12.
           03 FILLER            PIC X(34) VALUE
              "    CONTAS A PAGAR...........     ".
           03 TK12-1               PIC ZZZ,ZZZ,ZZ9.
           03 FILLER            PIC X(6) VALUE
              "      ".
           03 TK12-2               PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  TK-R14.
           03 FILLER            PIC X(40) VALUE
              "    TOTAIS DE RECEBER E PAGAR: SOMENTE T".
           03 FILLER            PIC X(16) VALUE
              "ITULOS EM ABERTO".
       PROCEDURE DIVISION.
       INICIO.
           MOVE ZERO TO WS-C-CLI WS-C-FOR WS-C-PRO WS-C-VEN
                        WS-C-IV WS-C-COM WS-C-IC WS-C-REC WS-C-PAG
                        WS-T-VEN WS-T-COM WS-T-REC WS-T-PAG.
           MOVE TL-TIT-PADRAO TO TL-TITULO.
           PERFORM TL-TITULO-P.
           MOVE 12 TO VT-LIN.
           MOVE 20 TO VT-COL.
           DISPLAY VT-POS VT-INV VT-PISCA
                   " CONTANDO OS REGISTROS DA BASE - AGUARDE "
                   VT-NORMAL.
           MOVE TL-ROD-LE TO TL-RODAPE.
           PERFORM TL-RODAPE-P.
           PERFORM CONTA-CLI THRU CCLI-FIM.
           PERFORM CONTA-FOR THRU CFOR-FIM.
           PERFORM CONTA-PRO THRU CPRO-FIM.
           PERFORM CONTA-VEN THRU CVEN-FIM.
           PERFORM CONTA-IV THRU CIV-FIM.
           PERFORM CONTA-COM THRU CCOM-FIM.
           PERFORM CONTA-IC THRU CIC-FIM.
           PERFORM CONTA-REC THRU CREC-FIM.
           PERFORM CONTA-PAG THRU CPAG-FIM.
           PERFORM TL-TITULO-P.
           DISPLAY " ".
           DISPLAY TK-R02.
           DISPLAY TK-R03.
           MOVE WS-C-CLI TO TK04-1.
           DISPLAY TK-R04.
           MOVE WS-C-FOR TO TK05-1.
           DISPLAY TK-R05.
           MOVE WS-C-PRO TO TK06-1.
           DISPLAY TK-R06.
           MOVE WS-C-VEN TO TK07-1.
           MOVE WS-T-VEN TO TK07-2.
           DISPLAY TK-R07.
           MOVE WS-C-IV TO TK08-1.
           DISPLAY TK-R08.
           MOVE WS-C-COM TO TK09-1.
           MOVE WS-T-COM TO TK09-2.
           DISPLAY TK-R09.
           MOVE WS-C-IC TO TK10-1.
           DISPLAY TK-R10.
           MOVE WS-C-REC TO TK11-1.
           MOVE WS-T-REC TO TK11-2.
           DISPLAY TK-R11.
           MOVE WS-C-PAG TO TK12-1.
           MOVE WS-T-PAG TO TK12-2.
           DISPLAY TK-R12.
           DISPLAY TL-TRACO.
           DISPLAY TK-R14.
           MOVE "AUDITORIA CONCLUIDA - CONFIRA COM A FOLHA"
             TO TL-MSG.
           PERFORM TL-MSG-P.
           MOVE TL-ROD-FIM TO TL-RODAPE.
           PERFORM TL-RODAPE-P.
           MOVE "TECLE RETURN PARA VOLTAR AO MENU" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           ACCEPT TL-PAUSA.
           GO TO FIM-PROGRAMA.

       CONTA-CLI.
           OPEN INPUT ARQ-CLI.
           MOVE ZERO TO CL-CODIGO.
           START ARQ-CLI KEY NOT LESS THAN CL-CHAVE
              INVALID KEY GO TO CCLI-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM CCLI-LER UNTIL WS-FIM = "S".
       CCLI-FIM.
           CLOSE ARQ-CLI.
       CCLI-LER.
           READ ARQ-CLI NEXT RECORD AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N" ADD 1 TO WS-C-CLI.

       CONTA-FOR.
           OPEN INPUT ARQ-FOR.
           MOVE ZERO TO FR-CODIGO.
           START ARQ-FOR KEY NOT LESS THAN FR-CHAVE
              INVALID KEY GO TO CFOR-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM CFOR-LER UNTIL WS-FIM = "S".
       CFOR-FIM.
           CLOSE ARQ-FOR.
       CFOR-LER.
           READ ARQ-FOR NEXT RECORD AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N" ADD 1 TO WS-C-FOR.

       CONTA-PRO.
           OPEN INPUT ARQ-PRO.
           MOVE ZERO TO PR-CODIGO.
           START ARQ-PRO KEY NOT LESS THAN PR-CHAVE
              INVALID KEY GO TO CPRO-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM CPRO-LER UNTIL WS-FIM = "S".
       CPRO-FIM.
           CLOSE ARQ-PRO.
       CPRO-LER.
           READ ARQ-PRO NEXT RECORD AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N" ADD 1 TO WS-C-PRO.

       CONTA-VEN.
           OPEN INPUT ARQ-VEN.
           MOVE ZERO TO VD-NUMERO.
           START ARQ-VEN KEY NOT LESS THAN VD-CHAVE
              INVALID KEY GO TO CVEN-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM CVEN-LER UNTIL WS-FIM = "S".
       CVEN-FIM.
           CLOSE ARQ-VEN.
       CVEN-LER.
           READ ARQ-VEN NEXT RECORD AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N"
              ADD 1 TO WS-C-VEN
              ADD VD-TOTAL TO WS-T-VEN.

       CONTA-IV.
           OPEN INPUT ARQ-IV.
           MOVE ZERO TO IV-VENDA IV-ITEM.
           START ARQ-IV KEY NOT LESS THAN IV-CHAVE
              INVALID KEY GO TO CIV-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM CIV-LER UNTIL WS-FIM = "S".
       CIV-FIM.
           CLOSE ARQ-IV.
       CIV-LER.
           READ ARQ-IV NEXT RECORD AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N" ADD 1 TO WS-C-IV.

       CONTA-COM.
           OPEN INPUT ARQ-COM.
           MOVE ZERO TO CP-NUMERO.
           START ARQ-COM KEY NOT LESS THAN CP-CHAVE
              INVALID KEY GO TO CCOM-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM CCOM-LER UNTIL WS-FIM = "S".
       CCOM-FIM.
           CLOSE ARQ-COM.
       CCOM-LER.
           READ ARQ-COM NEXT RECORD AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N"
              ADD 1 TO WS-C-COM
              ADD CP-TOTAL TO WS-T-COM.

       CONTA-IC.
           OPEN INPUT ARQ-IC.
           MOVE ZERO TO IC-COMPRA IC-ITEM.
           START ARQ-IC KEY NOT LESS THAN IC-CHAVE
              INVALID KEY GO TO CIC-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM CIC-LER UNTIL WS-FIM = "S".
       CIC-FIM.
           CLOSE ARQ-IC.
       CIC-LER.
           READ ARQ-IC NEXT RECORD AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N" ADD 1 TO WS-C-IC.

       CONTA-REC.
           OPEN INPUT ARQ-REC.
           MOVE ZERO TO CR-TITULO CR-PARCELA.
           START ARQ-REC KEY NOT LESS THAN CR-CHAVE
              INVALID KEY GO TO CREC-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM CREC-LER UNTIL WS-FIM = "S".
       CREC-FIM.
           CLOSE ARQ-REC.
       CREC-LER.
           READ ARQ-REC NEXT RECORD AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N"
              ADD 1 TO WS-C-REC
              IF CR-SITUACAO = "A" ADD CR-VALOR TO WS-T-REC.

       CONTA-PAG.
           OPEN INPUT ARQ-PAG.
           MOVE ZERO TO PG-TITULO PG-PARCELA.
           START ARQ-PAG KEY NOT LESS THAN PG-CHAVE
              INVALID KEY GO TO CPAG-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM CPAG-LER UNTIL WS-FIM = "S".
       CPAG-FIM.
           CLOSE ARQ-PAG.
       CPAG-LER.
           READ ARQ-PAG NEXT RECORD AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "N"
              ADD 1 TO WS-C-PAG
              IF PG-SITUACAO = "A" ADD PG-VALOR TO WS-T-PAG.
      *---- ROTINAS DE TELA VT100 (V17)
       TL-TITULO-P.
           DISPLAY VT-LIMPA VT-INV " " TL-TITULO VT-NORMAL.
           DISPLAY TL-TRACO.
       TL-MSG-P.
           MOVE 20 TO VT-LIN.
           MOVE 1 TO VT-COL.
           IF TL-MSG = SPACES
              DISPLAY VT-POS VT-APAGA
           ELSE
              IF TL-MSG-1 = "*"
                 DISPLAY VT-POS VT-APAGA VT-INV VT-PISCA " " TL-MSG
                         " " VT-NORMAL
              ELSE
                 DISPLAY VT-POS VT-APAGA VT-INV " " TL-MSG " "
                         VT-NORMAL.
       TL-PERGUNTA.
           MOVE 23 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA.
           MOVE 22 TO VT-LIN.
           DISPLAY VT-POS VT-APAGA VT-FORTE " " TL-PROMPT VT-NORMAL.
       TL-RODAPE-P.
           MOVE 24 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-INV TL-ROD-A VT-NORMAL " " VT-INV TL-ROD-B
                   VT-NORMAL VT-HOME.
       TL-FMT-DATA.
           MOVE TL-DTW-DD TO TL-DE-DD.
           MOVE TL-DTW-MM TO TL-DE-MM.
           MOVE TL-DTW-AA TO TL-DE-AA.
       FIM-PROGRAMA.
           EXIT PROGRAM.
