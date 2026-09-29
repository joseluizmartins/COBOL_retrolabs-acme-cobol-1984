       IDENTIFICATION DIVISION.
       PROGRAM-ID. RACCOM.
      *RETROLABS ACME - FONTES V23
       AUTHOR. JOSE LUIZ MARTINS - RETROLABS.
      *RETROLABS ACME - SISTEMA INTEGRADO COMERCIAL
      *AMBIENTE ALVO: CIS COBOL 4.5 / MP/M / ALTOS 5-15AD
      *VERSAO DOS FONTES: 0.1 - 1984 DEMO

      *V12: DADOS NO DRIVE B: / SEM LOCK (UM TERMINAL POR VEZ)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-COMPRA ASSIGN TO "B:COMP.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS CP-CHAVE
              FILE STATUS IS WS-FS1.
           SELECT ARQ-ICOMP ASSIGN TO "B:ITCOM.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS IC-CHAVE
              FILE STATUS IS WS-FS2.
           SELECT ARQ-FORNEC ASSIGN TO "B:FORN.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS FR-CHAVE
              FILE STATUS IS WS-FS3.
           SELECT ARQ-PROD ASSIGN TO "B:PROD.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS PR-CHAVE
              FILE STATUS IS WS-FS4.
           SELECT ARQ-PAGAR ASSIGN TO "B:PAG.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS PG-CHAVE
              FILE STATUS IS WS-FS5.
           SELECT ARQ-MOV ASSIGN TO "B:MOVEST.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS MV-CHAVE
              FILE STATUS IS WS-FS6.
           SELECT ARQ-PARAM ASSIGN TO "B:PARAM.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS PA-KEY
              FILE STATUS IS WS-FS7.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-COMPRA RECORD CONTAINS 43 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-COMPRA.
          03 CP-CHAVE.
             05 CP-NUMERO              PIC 9(8).
          03 CP-DATA                PIC 9(6).
          03 CP-FORNECEDOR          PIC 9(5).
          03 CP-OPERADOR            PIC X(8).
          03 CP-TERMINAL            PIC 99.
          03 CP-QTD-ITENS           PIC 9(3).
          03 CP-TOTAL               PIC 9(8)V99.
          03 CP-SITUACAO            PIC X.
       FD  ARQ-ICOMP RECORD CONTAINS 43 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-ITEM-COMPRA.
          03 IC-CHAVE.
             05 IC-COMPRA           PIC 9(8).
             05 IC-ITEM             PIC 9(3).
          03 IC-PRODUTO             PIC 9(7).
          03 IC-QUANTIDADE          PIC 9(7).
          03 IC-CUSTO               PIC 9(6)V99.
          03 IC-TOTAL               PIC 9(8)V99.
       FD  ARQ-FORNEC RECORD CONTAINS 330 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-FORNECEDOR.
          03 FR-CHAVE.
             05 FR-CODIGO              PIC 9(5).
          03 FR-RAZAO               PIC X(38).
          03 FR-FANTASIA            PIC X(25).
          03 FR-CGC-MF              PIC X(18).
          03 FR-INSC-EST            PIC X(15).
          03 FR-ENDERECO            PIC X(35).
          03 FR-BAIRRO              PIC X(20).
          03 FR-CIDADE              PIC X(22).
          03 FR-UF                  PIC X(2).
          03 FR-CEP                 PIC 9(5).
          03 FR-DDD                 PIC 9(3).
          03 FR-TELEFONE            PIC X(10).
          03 FR-TELEX               PIC X(15).
          03 FR-CONTATO             PIC X(25).
          03 FR-CARGO               PIC X(20).
          03 FR-PRACA               PIC X(22).
          03 FR-COND-PAGTO          PIC 9(3).
          03 FR-DESCONTO            PIC 99V9.
          03 FR-PRAZO-ENTREGA       PIC 9(3).
          03 FR-FRETE               PIC X(3).
          03 FR-TOTAL-ANO           PIC 9(9)V99.
          03 FR-SALDO               PIC 9(8)V99.
          03 FR-ULT-COMPRA          PIC 9(6).
          03 FR-TIT-ABERTOS         PIC 9(4).
          03 FR-DT-CADASTRO         PIC 9(6).
          03 FR-SITUACAO            PIC X.
       FD  ARQ-PROD RECORD CONTAINS 188 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-PRODUTO.
          03 PR-CHAVE.
             05 PR-CODIGO              PIC 9(7).
          03 PR-PART-NUMBER         PIC X(16).
          03 PR-DESCRICAO           PIC X(30).
          03 PR-FABRICANTE          PIC X(20).
          03 PR-GRUPO               PIC 9(3).
          03 PR-UNIDADE             PIC X(2).
          03 PR-LOCAL               PIC X(8).
          03 PR-ESTOQUE             PIC 9(7).
          03 PR-RESERVADO           PIC 9(7).
          03 PR-MINIMO              PIC 9(7).
          03 PR-MAXIMO              PIC 9(7).
          03 PR-PONTO-PEDIDO        PIC 9(7).
          03 PR-EM-COMPRA           PIC 9(7).
          03 PR-CUSTO-MEDIO         PIC 9(6)V99.
          03 PR-ULT-CUSTO           PIC 9(6)V99.
          03 PR-PRECO-VENDA         PIC 9(6)V99.
          03 PR-FORNEC-PRINC        PIC 9(5).
          03 PR-DT-ULT-ENT          PIC 9(6).
          03 PR-DT-ULT-VEN          PIC 9(6).
          03 PR-QTD-VEND-ANO        PIC 9(8).
          03 PR-VLR-VEND-ANO        PIC 9(8)V99.
          03 PR-SITUACAO            PIC X.
       FD  ARQ-PAGAR RECORD CONTAINS 65 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-PAGAR.
          03 PG-CHAVE.
             05 PG-TITULO           PIC 9(8).
             05 PG-PARCELA          PIC 99.
          03 PG-FORNECEDOR          PIC 9(5).
          03 PG-COMPRA              PIC 9(8).
          03 PG-EMISSAO             PIC 9(6).
          03 PG-VENCIMENTO          PIC 9(6).
          03 PG-VALOR               PIC 9(8)V99.
          03 PG-PAGO                PIC 9(8)V99.
          03 PG-DT-PAGTO            PIC 9(6).
          03 PG-PORTADOR            PIC 9(3).
          03 PG-SITUACAO            PIC X.
       FD  ARQ-MOV RECORD CONTAINS 50 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REG-MOV.
           03 MV-CHAVE.
              05 MV-TIPO             PIC X.
              05 MV-DOCUMENTO        PIC 9(8).
              05 MV-ITEM             PIC 9(3).
           03 MV-DATA                PIC 9(6).
           03 MV-PRODUTO             PIC 9(7).
           03 MV-QUANTIDADE          PIC 9(7).
           03 MV-CUSTO               PIC 9(6)V99.
           03 MV-OPERADOR            PIC X(8).
           03 MV-TERMINAL            PIC 99.
       FD  ARQ-PARAM RECORD CONTAINS 64 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REG-PARAM.
           03 PA-KEY.
              05 PA-CHAVE            PIC 99.
           03 PA-PROX-VENDA          PIC 9(8).
           03 PA-PROX-COMPRA         PIC 9(8).
           03 PA-PROX-MOV            PIC 9(6).
           03 PA-EMPRESA             PIC X(30).
           03 PA-ANO                 PIC 99.
           03 PA-VERSAO              PIC X(8).
       WORKING-STORAGE SECTION.
      *V24: FILE STATUS EM TODOS OS ARQUIVOS: UM ERRO DE ARQUIVO
      *NAO DERRUBA MAIS O PROGRAMA NO FECHAMENTO.
       01  WS-FS1              PIC XX VALUE "00".
       01  WS-FS2              PIC XX VALUE "00".
       01  WS-FS3              PIC XX VALUE "00".
       01  WS-FS4              PIC XX VALUE "00".
       01  WS-FS5              PIC XX VALUE "00".
       01  WS-FS6              PIC XX VALUE "00".
       01  WS-FS7              PIC XX VALUE "00".
       01  WS-DATA-DRIVE          PIC XX VALUE "B:".
       01  WS-CM-VALOR            PIC 9(13)V99.
       01  WS-CM-ENTRADA          PIC 9(13)V99.
       01 WS-I                      PIC 99.
       01 WS-N                      PIC 99.
       01 WS-OPCAO                  PIC X.
       01 WS-TOTAL                  PIC 9(8)V99.
       01 WS-VENCIMENTO             PIC 9(6).
       01 WS-ACHOU                  PIC X.
       01 WS-J                      PIC 99.
       01 WS-DUP                    PIC X.
       01 WS-ITENS.
          03 WT-ITEM OCCURS 20 TIMES.
             05 WT-PROD             PIC 9(7).
             05 WT-QTD              PIC 9(7).
             05 WT-CUSTO            PIC 9(6)V99.
             05 WT-TOTAL            PIC 9(8)V99.
             05 WT-DESCR            PIC X(30).
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
       01  TC-MSG               PIC X(76) VALUE SPACES.
       01  TC-MSG-R REDEFINES TC-MSG.
           03 TC-MSG-1          PIC X.
           03 FILLER            PIC X(75).
       01  TC-PROMPT            PIC X(40) VALUE SPACES.
       01  TC-RODAPE            PIC X(79) VALUE SPACES.
       01  TC-TRACO             PIC X(79) VALUE ALL "-".
       01  TC-FR-OK             PIC X VALUE "N".
       01  TC-K                 PIC 99.
       01  TC-INI               PIC 99.
       01  TC-LIN               PIC 99.
       01  TC-DTW               PIC 9(6).
       01  TC-DTW-R REDEFINES TC-DTW.
           03 TC-DTW-AA         PIC 99.
           03 TC-DTW-MM         PIC 99.
           03 TC-DTW-DD         PIC 99.
       01  TC-DT-ED.
           03 TC-DE-DD          PIC 99.
           03 FILLER            PIC X VALUE "/".
           03 TC-DE-MM          PIC 99.
           03 FILLER            PIC X VALUE "/".
           03 TC-DE-AA          PIC 99.
       01  TC-L1.
           03 FILLER            PIC X(33) VALUE
              " COMPRAS / ENTRADA DE MERCADORIA ".
           03 FILLER            PIC X(8) VALUE SPACES.
           03 FILLER            PIC X(7) VALUE "COMPRA ".
           03 TC1-NUM           PIC 9(8).
           03 FILLER            PIC X(8) VALUE SPACES.
           03 FILLER            PIC X(5) VALUE "DATA ".
           03 TC1-DATA          PIC X(8).
           03 FILLER            PIC XX VALUE SPACES.
       01  TC-L3.
           03 FILLER            PIC X(13) VALUE " FORNECEDOR: ".
           03 TC3-COD           PIC 9(5).
           03 FILLER            PIC XX VALUE SPACES.
           03 TC3-RAZAO         PIC X(38).
           03 FILLER            PIC XX VALUE SPACES.
           03 TC3-UF            PIC XX.
           03 FILLER            PIC XXX VALUE " - ".
           03 TC3-CIDADE        PIC X(14).
       01  TC-L4.
           03 FILLER            PIC X(13) VALUE " COND.PAGTO: ".
           03 TC4-COND          PIC ZZ9.
           03 FILLER            PIC X(22) VALUE
              " DIAS   PRAZO ENTREGA:".
           03 TC4-PRAZO         PIC ZZZ9.
           03 FILLER            PIC X(22) VALUE
              " DIAS   SALDO A PAGAR:".
           03 TC4-SALDO         PIC ZZ,ZZZ,ZZ9.99.
       01  TC-L5.
           03 FILLER            PIC X(13) VALUE " OPERADOR..: ".
           03 TC5-OPER          PIC X(8).
           03 FILLER            PIC X(13) VALUE "   TERMINAL: ".
           03 TC5-TERM          PIC 99.
           03 FILLER            PIC X(15) VALUE
              "   VENCIMENTO: ".
           03 TC5-VENC          PIC X(8).
       01  TC-L7.
           03 FILLER            PIC X(40) VALUE
              " IT  CODIGO   DESCRICAO                 ".
           03 FILLER            PIC X(39) VALUE
              "          QTD   CUSTO UN.         TOTAL".
       01  TC-L8.
           03 FILLER            PIC X(40) VALUE
              " --  -------  --------------------------".
           03 FILLER            PIC X(39) VALUE
              "----  -------  ----------  ------------".
       01  TC-IT.
           03 FILLER            PIC X VALUE SPACE.
           03 TCI-IT            PIC 99.
           03 FILLER            PIC XX VALUE SPACES.
           03 TCI-COD           PIC 9(7).
           03 FILLER            PIC XX VALUE SPACES.
           03 TCI-DESC          PIC X(30).
           03 FILLER            PIC XX VALUE SPACES.
           03 TCI-QTD           PIC ZZZZZZ9.
           03 FILLER            PIC XX VALUE SPACES.
           03 TCI-CUSTO         PIC ZZZ,ZZ9.99.
           03 FILLER            PIC XX VALUE SPACES.
           03 TCI-TOTAL         PIC Z,ZZZ,ZZ9.99.
       01  TC-L20A.
           03 FILLER            PIC X(45) VALUE SPACES.
           03 FILLER            PIC X(7) VALUE "ITENS: ".
           03 TC20-ITENS        PIC Z9.
           03 FILLER            PIC X(4) VALUE SPACES.
       01  TC-L20B.
           03 FILLER            PIC X(8) VALUE " TOTAL: ".
           03 TC20-TOTAL        PIC ZZ,ZZZ,ZZ9.99.
           03 FILLER            PIC X VALUE SPACE.
       01  TC-MSG-FAT.
           03 FILLER            PIC X(7) VALUE "COMPRA ".
           03 MC-NUM            PIC 9(8).
           03 FILLER            PIC X(32) VALUE
              " GRAVADA - TITULO A PAGAR GERADO".
       01  TC-MSG-PRO.
           03 MCP-DESC          PIC X(30).
           03 FILLER            PIC X(17) VALUE "  ESTOQUE ATUAL: ".
           03 MCP-EST           PIC ZZZZZZ9.
      *---- ENTRADA AMIGAVEL (V22)
       01  LC-ENT               PIC X(20) VALUE SPACES.
       01  LC-ENT-R REDEFINES LC-ENT.
           03 LC-C              PIC X OCCURS 20 TIMES.
       01  LC-DIGS              PIC X(10) VALUE "0123456789".
       01  LC-DIGS-R REDEFINES LC-DIGS.
           03 LC-DG             PIC X OCCURS 10 TIMES.
       01  LC-NUM               PIC 9(12) VALUE ZERO.
       01  LC-OK                PIC X VALUE "S".
       01  LC-BARRA             PIC X VALUE "N".
       01  LC-ND                PIC 99 VALUE ZERO.
       01  LC-I                 PIC 99 VALUE ZERO.
       01  LC-J                 PIC 99 VALUE ZERO.
       01  LC-D                 PIC 99 VALUE ZERO.
       01  LC-DATA              PIC 9(6) VALUE ZERO.
       01  LC-DATA-R REDEFINES LC-DATA.
           03 LC-AA             PIC 99.
           03 LC-MM             PIC 99.
           03 LC-DD             PIC 99.
       01  LC-DMY               PIC 9(6) VALUE ZERO.
       01  LC-DMY-R REDEFINES LC-DMY.
           03 LC-X1             PIC 99.
           03 LC-X2             PIC 99.
           03 LC-X3             PIC 99.
       01  LC-VAL               PIC 9(10)V99 VALUE ZERO.
       01  LC-SP                PIC 99 VALUE ZERO.
       01  LC-NDEC              PIC 99 VALUE ZERO.
       01  LC-DIAS              PIC 999 VALUE ZERO.
       01  LC-K                 PIC 999 VALUE ZERO.
       01  LC-FIMMES            PIC 99 VALUE ZERO.
       01  LC-TAB-MES           PIC X(24) VALUE
           "312931303130313130313031".
       01  LC-TAB-MES-R REDEFINES LC-TAB-MES.
           03 LC-UD             PIC 99 OCCURS 12 TIMES.
       01  LC-DT-ED.
           03 LC-E-DD           PIC 99.
           03 FILLER            PIC X VALUE "/".
           03 LC-E-MM           PIC 99.
           03 FILLER            PIC X VALUE "/".
           03 LC-E-AA           PIC 99.
      *---- PESQUISA POR LISTA (V22)
       01  PQ-PAG               PIC 99 VALUE 1.
       01  PQ-N                 PIC 99 VALUE ZERO.
       01  PQ-L                 PIC 99 VALUE ZERO.
       01  PQ-EOF               PIC X VALUE "N".
       01  PQ-FIM               PIC X VALUE "N".
       01  PQ-OK                PIC X VALUE "N".
       01  PQ-MAIS              PIC X VALUE "N".
       01  PQ-ED-PAG            PIC Z9.
       01  PQ-ED-LIN            PIC Z9.
       01  PQ-TAB-INI.
           03 PQ-INI            PIC X(12) OCCURS 60 TIMES.
       01  PQ-TAB-COD.
           03 PQ-COD            PIC X(12) OCCURS 15 TIMES.
       01  PQ-ROD1              PIC X(34) VALUE
           " = PROXIMA PAGINA   - ANTERIOR    ".
       01  PQ-ROD2              PIC X(44) VALUE
           " NUMERO DA LINHA=ESCOLHE    X=CANCELA      ".
       01  PQF-LIN.
           03 FILLER            PIC X VALUE SPACE.
           03 PQF-NL           PIC Z9.
           03 FILLER            PIC XX VALUE SPACES.
           03 PQF-C1               PIC 9(5).
           03 FILLER            PIC XX VALUE SPACES.
           03 PQF-C2               PIC X(38).
           03 FILLER            PIC XX VALUE SPACES.
           03 PQF-C3               PIC X(16).
           03 FILLER            PIC XX VALUE SPACES.
           03 PQF-C4               PIC XX.
           03 FILLER            PIC XX VALUE SPACES.
       01  PQF-TIT.
           03 FILLER            PIC X(50) VALUE
              "PESQUISA DE FORNECEDORES                          ".
           03 FILLER            PIC X(8) VALUE "PAGINA ".
           03 PQF-TP           PIC Z9.
       01  PQF-CAB.
           03 FILLER            PIC X(40) VALUE
              " LIN  CODIGO RAZAO SOCIAL               ".
           03 FILLER            PIC X(32) VALUE
              "             CIDADE           UF".
       01  PQP-LIN.
           03 FILLER            PIC X VALUE SPACE.
           03 PQP-NL           PIC Z9.
           03 FILLER            PIC XX VALUE SPACES.
           03 PQP-C1               PIC 9(7).
           03 FILLER            PIC XX VALUE SPACES.
           03 PQP-C2               PIC X(30).
           03 FILLER            PIC XX VALUE SPACES.
           03 PQP-C3               PIC ZZZZZZ9.
           03 FILLER            PIC XX VALUE SPACES.
           03 PQP-C4               PIC ZZZ,ZZ9.99.
           03 FILLER            PIC XX VALUE SPACES.
       01  PQP-TIT.
           03 FILLER            PIC X(50) VALUE
              "PESQUISA DE PRODUTOS                              ".
           03 FILLER            PIC X(8) VALUE "PAGINA ".
           03 PQP-TP           PIC Z9.
       01  PQP-CAB.
           03 FILLER            PIC X(40) VALUE
              " LIN  CODIGO   DESCRICAO                ".
           03 FILLER            PIC X(24) VALUE
              "       ESTOQUE ULT.CUSTO".
       01  TC-ROD1              PIC X(34) VALUE SPACES.
       01  TC-ROD2              PIC X(44) VALUE SPACES.
       01  WS-DATA-ULT          PIC 9(6) VALUE 841215.
       01  WS-OPER-ULT          PIC X(8) VALUE "DEMO".
       01  WS-TERM-ULT          PIC 99 VALUE 01.
       01  WS-PF-FIM            PIC X VALUE "N".
       01  WS-SUGQ              PIC S9(8) VALUE ZERO.
       01  WS-PR-DATA.
           03 FILLER            PIC X(16) VALUE "DATA DA COMPRA [".
           03 WP-DATA           PIC X(8).
           03 FILLER            PIC X(2) VALUE "]:".
       01  WS-PR-VENC.
           03 FILLER            PIC X(22) VALUE
              "VENCIMENTO DO TITULO [".
           03 WP-VENC           PIC X(8).
           03 FILLER            PIC X(2) VALUE "]:".
       01  WS-PR-OPER.
           03 FILLER            PIC X(10) VALUE "OPERADOR [".
           03 WP-OPER           PIC X(8).
           03 FILLER            PIC X(2) VALUE "]:".
       01  WS-PR-TERM.
           03 FILLER            PIC X(10) VALUE "TERMINAL [".
           03 WP-TERM           PIC 99.
           03 FILLER            PIC X(2) VALUE "]:".
       01  WS-PR-QTD.
           03 FILLER            PIC X(12) VALUE "QUANTIDADE [".
           03 WP-QTD            PIC Z(6)9.
           03 FILLER            PIC X(2) VALUE "]:".
       01  WS-PR-CUSTO.
           03 FILLER            PIC X(16) VALUE "CUSTO UNITARIO [".
           03 WP-CUSTO          PIC ZZZ,ZZ9.99.
           03 FILLER            PIC X(2) VALUE "]:".
       01  WS-MSG-NF.
           03 FILLER            PIC X(15) VALUE "*** FORNECEDOR ".
           03 WM-FOR            PIC 9(5).
           03 FILLER            PIC X(34) VALUE
              " NAO CADASTRADO - USE ? PARA LISTA".
           03 FILLER            PIC X(4) VALUE " ***".
       01  WS-MSG-NP.
           03 FILLER            PIC X(12) VALUE "*** PRODUTO ".
           03 WM-PRO            PIC 9(7).
           03 FILLER            PIC X(34) VALUE
              " NAO CADASTRADO - USE ? PARA LISTA".
           03 FILLER            PIC X(4) VALUE " ***".
       PROCEDURE DIVISION.
       INICIO.
           OPEN I-O ARQ-COMPRA.
           OPEN I-O ARQ-ICOMP.
           OPEN I-O ARQ-FORNEC.
           OPEN I-O ARQ-PROD.
           OPEN I-O ARQ-MOV.
           MOVE SPACE TO WS-OPCAO.
           PERFORM NOVA-COMPRA THRU COMPRA-FIM UNTIL WS-OPCAO = "X".
           CLOSE ARQ-COMPRA.
           CLOSE ARQ-ICOMP.
           CLOSE ARQ-FORNEC.
           CLOSE ARQ-PROD.
           CLOSE ARQ-MOV.
           GO TO FIM-PROGRAMA.
       NOVA-COMPRA.
           MOVE ZERO TO WS-N WS-TOTAL.
           MOVE ZERO TO CP-NUMERO CP-DATA WS-VENCIMENTO.
           MOVE "N" TO TC-FR-OK.
           MOVE SPACES TO TC-MSG.
           MOVE " RETURN ACEITA O VALOR SUGERIDO  " TO TC-ROD1.
           MOVE " INFORME OS DADOS DA COMPRA" TO TC-ROD2.
           PERFORM TC-MOLDURA.
           OPEN I-O ARQ-PARAM.
           MOVE 01 TO PA-CHAVE.
           MOVE "S" TO WS-ACHOU.
           READ ARQ-PARAM
              INVALID KEY MOVE "N" TO WS-ACHOU.
           IF WS-ACHOU = "N"
              CLOSE ARQ-PARAM
              MOVE "*** PARAMETROS NAO ENCONTRADOS ***" TO TC-MSG
              GO TO COMPRA-FIM.
           MOVE PA-PROX-COMPRA TO CP-NUMERO.
           ADD 1 TO PA-PROX-COMPRA.
           REWRITE REG-PARAM
              INVALID KEY MOVE "N" TO WS-ACHOU.
           CLOSE ARQ-PARAM.
           IF WS-ACHOU = "N"
              MOVE "*** ERRO RESERVANDO NUMERO ***" TO TC-MSG
              GO TO COMPRA-FIM.
           PERFORM TC-LINHA-1.
           MOVE WS-DATA-ULT TO LC-DATA.
           PERFORM LC-FMT.
           MOVE LC-DT-ED TO WP-DATA.
           MOVE WS-PR-DATA TO TC-PROMPT.
           PERFORM TC-PERGUNTA.
           PERFORM LC-LE-DATA.
           MOVE LC-DATA TO CP-DATA.
           MOVE LC-DATA TO WS-DATA-ULT.
           PERFORM TC-LINHA-1.
           MOVE " DIGITE O CODIGO SEM OS ZEROS    " TO TC-ROD1.
           MOVE " ? = LISTA DE FORNECEDORES   X = CANCELA" TO TC-ROD2.
           PERFORM TC-RODAPE-P.
           MOVE "N" TO TC-FR-OK.
           MOVE "N" TO WS-PF-FIM.
           PERFORM PEDE-FORN THRU PF-FIM UNTIL WS-PF-FIM = "S".
           IF TC-FR-OK NOT = "S"
              MOVE "COMPRA CANCELADA" TO TC-MSG
              GO TO COMPRA-FIM.
           MOVE " RETURN ACEITA O VALOR SUGERIDO  " TO TC-ROD1.
           MOVE " OU DIGITE OUTRO VALOR" TO TC-ROD2.
           PERFORM TC-RODAPE-P.
           MOVE WS-OPER-ULT TO CP-OPERADOR.
           MOVE WS-TERM-ULT TO CP-TERMINAL.
           PERFORM TC-LINHA-5.
           MOVE WS-OPER-ULT TO WP-OPER.
           MOVE WS-PR-OPER TO TC-PROMPT.
           PERFORM TC-PERGUNTA.
           MOVE SPACES TO LC-ENT.
           ACCEPT LC-ENT.
           IF LC-ENT NOT = SPACES MOVE LC-ENT TO CP-OPERADOR.
           MOVE CP-OPERADOR TO WS-OPER-ULT.
           PERFORM TC-LINHA-5.
           MOVE WS-TERM-ULT TO WP-TERM.
           MOVE WS-PR-TERM TO TC-PROMPT.
           PERFORM TC-PERGUNTA.
           PERFORM LC-LE-CODIGO.
           IF LC-ENT NOT = SPACES
              IF LC-OK = "S"
                 MOVE LC-NUM TO CP-TERMINAL.
           MOVE CP-TERMINAL TO WS-TERM-ULT.
           MOVE CP-DATA TO LC-DATA.
           MOVE FR-COND-PAGTO TO LC-DIAS.
           PERFORM LC-SOMA-DIAS.
           MOVE LC-DATA TO WS-VENCIMENTO.
           PERFORM TC-LINHA-5.
           PERFORM LC-FMT.
           MOVE LC-DT-ED TO WP-VENC.
           MOVE WS-PR-VENC TO TC-PROMPT.
           PERFORM TC-PERGUNTA.
           PERFORM LC-LE-DATA.
           MOVE LC-DATA TO WS-VENCIMENTO.
           PERFORM TC-LINHA-5.
           MOVE " RETURN SEM CODIGO = FIM DOS ITENS" TO TC-ROD1.
           MOVE " ? = LISTA DE PRODUTOS    ATE 20 ITENS" TO TC-ROD2.
           PERFORM TC-RODAPE-P.
           MOVE "S" TO WS-OPCAO.
           PERFORM DIGITA-ITEM THROUGH DI-FIM
              UNTIL WS-OPCAO = "F".
           IF WS-N = ZERO GO TO COMPRA-FIM.
           MOVE WS-N TO CP-QTD-ITENS.
           MOVE WS-TOTAL TO CP-TOTAL.
           MOVE " S = GRAVA A COMPRA               " TO TC-ROD1.
           MOVE " N = CANCELA A COMPRA" TO TC-ROD2.
           PERFORM TC-RODAPE-P.
           PERFORM TC-MSG-P.
           MOVE "CONFIRMA A COMPRA (S/N)?" TO TC-PROMPT.
           PERFORM TC-PERGUNTA.
           ACCEPT WS-OPCAO.
           IF WS-OPCAO = "s" MOVE "C" TO WS-OPCAO.
           IF WS-OPCAO = "S" MOVE "C" TO WS-OPCAO.
           IF WS-OPCAO = "c" MOVE "C" TO WS-OPCAO.
           IF WS-OPCAO NOT = "C"
              MOVE "COMPRA CANCELADA" TO TC-MSG.
           IF WS-OPCAO = "C"
              PERFORM GRAVA-COMPRA THROUGH GC-FIM.
       COMPRA-FIM.
           IF TC-FR-OK = "S" PERFORM TC-LINHAS-FORN.
           MOVE " RETURN OU S = NOVA COMPRA        " TO TC-ROD1.
           MOVE " N = VOLTA AO MENU PRINCIPAL" TO TC-ROD2.
           PERFORM TC-RODAPE-P.
           PERFORM TC-MSG-P.
           MOVE "OUTRA COMPRA (S/N)?" TO TC-PROMPT.
           PERFORM TC-PERGUNTA.
           ACCEPT WS-OPCAO.
           IF WS-OPCAO = "n" MOVE "X" TO WS-OPCAO.
           IF WS-OPCAO = "N" MOVE "X" TO WS-OPCAO.
           IF WS-OPCAO = "x" MOVE "X" TO WS-OPCAO.
           IF WS-OPCAO NOT = "X" MOVE "S" TO WS-OPCAO.
       DIGITA-ITEM.
           PERFORM TC-MSG-P.
           MOVE "PRODUTO (? = LISTA, RETURN = FIM):" TO TC-PROMPT.
           PERFORM TC-PERGUNTA.
           MOVE SPACES TO TC-MSG.
           PERFORM LC-LE-CODIGO.
           IF LC-C (1) = "?"
              PERFORM PQP-PESQUISA
              PERFORM TC-REDESENHA
           ELSE
              MOVE LC-NUM TO PR-CODIGO.
           IF LC-C (1) = "?"
              IF PQ-OK = "N" GO TO DI-FIM.
           IF LC-OK = "N"
              IF LC-C (1) NOT = "?"
                 MOVE "*** CODIGO INVALIDO ***" TO TC-MSG
                 GO TO DI-FIM.
           IF PR-CODIGO = ZERO
              MOVE "F" TO WS-OPCAO
              GO TO DI-FIM.
           MOVE "S" TO WS-ACHOU.
           READ ARQ-PROD
              INVALID KEY MOVE "N" TO WS-ACHOU.
           IF WS-ACHOU = "N"
              MOVE PR-CODIGO TO WM-PRO
              MOVE WS-MSG-NP TO TC-MSG
              GO TO DI-FIM.
           IF PR-SITUACAO = "B"
              MOVE "*** PRODUTO INATIVO ***" TO TC-MSG
              GO TO DI-FIM.
           MOVE "N" TO WS-DUP.
           MOVE 1 TO WS-J.
           PERFORM V14-01 UNTIL WS-J > WS-N.
           IF WS-DUP = "S"
              MOVE "*** PRODUTO JA INFORMADO NESTA COMPRA ***"
                TO TC-MSG
              GO TO DI-FIM.
           MOVE PR-DESCRICAO TO MCP-DESC.
           MOVE PR-ESTOQUE TO MCP-EST.
           MOVE TC-MSG-PRO TO TC-MSG.
           PERFORM TC-MSG-P.
           MOVE SPACES TO TC-MSG.
           ADD 1 TO WS-N.
           MOVE PR-CODIGO TO WT-PROD(WS-N).
           MOVE PR-DESCRICAO TO WT-DESCR(WS-N).
      *SUGERE A QUANTIDADE PARA COMPLETAR O ESTOQUE MAXIMO
           SUBTRACT PR-ESTOQUE FROM PR-MAXIMO GIVING WS-SUGQ.
           IF WS-SUGQ < 1 MOVE PR-MINIMO TO WS-SUGQ.
           IF WS-SUGQ < 1 MOVE 1 TO WS-SUGQ.
           MOVE WS-SUGQ TO WP-QTD.
           MOVE WS-PR-QTD TO TC-PROMPT.
           PERFORM TC-PERGUNTA.
           PERFORM LC-LE-CODIGO.
           IF LC-ENT = SPACES MOVE WS-SUGQ TO LC-NUM.
           IF LC-OK = "N" MOVE ZERO TO LC-NUM.
           MOVE LC-NUM TO WT-QTD(WS-N).
           IF WT-QTD(WS-N) = ZERO
              MOVE "*** QUANTIDADE INVALIDA ***" TO TC-MSG
              SUBTRACT 1 FROM WS-N
              GO TO DI-FIM.
           MOVE PR-ULT-CUSTO TO WP-CUSTO.
           MOVE WS-PR-CUSTO TO TC-PROMPT.
           PERFORM TC-PERGUNTA.
           PERFORM LC-LE-VALOR.
           IF LC-ENT = SPACES MOVE PR-ULT-CUSTO TO LC-VAL.
           IF LC-OK = "N" MOVE PR-ULT-CUSTO TO LC-VAL.
           MOVE LC-VAL TO WT-CUSTO(WS-N).
           MULTIPLY WT-QTD(WS-N) BY WT-CUSTO(WS-N)
              GIVING WT-TOTAL(WS-N).
           ADD WT-TOTAL(WS-N) TO WS-TOTAL.
           PERFORM TC-ITENS.
           PERFORM TC-TOTAL.
           IF WS-N = 20 MOVE "F" TO WS-OPCAO.
       DI-FIM.
           EXIT.
       VERIF-DUPLICADO.
           IF WT-PROD(WS-J) = PR-CODIGO
              MOVE "S" TO WS-DUP.
       GRAVA-COMPRA.
           MOVE "P" TO CP-SITUACAO.
           WRITE REG-COMPRA
              INVALID KEY MOVE "*** COMPRA JA EXISTENTE ***" TO TC-MSG
                          GO TO GC-FIM.
           MOVE 1 TO WS-I.
           PERFORM V14-02 UNTIL WS-I > WS-N.
           MOVE CP-FORNECEDOR TO FR-CODIGO.
           MOVE "S" TO WS-ACHOU.
           READ ARQ-FORNEC
              INVALID KEY MOVE "N" TO WS-ACHOU.
           IF WS-ACHOU = "N"
              DISPLAY "ERRO RELEITURA FORNECEDOR"
              GO TO GC-FIM.
           ADD CP-TOTAL TO FR-TOTAL-ANO
           ADD CP-TOTAL TO FR-SALDO.
           ADD 1 TO FR-TIT-ABERTOS.
           MOVE CP-DATA TO FR-ULT-COMPRA.
           REWRITE REG-FORNECEDOR
              INVALID KEY DISPLAY "ERRO ATUALIZANDO FORNECEDOR".
           OPEN I-O ARQ-PAGAR.
           MOVE CP-NUMERO TO PG-TITULO PG-COMPRA.
           MOVE 1 TO PG-PARCELA.
           MOVE CP-FORNECEDOR TO PG-FORNECEDOR.
           MOVE CP-DATA TO PG-EMISSAO.
           MOVE WS-VENCIMENTO TO PG-VENCIMENTO.
           MOVE CP-TOTAL TO PG-VALOR.
           MOVE ZERO TO PG-PAGO PG-DT-PAGTO PG-PORTADOR.
           MOVE "A" TO PG-SITUACAO.
           MOVE "S" TO WS-ACHOU.
           WRITE REG-PAGAR
              INVALID KEY MOVE "N" TO WS-ACHOU.
           CLOSE ARQ-PAGAR.
           IF WS-ACHOU = "N"
              MOVE "*** ERRO GERANDO TITULO A PAGAR ***" TO TC-MSG
              GO TO GC-FIM.
           MOVE "F" TO CP-SITUACAO.
           REWRITE REG-COMPRA
              INVALID KEY DISPLAY "ERRO FINALIZANDO COMPRA".
           MOVE CP-NUMERO TO MC-NUM.
           MOVE TC-MSG-FAT TO TC-MSG.
       GC-FIM.
           EXIT.
       GRAVA-ITEM.
           MOVE CP-NUMERO TO IC-COMPRA.
           MOVE WS-I TO IC-ITEM.
           MOVE WT-PROD(WS-I) TO IC-PRODUTO PR-CODIGO.
           MOVE WT-QTD(WS-I) TO IC-QUANTIDADE.
           MOVE WT-CUSTO(WS-I) TO IC-CUSTO.
           MOVE WT-TOTAL(WS-I) TO IC-TOTAL.
           WRITE REG-ITEM-COMPRA
              INVALID KEY DISPLAY "ERRO ITEM COMPRA".
           MOVE "S" TO WS-OPCAO.
           READ ARQ-PROD
              INVALID KEY MOVE "E" TO WS-OPCAO.
           IF WS-OPCAO = "E"
              DISPLAY "ERRO PRODUTO"
           ELSE
              MULTIPLY PR-ESTOQUE BY PR-CUSTO-MEDIO
                 GIVING WS-CM-VALOR
              MULTIPLY IC-QUANTIDADE BY IC-CUSTO
                 GIVING WS-CM-ENTRADA
              ADD WS-CM-ENTRADA TO WS-CM-VALOR
              ADD IC-QUANTIDADE TO PR-ESTOQUE
              DIVIDE PR-ESTOQUE INTO WS-CM-VALOR
                 GIVING PR-CUSTO-MEDIO
              MOVE IC-CUSTO TO PR-ULT-CUSTO
              MOVE CP-DATA TO PR-DT-ULT-ENT
              REWRITE REG-PRODUTO
                 INVALID KEY DISPLAY "ERRO ATUALIZANDO PRODUTO".
           MOVE "E" TO MV-TIPO.
           MOVE CP-NUMERO TO MV-DOCUMENTO.
           MOVE WS-I TO MV-ITEM.
           MOVE CP-DATA TO MV-DATA.
           MOVE IC-PRODUTO TO MV-PRODUTO.
           MOVE IC-QUANTIDADE TO MV-QUANTIDADE.
           MOVE IC-CUSTO TO MV-CUSTO.
           MOVE CP-OPERADOR TO MV-OPERADOR.
           MOVE CP-TERMINAL TO MV-TERMINAL.
           WRITE REG-MOV
              INVALID KEY DISPLAY "AVISO: MOVIMENTO NAO GRAVADO".

      *---- TELA VT100 DE COMPRAS (MESMO PADRAO DA VENDA)
       TC-MOLDURA.
           DISPLAY VT-LIMPA.
           PERFORM TC-LINHA-1.
           DISPLAY TC-TRACO.
           DISPLAY " FORNECEDOR: ".
           DISPLAY " COND.PAGTO: ".
           DISPLAY " OPERADOR..: ".
           DISPLAY TC-TRACO.
           DISPLAY VT-FORTE TC-L7 VT-NORMAL.
           DISPLAY TC-L8.
           MOVE 19 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS TC-TRACO.
           PERFORM TC-TOTAL.
           PERFORM TC-RODAPE-P.
       TC-LINHA-1.
           MOVE CP-NUMERO TO TC1-NUM.
           MOVE CP-DATA TO TC-DTW.
           PERFORM TC-FMT-DATA.
           MOVE TC-DT-ED TO TC1-DATA.
           IF CP-DATA = ZERO MOVE SPACES TO TC1-DATA.
           MOVE 1 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-INV TC-L1 VT-NORMAL.
       TC-LINHAS-FORN.
           MOVE FR-CODIGO TO TC3-COD.
           MOVE FR-RAZAO TO TC3-RAZAO.
           MOVE FR-UF TO TC3-UF.
           MOVE FR-CIDADE TO TC3-CIDADE.
           MOVE 3 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA VT-FORTE TC-L3 VT-NORMAL.
           MOVE FR-COND-PAGTO TO TC4-COND.
           MOVE FR-PRAZO-ENTREGA TO TC4-PRAZO.
           MOVE FR-SALDO TO TC4-SALDO.
           MOVE 4 TO VT-LIN.
           DISPLAY VT-POS VT-APAGA TC-L4.
       TC-LINHA-5.
           MOVE CP-OPERADOR TO TC5-OPER.
           MOVE CP-TERMINAL TO TC5-TERM.
           MOVE WS-VENCIMENTO TO TC-DTW.
           PERFORM TC-FMT-DATA.
           MOVE TC-DT-ED TO TC5-VENC.
           IF WS-VENCIMENTO = ZERO MOVE SPACES TO TC5-VENC.
           MOVE 5 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA TC-L5.
       TC-ITENS.
           MOVE 1 TO TC-INI.
           IF WS-N > 10
              SUBTRACT 9 FROM WS-N GIVING TC-INI.
           MOVE TC-INI TO TC-K.
           MOVE 1 TO TC-LIN.
           IF WS-N > 10
              PERFORM TC-ITEM UNTIL TC-LIN > 10
           ELSE
              MOVE WS-N TO TC-K
              MOVE WS-N TO TC-LIN
              PERFORM TC-ITEM.
       TC-ITEM.
           ADD 8 TC-LIN GIVING VT-LIN.
           MOVE 1 TO VT-COL.
           MOVE TC-K TO TCI-IT.
           MOVE WT-PROD(TC-K) TO TCI-COD.
           MOVE WT-DESCR(TC-K) TO TCI-DESC.
           MOVE WT-QTD(TC-K) TO TCI-QTD.
           MOVE WT-CUSTO(TC-K) TO TCI-CUSTO.
           MOVE WT-TOTAL(TC-K) TO TCI-TOTAL.
           DISPLAY VT-POS VT-APAGA TC-IT.
           ADD 1 TO TC-K.
           ADD 1 TO TC-LIN.
       TC-TOTAL.
           MOVE WS-N TO TC20-ITENS.
           MOVE WS-TOTAL TO TC20-TOTAL.
           MOVE 20 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA TC-L20A VT-INV VT-FORTE TC-L20B
                   VT-NORMAL.
       TC-MSG-P.
           MOVE 21 TO VT-LIN.
           MOVE 1 TO VT-COL.
           IF TC-MSG = SPACES
              DISPLAY VT-POS VT-APAGA
           ELSE
              IF TC-MSG-1 = "*"
                 DISPLAY VT-POS VT-APAGA VT-INV VT-PISCA " " TC-MSG
                         " " VT-NORMAL
              ELSE
                 IF TC-MSG-1 = "C"
                    DISPLAY VT-POS VT-APAGA VT-INV " " TC-MSG " "
                            VT-NORMAL
                 ELSE
                    DISPLAY VT-POS VT-APAGA " " TC-MSG.
       TC-PERGUNTA.
           MOVE 23 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA.
           MOVE 22 TO VT-LIN.
           DISPLAY VT-POS VT-APAGA VT-FORTE " " TC-PROMPT VT-NORMAL.
       TC-RODAPE-P.
           MOVE 24 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-INV TC-ROD1 VT-NORMAL " " VT-INV TC-ROD2
                   VT-NORMAL VT-HOME.
       TC-FMT-DATA.
           MOVE TC-DTW-DD TO TC-DE-DD.
           MOVE TC-DTW-MM TO TC-DE-MM.
           MOVE TC-DTW-AA TO TC-DE-AA.
      *---- FORNECEDOR (V22)
       PEDE-FORN.
           PERFORM TC-MSG-P.
           MOVE "CODIGO DO FORNECEDOR (? = LISTA):" TO TC-PROMPT.
           PERFORM TC-PERGUNTA.
           MOVE SPACES TO TC-MSG.
           PERFORM LC-LE-CODIGO.
           IF LC-C (1) = "x" MOVE "X" TO LC-C (1).
           IF LC-C (1) = "X"
              MOVE "S" TO WS-PF-FIM
              GO TO PF-FIM.
           IF LC-C (1) = "?"
              PERFORM PQF-PESQUISA
              PERFORM TC-REDESENHA
              IF PQ-OK = "N"
                 GO TO PF-FIM
              ELSE
                 MOVE FR-CODIGO TO CP-FORNECEDOR
                 GO TO PF-LER.
           IF LC-ENT = SPACES GO TO PF-FIM.
           IF LC-OK = "N"
              MOVE "*** CODIGO INVALIDO ***" TO TC-MSG
              GO TO PF-FIM.
           MOVE LC-NUM TO CP-FORNECEDOR.
       PF-LER.
           MOVE CP-FORNECEDOR TO FR-CODIGO.
           MOVE "S" TO WS-ACHOU.
           READ ARQ-FORNEC
              INVALID KEY MOVE "N" TO WS-ACHOU.
           IF WS-ACHOU = "N"
              MOVE CP-FORNECEDOR TO WM-FOR
              MOVE WS-MSG-NF TO TC-MSG
              GO TO PF-FIM.
           IF FR-SITUACAO = "B"
              MOVE "*** FORNECEDOR INATIVO - ESCOLHA OUTRO ***"
                TO TC-MSG
              GO TO PF-FIM.
           MOVE "S" TO TC-FR-OK.
           MOVE "S" TO WS-PF-FIM.
           PERFORM TC-LINHAS-FORN.
       PF-FIM.
           EXIT.
       TC-REDESENHA.
           PERFORM TC-MOLDURA.
           IF TC-FR-OK = "S" PERFORM TC-LINHAS-FORN.
           PERFORM TC-LINHA-5.
           MOVE 1 TO TC-INI.
           IF WS-N > 10
              SUBTRACT 9 FROM WS-N GIVING TC-INI.
           MOVE TC-INI TO TC-K.
           MOVE 1 TO TC-LIN.
           PERFORM TC-ITEM UNTIL TC-K > WS-N.
           PERFORM TC-TOTAL.
      *---- PESQUISA: PESQUISA DE FORNECEDORES
      *SAIDA: PQ-OK = "S" E A CHAVE ESCOLHIDA EM FR-CHAVE.
       PQF-PESQUISA.
           MOVE "N" TO PQ-OK.
           MOVE "N" TO PQ-FIM.
           MOVE 1 TO PQ-PAG.
           MOVE LOW-VALUES TO FR-CHAVE.
           MOVE FR-CHAVE TO PQ-INI (1).
           PERFORM PQF-TELA THRU PQF-TELA-FIM UNTIL PQ-FIM = "S".
       PQF-TELA.
           MOVE PQ-INI (PQ-PAG) TO FR-CHAVE.
           MOVE "N" TO PQ-EOF.
           IF PQ-PAG = 1
              START ARQ-FORNEC KEY IS NOT < FR-CHAVE
                 INVALID KEY MOVE "S" TO PQ-EOF
           ELSE
              START ARQ-FORNEC KEY IS GREATER THAN FR-CHAVE
                 INVALID KEY MOVE "S" TO PQ-EOF.
           MOVE PQ-PAG TO PQF-TP.
           DISPLAY VT-LIMPA VT-INV " " PQF-TIT "                 "
                   VT-NORMAL.
           DISPLAY "-------------------------------------------"
                   "------------------------------------".
           DISPLAY VT-FORTE PQF-CAB VT-NORMAL.
           DISPLAY "-------------------------------------------"
                   "------------------------------------".
           MOVE ZERO TO PQ-N.
           MOVE ZERO TO PQ-L.
           PERFORM PQF-LINHA UNTIL PQ-L NOT < 15.
           MOVE "N" TO PQ-MAIS.
           IF PQ-EOF = "N"
              READ ARQ-FORNEC NEXT RECORD
                 AT END MOVE "S" TO PQ-EOF.
           IF PQ-EOF = "N"
              MOVE "S" TO PQ-MAIS.
           MOVE 24 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-INV PQ-ROD1 VT-NORMAL " " VT-INV PQ-ROD2
                   VT-NORMAL VT-HOME.
           MOVE 21 TO VT-LIN.
           IF PQ-MAIS = "N"
              DISPLAY VT-POS VT-APAGA " " VT-FORTE "FIM DA LISTA"
                      VT-NORMAL.
           MOVE 23 TO VT-LIN.
           DISPLAY VT-POS VT-APAGA.
           MOVE 22 TO VT-LIN.
           DISPLAY VT-POS VT-APAGA VT-FORTE
                   " LINHA ESCOLHIDA (OU =, -, X):" VT-NORMAL.
           PERFORM LC-LE-CODIGO.
           IF LC-C (1) = "="
              IF LC-C (2) = SPACE
                 MOVE SPACES TO LC-ENT.
           IF LC-ENT = SPACES
              IF PQ-MAIS = "S"
                 IF PQ-PAG < 60
                    ADD 1 TO PQ-PAG
                    MOVE PQ-COD (15) TO PQ-INI (PQ-PAG).
           IF LC-ENT = SPACES GO TO PQF-TELA-FIM.
           IF LC-C (1) = "-"
              IF PQ-PAG > 1
                 SUBTRACT 1 FROM PQ-PAG.
           IF LC-C (1) = "-" GO TO PQF-TELA-FIM.
           IF LC-C (1) = "x" MOVE "X" TO LC-C (1).
           IF LC-C (1) = "X"
              MOVE "S" TO PQ-FIM
              GO TO PQF-TELA-FIM.
           IF LC-OK = "S"
              IF LC-NUM > ZERO
                 IF LC-NUM NOT > PQ-N
                    MOVE LC-NUM TO PQ-L
                    MOVE PQ-COD (PQ-L) TO FR-CHAVE
                    MOVE "S" TO PQ-OK
                    MOVE "S" TO PQ-FIM.
       PQF-TELA-FIM.
           EXIT.
       PQF-LINHA.
           ADD 1 TO PQ-L.
           IF PQ-EOF = "N"
              READ ARQ-FORNEC NEXT RECORD
                 AT END MOVE "S" TO PQ-EOF.
           IF PQ-EOF = "N"
              ADD 1 TO PQ-N
              MOVE FR-CHAVE TO PQ-COD (PQ-N)
              MOVE PQ-N TO PQF-NL
              MOVE FR-CODIGO TO PQF-C1
              MOVE FR-RAZAO TO PQF-C2
              MOVE FR-CIDADE TO PQF-C3
              MOVE FR-UF TO PQF-C4
              DISPLAY PQF-LIN.
      *---- PESQUISA: PESQUISA DE PRODUTOS
      *SAIDA: PQ-OK = "S" E A CHAVE ESCOLHIDA EM PR-CHAVE.
       PQP-PESQUISA.
           MOVE "N" TO PQ-OK.
           MOVE "N" TO PQ-FIM.
           MOVE 1 TO PQ-PAG.
           MOVE LOW-VALUES TO PR-CHAVE.
           MOVE PR-CHAVE TO PQ-INI (1).
           PERFORM PQP-TELA THRU PQP-TELA-FIM UNTIL PQ-FIM = "S".
       PQP-TELA.
           MOVE PQ-INI (PQ-PAG) TO PR-CHAVE.
           MOVE "N" TO PQ-EOF.
           IF PQ-PAG = 1
              START ARQ-PROD KEY IS NOT < PR-CHAVE
                 INVALID KEY MOVE "S" TO PQ-EOF
           ELSE
              START ARQ-PROD KEY IS GREATER THAN PR-CHAVE
                 INVALID KEY MOVE "S" TO PQ-EOF.
           MOVE PQ-PAG TO PQP-TP.
           DISPLAY VT-LIMPA VT-INV " " PQP-TIT "                 "
                   VT-NORMAL.
           DISPLAY "-------------------------------------------"
                   "------------------------------------".
           DISPLAY VT-FORTE PQP-CAB VT-NORMAL.
           DISPLAY "-------------------------------------------"
                   "------------------------------------".
           MOVE ZERO TO PQ-N.
           MOVE ZERO TO PQ-L.
           PERFORM PQP-LINHA UNTIL PQ-L NOT < 15.
           MOVE "N" TO PQ-MAIS.
           IF PQ-EOF = "N"
              READ ARQ-PROD NEXT RECORD
                 AT END MOVE "S" TO PQ-EOF.
           IF PQ-EOF = "N"
              MOVE "S" TO PQ-MAIS.
           MOVE 24 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-INV PQ-ROD1 VT-NORMAL " " VT-INV PQ-ROD2
                   VT-NORMAL VT-HOME.
           MOVE 21 TO VT-LIN.
           IF PQ-MAIS = "N"
              DISPLAY VT-POS VT-APAGA " " VT-FORTE "FIM DA LISTA"
                      VT-NORMAL.
           MOVE 23 TO VT-LIN.
           DISPLAY VT-POS VT-APAGA.
           MOVE 22 TO VT-LIN.
           DISPLAY VT-POS VT-APAGA VT-FORTE
                   " LINHA ESCOLHIDA (OU =, -, X):" VT-NORMAL.
           PERFORM LC-LE-CODIGO.
           IF LC-C (1) = "="
              IF LC-C (2) = SPACE
                 MOVE SPACES TO LC-ENT.
           IF LC-ENT = SPACES
              IF PQ-MAIS = "S"
                 IF PQ-PAG < 60
                    ADD 1 TO PQ-PAG
                    MOVE PQ-COD (15) TO PQ-INI (PQ-PAG).
           IF LC-ENT = SPACES GO TO PQP-TELA-FIM.
           IF LC-C (1) = "-"
              IF PQ-PAG > 1
                 SUBTRACT 1 FROM PQ-PAG.
           IF LC-C (1) = "-" GO TO PQP-TELA-FIM.
           IF LC-C (1) = "x" MOVE "X" TO LC-C (1).
           IF LC-C (1) = "X"
              MOVE "S" TO PQ-FIM
              GO TO PQP-TELA-FIM.
           IF LC-OK = "S"
              IF LC-NUM > ZERO
                 IF LC-NUM NOT > PQ-N
                    MOVE LC-NUM TO PQ-L
                    MOVE PQ-COD (PQ-L) TO PR-CHAVE
                    MOVE "S" TO PQ-OK
                    MOVE "S" TO PQ-FIM.
       PQP-TELA-FIM.
           EXIT.
       PQP-LINHA.
           ADD 1 TO PQ-L.
           IF PQ-EOF = "N"
              READ ARQ-PROD NEXT RECORD
                 AT END MOVE "S" TO PQ-EOF.
           IF PQ-EOF = "N"
              ADD 1 TO PQ-N
              MOVE PR-CHAVE TO PQ-COD (PQ-N)
              MOVE PQ-N TO PQP-NL
              MOVE PR-CODIGO TO PQP-C1
              MOVE PR-DESCRICAO TO PQP-C2
              MOVE PR-ESTOQUE TO PQP-C3
              MOVE PR-ULT-CUSTO TO PQP-C4
              DISPLAY PQP-LIN.
       LC-LE-CODIGO.
           MOVE SPACES TO LC-ENT.
           ACCEPT LC-ENT.
           PERFORM LC-CONVERTE.
       LC-CONVERTE.
           MOVE ZERO TO LC-NUM.
           MOVE ZERO TO LC-ND.
           MOVE "S" TO LC-OK.
           MOVE "N" TO LC-BARRA.
           MOVE 1 TO LC-I.
           PERFORM LC-DIG UNTIL LC-I > 20.
       LC-DIG.
           IF LC-C (LC-I) NOT = SPACE
              IF LC-C (LC-I) = "/"
                 MOVE "S" TO LC-BARRA
              ELSE
                 MOVE ZERO TO LC-J
                 MOVE 99 TO LC-D
                 PERFORM LC-PROCURA UNTIL LC-J > 9
                 IF LC-D = 99
                    MOVE "N" TO LC-OK
                 ELSE
                    MULTIPLY 10 BY LC-NUM
                    ADD LC-D TO LC-NUM
                    ADD 1 TO LC-ND.
           ADD 1 TO LC-I.
       LC-PROCURA.
           ADD 1 TO LC-J.
           IF LC-DG (LC-J) = LC-C (LC-I)
              SUBTRACT 1 FROM LC-J GIVING LC-D
              MOVE 10 TO LC-J.
      *---- LC-LE-VALOR: VALOR COM DECIMAIS (6.40  6,40  1.234,56)
      *O ULTIMO PONTO OU VIRGULA SEPARA OS DECIMAIS. SAIDA: LC-VAL
       LC-LE-VALOR.
           MOVE SPACES TO LC-ENT.
           ACCEPT LC-ENT.
           PERFORM LC-CONV-VALOR.
       LC-CONV-VALOR.
           MOVE ZERO TO LC-SP.
           MOVE 1 TO LC-I.
           PERFORM LC-ACHA-SEP UNTIL LC-I > 20.
           MOVE ZERO TO LC-NUM.
           MOVE ZERO TO LC-NDEC.
           MOVE "S" TO LC-OK.
           MOVE 1 TO LC-I.
           PERFORM LC-DIG-V UNTIL LC-I > 20.
           MOVE LC-NUM TO LC-VAL.
           PERFORM LC-DIV10 UNTIL LC-NDEC = ZERO.
       LC-ACHA-SEP.
           IF LC-C (LC-I) = "."
              MOVE LC-I TO LC-SP.
           IF LC-C (LC-I) = ","
              MOVE LC-I TO LC-SP.
           ADD 1 TO LC-I.
       LC-DIG-V.
           IF LC-C (LC-I) NOT = SPACE
              IF LC-C (LC-I) NOT = "."
                 IF LC-C (LC-I) NOT = ","
                    MOVE ZERO TO LC-J
                    MOVE 99 TO LC-D
                    PERFORM LC-PROCURA UNTIL LC-J > 9
                    IF LC-D = 99
                       MOVE "N" TO LC-OK
                    ELSE
                       MULTIPLY 10 BY LC-NUM
                       ADD LC-D TO LC-NUM
                       IF LC-SP > ZERO
                          IF LC-I > LC-SP
                             ADD 1 TO LC-NDEC.
           ADD 1 TO LC-I.
       LC-DIV10.
           DIVIDE 10 INTO LC-VAL.
           SUBTRACT 1 FROM LC-NDEC.
      *---- LC-LE-DATA: RETURN MANTEM LC-DATA; ACEITA DD/MM/AA OU AAMMDD
       LC-LE-DATA.
           MOVE SPACES TO LC-ENT.
           ACCEPT LC-ENT.
           IF LC-ENT NOT = SPACES
              PERFORM LC-CONVERTE
              IF LC-OK = "S"
                 IF LC-ND = 6
                    PERFORM LC-MONTA-DATA.
       LC-MONTA-DATA.
           MOVE LC-NUM TO LC-DMY.
           IF LC-BARRA = "S"
              MOVE LC-X3 TO LC-AA
              MOVE LC-X2 TO LC-MM
              MOVE LC-X1 TO LC-DD
           ELSE
              MOVE LC-X1 TO LC-AA
              MOVE LC-X2 TO LC-MM
              MOVE LC-X3 TO LC-DD.
           IF LC-MM < 1 MOVE 12 TO LC-MM.
           IF LC-MM > 12 MOVE 12 TO LC-MM.
           IF LC-DD < 1 MOVE 1 TO LC-DD.
           IF LC-DD > LC-UD (LC-MM) MOVE LC-UD (LC-MM) TO LC-DD.
      *---- LC-SOMA-DIAS: LC-DATA + LC-DIAS (ANOS 80, 84 BISSEXTO)
       LC-SOMA-DIAS.
           MOVE ZERO TO LC-K.
           PERFORM LC-UM-DIA UNTIL LC-K NOT < LC-DIAS.
       LC-UM-DIA.
           ADD 1 TO LC-K.
           MOVE LC-UD (LC-MM) TO LC-FIMMES.
           IF LC-MM = 2
              MOVE 28 TO LC-FIMMES
              IF LC-AA = 84
                 MOVE 29 TO LC-FIMMES
              ELSE
                 IF LC-AA = 88
                    MOVE 29 TO LC-FIMMES.
           ADD 1 TO LC-DD.
           IF LC-DD > LC-FIMMES
              MOVE 1 TO LC-DD
              ADD 1 TO LC-MM
              IF LC-MM > 12
                 MOVE 1 TO LC-MM
                 ADD 1 TO LC-AA.
       LC-FMT.
           MOVE LC-DD TO LC-E-DD.
           MOVE LC-MM TO LC-E-MM.
           MOVE LC-AA TO LC-E-AA.
       FIM-PROGRAMA.
           EXIT PROGRAM.
      *V14: LACOS (CIS COBOL NAO TEM PERFORM VARYING)
       V14-01.
           PERFORM VERIF-DUPLICADO.
           ADD 1 TO WS-J.
       V14-02.
           PERFORM GRAVA-ITEM.
           ADD 1 TO WS-I.
