       IDENTIFICATION DIVISION.
       PROGRAM-ID. RACVEN.
      *RETROLABS ACME - FONTES V23
       AUTHOR. JOSE LUIZ MARTINS - RETROLABS.
      *RETROLABS ACME - SISTEMA INTEGRADO COMERCIAL
      *AMBIENTE ALVO: CIS COBOL 4.5 / MP/M / ALTOS 5-15AD
      *VERSAO DOS FONTES: 0.1 - 1984 DEMO

      *V12: DADOS NO DRIVE B: / SEM LOCK (UM TERMINAL POR VEZ)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-VENDA ASSIGN TO "B:VEN.DAT"
               ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
               RECORD KEY IS VD-CHAVE
              FILE STATUS IS WS-FS1.
           SELECT ARQ-ITEM ASSIGN TO "B:ITVEN.DAT"
               ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
               RECORD KEY IS IV-CHAVE
              FILE STATUS IS WS-FS2.
           SELECT ARQ-CLIENTE ASSIGN TO "B:CLI.DAT"
               ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
               RECORD KEY IS CL-CHAVE
              FILE STATUS IS WS-FS3.
           SELECT ARQ-PROD ASSIGN TO "B:PROD.DAT"
               ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
               RECORD KEY IS PR-CHAVE
              FILE STATUS IS WS-FS4.
           SELECT ARQ-RECEBER ASSIGN TO "B:REC.DAT"
               ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
               RECORD KEY IS CR-CHAVE
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
       FD  ARQ-VENDA RECORD CONTAINS 77 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-VENDA.
          03 VD-CHAVE.
             05 VD-NUMERO              PIC 9(8).
          03 VD-DATA                PIC 9(6).
          03 VD-CLIENTE             PIC 9(6).
          03 VD-VENDEDOR            PIC 9(3).
          03 VD-OPERADOR            PIC X(8).
          03 VD-TERMINAL            PIC 99.
          03 VD-COND-PAGTO          PIC 9(3).
          03 VD-QTD-ITENS           PIC 9(3).
          03 VD-VLR-MERC            PIC 9(8)V99.
          03 VD-DESCONTO            PIC 9(7)V99.
          03 VD-FRETE               PIC 9(6)V99.
          03 VD-TOTAL               PIC 9(8)V99.
          03 VD-SITUACAO            PIC X.
       FD  ARQ-ITEM RECORD CONTAINS 45 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-ITEM-VENDA.
          03 IV-CHAVE.
             05 IV-VENDA            PIC 9(8).
             05 IV-ITEM             PIC 9(3).
          03 IV-PRODUTO             PIC 9(7).
          03 IV-QUANTIDADE          PIC 9(6).
          03 IV-PRECO               PIC 9(6)V99.
          03 IV-DESCONTO            PIC 99V9.
          03 IV-TOTAL               PIC 9(8)V99.
       FD  ARQ-CLIENTE RECORD CONTAINS 341 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-CLIENTE.
          03 CL-CHAVE.
             05 CL-CODIGO              PIC 9(6).
          03 CL-RAZAO               PIC X(38).
          03 CL-FANTASIA            PIC X(25).
          03 CL-CGC-MF              PIC X(18).
          03 CL-INSC-EST            PIC X(15).
          03 CL-ENDERECO            PIC X(35).
          03 CL-BAIRRO              PIC X(20).
          03 CL-CIDADE              PIC X(22).
          03 CL-UF                  PIC X(2).
          03 CL-CEP                 PIC 9(5).
          03 CL-DDD                 PIC 9(3).
          03 CL-TELEFONE            PIC X(10).
          03 CL-TELEX               PIC X(15).
          03 CL-CONTATO             PIC X(25).
          03 CL-CARGO               PIC X(20).
          03 CL-PRACA               PIC X(22).
          03 CL-COND-PAGTO          PIC 9(3).
          03 CL-VENDEDOR            PIC 9(3).
          03 CL-LIM-CREDITO         PIC 9(8)V99.
          03 CL-DESCONTO            PIC 99V9.
          03 CL-TOTAL-ANO           PIC 9(9)V99.
          03 CL-SALDO               PIC 9(8)V99.
          03 CL-ULT-COMPRA          PIC 9(6).
          03 CL-TIT-ABERTOS         PIC 9(4).
          03 CL-MAIOR-ATRASO        PIC 9(3).
          03 CL-DT-CADASTRO         PIC 9(6).
          03 CL-SITUACAO            PIC X.
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
       FD  ARQ-RECEBER RECORD CONTAINS 102 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-RECEBER.
          03 CR-CHAVE.
             05 CR-TITULO           PIC 9(8).
             05 CR-PARCELA          PIC 99.
          03 CR-CLIENTE             PIC 9(6).
          03 CR-VENDA               PIC 9(8).
          03 CR-FATURA              PIC 9(8).
          03 CR-EMISSAO             PIC 9(6).
          03 CR-VENCIMENTO          PIC 9(6).
          03 CR-VALOR               PIC 9(8)V99.
          03 CR-JUROS               PIC 9(6)V99.
          03 CR-DESCONTO            PIC 9(6)V99.
          03 CR-RECEBIDO            PIC 9(8)V99.
          03 CR-DT-PAGTO            PIC 9(6).
          03 CR-PORTADOR            PIC 9(3).
          03 CR-NOSSO-NUMERO        PIC X(12).
          03 CR-SITUACAO            PIC X.
       FD  ARQ-MOV RECORD CONTAINS 50 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-MOV.
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
       01 WS-OPCAO                  PIC X.
       01 WS-ACHOU                  PIC X.
       01 WS-I                      PIC 99.
       01 WS-NITENS                 PIC 99.
       01 WS-TOTAL                  PIC 9(8)V99.
       01 WS-SUBTOTAL               PIC 9(8)V99.
       01 WS-DISPONIVEL             PIC 9(7).
       01 WS-VENCIMENTO             PIC 9(6).
       01 WS-ERRO                   PIC X VALUE SPACE.
       01 WS-J                      PIC 99.
       01 WS-K                      PIC 99.
       01 WS-INI                    PIC 99.
       01 WS-LIN                    PIC 99.
       01 WS-CL-OK                  PIC X VALUE "N".
       01 WS-MSG                    PIC X(76) VALUE SPACES.
       01  WS-MSG-R REDEFINES WS-MSG.
           03 WS-MSG-1          PIC X.
           03 FILLER            PIC X(75).
       01 WS-PROMPT                 PIC X(40) VALUE SPACES.
       01 WS-LIVRE                  PIC 9(8)V99.
       01 WS-DTW                    PIC 9(6).
       01 WS-DTW-R REDEFINES WS-DTW.
          03 WS-DTW-AA              PIC 99.
          03 WS-DTW-MM              PIC 99.
          03 WS-DTW-DD              PIC 99.
       01 WS-DT-ED.
          03 WS-DE-DD               PIC 99.
          03 FILLER                 PIC X VALUE "/".
          03 WS-DE-MM               PIC 99.
          03 FILLER                 PIC X VALUE "/".
          03 WS-DE-AA               PIC 99.
      *---- LINHAS DA TELA DE VENDAS (80 COLUNAS, USA 79)
       01 TV-TRACO                  PIC X(79) VALUE ALL "-".
       01 TV-L1.
          03 FILLER                 PIC X(20) VALUE
             "FATURAMENTO / VENDAS".
          03 FILLER                 PIC X(20) VALUE SPACES.
          03 FILLER                 PIC X(7) VALUE "PEDIDO ".
          03 TV1-PEDIDO             PIC 9(8).
          03 FILLER                 PIC X(8) VALUE SPACES.
          03 FILLER                 PIC X(5) VALUE "DATA ".
          03 TV1-DATA               PIC X(8).
       01 TV-L3.
          03 FILLER                 PIC X(11) VALUE " CLIENTE : ".
          03 TV3-COD                PIC 9(6).
          03 FILLER                 PIC XX VALUE SPACES.
          03 TV3-RAZAO              PIC X(38).
          03 FILLER                 PIC XX VALUE SPACES.
          03 TV3-UF                 PIC XX.
          03 FILLER                 PIC XXX VALUE " - ".
          03 TV3-CIDADE             PIC X(16).
       01 TV-L4.
          03 FILLER                 PIC X(11) VALUE " LIMITE  : ".
          03 TV4-LIM                PIC ZZ,ZZZ,ZZ9.99.
          03 FILLER                 PIC X(10) VALUE "   SALDO: ".
          03 TV4-SALDO              PIC ZZ,ZZZ,ZZ9.99.
          03 FILLER                 PIC X(15) VALUE
             "   DISPONIVEL: ".
          03 TV4-LIVRE              PIC ZZ,ZZZ,ZZ9.99.
       01 TV-L5.
          03 FILLER                 PIC X(11) VALUE " OPERADOR: ".
          03 TV5-OPER               PIC X(8).
          03 FILLER                 PIC X(13) VALUE "   TERMINAL: ".
          03 TV5-TERM               PIC 99.
          03 FILLER                 PIC X(15) VALUE
             "   VENCIMENTO: ".
          03 TV5-VENC               PIC X(8).
          03 FILLER                 PIC X(9) VALUE "   COND: ".
          03 TV5-COND               PIC ZZ9.
          03 FILLER                 PIC X(5) VALUE " DIAS".
       01 TV-L7.
          03 FILLER                 PIC X(40) VALUE
             " IT  CODIGO   DESCRICAO                 ".
          03 FILLER                 PIC X(38) VALUE
             "         QTD   PRECO UN.         TOTAL".
       01 TV-L8.
          03 FILLER                 PIC X(40) VALUE
             " --  -------  --------------------------".
          03 FILLER                 PIC X(38) VALUE
             "----  ------  ----------  ------------".
       01 TV-IT.
          03 FILLER                 PIC X VALUE SPACE.
          03 TVI-IT                 PIC 99.
          03 FILLER                 PIC XX VALUE SPACES.
          03 TVI-COD                PIC 9(7).
          03 FILLER                 PIC XX VALUE SPACES.
          03 TVI-DESC               PIC X(30).
          03 FILLER                 PIC XX VALUE SPACES.
          03 TVI-QTD                PIC ZZZZZ9.
          03 FILLER                 PIC XX VALUE SPACES.
          03 TVI-PRECO              PIC ZZZ,ZZ9.99.
          03 FILLER                 PIC XX VALUE SPACES.
          03 TVI-TOTAL              PIC Z,ZZZ,ZZ9.99.
       01 TV-L20.
          03 FILLER                 PIC X(45) VALUE SPACES.
          03 FILLER                 PIC X(7) VALUE "ITENS: ".
          03 TV20-ITENS             PIC Z9.
          03 FILLER                 PIC X(11) VALUE "    TOTAL: ".
          03 TV20-TOTAL             PIC ZZ,ZZZ,ZZ9.99.
       01 WS-MSG-FAT.
          03 FILLER                 PIC X(7) VALUE "PEDIDO ".
          03 MF-PEDIDO              PIC 9(8).
          03 FILLER                 PIC X(22) VALUE
             " FATURADO - DUPLICATA ".
          03 MF-TITULO              PIC 9(8).
          03 FILLER                 PIC X VALUE "/".
          03 MF-PARCELA             PIC 99.
       01 ED-DISP                   PIC ZZZZZZ9.
       01 ED-PRECO                  PIC ZZZ,ZZ9.99.
       01 TV-L21.
          03 FILLER                 PIC X VALUE SPACE.
          03 TV21-MSG               PIC X(76).
       01 TV-L23.
          03 FILLER                 PIC X VALUE SPACE.
          03 TV23-PROMPT            PIC X(40).
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
       01  TV-RODAPE            PIC X(79) VALUE SPACES.
       01  WS-MSG-PRO.
           03 MP-DESC           PIC X(30).
           03 FILLER            PIC X(14) VALUE "  DISPONIVEL: ".
           03 MP-DISP           PIC ZZZZZZ9.
           03 FILLER            PIC X(9) VALUE "  PRECO: ".
           03 MP-PRECO          PIC ZZZ,ZZ9.99.
       01  TV-L20A.
           03 FILLER            PIC X(45) VALUE SPACES.
           03 FILLER            PIC X(7) VALUE "ITENS: ".
           03 TV20A-ITENS       PIC Z9.
           03 FILLER            PIC X(4) VALUE SPACES.
       01  TV-L20B.
           03 FILLER            PIC X(8) VALUE " TOTAL: ".
           03 TV20B-TOTAL       PIC ZZ,ZZZ,ZZ9.99.
           03 FILLER            PIC X VALUE SPACE.
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
       01  PQC-LIN.
           03 FILLER            PIC X VALUE SPACE.
           03 PQC-NL           PIC Z9.
           03 FILLER            PIC XX VALUE SPACES.
           03 PQC-C1               PIC 9(6).
           03 FILLER            PIC XX VALUE SPACES.
           03 PQC-C2               PIC X(38).
           03 FILLER            PIC XX VALUE SPACES.
           03 PQC-C3               PIC X(16).
           03 FILLER            PIC XX VALUE SPACES.
           03 PQC-C4               PIC XX.
           03 FILLER            PIC XX VALUE SPACES.
       01  PQC-TIT.
           03 FILLER            PIC X(50) VALUE
              "PESQUISA DE CLIENTES                              ".
           03 FILLER            PIC X(8) VALUE "PAGINA ".
           03 PQC-TP           PIC Z9.
       01  PQC-CAB.
           03 FILLER            PIC X(40) VALUE
              " LIN  CODIGO  RAZAO SOCIAL              ".
           03 FILLER            PIC X(33) VALUE
              "              CIDADE           UF".
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
           03 FILLER            PIC X(25) VALUE
              "       ESTOQUE      PRECO".
       01  TV-ROD1              PIC X(34) VALUE SPACES.
       01  TV-ROD2              PIC X(44) VALUE SPACES.
       01  WS-DATA-ULT          PIC 9(6) VALUE 841215.
       01  WS-OPER-ULT          PIC X(8) VALUE "DEMO".
       01  WS-TERM-ULT          PIC 99 VALUE 01.
       01  WS-PC-FIM            PIC X VALUE "N".
       01  WS-PR-DATA.
           03 FILLER            PIC X(16) VALUE "DATA DO PEDIDO [".
           03 WP-DATA           PIC X(8).
           03 FILLER            PIC X(2) VALUE "]:".
       01  WS-PR-VENC.
           03 FILLER            PIC X(25) VALUE
              "VENCIMENTO DA DUPLICATA [".
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
       01  WS-MSG-NC.
           03 FILLER            PIC X(12) VALUE "*** CLIENTE ".
           03 WM-CLI            PIC 9(6).
           03 FILLER            PIC X(34) VALUE
              " NAO CADASTRADO - USE ? PARA LISTA".
           03 FILLER            PIC X(4) VALUE " ***".
       01  WS-MSG-NP.
           03 FILLER            PIC X(12) VALUE "*** PRODUTO ".
           03 WM-PRO            PIC 9(7).
           03 FILLER            PIC X(34) VALUE
              " NAO CADASTRADO - USE ? PARA LISTA".
           03 FILLER            PIC X(4) VALUE " ***".
       01 WS-ITEM-TAB.
          03 WS-ITEM OCCURS 20 TIMES.
             05 WT-PRODUTO          PIC 9(7).
             05 WT-QTD              PIC 9(6).
             05 WT-PRECO            PIC 9(6)V99.
             05 WT-DESC             PIC 99V9.
             05 WT-TOTAL            PIC 9(8)V99.
             05 WT-DESCR            PIC X(30).
       PROCEDURE DIVISION.
       INICIO.
           OPEN I-O ARQ-VENDA.
           OPEN I-O ARQ-ITEM.
           OPEN I-O ARQ-CLIENTE.
           OPEN I-O ARQ-PROD.
           OPEN I-O ARQ-MOV.
           MOVE "S" TO WS-OPCAO.
           PERFORM NOVA-VENDA THRU VENDA-FIM UNTIL WS-OPCAO = "X".
           CLOSE ARQ-VENDA.
           CLOSE ARQ-ITEM.
           CLOSE ARQ-CLIENTE.
           CLOSE ARQ-PROD.
           CLOSE ARQ-MOV.
           GO TO FIM-PROGRAMA.
       NOVA-VENDA.
           MOVE ZERO TO WS-NITENS WS-TOTAL.
           MOVE "N" TO WS-CL-OK.
           MOVE SPACES TO WS-MSG.
           MOVE ZERO TO VD-NUMERO VD-DATA WS-VENCIMENTO.
           MOVE " RETURN ACEITA O VALOR SUGERIDO  " TO TV-ROD1.
           MOVE " INFORME OS DADOS DO PEDIDO" TO TV-ROD2.
           PERFORM MOLDURA.
           OPEN I-O ARQ-PARAM.
           MOVE 01 TO PA-CHAVE.
           MOVE "S" TO WS-ACHOU.
           READ ARQ-PARAM
              INVALID KEY MOVE "N" TO WS-ACHOU.
           IF WS-ACHOU = "N"
              CLOSE ARQ-PARAM
              MOVE "PARAMETROS NAO ENCONTRADOS" TO WS-MSG
              GO TO VENDA-FIM.
           MOVE PA-PROX-VENDA TO VD-NUMERO.
           ADD 1 TO PA-PROX-VENDA.
           REWRITE REG-PARAM
              INVALID KEY MOVE "N" TO WS-ACHOU.
           CLOSE ARQ-PARAM.
           IF WS-ACHOU = "N"
              MOVE "ERRO RESERVANDO NUMERO" TO WS-MSG
              GO TO VENDA-FIM.
           PERFORM LINHA-1.
      *---- DATA: SUGERE A ULTIMA USADA (15/12/84 NA DEMONSTRACAO)
           MOVE WS-DATA-ULT TO LC-DATA.
           PERFORM LC-FMT.
           MOVE LC-DT-ED TO WP-DATA.
           MOVE WS-PR-DATA TO WS-PROMPT.
           PERFORM PERGUNTA.
           PERFORM LC-LE-DATA.
           MOVE LC-DATA TO VD-DATA.
           MOVE LC-DATA TO WS-DATA-ULT.
           PERFORM LINHA-1.
      *---- CLIENTE: ACEITA SEM ZEROS, ? ABRE A LISTA, X CANCELA
           MOVE " DIGITE O CODIGO SEM OS ZEROS    " TO TV-ROD1.
           MOVE " ? = LISTA DE CLIENTES    X = CANCELA" TO TV-ROD2.
           PERFORM RODAPE.
           MOVE "N" TO WS-CL-OK.
           MOVE "N" TO WS-PC-FIM.
           PERFORM PEDE-CLIENTE THRU PC-FIM UNTIL WS-PC-FIM = "S".
           IF WS-CL-OK NOT = "S"
              MOVE "PEDIDO CANCELADO" TO WS-MSG
              GO TO VENDA-FIM.
           MOVE CL-VENDEDOR TO VD-VENDEDOR.
           MOVE CL-COND-PAGTO TO VD-COND-PAGTO.
           MOVE " RETURN ACEITA O VALOR SUGERIDO  " TO TV-ROD1.
           MOVE " OU DIGITE OUTRO VALOR" TO TV-ROD2.
           PERFORM RODAPE.
           MOVE WS-OPER-ULT TO VD-OPERADOR.
           MOVE WS-TERM-ULT TO VD-TERMINAL.
           PERFORM LINHA-5.
           MOVE WS-OPER-ULT TO WP-OPER.
           MOVE WS-PR-OPER TO WS-PROMPT.
           PERFORM PERGUNTA.
           MOVE SPACES TO LC-ENT.
           ACCEPT LC-ENT.
           IF LC-ENT NOT = SPACES MOVE LC-ENT TO VD-OPERADOR.
           MOVE VD-OPERADOR TO WS-OPER-ULT.
           PERFORM LINHA-5.
           MOVE WS-TERM-ULT TO WP-TERM.
           MOVE WS-PR-TERM TO WS-PROMPT.
           PERFORM PERGUNTA.
           PERFORM LC-LE-CODIGO.
           IF LC-ENT NOT = SPACES
              IF LC-OK = "S"
                 MOVE LC-NUM TO VD-TERMINAL.
           MOVE VD-TERMINAL TO WS-TERM-ULT.
      *---- VENCIMENTO: SUGERE DATA DO PEDIDO + PRAZO DO CLIENTE
           MOVE VD-DATA TO LC-DATA.
           MOVE CL-COND-PAGTO TO LC-DIAS.
           PERFORM LC-SOMA-DIAS.
           MOVE LC-DATA TO WS-VENCIMENTO.
           PERFORM LINHA-5.
           PERFORM LC-FMT.
           MOVE LC-DT-ED TO WP-VENC.
           MOVE WS-PR-VENC TO WS-PROMPT.
           PERFORM PERGUNTA.
           PERFORM LC-LE-DATA.
           MOVE LC-DATA TO WS-VENCIMENTO.
           PERFORM LINHA-5.
           MOVE " RETURN SEM CODIGO = FIM DOS ITENS" TO TV-ROD1.
           MOVE " ? = LISTA DE PRODUTOS    ATE 20 ITENS" TO TV-ROD2.
           PERFORM RODAPE.
           MOVE "S" TO WS-OPCAO.
           PERFORM DIGITA-ITEM THROUGH ITEM-FIM
              UNTIL WS-OPCAO = "F".
           IF WS-NITENS = ZERO GO TO VENDA-FIM.
           MOVE WS-NITENS TO VD-QTD-ITENS.
           MOVE WS-TOTAL TO VD-VLR-MERC VD-TOTAL.
           MOVE ZERO TO VD-DESCONTO VD-FRETE.
           MOVE " S = GRAVA O PEDIDO               " TO TV-ROD1.
           MOVE " N = CANCELA O PEDIDO" TO TV-ROD2.
           PERFORM RODAPE.
           PERFORM MOSTRA-MSG.
           MOVE "CONFIRMA O PEDIDO (S/N)?" TO WS-PROMPT.
           PERFORM PERGUNTA.
           ACCEPT WS-OPCAO.
           IF WS-OPCAO = "s" MOVE "C" TO WS-OPCAO.
           IF WS-OPCAO = "S" MOVE "C" TO WS-OPCAO.
           IF WS-OPCAO = "c" MOVE "C" TO WS-OPCAO.
           IF WS-OPCAO NOT = "C"
              MOVE "PEDIDO CANCELADO" TO WS-MSG.
           IF WS-OPCAO = "C"
              PERFORM GRAVAR-VENDA THROUGH GRAVA-FIM.
       VENDA-FIM.
           IF WS-CL-OK = "S" PERFORM LINHAS-CLIENTE.
           MOVE " RETURN OU S = NOVO PEDIDO        " TO TV-ROD1.
           MOVE " N = VOLTA AO MENU PRINCIPAL" TO TV-ROD2.
           PERFORM RODAPE.
           PERFORM MOSTRA-MSG.
           MOVE "OUTRO PEDIDO (S/N)?" TO WS-PROMPT.
           PERFORM PERGUNTA.
           ACCEPT WS-OPCAO.
           IF WS-OPCAO = "n" MOVE "X" TO WS-OPCAO.
           IF WS-OPCAO = "N" MOVE "X" TO WS-OPCAO.
           IF WS-OPCAO = "x" MOVE "X" TO WS-OPCAO.
           IF WS-OPCAO NOT = "X" MOVE "S" TO WS-OPCAO.
       DIGITA-ITEM.
           PERFORM MOSTRA-MSG.
           MOVE "PRODUTO (? = LISTA, RETURN = FIM):" TO WS-PROMPT.
           PERFORM PERGUNTA.
           MOVE SPACES TO WS-MSG.
           PERFORM LC-LE-CODIGO.
           IF LC-C (1) = "?"
              PERFORM PQP-PESQUISA
              PERFORM REDESENHA
           ELSE
              MOVE LC-NUM TO PR-CODIGO.
           IF LC-C (1) = "?"
              IF PQ-OK = "N" GO TO ITEM-FIM.
           IF LC-OK = "N"
              IF LC-C (1) NOT = "?"
                 MOVE "*** CODIGO INVALIDO ***" TO WS-MSG
                 GO TO ITEM-FIM.
           IF PR-CODIGO = ZERO
              MOVE "F" TO WS-OPCAO
              GO TO ITEM-FIM.
           MOVE "S" TO WS-ACHOU.
           READ ARQ-PROD
              INVALID KEY MOVE "N" TO WS-ACHOU.
           IF WS-ACHOU = "N"
              MOVE PR-CODIGO TO WM-PRO
              MOVE WS-MSG-NP TO WS-MSG
              GO TO ITEM-FIM.
           IF PR-SITUACAO = "B"
              MOVE "*** PRODUTO INATIVO ***" TO WS-MSG
              GO TO ITEM-FIM.
           MOVE SPACE TO WS-ERRO.
           MOVE 1 TO WS-J.
           PERFORM V14-01 UNTIL WS-J > WS-NITENS.
           IF WS-ERRO = "D"
              MOVE "*** PRODUTO JA INFORMADO NESTE PEDIDO ***"
                TO WS-MSG
              MOVE SPACE TO WS-ERRO
              GO TO ITEM-FIM.
           MOVE ZERO TO WS-DISPONIVEL.
           IF PR-ESTOQUE > PR-RESERVADO
              SUBTRACT PR-RESERVADO FROM PR-ESTOQUE
                 GIVING WS-DISPONIVEL.
           MOVE PR-DESCRICAO TO MP-DESC.
           MOVE WS-DISPONIVEL TO MP-DISP.
           MOVE PR-PRECO-VENDA TO MP-PRECO.
           MOVE WS-MSG-PRO TO WS-MSG.
           PERFORM MOSTRA-MSG.
           MOVE SPACES TO WS-MSG.
           ADD 1 TO WS-NITENS.
           MOVE PR-CODIGO TO WT-PRODUTO(WS-NITENS).
           MOVE PR-DESCRICAO TO WT-DESCR(WS-NITENS).
           MOVE "QUANTIDADE [1]:" TO WS-PROMPT.
           PERFORM PERGUNTA.
           PERFORM LC-LE-CODIGO.
           IF LC-ENT = SPACES MOVE 1 TO LC-NUM.
           IF LC-OK = "N" MOVE ZERO TO LC-NUM.
           MOVE LC-NUM TO WT-QTD(WS-NITENS).
           IF WT-QTD(WS-NITENS) = ZERO
              MOVE "*** QUANTIDADE INVALIDA ***" TO WS-MSG
              SUBTRACT 1 FROM WS-NITENS
              GO TO ITEM-FIM.
           IF WT-QTD(WS-NITENS) > WS-DISPONIVEL
              MOVE "*** ESTOQUE INSUFICIENTE ***" TO WS-MSG
              SUBTRACT 1 FROM WS-NITENS
              GO TO ITEM-FIM.
           MOVE PR-PRECO-VENDA TO WT-PRECO(WS-NITENS).
           MOVE ZERO TO WT-DESC(WS-NITENS).
           MULTIPLY WT-QTD(WS-NITENS) BY WT-PRECO(WS-NITENS)
              GIVING WT-TOTAL(WS-NITENS).
           ADD WT-TOTAL(WS-NITENS) TO WS-TOTAL.
           PERFORM MOSTRA-ITENS.
           PERFORM LINHA-TOTAL.
           IF WS-NITENS = 20 MOVE "F" TO WS-OPCAO.
       ITEM-FIM.
           EXIT.
       VERIF-DUPLICADO.
           IF WT-PRODUTO(WS-J) = PR-CODIGO
              MOVE "D" TO WS-ERRO.
       GRAVAR-VENDA.
      *V11: SEM LOCK. CLIENTE E PRODUTOS SAO RELIDOS NA GRAVACAO.
           MOVE VD-CLIENTE TO CL-CODIGO.
           READ ARQ-CLIENTE
              INVALID KEY DISPLAY "CLIENTE DESAPARECEU" GO TO GRAVA-FIM.
           ADD CL-SALDO VD-TOTAL GIVING WS-SUBTOTAL.
           IF WS-SUBTOTAL > CL-LIM-CREDITO
              MOVE "*** LIMITE DE CREDITO EXCEDIDO ***" TO WS-MSG
              GO TO GRAVA-FIM.
           MOVE "N" TO WS-ERRO.
           MOVE 1 TO WS-I.
           MOVE 1 TO WS-I.
           PERFORM V14-02 UNTIL WS-I > WS-NITENS.
           IF WS-ERRO = "S" GO TO GRAVA-FIM.
           MOVE "P" TO VD-SITUACAO.
           WRITE REG-VENDA
              INVALID KEY MOVE "*** PEDIDO JA EXISTENTE ***" TO WS-MSG
              GO TO GRAVA-FIM.
           MOVE 1 TO WS-I.
           PERFORM V14-03 UNTIL WS-I > WS-NITENS.
           ADD VD-TOTAL TO CL-TOTAL-ANO
           ADD VD-TOTAL TO CL-SALDO.
           MOVE VD-DATA TO CL-ULT-COMPRA.
           ADD 1 TO CL-TIT-ABERTOS.
           REWRITE REG-CLIENTE
              INVALID KEY DISPLAY "ERRO ATUALIZANDO CLIENTE".
           OPEN I-O ARQ-RECEBER.
           MOVE VD-NUMERO TO CR-TITULO CR-VENDA CR-FATURA.
           MOVE 1 TO CR-PARCELA.
           MOVE VD-CLIENTE TO CR-CLIENTE.
           MOVE VD-DATA TO CR-EMISSAO.
           MOVE WS-VENCIMENTO TO CR-VENCIMENTO.
           MOVE VD-TOTAL TO CR-VALOR.
           MOVE ZERO TO CR-JUROS CR-DESCONTO CR-RECEBIDO
                        CR-DT-PAGTO CR-PORTADOR.
           MOVE SPACE TO CR-NOSSO-NUMERO.
           MOVE "A" TO CR-SITUACAO.
           MOVE "S" TO WS-ACHOU.
           WRITE REG-RECEBER
              INVALID KEY MOVE "N" TO WS-ACHOU.
           CLOSE ARQ-RECEBER.
           IF WS-ACHOU = "N"
              MOVE "*** ERRO GERANDO DUPLICATA ***" TO WS-MSG
              GO TO GRAVA-FIM.
           MOVE "F" TO VD-SITUACAO.
           REWRITE REG-VENDA
              INVALID KEY DISPLAY "ERRO FINALIZANDO PEDIDO".
           MOVE VD-NUMERO TO MF-PEDIDO.
           MOVE CR-TITULO TO MF-TITULO.
           MOVE CR-PARCELA TO MF-PARCELA.
           MOVE WS-MSG-FAT TO WS-MSG.
       GRAVA-FIM.
           EXIT.
       CONFERE-PRODUTO.
           MOVE WT-PRODUTO(WS-I) TO PR-CODIGO.
           MOVE "S" TO WS-ACHOU.
           READ ARQ-PROD
              INVALID KEY MOVE "N" TO WS-ACHOU.
           IF WS-ACHOU = "N"
              MOVE "*** PRODUTO INDISPONIVEL NA GRAVACAO ***" TO WS-MSG
              MOVE "S" TO WS-ERRO
              GO TO CONFERE-FIM.
           MOVE ZERO TO WS-DISPONIVEL.
           IF PR-ESTOQUE > PR-RESERVADO
              SUBTRACT PR-RESERVADO FROM PR-ESTOQUE
                 GIVING WS-DISPONIVEL.
           IF WT-QTD(WS-I) > WS-DISPONIVEL
              MOVE "*** ESTOQUE ALTERADO - PEDIDO NAO GRAVADO ***"
                TO WS-MSG
              MOVE "S" TO WS-ERRO.
       CONFERE-FIM.
           EXIT.
       GRAVA-ITENS.
           MOVE VD-NUMERO TO IV-VENDA.
           MOVE WS-I TO IV-ITEM.
           MOVE WT-PRODUTO(WS-I) TO IV-PRODUTO PR-CODIGO.
           MOVE WT-QTD(WS-I) TO IV-QUANTIDADE.
           MOVE WT-PRECO(WS-I) TO IV-PRECO.
           MOVE WT-DESC(WS-I) TO IV-DESCONTO.
           MOVE WT-TOTAL(WS-I) TO IV-TOTAL.
           WRITE REG-ITEM-VENDA
              INVALID KEY DISPLAY "ERRO GRAVANDO ITEM".
           MOVE "S" TO WS-ACHOU.
           READ ARQ-PROD
              INVALID KEY MOVE "N" TO WS-ACHOU.
           IF WS-ACHOU = "N"
              DISPLAY "ERRO RELEITURA PRODUTO"
           ELSE
              SUBTRACT IV-QUANTIDADE FROM PR-ESTOQUE
              ADD IV-QUANTIDADE TO PR-QTD-VEND-ANO
              ADD IV-TOTAL TO PR-VLR-VEND-ANO
              MOVE VD-DATA TO PR-DT-ULT-VEN
              REWRITE REG-PRODUTO
                 INVALID KEY DISPLAY "ERRO ATUALIZANDO ESTOQUE".
           MOVE "S" TO MV-TIPO.
           MOVE VD-NUMERO TO MV-DOCUMENTO.
           MOVE WS-I TO MV-ITEM.
           MOVE VD-DATA TO MV-DATA.
           MOVE IV-PRODUTO TO MV-PRODUTO.
           MOVE IV-QUANTIDADE TO MV-QUANTIDADE.
           MOVE PR-CUSTO-MEDIO TO MV-CUSTO.
           MOVE VD-OPERADOR TO MV-OPERADOR.
           MOVE VD-TERMINAL TO MV-TERMINAL.
           WRITE REG-MOV
              INVALID KEY DISPLAY "AVISO: MOVIMENTO NAO GRAVADO".

      *---- TELA VT100: MOLDURA DESENHADA UMA VEZ, DEPOIS SO OS
      *CAMPOS QUE MUDAM. LINHA 22 = PERGUNTA, 23 = DIGITACAO,
      *24 = RODAPE EM VIDEO INVERSO.
       MOLDURA.
           DISPLAY VT-LIMPA.
           PERFORM LINHA-1.
           DISPLAY TV-TRACO.
           DISPLAY " CLIENTE : ".
           DISPLAY " LIMITE  : ".
           DISPLAY " OPERADOR: ".
           DISPLAY TV-TRACO.
           DISPLAY VT-FORTE TV-L7 VT-NORMAL.
           DISPLAY TV-L8.
           MOVE 19 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS TV-TRACO.
           PERFORM LINHA-TOTAL.
           PERFORM RODAPE.
       LINHA-1.
           MOVE VD-NUMERO TO TV1-PEDIDO.
           MOVE VD-DATA TO WS-DTW.
           PERFORM FORMATA-DATA.
           MOVE WS-DT-ED TO TV1-DATA.
           IF VD-DATA = ZERO MOVE SPACES TO TV1-DATA.
           MOVE 1 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-INV " " TV-L1 "  " VT-NORMAL.
       LINHAS-CLIENTE.
           MOVE CL-CODIGO TO TV3-COD.
           MOVE CL-RAZAO TO TV3-RAZAO.
           MOVE CL-CIDADE TO TV3-CIDADE.
           MOVE CL-UF TO TV3-UF.
           MOVE 3 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA VT-FORTE TV-L3 VT-NORMAL.
           MOVE CL-LIM-CREDITO TO TV4-LIM.
           MOVE CL-SALDO TO TV4-SALDO.
           MOVE ZERO TO WS-LIVRE.
           IF CL-LIM-CREDITO > CL-SALDO
              SUBTRACT CL-SALDO FROM CL-LIM-CREDITO GIVING WS-LIVRE.
           MOVE WS-LIVRE TO TV4-LIVRE.
           MOVE 4 TO VT-LIN.
           DISPLAY VT-POS VT-APAGA TV-L4.
       LINHA-5.
           MOVE VD-OPERADOR TO TV5-OPER.
           MOVE VD-TERMINAL TO TV5-TERM.
           MOVE WS-VENCIMENTO TO WS-DTW.
           PERFORM FORMATA-DATA.
           MOVE WS-DT-ED TO TV5-VENC.
           IF WS-VENCIMENTO = ZERO MOVE SPACES TO TV5-VENC.
           MOVE VD-COND-PAGTO TO TV5-COND.
           MOVE 5 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA TV-L5.
       MOSTRA-ITENS.
           MOVE 1 TO WS-INI.
           IF WS-NITENS > 10
              SUBTRACT 9 FROM WS-NITENS GIVING WS-INI.
           MOVE WS-INI TO WS-K.
           MOVE 1 TO WS-LIN.
           IF WS-NITENS > 10
              PERFORM TELA-ITEM UNTIL WS-LIN > 10
           ELSE
              MOVE WS-NITENS TO WS-K
              MOVE WS-NITENS TO WS-LIN
              PERFORM TELA-ITEM.
       TELA-ITEM.
           ADD 8 WS-LIN GIVING VT-LIN.
           MOVE 1 TO VT-COL.
           MOVE WS-K TO TVI-IT.
           MOVE WT-PRODUTO(WS-K) TO TVI-COD.
           MOVE WT-DESCR(WS-K) TO TVI-DESC.
           MOVE WT-QTD(WS-K) TO TVI-QTD.
           MOVE WT-PRECO(WS-K) TO TVI-PRECO.
           MOVE WT-TOTAL(WS-K) TO TVI-TOTAL.
           DISPLAY VT-POS VT-APAGA TV-IT.
           ADD 1 TO WS-K.
           ADD 1 TO WS-LIN.
       LINHA-TOTAL.
           MOVE WS-NITENS TO TV20A-ITENS.
           MOVE WS-TOTAL TO TV20B-TOTAL.
           MOVE 20 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA TV-L20A VT-INV VT-FORTE TV-L20B
                   VT-NORMAL.
       MOSTRA-MSG.
           MOVE 21 TO VT-LIN.
           MOVE 1 TO VT-COL.
           IF WS-MSG = SPACES
              DISPLAY VT-POS VT-APAGA
           ELSE
              IF WS-MSG-1 = "*"
                 DISPLAY VT-POS VT-APAGA VT-INV VT-PISCA " " WS-MSG
                         " " VT-NORMAL
              ELSE
                 IF WS-MSG-1 = "P"
                    DISPLAY VT-POS VT-APAGA VT-INV " " WS-MSG " "
                            VT-NORMAL
                 ELSE
                    DISPLAY VT-POS VT-APAGA " " WS-MSG.
       PERGUNTA.
           MOVE 23 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA.
           MOVE 22 TO VT-LIN.
           DISPLAY VT-POS VT-APAGA VT-FORTE " " WS-PROMPT VT-NORMAL.
       RODAPE.
           MOVE 24 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-INV TV-ROD1 VT-NORMAL " " VT-INV TV-ROD2
                   VT-NORMAL VT-HOME.
       FORMATA-DATA.
           MOVE WS-DTW-DD TO WS-DE-DD.
           MOVE WS-DTW-MM TO WS-DE-MM.
           MOVE WS-DTW-AA TO WS-DE-AA.
      *---- CLIENTE (V22)
       PEDE-CLIENTE.
           PERFORM MOSTRA-MSG.
           MOVE "CODIGO DO CLIENTE (? = LISTA):" TO WS-PROMPT.
           PERFORM PERGUNTA.
           MOVE SPACES TO WS-MSG.
           PERFORM LC-LE-CODIGO.
           IF LC-C (1) = "x" MOVE "X" TO LC-C (1).
           IF LC-C (1) = "X"
              MOVE "S" TO WS-PC-FIM
              GO TO PC-FIM.
           IF LC-C (1) = "?"
              PERFORM PQC-PESQUISA
              PERFORM REDESENHA
              IF PQ-OK = "N"
                 GO TO PC-FIM
              ELSE
                 MOVE CL-CODIGO TO VD-CLIENTE
                 GO TO PC-LER.
           IF LC-ENT = SPACES GO TO PC-FIM.
           IF LC-OK = "N"
              MOVE "*** CODIGO INVALIDO ***" TO WS-MSG
              GO TO PC-FIM.
           MOVE LC-NUM TO VD-CLIENTE.
       PC-LER.
           MOVE VD-CLIENTE TO CL-CODIGO.
           MOVE "S" TO WS-ACHOU.
           READ ARQ-CLIENTE
              INVALID KEY MOVE "N" TO WS-ACHOU.
           IF WS-ACHOU = "N"
              MOVE VD-CLIENTE TO WM-CLI
              MOVE WS-MSG-NC TO WS-MSG
              GO TO PC-FIM.
           IF CL-SITUACAO = "B"
              MOVE "*** CLIENTE INATIVO - ESCOLHA OUTRO ***" TO WS-MSG
              GO TO PC-FIM.
           MOVE "S" TO WS-CL-OK.
           MOVE "S" TO WS-PC-FIM.
           PERFORM LINHAS-CLIENTE.
       PC-FIM.
           EXIT.
      *---- REDESENHA A TELA INTEIRA (DEPOIS DE UMA PESQUISA)
       REDESENHA.
           PERFORM MOLDURA.
           IF WS-CL-OK = "S" PERFORM LINHAS-CLIENTE.
           PERFORM LINHA-5.
           MOVE 1 TO WS-INI.
           IF WS-NITENS > 10
              SUBTRACT 9 FROM WS-NITENS GIVING WS-INI.
           MOVE WS-INI TO WS-K.
           MOVE 1 TO WS-LIN.
           PERFORM TELA-ITEM UNTIL WS-K > WS-NITENS.
           PERFORM LINHA-TOTAL.
      *---- PESQUISA: PESQUISA DE CLIENTES
      *SAIDA: PQ-OK = "S" E A CHAVE ESCOLHIDA EM CL-CHAVE.
       PQC-PESQUISA.
           MOVE "N" TO PQ-OK.
           MOVE "N" TO PQ-FIM.
           MOVE 1 TO PQ-PAG.
           MOVE LOW-VALUES TO CL-CHAVE.
           MOVE CL-CHAVE TO PQ-INI (1).
           PERFORM PQC-TELA THRU PQC-TELA-FIM UNTIL PQ-FIM = "S".
       PQC-TELA.
           MOVE PQ-INI (PQ-PAG) TO CL-CHAVE.
           MOVE "N" TO PQ-EOF.
           IF PQ-PAG = 1
              START ARQ-CLIENTE KEY IS NOT < CL-CHAVE
                 INVALID KEY MOVE "S" TO PQ-EOF
           ELSE
              START ARQ-CLIENTE KEY IS GREATER THAN CL-CHAVE
                 INVALID KEY MOVE "S" TO PQ-EOF.
           MOVE PQ-PAG TO PQC-TP.
           DISPLAY VT-LIMPA VT-INV " " PQC-TIT "                 "
                   VT-NORMAL.
           DISPLAY "-------------------------------------------"
                   "------------------------------------".
           DISPLAY VT-FORTE PQC-CAB VT-NORMAL.
           DISPLAY "-------------------------------------------"
                   "------------------------------------".
           MOVE ZERO TO PQ-N.
           MOVE ZERO TO PQ-L.
           PERFORM PQC-LINHA UNTIL PQ-L NOT < 15.
           MOVE "N" TO PQ-MAIS.
           IF PQ-EOF = "N"
              READ ARQ-CLIENTE NEXT RECORD
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
           IF LC-ENT = SPACES GO TO PQC-TELA-FIM.
           IF LC-C (1) = "-"
              IF PQ-PAG > 1
                 SUBTRACT 1 FROM PQ-PAG.
           IF LC-C (1) = "-" GO TO PQC-TELA-FIM.
           IF LC-C (1) = "x" MOVE "X" TO LC-C (1).
           IF LC-C (1) = "X"
              MOVE "S" TO PQ-FIM
              GO TO PQC-TELA-FIM.
           IF LC-OK = "S"
              IF LC-NUM > ZERO
                 IF LC-NUM NOT > PQ-N
                    MOVE LC-NUM TO PQ-L
                    MOVE PQ-COD (PQ-L) TO CL-CHAVE
                    MOVE "S" TO PQ-OK
                    MOVE "S" TO PQ-FIM.
       PQC-TELA-FIM.
           EXIT.
       PQC-LINHA.
           ADD 1 TO PQ-L.
           IF PQ-EOF = "N"
              READ ARQ-CLIENTE NEXT RECORD
                 AT END MOVE "S" TO PQ-EOF.
           IF PQ-EOF = "N"
              ADD 1 TO PQ-N
              MOVE CL-CHAVE TO PQ-COD (PQ-N)
              MOVE PQ-N TO PQC-NL
              MOVE CL-CODIGO TO PQC-C1
              MOVE CL-RAZAO TO PQC-C2
              MOVE CL-CIDADE TO PQC-C3
              MOVE CL-UF TO PQC-C4
              DISPLAY PQC-LIN.
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
              MOVE PR-PRECO-VENDA TO PQP-C4
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
           PERFORM CONFERE-PRODUTO THRU CONFERE-FIM.
           ADD 1 TO WS-I.
       V14-03.
           PERFORM GRAVA-ITENS.
           ADD 1 TO WS-I.
