       IDENTIFICATION DIVISION.
       PROGRAM-ID. RACREL.
      *RETROLABS ACME - FONTES V23
       AUTHOR. JOSE LUIZ MARTINS - RETROLABS.
      *RETROLABS ACME - RELATORIOS
      *V12: DADOS NO DRIVE B: / SEM LOCK (UM TERMINAL POR VEZ)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-CLIENTE ASSIGN TO "B:CLI.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CL-CHAVE.
           SELECT ARQ-FORNEC ASSIGN TO "B:FORN.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FR-CHAVE.
           SELECT ARQ-PROD ASSIGN TO "B:PROD.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PR-CHAVE.
           SELECT ARQ-VENDA ASSIGN TO "B:VEN.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS VD-CHAVE.
           SELECT ARQ-ITVEN ASSIGN TO "B:ITVEN.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS IV-CHAVE.
           SELECT ARQ-RECEBER ASSIGN TO "B:REC.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CR-CHAVE.
           SELECT ARQ-PAGAR ASSIGN TO "B:PAG.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PG-CHAVE.
           SELECT ARQ-COMPRA ASSIGN TO "B:COMP.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CP-CHAVE.
           SELECT ARQ-ITCOM ASSIGN TO "B:ITCOM.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS IC-CHAVE.
           SELECT IMPRESSORA ASSIGN TO "LST:"
               ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-CLIENTE RECORD CONTAINS 341 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REG-CLIENTE.
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
       FD  ARQ-FORNEC RECORD CONTAINS 330 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REG-FORNECEDOR.
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
       01  REG-PRODUTO.
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
       FD  ARQ-VENDA RECORD CONTAINS 77 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REG-VENDA.
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
       FD  ARQ-ITVEN RECORD CONTAINS 45 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REG-ITVEN.
           03 IV-CHAVE.
              05 IV-VENDA            PIC 9(8).
              05 IV-ITEM             PIC 9(3).
           03 IV-PRODUTO             PIC 9(7).
           03 IV-QUANTIDADE          PIC 9(6).
           03 IV-PRECO               PIC 9(6)V99.
           03 IV-DESCONTO            PIC 99V9.
           03 IV-TOTAL               PIC 9(8)V99.
       FD  ARQ-RECEBER RECORD CONTAINS 102 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REG-RECEBER.
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
       FD  ARQ-PAGAR RECORD CONTAINS 65 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REG-PAGAR.
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
       FD  ARQ-COMPRA RECORD CONTAINS 43 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REG-COMPRA.
           03 CP-CHAVE.
              05 CP-NUMERO              PIC 9(8).
           03 CP-DATA                PIC 9(6).
           03 CP-FORNECEDOR          PIC 9(5).
           03 CP-OPERADOR            PIC X(8).
           03 CP-TERMINAL            PIC 99.
           03 CP-QTD-ITENS           PIC 9(3).
           03 CP-TOTAL               PIC 9(8)V99.
           03 CP-SITUACAO            PIC X.
       FD  ARQ-ITCOM RECORD CONTAINS 43 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REG-ITCOM.
           03 IC-CHAVE.
              05 IC-COMPRA           PIC 9(8).
              05 IC-ITEM             PIC 9(3).
           03 IC-PRODUTO             PIC 9(7).
           03 IC-QUANTIDADE          PIC 9(7).
           03 IC-CUSTO               PIC 9(6)V99.
           03 IC-TOTAL               PIC 9(8)V99.
       FD  IMPRESSORA
           LABEL RECORDS ARE OMITTED.
       01  LINHA-IMP                 PIC X(80).
       WORKING-STORAGE SECTION.
       01  WS-DATA-DRIVE          PIC XX VALUE "B:".
       01  WS-OPCAO                  PIC X.
       01  CAB-REL-01.
           03 FILLER            PIC X(40) VALUE
              "CODIGO            RAZAO SOCIAL          ".
           03 FILLER            PIC X(25) VALUE
              "       CIDADE          UF".
       01  CAB-REL-02.
           03 FILLER            PIC X(40) VALUE
              "CODIGO            CLIENTE               ".
           03 FILLER            PIC X(30) VALUE
              "                SALDO  TITULOS".
       01  CAB-REL-03.
           03 FILLER            PIC X(40) VALUE
              "PEDIDO   DATA     CLIENTE               ".
           03 FILLER            PIC X(28) VALUE
              "                TOTAL  ITENS".
       01  CAB-REL-04.
           03 FILLER            PIC X(40) VALUE
              "PEDIDO   ITEM     PRODUTO               ".
           03 FILLER            PIC X(33) VALUE
              "                TOTAL  QUANTIDADE".
       01  CAB-REL-05.
           03 FILLER            PIC X(40) VALUE
              "CODIGO   PART-NR  DESCRICAO             ".
           03 FILLER            PIC X(21) VALUE
              "                PRECO".
       01  CAB-REL-06.
           03 FILLER            PIC X(40) VALUE
              "CODIGO            PRODUTO               ".
           03 FILLER            PIC X(28) VALUE
              "       DISPONIVEL      LOCAL".
       01  CAB-REL-07.
           03 FILLER            PIC X(40) VALUE
              "CODIGO            PRODUTO               ".
           03 FILLER            PIC X(33) VALUE
              "       ESTOQUE         FORNECEDOR".
       01  CAB-REL-08.
           03 FILLER            PIC X(40) VALUE
              "TITULO   PARC     CLIENTE               ".
           03 FILLER            PIC X(31) VALUE
              "                VALOR  SITUACAO".
       01  CAB-REL-09.
           03 FILLER            PIC X(40) VALUE
              "TITULO   PARC     FORNECEDOR            ".
           03 FILLER            PIC X(31) VALUE
              "                VALOR  SITUACAO".
       01  CAB-REL-10.
           03 FILLER            PIC X(40) VALUE
              "COMPRA   DATA     FORNECEDOR            ".
           03 FILLER            PIC X(28) VALUE
              "                TOTAL  ITENS".
       01  CAB-REL-11.
           03 FILLER            PIC X(40) VALUE
              "COMPRA   ITEM     PRODUTO               ".
           03 FILLER            PIC X(33) VALUE
              "                TOTAL  QUANTIDADE".
       01  WS-DESTINO                PIC X.
       01  WS-FIM                    PIC X.
       01  WS-ABORTA                 PIC X VALUE "N".
       01  WS-IMP-ABERTA             PIC X.
       01  WS-PAGINA                 PIC 999.
       01  WS-LINHAS                 PIC 99.
       01  WS-TOTAL                  PIC 9(10)V99.
       01  WS-CONTA                  PIC 9(6).
       01  WS-DISPONIVEL             PIC 9(7).
       01  WS-TITULO                 PIC X(40).
       01  WS-CAB-COL                PIC X(80).
       01  WS-LINHA-SAV              PIC X(80).
       01  WS-LIN-TELA               PIC 99 VALUE ZERO.
       01  WS-PAUSA                  PIC X VALUE SPACE.
       01  WS-EM-CAB                 PIC X VALUE "N".
       01  WS-LINHA                  PIC X(80).
       01  WS-GERAL REDEFINES WS-LINHA.
           03 GL-1                   PIC X(8).
           03 FILLER                 PIC X.
           03 GL-2                   PIC X(8).
           03 FILLER                 PIC X.
           03 GL-3                   PIC X(28).
           03 FILLER                 PIC X.
           03 GL-4                   PIC X(15).
           03 FILLER                 PIC X.
           03 GL-5                   PIC X(17).
       01  WS-RES REDEFINES WS-LINHA.
           03 RS-MES                 PIC X(6).
           03 FILLER                 PIC X(3).
           03 RS-QTD                 PIC ZZZZ9.
           03 FILLER                 PIC X(3).
           03 RS-VLR                 PIC ZZZ,ZZZ,ZZ9.99.
           03 FILLER                 PIC X(2).
           03 RS-BARRA               PIC X(40).
           03 FILLER                 PIC X(7).
       01  WS-CAB REDEFINES WS-LINHA.
           03 CAB-TEXTO              PIC X(60).
           03 FILLER                 PIC X(10).
           03 CAB-PAG-TXT            PIC X(4).
           03 CAB-PAG                PIC 999.
           03 FILLER                 PIC X(3).
       01  WS-MESES-TXT              PIC X(36) VALUE
           "JANFEVMARABRMAIJUNJULAGOSETOUTNOVDEZ".
       01  WS-MESES REDEFINES WS-MESES-TXT.
           03 WS-MES-NOME            PIC X(3) OCCURS 12 TIMES.
       01  WS-TAB-MES.
           03 WS-TM OCCURS 12 TIMES.
              05 TM-QTD              PIC 9(5).
              05 TM-VLR              PIC 9(10)V99.
       01  WS-DT-VEN                 PIC 9(6).
       01  WS-DT-R REDEFINES WS-DT-VEN.
           03 WS-DT-AA               PIC 99.
           03 WS-DT-MM               PIC 99.
           03 WS-DT-DD               PIC 99.
       01  WS-MES-ED.
           03 WS-ME-NOME             PIC X(3).
           03 WS-ME-BARRA            PIC X VALUE "/".
           03 WS-ME-AA               PIC 99.
       01  WS-ANO-RES                PIC 99 VALUE 84.
       01  WS-M                      PIC 99.
       01  WS-B                      PIC 99.
       01  WS-NBAR                   PIC 99.
       01  WS-MAX-MES                PIC 9(10)V99.
       01  WS-AUX-MES                PIC 9(12)V99.
       01  WS-BARRA.
           03 WS-BCH                 PIC X OCCURS 40 TIMES.
       01  ED-VLR                    PIC ZZZ,ZZZ,ZZ9.99.
       01  ED-N8                     PIC 9(8).
       01  ED-N7                     PIC 9(7).
       01  ED-N6                     PIC 9(6).
       01  ED-N5                     PIC 9(5).
       01  ED-N3                     PIC 999.
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
              "RELATORIOS                             ".
           03 FILLER            PIC X(39) VALUE
              "                       RETROLABS ACME  ".
       01  TL-RODAPE            PIC X(79) VALUE SPACES.
       01  TL-RODAPE-R REDEFINES TL-RODAPE.
           03 TL-ROD-A          PIC X(34).
           03 TL-ROD-B          PIC X(45).
       01  TL-ROD-MENU.
           03 FILLER            PIC X(39) VALUE
              " ESCOLHA O RELATORIO PELA TECLA    DEPO".
           03 FILLER            PIC X(39) VALUE
              "IS O DESTINO: 1=VIDEO 2=IMPRESSORA     ".
           03 FILLER            PIC X(1) VALUE
              " ".
       01  TL-ROD-VIDEO.
           03 FILLER            PIC X(39) VALUE
              " RETURN = PROXIMA TELA             X = ".
           03 FILLER            PIC X(39) VALUE
              "ENCERRA O RELATORIO E VOLTA AO MENU    ".
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
       01  TR-R02.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
           03 FILLER            PIC X(40) VALUE
              "    C A D A S T R O S                   ".
           03 FILLER            PIC X(19) VALUE
              "  M O V I M E N T O".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  TR-R03.
           03 FILLER            PIC X(40) VALUE
              "    -----------------                   ".
           03 FILLER            PIC X(19) VALUE
              "  -----------------".
       01  TR-R04.
           03 FILLER            PIC X(3) VALUE
              "   ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 FILLER            PIC X(3) VALUE
              " 1 ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(33) VALUE
              " RELACAO DE CLIENTES             ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 FILLER            PIC X(3) VALUE
              " 3 ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(18) VALUE
              " VENDAS DO PERIODO".
       01  TR-R05.
           03 FILLER            PIC X(3) VALUE
              "   ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 FILLER            PIC X(3) VALUE
              " 2 ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(33) VALUE
              " POSICAO FINANCEIRA CLIENTES     ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 FILLER            PIC X(3) VALUE
              " 4 ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(15) VALUE
              " ITENS VENDIDOS".
       01  TR-R06.
           03 FILLER            PIC X(3) VALUE
              "   ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 FILLER            PIC X(3) VALUE
              " 5 ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(33) VALUE
              " RELACAO DE FORNECEDORES         ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 FILLER            PIC X(3) VALUE
              " B ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(8) VALUE
              " COMPRAS".
       01  TR-R07.
           03 FILLER            PIC X(3) VALUE
              "   ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 FILLER            PIC X(3) VALUE
              " 6 ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(33) VALUE
              " RELACAO DE PRODUTOS             ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 FILLER            PIC X(3) VALUE
              " C ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(16) VALUE
              " ITENS COMPRADOS".
       01  TR-R09.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
           03 FILLER            PIC X(40) VALUE
              "    E S T O Q U E                       ".
           03 FILLER            PIC X(21) VALUE
              "  F I N A N C E I R O".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  TR-R10.
           03 FILLER            PIC X(40) VALUE
              "    -------------                       ".
           03 FILLER            PIC X(21) VALUE
              "  -------------------".
       01  TR-R11.
           03 FILLER            PIC X(3) VALUE
              "   ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 FILLER            PIC X(3) VALUE
              " 7 ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(33) VALUE
              " POSICAO DE ESTOQUE              ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 FILLER            PIC X(3) VALUE
              " 9 ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(17) VALUE
              " CONTAS A RECEBER".
       01  TR-R12.
           03 FILLER            PIC X(3) VALUE
              "   ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 FILLER            PIC X(3) VALUE
              " 8 ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(33) VALUE
              " ESTOQUE ABAIXO DO MINIMO        ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 FILLER            PIC X(3) VALUE
              " A ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(15) VALUE
              " CONTAS A PAGAR".
       01  TR-R14.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
           03 FILLER            PIC X(21) VALUE
              "    G E R E N C I A L".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  TR-R15.
           03 FILLER            PIC X(21) VALUE
              "    -----------------".
       01  TR-R16.
           03 FILLER            PIC X(3) VALUE
              "   ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 FILLER            PIC X(3) VALUE
              " D ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(33) VALUE
              " RESUMO DE VENDAS POR MES        ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 FILLER            PIC X(3) VALUE
              " X ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(26) VALUE
              " RETORNA AO MENU PRINCIPAL".
       PROCEDURE DIVISION.
       INICIO.
           MOVE SPACE TO WS-OPCAO.
           PERFORM MENU THRU MENU-FIM UNTIL WS-OPCAO = "X".
           GO TO FIM-PROGRAMA.
       MENU.
           MOVE TL-TIT-PADRAO TO TL-TITULO.
           PERFORM TL-TITULO-P.
           PERFORM TELA-MENU.
           PERFORM TL-MSG-P.
           MOVE TL-ROD-MENU TO TL-RODAPE.
           PERFORM TL-RODAPE-P.
           MOVE "OPCAO:" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           MOVE SPACES TO TL-MSG.
           ACCEPT WS-OPCAO.
           IF WS-OPCAO = "X" GO TO MENU-FIM.
           PERFORM DESTINO.
           IF WS-OPCAO = "1" PERFORM REL-CLIENTES THRU RC-FIM.
           IF WS-OPCAO = "2" PERFORM REL-FINCLI THRU RFC-FIM.
           IF WS-OPCAO = "3" PERFORM REL-VENDAS THRU RV-FIM.
           IF WS-OPCAO = "4" PERFORM REL-ITVENDAS THRU RIV-FIM.
           IF WS-OPCAO = "5" PERFORM REL-FORNEC THRU RF-FIM.
           IF WS-OPCAO = "6" PERFORM REL-PRODUTOS THRU RP-FIM.
           IF WS-OPCAO = "7" PERFORM REL-ESTOQUE THRU RE-FIM.
           IF WS-OPCAO = "8" PERFORM REL-MINIMO THRU RM-FIM.
           IF WS-OPCAO = "9" PERFORM REL-RECEBER THRU RR-FIM.
           IF WS-OPCAO = "A" PERFORM REL-PAGAR THRU RPG-FIM.
           IF WS-OPCAO = "B" PERFORM REL-COMPRAS THRU RCO-FIM.
           IF WS-OPCAO = "C" PERFORM REL-ITCOMPRAS THRU RIC-FIM.
           IF WS-OPCAO = "D" PERFORM REL-RESUMO THRU RRS-FIM.
           PERFORM FECHA-DESTINO.
       MENU-FIM.
           EXIT.
       DESTINO.
           MOVE "N" TO WS-IMP-ABERTA.
           MOVE "DESTINO (1=VIDEO  2=IMPRESSORA):" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           ACCEPT WS-DESTINO.
           IF WS-DESTINO = "2"
              OPEN OUTPUT IMPRESSORA
              MOVE "S" TO WS-IMP-ABERTA.
       FECHA-DESTINO.
           IF WS-IMP-ABERTA = "S"
              CLOSE IMPRESSORA
              MOVE "N" TO WS-IMP-ABERTA
              MOVE "RELATORIO ENVIADO PARA A IMPRESSORA" TO TL-MSG
           ELSE
              IF WS-ABORTA = "S"
                 MOVE "RELATORIO ENCERRADO PELO OPERADOR" TO TL-MSG
              ELSE
                 IF WS-LIN-TELA > ZERO
                    MOVE "-- FIM DO RELATORIO - TECLE RETURN --"
                      TO TL-PROMPT
                    PERFORM PAUSA-TELA.
           MOVE "N" TO WS-ABORTA.
           MOVE ZERO TO WS-LIN-TELA.
       EMITE.
           IF WS-LINHAS > 58
              MOVE WS-LINHA TO WS-LINHA-SAV
              PERFORM NOVA-PAGINA
              MOVE WS-LINHA-SAV TO WS-LINHA.
           PERFORM EMITE-FISICO.
       EMITE-FISICO.
           IF WS-DESTINO = "2"
              MOVE WS-LINHA TO LINHA-IMP
              WRITE LINHA-IMP
           ELSE
              IF WS-EM-CAB NOT = "S"
                 IF WS-ABORTA NOT = "S"
                    PERFORM PAUSA-VIDEO
                    IF WS-ABORTA NOT = "S"
                       DISPLAY WS-LINHA.
           ADD 1 TO WS-LINHAS.
       PAUSA-VIDEO.
           IF WS-LIN-TELA = ZERO PERFORM CAB-VIDEO.
           IF WS-LIN-TELA > 17
              MOVE "RETURN = CONTINUA     X = ENCERRA" TO TL-PROMPT
              PERFORM PAUSA-TELA
              IF WS-ABORTA NOT = "S"
                 PERFORM CAB-VIDEO.
           ADD 1 TO WS-LIN-TELA.
       CAB-VIDEO.
           DISPLAY VT-LIMPA.
           MOVE TL-ROD-VIDEO TO TL-RODAPE.
           PERFORM TL-RODAPE-P.
           MOVE WS-TITULO TO TL-TITULO.
           MOVE 1 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-INV " " TL-TITULO VT-NORMAL.
           DISPLAY TL-TRACO.
           DISPLAY VT-FORTE WS-CAB-COL VT-NORMAL.
           MOVE ZERO TO WS-LIN-TELA.
       PAUSA-TELA.
           PERFORM TL-PERGUNTA.
           MOVE SPACE TO WS-PAUSA.
           ACCEPT WS-PAUSA.
      *X ENCERRA O RELATORIO: PARA A LEITURA E NAO MOSTRA MAIS NADA
           IF WS-PAUSA = "x" MOVE "X" TO WS-PAUSA.
           IF WS-PAUSA = "X"
              MOVE "S" TO WS-ABORTA
              MOVE "S" TO WS-FIM.
       NOVA-PAGINA.
           MOVE "S" TO WS-EM-CAB.
           IF WS-PAGINA > ZERO PERFORM SALTA-PAGINA.
           ADD 1 TO WS-PAGINA.
           MOVE ZERO TO WS-LINHAS.
           MOVE SPACES TO WS-LINHA.
           MOVE WS-TITULO TO CAB-TEXTO.
           MOVE "PAG." TO CAB-PAG-TXT.
           MOVE WS-PAGINA TO CAB-PAG.
           PERFORM EMITE-FISICO.
           MOVE "RETROLABS ACME - SISTEMA COMERCIAL - 1984"
             TO WS-LINHA.
           PERFORM EMITE-FISICO.
           MOVE ALL "-" TO WS-LINHA.
           PERFORM EMITE-FISICO.
           IF WS-CAB-COL NOT = SPACES
              MOVE WS-CAB-COL TO WS-LINHA
              PERFORM EMITE-FISICO.
           MOVE "N" TO WS-EM-CAB.
       SALTA-PAGINA.
           IF WS-DESTINO = "2"
              MOVE SPACES TO LINHA-IMP
              WRITE LINHA-IMP AFTER ADVANCING PAGE
           ELSE
              DISPLAY SPACE.
       PREPARA.
           MOVE ZERO TO WS-PAGINA WS-LINHAS WS-LIN-TELA.
           MOVE "N" TO WS-ABORTA.
           PERFORM NOVA-PAGINA.
       LIMPA-LINHA.
           MOVE SPACES TO WS-LINHA.

       REL-CLIENTES.
           OPEN INPUT ARQ-CLIENTE.
           MOVE "RELACAO DE CLIENTES" TO WS-TITULO.
           MOVE CAB-REL-01 TO WS-CAB-COL.
           PERFORM PREPARA.
           MOVE ZERO TO CL-CODIGO.
           START ARQ-CLIENTE KEY IS NOT < CL-CHAVE
              INVALID KEY GO TO RC-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM RC-LER THROUGH RC-LER-FIM
              UNTIL WS-FIM = "S".
       RC-FIM.
           CLOSE ARQ-CLIENTE.
       RC-LER.
           READ ARQ-CLIENTE NEXT RECORD
              AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "S" GO TO RC-LER-FIM.
           PERFORM LIMPA-LINHA.
           MOVE CL-CODIGO TO ED-N6.
           MOVE ED-N6 TO GL-1.
           MOVE CL-RAZAO TO GL-3.
           MOVE CL-CIDADE TO GL-4.
           MOVE CL-UF TO GL-5.
           PERFORM EMITE.
       RC-LER-FIM.
           EXIT.

       REL-FINCLI.
           OPEN INPUT ARQ-CLIENTE.
           MOVE "POSICAO FINANCEIRA CLIENTES" TO WS-TITULO.
           MOVE CAB-REL-02 TO WS-CAB-COL.
           PERFORM PREPARA.
           MOVE ZERO TO CL-CODIGO.
           START ARQ-CLIENTE KEY IS NOT < CL-CHAVE
              INVALID KEY GO TO RFC-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM RFC-LER THROUGH RFC-LER-FIM
              UNTIL WS-FIM = "S".
       RFC-FIM.
           CLOSE ARQ-CLIENTE.
       RFC-LER.
           READ ARQ-CLIENTE NEXT RECORD
              AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "S" GO TO RFC-LER-FIM.
           PERFORM LIMPA-LINHA.
           MOVE CL-CODIGO TO ED-N6.
           MOVE ED-N6 TO GL-1.
           MOVE CL-RAZAO TO GL-3.
           MOVE CL-SALDO TO ED-VLR.
           MOVE ED-VLR TO GL-4.
           MOVE CL-TIT-ABERTOS TO ED-N3.
           MOVE ED-N3 TO GL-5.
           PERFORM EMITE.
       RFC-LER-FIM.
           EXIT.

       REL-VENDAS.
           OPEN INPUT ARQ-VENDA.
           MOVE "VENDAS" TO WS-TITULO.
           MOVE CAB-REL-03 TO WS-CAB-COL.
           PERFORM PREPARA.
           MOVE ZERO TO VD-NUMERO WS-TOTAL WS-CONTA.
           START ARQ-VENDA KEY IS NOT < VD-CHAVE
              INVALID KEY GO TO RV-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM RV-LER THROUGH RV-LER-FIM
              UNTIL WS-FIM = "S".
       RV-FIM.
           CLOSE ARQ-VENDA.
           PERFORM LIMPA-LINHA.
           MOVE "TOTAL" TO GL-3.
           MOVE WS-TOTAL TO ED-VLR.
           MOVE ED-VLR TO GL-4.
           PERFORM EMITE.
       RV-LER.
           READ ARQ-VENDA NEXT RECORD
              AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "S" GO TO RV-LER-FIM.
           ADD 1 TO WS-CONTA.
           ADD VD-TOTAL TO WS-TOTAL.
           PERFORM LIMPA-LINHA.
           MOVE VD-NUMERO TO ED-N8.
           MOVE ED-N8 TO GL-1.
           MOVE VD-DATA TO ED-N6.
           MOVE ED-N6 TO GL-2.
           MOVE VD-CLIENTE TO ED-N6.
           MOVE ED-N6 TO GL-3.
           MOVE VD-TOTAL TO ED-VLR.
           MOVE ED-VLR TO GL-4.
           MOVE VD-QTD-ITENS TO ED-N3.
           MOVE ED-N3 TO GL-5.
           PERFORM EMITE.
       RV-LER-FIM.
           EXIT.

       REL-ITVENDAS.
           OPEN INPUT ARQ-ITVEN.
           MOVE "ITENS VENDIDOS" TO WS-TITULO.
           MOVE CAB-REL-04 TO WS-CAB-COL.
           PERFORM PREPARA.
           MOVE ZERO TO IV-VENDA IV-ITEM.
           START ARQ-ITVEN KEY IS NOT < IV-CHAVE
              INVALID KEY GO TO RIV-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM RIV-LER THROUGH RIV-LER-FIM
              UNTIL WS-FIM = "S".
       RIV-FIM.
           CLOSE ARQ-ITVEN.
       RIV-LER.
           READ ARQ-ITVEN NEXT RECORD
              AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "S" GO TO RIV-LER-FIM.
           PERFORM LIMPA-LINHA.
           MOVE IV-VENDA TO ED-N8.
           MOVE ED-N8 TO GL-1.
           MOVE IV-ITEM TO ED-N3.
           MOVE ED-N3 TO GL-2.
           MOVE IV-PRODUTO TO ED-N7.
           MOVE ED-N7 TO GL-3.
           MOVE IV-TOTAL TO ED-VLR.
           MOVE ED-VLR TO GL-4.
           MOVE IV-QUANTIDADE TO ED-N6.
           MOVE ED-N6 TO GL-5.
           PERFORM EMITE.
       RIV-LER-FIM.
           EXIT.

       REL-FORNEC.
           OPEN INPUT ARQ-FORNEC.
           MOVE "RELACAO DE FORNECEDORES" TO WS-TITULO.
           MOVE CAB-REL-01 TO WS-CAB-COL.
           PERFORM PREPARA.
           MOVE ZERO TO FR-CODIGO.
           START ARQ-FORNEC KEY IS NOT < FR-CHAVE
              INVALID KEY GO TO RF-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM RF-LER THROUGH RF-LER-FIM
              UNTIL WS-FIM = "S".
       RF-FIM.
           CLOSE ARQ-FORNEC.
       RF-LER.
           READ ARQ-FORNEC NEXT RECORD
              AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "S" GO TO RF-LER-FIM.
           PERFORM LIMPA-LINHA.
           MOVE FR-CODIGO TO ED-N5.
           MOVE ED-N5 TO GL-1.
           MOVE FR-RAZAO TO GL-3.
           MOVE FR-CIDADE TO GL-4.
           MOVE FR-UF TO GL-5.
           PERFORM EMITE.
       RF-LER-FIM.
           EXIT.

       REL-PRODUTOS.
           OPEN INPUT ARQ-PROD.
           MOVE "RELACAO DE PRODUTOS" TO WS-TITULO.
           MOVE CAB-REL-05 TO WS-CAB-COL.
           PERFORM PREPARA.
           MOVE ZERO TO PR-CODIGO.
           START ARQ-PROD KEY IS NOT < PR-CHAVE
              INVALID KEY GO TO RP-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM RP-LER THROUGH RP-LER-FIM
              UNTIL WS-FIM = "S".
       RP-FIM.
           CLOSE ARQ-PROD.
       RP-LER.
           READ ARQ-PROD NEXT RECORD
              AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "S" GO TO RP-LER-FIM.
           PERFORM LIMPA-LINHA.
           MOVE PR-CODIGO TO ED-N7.
           MOVE ED-N7 TO GL-1.
           MOVE PR-PART-NUMBER TO GL-2.
           MOVE PR-DESCRICAO TO GL-3.
           MOVE PR-PRECO-VENDA TO ED-VLR.
           MOVE ED-VLR TO GL-4.
           PERFORM EMITE.
       RP-LER-FIM.
           EXIT.

       REL-ESTOQUE.
           OPEN INPUT ARQ-PROD.
           MOVE "POSICAO DE ESTOQUE" TO WS-TITULO.
           MOVE CAB-REL-06 TO WS-CAB-COL.
           PERFORM PREPARA.
           MOVE ZERO TO PR-CODIGO.
           START ARQ-PROD KEY IS NOT < PR-CHAVE
              INVALID KEY GO TO RE-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM RE-LER THROUGH RE-LER-FIM
              UNTIL WS-FIM = "S".
       RE-FIM.
           CLOSE ARQ-PROD.
       RE-LER.
           READ ARQ-PROD NEXT RECORD
              AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "S" GO TO RE-LER-FIM.
           SUBTRACT PR-RESERVADO FROM PR-ESTOQUE
              GIVING WS-DISPONIVEL.
           PERFORM LIMPA-LINHA.
           MOVE PR-CODIGO TO ED-N7.
           MOVE ED-N7 TO GL-1.
           MOVE PR-DESCRICAO TO GL-3.
           MOVE WS-DISPONIVEL TO ED-N7.
           MOVE ED-N7 TO GL-4.
           MOVE PR-LOCAL TO GL-5.
           PERFORM EMITE.
       RE-LER-FIM.
           EXIT.

       REL-MINIMO.
           OPEN INPUT ARQ-PROD.
           MOVE "ESTOQUE ABAIXO DO MINIMO" TO WS-TITULO.
           MOVE CAB-REL-07 TO WS-CAB-COL.
           PERFORM PREPARA.
           MOVE ZERO TO PR-CODIGO.
           START ARQ-PROD KEY IS NOT < PR-CHAVE
              INVALID KEY GO TO RM-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM RM-LER THROUGH RM-LER-FIM
              UNTIL WS-FIM = "S".
       RM-FIM.
           CLOSE ARQ-PROD.
       RM-LER.
           READ ARQ-PROD NEXT RECORD
              AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "S" GO TO RM-LER-FIM.
           IF PR-ESTOQUE NOT < PR-MINIMO GO TO RM-LER-FIM.
           PERFORM LIMPA-LINHA.
           MOVE PR-CODIGO TO ED-N7.
           MOVE ED-N7 TO GL-1.
           MOVE PR-DESCRICAO TO GL-3.
           MOVE PR-ESTOQUE TO ED-N7.
           MOVE ED-N7 TO GL-4.
           MOVE PR-FORNEC-PRINC TO ED-N5.
           MOVE ED-N5 TO GL-5.
           PERFORM EMITE.
       RM-LER-FIM.
           EXIT.

       REL-RECEBER.
           OPEN INPUT ARQ-RECEBER.
           MOVE "CONTAS A RECEBER" TO WS-TITULO.
           MOVE CAB-REL-08 TO WS-CAB-COL.
           PERFORM PREPARA.
           MOVE ZERO TO CR-TITULO CR-PARCELA WS-TOTAL.
           START ARQ-RECEBER KEY IS NOT < CR-CHAVE
              INVALID KEY GO TO RR-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM RR-LER THROUGH RR-LER-FIM
              UNTIL WS-FIM = "S".
       RR-FIM.
           CLOSE ARQ-RECEBER.
           PERFORM LIMPA-LINHA.
           MOVE "TOTAL ABERTO" TO GL-3.
           MOVE WS-TOTAL TO ED-VLR.
           MOVE ED-VLR TO GL-4.
           PERFORM EMITE.
       RR-LER.
           READ ARQ-RECEBER NEXT RECORD
              AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "S" GO TO RR-LER-FIM.
           IF CR-SITUACAO = "A" ADD CR-VALOR TO WS-TOTAL.
           PERFORM LIMPA-LINHA.
           MOVE CR-TITULO TO ED-N8.
           MOVE ED-N8 TO GL-1.
           MOVE CR-PARCELA TO GL-2.
           MOVE CR-CLIENTE TO ED-N6.
           MOVE ED-N6 TO GL-3.
           MOVE CR-VALOR TO ED-VLR.
           MOVE ED-VLR TO GL-4.
           MOVE CR-SITUACAO TO GL-5.
           PERFORM EMITE.
       RR-LER-FIM.
           EXIT.

       REL-PAGAR.
           OPEN INPUT ARQ-PAGAR.
           MOVE "CONTAS A PAGAR" TO WS-TITULO.
           MOVE CAB-REL-09 TO WS-CAB-COL.
           PERFORM PREPARA.
           MOVE ZERO TO PG-TITULO PG-PARCELA WS-TOTAL.
           START ARQ-PAGAR KEY IS NOT < PG-CHAVE
              INVALID KEY GO TO RPG-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM RPG-LER THROUGH RPG-LER-FIM
              UNTIL WS-FIM = "S".
       RPG-FIM.
           CLOSE ARQ-PAGAR.
           PERFORM LIMPA-LINHA.
           MOVE "TOTAL ABERTO" TO GL-3.
           MOVE WS-TOTAL TO ED-VLR.
           MOVE ED-VLR TO GL-4.
           PERFORM EMITE.
       RPG-LER.
           READ ARQ-PAGAR NEXT RECORD
              AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "S" GO TO RPG-LER-FIM.
           IF PG-SITUACAO = "A" ADD PG-VALOR TO WS-TOTAL.
           PERFORM LIMPA-LINHA.
           MOVE PG-TITULO TO ED-N8.
           MOVE ED-N8 TO GL-1.
           MOVE PG-PARCELA TO GL-2.
           MOVE PG-FORNECEDOR TO ED-N5.
           MOVE ED-N5 TO GL-3.
           MOVE PG-VALOR TO ED-VLR.
           MOVE ED-VLR TO GL-4.
           MOVE PG-SITUACAO TO GL-5.
           PERFORM EMITE.
       RPG-LER-FIM.
           EXIT.

       REL-COMPRAS.
           OPEN INPUT ARQ-COMPRA.
           MOVE "COMPRAS" TO WS-TITULO.
           MOVE CAB-REL-10 TO WS-CAB-COL.
           PERFORM PREPARA.
           MOVE ZERO TO CP-NUMERO WS-TOTAL WS-CONTA.
           START ARQ-COMPRA KEY IS NOT < CP-CHAVE
              INVALID KEY GO TO RCO-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM RCO-LER THROUGH RCO-LER-FIM
              UNTIL WS-FIM = "S".
       RCO-FIM.
           CLOSE ARQ-COMPRA.
           PERFORM LIMPA-LINHA.
           MOVE "TOTAL" TO GL-3.
           MOVE WS-TOTAL TO ED-VLR.
           MOVE ED-VLR TO GL-4.
           PERFORM EMITE.
       RCO-LER.
           READ ARQ-COMPRA NEXT RECORD
              AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "S" GO TO RCO-LER-FIM.
           ADD 1 TO WS-CONTA.
           ADD CP-TOTAL TO WS-TOTAL.
           PERFORM LIMPA-LINHA.
           MOVE CP-NUMERO TO ED-N8.
           MOVE ED-N8 TO GL-1.
           MOVE CP-DATA TO ED-N6.
           MOVE ED-N6 TO GL-2.
           MOVE CP-FORNECEDOR TO ED-N5.
           MOVE ED-N5 TO GL-3.
           MOVE CP-TOTAL TO ED-VLR.
           MOVE ED-VLR TO GL-4.
           MOVE CP-QTD-ITENS TO ED-N3.
           MOVE ED-N3 TO GL-5.
           PERFORM EMITE.
       RCO-LER-FIM.
           EXIT.

       REL-ITCOMPRAS.
           OPEN INPUT ARQ-ITCOM.
           MOVE "ITENS COMPRADOS" TO WS-TITULO.
           MOVE CAB-REL-11 TO WS-CAB-COL.
           PERFORM PREPARA.
           MOVE ZERO TO IC-COMPRA IC-ITEM.
           START ARQ-ITCOM KEY IS NOT < IC-CHAVE
              INVALID KEY GO TO RIC-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM RIC-LER THROUGH RIC-LER-FIM
              UNTIL WS-FIM = "S".
       RIC-FIM.
           CLOSE ARQ-ITCOM.
       RIC-LER.
           READ ARQ-ITCOM NEXT RECORD
              AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "S" GO TO RIC-LER-FIM.
           PERFORM LIMPA-LINHA.
           MOVE IC-COMPRA TO ED-N8.
           MOVE ED-N8 TO GL-1.
           MOVE IC-ITEM TO ED-N3.
           MOVE ED-N3 TO GL-2.
           MOVE IC-PRODUTO TO ED-N7.
           MOVE ED-N7 TO GL-3.
           MOVE IC-TOTAL TO ED-VLR.
           MOVE ED-VLR TO GL-4.
           MOVE IC-QUANTIDADE TO ED-N7.
           MOVE ED-N7 TO GL-5.
           PERFORM EMITE.
       RIC-LER-FIM.
           EXIT.

       REL-RESUMO.
           OPEN INPUT ARQ-VENDA.
           MOVE "RESUMO DE VENDAS POR MES" TO WS-TITULO.
           MOVE "MES      PEDIDOS     FATURAMENTO  PARTICIPACAO"
             TO WS-CAB-COL.
           PERFORM PREPARA.
           MOVE ZERO TO VD-NUMERO WS-TOTAL WS-CONTA WS-TAB-MES
                        WS-MAX-MES.
           MOVE 84 TO WS-ANO-RES.
           START ARQ-VENDA KEY IS NOT < VD-CHAVE
              INVALID KEY GO TO RRS-FIM.
           MOVE "N" TO WS-FIM.
           PERFORM RRS-LER THROUGH RRS-LER-FIM
              UNTIL WS-FIM = "S".
       RRS-FIM.
           CLOSE ARQ-VENDA.
           MOVE 1 TO WS-M.
           PERFORM V14-01 UNTIL WS-M > 12.
           MOVE 1 TO WS-M.
           PERFORM V14-02 UNTIL WS-M > 12.
           PERFORM LIMPA-LINHA.
           MOVE ALL "-" TO WS-LINHA.
           PERFORM EMITE.
           PERFORM LIMPA-LINHA.
           MOVE "TOTAL " TO RS-MES.
           MOVE WS-CONTA TO RS-QTD.
           MOVE WS-TOTAL TO RS-VLR.
           PERFORM EMITE.
       RRS-LER.
           READ ARQ-VENDA NEXT RECORD
              AT END MOVE "S" TO WS-FIM.
           IF WS-FIM = "S" GO TO RRS-LER-FIM.
           ADD 1 TO WS-CONTA.
           ADD VD-TOTAL TO WS-TOTAL.
           MOVE VD-DATA TO WS-DT-VEN.
           MOVE WS-DT-AA TO WS-ANO-RES.
           MOVE WS-DT-MM TO WS-M.
           IF WS-M > ZERO
              IF WS-M < 13
                 ADD 1 TO TM-QTD (WS-M)
                 ADD VD-TOTAL TO TM-VLR (WS-M).
       RRS-LER-FIM.
           EXIT.
       RRS-MAXIMO.
           IF TM-VLR (WS-M) > WS-MAX-MES
              MOVE TM-VLR (WS-M) TO WS-MAX-MES.
       RRS-LINHA.
           PERFORM LIMPA-LINHA.
           MOVE WS-MES-NOME (WS-M) TO WS-ME-NOME.
           MOVE WS-ANO-RES TO WS-ME-AA.
           MOVE WS-MES-ED TO RS-MES.
           MOVE TM-QTD (WS-M) TO RS-QTD.
           MOVE TM-VLR (WS-M) TO RS-VLR.
           MOVE SPACES TO WS-BARRA.
           MOVE ZERO TO WS-NBAR.
           IF WS-MAX-MES = ZERO GO TO RRS-LINHA-EMITE.
           MULTIPLY TM-VLR (WS-M) BY 40 GIVING WS-AUX-MES.
           DIVIDE WS-MAX-MES INTO WS-AUX-MES
              GIVING WS-NBAR.
           MOVE 1 TO WS-B.
           PERFORM V14-03 UNTIL WS-B > WS-NBAR.
       RRS-LINHA-EMITE.
           MOVE WS-BARRA TO RS-BARRA.
           PERFORM EMITE.
       RRS-LINHA-FIM.
           EXIT.
       RRS-BARRA.
           MOVE "*" TO WS-BCH (WS-B).
       TELA-MENU.
           DISPLAY " ".
           DISPLAY TR-R02.
           DISPLAY TR-R03.
           DISPLAY TR-R04.
           DISPLAY TR-R05.
           DISPLAY TR-R06.
           DISPLAY TR-R07.
           DISPLAY " ".
           DISPLAY TR-R09.
           DISPLAY TR-R10.
           DISPLAY TR-R11.
           DISPLAY TR-R12.
           DISPLAY " ".
           DISPLAY TR-R14.
           DISPLAY TR-R15.
           DISPLAY TR-R16.
           DISPLAY TL-TRACO.
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
      *V14: LACOS (CIS COBOL NAO TEM PERFORM VARYING)
       V14-01.
           PERFORM RRS-MAXIMO.
           ADD 1 TO WS-M.
       V14-02.
           PERFORM RRS-LINHA THRU RRS-LINHA-FIM.
           ADD 1 TO WS-M.
       V14-03.
           PERFORM RRS-BARRA.
           ADD 1 TO WS-B.
