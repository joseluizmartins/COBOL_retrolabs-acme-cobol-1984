       IDENTIFICATION DIVISION.
       PROGRAM-ID. RACPRO.
      *RETROLABS ACME - FONTES V23
       AUTHOR. JOSE LUIZ MARTINS - RETROLABS.
      *RETROLABS ACME - SISTEMA INTEGRADO COMERCIAL
      *AMBIENTE ALVO: CIS COBOL 4.5 / MP/M / ALTOS 5-15AD
      *VERSAO DOS FONTES: 0.1 - 1984 DEMO

      *V12: DADOS NO DRIVE B: / SEM LOCK (UM TERMINAL POR VEZ)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT ARQ-PROD ASSIGN TO "B:PROD.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PR-CHAVE.
       DATA DIVISION.
       FILE SECTION.
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

       WORKING-STORAGE SECTION.
       01  WS-DATA-DRIVE          PIC XX VALUE "B:".
       01  WS-OPCAO              PIC X.
       01  WS-ACHOU              PIC X.
       01  WS-DISPONIVEL         PIC 9(7).
       01  WS-TEM                PIC X VALUE "N".
       01  WS-MARG-A             PIC S9(6)V99.
       01  WS-MARG-B             PIC S9(8)V99.
       01  WS-MARGEM             PIC S9(4)V9.
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
       01  WS-MSG                PIC X(76) VALUE SPACES.
       01  WS-MSG-R REDEFINES WS-MSG.
           03 WS-MSG-1           PIC X.
           03 FILLER             PIC X(75).
       01  WS-PROMPT             PIC X(40) VALUE SPACES.
       01  TP-TRACO              PIC X(79) VALUE ALL "-".
       01  TP-L1.
           03 FILLER             PIC X(60) VALUE
              "PRODUTOS / ESTOQUE".
           03 FILLER             PIC X(14) VALUE "RETROLABS ACME".
       01  TP-L3.
           03 FILLER             PIC X(14) VALUE " CODIGO.....: ".
           03 TP3-COD            PIC 9(7).
           03 FILLER             PIC X(10) VALUE SPACES.
           03 FILLER             PIC X(13) VALUE "PART NUMBER: ".
           03 TP3-PN             PIC X(16).
       01  TP-L4.
           03 FILLER             PIC X(14) VALUE " DESCRICAO..: ".
           03 TP4-DESC           PIC X(30).
       01  TP-L5.
           03 FILLER             PIC X(14) VALUE " FABRICANTE.: ".
           03 TP5-FAB            PIC X(20).
           03 FILLER             PIC X(10) VALUE "   GRUPO: ".
           03 TP5-GRUPO          PIC 999.
           03 FILLER             PIC X(12) VALUE "     LOCAL: ".
           03 TP5-LOCAL          PIC X(8).
       01  TP-L7.
           03 FILLER             PIC X(40) VALUE
              "    E S T O Q U E".
           03 FILLER             PIC X(27) VALUE
              "C U S T O S   E   P R E C O".
       01  TP-L8.
           03 FILLER             PIC X(40) VALUE
              "    -------------".
           03 FILLER             PIC X(27) VALUE
              "---------------------------".
       01  TP-DUP.
           03 FILLER             PIC X(4) VALUE SPACES.
           03 TD-ROT1            PIC X(12).
           03 FILLER             PIC XX VALUE ": ".
           03 TD-VAL1            PIC ZZZZZZ9.
           03 FILLER             PIC X(15) VALUE SPACES.
           03 TD-ROT2            PIC X(13).
           03 FILLER             PIC XX VALUE ": ".
           03 TD-VAL2            PIC X(12).
       01  TP-L16.
           03 FILLER             PIC X(21) VALUE
              "    VENDIDO EM 1984: ".
           03 TP16-QTD           PIC ZZZ,ZZ9.
           03 FILLER             PIC X(15) VALUE " UN     VALOR: ".
           03 TP16-VLR           PIC Z,ZZZ,ZZ9.99.
       01  TP-MSG.
           03 FILLER             PIC X VALUE SPACE.
           03 TPM-TXT            PIC X(76).
       01  TP-OPCOES.
           03 FILLER             PIC X(30) VALUE
              " 1-INCLUI  2-CONSULTA  3-ALTER".
           03 FILLER             PIC X(24) VALUE
              "A  4-INATIVA  X-RETORNA".
       01  ED-VALOR              PIC ZZZ,ZZ9.99.
       01  ED-MARGEM             PIC -ZZZ9.9.
       01  ED-FORN               PIC 9(5).
       01  TL-MSG               PIC X(76) VALUE SPACES.
       01  TL-MSG-R REDEFINES TL-MSG.
           03 TL-MSG-1          PIC X.
           03 FILLER            PIC X(75).
       01  TL-PROMPT            PIC X(40) VALUE SPACES.
       01  TL-TRACO             PIC X(79) VALUE ALL "-".
       01  TL-TITULO            PIC X(78) VALUE SPACES.
       01  TL-TIT-PADRAO.
           03 FILLER            PIC X(39) VALUE
              "PRODUTOS / ESTOQUE                     ".
           03 FILLER            PIC X(39) VALUE
              "                       RETROLABS ACME  ".
       01  TL-RODAPE            PIC X(79) VALUE SPACES.
       01  TL-RODAPE-R REDEFINES TL-RODAPE.
           03 TL-ROD-A          PIC X(34).
           03 TL-ROD-B          PIC X(45).
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
       01  EC-TABV.
           03 FILLER            PIC X(40) VALUE
              "PART NUMBER:                            ".
           03 FILLER            PIC X(28) VALUE
              "PART NUMBER           0345XN".
           03 FILLER            PIC X(40) VALUE
              "DESCRICAO:                              ".
           03 FILLER            PIC X(28) VALUE
              "DESCRICAO             0415XN".
           03 FILLER            PIC X(40) VALUE
              "FABRICANTE:                             ".
           03 FILLER            PIC X(28) VALUE
              "FABRICANTE            0515XN".
           03 FILLER            PIC X(40) VALUE
              "GRUPO:                                  ".
           03 FILLER            PIC X(28) VALUE
              "GRUPO                 0545IN".
           03 FILLER            PIC X(40) VALUE
              "LOCAL NO ALMOXARIFADO:                  ".
           03 FILLER            PIC X(28) VALUE
              "LOCAL NO ALMOXARIFADO 0560XN".
           03 FILLER            PIC X(40) VALUE
              "ESTOQUE MINIMO:                         ".
           03 FILLER            PIC X(28) VALUE
              "ESTOQUE MINIMO        1219IN".
           03 FILLER            PIC X(40) VALUE
              "ESTOQUE MAXIMO:                         ".
           03 FILLER            PIC X(28) VALUE
              "ESTOQUE MAXIMO        1319IN".
           03 FILLER            PIC X(40) VALUE
              "PONTO DE PEDIDO:                        ".
           03 FILLER            PIC X(28) VALUE
              "PONTO DE PEDIDO       1419IN".
           03 FILLER            PIC X(40) VALUE
              "PRECO DE VENDA:                         ".
           03 FILLER            PIC X(28) VALUE
              "PRECO DE VENDA        1156MN".
           03 FILLER            PIC X(40) VALUE
              "FORNECEDOR PRINCIPAL:                   ".
           03 FILLER            PIC X(28) VALUE
              "FORNECEDOR PRINCIPAL  1356IN".
           03 FILLER            PIC X(40) VALUE
              "SITUACAO (A=ATIVO  B=INATIVO):          ".
           03 FILLER            PIC X(28) VALUE
              "SITUACAO              1457SN".
       01  EC-TAB REDEFINES EC-TABV.
           03 EC-D              OCCURS 11 TIMES.
              05 EC-PROMPT      PIC X(40).
              05 EC-LAB         PIC X(22).
              05 EC-LIN         PIC 99.
              05 EC-COL         PIC 99.
              05 EC-TIPO        PIC X.
              05 EC-INC         PIC X.
       01  EC-I                 PIC 99 VALUE ZERO.
       01  EC-J                 PIC 99 VALUE ZERO.
       01  EC-ORD               PIC 99 VALUE ZERO.
       01  EC-TOT               PIC 99 VALUE ZERO.
       01  EC-X                 PIC X(40) VALUE SPACES.
       01  EC-N                 PIC 9(10)V99 VALUE ZERO.
       01  EC-SHOW              PIC X(38) VALUE SPACES.
       01  EC-ED-I              PIC Z(9)9.
       01  EC-ED-M              PIC ZZ,ZZZ,ZZ9.99.
       01  EC-ED-D              PIC Z(8)9.9.
       01  EC-ED-ORD            PIC Z9.
       01  EC-ED-TOT            PIC Z9.
       01  EC-NUM               PIC 99 VALUE 11.
       01  WS-FIM               PIC X VALUE "N".
       01  WS-MODO              PIC X VALUE "A".
       01  WS-CANCELA           PIC X VALUE "N".
       01  TL-ENT               PIC X(40) VALUE SPACES.
       01  TL-ENT-R REDEFINES TL-ENT.
           03 TL-ENT-C          PIC X OCCURS 40 TIMES.
       01  TL-DIGITOS           PIC X(10) VALUE "0123456789".
       01  TL-DIGITOS-R REDEFINES TL-DIGITOS.
           03 TL-DIGITO         PIC X OCCURS 10 TIMES.
       01  TL-CH                PIC X.
       01  TL-D                 PIC 99.
       01  TL-INT               PIC 9(12).
       01  TL-NUM               PIC 9(10)V99.
       01  TL-NDEC              PIC 99.
       01  TL-SEP               PIC 99.
       01  TL-NUM-OK            PIC X VALUE "S".
       01  TL-M-INV             PIC X(43) VALUE
           "*** VALOR INVALIDO - MANTIDO O ANTERIOR ***".
       01  TL-ROD-EDICAO.
           03 FILLER            PIC X(39) VALUE
              " RETURN = MANTEM O VALOR ATUAL     DIGI".
           03 FILLER            PIC X(39) VALUE
              "TE PARA TROCAR   * = CANCELA A EDICAO  ".
           03 FILLER            PIC X(1) VALUE
              " ".
       01  WS-NALT              PIC 99 VALUE ZERO.
       01  WS-SEMALT            PIC X VALUE "N".
       01  NV-MUDOU             PIC X VALUE "N".
       01  NV-ST                PIC X(10) VALUE SPACES.
       01  NV-PREVL             PIC X(22) VALUE SPACES.
       01  NV-PERG-ALT.
           03 NV-ED-NALT        PIC Z9.
           03 FILLER            PIC X(36) VALUE
              " CAMPO(S) ALTERADO(S) - GRAVA (S/N)?".
       01  NV-POS               PIC 9999 VALUE ZERO.
       01  NV-N                 PIC 9999 VALUE ZERO.
       01  NV-I                 PIC 9999 VALUE ZERO.
       01  NV-J                 PIC 9999 VALUE ZERO.
       01  NV-K                 PIC 9(6) VALUE ZERO.
       01  NV-FIM               PIC X VALUE "N".
       01  NV-ACHOU             PIC X VALUE "N".
       01  NV-TAB-OK            PIC X VALUE "N".
       01  NV-CHEIA             PIC X VALUE "S".
       01  NV-TELA-OK           PIC X VALUE "N".
       01  NV-KSAVE             PIC X(7).
       01  NV-KATU              PIC X(7) VALUE SPACES.
       01  NV-ED-POS            PIC ZZZ9.
       01  NV-ED-N              PIC ZZZ9.
       01  NV-ENT               PIC X(20) VALUE SPACES.
       01  NV-ENT-R REDEFINES NV-ENT.
           03 NV-ENT-C          PIC X OCCURS 20 TIMES.
       01  NV-MSG-NC.
           03 FILLER            PIC X(4) VALUE "*** ".
           03 NV-MK             PIC X(7).
           03 FILLER            PIC X(35) VALUE
              " NAO EXISTE - CONFIRA O CODIGO    ".
           03 FILLER            PIC X(3) VALUE "***".
       01  NV-ROD1              PIC X(34) VALUE
           " = PROXIMO  I INICIO              ".
       01  NV-TAB.
           03 NV-TK             PIC X(7) OCCURS 1 TIMES.
       01  NV-COPIA             PIC X(188).
       01  NV-ROD2.
           03 FILLER            PIC X(39) VALUE
              " 1 INCLUI   3 ALTERA   4 INATIVA   X SA".
           03 FILLER            PIC X(5) VALUE
              "I    ".
       01  TL-M-INAT            PIC X(30) VALUE
           "*** ERRO NA INATIVACAO ***".
       01  TL-M-ALT             PIC X(30) VALUE
           "*** ERRO NA ALTERACAO ***".
       01  TL-M-GRAV            PIC X(30) VALUE
           "*** ERRO NA GRAVACAO ***".
       01  TL-M-EXC             PIC X(30) VALUE
           "*** ERRO NA EXCLUSAO ***".
       01  TL-TIT-INC.
           03 FILLER            PIC X(39) VALUE
              " PRODUTOS / ESTOQUE - INCLUSAO         ".
           03 FILLER            PIC X(39) VALUE
              "                                       ".
       01  TL-TIT-ALT.
           03 FILLER            PIC X(39) VALUE
              " PRODUTOS / ESTOQUE - ALTERACAO        ".
           03 FILLER            PIC X(39) VALUE
              "                                       ".
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
       PROCEDURE DIVISION.
      *V21: FICHA SEMPRE NA TELA; RETURN=PROXIMO, - ANTERIOR,
      *I INICIO, F FIM, CODIGO DIRETO; BARRA DE POSICAO; SO OS
      *VALORES SAO REDESENHADOS; EDICAO CAMPO A CAMPO COM PROGRESSO.
       INICIO.
           MOVE TL-TIT-PADRAO TO TL-TITULO.
           OPEN I-O ARQ-PROD.
           PERFORM NV-CARREGA THRU NV-CC-FIM.
           PERFORM LIMPA-REGISTRO.
           MOVE ZERO TO NV-POS.
           IF NV-N > ZERO
              MOVE 1 TO NV-POS
              PERFORM NV-LE-POS.
           MOVE "N" TO NV-TELA-OK.
           MOVE SPACE TO WS-OPCAO.
           PERFORM MENU UNTIL WS-OPCAO = "X".
           CLOSE ARQ-PROD.
           GO TO FIM-PROGRAMA.
       MENU.
           PERFORM TELA-FICHA.
           MOVE "OPCAO OU CODIGO:" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           MOVE SPACES TO TL-MSG.
           PERFORM NV-LE-OPCAO.
           IF WS-OPCAO = "1" PERFORM INCLUIR THRU INC-FIM.
           IF WS-OPCAO = "2" PERFORM CONSULTAR.
           IF WS-OPCAO = "3" PERFORM ALTERAR THRU ALT-FIM.
           IF WS-OPCAO = "4" PERFORM EXCLUIR THRU EXC-FIM.
           PERFORM NV-ACAO.
       TELA-FICHA.
           MOVE "N" TO NV-CHEIA.
           IF NV-TELA-OK = "N"
              MOVE "S" TO NV-CHEIA
              MOVE "S" TO NV-TELA-OK
              MOVE "S" TO NV-CHEIA.
           MOVE "S" TO WS-TEM.
           MOVE SPACES TO WS-MSG.
           PERFORM FICHA THRU FICHA-FIM.
           IF TL-MSG = SPACES MOVE WS-MSG TO TL-MSG.
           PERFORM TL-MSG-P.
           PERFORM NV-BARRA-P THRU NV-BP-FIM.
           IF NV-CHEIA = "S" PERFORM NV-RODAPE-P.
       LIMPA-REGISTRO.
           MOVE SPACES TO REG-PRODUTO.
           MOVE ZERO TO PR-CODIGO.
           MOVE ZERO TO PR-GRUPO.
           MOVE ZERO TO PR-ESTOQUE.
           MOVE ZERO TO PR-RESERVADO.
           MOVE ZERO TO PR-MINIMO.
           MOVE ZERO TO PR-MAXIMO.
           MOVE ZERO TO PR-PONTO-PEDIDO.
           MOVE ZERO TO PR-EM-COMPRA.
           MOVE ZERO TO PR-CUSTO-MEDIO.
           MOVE ZERO TO PR-ULT-CUSTO.
           MOVE ZERO TO PR-PRECO-VENDA.
           MOVE ZERO TO PR-FORNEC-PRINC.
           MOVE ZERO TO PR-DT-ULT-ENT.
           MOVE ZERO TO PR-DT-ULT-VEN.
           MOVE ZERO TO PR-QTD-VEND-ANO.
           MOVE ZERO TO PR-VLR-VEND-ANO.
           MOVE "PC" TO PR-UNIDADE.
           MOVE "A" TO PR-SITUACAO.
       LER-CHAVE.
           MOVE "CODIGO DO PRODUTO:" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           PERFORM LC-LE-CODIGO.
           MOVE LC-NUM TO PR-CODIGO.
       CONSULTAR.
           PERFORM LER-CHAVE.
           PERFORM NV-VAI-CHAVE.
       VOLTA-ATUAL.
           IF NV-POS > ZERO
              PERFORM NV-LE-POS
           ELSE
              PERFORM LIMPA-REGISTRO.
       INCLUIR.
           PERFORM LER-CHAVE.
           MOVE PR-CHAVE TO NV-KSAVE.
           MOVE "S" TO NV-ACHOU.
           READ ARQ-PROD
               INVALID KEY MOVE "N" TO NV-ACHOU.
           IF NV-ACHOU = "S"
              PERFORM NV-ACHA
              PERFORM NV-LE-POS
              MOVE "*** PRODUTO JA CADASTRADO ***" TO TL-MSG
              GO TO INC-FIM.
           PERFORM LIMPA-REGISTRO.
           MOVE NV-KSAVE TO PR-CHAVE.
           MOVE "I" TO WS-MODO.
           MOVE TL-TIT-INC TO TL-TITULO.
           PERFORM EDICAO THRU ED-FIM.
           MOVE TL-TIT-PADRAO TO TL-TITULO.
           IF WS-CANCELA = "S"
              MOVE "INCLUSAO CANCELADA" TO TL-MSG
              PERFORM VOLTA-ATUAL
              GO TO INC-FIM.
           MOVE "PRODUTO INCLUIDO" TO TL-MSG.
           WRITE REG-PRODUTO
              INVALID KEY MOVE TL-M-GRAV TO TL-MSG.
           IF TL-MSG = TL-M-GRAV
              PERFORM VOLTA-ATUAL
           ELSE
              PERFORM NV-INSERE.
       INC-FIM.
           EXIT.
       ALTERAR.
           IF NV-POS = ZERO
              MOVE "*** NENHUM REGISTRO NA TELA ***" TO TL-MSG
              GO TO ALT-FIM.
           MOVE "A" TO WS-MODO.
           MOVE REG-PRODUTO TO NV-COPIA.
           MOVE TL-TIT-ALT TO TL-TITULO.
           PERFORM EDICAO THRU ED-FIM.
           MOVE TL-TIT-PADRAO TO TL-TITULO.
           IF WS-CANCELA = "S"
              MOVE NV-COPIA TO REG-PRODUTO
              MOVE "ALTERACAO CANCELADA" TO TL-MSG
              IF WS-SEMALT = "S"
                 MOVE "NENHUM CAMPO ALTERADO" TO TL-MSG
                 GO TO ALT-FIM
              ELSE
                 GO TO ALT-FIM.
           MOVE "PRODUTO ALTERADO" TO TL-MSG.
           REWRITE REG-PRODUTO
              INVALID KEY MOVE TL-M-ALT TO TL-MSG.
       ALT-FIM.
           EXIT.
       EXCLUIR.
           IF NV-POS = ZERO
              MOVE "*** NENHUM REGISTRO NA TELA ***" TO TL-MSG
              GO TO EXC-FIM.
           MOVE "CONFIRMA A INATIVACAO (S/N)?" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           ACCEPT WS-OPCAO.
           IF WS-OPCAO = "s" MOVE "S" TO WS-OPCAO.
           IF WS-OPCAO NOT = "S"
              MOVE "OPERACAO CANCELADA" TO TL-MSG
              GO TO EXC-FIM.
           MOVE "B" TO PR-SITUACAO.
           MOVE "PRODUTO INATIVADO" TO TL-MSG.
           REWRITE REG-PRODUTO
              INVALID KEY MOVE TL-M-INAT TO TL-MSG.
       EXC-FIM.
           MOVE SPACE TO WS-OPCAO.
      *---- EDICAO CAMPO A CAMPO SOBRE A FICHA
       EDICAO.
           MOVE "N" TO WS-CANCELA.
           MOVE "N" TO WS-SEMALT.
           MOVE ZERO TO WS-NALT.
           MOVE "INICIO DA FICHA" TO NV-PREVL.
           MOVE SPACES TO NV-ST.
           MOVE "S" TO NV-CHEIA.
              MOVE "S" TO NV-CHEIA.
           MOVE "S" TO WS-TEM.
           MOVE SPACES TO WS-MSG.
           PERFORM FICHA THRU FICHA-FIM.
           IF TL-MSG = SPACES MOVE WS-MSG TO TL-MSG.
           MOVE 21 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA.
           MOVE TL-ROD-EDICAO TO TL-RODAPE.
           PERFORM TL-RODAPE-P.
           PERFORM EC-EDITA.
           PERFORM CONFIRMA-GRAV THRU CG-FIM.
           MOVE "N" TO NV-TELA-OK.
       ED-FIM.
           EXIT.
      *---- EDICAO CAMPO A CAMPO DIRIGIDA POR TABELA (V23)
       EC-EDITA.
           MOVE ZERO TO EC-TOT.
           MOVE 1 TO EC-I.
           PERFORM EC-CONTA UNTIL EC-I > EC-NUM.
           MOVE ZERO TO EC-ORD.
           MOVE 1 TO EC-I.
           PERFORM EC-CAMPO THRU EC-CF UNTIL EC-I > EC-NUM.
       EC-CONTA.
           IF EC-INC (EC-I) = "N"
              ADD 1 TO EC-TOT
           ELSE
              IF WS-MODO = "I" ADD 1 TO EC-TOT.
           ADD 1 TO EC-I.
       EC-CAMPO.
           IF WS-CANCELA = "S"
              MOVE 90 TO EC-I
              GO TO EC-CF.
           IF EC-INC (EC-I) = "S"
              IF WS-MODO NOT = "I" GO TO EC-CF.
           ADD 1 TO EC-ORD.
           MOVE "N" TO NV-MUDOU.
           PERFORM EC-GET THRU EC-GET-FIM.
           PERFORM EC-FORMATA.
           MOVE EC-ORD TO EC-ED-ORD.
           MOVE EC-TOT TO EC-ED-TOT.
           MOVE 1 TO VT-LIN.
           MOVE 60 TO VT-COL.
           DISPLAY VT-POS VT-INV "CAMPO " EC-ED-ORD " DE " EC-ED-TOT " "
                   VT-NORMAL.
           MOVE 20 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA " " VT-FORTE "ANTERIOR:" VT-NORMAL
                   " " NV-ST " " NV-PREVL.
           MOVE 21 TO VT-LIN.
           DISPLAY VT-POS VT-APAGA VT-INV " ATUAL: " EC-SHOW
                   " (RETURN MANTEM, * CANCELA) " VT-NORMAL.
           MOVE EC-LIN (EC-I) TO VT-LIN.
           SUBTRACT 1 FROM EC-COL (EC-I) GIVING VT-COL.
           DISPLAY VT-POS VT-INV VT-PISCA ">" VT-NORMAL VT-HOME.
           MOVE EC-PROMPT (EC-I) TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           MOVE SPACES TO TL-ENT.
           ACCEPT TL-ENT.
           MOVE EC-LIN (EC-I) TO VT-LIN.
           SUBTRACT 1 FROM EC-COL (EC-I) GIVING VT-COL.
           DISPLAY VT-POS " " VT-HOME.
           IF TL-ENT = "*"
              MOVE "S" TO WS-CANCELA
              GO TO EC-CF.
           IF TL-ENT NOT = SPACES PERFORM EC-APLICA.
           IF NV-MUDOU = "S"
              PERFORM EC-PUT THRU EC-PUT-FIM
              ADD 1 TO WS-NALT
              MOVE "(ALTERADO)" TO NV-ST
              MOVE "N" TO NV-CHEIA
              MOVE "S" TO WS-TEM
              MOVE SPACES TO WS-MSG
              PERFORM FICHA THRU FICHA-FIM
              IF TL-MSG = SPACES MOVE WS-MSG TO TL-MSG
           ELSE
              MOVE "(MANTIDO) " TO NV-ST.
           MOVE EC-LAB (EC-I) TO NV-PREVL.
       EC-CF.
           ADD 1 TO EC-I.
       EC-APLICA.
           MOVE TL-ENT TO LC-ENT.
           IF EC-TIPO (EC-I) = "X"
              MOVE TL-ENT TO EC-X
              MOVE "S" TO NV-MUDOU.
           IF EC-TIPO (EC-I) = "I"
              PERFORM LC-CONVERTE
              IF LC-OK = "S"
                 MOVE LC-NUM TO EC-N
                 MOVE "S" TO NV-MUDOU.
           IF EC-TIPO (EC-I) = "T"
              PERFORM LC-CONVERTE
              IF LC-OK = "S"
                 IF LC-ND = 6
                    PERFORM LC-MONTA-DATA
                    MOVE LC-DATA TO EC-N
                    MOVE "S" TO NV-MUDOU.
           IF EC-TIPO (EC-I) = "M"
              PERFORM LC-CONV-VALOR
              IF LC-OK = "S"
                 MOVE LC-VAL TO EC-N
                 MOVE "S" TO NV-MUDOU.
           IF EC-TIPO (EC-I) = "D"
              PERFORM LC-CONV-VALOR
              IF LC-OK = "S"
                 MOVE LC-VAL TO EC-N
                 MOVE "S" TO NV-MUDOU.
           IF LC-C (1) = "a" MOVE "A" TO LC-C (1).
           IF LC-C (1) = "b" MOVE "B" TO LC-C (1).
           IF LC-C (1) = "s" MOVE "S" TO LC-C (1).
           IF LC-C (1) = "n" MOVE "N" TO LC-C (1).
           IF EC-TIPO (EC-I) = "S"
              IF LC-C (1) = "A"
                 MOVE "A" TO EC-X
                 MOVE "S" TO NV-MUDOU
              ELSE
                 IF LC-C (1) = "B"
                    MOVE "B" TO EC-X
                    MOVE "S" TO NV-MUDOU.
           IF EC-TIPO (EC-I) = "P"
              IF LC-C (1) = "S"
                 MOVE "S" TO EC-X
                 MOVE "S" TO NV-MUDOU
              ELSE
                 IF LC-C (1) = "N"
                    MOVE "N" TO EC-X
                    MOVE "S" TO NV-MUDOU.
           IF NV-MUDOU = "N"
              MOVE TL-M-INV TO TL-MSG
              PERFORM TL-MSG-P
              MOVE SPACES TO TL-MSG.
       EC-FORMATA.
           MOVE EC-X TO EC-SHOW.
           IF EC-TIPO (EC-I) = "I"
              MOVE EC-N TO EC-ED-I
              MOVE EC-ED-I TO EC-SHOW.
           IF EC-TIPO (EC-I) = "M"
              MOVE EC-N TO EC-ED-M
              MOVE EC-ED-M TO EC-SHOW.
           IF EC-TIPO (EC-I) = "D"
              MOVE EC-N TO EC-ED-D
              MOVE EC-ED-D TO EC-SHOW.
           IF EC-TIPO (EC-I) = "T"
              MOVE EC-N TO LC-DATA
              PERFORM LC-FMT
              MOVE LC-DT-ED TO EC-SHOW.
           IF EC-TIPO (EC-I) = "S"
              MOVE "ATIVO" TO EC-SHOW
              IF EC-X = "B" MOVE "INATIVO" TO EC-SHOW.
           IF EC-TIPO (EC-I) = "P"
              MOVE "NAO" TO EC-SHOW
              IF EC-X = "S" MOVE "SIM" TO EC-SHOW.
       EC-GET.
           IF EC-I = 1
              GO TO EC-G01.
           IF EC-I = 2
              GO TO EC-G02.
           IF EC-I = 3
              GO TO EC-G03.
           IF EC-I = 4
              GO TO EC-G04.
           IF EC-I = 5
              GO TO EC-G05.
           IF EC-I = 6
              GO TO EC-G06.
           IF EC-I = 7
              GO TO EC-G07.
           IF EC-I = 8
              GO TO EC-G08.
           IF EC-I = 9
              GO TO EC-G09.
           IF EC-I = 10
              GO TO EC-G10.
           IF EC-I = 11
              GO TO EC-G11.
           GO TO EC-GET-FIM.
       EC-G01.
           MOVE PR-PART-NUMBER TO EC-X.
           GO TO EC-GET-FIM.
       EC-G02.
           MOVE PR-DESCRICAO TO EC-X.
           GO TO EC-GET-FIM.
       EC-G03.
           MOVE PR-FABRICANTE TO EC-X.
           GO TO EC-GET-FIM.
       EC-G04.
           MOVE PR-GRUPO TO EC-N.
           GO TO EC-GET-FIM.
       EC-G05.
           MOVE PR-LOCAL TO EC-X.
           GO TO EC-GET-FIM.
       EC-G06.
           MOVE PR-MINIMO TO EC-N.
           GO TO EC-GET-FIM.
       EC-G07.
           MOVE PR-MAXIMO TO EC-N.
           GO TO EC-GET-FIM.
       EC-G08.
           MOVE PR-PONTO-PEDIDO TO EC-N.
           GO TO EC-GET-FIM.
       EC-G09.
           MOVE PR-PRECO-VENDA TO EC-N.
           GO TO EC-GET-FIM.
       EC-G10.
           MOVE PR-FORNEC-PRINC TO EC-N.
           GO TO EC-GET-FIM.
       EC-G11.
           MOVE PR-SITUACAO TO EC-X.
           GO TO EC-GET-FIM.
       EC-GET-FIM.
           EXIT.
       EC-PUT.
           IF EC-I = 1
              GO TO EC-P01.
           IF EC-I = 2
              GO TO EC-P02.
           IF EC-I = 3
              GO TO EC-P03.
           IF EC-I = 4
              GO TO EC-P04.
           IF EC-I = 5
              GO TO EC-P05.
           IF EC-I = 6
              GO TO EC-P06.
           IF EC-I = 7
              GO TO EC-P07.
           IF EC-I = 8
              GO TO EC-P08.
           IF EC-I = 9
              GO TO EC-P09.
           IF EC-I = 10
              GO TO EC-P10.
           IF EC-I = 11
              GO TO EC-P11.
           GO TO EC-PUT-FIM.
       EC-P01.
           MOVE EC-X TO PR-PART-NUMBER.
           GO TO EC-PUT-FIM.
       EC-P02.
           MOVE EC-X TO PR-DESCRICAO.
           GO TO EC-PUT-FIM.
       EC-P03.
           MOVE EC-X TO PR-FABRICANTE.
           GO TO EC-PUT-FIM.
       EC-P04.
           MOVE EC-N TO PR-GRUPO.
           GO TO EC-PUT-FIM.
       EC-P05.
           MOVE EC-X TO PR-LOCAL.
           GO TO EC-PUT-FIM.
       EC-P06.
           MOVE EC-N TO PR-MINIMO.
           GO TO EC-PUT-FIM.
       EC-P07.
           MOVE EC-N TO PR-MAXIMO.
           GO TO EC-PUT-FIM.
       EC-P08.
           MOVE EC-N TO PR-PONTO-PEDIDO.
           GO TO EC-PUT-FIM.
       EC-P09.
           MOVE EC-N TO PR-PRECO-VENDA.
           GO TO EC-PUT-FIM.
       EC-P10.
           MOVE EC-N TO PR-FORNEC-PRINC.
           GO TO EC-PUT-FIM.
       EC-P11.
           MOVE EC-X TO PR-SITUACAO.
           GO TO EC-PUT-FIM.
       EC-PUT-FIM.
           EXIT.

       EC-FIM.
           EXIT.
       CONFIRMA-GRAV.
           MOVE 20 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA.
           MOVE 21 TO VT-LIN.
           DISPLAY VT-POS VT-APAGA.
           IF WS-CANCELA = "S" GO TO CG-FIM.
           IF WS-MODO = "A"
              IF WS-NALT = ZERO
                 MOVE "S" TO WS-CANCELA
                 MOVE "S" TO WS-SEMALT
                 GO TO CG-FIM.
           MOVE WS-NALT TO NV-ED-NALT.
           IF WS-MODO = "A"
              MOVE NV-PERG-ALT TO TL-PROMPT
           ELSE
              MOVE "CONFIRMA A INCLUSAO (S/N)?" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           MOVE SPACES TO TL-ENT.
           ACCEPT TL-ENT.
           IF TL-ENT-C (1) = "s" MOVE "S" TO TL-ENT-C (1).
           IF TL-ENT-C (1) NOT = "S" MOVE "S" TO WS-CANCELA.
       CG-FIM.
           EXIT.
      *---- NAVEGACAO SEQUENCIAL (SEM TABELA NA MEMORIA)
      *RETURN=PROXIMO, I=INICIO, CODIGO DIRETO (MOSTRA O SEGUINTE).
       NV-CARREGA.
           MOVE ZERO TO NV-N.
           MOVE ZERO TO NV-POS.
           MOVE LOW-VALUES TO PR-CHAVE.
           START ARQ-PROD KEY IS NOT < PR-CHAVE
              INVALID KEY GO TO NV-CC-FIM.
           MOVE "N" TO NV-FIM.
           READ ARQ-PROD NEXT RECORD
              AT END MOVE "S" TO NV-FIM.
           IF NV-FIM = "N"
              MOVE 1 TO NV-N
              MOVE PR-CHAVE TO NV-KATU.
       NV-CC-FIM.
           EXIT.
       NV-LE-POS.
           MOVE NV-KATU TO PR-CHAVE.
           MOVE "S" TO NV-ACHOU.
           READ ARQ-PROD
              INVALID KEY MOVE "N" TO NV-ACHOU.
           PERFORM NV-APOS-LER.
       NV-APOS-LER.
           IF NV-ACHOU = "S" MOVE PR-CHAVE TO NV-KATU.
           IF NV-ACHOU = "S"
              IF PR-SITUACAO = "B"
                 MOVE "*** PRODUTO INATIVO ***"
                   TO TL-MSG.
           IF NV-ACHOU = "N" PERFORM LIMPA-REGISTRO.
           MOVE SPACE TO NV-FIM.
       NV-LE-OPCAO.
           MOVE SPACES TO NV-ENT.
           ACCEPT NV-ENT.
           MOVE SPACE TO WS-OPCAO.
           IF NV-ENT = SPACES
              MOVE "6" TO WS-OPCAO
           ELSE
              IF NV-ENT-C (2) = SPACE
                 MOVE NV-ENT-C (1) TO WS-OPCAO
              ELSE
                 MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "x" MOVE "X" TO WS-OPCAO.
           IF WS-OPCAO = "i" MOVE "I" TO WS-OPCAO.
           IF WS-OPCAO = "0" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "5" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "6" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "7" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "8" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "9" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "+" MOVE "6" TO WS-OPCAO.
           IF WS-OPCAO = "=" MOVE "6" TO WS-OPCAO.
       NV-ACAO.
           IF WS-OPCAO = "6" PERFORM NV-PROXIMO.
           IF WS-OPCAO = "I" PERFORM NV-PRIMEIRO.
           IF WS-OPCAO = "K"
              PERFORM NV-K-CONV
              PERFORM NV-VAI-CHAVE.
       NV-K-CONV.
           MOVE NV-ENT TO LC-ENT.
           PERFORM LC-CONVERTE.
           IF LC-OK = "S"
              MOVE LC-NUM TO PR-CODIGO
           ELSE
              MOVE NV-ENT TO PR-CHAVE.
       NV-VAI-CHAVE.
           MOVE PR-CHAVE TO NV-KSAVE.
           MOVE "S" TO NV-ACHOU.
           READ ARQ-PROD
              INVALID KEY MOVE "N" TO NV-ACHOU.
           IF NV-ACHOU = "S"
              PERFORM NV-APOS-LER
           ELSE
              PERFORM NV-SEGUINTE.
       NV-SEGUINTE.
           MOVE NV-KSAVE TO PR-CHAVE.
           MOVE "S" TO NV-ACHOU.
           START ARQ-PROD KEY IS NOT < PR-CHAVE
              INVALID KEY MOVE "N" TO NV-ACHOU.
           IF NV-ACHOU = "S"
              READ ARQ-PROD NEXT RECORD
                 AT END MOVE "N" TO NV-ACHOU.
           IF NV-ACHOU = "S"
              PERFORM NV-APOS-LER
           ELSE
              PERFORM NV-PRIMEIRO.
           MOVE NV-KSAVE TO NV-MK.
           MOVE NV-MSG-NC TO TL-MSG.
       NV-PROXIMO.
           IF NV-N = ZERO
              MOVE "*** ARQUIVO VAZIO ***" TO TL-MSG
           ELSE
              MOVE NV-KATU TO PR-CHAVE
              MOVE "S" TO NV-ACHOU
              START ARQ-PROD KEY IS GREATER THAN PR-CHAVE
                 INVALID KEY MOVE "N" TO NV-ACHOU.
           IF NV-N > ZERO
              IF NV-ACHOU = "S"
                 READ ARQ-PROD NEXT RECORD
                    AT END MOVE "N" TO NV-ACHOU.
           IF NV-N > ZERO
              IF NV-ACHOU = "S"
                 PERFORM NV-APOS-LER
              ELSE
                 PERFORM NV-PRIMEIRO
                 MOVE "FIM DO ARQUIVO - VOLTOU AO PRIMEIRO REGISTRO"
                   TO TL-MSG.
       NV-PRIMEIRO.
           PERFORM NV-CARREGA THRU NV-CC-FIM.
           IF NV-N > ZERO
              MOVE 1 TO NV-POS
              PERFORM NV-LE-POS.
       NV-ACHA.
           MOVE PR-CHAVE TO NV-KATU.
           MOVE 1 TO NV-POS.
           MOVE "S" TO NV-TAB-OK.
       NV-INSERE.
           MOVE PR-CHAVE TO NV-KATU.
           MOVE 1 TO NV-N.
           MOVE 1 TO NV-POS.
           PERFORM NV-LE-POS.
       NV-REMOVE.
           MOVE NV-KATU TO NV-KSAVE.
           PERFORM NV-PROXIMO.
       NV-BARRA-P.
           MOVE 21 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA.
       NV-BP-FIM.
           EXIT.
       NV-RODAPE-P.
           MOVE 24 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-INV NV-ROD1 VT-NORMAL " " VT-INV NV-ROD2
                   VT-NORMAL VT-HOME.
       FICHA.
           IF NV-CHEIA = "S"
              MOVE 1 TO VT-LIN
              MOVE 1 TO VT-COL
              DISPLAY VT-POS VT-APAGA
                 VT-LIMPA VT-INV " " TP-L1 "    " VT-NORMAL.
           IF NV-CHEIA = "S"
              MOVE 2 TO VT-LIN
              MOVE 1 TO VT-COL
              DISPLAY VT-POS VT-APAGA
                 TP-TRACO.
           IF WS-TEM NOT = "S" GO TO FICHA-VAZIA.
           MOVE ZERO TO WS-DISPONIVEL.
           IF PR-ESTOQUE > PR-RESERVADO
              SUBTRACT PR-RESERVADO FROM PR-ESTOQUE
                 GIVING WS-DISPONIVEL.
           MOVE PR-CODIGO TO TP3-COD.
           MOVE PR-PART-NUMBER TO TP3-PN.
           MOVE 3 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA
              TP-L3.
           MOVE PR-DESCRICAO TO TP4-DESC.
           MOVE 4 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA
              TP-L4.
           MOVE PR-FABRICANTE TO TP5-FAB.
           MOVE PR-GRUPO TO TP5-GRUPO.
           MOVE PR-LOCAL TO TP5-LOCAL.
           MOVE 5 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA
              TP-L5.
           IF NV-CHEIA = "S"
              MOVE 6 TO VT-LIN
              MOVE 1 TO VT-COL
              DISPLAY VT-POS VT-APAGA
                 TP-TRACO.
           MOVE 7 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA
              VT-FORTE TP-L7 VT-NORMAL.
           MOVE 8 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA
              TP-L8.
           MOVE "ATUAL......." TO TD-ROT1.
           MOVE PR-ESTOQUE TO TD-VAL1.
           MOVE "CUSTO MEDIO.." TO TD-ROT2.
           MOVE PR-CUSTO-MEDIO TO ED-VALOR.
           MOVE ED-VALOR TO TD-VAL2.
           MOVE 9 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA
              TP-DUP.
           MOVE "RESERVADO..." TO TD-ROT1.
           MOVE PR-RESERVADO TO TD-VAL1.
           MOVE "ULTIMO CUSTO." TO TD-ROT2.
           MOVE PR-ULT-CUSTO TO ED-VALOR.
           MOVE ED-VALOR TO TD-VAL2.
           MOVE 10 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA
              TP-DUP.
           MOVE "DISPONIVEL.." TO TD-ROT1.
           MOVE WS-DISPONIVEL TO TD-VAL1.
           MOVE "PRECO VENDA.." TO TD-ROT2.
           MOVE PR-PRECO-VENDA TO ED-VALOR.
           MOVE ED-VALOR TO TD-VAL2.
           MOVE 11 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA
              "    " TD-ROT1 ": " VT-INV TD-VAL1 VT-NORMAL
              "               " TD-ROT2 ": " VT-FORTE TD-VAL2
              VT-NORMAL.
           MOVE "MINIMO......" TO TD-ROT1.
           MOVE PR-MINIMO TO TD-VAL1.
           MOVE "MARGEM......." TO TD-ROT2.
           PERFORM CALCULA-MARGEM THRU MARGEM-FIM.
           MOVE 12 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA
              TP-DUP.
           MOVE "MAXIMO......" TO TD-ROT1.
           MOVE PR-MAXIMO TO TD-VAL1.
           MOVE "FORNECEDOR..." TO TD-ROT2.
           MOVE PR-FORNEC-PRINC TO ED-FORN.
           MOVE SPACES TO TD-VAL2.
           MOVE ED-FORN TO TD-VAL2.
           MOVE 13 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA
              TP-DUP.
           MOVE "PONTO PEDIDO" TO TD-ROT1.
           MOVE PR-PONTO-PEDIDO TO TD-VAL1.
           MOVE "SITUACAO....." TO TD-ROT2.
           MOVE "ATIVO" TO TD-VAL2.
           IF PR-SITUACAO = "B" MOVE "INATIVO" TO TD-VAL2.
           MOVE 14 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA
              "    " TD-ROT1 ": " TD-VAL1 "               "
              TD-ROT2 ": " VT-INV " " TD-VAL2 VT-NORMAL.
           IF NV-CHEIA = "S"
              MOVE 15 TO VT-LIN
              MOVE 1 TO VT-COL
              DISPLAY VT-POS VT-APAGA
                 " ".
           MOVE PR-QTD-VEND-ANO TO TP16-QTD.
           MOVE PR-VLR-VEND-ANO TO TP16-VLR.
           MOVE 16 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA
              TP-L16.
           IF NV-CHEIA = "S"
              MOVE 17 TO VT-LIN
              MOVE 1 TO VT-COL
              DISPLAY VT-POS VT-APAGA
                 " ".
           IF WS-MSG = SPACES
              IF WS-DISPONIVEL NOT > PR-PONTO-PEDIDO
                 MOVE "*** ESTOQUE NO PONTO DE PEDIDO - REPOR ***"
                   TO WS-MSG.
           GO TO FICHA-FIM.
       FICHA-VAZIA.
           MOVE "S" TO WS-TEM.
       FICHA-FIM.
           IF NV-CHEIA = "S"
              MOVE 18 TO VT-LIN
              MOVE 1 TO VT-COL
              DISPLAY VT-POS VT-APAGA
                 TP-TRACO.
       CALCULA-MARGEM.
           MOVE "        ---" TO TD-VAL2.
           IF PR-CUSTO-MEDIO = ZERO GO TO MARGEM-FIM.
           SUBTRACT PR-CUSTO-MEDIO FROM PR-PRECO-VENDA
              GIVING WS-MARG-A.
           MULTIPLY WS-MARG-A BY 100 GIVING WS-MARG-B.
           DIVIDE PR-CUSTO-MEDIO INTO WS-MARG-B
              GIVING WS-MARGEM.
           MOVE WS-MARGEM TO ED-MARGEM.
           MOVE SPACES TO TD-VAL2.
           MOVE ED-MARGEM TO TD-VAL2.
       MARGEM-FIM.
           EXIT.
       MOSTRA-MSG.
           MOVE 20 TO VT-LIN.
           MOVE 1 TO VT-COL.
           IF WS-MSG = SPACES
              DISPLAY VT-POS VT-APAGA
           ELSE
              IF WS-MSG-1 = "*"
                 DISPLAY VT-POS VT-APAGA VT-INV VT-PISCA " " WS-MSG
                         " " VT-NORMAL
              ELSE
                 DISPLAY VT-POS VT-APAGA VT-INV " " WS-MSG " "
                         VT-NORMAL.
       PERGUNTA.
           MOVE 23 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA.
           MOVE 22 TO VT-LIN.
           DISPLAY VT-POS VT-APAGA VT-FORTE " " WS-PROMPT VT-NORMAL.
       RODAPE.
           MOVE 24 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-INV TP-OPCOES
                   "                         " VT-NORMAL VT-HOME.
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
       LC-FMT.
           MOVE LC-DD TO LC-E-DD.
           MOVE LC-MM TO LC-E-MM.
           MOVE LC-AA TO LC-E-AA.
       FIM-PROGRAMA.
           EXIT PROGRAM.
