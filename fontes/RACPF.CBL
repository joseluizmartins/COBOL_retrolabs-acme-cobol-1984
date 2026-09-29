       IDENTIFICATION DIVISION.
       PROGRAM-ID. RACPF.
      *RETROLABS ACME - FONTES V23
       AUTHOR. JOSE LUIZ MARTINS - RETROLABS.
      *RETROLABS ACME - SISTEMA INTEGRADO COMERCIAL
      *AMBIENTE ALVO: CIS COBOL 4.5 / MP/M / ALTOS 5-15AD
      *VERSAO DOS FONTES: 0.1 - 1984 DEMO

      *V12: DADOS NO DRIVE B: / SEM LOCK (UM TERMINAL POR VEZ)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-PRODF ASSIGN TO "B:PRODF.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PF-CHAVE.
           SELECT ARQ-PROD ASSIGN TO "B:PROD.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PR-CHAVE.
           SELECT ARQ-FOR ASSIGN TO "B:FORN.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FR-CHAVE.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-PRODF RECORD CONTAINS 60 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REG-PRODFORN.
           03 PF-CHAVE.
              05 PF-PRODUTO          PIC 9(7).
              05 PF-FORNECEDOR       PIC 9(5).
           03 PF-PART-FORNEC         PIC X(16).
           03 PF-CUSTO               PIC 9(6)V99.
           03 PF-QTD-MINIMA          PIC 9(6).
           03 PF-PRAZO               PIC 9(3).
           03 PF-ULT-COMPRA          PIC 9(6).
           03 PF-ULT-CUSTO           PIC 9(6)V99.
           03 PF-PREFERENCIAL        PIC X.
       FD  ARQ-PROD RECORD CONTAINS 188 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REG-PROD.
           03 PR-CHAVE.
              05 PR-CODIGO           PIC 9(7).
           03 PR-PART-NUMBER         PIC X(16).
           03 PR-DESCRICAO           PIC X(30).
           03 PR-RESTO               PIC X(135).
       FD  ARQ-FOR RECORD CONTAINS 330 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REG-FOR.
           03 FR-CHAVE.
              05 FR-CODIGO           PIC 9(5).
           03 FR-RAZAO               PIC X(38).
           03 FR-RESTO               PIC X(287).
       WORKING-STORAGE SECTION.
       01  WS-DATA-DRIVE          PIC XX VALUE "B:".
       01  WS-OPCAO                  PIC X.
       01  WS-ACHOU                  PIC X.
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
              "PRODUTO X FORNECEDOR                   ".
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
       01  TL-ROD-PADRAO.
           03 FILLER            PIC X(39) VALUE
              " 1-INCLUI  2-CONSULTA  3-ALTERA  4-EXCL".
           03 FILLER            PIC X(39) VALUE
              "UI  X-RETORNA                          ".
           03 FILLER            PIC X(1) VALUE
              " ".
       01  TL-TIT-INC.
           03 FILLER            PIC X(39) VALUE
              " PRODUTO X FORNECEDOR - INCLUSAO       ".
           03 FILLER            PIC X(39) VALUE
              "                                       ".
       01  TL-TIT-ALT.
           03 FILLER            PIC X(39) VALUE
              " PRODUTO X FORNECEDOR - ALTERACAO      ".
           03 FILLER            PIC X(39) VALUE
              "                                       ".
       01  TL-M-ALT             PIC X(30) VALUE
           "*** ERRO NA ALTERACAO ***".
       01  TL-M-GRAV            PIC X(30) VALUE
           "*** ERRO NA GRAVACAO ***".
       01  TL-M-EXC             PIC X(30) VALUE
           "*** ERRO NA EXCLUSAO ***".
       01  TF-R01.
           03 FILLER            PIC X(14) VALUE
              " PRODUTO....: ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
           03 TF01-1               PIC 9(7).
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(2) VALUE
              "  ".
           03 TF01-2               PIC X(30).
       01  TF-R02.
           03 FILLER            PIC X(14) VALUE
              " FORNECEDOR.: ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
           03 TF02-1               PIC 9(5).
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(4) VALUE
              "    ".
           03 TF02-2               PIC X(38).
       01  TF-R04.
           03 FILLER            PIC X(28) VALUE
              "    PART NUMBER FORNECEDOR: ".
           03 TF04-1               PIC X(16).
           03 FILLER            PIC X(17) VALUE
              "   PREFERENCIAL: ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 FILLER            PIC X(1) VALUE
              " ".
           03 TF04-2               PIC X(3).
           03 FILLER            PIC X(1) VALUE
              " ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  TF-R06.
           03 FILLER            PIC X(20) VALUE
              "    CUSTO.........: ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
           03 TF06-1               PIC ZZZ,ZZ9.99.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(27) VALUE
              "           QTD.MINIMA....: ".
           03 TF06-2               PIC ZZZ,ZZ9.
       01  TF-R07.
           03 FILLER            PIC X(20) VALUE
              "    ULTIMO CUSTO..: ".
           03 TF07-1               PIC ZZZ,ZZ9.99.
           03 FILLER            PIC X(27) VALUE
              "           PRAZO ENTREGA.: ".
           03 TF07-2               PIC ZZZZZZ9.
           03 FILLER            PIC X(5) VALUE
              " DIAS".
       01  TF-R08.
           03 FILLER            PIC X(22) VALUE
              "    ULTIMA COMPRA.:   ".
           03 TF08-1               PIC X(8).
       01  EC-TABV.
           03 FILLER            PIC X(40) VALUE
              "PART NUMBER NO FORNECEDOR:              ".
           03 FILLER            PIC X(28) VALUE
              "PART NUMBER NO FORNECE0629XN".
           03 FILLER            PIC X(40) VALUE
              "CUSTO:                                  ".
           03 FILLER            PIC X(28) VALUE
              "CUSTO                 0821MN".
           03 FILLER            PIC X(40) VALUE
              "QUANTIDADE MINIMA:                      ".
           03 FILLER            PIC X(28) VALUE
              "QUANTIDADE MINIMA     0858IN".
           03 FILLER            PIC X(40) VALUE
              "ULTIMO CUSTO:                           ".
           03 FILLER            PIC X(28) VALUE
              "ULTIMO CUSTO          0921MN".
           03 FILLER            PIC X(40) VALUE
              "PRAZO DE ENTREGA (DIAS):                ".
           03 FILLER            PIC X(28) VALUE
              "PRAZO DE ENTREGA      0958IN".
           03 FILLER            PIC X(40) VALUE
              "ULTIMA COMPRA (AAMMDD):                 ".
           03 FILLER            PIC X(28) VALUE
              "ULTIMA COMPRA         1023TN".
           03 FILLER            PIC X(40) VALUE
              "PREFERENCIAL (S/N):                     ".
           03 FILLER            PIC X(28) VALUE
              "PREFERENCIAL          0663PN".
       01  EC-TAB REDEFINES EC-TABV.
           03 EC-D              OCCURS 7 TIMES.
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
       01  EC-NUM               PIC 99 VALUE 7.
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
       01  NV-KSAVE             PIC X(12).
       01  NV-KATU              PIC X(12) VALUE SPACES.
       01  NV-ED-POS            PIC ZZZ9.
       01  NV-ED-N              PIC ZZZ9.
       01  NV-ENT               PIC X(20) VALUE SPACES.
       01  NV-ENT-R REDEFINES NV-ENT.
           03 NV-ENT-C          PIC X OCCURS 20 TIMES.
       01  NV-MSG-NC.
           03 FILLER            PIC X(4) VALUE "*** ".
           03 NV-MK             PIC X(12).
           03 FILLER            PIC X(35) VALUE
              " NAO EXISTE - CONFIRA O CODIGO    ".
           03 FILLER            PIC X(3) VALUE "***".
       01  NV-ROD1              PIC X(34) VALUE
           " = PROXIMO  I INICIO              ".
       01  NV-TAB.
           03 NV-TK             PIC X(12) OCCURS 1 TIMES.
       01  NV-COPIA             PIC X(60).
       01  NV-ROD2.
           03 FILLER            PIC X(39) VALUE
              " 1 INCLUI 2 BUSCA 3 ALTERA 4 EXCLUI X S".
           03 FILLER            PIC X(5) VALUE
              "AI   ".
       01  WS-SILENCIO          PIC X VALUE "N".
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
           OPEN I-O ARQ-PRODF ARQ-PROD ARQ-FOR.
           PERFORM NV-CARREGA THRU NV-CC-FIM.
           PERFORM LIMPA-REGISTRO.
           MOVE ZERO TO NV-POS.
           IF NV-N > ZERO
              MOVE 1 TO NV-POS
              PERFORM NV-LE-POS.
           MOVE "N" TO NV-TELA-OK.
           MOVE SPACE TO WS-OPCAO.
           PERFORM MENU UNTIL WS-OPCAO = "X".
           CLOSE ARQ-PRODF ARQ-PROD ARQ-FOR.
           GO TO FIM-PROGRAMA.
       MENU.
           PERFORM TELA-FICHA.
           MOVE "OPCAO OU CODIGO DO PRODUTO:" TO TL-PROMPT.
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
              PERFORM TL-TITULO-P.
           PERFORM MOSTRAR.
           PERFORM TL-MSG-P.
           PERFORM NV-BARRA-P THRU NV-BP-FIM.
           IF NV-CHEIA = "S" PERFORM NV-RODAPE-P.
       LIMPA-REGISTRO.
           MOVE SPACES TO REG-PRODFORN.
           MOVE ZERO TO PF-PRODUTO.
           MOVE ZERO TO PF-FORNECEDOR.
           MOVE ZERO TO PF-CUSTO.
           MOVE ZERO TO PF-QTD-MINIMA.
           MOVE ZERO TO PF-PRAZO.
           MOVE ZERO TO PF-ULT-COMPRA.
           MOVE ZERO TO PF-ULT-CUSTO.
           MOVE "N" TO PF-PREFERENCIAL.
       LER-CHAVE.
           MOVE "CODIGO DO PRODUTO:" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           PERFORM LC-LE-CODIGO.
           MOVE LC-NUM TO PF-PRODUTO.
           MOVE "CODIGO DO FORNECEDOR:" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           PERFORM LC-LE-CODIGO.
           MOVE LC-NUM TO PF-FORNECEDOR.
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
           MOVE PF-CHAVE TO NV-KSAVE.
           MOVE "S" TO NV-ACHOU.
           READ ARQ-PRODF
               INVALID KEY MOVE "N" TO NV-ACHOU.
           IF NV-ACHOU = "S"
              PERFORM NV-ACHA
              PERFORM NV-LE-POS
              MOVE "*** RELACAO JA CADASTRADA ***" TO TL-MSG
              GO TO INC-FIM.
           MOVE PF-PRODUTO TO PR-CODIGO.
           READ ARQ-PROD
              INVALID KEY MOVE "*** PRODUTO NAO CADASTRADO ***"
                            TO TL-MSG.
           MOVE PF-FORNECEDOR TO FR-CODIGO.
           READ ARQ-FOR
              INVALID KEY MOVE "*** FORNECEDOR NAO CADASTRADO ***"
                            TO TL-MSG.
           IF TL-MSG NOT = SPACES
              PERFORM VOLTA-ATUAL
              GO TO INC-FIM.
           PERFORM LIMPA-REGISTRO.
           MOVE NV-KSAVE TO PF-CHAVE.
           MOVE "I" TO WS-MODO.
           MOVE TL-TIT-INC TO TL-TITULO.
           PERFORM EDICAO THRU ED-FIM.
           MOVE TL-TIT-PADRAO TO TL-TITULO.
           IF WS-CANCELA = "S"
              MOVE "INCLUSAO CANCELADA" TO TL-MSG
              PERFORM VOLTA-ATUAL
              GO TO INC-FIM.
           MOVE "RELACAO INCLUIDA" TO TL-MSG.
           WRITE REG-PRODFORN
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
           MOVE REG-PRODFORN TO NV-COPIA.
           MOVE TL-TIT-ALT TO TL-TITULO.
           PERFORM EDICAO THRU ED-FIM.
           MOVE TL-TIT-PADRAO TO TL-TITULO.
           IF WS-CANCELA = "S"
              MOVE NV-COPIA TO REG-PRODFORN
              MOVE "ALTERACAO CANCELADA" TO TL-MSG
              IF WS-SEMALT = "S"
                 MOVE "NENHUM CAMPO ALTERADO" TO TL-MSG
                 GO TO ALT-FIM
              ELSE
                 GO TO ALT-FIM.
           MOVE "RELACAO ALTERADA" TO TL-MSG.
           REWRITE REG-PRODFORN
              INVALID KEY MOVE TL-M-ALT TO TL-MSG.
       ALT-FIM.
           EXIT.
       EXCLUIR.
           IF NV-POS = ZERO
              MOVE "*** NENHUM REGISTRO NA TELA ***" TO TL-MSG
              GO TO EXC-FIM.
           MOVE "CONFIRMA A EXCLUSAO (S/N)?" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           ACCEPT WS-OPCAO.
           IF WS-OPCAO = "s" MOVE "S" TO WS-OPCAO.
           IF WS-OPCAO NOT = "S"
              MOVE "OPERACAO CANCELADA" TO TL-MSG
              GO TO EXC-FIM.
           MOVE "RELACAO EXCLUIDA" TO TL-MSG.
           DELETE ARQ-PRODF RECORD
              INVALID KEY MOVE TL-M-EXC TO TL-MSG.
           IF TL-MSG NOT = TL-M-EXC PERFORM NV-REMOVE.
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
              PERFORM TL-TITULO-P.
           PERFORM MOSTRAR.
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
              PERFORM MOSTRAR
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
           GO TO EC-GET-FIM.
       EC-G01.
           MOVE PF-PART-FORNEC TO EC-X.
           GO TO EC-GET-FIM.
       EC-G02.
           MOVE PF-CUSTO TO EC-N.
           GO TO EC-GET-FIM.
       EC-G03.
           MOVE PF-QTD-MINIMA TO EC-N.
           GO TO EC-GET-FIM.
       EC-G04.
           MOVE PF-ULT-CUSTO TO EC-N.
           GO TO EC-GET-FIM.
       EC-G05.
           MOVE PF-PRAZO TO EC-N.
           GO TO EC-GET-FIM.
       EC-G06.
           MOVE PF-ULT-COMPRA TO EC-N.
           GO TO EC-GET-FIM.
       EC-G07.
           MOVE PF-PREFERENCIAL TO EC-X.
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
           GO TO EC-PUT-FIM.
       EC-P01.
           MOVE EC-X TO PF-PART-FORNEC.
           GO TO EC-PUT-FIM.
       EC-P02.
           MOVE EC-N TO PF-CUSTO.
           GO TO EC-PUT-FIM.
       EC-P03.
           MOVE EC-N TO PF-QTD-MINIMA.
           GO TO EC-PUT-FIM.
       EC-P04.
           MOVE EC-N TO PF-ULT-CUSTO.
           GO TO EC-PUT-FIM.
       EC-P05.
           MOVE EC-N TO PF-PRAZO.
           GO TO EC-PUT-FIM.
       EC-P06.
           MOVE EC-N TO PF-ULT-COMPRA.
           GO TO EC-PUT-FIM.
       EC-P07.
           MOVE EC-X TO PF-PREFERENCIAL.
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
           MOVE LOW-VALUES TO PF-CHAVE.
           START ARQ-PRODF KEY IS NOT < PF-CHAVE
              INVALID KEY GO TO NV-CC-FIM.
           MOVE "N" TO NV-FIM.
           READ ARQ-PRODF NEXT RECORD
              AT END MOVE "S" TO NV-FIM.
           IF NV-FIM = "N"
              MOVE 1 TO NV-N
              MOVE PF-CHAVE TO NV-KATU.
       NV-CC-FIM.
           EXIT.
       NV-LE-POS.
           MOVE NV-KATU TO PF-CHAVE.
           MOVE "S" TO NV-ACHOU.
           READ ARQ-PRODF
              INVALID KEY MOVE "N" TO NV-ACHOU.
           PERFORM NV-APOS-LER.
       NV-APOS-LER.
           IF NV-ACHOU = "S" MOVE PF-CHAVE TO NV-KATU.
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
              IF LC-ND < 8
                 MOVE LC-NUM TO PF-PRODUTO
                 MOVE ZERO TO PF-FORNECEDOR
                 MOVE "S" TO WS-SILENCIO
              ELSE
                 MOVE NV-ENT TO PF-CHAVE
           ELSE
              MOVE NV-ENT TO PF-CHAVE.
       NV-VAI-CHAVE.
           MOVE PF-CHAVE TO NV-KSAVE.
           MOVE "S" TO NV-ACHOU.
           READ ARQ-PRODF
              INVALID KEY MOVE "N" TO NV-ACHOU.
           IF NV-ACHOU = "S"
              PERFORM NV-APOS-LER
           ELSE
              PERFORM NV-SEGUINTE.
       NV-SEGUINTE.
           MOVE NV-KSAVE TO PF-CHAVE.
           MOVE "S" TO NV-ACHOU.
           START ARQ-PRODF KEY IS NOT < PF-CHAVE
              INVALID KEY MOVE "N" TO NV-ACHOU.
           IF NV-ACHOU = "S"
              READ ARQ-PRODF NEXT RECORD
                 AT END MOVE "N" TO NV-ACHOU.
           IF NV-ACHOU = "S"
              PERFORM NV-APOS-LER
           ELSE
              PERFORM NV-PRIMEIRO.
           IF WS-SILENCIO = "N"
              MOVE NV-KSAVE TO NV-MK
              MOVE NV-MSG-NC TO TL-MSG.
           MOVE "N" TO WS-SILENCIO.
       NV-PROXIMO.
           IF NV-N = ZERO
              MOVE "*** ARQUIVO VAZIO ***" TO TL-MSG
           ELSE
              MOVE NV-KATU TO PF-CHAVE
              MOVE "S" TO NV-ACHOU
              START ARQ-PRODF KEY IS GREATER THAN PF-CHAVE
                 INVALID KEY MOVE "N" TO NV-ACHOU.
           IF NV-N > ZERO
              IF NV-ACHOU = "S"
                 READ ARQ-PRODF NEXT RECORD
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
           MOVE PF-CHAVE TO NV-KATU.
           MOVE 1 TO NV-POS.
           MOVE "S" TO NV-TAB-OK.
       NV-INSERE.
           MOVE PF-CHAVE TO NV-KATU.
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
       MOSTRAR.
           MOVE PF-PRODUTO TO PR-CODIGO.
           READ ARQ-PROD
              INVALID KEY MOVE SPACES TO PR-DESCRICAO.
           MOVE PF-FORNECEDOR TO FR-CODIGO.
           READ ARQ-FOR
              INVALID KEY MOVE SPACES TO FR-RAZAO.
           MOVE PF-PRODUTO TO TF01-1.
           MOVE PR-DESCRICAO TO TF01-2.
           MOVE 3 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA
              TF-R01.
           MOVE PF-FORNECEDOR TO TF02-1.
           MOVE FR-RAZAO TO TF02-2.
           DISPLAY VT-APAGA
              TF-R02.
           DISPLAY VT-APAGA
              TL-TRACO.
           MOVE PF-PART-FORNEC TO TF04-1.
           MOVE "NAO" TO TF04-2.
           IF PF-PREFERENCIAL = "S" MOVE "SIM" TO TF04-2.
           DISPLAY VT-APAGA
              TF-R04.
           DISPLAY VT-APAGA
              " ".
           MOVE PF-CUSTO TO TF06-1.
           MOVE PF-QTD-MINIMA TO TF06-2.
           DISPLAY VT-APAGA
              TF-R06.
           MOVE PF-ULT-CUSTO TO TF07-1.
           MOVE PF-PRAZO TO TF07-2.
           DISPLAY VT-APAGA
              TF-R07.
           MOVE PF-ULT-COMPRA TO TL-DTW.
           PERFORM TL-FMT-DATA.
           MOVE TL-DT-ED TO TF08-1.
           IF PF-ULT-COMPRA = ZERO MOVE SPACES TO TF08-1.
           DISPLAY VT-APAGA
              TF-R08.
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