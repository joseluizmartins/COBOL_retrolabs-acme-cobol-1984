# Os programas

Todos os fontes estão em [`fontes/`](../fontes). Cada um compila em um `.INT`
com o mesmo nome e é chamado pelo menu.

## Sistema (fontes/)

| Programa | Menu | Função | Arquivos |
|---|---|---|---|
| **RACMENU** | — | Tela de abertura e menu principal. Chama os módulos com `CALL "A:RACxxx.INT"` e os libera com `CANCEL`. | nenhum |
| **RACCLI** | 1 | Cadastro de clientes: ficha completa (dados, crédito e movimento de 1984), navegação, inclusão, alteração campo a campo e inativação. | CLI |
| **RACFOR** | 2 | Cadastro de fornecedores, no mesmo padrão do de clientes. | FORN |
| **RACPRO** | 3 | Produtos e estoque: estoque atual, reservado, disponível, mínimo, máximo, ponto de pedido, custos, margem e aviso de reposição. | PROD |
| **RACPF** | 4 | Produto × fornecedor: part number no fornecedor, custo, quantidade mínima, prazo e fornecedor preferencial. | PRODF, PROD, FORN |
| **RACVEN** | 5 | Faturamento: pedido com até 20 itens, lista de clientes e produtos (`?`), valores sugeridos, conferência de estoque e de limite de crédito. Grava o pedido, os itens, a saída de estoque, a duplicata e atualiza o cliente. | VEN, ITVEN, CLI, PROD, MOVEST, REC, PARAM |
| **RACCOM** | 6 | Compras: entrada de mercadoria com quantidade e custo sugeridos. Grava a compra, os itens, a entrada de estoque com custo médio, o título a pagar e atualiza o fornecedor. | COMP, ITCOM, FORN, PROD, MOVEST, PAG, PARAM |
| **RACREC** | 7 | Contas a receber: consulta e baixa do título, atualizando o saldo do cliente. | REC, CLI |
| **RACPAG** | 8 | Contas a pagar: consulta e baixa, atualizando o saldo do fornecedor. | PAG, FORN |
| **RACMOV** | 9 | Consulta do movimento de estoque (entradas, saídas e inventário). | MOVEST |
| **RACIV** | A | Consulta dos itens de venda. | ITVEN |
| **RACIC** | B | Consulta dos itens de compra. | ITCOM |
| **RACREL** | R | 13 relatórios no vídeo (com pausa por tela e X para encerrar) ou na impressora: clientes, posição financeira, vendas, itens vendidos, fornecedores, produtos, posição de estoque, estoque abaixo do mínimo, contas a receber, contas a pagar, compras, itens comprados e **resumo de vendas por mês com gráfico**. | todos, um de cada vez |
| **RACCHK** | C | Auditoria: conta os registros de todos os arquivos e soma vendas, compras e títulos em aberto, para conferência com a folha da demonstração. | todos, um de cada vez |

Arquivos de apoio em `fontes/`:
- `COMPILA.SUB` compila os 14 programas; `COMP1.SUB` e `COMP2.SUB` dividem a
  compilação em dois disquetes.
- `CISCOBOL.TXT`: as regras do dialeto CIS COBOL levantadas no projeto.

## Estrutura interna comum

- **Telas VT100**: as sequências de controle ficam em itens `VT-LIMPA`, `VT-INV`,
  `VT-FORTE`, `VT-PISCA`, `VT-POS` (linha e coluna) e `VT-APAGA`.
- **Rotinas `TL-`**: título, mensagem (linha 20), pergunta (linhas 22 e 23) e
  rodapé em dois grupos (linha 24).
- **Rotinas `NV-`**: navegação entre registros. Os arquivos pequenos guardam as
  chaves numa tabela em memória (permite voltar); os grandes leem em sequência.
- **Rotinas `LC-`**: entrada amigável — código sem zeros à esquerda, valores com
  vírgula ou ponto, datas em `DD/MM/AA` ou `AAMMDD`, soma de dias ao vencimento.
- **Rotinas `EC-`**: edição campo a campo dirigida por uma tabela de campos
  (posição, tipo e texto da pergunta).
- **Rotinas `PQ`**: listas paginadas de clientes, fornecedores e produtos (`?`)
  na venda e na compra.

## Utilitários (utilitarios/)

| Programa | Função |
|---|---|
| **RACLOAD** | Carga automática da base: lê os 12 `.TXT` do A: e cria os arquivos indexados no B:, mostrando o andamento (1/12 … 12/12). |
| **RACINIT** | Cria os 12 arquivos indexados vazios. |
| **RACPAR** | Manutenção do arquivo de parâmetros (próximo pedido, próxima compra). |

## Testes (testes/)

| Programa | O que testa |
|---|---|
| **TSTVT** | se o terminal aceita as sequências VT100 enviadas pelo programa |
| **TSTAT** | o `DISPLAY/ACCEPT AT` do próprio CIS (posicionamento pelo RUNA) |
| **TSTSHR** | abertura de arquivo em uso por outro terminal (MP/M) |
| **TSTCALL** + **TSTSUB** | as formas do `CALL` (drive atual, drive fixo) |
