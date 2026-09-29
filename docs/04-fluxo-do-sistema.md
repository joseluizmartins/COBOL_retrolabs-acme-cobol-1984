# Fluxo do sistema

## Visão geral

```mermaid
flowchart TD
    A[Tela de abertura] --> M[RACMENU - menu principal]
    M -->|1| CLI[RACCLI clientes]
    M -->|2| FOR[RACFOR fornecedores]
    M -->|3| PRO[RACPRO produtos]
    M -->|4| PF[RACPF produto x fornecedor]
    M -->|5| VEN[RACVEN vendas]
    M -->|6| COM[RACCOM compras]
    M -->|7| REC[RACREC contas a receber]
    M -->|8| PAG[RACPAG contas a pagar]
    M -->|9| MOV[RACMOV movimento de estoque]
    M -->|A| IV[RACIV itens de venda]
    M -->|B| IC[RACIC itens de compra]
    M -->|R| REL[RACREL relatórios]
    M -->|C| CHK[RACCHK auditoria]
```

Cada opção faz `CALL "A:RACxxx.INT"`; ao sair do módulo com X, o menu faz
`CANCEL` e libera a memória.

## Venda (Terminal 1)

```mermaid
flowchart TD
    V1[Data do pedido - sugere 15/12/84] --> V2[Cliente - código ou ? para lista]
    V2 -->|não existe| V2
    V2 --> V3[Operador, terminal, vencimento sugerido]
    V3 --> V4[Produto e quantidade]
    V4 -->|estoque insuficiente| V4
    V4 -->|mais itens| V4
    V4 -->|RETURN sem código| V6[Confirma S/N]
    V6 -->|S| V5{Limite de crédito e estoque}
    V5 -->|excedido| V9[Pedido não gravado]
    V5 -->|ok| G[Gravação]
    G --> G1[VEN + ITVEN]
    G --> G2[PROD: estoque, vendido no ano]
    G --> G3[MOVEST: saída]
    G --> G4[REC: duplicata]
    G --> G5[CLI: saldo, total do ano, títulos]
    G --> G6[PARAM: próximo pedido]
```

## Compra (Terminal 2)

```mermaid
flowchart TD
    C1[Data da compra] --> C2[Fornecedor - código ou ?]
    C2 --> C3[Produto]
    C3 --> C4[Quantidade sugerida = completa o estoque máximo]
    C4 --> C5[Custo sugerido = último custo]
    C5 -->|mais itens| C3
    C5 -->|RETURN sem código| C6[Confirma S/N]
    C6 -->|S| H[Gravação]
    H --> H1[COMP + ITCOM]
    H --> H2[PROD: estoque, custo médio, último custo]
    H --> H3[MOVEST: entrada]
    H --> H4[PAG: título a pagar]
    H --> H5[FORN: saldo e total do ano]
```

## Baixa de títulos (Terminal 3)

O título mostrado na tela é baixado com a data do pagamento (sugerida 15/12/84).
Na baixa a receber, o saldo e a quantidade de títulos em aberto do cliente são
atualizados; na baixa a pagar, os do fornecedor.

## Arquivos

Todos são indexados (`.DAT` + `.IDX`) e ficam no drive B:.

| Arquivo | Conteúdo | Chave | Registro |
|---|---|---|---|
| CLI | clientes | código (6) | 341 bytes |
| FORN | fornecedores | código (5) | 330 bytes |
| PROD | produtos e estoque | código (7) | 188 bytes |
| PRODF | produto × fornecedor | produto (7) + fornecedor (5) | 60 bytes |
| VEN | pedidos de venda | número (8) | 77 bytes |
| ITVEN | itens de venda | pedido (8) + item (3) | 45 bytes |
| COMP | compras | número (8) | 43 bytes |
| ITCOM | itens de compra | compra (8) + item (3) | 43 bytes |
| REC | contas a receber | título (8) + parcela (2) | 102 bytes |
| PAG | contas a pagar | título (8) + parcela (2) | 65 bytes |
| MOVEST | movimento de estoque | tipo (E/S/I) + documento (8) + item (3) | 50 bytes |
| PARAM | parâmetros (próximo pedido, próxima compra) | chave (2) | 64 bytes |

## Navegação nas telas de consulta

| Tecla | Ação |
|---|---|
| `=` (ou RETURN) | próximo registro |
| `-` | registro anterior (clientes, fornecedores, títulos) |
| `I` / `F` | primeiro / último |
| código | vai direto ao registro (aceita sem zeros: `1472`) |
| `1` `3` `4` | inclui, altera (campo a campo), inativa |
| `X` | volta ao menu |

Na edição, cada campo mostra o valor atual; RETURN mantém, digitar substitui,
`*` cancela tudo, e uma confirmação S/N antecede a gravação.

![Edição campo a campo](telas/04-clientes-edicao.png)
