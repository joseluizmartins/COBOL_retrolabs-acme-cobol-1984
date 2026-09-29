# Compilação, carga e operação

## Compilação

Com os fontes e o compilador no mesmo disquete:

```
A>SUBMIT COMPILA
```

Ou em dois disquetes: `SUBMIT COMP1` e `SUBMIT COMP2`. Confira no fim de cada
compilação a linha `ERRORS=00000`. Ligue o log do TeraTerm (File › Log) para
guardar a saída. O compilador grava o `.INT` no mesmo disco do fonte: deixe
espaço livre e copie os `.INT` para o disco de execução.

## Disposição dos discos para executar

| Drive | Deve conter |
|---|---|
| A: | `RUNA.COM` e os 14 `.INT` (o menu chama `A:RACxxx.INT`) |
| B: | a base de dados carregada |

## Carga da base

1. Coloque os 12 `.TXT` de [`dados/`](../dados) no A:.
2. `B:RUNA A:RACLOAD.INT` e responda `S`.
3. Conferência no menu, opção **C**: 100 clientes, 40 fornecedores, 500 produtos,
   93 vendas (13.554,99), 18 compras (14.811,51), receber em aberto 7.282,31,
   pagar em aberto 8.277,79.
4. Faça uma cópia do disco B: carregado (**BASE MESTRE**).

O RUNA passa ao programa os parâmetros da linha de comando, e a primeira leitura
do console pode vir vazia. Por isso a confirmação do RACLOAD se repete até
receber S ou N, e a tela de abertura do menu absorve essa primeira leitura.

## Roteiro de demonstração e valores esperados

| Passo | Ação | Resultado |
|---|---|---|
| 1 | Venda: cliente `1472`, produto 1001327 × 10 e 1001194 × 8, `S` | total 153,96; estoque do 1001327 de 41 para 31; saldo do cliente de 378,30 para 532,26 |
| 2 | Compra: fornecedor `290`, 1001327 × 50 a 6,40, `S` | estoque do 1001327 volta a 81 |
| 3 | Contas a receber: `92`, `2`, RETURN | título 00000092/01 baixado |
| 4 | Contas a pagar: `5`, `2`, RETURN | título 00000005/01 pago |
| 5 | Auditoria (C) | 94 vendas (13.708,95), 19 compras (15.131,51), receber 7.325,27, pagar 7.544,28 |

## TeraTerm

- Setup › Terminal: **Terminal ID VT100**, New-line Transmit **CR**.
- CAPS LOCK ligado.

### Cores do TeraTerm

Para ficar monocromático, em Setup › Window (ou no `TERATERM.INI`):

| | Verde | Âmbar |
|---|---|---|
| `VTColor` | `51,255,51,0,0,0` | `255,176,0,0,0,0` |
| `VTBoldColor` | `190,255,190,0,0,0` | `255,220,130,0,0,0` |
| `VTBlinkColor` | `51,255,51,0,90,0` | `255,176,0,90,55,0` |
| `VTReverseColor` | `0,0,0,51,255,51` | `0,0,0,255,176,0` |

com `EnableBoldAttrColor=on`, `EnableBlinkAttrColor=on` e
`EnableReverseAttrColor=on`.

## Contingência

- Se algo travar: reinicie o Altos e restaure a BASE MESTRE.
- Erro 164 ao escolher uma opção: o `.INT` daquele módulo não está no A:.
- Erro 157: falta memória para o módulo.
- Os departamentos trabalham em rodízio (veja [03-departamentos.md](03-departamentos.md#uso-simultâneo-dos-terminais)).
