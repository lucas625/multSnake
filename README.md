# multSnake

Esse projeto foi desenvolvido para a cadeira de PLC em 2019.1.
Contém Um jogo de cobrinha multiplayer.

## Jogar

- Como player 1: 
    - **seta pra cima** = cima
    - **seta pra baixo** = baixo
    - **seta esquerda** = esquerda
    - **seta direita** = direita
- Como player 2: 
    - **w** = cima
    - **s** = baixo
    - **a** = esquerda
    - **d** = direita

## Instalação

- Instale o cabal:
    - Veja este [link](https://www.haskell.org/cabal/).
    - Pode ser necessário usar o comando:
        - `$ cabal update`
    - Instale o gloss:
        - `$ cabal install gloss`
        - A instalação ou o comando de rodar pode dar erro, caso isso aconteça, repita o passo a passo da instalação.
    - Instale o resto:
        - `$ cabal install`
    - Este comando da build. 
        - `$ cabal init -n --is-executable`
    - Este comando roda.
        - `$ cabal run`
        - Às vezes pode ser necessário usar este comando ao invés do `cabal run`:
            - `$ cabal v2-run`


