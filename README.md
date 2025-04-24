# Taskell - Lista de Tarefas em Haskell

Um gerenciador de tarefas simples com persistência de dados via linha de comando desenvolvido totalmente em Haskell.

## Pré-requisitos

- Haskell GHC (>=8.10)
- Cabal e/ou Stack
- Pacotes necessários: `time`, `directory`, `filepath`, `QuickCheck` 

## Compilar e Executar

### Usando Cabal
```bash
# Instalar dependências e construir
cabal update
cabal build

# Executar aplicação
cabal run 
```

### Usando GHC
```bash
ghc --make app/Main.hs -isrc -o taskell \
  -package time \
  -package directory \
  -package filepath 
./taskell
```
