# lambdem-poker

[![Build Status](https://travis-ci.org/cmc-haskell-2018/lambdem-poker.svg?branch=master)](https://travis-ci.org/cmc-haskell-2018/lambdem-poker)

Клиент для игры в покер.

## Сборка и запуск

Соберите проект при помощи [утилиты Stack](https://www.haskellstack.org):

```
stack setup
stack build
```

Собрать и запустить проект можно при помощи команды

```
stack build && stack exec lambdem-poker
```

Запустить тесты можно при помощи команды

```
stack test
```

Чтобы запустить интепретатор GHCi и автоматически подгрузить все модули проекта, используйте команду

```
stack ghci
```

