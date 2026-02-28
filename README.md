# Gitty

Uma cópia do git usando Haskell.

Comandos já suportados (parcialmente):

```txt
gitty init

gitty cat-file (-k | -s | -p) OBJECT

gitty rev-parse (--gitty-dir | --show-toplevel | --is-inside-work-tree | --show-prefix | [--quiet] --verify REF | REF)

gitty hash-object [-w|--write] [-k|--kind KIND] FILE

gitty update-index [--add] ([--cacheinfo MODE,OBJECT,PATH] ... | FILES...)

gitty write-tree

gitty commit-tree TREE [-p|--parent PARENT] [-m|--message MESSAGE]

gitty symbolic-ref NAME [REF]

gitty update-ref [-d] REF [NEWVALUE]

gitty add FILES/DIRS

gitty commit [-m|--message MESSAGE]
```