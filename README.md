# Shiatsu

Gentle massaging of documents by way of internally-defined macro expansion.

Shiatsu implements and expands the `newcommand` command from TeX.


## Example

```latex
\newcommand{\hello}[1]{Hello #1}
\hello{World!}
```

results in

```
Hello World!
```
