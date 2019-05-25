# Format
Typed Formating System



# Grammar for Usage

```
[environment_name]
Content
[/environment_name]
```

For example:

Inline equation:

```
[math]E=mc^2[/math]
```

Enumerate:

```
[enumerate]
[item] Hi [/item]
[/enumerate]
```



Tabucular

```
[table row-lines=((Table.line Table.line) (Table.line) () () () () (Table.line)) column-lines=(() () () () ()))]
$Table(content=(
(1 50 837 930)
(2 47 877 230)
(3 23 342 231)), align=center)
[/table]
```



Matrix

```
[matrix]
[row] x_{11} | x_{12} | x_{13} | \dots | x_{1n} [/row]
[row] x_{21} | x_{22} | x_{23} | \dots | x_{2n} [/row]
[row] Matrix.dots(5) [/row]
[row] x_{d1} | x_{d2} | x_{d3} | \dots | x_{dn} [/row]
[/matrix]
```

