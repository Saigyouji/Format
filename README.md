# Format
Typed Formating System

Temporary version for displaying, not ready for daily use!

# Usage

python3 fmt.py fmt-file lisp-file

Typing: racket typing.rkt < lisp-file

Interpret: racket interpreter.rkt < lisp-file > tex-file


# Grammar for Usage

```
[environment_name]
Content
[/environment_name]
```

For example:

Header:

```
[header class = "article" title = "Test Document" author = "Falsyta" include = (list (string) "amsmath")]
[/header]
```


Itemize:

```
[itemize]
[item] Hi [/item]
[/itemize]
```



Test document:

```
[header class = "article" title = "Test Document" author = "Falsyta" include = (list (string) "amsmath")]
[/header]

[document]
	Hi

	$equation$
[/document]
```

