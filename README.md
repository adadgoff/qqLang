# QQ Language

> The title is taken from the song [Coco Jambo](https://www.youtube.com/watch?v=EScLmWJs82I).
>
> `Coco` is simplified to `QQ`.

> by Dadykov Artemy.

## Features

- `float` datatype;
- conditions `if/then/else`;
- functions `def`;
- functions recursive `def rec`;
- print function `print`;
- variables.

## Examples

### Example with function

```python
def double(x) {
    x * 2
}
print(double(4))
```

### Example with variables

```python
var PI = 3.14
var radius = 10
var radiusSquare = radius * radius
var square = PI * radiusSquare
print(square)
```

### Example with recursive function

```python
def rec fact(n) {
    if n == 1 then {
        1
    }
    else {
        n * fact(n - 1)
    }
}
print(fact(5))
```

## Structure
```bash
src
├── ast.fs
├── main.fs
└── parser.fs
```

## How to run

> change `codeSample` in `main.fs`.