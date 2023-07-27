# Solve

Solve is an expression parser for Delphi. The ```Solve()``` function converts a string expression (e.g. ```(1+2)*3```) to an integer. Additionally, ```SolveStr()``` outputs the value as a string and ```SolveHex()``` outputs a hex string (e.g. ```$1234```).

Solve can also read data from a file if it's loaded with the ```LoadFile()``` procedure.

## Supported operations

Symbol | Description
--- | ---
```+``` | Addition
```-``` | Subtraction (also used as a prefix for negative integers)
```*``` | Multiplication
```/``` | Division (integer, rounded down)
```\``` | Division (integer, rounded up)
```R``` | Round up to nearest multiple (e.g. ```7R5 = 10```)
```r``` | Round down to nearest multiple
```%``` | Modulo (also used as a prefix for binary integers)
<tt>&vert;</tt> | OR
```&``` | AND
```^``` | XOR
```>>``` | Bit shift right
```<<``` | Bit shift left
```**``` | Exponentiation
```=``` | Equals
```>``` | Greater than
```<``` | Less than
```>=``` | Greater than or equal to
```<=``` | Less than or equal to
```<>``` | Not equal
```()``` | Parentheses
```$``` | Prefix for hexadecimal integers (```0x``` also works)
```"str"``` | String (actually processed as CRC32 of string)
```{b:x}``` | Read byte from address ```x```
```{w:x}``` | Read word from address ```x```
```{t:x}``` | Read 3 bytes from address ```x```
```{d:x}``` | Read longword from address ```x```
```{_w:x}``` | Read reversed (little-endian) word from address ```x```
```{_t:x}``` | Read reversed (little-endian) 3 bytes from address ```x```
```{_d:x}``` | Read reversed (little-endian) longword from address ```x```
```{s:x,y}``` | Read string from address ```x``` with maximum length ```y``` (returns CRC32 of string)
```{i:x,y}``` | Read string integer from address ```x``` with maximum length ```y``` (returns integer itself, or 0 if not found)
```{find:x,y,"str"}``` | Find location of string between the addresses ```x``` and ```y``` (returns -1 if not found)
```{filesize}``` | Size of loaded file in bytes (0 if no file is loaded)
```{val}``` | Value stored in the variable ```val```
