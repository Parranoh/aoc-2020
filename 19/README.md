Compiling with `-O3` is recommended.

`b.sed` assumes that 132 is the maximal number of any rule. If that is not the
case, it has to be changed accordingly.

The maximal number of any rule can be found out by piping the problem input into
```sh
sed '/^$/q;s/^([0-9]+).*$/\1/' | sort -h | tail -n1
```
