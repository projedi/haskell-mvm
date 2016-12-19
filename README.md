MVM is pretty much the mathvm language https://code.google.com/archive/p/mathvm/.

Types: `int`, `double`, `string`
Conditional statements: `if`, `for`, `while`
Operators: `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `<=`, `>`, `>=`
Builtins: `print` (variadic func)
Expressions: `var`, `literal`, `expr op expr`, `func(expr, …)`
Declarations: `type var;`, `type|void func(type var, …) stmt`
Statements: `func(expr, expr, …);`, `var = expr;`, `if (expr) stmt;`, `if (expr) stmt else stmt`, `for (var in expr..expr) stmt`, `while (expr) stmt`, `{ stmt; stmt; … }`, `return expr;`
Literals: `0`, `0.0`, `"abc"`

Declaration must precede the usage.

Scoping:
`{}` defines the scope. Normal shadowing rules apply. Function definitions inside a scope form a closure:

```
{
  int i;
  int f() {
    // i is visible here.
    // j is not.
    return i + 1;
  }
  int j;

  i = 0;
  print(f()); // prints 1
  i = 1;
  print(f()); // prints 2
}
```

Differences from mathvm:
* No `function` keyword
* Use `""` instead of `''` for strings
* Foreign functions are defined as `foreign <fundecl>` instead of `<fundecl> native 'name'`
* Has `dlopen()` call to link to shared libraries. Used to bind foreign function.
