# milp

## LP monad

`general` yields a free integer variable. Variables are instance of `Num` and can be combined with standard numerical operators to form new expressions. Expressions may be objectives and can be subject to several constraints.

```haskell

program :: LP (Var, Var)
program = do
  x <- general
  y <- general
  objective $ x + y
  x <=^ 2 <|> x + y >=^ 4
  pure (x, y)

```

Constraints are declared by using one of `=^`, `<=^`, `>=^` operators.

Disjunctive inequalities can be expressed by using the `Alternative` instance of the LP monad.


## Optimization

Integer programs can be `minimize`d or `maximize`d in the IO monad by using some MILP solver in background.
The result function returned can be used to retrieve feasible solutions.

```haskell

main :: IO ()
main = do
  ((x1, x2), result) <- minimize program
  putStrLn $ show $ (,) <$> result x1 <*> result x2

```
