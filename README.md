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

### Notes on Big M

For disjunctions to work properly we need to find some sufficiently large `M`, which is infered from the bounds of the program. In essence, you *should* use explicit bounds when integer programming.

```haskell
0 <=. x .<= 8 :: LP ()
```

Setting this bound explictly implies `M > 8`.


## Optimization

Integer programs can be `minimize`d or `maximize`d in the IO monad by using some MILP solver in background.
The result function returned can be used to retrieve feasible solutions.

```haskell

main :: IO ()
main = do
  ((x1, x2), result) <- minimize program
  putStrLn $ show $ (,) <$> result x1 <*> result x2

```

` >>> Just (1,1) `  
