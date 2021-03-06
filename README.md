# milp

## Dependencies

 - Install coin-or-cbc: https://projects.coin-or.org/Cbc
 - Check that `cbc` is available in `$PATH`.


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

For disjunctions to work properly we need to find some sufficiently large `M`, which is inferred from the bounds of the program. In essence, you *should* use explicit bounds when integer programming.

```haskell
0 <=. x .<= 8 :: LP ()
```

Explicitly setting this bound implies `M > 8`.


## Optimization

Integer programs may be minimized or maximize by the coin-or software.
The result function operates in the `Maybe` monad for your convenience.

```haskell

main :: IO ()
main = do
  ((x, y), result) <- minimize program
  putStrLn $ show $ (,) <$> result x <*> result y

```

` >>> Just (1,0) `  
