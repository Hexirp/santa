{-# LANGUAGE GADTs #-}

module Santa.Expression where
  import Prelude

  data Expression v where
    Application :: Expression v -> Expression v -> Expression v
    Lambda :: (v -> Expression v) -> Expression v
    Variable :: v -> Expression v
    Array :: [Expression v] -> Expression v
    Number :: Integer -> Expression v
    Hole :: Expression v -> Expression v

  newtype Expr = Expr (Expression Expr)

  eval :: Expr -> Expr
  eval (Expr e) = Expr (eval_ e)
   where
    eval_ :: Expression Expr -> Expression Expr
    eval_ x = case x of
      Application xL xR -> case eval_ xL of
        Lambda xLF -> case eval_ xR of
          Lambda xRF -> eval_ (xLF (Expr (Lambda xRF)))
          Array xRA -> eval_ (xLF (Expr (Array xRA)))
          Number xRN -> eval_ (xLF (Expr (Number xRN)))
          Hole xRH -> Hole (Application (Lambda xLF) (Hole xRH))
        Array xLA -> case eval_ xR of
          Lambda xRF -> Hole (Application (Array xLA) (Lambda xRF))
          Array xRA -> Hole (Application (Array xLA) (Array xRA))
          Number xRN -> case index xLA xRN of
            Nothing -> Hole (Application (Array xLA) (Number xRN))
            Just x' -> x'
          Hole xRH -> Hole (Application (Array xLA) (Hole xRH))
        Number xLN -> Hole (Application (Number xLN) xR)
        Hole xLH -> Hole (Application (Hole xLH) xR)
      Lambda xLF -> Lambda xLF
      Variable xV -> case xV of
        Expr xVE -> xVE
      Array xLA -> Array xLA
      Number xLN -> Number xLN
      Hole xLH -> Hole xLH

  index :: [a] -> Integer -> Maybe a
  index x n = if n < 0 then Nothing else index_ x n
   where
    index_ x n = case x of
      [] -> Nothing
      x : xs -> if n == 0 then Just x else index_ xs (n - 1)
