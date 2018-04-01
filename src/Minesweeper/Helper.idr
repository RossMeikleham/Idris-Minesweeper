-- Contains Helper functions
module Minesweeper.Helper

export
readNat : String -> Maybe Nat
readNat s = go 0 (unpack s) where

  readDigit : Char -> Maybe Nat
  readDigit c = case c of
    '0' => Just 0
    '1' => Just 1
    '2' => Just 2
    '3' => Just 3
    '4' => Just 4
    '5' => Just 5
    '6' => Just 6
    '7' => Just 7
    '8' => Just 8
    '9' => Just 9
    _   => Nothing

  go : Nat -> List Char -> Maybe Nat
  go acc l = case l of
    [] => Just acc
    (c::cs) => do
                  d <- readDigit c
                  go ((acc * 10) + d) cs
