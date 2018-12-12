module Language.OCaml.Parser.Int.Test
  ( testStrings
  ) where

testStrings :: [String]
testStrings =
  -- decimal literals
  [ "0"
  , "1234"
  , "01_23_45_67_89"
  -- hex literals
  , "0x01_23_45_67_89_AB_CD_EF"
  , "0X01_23_45_67_89_ab_cd_ef"
  -- oct literals
  , "0o01_23_45_67"
  , "0O01_23_45_67"
  -- bin literals
  , "0b01"
  , "0B01"
  ]
