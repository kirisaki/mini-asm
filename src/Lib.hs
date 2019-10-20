{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Data.ByteString.FastBuilder

class Ident a where
  ident :: a -> Builder

data Class
  = ClassNone
  | Class32
  | Class64
  deriving(Show, Eq)

instance Ident Class where
  ident ClassNone = "\x00"
  ident Class32 = "\x01"
  ident Class64 = "\x02"

data Data
  = DataNone
  | Data2LSB
  | Data2MSB
  deriving(Show, Eq)

instance Ident Data where
  ident DataNone = "\x00"
  ident Data2LSB = "\x01"
  ident Data2MSB = "\x02"

data Version
  = VersionNone
  | VersionCurrent
  deriving(Show, Eq)

instance Ident Version where
  ident VersionCurrent = "\x01"

data OSABI
  = OSABINone
  | OSABIGNU
  deriving(Show, Eq)

instance Ident OSABI where
  ident OSABINone = "\x00"
  ident OSABIGNU = "\x03"

magicNumber :: Builder
magicNumber = "\x7fELF"

padding :: Builder
padding = mconcat $ replicate 9 "\x00"

elfIdent :: Class -> Data -> Version -> OSABI -> Builder
elfIdent c d v o = ident c <> ident d <> ident v <> ident o

class Elf a where
  elf :: a -> Builder

data Type
  = TypeNone
  | TypeRel
  | TypeExec
  | TypeDyn
  | TypeCore
  deriving(Show, Eq)

instance Elf Type where
  elf TypeNone = "\x00"
  elf TypeRel  = "\x01"
  elf TypeExec = "\x02"
  elf TypeDyn  = "\x03"
  elf TypeCore = "\x04"

data Machine
  = MachineNone
  | MachineX86_64
  deriving(Show, Eq)

instance Elf Machine where
  elf MachineNone = "\x00"
  elf MachineX86_64 = "\x62"

instance Elf Version where
  elf VersionNone = "\x00"
  elf VersionCurrent = "\x01"

  

someFunc :: IO ()
someFunc = putStrLn "someFunc"

