module SpreadsheetTypes where

import Array exposing (Array)

type alias GridId = Int -- because using Array.set and Array.get

type alias Cell =
            { dependentCells: List GridId
            , underlying: CellContent
            , currentValue : Atom
            }
type alias Grid =
            { width : Int
            , height : Int
            , data  : Array Cell
            }

type Atom = Number Float | Text String | Error
type AST a = Node Op (List (AST a)) | Leaf a
type CellContent = Value Atom | Reference GridId | Formula (AST CellContent)

type Op = Niladic NiladicOp | Monadic MonadicOp | Dyadic DyadicOp | Variadic VariadicOp
type NiladicOp = ErrorOp
type MonadicOp = Minus
type DyadicOp = Sub | Pow
type VariadicOp = Add | Mult
