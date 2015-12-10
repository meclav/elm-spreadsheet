module Spreadsheet where
import String
import Result exposing (Result)
import Array exposing (Array, get, set)



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

cellsReferenced: CellContent -> List GridId
cellsReferenced cell =
  let refs x c = case c of
    Value atom
      -> x
    Reference ref
      -> ref::x
    Formula (Leaf c')
      -> refs x c'
    Formula (Node op cs)
      -> x ++ List.concat (List.map (\c-> refs [] (Formula c)) cs )
  in refs [] cell

parse: String -> Result String CellContent
parse s = Ok (Value (Number 42))
--TODO

updateData : Array Cell -> GridId -> CellContent -> Result String (Array Cell)
updateData data ix newContent =
  case get ix data of
    Nothing
      -> Err "Failed lookup"
    Just cell
      -> let
          data1 = set ix {cell | underlying = newContent, currentValue = Error } data
          data2 = addDependenciesFromNewCell data1 newContent
          indicesToRecompute = case getRecomputeOrder data2 ix of
            Err (Message str)
              -> Err str
            Err (CircularDependencyDetected)
              -> Err "Circular dependency TODO handle"
            Ok indices -> Ok indices
        in
          Result.map (recompute data2) indicesToRecompute

{-
Recomputes current values based on supplied safe order.
-}
recompute: Array Cell -> List GridId -> Array Cell
recompute data changed =
  List.foldr (recomputeOne) data changed


recomputeOne: GridId -> Array Cell -> Array Cell
recomputeOne changedCellId data =
    case get changedCellId data of
        Nothing
          -> data
        Just cell
          ->  let
                getRef ref = Maybe.map .currentValue (get ref data)
                newCell = {cell | currentValue = eval getRef cell.underlying }
              in  set changedCellId newCell data

type Atom = Number Float | Text String | Error
type AST a = Node Op (List (AST a)) | Leaf a

type CellContent = Value Atom | Reference GridId | Formula (AST CellContent)

eval : (GridId -> Maybe Atom) -> CellContent -> Atom
eval getRef underlying = case underlying of
  Value atom
    -> atom
  Reference gridId
    -> getRef gridId |> Maybe.withDefault Error
  Formula (Leaf cellContent)
    -> eval getRef cellContent
  Formula (Node op asts)
    -> doOp op (List.map (\ast -> eval getRef (Formula ast)) asts)

type Op = Niladic NiladicOp | Monadic MonadicOp | Dyadic DyadicOp | Variadic VariadicOp
type NiladicOp = ErrorOp
doNiladic opName = case opName of
    ErrorOp -> Error

type MonadicOp = Minus
doMonadic opName x = case (opName,x) of
  (_, Error) -> Error
  (Minus, Number v) -> Number (-v)
  (Minus, _) -> Error

type DyadicOp = Sub | Pow
doDyadic opName x1 x2 = case (opName, x1, x2) of
  (_, _, Error) -> Error
  (_,Error,_) -> Error
  (Sub, Number v1, Number v2) -> Number (v1-v2)
  (Sub, _, _) -> Error
  (Pow, Number v1, Number v2) -> Number (v1^v2)
  (Pow, _,_) -> Error

type VariadicOp = Add | Mult
doVariadic opName xs = case (opName, xs) of
  (Add, xs)
    -> List.foldr (valueMap2 (\x y -> x+y)) (Number 0) xs
  (Mult, xs)
    -> List.foldr (valueMap2 (\x y -> x*y)) (Number 1) xs

valueMap2: (Float-> Float -> Float) -> Atom -> Atom -> Atom
valueMap2 f v1 v2 = case (v1,v2) of
                  (Number x1, Number x2) -> (Number (f x1 x2) )
                  _-> Error

doOp: Op -> List Atom -> Atom
doOp op args = case (op,args) of
    (Niladic opName, [])
      -> doNiladic opName
    (Monadic opName, x::[])
      -> doMonadic opName x
    (Dyadic opName, x1::x2::[])
      -> doDyadic opName x1 x2
    (Variadic opName, xs)
      -> doVariadic opName xs
    (_,_)-> Error



addDependenciesFromNewCell: Array Cell -> CellContent -> Array Cell
addDependenciesFromNewCell data cellContent =
    let
      updateCell data1 ref = case get ref data of
          Nothing
            -> data1
          Just cell
            -> set ref {cell | dependentCells = ref:: cell.dependentCells } data1
      updateCells data1 refs = case refs of
        []
          -> data1
        r::rs
          ->
        updateCells (updateCell data1 r) rs
    in
      updateCells data (cellsReferenced cellContent)

type MyFailure = Message String | CircularDependencyDetected

--toposort could be better
getRecomputeOrder: Array Cell -> GridId -> Result MyFailure (List GridId)
getRecomputeOrder data start =
  case visit data start (Just ([],[])) of
    Nothing -> Err CircularDependencyDetected
    Just res-> Ok (fst res)

visit: Array Cell -> GridId -> Maybe (List GridId,List GridId)  -> Maybe (List GridId,List GridId)
visit data node resultSoFar =
  case resultSoFar of
    Nothing --failure
      -> Nothing
    Just (result, temporarilyMarked)
      ->
        if List.member node temporarilyMarked
        then Nothing
        else if not (List.member node result)
          then
            let
              resultFurther = getDependentCells data node
                            |> List.foldr (visit data) (Just (result, node::temporarilyMarked))
            in
            case resultFurther of
              Nothing
                -> Nothing
              Just (resultLater, temporarilyMarkedLater)
                -> Just (node::resultLater, List.filter (\n -> n == node) temporarilyMarkedLater)
        else resultSoFar

{--
Pseudocode:
function visit(node n)
    if n has a temporary mark then stop (not a DAG)
    if n is not marked (i.e. has not been visited yet) then
        mark n temporarily
        for each node m with an edge from n to m do
            visit(m)
        mark n permanently
        unmark n temporarily
        add n to head of L
-}


getDependentCells data x = case get x data of
    Nothing-> []
    Just cell -> cell.dependentCells


{-

update : Grid -> Int -> Int ->String-> Result String Grid
update grid x y newContent =
  let
  parseResult = parse newContent
  cellResult  = if (x<1 or x>grid.width or y<1 or y>grid.height)
                  Err "Wrong dimensions"
                else
                  Ok (y-1)*grid.width + x
                  `andThen`
                  \ix -> get ix grid.data
                  `andThen`
                  \ar -> case ar of
                    Nothing
                      -> Err "Failed array lookup, should not happen"
                    Just cell
                      -> Ok cell
  in
    map2 (updateCellIn grid) parseResult cellResult

getCell: Grid -> GridId -> Maybe Cell
getCell grid ix

updateCellIn: Grid -> CellContent -> Cell -> Grid
--}
