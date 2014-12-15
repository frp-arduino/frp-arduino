module Stages.CodeGen where

import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified Data.Map as M

import qualified Types.AST as AST
import Stages.Stream
import Types.DAG

programToC :: AST.Program -> String
programToC = streamTreeToC . programToStreamTree

streamTreeToC :: Streams -> String
streamTreeToC tree = unlines $
    [ "#include <avr/io.h>"
    , "#include <util/delay.h>"
    , "#include <stdbool.h>"
    ]
    ++
    concatMap (streamToToCHeader tree) (streamsInTree tree)
    ++
    concatMap (streamToCBody tree) (streamsInTree tree)
    ++
    [ ""
    , "int main(void) {"
    ]
    ++
    concatMap foo (outputPins tree)
    ++
    [ "  while (1) {"
    , "    clock(0);"
    , "    _delay_ms(1000);"
    , "  }"
    , "  return 0;"
    , "}"
    ]

foo :: AST.Pin -> [String]
foo pin =
    ["  " ++ AST.directionRegister pin ++ " |= " ++ AST.pinMask pin ++ ";"
    ]

streamToToCHeader :: Streams -> Stream -> [String]
streamToToCHeader tree stream =
    [ ""
    , "static void " ++ name stream ++ "(" ++ streamInputString tree stream ++ ");"
    ]

streamToCBody :: Streams -> Stream -> [String]
streamToCBody tree stream =
    [ ""
    , "static void " ++ name stream ++ "(" ++ streamInputString tree stream ++ ") {"
    , "  " ++ (streamCType tree) stream ++ " output;"
    ]
    ++
    streamBodyToC (body stream)
    ++
    map (\x -> "  " ++ x ++ "(output);") (outputs stream)
    ++
    [ "}"
    ]

streamInputString :: Streams -> Stream -> String
streamInputString tree stream =
    intercalate ", " $
    map (\(input, t) -> t ++ " input_" ++ show input) $
    zip [0..] $
    map (streamCType tree) (map (streamFromId tree) (inputs stream))

streamBodyToC :: Body -> [String]
streamBodyToC body = case body of
    (OutputPin pin) ->
        [ "  if (input_0) {"
        , "    " ++ AST.portRegister pin ++ " |= " ++ AST.pinMask pin ++ ";"
        , "  } else {"
        , "    " ++ AST.portRegister pin ++ " &= ~(" ++ AST.pinMask pin ++ ");"
        , "  }"
        ]
    (Transform expression) ->
        [ "  output = " ++ expressionToC expression ++ ";"
        ]
    (Builtin name) ->
        [ "  static unsigned int i = 0U;"
        , "  i++;"
        , "  output = i;"
        ]

expressionToC :: AST.Expression -> String
expressionToC expression = case expression of
    (AST.Not expression) -> "!" ++ expressionToC expression
    (AST.Even expression) -> "(" ++ expressionToC expression ++ ") % 2 == 0"
    (AST.Input x) -> "input_" ++ show x

streamCType :: Streams -> Stream -> String
streamCType tree stream = case body stream of
    (OutputPin _) -> "bool"
    (Builtin "clock") -> "unsigned int"
    (Transform expression) -> expressionCType inputMap expression
    where
        inputMap = M.fromList $ zip [0..] $ map (streamCType tree) x
        x = (map (streamFromId tree) (inputs stream))

expressionCType :: M.Map Int String -> AST.Expression -> String
expressionCType inputMap (AST.Input x) = fromJust $ M.lookup x inputMap
expressionCType _        (AST.Not _)   = "bool"
expressionCType _        (AST.Even _)  = "bool"
