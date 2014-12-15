module Stages.CodeGen where

import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified Data.Map as M

import qualified Types.AST as AST
import Types.DAG

streamsToC :: Streams -> String
streamsToC streams = unlines $
    [ "#include <avr/io.h>"
    , "#include <util/delay.h>"
    , "#include <stdbool.h>"
    ]
    ++
    concatMap (streamToFunctionDeclaration streams) (streamsInTree streams)
    ++
    concatMap (streamToFunctionDefinition streams) (streamsInTree streams)
    ++
    [ ""
    , "int main(void) {"
    ]
    ++
    concatMap pinInitToC (outputPins streams)
    ++
    [ "  while (1) {"
    , "    clock(0);"
    , "    _delay_ms(1000);"
    , "  }"
    , "  return 0;"
    , "}"
    ]

pinInitToC :: AST.Pin -> [String]
pinInitToC pin =
    ["  " ++ AST.directionRegister pin ++ " |= " ++ AST.pinMask pin ++ ";"
    ]

streamToFunctionDeclaration :: Streams -> Stream -> [String]
streamToFunctionDeclaration streams stream =
    [ ""
    , "static void " ++ name stream ++ "(" ++ streamToArgumentList streams stream ++ ");"
    ]

streamToFunctionDefinition :: Streams -> Stream -> [String]
streamToFunctionDefinition streams stream =
    [ ""
    , "static void " ++ name stream ++ "(" ++ streamToArgumentList streams stream ++ ") {"
    , "  " ++ (streamCType streams) stream ++ " output;"
    ]
    ++
    streamBodyToC (body stream)
    ++
    map (\x -> "  " ++ x ++ "(output);") (outputs stream)
    ++
    [ "}"
    ]

streamToArgumentList :: Streams -> Stream -> String
streamToArgumentList streams stream =
    intercalate ", " $
    map (\(input, t) -> t ++ " input_" ++ show input) $
    zip [0..] $
    map (streamCType streams) (map (streamFromId streams) (inputs stream))

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
streamCType streams stream = case body stream of
    (OutputPin _) -> "bool"
    (Builtin "clock") -> "unsigned int"
    (Transform expression) -> expressionCType inputMap expression
    where
        inputMap = M.fromList $ zip [0..] $ map (streamCType streams) x
        x = (map (streamFromId streams) (inputs stream))

expressionCType :: M.Map Int String -> AST.Expression -> String
expressionCType inputMap (AST.Input x) = fromJust $ M.lookup x inputMap
expressionCType _        (AST.Not _)   = "bool"
expressionCType _        (AST.Even _)  = "bool"
