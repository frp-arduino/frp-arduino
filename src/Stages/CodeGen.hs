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

pinInitToC :: AST.Output -> [String]
pinInitToC (AST.Pin _ _ directionRegister pinMask) =
    ["  " ++ directionRegister ++ " |= " ++ pinMask ++ ";"
    ]
pinInitToC (AST.UART) =
    [ "  #define F_CPU 16000000UL"
    , "  #define BAUD 9600"
    , "  #include <util/setbaud.h>"
    , "  UBRR0H = UBRRH_VALUE;"
    , "  UBRR0L = UBRRL_VALUE;"
    , "  #if USE_2X"
    , "      UCSR0A |= (1 << U2X0);"
    , "  #else"
    , "      UCSR0A &= ~((1 << U2X0));"
    , "  #endif"
    , "  UCSR0C = (1 << UCSZ01) |(1 << UCSZ00);"
    , "  UCSR0B = (1 << RXEN0) | (1 << TXEN0);"
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
    (OutputPin (AST.Pin _ portRegister _ pinMask)) ->
        [ "  if (input_0) {"
        , "    " ++ portRegister ++ " |= " ++ pinMask ++ ";"
        , "  } else {"
        , "    " ++ portRegister ++ " &= ~(" ++ pinMask ++ ");"
        , "  }"
        ]
    (OutputPin (AST.UART)) ->
        [ "  while (*input_0 != 0) {"
        , "    while ((UCSR0A & (1 << UDRE0)) == 0) {"
        , "    }"
        , "    UDR0 = *input_0;"
        , "    input_0++;"
        , "  }"
        ]
    (Transform expression) ->
        extraLines
        ++
        [ "  output = " ++ expressionString ++ ";"
        ]
        where
            (expressionString, extraLines) = expressionToC expression
    (Builtin name) ->
        [ "  static unsigned int i = 0U;"
        , "  i++;"
        , "  output = i;"
        ]

expressionToC :: AST.Expression -> (String, [String])
expressionToC expression = case expression of
    (AST.Not expression) -> ("!" ++ expressionString, extraLines)
        where
            (expressionString, extraLines) = expressionToC expression
    (AST.Even expression) -> ("(" ++ expressionString ++ ") % 2 == 0", extraLines)
        where
            (expressionString, extraLines) = expressionToC expression
    (AST.Input value) -> ("input_" ++ show value, [])
    (AST.StringConstant value) -> ("tmp", ["  char tmp[] = " ++ show value ++ ";"])

streamCType :: Streams -> Stream -> String
streamCType streams stream = case body stream of
    (OutputPin (AST.Pin _ _ _ _)) -> "bool"
    (OutputPin (AST.UART)) -> "char *"
    (Builtin "clock") -> "unsigned int"
    (Transform expression) -> expressionCType inputMap expression
    where
        inputMap = M.fromList $ zip [0..] $ map (streamCType streams) x
        x = (map (streamFromId streams) (inputs stream))

expressionCType :: M.Map Int String -> AST.Expression -> String
expressionCType inputMap (AST.Input x)           = fromJust $ M.lookup x inputMap
expressionCType _        (AST.Not _)             = "bool"
expressionCType _        (AST.Even _)            = "bool"
expressionCType _        (AST.StringConstant _)  = "char *"
