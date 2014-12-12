module CodeGen where

import qualified AST
import Stream

programToC :: AST.Program -> String
programToC = streamTreeToC . programToStreamTree

streamTreeToC :: StreamTree -> String
streamTreeToC tree = unlines $
    [ "#include <avr/io.h>"
    , "#include <util/delay.h>"
    , "#include <stdbool.h>"
    ]
    ++
    concatMap streamToToCHeader (streamsInTree tree)
    ++
    concatMap streamToCBody (streamsInTree tree)
    ++
    [ ""
    , "int main(void) {"
    ]
    ++
    concatMap foo (outputPins tree)
    ++
    [ "  while (1) {"
    , "    clock();"
    , "    _delay_ms(1000);"
    , "  }"
    , "  return 0;"
    , "}"
    ]

foo :: AST.Pin -> [String]
foo pin =
    ["  " ++ AST.directionRegister pin ++ " |= " ++ AST.pinMask pin ++ ";"
    ]

streamToToCHeader :: Stream -> [String]
streamToToCHeader (Stream name inputType outputType body dependencies) =
    [ ""
    , "static void " ++ name ++ "(" ++ toCInType inputType ++ ");"
    ]

streamToCBody :: Stream -> [String]
streamToCBody (Stream name inputType outputType body dependencies) =
    [ ""
    , "static void " ++ name ++ "(" ++ toCInType inputType ++ ") {"
    ]
    ++
    streamBodyToC body
    ++
    map (\x -> "  " ++ x ++ "(" ++ toCOutType outputType ++ ");") dependencies
    ++
    [ "}"
    ]

toCInType :: StreamType -> String
toCInType StreamTypeVoid = "void"
toCInType StreamTypeBool = "bool input"

toCOutType :: StreamType -> String
toCOutType StreamTypeVoid = ""
toCOutType _              = "value"

streamBodyToC :: Body -> [String]
streamBodyToC (OutputPin pin) =
    [ "  if (input) {"
    , "    " ++ AST.portRegister pin ++ " |= " ++ AST.pinMask pin ++ ";"
    , "  } else {"
    , "    " ++ AST.portRegister pin ++ " &= ~(" ++ AST.pinMask pin ++ ");"
    , "  }"
    ]
streamBodyToC (Builtin "toggle") =
    [ "  static bool value = true;"
    , "  value = !value;"
    ]
streamBodyToC (Builtin "invert") =
    [ "  bool value = !input;"
    ]
streamBodyToC (Builtin _) =
    [
    ]
