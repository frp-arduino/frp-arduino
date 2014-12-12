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

streamToToCHeader :: Stream -> [String]
streamToToCHeader stream =
    [ ""
    , "static void " ++ name stream ++ "(void *input);"
    ]

streamToCBody :: Stream -> [String]
streamToCBody stream =
    [ ""
    , "static void " ++ name stream ++ "(void *input) {"
    , "  void *output;"
    ]
    ++
    streamBodyToC (body stream)
    ++
    map (\x -> "  " ++ x ++ "(output);") (dependencies stream)
    ++
    [ "}"
    ]

streamBodyToC :: Body -> [String]
streamBodyToC (OutputPin pin) =
    [ "  if (*((bool*)input)) {"
    , "    " ++ AST.portRegister pin ++ " |= " ++ AST.pinMask pin ++ ";"
    , "  } else {"
    , "    " ++ AST.portRegister pin ++ " &= ~(" ++ AST.pinMask pin ++ ");"
    , "  }"
    ]
streamBodyToC (Builtin "toggle") =
    [ "  static bool value = true;"
    , "  value = !value;"
    , "  output = (void*)(&value);"
    ]
streamBodyToC (Builtin "invert") =
    [ "  bool value = !(*((bool*)input));"
    , "  output = (void*)(&value);"
    ]
streamBodyToC (Builtin _) =
    [
    ]
