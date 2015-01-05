module CCodeGen where

import Control.Monad.State

data GenState = GenState
    { labelCounter :: Int
    , indentLevel  :: Int
    , headerLines  :: [String]
    , bodyLines    :: [String]
    }

type Gen a = State GenState a

runGen :: Gen a -> String
runGen gen = unlines $ reverse (headerLines genState) ++
                       reverse (bodyLines genState)
    where
        genState = execState gen emptyGenState

emptyGenState :: GenState
emptyGenState = GenState 0 0 [] []

label :: Gen String
label = do
    genState <- get
    modify $ \genState -> genState { labelCounter = 1 + labelCounter genState }
    return $ "temp" ++ show (labelCounter genState)

block :: String -> Gen a -> Gen a
block x gen = do
    line x
    indent gen
    where
        indent :: Gen a -> Gen a
        indent gen = do
            modify $ \genState -> genState { indentLevel = indentLevel genState + 1 }
            x <- gen
            modify $ \genState -> genState { indentLevel = indentLevel genState - 1 }
            return x

header :: String -> Gen ()
header line = do
    modify (prependLine line)
    where
        prependLine line genState = genState { headerLines = line : headerLines genState }

line :: String -> Gen ()
line line = do
    modify (prependLine line)
    where
        prependLine line genState = genState { bodyLines = ((concat (replicate (indentLevel genState) "  ")) ++ line) : bodyLines genState }

cFunction :: String -> Gen a -> Gen a
cFunction declaration gen = do
    header $ ""
    header $ declaration ++ ";"
    line $ ""
    x <- block (declaration ++ " {") gen
    line $ "}"
    return x
