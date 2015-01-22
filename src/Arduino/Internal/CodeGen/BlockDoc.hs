-- Copyright (c) 2014 Contributors as noted in the AUTHORS file
--
-- This file is part of frp-arduino.
--
-- frp-arduino is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- frp-arduino is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with frp-arduino.  If not, see <http://www.gnu.org/licenses/>.

module Arduino.Internal.CodeGen.BlockDoc
    ( Gen()
    , runGen
    , header
    , line
    , block
    , label
    ) where

import Control.Monad.State

data GenState = GenState
    { labelCounter :: Int
    , getBlock     :: Block
    }

data Block = Block
    { headerLines :: [String]
    , bodyLines   :: [String]
    }

type Gen a = State GenState a

indentLevel :: Int
indentLevel = 2

runGen :: Gen a -> String
runGen gen = unlines $ blockToLines $ getBlock $ execState gen emptyGenState

blockToLines :: Block -> [String]
blockToLines block = reverse (headerLines block) ++ reverse (bodyLines block)

emptyGenState :: GenState
emptyGenState = GenState 0 (Block [] [])

label :: Gen String
label = do
    genState <- get
    modify $ \genState -> genState { labelCounter = 1 + labelCounter genState }
    return $ "temp" ++ show (labelCounter genState)

block :: String -> Gen a -> Gen a
block initText gen = do
    line initText
    beforeBlock <- newBlock
    result <- gen
    afterBlock <- restoreBlock beforeBlock
    mapM_ indentedLine (blockToLines afterBlock)
    return result

newBlock :: Gen Block
newBlock = do
    genState <- get
    modify $ \genState -> genState { getBlock = Block [] [] }
    return (getBlock genState)

restoreBlock :: Block -> Gen Block
restoreBlock block = do
    genState <- get
    modify $ \genState -> genState { getBlock = block }
    return (getBlock genState)

header :: String -> Gen ()
header text = modifyBlock (prependHeaderLine text)

indentedLine :: String -> Gen ()
indentedLine text = line $ (replicate indentLevel ' ') ++ text

line :: String -> Gen ()
line text = modifyBlock (prependBodyLine text)

modifyBlock :: (Block -> Block) -> Gen ()
modifyBlock fn = modify $ \genState ->
    genState { getBlock = fn (getBlock genState) }

prependHeaderLine :: String -> Block -> Block
prependHeaderLine text block = block { headerLines = text : headerLines block }

prependBodyLine :: String -> Block -> Block
prependBodyLine text block = block { bodyLines = text : bodyLines block }
