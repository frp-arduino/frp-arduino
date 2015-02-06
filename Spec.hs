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

import Arduino.Uno
import Control.Monad (when)
import Data.List
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Test.Hspec

main :: IO ()
main = hspec $ do

    describe "cabal file" $ do

        describe "sdist" $ do

            it "contains all source code" $ do
                name <- getCabalName
                let extracted = "dist/" ++ name
                assertSucceeds ["cabal", "sdist"] "."
                assertSucceeds ["rm", "-rf", name] "dist"
                assertSucceeds ["tar", "xvvf", name ++ ".tar.gz"] "dist"
                assertSucceeds ["cabal", "configure"] extracted
                assertSucceeds ["cabal", "build"] extracted
                assertSucceeds ["diff", "-r", "../../src", "src"] extracted

    describe "dsl" $ do

        describe "prevents multiple 'resource' usage" $ do

            it "same output is assigned to twice" $ do
                let result = parseProgram $ do
                    digitalOutput pin12 =: digitalRead pin11
                    digitalOutput pin12 =: digitalRead pin11
                result `shouldBe` Left ["pin12 used twice"]

            it "same pin is used both as input and output" $ do
                let result = parseProgram $ do
                    digitalOutput pin12 =: digitalRead pin12
                result `shouldBe` Left ["pin12 used twice"]

getCabalName :: IO String
getCabalName = do
    cabalLines <- fmap lines $ readFile "frp-arduino.cabal"
    return $ "frp-arduino-" ++ extractKey versionPrefix cabalLines
    where
        extractKey versionPrefix (x:xs)
            | versionPrefix `isPrefixOf` x = drop (length versionPrefix) x
            | otherwise                    = extractKey versionPrefix xs
        versionPrefix = "version:             "

assertSucceeds args cwd = do
    root <- getCurrentDirectory
    let testReportPath = root </> "test_report.txt"
    let shellCmd = intercalate " " args ++ " > " ++ testReportPath ++ " 2>&1"
    (_, _, _, p) <- createProcess (shell shellCmd){ cwd = Just cwd }
    exitCode <- waitForProcess p
    testReport <- readFile testReportPath
    removeFile testReportPath
    when (exitCode /= ExitSuccess) $ do
        expectationFailure $ cwd ++ "> " ++ shellCmd ++ "\n\n" ++ testReport
