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

module Arduino.Library.Time
    ( accumulator
    , accumulatorConstLimit
    ) where

import Arduino.DSL
import Arduino.Library.Tuples
import Prelude hiding (Word)

-- | The snippet
--
-- @accumulator limitStream deltaStream@
--
-- creates a stream that produces a unit value every time the accumulated
-- deltas have reached the limit.
accumulator :: Stream Word -> Stream Word -> Stream ()
accumulator limitStream deltaStream = unitStream
    where
        unitStream = mapS (const unit) wrappedStream
        wrappedStream = filterS2Tuple isWrap didWrapTotalStream
        didWrapTotalStream = foldpS accumulate (pack2 (0, 0)) limitDeltaStream
        limitDeltaStream = pack2Stream limitStream deltaStream

        accumulate :: Expression (Word, Word)
                   -> Expression (Word, Word)
                   -> Expression (Word, Word)
        accumulate limitDelta didWrapTotal =
            let (limit, delta) = unpack2 limitDelta
                (didWrap, total) = unpack2 didWrapTotal
            in if_ (greater (total+delta) limit)
                   (pack2 (0, 0))
                   (pack2 (1, total+delta))

        isWrap :: (Expression Word, Expression Word) -> Expression Bool
        isWrap (didWrap, _) = isEqual didWrap 0

accumulatorConstLimit :: Expression Word -> Stream Word -> Stream ()
accumulatorConstLimit limit = accumulator (constStream limit)
