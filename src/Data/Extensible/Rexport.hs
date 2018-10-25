module Data.Extensible.Rexport
  ( module X
  , OrigRecord
  ) where
import           Data.Extensible as X hiding (Nullable, Record, record)
import qualified Data.Extensible as E
type OrigRecord = E.Record
