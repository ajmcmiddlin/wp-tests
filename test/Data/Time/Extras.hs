module Data.Time.Extras where

import           Control.Applicative (liftA2)
import           Data.Time           (LocalTime (LocalTime), NominalDiffTime,
                                      diffUTCTime, fromGregorian,
                                      getCurrentTime, localTimeToUTC,
                                      nominalDay, secondsToDiffTime,
                                      timeToTimeOfDay, utc, utcToLocalTime)

nominalWithin ::
  NominalDiffTime
  -> LocalTime
  -> LocalTime
  -> Bool
nominalWithin delta a b =
  abs (nominalDiff a b) < delta

nominalDiff ::
  LocalTime
  -> LocalTime
  -> NominalDiffTime
nominalDiff a b =
  liftA2 diffUTCTime ($ a) ($ b) (localTimeToUTC utc)

nominalHour ::
  NominalDiffTime
nominalHour =
  nominalDay / 24

nominalMinute ::
  NominalDiffTime
nominalMinute =
  nominalHour / 60

nominalSecond ::
  NominalDiffTime
nominalSecond =
  nominalMinute / 60
