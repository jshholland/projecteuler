module Main where

import Control.Arrow ((***))

{-
You are given the following information, but you may prefer to do some research for yourself.

1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
-}

type Year = Int
data Day  = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show, Eq, Bounded, Enum, Ord)
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  deriving (Show, Eq, Bounded, Enum, Ord)
type IsLeap = Bool

data Date = Date
  { day   :: Int
  , month :: Month
  , year  :: Year
  } deriving (Show, Eq)

daysInMonth :: IsLeap -> Month -> Int
daysInMonth _      Jan = 31
daysInMonth False  Feb = 28
daysInMonth True   Feb = 29
daysInMonth _      Mar = 31
daysInMonth _      Apr = 30
daysInMonth _      May = 31
daysInMonth _      Jun = 30
daysInMonth _      Jul = 31
daysInMonth _      Aug = 31
daysInMonth _      Sep = 30
daysInMonth _      Oct = 31
daysInMonth _      Nov = 30
daysInMonth _      Dec = 31

isLeap :: Year -> IsLeap
isLeap y = (y `mod` 4 == 0) && (y `mod` 100 /= 0) || (y `mod` 400 == 0)

next :: Day -> Day
next Sun = Mon
next d   = succ d

tomorrow :: Date -> Date
tomorrow (Date d m y)
  | d + 1 <= daysInMonth (isLeap y) m = Date (d+1) m y
  | m /= Dec = Date 1 (succ m) y
  | otherwise = Date 1 Jan (y + 1)

days :: [(Date, Day)]
days = iterate (tomorrow *** next) (Date 1 Jan 1901, Tue)

firstSundays :: [(Date, Day)] -> [(Date, Day)]
firstSundays = filter ((== Sun) . snd) . filter ((== 1) . day . fst)

main :: IO ()
main = print . length . firstSundays $ takeWhile ((<= 2000) . year . fst) days
