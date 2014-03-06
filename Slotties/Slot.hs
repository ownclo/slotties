{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Slotties.Slot where

import Control.Applicative
import qualified Data.Bimap as BM
import Data.Bimap( Bimap )
import Data.SafeCopy
import Data.Typeable

data Person = Person {
        personId :: Int
    } deriving (Eq, Show, Ord, Typeable)

data Slot = Slot {
        slotTime :: Int
    } deriving (Eq, Show, Ord, Typeable)

data ReservationResult
    = NewReservationOk  Schedule
    | SameReservationOk Schedule
    | PersonHasSlot     Slot
    | SlotReservedBy    Person
    deriving (Show, Typeable)

type Schedule = Bimap Slot Person
instance Typeable Schedule

deriveSafeCopy 0 'base ''Person
deriveSafeCopy 0 'base ''Slot
deriveSafeCopy 0 'base ''ReservationResult

instance (SafeCopy a, SafeCopy b,
          Ord a, Ord b) =>
             SafeCopy (Bimap a b) where
    putCopy = contain . safePut . BM.toAscList
    getCopy = contain $ BM.fromAscPairListUnchecked <$> safeGet


emptySchedule :: Schedule
emptySchedule = BM.empty

-- Will be easier to program against
-- domain-specific interface (later).
getSlotHolder :: Slot -> Schedule -> Maybe Person
getSlotHolder = BM.lookup

getSlotForPerson :: Person -> Schedule -> Maybe Slot
getSlotForPerson = BM.lookupR

cancelReservation :: Person -> Schedule -> Schedule
cancelReservation = BM.deleteR

freeSlot :: Slot -> Schedule -> Schedule
freeSlot = BM.delete

maybeSchedule :: ReservationResult -> Maybe Schedule
maybeSchedule (NewReservationOk s) = Just s
maybeSchedule (SameReservationOk s) = Just s
maybeSchedule _ = Nothing

reserveSlot :: Slot
            -> Person
            -> Schedule
            -> ReservationResult
reserveSlot slot person schedule
    | (slot, person) `BM.pairMember` schedule
        = SameReservationOk schedule  -- nothing to do
    | Just holder <- getSlotHolder slot schedule
        = SlotReservedBy holder
    | Just slot' <- getSlotForPerson person schedule
        = PersonHasSlot slot'
    | otherwise  -- fresh slot-person pair
        = NewReservationOk $! BM.insert slot person schedule

transferReservation :: Slot
                    -> Person
                    -> Schedule
                    -> ReservationResult
transferReservation slot person =
        reserveSlot slot person . cancelReservation person

-- remove all slots that are older than provided one.
-- NOTE: filter is O(n), so we check for min beforehand.
-- NOTE: in case this is a bottleneck, try to recursively
-- 'findMin' and remove it while necessary. Supposing
-- that map is large and there are few outdated slots, this
-- can be a win.
removeOldSlots :: Slot -> Schedule -> Schedule
removeOldSlots now schedule
    | now < fst (BM.findMin schedule) = schedule  -- up-to-date
    | otherwise = BM.filter older schedule
        where older slot _ = slot < now
