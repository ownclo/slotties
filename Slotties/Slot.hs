module Slotties.Slot where

import Data.Function( on )

import qualified Data.Bimap as BM
import Data.Bimap( Bimap )

data Person = Person {
        personId :: Int
    } deriving (Eq, Show, Ord)

data Slot = Slot {
        slotTime :: Int
    } deriving (Eq, Show, Ord)

type Schedule = Bimap Slot Person

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

data ReservationResult
    = NewReservationOk  Schedule
    | SameReservationOk Schedule
    | PersonHasSlot     Slot
    | SlotReservedBy    Person
    deriving Show

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
