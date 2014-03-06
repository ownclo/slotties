{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Slotties.SlotAcid where

import Control.Monad.Reader( asks )
import Control.Monad.State( modify )
import Control.Lens( makeLenses
                   , views
                   , over
                   , use
                   , (.=)
                   )

import Data.Acid
import Data.SafeCopy
import Data.Typeable

import Slotties.Slot

data ScheduleDB = ScheduleDB {
        _allSchedule :: Schedule
    } deriving Typeable

makeLenses ''ScheduleDB
deriveSafeCopy 0 'base ''ScheduleDB

emptyScheduleDB :: ScheduleDB
emptyScheduleDB = ScheduleDB emptySchedule

queryAllSchedule :: Query ScheduleDB Schedule
queryAllSchedule = asks _allSchedule

getSlotHolderDB :: Slot -> Query ScheduleDB (Maybe Person)
getSlotHolderDB = views allSchedule . getSlotHolder

getSlotForPersonDB :: Person -> Query ScheduleDB (Maybe Slot)
getSlotForPersonDB = views allSchedule . getSlotForPerson

cancelReservationDB :: Person -> Update ScheduleDB ()
cancelReservationDB = modify . over allSchedule . cancelReservation

freeSlotDB :: Slot -> Update ScheduleDB ()
freeSlotDB = modify . over allSchedule . freeSlot

reserveSlotDB :: Slot
              -> Person
              -> Update ScheduleDB ReservationResult
reserveSlotDB slot person = do
    schedule <- use allSchedule
    updateIfNew $ reserveSlot slot person schedule

transferReservationDB :: Slot
                      -> Person
                      -> Update ScheduleDB ReservationResult
transferReservationDB slot person = do
    schedule <- use allSchedule
    updateIfNew $ transferReservation slot person schedule

updateIfNew :: ReservationResult
            -> Update ScheduleDB ReservationResult
updateIfNew res = do
        case res of
          NewReservationOk s -> allSchedule .= s
          _ -> return ()
        return res

removeOldSlotsDB :: Slot -> Update ScheduleDB ()
removeOldSlotsDB = modify . over allSchedule . removeOldSlots


makeAcidic ''ScheduleDB
    [ 'queryAllSchedule
    , 'getSlotHolderDB
    , 'getSlotForPersonDB
    , 'cancelReservationDB
    , 'freeSlotDB
    , 'reserveSlotDB
    , 'transferReservationDB
    , 'removeOldSlotsDB
    ]
