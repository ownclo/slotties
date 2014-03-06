import Slotties.Slot
import Slotties.SlotAcid

import Data.Acid

-- TODO: - Build a command-line interface
--       - Possibly deal with poor linked list perf
--       - Add a notion of current time
main :: IO ()
main = do
    let person1  = Person 1
        slot1    = Slot 1
        _person2 = Person 2
        _slot2   = Slot 2

    state <- openLocalState emptyScheduleDB
    schedule <- query state QueryAllSchedule
    print schedule

    res <- update state $ ReserveSlotDB slot1 person1
    print res
