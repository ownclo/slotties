import Slotties.Slot

main :: IO ()
main = do
    let schedule = emptySchedule
        person1 = Person 1
        person2 = Person 2
        slot1 = Slot 1
        slot2 = Slot 2

        NewReservationOk newSchedule = reserveSlot slot1 person1 schedule

    print newSchedule
    print $ reserveSlot slot1 person1 newSchedule
    print $ reserveSlot slot1 person2 newSchedule
    print $ reserveSlot slot2 person1 newSchedule
    print $ reserveSlot slot2 person2 newSchedule
