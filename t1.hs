type Name = String
type Amount = Integer
type NextApplication = Integer
type CurrentTime = Integer
type Time = [Integer]
type Medicine = (Name,Amount)
type Medicines = [Medicine]
type Prescription = (Name,Time,NextApplication)
type MedicinePlan = [Prescription]
type Price = Int
type Pharmacy = (Name,[(Medicine,Price)])
type Market = [Pharmacy]
type Buy = (Price, Nome)

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

inList :: (Eq a) => a -> [a] -> Bool
inList _ [] = False
inList a (x:xs) = a == x || a `inList` xs

addMed :: Medicine -> Medicines -> Medicines 
addMed med medList = map isMed medList
        where 
    isMed medItem 
            | fst medItem == fst med = (fst medItem, snd med + snd medItem)
            | otherwise = medItem
    
removeMed :: Name -> Medicines -> Medicines
removeMed medName medList = filter (\medItem -> medName /= fst medItem) medList

getMed :: Name -> Medicines -> Medicine
getMed medName medList
            | listFiltered == [] = ("", 0)
            | length listFiltered == 1 = head listFiltered
            where listFiltered = filter (\medItem ->  medName == fst medItem) medList 

changeMed :: Medicine -> Medicines -> Medicines
changeMed med medList = [if fst medItem == fst med then med else medItem | medItem <- medList]

takeMedsSOS :: Name -> Medicines -> Medicines
takeMedsSOS medName medList = [if fst medItem == medName then (fst medItem, snd medItem - 1) else medItem | medItem <- medList]

takeMedsTime :: MedicinePlan -> Medicines -> CurrentTime -> (MedicinePlan, Medicines)
takeMedsTime plan meds currTime = ([(name, [h | h <- hours, h /= currTime], next ) | (name, hours, next) <- plan], 
                                    [if currTime `inList` time && name == fst med then (fst med, snd med -1) else med 
                                        | (name, time, _) <- plan, med <- meds])

registerAlarms :: MedicinePlan -> Time 
registerAlarms plan = quicksort . concat $ [time | (_, time, _) <- plan ]

listMedicinesToBuy :: Medicines -> Medicines
listMedicinesToBuy meds = [med | med <- meds, snd med == 0]

buyMedicinesDays :: MedicinePlan -> Medicines -> Int -> Medicines
buyMedicinesDays plan meds days = [(name, -1 * (amount - toInteger ((length time) * days))) 
                                        | (name, time, _) <- plan, let amount = snd $ getMed name meds]

meds = [("Tylenol", 0), ("Dipirona", 2)]
plan = [("Tylenol", [toInteger(5), toInteger(7)], 6), ("Dipirona", [5, 9], 6)]
