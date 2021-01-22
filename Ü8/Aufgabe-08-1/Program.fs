module Program
open Mini

// b)
let rec queryNat (msg: String): Nat =
    putline ("Bitte geben Sie eine natuerliche Zahl ein :") 
    let eingabe = getline()
    let msg = readNat eingabe
    if msg <> Nat then putline "Eingabe ist keine natuerliche Zahl!"

   

// c)
let main(): Unit =
    putline ("Bitte geben Sie drei natuerliche Zahlen ein :") 
    let eingabe = getline()
    let x = readNat eingabe
    let y = readNat eingabe
    let z = readNat eingabe
    let Zahlen = [x ; y; z]
    if Zahlen <> Nat then putline "Eingabe ist keine natuerliche Zahl!"
        else min(zahl)
