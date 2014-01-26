(* Steuerklassen mit Records und Varianten---OCaml
 * Quelle: http://steuerklassen.biz/
 *)

type geld = float

type ehegatte = { einkommen : geld }

type familienstand =
  [ `ledig
  | `verheiratet of ehegatte
  | `getrennt
  | `verwitwet
  ]

(* (Genauer gesagt ist `familienstand` ein Typ von
   "polymorphischen" Varianten.) *)

type mensch = { familienstand : familienstand }

(* Steuerklasse 1
 * gilt für ledige, geschiedene und lӓngst verwitwete Arbeitnehmer.
 *)

let michel : mensch
  = { familienstand = `ledig }

let steuerklasse1 : mensch -> bool
  = fun michel ->
    match michel.familienstand with
        `verheiratet _ -> false
      | _ -> true

let michel_hat_sk1 = steuerklasse1 michel

(* Steuerklasse 2
 * gilt für die unter Steuerklasse 1 genannten Arbeitnehmer,
 * wenn ihnen der Entlastungsbetrag für Alleinerziehende zusteht.
 *)

type familienmensch =
 { familienstand : familienstand
 ; kinder        : int
 }

let wilhelm : familienmensch
  = { familienstand = `verwitwet
    ; kinder        = 2 (* Hӓnsel und Gretel *)
    }

let steuerklasse2 : familienmensch -> bool
  = fun wilhelm ->
    match wilhelm.familienstand with
        `verheiratet _ -> false
      | _ -> wilhelm.kinder > 0

let wilhelm_hat_sk2 = steuerklasse2 wilhelm

(* let wilhelm_hat_sk1 = steuerklasse1 wilhelm
 *
 * Typfehler: OCaml unterstüzt kein Subtyping für Records
 *)

(* Steuerklasse 3
 * gilt auf Antrag für verheiratete Arbeitnehmer, wenn
 * der Ehegatte des Arbeitnehmers keinen Arbeitslohn bezieht
 * oder Arbeitslohn bezieht und in die Steuerklasse 5 eingereiht
 * wird.
 *
 * Die Steuerklassenkombination 3/5 ist so gestaltet, dass die
 * Summe der Steuerabzugsbeträge für beide Ehegatten in etwa der
 * gemeinsamen Jahressteuer entspricht, wenn der Ehegatte mit
 * Steuerklasse 3 ca. 60 % und der Ehegatte mit Steuerklasse 5
 * ca. 40 % des gemeinsamen Arbeitseinkommens erzielt.
 *)

type arbeitnehmer =
  { familienstand : familienstand
  ; lohn : geld
  }

let max : ehegatte = { einkommen = 33.00 }

let erika : arbeitnehmer =
  { familienstand = `verheiratet max
  ; lohn = 67.00
  }

let steuerklasse3 : arbeitnehmer -> bool
  = fun erika ->
    match erika.familienstand with
        `verheiratet max ->
          let gemeinsamesEinkommen = erika.lohn +. max.einkommen
          in  (erika.lohn /. gemeinsamesEinkommen) >= 0.60
      | _ -> false

let erika_hat_sk3 = steuerklasse3 erika

(* Steuerklasse 5
 * gilt für einen der Ehegatten, wenn der andere Ehegatte auf
 * Antrag beider Ehegatten in die Steuerklasse 3 eingereiht wird.
 *)

let erika : ehegatte = { einkommen = 65.00 }

let max : arbeitnehmer =
  { familienstand = `verheiratet erika
  ; lohn = 35.00
  }

let steuerklasse5 : arbeitnehmer -> bool
  = fun max ->
    match max.familienstand with
        `verheiratet erika ->
          let gemeinsamesEinkommen = max.lohn +. erika.einkommen
          in  (max.lohn /. gemeinsamesEinkommen) <= 0.40
      | _ -> false

let max_hat_sk5 = steuerklasse5 max

(* Steuerklasse 4
 * gilt für verheiratete Arbeitnehmer, wenn beide Ehegatten
 * Arbeitslohn beziehen, im Inland wohnen und nicht dauernd
 * getrennt leben.
 *)

let lieschen : ehegatte = { einkommen = 45.00 }

let otto : arbeitnehmer =
  { familienstand = `verheiratet lieschen
  ; lohn = 55.00
  }

let steuerklasse4 : arbeitnehmer -> bool
  = fun otto ->
    match otto.familienstand with
        `verheiratet _ ->
          not (steuerklasse3 otto) &&
            not (steuerklasse5 otto)
      | _ -> false

let otto_hat_sk4 = steuerklasse4 otto

(* Jetzt fassen wir alles zusammen. *)

type person =
  { familienstand : familienstand
  ; kinder        : int
  ; lohn          : geld
  }

let michel : person =
  { familienstand = `ledig
  ; kinder = 0
  ; lohn = 100.0
  }

let wilhelm : person =
  { familienstand = `verwitwet
  ; kinder = 2
  ; lohn = 100.0
  }

let erikasMann = { einkommen = 35.00 }
let maxsFrau   = { einkommen = 65.00 }

let erika : person =
  { familienstand = `verheiratet erikasMann
  ; kinder = 2 (* Markus & Leon Mustermann *)
  ; lohn = 65.00
  }

let max : person =
  { familienstand = `verheiratet maxsFrau
  ; kinder = 2 (* Markus & Leon Mustermann *)
  ; lohn = 35.00
  }

let lieschensMann = { einkommen = 55.00 }
let ottosFrau     = { einkommen = 45.00 }

let lieschen : person =
  { familienstand = `verheiratet lieschensMann
  ; kinder = 0
  ; lohn = 45.00
  }

let otto : person =
  { familienstand = `verheiratet ottosFrau
  ; kinder = 0
  ; lohn = 55.00
  }

let steuerklasse : person -> int
  = fun person ->
    (* Bitte vervollstӓndigen Sie diese Funktion. *)
    0

let test1 = 1 == steuerklasse michel
let test2 = 2 == steuerklasse wilhelm
let test3 = 3 == steuerklasse erika
let test4 = 5 == steuerklasse max
let test5 = 4 == steuerklasse lieschen
let test6 = 4 == steuerklasse otto
