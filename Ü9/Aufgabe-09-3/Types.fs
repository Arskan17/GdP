[<AutoOpen>]
module Types
open Mini

type Item<'a> =
    { mutable value: 'a
      mutable next: Option<Item<'a>> }

type MList<'a> =
    { mutable first: Option<Item<'a>>
      mutable last: Option<Item<'a>>
      mutable size: Nat }
