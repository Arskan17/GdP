[<AutoOpen>]
module PriorityQueueTypes
open Mini

type QElem<'a> =       // Element der Warteschlange
    { priority: Nat    // Priorität
      value: 'a }      // beliebiger Wert

type PQ<'a> = List<QElem<'a>> // Die Prioritätswarteschlange