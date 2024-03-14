val (+) : Stlc.gremVal -> Stlc.gremVal -> Stlc.gremVal
val (-) : Stlc.gremVal -> Stlc.gremVal -> Stlc.gremVal
val (+*) : Stlc.gremVal -> Stlc.gremVal -> Stlc.gremVal
val (/) : Stlc.gremVal -> Stlc.gremVal -> Stlc.gremVal
val (>) : Stlc.gremVal -> Stlc.gremVal -> Stlc.gremVal
val (>=) : Stlc.gremVal -> Stlc.gremVal -> Stlc.gremVal
val (<) : Stlc.gremVal -> Stlc.gremVal -> Stlc.gremVal
val (<=) : Stlc.gremVal -> Stlc.gremVal -> Stlc.gremVal
val (=) : Stlc.gremVal -> Stlc.gremVal -> Stlc.gremVal
val (<>) : Stlc.gremVal -> Stlc.gremVal -> Stlc.gremVal
val unpackInteger : Stlc.gremVal -> int
val unpackBoolean : Stlc.gremVal -> bool
val unpackClosure : Stlc.gremVal -> 
                    (Stlc.livBinder * Stlc.livTerm * Stlc.gremEnv)
