// Hrmm...  if vs then?

// More traditional
5 2 + 7 = if {
    "hello" printf
} else {
    "world" printf
}

// Less confusing since the predicate comes _before_
5 2 + 7 = then {
    "hello" printf
} else (true) then {
    "world" printf
}

// Maybe _EITHER_ :eyes:

// Switch

5 2 + switch {
    7 -> "hello" printf,
    8 -> {
        "world" printf
    }
    _ -> "die" printf,
}
