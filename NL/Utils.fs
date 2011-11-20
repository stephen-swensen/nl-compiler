﻿namespace Swensen.NL

module Option =
    let fromNullable nullable =
        match nullable with
        | null -> None
        | _ -> Some(nullable)

