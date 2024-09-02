open Frontend

[<EntryPoint>]
let main(args) =
    let cfg = parse_args (Array.toList args)
    in run_with_cfg cfg |> ignore
    0
