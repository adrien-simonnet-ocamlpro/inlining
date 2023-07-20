let stdout = ref Format.std_formatter
let level = ref 0

let start s = level := !level + 1; for _i = 1 to !level - 1 do Format.fprintf !stdout " " done; Format.fprintf !stdout s
let stop () = level := !level - 1
let log s = for _i = 1 to !level do Format.fprintf !stdout " " done; Format.fprintf !stdout s
