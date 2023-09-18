let g x = (let y = x + x in 
    fun z -> y + z) in
let machin h = h 150 in
let truc = g (if 1 then 1 else 1000) in
machin truc