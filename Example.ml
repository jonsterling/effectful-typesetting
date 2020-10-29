type size = [`normal | `big | `Big | `bigg | `Bigg]
type node = [`Str of string | `Size of size | `Lparen | `Rparen]

module Size =
struct
  type t = size

  let int_of_size =
    function
    | `normal -> 0
    | `big -> 1
    | `Big -> 2
    | `bigg -> 3
    | `Bigg -> 4

  let default = `normal

  let merge s0 s1 =
    if int_of_size s0 > int_of_size s1
    then s0
    else s1
end

module TeX =
struct
  type t = node list
  let empty = []
  let append = List.append
end


module B = Bubble.Make (Size) (TeX)

let delim ~left ~right bdy =
  B.scope (B.join bdy) @@ fun size out ->
  B.write @@
  [`Size size; left]
  @ out
  @ [`Size size; right]

let prn : B.doc list -> B.doc =
  delim ~left:`Lparen ~right:`Rparen

let str x = B.write [`Str x ]

let example : B.doc =
  prn [prn [B.bubble `Big; str "asdf"]; prn [str "foo"]]

let foo =
  B.run example
