module type Merge =
sig
  type t
  val default : t
  val merge : t -> t -> t
end

module type Monoid =
sig
  type t
  val empty : t
  val append : t -> t -> t
end

module Builder (S : Merge) (N : Monoid) :
sig
  type doc
  val bubble : S.t -> doc
  val write : N.t -> doc
  val join : doc list -> doc
  val scope : doc -> (S.t -> N.t -> doc) -> doc

  val run : doc -> N.t * S.t
end =
struct
  effect Bubble : S.t -> unit
  effect Write : N.t -> unit

  type doc = unit -> unit

  let bubble st () =
    perform @@ Bubble st

  let write x () =
    perform @@ Write x

  let rec join docs =
    match docs with
    | [] -> fun () -> ()
    | doc :: docs ->
      fun () ->
        doc ();
        join docs ()

  let run doc =
    match doc () with
    | () -> N.empty, S.default
    | effect (Write x) k ->
      let out, st = continue k () in
      N.append x out, st
    | effect (Bubble st) k ->
      let out, st' = continue k () in
      out, S.merge st st'

  let scope (doc : doc) (kont : S.t -> N.t -> doc) : doc =
    fun () ->
    let out, st =
      match doc () with
      | () -> N.empty, S.default
      | effect (Write x) k ->
        let out, st = continue k () in
        N.append x out, st
      | effect (Bubble st) k ->
        let out, st' = continue k () in
        out, S.merge st st'
    in
    join [bubble st; kont st out] ()
end


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


module B = Builder (Size) (TeX)

let delim ~left ~right bdy =
  B.scope (B.join bdy) @@ fun size out ->
  B.join
    [B.write [`Size size; left];
     B.write out;
     B.write [`Size size; right]]

let prn : B.doc list -> B.doc =
  delim ~left:`Lparen ~right:`Rparen

let str x = B.write [`Str x ]

let example : B.doc =
  prn [B.bubble `Big; prn [str "asdf"]]

let foo =
  B.run example
