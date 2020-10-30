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

module Make (S : Merge) (N : Monoid) =
struct
  effect Bubble : S.t -> unit
  effect Write : N.t -> unit

  type doc = unit -> unit

  let bubble st () =
    perform @@ Bubble st

  let write x () =
    perform @@ Write x

  let empty () = ()

  let rec join docs =
    match docs with
    | [] -> empty
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
    let out, st = run doc in
    join [bubble st; kont st out] ()
end

