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


module Make (S : Merge) (N : Monoid) :
sig
  type doc

  val bubble : S.t -> doc
  val write : N.t -> doc
  val empty : doc
  val join : doc list -> doc
  val scope : doc -> (S.t -> N.t -> doc) -> doc

  val run : doc -> N.t * S.t
end
