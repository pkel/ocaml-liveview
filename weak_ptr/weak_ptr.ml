(* This is an experiment for finding out if I can send weak function pointers
   to the client, to resolve and apply them later on the server. The pointers
   have to be serializable for that to work. *)

module Effect : sig
 (* this is the thing I want to refer to from the client *)
  type t
  val create : (unit -> unit) -> t
  val execute : t -> unit
end = struct
  type t = unit -> unit
  let create t = t
  let execute t = t ()
end

type effect = Effect.t

module Registry : sig
  type 'a t
  type id

  val create : unit -> 'a t
  val register : 'a t -> 'a -> id
  val find_opt : 'a t -> id -> 'a option
  val string_of_id : id -> string
  val id_of_string : string -> id option

  (*
    goals:
    - id's are unique within the given registry
    - registering a value does not block garbage collection of this value
    - elements are cleared from the registry on garbage collection

    missing:
    - I think the same element can be registered multiple times. This might
      lead to many redundant entries in the registry and updates sent to the
      client (in the liveview model). When I use this for Bonesai/liveview I
      have to ensure that register is only called for distinct values.
      Bonesai/FRP has tooling for that available already.
   *)
end = struct
  type id = int
  type 'a el = 'a Weak.t
  type 'a t =
    { tab: (int, 'a el) Hashtbl.t
    ; mutable last: int
    }

  let create () =
    { tab = Hashtbl.create 7
    ; last = -1
    }

  let register t el =
    let id = t.last + 1 in
    let box = Weak.create 1 in
    let () =
      Weak.set box 0 (Some el);
      t.last <- id;
      Hashtbl.replace t.tab id box
    in
    id

  let find_opt t id =
    match Hashtbl.find_opt t.tab id with
    | Some box -> Weak.get box 0
    | None -> None

  let string_of_id = Printf.sprintf "0x%x"
  let id_of_string s = Scanf.sscanf_opt s "%x" (fun x -> x)
end
