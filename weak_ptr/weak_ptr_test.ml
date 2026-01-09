open Weak_ptr

let t : effect Registry.t = Registry.create ()
let eff i = Effect.create (fun () -> Printf.eprintf "effect %i\n" i)
let arr = Array.init 5 eff
let ptr = Array.map (fun el -> Registry.register t el) arr

let () =
  Array.iter
    (fun p ->
      match Registry.find_opt t p with
      | None ->
          print_endline "fail pre GC";
          exit 1
      | Some _ -> ())
    ptr

let () = Gc.full_major ()

let () =
  Array.iter
    (fun p ->
      match Registry.find_opt t p with
      | None ->
          print_endline "fail post GC";
          exit 2
      | Some _ -> ())
    ptr

(* up to now; everything is and should be alive *)

(* now: kill one of the effects *)

let () = Array.set arr 3 (eff 42)

let () =
  match Registry.find_opt t (Array.get ptr 3) with
  | Some _ -> ()
  | None ->
      print_endline "effect 3 should still be around";
      exit 3

let () = Gc.full_major ()

let () =
  match Registry.find_opt t (Array.get ptr 3) with
  | Some _ ->
      print_endline "effect 3 should be collected";
      exit 4
  | None -> ()
