(*
   Rewrite pattern for more efficient matching.

   Right now, this just collapses consecutive '...' into one.
*)

open Pattern_AST

let safe_list_map f l = List.rev_map f l |> List.rev

let rec collapse_ellipses acc pat =
  match pat with
  | (Dots { loc = loc1; name = name1; repeats = n1 } as head)
    :: (Dots { loc = loc2; name = name2; repeats = n2 } :: tail2 as tail1) -> (
      assert (n1 > 0);
      assert (n2 > 0);
      match (name1, name2) with
      | Some _, Some _ ->
          (* Can't collapse these ellipses due to having different names. *)
          head :: collapse_ellipses (head :: acc) tail1
      | _ ->
          (* If one of the ellipses has a name, the ellipsis that results
             from the collapse get this name. The maximum line span
             extended by adding up the 'repeat' fields. *)
          let loc = Loc.range loc1 loc2 in
          let name =
            match (name1, name2) with
            | None, None -> None
            | Some name, None | None, Some name -> Some name
            | Some _, Some _ -> assert false
          in
          let repeats = n1 + n2 in
          collapse_ellipses acc (Dots { loc; name; repeats } :: tail2) )
  | head :: tail -> collapse_ellipses (head :: acc) tail
  | [] -> List.rev acc |> collapse_children

and collapse_children pat =
  safe_list_map
    (function List pat -> List (collapse_ellipses [] pat) | x -> x)
    pat

let simplify pat = collapse_ellipses [] pat
