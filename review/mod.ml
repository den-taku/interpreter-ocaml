module type BST_WITHOUT_MIN =
  sig
    type t = Lf | Br of {left: t; value: int; right: t}
    val find: t -> int -> bool
    val insert: t -> int -> t
    val delete: t -> int -> t
  end
;;

module Bst : BST_WITHOUT_MIN =
  struct
    type t =
       Lf
      |Br of {
          left: t;
          value: int;
          right: t;
        }

    let rec find t n =
      match t with
       Lf -> false
      |Br {left=l;value=v;right=r} ->
        if n = v then true
        else if n < v then find l n 
        else (* n > v *) find r n

    let rec insert t n =
      match t with
       Lf -> Br {left=Lf;value=n;right=Lf}
      |Br {left=l;value=v;right=r} ->
        if n = v then t
        else if n < v then Br {left=insert l n;value=v;right=r}
        else (* n > v *) Br {left=l; value=v;right=insert r n}

    let rec min t =
      match t with
       Lf -> min_int
      |Br {left=Lf;value=v} -> v
      |Br {left=l} -> min l

    let rec delete t n =
      match t with
       Lf -> t
      |Br {left=l;value=v;right=r} ->
        if n = v then 
          match l,r with
           Lf, Lf -> Lf
          |Br _, Lf -> l
          |Lf, Br _ -> r
          |Br _, Br _ -> 
            let m = min r in
            Br {left=l;value=m;right=delete r m}
        else if n < v then Br {left=delete l n;value=v;right=r}
        else (* n > v *) Br {left=l;value=v;right=delete r n}
  end
;;

let t1 = let open Bst in Lf;;
let s1 = let open Bst in find t1 0;;

module type ABSTRACT_BST_VER1 =
  sig
    type t
    val find: t -> int -> bool
    val insert: t -> int -> t
    val delete: t -> int -> t
  end
;;

module type ABSTRACT_BST_VER2 =
  sig
    type t
    val empty: t
    val find: t -> int -> bool
    val insert: t -> int -> t
    val delete: t -> int -> t
  end
;;

module Bst : ABSTRACT_BST_VER2 =
  struct
    type t =
       Lf
      |Br of {
          left: t;
          value: int;
          right: t;
        }

    let empty = Lf

    let rec find t n =
      match t with
       Lf -> false
      |Br {left=l;value=v;right=r} ->
        if n = v then true
        else if n < v then find l n 
        else (* n > v *) find r n

    let rec insert t n =
      match t with
       Lf -> Br {left=Lf;value=n;right=Lf}
      |Br {left=l;value=v;right=r} ->
        if n = v then t
        else if n < v then Br {left=insert l n;value=v;right=r}
        else (* n > v *) Br {left=l; value=v;right=insert r n}

    let rec min t =
      match t with
       Lf -> min_int
      |Br {left=Lf;value=v} -> v
      |Br {left=l} -> min l

    let rec delete t n =
      match t with
       Lf -> t
      |Br {left=l;value=v;right=r} ->
        if n = v then 
          match l,r with
           Lf, Lf -> Lf
          |Br _, Lf -> l
          |Lf, Br _ -> r
          |Br _, Br _ -> 
            let m = min r in
            Br {left=l;value=m;right=delete r m}
        else if n < v then Br {left=delete l n;value=v;right=r}
        else (* n > v *) Br {left=l;value=v;right=delete r n}
  end
;;

let t1 = Bst.insert Bst.empty 10;;
let t2 = Bst.delete t1 10;;
t2;;
