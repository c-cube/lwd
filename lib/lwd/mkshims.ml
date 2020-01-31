
module C = Configurator.V1

let write_file f s =
  let out = open_out f in
  output_string out s; flush out; close_out out

let let_op_common = "
  module type ARG = sig
    type 'a t
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    val monoid_product : 'a t -> 'b t -> ('a * 'b) t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  end
  "

let shims_let_op_pre_408 =
  let_op_common ^
  "
   module type S = sig type 'a t_let end
   module Make(X:ARG) = struct type 'a t_let = 'a X.t end
"
let shims_let_op_post_408 =
  let_op_common ^
  "
    module type S = sig
      type 'a t_let
      val (let+) : 'a t_let -> ('a -> 'b) -> 'b t_let
      val (and+) : 'a t_let -> 'b t_let -> ('a * 'b) t_let
      val (let*) : 'a t_let -> ('a -> 'b t_let) -> 'b t_let
      val (and*) : 'a t_let -> 'b t_let -> ('a * 'b) t_let
    end
   module Make(X:ARG) : S with type 'a t_let = 'a X.t = struct
      type 'a t_let = 'a X.t
      let (let+) = X.(>|=)
      let (and+) = X.monoid_product
      let (let*) = X.(>>=)
      let (and*) = X.monoid_product
  end[@@inline]
"

let () =
  C.main ~name:"mkshims" (fun c ->
    let version = C.ocaml_config_var_exn c "version" in
    let major, minor = Scanf.sscanf version "%u.%u" (fun maj min -> maj, min) in
    write_file "LwdShimsMkLet_.ml" (if (major, minor) >= (4,8) then shims_let_op_post_408 else shims_let_op_pre_408);
  )
