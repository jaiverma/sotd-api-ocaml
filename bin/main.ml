open Lwt.Infix

module type DB = Caqti_lwt.CONNECTION

module R = Caqti_request
module T = Caqti_type

let list_comments =
  let query = R.collect T.unit T.(tup2 int string) "SELECT id, text FROM comment" in
  fun (module Db : DB) -> Db.collect_list query () >>= Caqti_lwt.or_fail
;;

let add_comment =
  let query = R.exec T.string "INSERT INTO comment (text) VALUES ($1)" in
  fun text (module Db : DB) -> Db.exec query text >>= Caqti_lwt.or_fail
;;

let () =
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.sql_pool "postgresql://dream:dream@localhost/dream"
  @@ Dream.sql_sessions
  @@ Dream.router
       [ Dream.get "/" (fun request ->
             Dream.sql request list_comments
             >>= fun comments -> Dream.html (Render.render comments request))
       ; Dream.post "/" (fun request ->
             Dream.form request
             >>= function
             | `Ok [ ("text", text) ] ->
               Dream.sql request (add_comment text)
               >>= fun () -> Dream.redirect request "/"
             | _ -> Dream.empty `Bad_Request)
       ]
;;
