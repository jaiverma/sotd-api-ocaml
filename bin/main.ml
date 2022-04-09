open Lwt.Infix

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router
       [ Dream.post "/echo" (fun request ->
             Dream.body request
             >>= Dream.respond ~headers:[ "Content-Type", "application/octet-stream" ])
       ]
;;
