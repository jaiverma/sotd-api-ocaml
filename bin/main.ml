open Lwt.Infix

let successful = ref 0
let failed = ref 0

let count_requests inner_handler request =
  Lwt.catch
    (fun () ->
      inner_handler request
      >>= fun response ->
      successful := !successful + 1;
      Lwt.return response)
    (fun exn ->
      failed := !failed + 1;
      raise exn)
;;

let () =
  Dream.run
  @@ Dream.logger
  @@ count_requests
  @@ Dream.router
       [ Dream.get "/fail" (fun _ -> raise (Failure "The web app failed"))
       ; Dream.get "/" (fun _ ->
             Dream.html
               (Printf.sprintf
                  "%3i requests successful<br>%3i requests failed"
                  !successful
                  !failed))
       ]
;;
