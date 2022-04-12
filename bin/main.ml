open Lwt.Infix

module Bike = struct
  type t =
    { frameno : string
    ; owner : string
    ; stolen : Ptime.t option
    }
end

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let bike =
    let open Bike in
    let encode { frameno; owner; stolen } = Ok (frameno, owner, stolen) in
    let decode (frameno, owner, stolen) = Ok { frameno; owner; stolen } in
    let rep = Caqti_type.(tup3 string string (option ptime)) in
    custom ~encode ~decode rep
  ;;

  let create_bikereg =
    (unit ->. unit)
    @@ {eos|
    CREATE TABLE bikereg (
      frameno text NOT NULL,
      owner text NOT NULL,
      stolen timestamp NULL
    )
    |eos}
  ;;

  let reg_bike =
    (tup2 string string ->. unit) @@ "INSERT INTO bikereg (frameno, owner) VALUES (?, ?)"
  ;;

  let report_stolen =
    (string ->. unit) @@ "UPDATE bikereg SET stolen = current_timestamp WHERE frameno = ?"
  ;;

  let select_stolen = (unit ->* bike) @@ "SELECT * FROM bikereg WHERE NOT stolen IS NULL"
  let select_owner = (string ->? string) @@ "SELECT owner FROM bikereg WHERE frameno = ?"
end

let create_bikereg (module Db : Caqti_lwt.CONNECTION) = Db.exec Q.create_bikereg ()

let reg_bike (module Db : Caqti_lwt.CONNECTION) frameno owner =
  Db.exec Q.reg_bike (frameno, owner)
;;

let report_stolen (module Db : Caqti_lwt.CONNECTION) frameno =
  Db.exec Q.report_stolen frameno
;;

let find_bike_owner frameno (module Db : Caqti_lwt.CONNECTION) =
  Db.find_opt Q.select_owner frameno
;;

let iter_s_stolen (module Db : Caqti_lwt.CONNECTION) f = Db.iter_s Q.select_stolen f ()

let ( >>=? ) m f =
  m
  >>= function
  | Ok x -> f x
  | Error err -> Lwt.return (Error err)
;;

let test db =
  create_bikereg db
  >>=? fun () ->
  reg_bike db "BIKE-0000" "Arthur Dent"
  >>=? fun () ->
  reg_bike db "BIKE-0001" "Ford Prefect"
  >>=? fun () ->
  reg_bike db "BIKE-0002" "Zaphod Beeblebrox"
  >>=? fun () ->
  reg_bike db "BIKE-0003" "Trillian"
  >>=? fun () ->
  reg_bike db "BIKE-0004" "Marvin"
  >>=? fun () ->
  report_stolen db "BIKE-0000"
  >>=? fun () ->
  report_stolen db "BIKE-0004"
  >>=? fun () ->
  let show_owner frameno =
    find_bike_owner frameno db
    >>=? fun owner_opt ->
    (match owner_opt with
    | Some owner -> Lwt_io.printf "%s is owned by %s\n" frameno owner
    | None -> Lwt_io.printf "%s is not registered\n" frameno)
    >>= Lwt.return_ok
  in
  show_owner "BIKE-0003"
  >>=? fun () ->
  show_owner "BIKE-0042"
  >>=? fun () ->
  Lwt_io.printf "Stolen:"
  >>= fun () ->
  iter_s_stolen db (fun bike ->
      let stolen =
        match bike.stolen with
        | Some x -> x
        | None -> assert false
      in
      Lwt_io.printf "\t%s %s %s\n" bike.frameno (Ptime.to_rfc3339 stolen) bike.owner
      >>= Lwt.return_ok)
;;

let report_error = function
  | Ok () -> Lwt.return_unit
  | Error err -> Lwt_io.eprintl (Caqti_error.show err) >|= fun () -> exit 69
;;

let () =
  Lwt_main.run
    (Caqti_lwt.with_connection (Uri.of_string "postgresql://dream:dream@localhost") test
    >>= report_error)
;;
