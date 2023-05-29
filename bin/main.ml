open Webdriver_cohttp_lwt_unix
open Infix

let rec list_iter f = function
  | [] -> return ()
  | x :: xs ->
      let* () = f x in
      list_iter f xs

let rec wait cmd =
  Error.catch (fun () -> cmd)
    ~errors:[`no_such_element]
    (fun _ -> sleep 100 >>= fun () -> wait cmd)
let action =
  let* () = goto "https://www.scrapethissite.com/pages/ajax-javascript" in
  let* link =
    find_first
      `css
      "a.year-link"
  in
  
  let* () = click link in
  let* _ = wait (find_first `css "tr.film") in
  let* movies = 
    find_all
      `css
      "tr.film .film-title" in
  list_iter (fun movie_name -> 
      let* name = text movie_name in
      Printf.printf "%s\n" name;
      return ()
    ) movies

let host = "http://127.0.0.1:4444"
let () =
  try Lwt_main.run (run ~host Capabilities.firefox_headless action)
  with Webdriver e ->
    Printf.fprintf stderr "[FAIL] Webdriver error: %s\n%!" (Error.to_string e) ;
    Printexc.print_backtrace stderr ;
    Printf.fprintf stderr "\n%!"