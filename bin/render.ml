let render comments request =
  let comments = List.map (fun (_, y) -> y) comments in
  let comments = "<p>" ^ String.concat "<br>" comments ^ "</p>" in
  let form = "<form method=\"POST\" action=\"/\">\n" in
  let form = form ^ Dream.csrf_tag request ^ "\n" in
  let form = form ^ "<input name=\"text\" autofocus>\n" in
  let form = form ^ "</form>" in
  comments ^ form
;;
(* comments |> List.iter (fun (_id, comment) ->
    <p><%s comment %></p>); %>

    <form method="POST" action="/">
      <%s! Dream.csrf_tag request %>
      <input name="text" autofocus>
    </form>

  </body>
  </html> *)
