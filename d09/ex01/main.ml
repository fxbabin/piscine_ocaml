let () =
  let p1:App.App.project = ("p1", "", 20) in
  let p2:App.App.project = ("p2", "", 20) in
  App.print_proj p1;
  App.print_proj p2;
  App.print_proj (App.App.combine p1 p2);
  let p3:App.App.project = ("p1", "", 90) in
  let p4:App.App.project = ("p2", "", 75) in
  App.print_proj p3;
  App.print_proj p4;
  App.print_proj (App.App.combine p3 p4);
  App.print_proj (App.App.fail p3);
  App.print_proj (App.App.success p4);
  ()



  