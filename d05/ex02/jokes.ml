let () =
  let joke_arr: string array = [|
    "C'est un viking, il rencontre un ami gaulois dans un port. et le gaulois il lui fait : 'he dis donc toi avec ton casque a corne, presente moi ta femme tu sauras pourquoi tu as des cornes !' #Kaamelott";
    "Pourquoi un chasseur emmène t-il son fusil au au toilette ? : Pour tirer la chasse";
    "Tu savais que la « ouate » vient des phoques ? : Ouate de Phoques !";
    "C’est l’histoire d’un poil avant il était bien maintenant il est pubien";
    "Pourquoi les mamies sont-elles très efficaces pour faire le ménage ? : Car elles ont 80 balais.";
    "Comment appelle-t-on un nain en prison ? Un incarcéré !";
    "Où les poules vivent-elles en hiver ? A Liverpool."
  |] in
  Random.self_init ();
  print_string(Array.get joke_arr (Random.int (Array.length joke_arr)));
  print_endline("")