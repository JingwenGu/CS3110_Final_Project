(* open DT

let () =
  let dataset = [
    (1.0, 1.0); (2.0, 1.5); (1.5, 2.0);  (* bottom-left quadrant → class 0 *)
    (1.0, 9.0); (2.0, 8.5); (1.5, 7.5);  (* top-left quadrant    → class 1 *)
    (8.0, 1.0); (9.0, 1.5); (7.5, 2.0);  (* bottom-right         → class 1 *)
    (8.0, 9.0); (9.0, 8.0); (7.5, 7.5);  (* top-right            → class 0 *)
    (5.0, 5.0); (5.5, 5.5); (4.5, 4.5)   (* center cluster       → class 0 *)
  ] in
  let labels = [
    0; 0; 0;
    1; 1; 1;
    1; 1; 1;
    0; 0; 0;
    0; 0; 0
  ] in
  let tree = DT.DecisionTree2D.build_tree (dataset, labels) 0 3 in
  let test_points = [(2.5, 2.5); (7.0,3.0); (7.5, 9.0); (0.5,9.5); (5.0, 5.0)] in
  List.iter (fun pt ->
    let label = DT.DecisionTree2D.predict tree pt in
    Printf.printf "Point (%f, %f) => Class %d\n" (fst pt) (snd pt) label
  ) test_points;
  let mesh = { DT.Display2D.x_min = 0.0; y_min = 0.0; x_max = 10.0; y_max = 10.0; x_unit = 0.5; y_unit = 0.5 } in
  let _ = DT.Display2D.plot_DT mesh (fun pt ->float_of_int(DT.DecisionTree2D.predict tree pt)) DT.Display2D.dict_demo in ();

  let dataset_arr = List.map (fun (x,y) -> [|x;y|]) dataset in
  let tree_arr = DT.DecisionTree.build_tree (dataset_arr, labels) 0 3 in
  let test_points_arr = List.map (fun (x,y) -> [|x;y|]) test_points in
  List.iter (fun pt ->
    let label = DT.DecisionTree.predict tree_arr pt in
    Printf.printf "Point (%f, %f) => Class %d\n" (pt.(0)) (pt.(1)) label
  ) test_points_arr;
  let convert_point (x,y) = [|x;y|] in
  let _ = DT.Display2D.plot_DT mesh (fun pt ->float_of_int(DT.DecisionTree.predict tree_arr (convert_point pt))) DT.Display2D.dict_demo in ();

  let points_spiral = [(0.0,0.0); (0.09950041652780259,0.009983341664682815); (0.19601331556824833,0.03973386615901225); (0.2866009467376818,0.0886560619984019); (0.3684243976011541,0.15576733692346023); (0.4387912809451864,0.2397127693021015); (0.495201368945807,0.33878548403702136); (0.5353895310991419,0.45095238106638386); (0.5573653674777324,0.5738848727196183); (0.559448971443598,0.7049942186647351); 
  (0.5403023058681398,0.8414709848078965); (0.4989557335681351,0.9803280960675791); (0.43482930537200815,1.1184469031606719); (0.34774847721196356,1.2526256410423509); (0.23795400006033715,1.3796296219838444); (0.10610580250155435,1.4962424799060816); (-0.046719235682062106,1.5993177648664083); (-0.21903564030239228,1.6858301777691966); (-0.40896377044755683,1.7529257355807513); (-0.6142501770406569,1.7979701666060877); 
  (-0.8322936730942848,1.8185948536513634); (-1.060176819659701,1.8127396699626348); (-1.294702457961761,1.7786920884030983); (-1.5324348489435964,1.7151219880064563); (-1.76974491729899,1.6211116333227618); (-2.002859038867334,1.496180360259891); (-2.227910758759263,1.340303566735807); (-2.4409947834460652,1.1539256766313404); (-2.6382225538722435,0.9379668204365331); (-2.815778678933813,0.6938230547205478); 
  (-2.9699774898013365,0.4233600241796016); (-3.0973189658471663,0.12890005354320053); (-3.19454328254321,-0.18679725896825627); (-3.2586832406992543,-0.5205607906727207); (-3.2871138547701673,-0.8688397468912278); (-3.277598405517787,-1.2277412969136694); (-3.228330298802929,-1.5930735958614688); (-3.1379701173285097,-1.9603937213614258); (-3.0056773052747827,-2.3250599855823335); (-2.831135986380546,-2.682288020817499); 
  (-2.6145744834544478,-3.027209981231713); (-2.3567781807864012,-3.3549361553640846); (-2.0590954496309375,-3.6606182441370705); (-1.7234364399438944,-3.939513528022656); (-1.3522646279050452,-4.187049125113871); (-0.9485810974385086,-4.3988855294929365); (-0.5159016239012484,-4.570978616713937); (-0.058226718275585634,-4.699639310551274); (0.41999512050934695,-4.781590122412036); (0.9139106101706213,-4.8140178018592295); 
  (1.4183109273161312,-4.794621373315692); (1.9276864878362037,-4.721654879871434); (2.436286690761961,-4.593964209744796); (2.9381839817495568,-4.411017443786675); (3.4273415300902275,-4.172928232802331); (3.89768375860193,-3.8804717906371557); (4.343168919657401,-3.535093172084997); (4.7578628735832105,-3.138907592806534); (5.136013198259653,-2.6946926405997886); (5.472122741389812,-2.2058723224983927); 
  (5.761021719902196,-1.6764929891935552); (5.997937474499767,-1.1111912760597797); (6.178561001543948,-0.5151542974684776); (6.299109409215516,0.10592757305141035); (6.356383480052433,0.7459149110431609); (6.347819567232152,1.3982799225708025); (6.271535106926294,2.056172999188301); (6.12636709317664,2.71249446813121); (5.9119029343788085,3.3599707877425424); (5.628503190864961,3.9912343742785823); 
  (5.277315780403131,4.598906191031525); (4.860281331743921,5.175680184893725); (4.3801294646322315,5.714408619713904); (3.8403658768820628,6.208187330588526); (3.245250224050487,6.650439909006041); (2.599764883762692,7.034999825810542); (1.9095748036251354,7.356189507439299); (1.1809787376915537,7.608895400852904); (0.4208522803886593,7.78863809392192); (-0.3634167925523438,7.891636600534199); 
  (-1.16400027046891,7.914865972987054); (-1.97270764525991,7.856107467845199); (-2.7810698600674595,7.713990564774135); (-3.5804278132561547,7.488026220177235); (-4.36202469458016,7.178630827941555); (-5.117101172821003,6.7871404572996665); (-5.836992406952108,6.315815041717373); (-6.513225816697381,5.767832301714982); (-7.137618523742574,5.147271297447501); (-7.7023733622459005,4.459085622475172); 
  (-8.200172356962094,3.7090663671758075); (-8.624266579393119,2.903795097379102); (-8.96856131691831,2.05058720972226); (-9.227695526709214,1.1574261386156717); (-9.397114595130942,0.2328889992615607); (-9.473135483865596,-0.7139356443871907); (-9.453003415623618,-1.6735370997406238); (-9.334939334363705,-2.636078076186161); (-9.118177466626582,-3.591495466668901); (-8.802992410991072,-4.529605348375684); 
  (-8.390715290764524,-5.4402111088937); (-7.883738619708744,-6.313213553818126); (-7.285509650677429,-7.138721813454147); (-6.60051210122843,-7.907163840564901); (-5.8342362772431775,-8.6093952784908); (-4.993137743957919,-9.236805479702538); (-4.084584822181375,-9.78141946909576); (-3.116795314418377,-10.235994674091017); (-2.0987629897176134,-10.59411128471811); (-1.0401744759103595,-10.850255161039515); 
  (0.04868267786856134,-10.999892272057739); (1.1570198982092577,-11.03953372906428); (2.273654474770027,-10.966790566494751); (3.3871223790302714,-10.780417523893604); (4.485795876365959,-10.480345192577301); (5.55800472565957,-10.067700008916926); (6.5921597053085135,-9.544811701637009); (7.576877162250367,-8.915207931852676); (8.501103252091898,-8.183596000370041); (9.35423652407837,-7.355831635620695); 
  (10.126247504789907,-6.438875016005216); (10.807793949262814,-5.440734320868656); (11.390330457710585,-4.370397243289288); (11.866211200005301,-3.237751033799349); (12.22878454825791,-2.0534917755590336); (12.472478489732259,-0.8290237168900056); (12.592875777312662,0.42365039498635093); (12.586777872041013,1.6920469260332949); (12.452256840927172,2.963325761299705); (12.1886944922622,4.2244202648762466); 
  (11.796808158852548,5.462171478746337); (11.278662675656202,6.663465183278122); (10.637668240853934,7.815370394135368); (9.8785639969588,8.905277837214816); (9.007387319616468,9.921036925362817); (8.031428954683536,10.851089758446886); (6.9591742973628,11.68460068204836); (5.800231258974089,12.41157996962594); (4.565245315696842,13.023000238328649); (3.265802477703534,13.510904269390167); 
  (1.9143210549096634,13.868502979728184); (0.5239332255506204,14.090262381345642); (-0.8916424655219932,14.171978468572336); (-2.318236441945978,14.11083908912697); (-3.7413699294780907,13.905471982309594); (-5.146401868436229,13.555978305107901); (-6.5186794146191085,13.063951113251347); (-7.84369048286006,12.432478417801116); (-9.107216747729874,11.666130597154677); (-10.295485492714981,10.770932107728438); 
  (-0.0,-0.0); (-0.09950041652780259,-0.009983341664682815); (-0.19601331556824833,-0.03973386615901225); (-0.2866009467376818,-0.0886560619984019); (-0.3684243976011541,-0.15576733692346023); (-0.4387912809451864,-0.2397127693021015); (-0.495201368945807,-0.33878548403702136); (-0.5353895310991419,-0.45095238106638386); (-0.5573653674777324,-0.5738848727196183); (-0.559448971443598,-0.7049942186647351); 
  (-0.5403023058681398,-0.8414709848078965); (-0.4989557335681351,-0.9803280960675791); (-0.43482930537200815,-1.1184469031606719); (-0.34774847721196356,-1.2526256410423509); (-0.23795400006033715,-1.3796296219838444); (-0.10610580250155435,-1.4962424799060816); (0.046719235682062106,-1.5993177648664083); (0.21903564030239228,-1.6858301777691966); (0.40896377044755683,-1.7529257355807513); (0.6142501770406569,-1.7979701666060877); 
  (0.8322936730942848,-1.8185948536513634); (1.060176819659701,-1.8127396699626348); (1.294702457961761,-1.7786920884030983); (1.5324348489435964,-1.7151219880064563); (1.76974491729899,-1.6211116333227618); (2.002859038867334,-1.496180360259891); (2.227910758759263,-1.340303566735807); (2.4409947834460652,-1.1539256766313404); (2.6382225538722435,-0.9379668204365331); (2.815778678933813,-0.6938230547205478); 
  (2.9699774898013365,-0.4233600241796016); (3.0973189658471663,-0.12890005354320053); (3.19454328254321,0.18679725896825627); (3.2586832406992543,0.5205607906727207); (3.2871138547701673,0.8688397468912278); (3.277598405517787,1.2277412969136694); (3.228330298802929,1.5930735958614688); (3.1379701173285097,1.9603937213614258); (3.0056773052747827,2.3250599855823335); (2.831135986380546,2.682288020817499); 
  (2.6145744834544478,3.027209981231713); (2.3567781807864012,3.3549361553640846); (2.0590954496309375,3.6606182441370705); (1.7234364399438944,3.939513528022656); (1.3522646279050452,4.187049125113871); (0.9485810974385086,4.3988855294929365); (0.5159016239012484,4.570978616713937); (0.058226718275585634,4.699639310551274); (-0.41999512050934695,4.781590122412036); (-0.9139106101706213,4.8140178018592295); 
  (-1.4183109273161312,4.794621373315692); (-1.9276864878362037,4.721654879871434); (-2.436286690761961,4.593964209744796); (-2.9381839817495568,4.411017443786675); (-3.4273415300902275,4.172928232802331); (-3.89768375860193,3.8804717906371557); (-4.343168919657401,3.535093172084997); (-4.7578628735832105,3.138907592806534); (-5.136013198259653,2.6946926405997886); (-5.472122741389812,2.2058723224983927); 
  (-5.761021719902196,1.6764929891935552); (-5.997937474499767,1.1111912760597797); (-6.178561001543948,0.5151542974684776); (-6.299109409215516,-0.10592757305141035); (-6.356383480052433,-0.7459149110431609); (-6.347819567232152,-1.3982799225708025); (-6.271535106926294,-2.056172999188301); (-6.12636709317664,-2.71249446813121); (-5.9119029343788085,-3.3599707877425424); (-5.628503190864961,-3.9912343742785823); 
  (-5.277315780403131,-4.598906191031525); (-4.860281331743921,-5.175680184893725); (-4.3801294646322315,-5.714408619713904); (-3.8403658768820628,-6.208187330588526); (-3.245250224050487,-6.650439909006041); (-2.599764883762692,-7.034999825810542); (-1.9095748036251354,-7.356189507439299); (-1.1809787376915537,-7.608895400852904); (-0.4208522803886593,-7.78863809392192); (0.3634167925523438,-7.891636600534199); 
  (1.16400027046891,-7.914865972987054); (1.97270764525991,-7.856107467845199); (2.7810698600674595,-7.713990564774135); (3.5804278132561547,-7.488026220177235); (4.36202469458016,-7.178630827941555); (5.117101172821003,-6.7871404572996665); (5.836992406952108,-6.315815041717373); (6.513225816697381,-5.767832301714982); (7.137618523742574,-5.147271297447501); (7.7023733622459005,-4.459085622475172); 
  (8.200172356962094,-3.7090663671758075); (8.624266579393119,-2.903795097379102); (8.96856131691831,-2.05058720972226); (9.227695526709214,-1.1574261386156717); (9.397114595130942,-0.2328889992615607); (9.473135483865596,0.7139356443871907); (9.453003415623618,1.6735370997406238); (9.334939334363705,2.636078076186161); (9.118177466626582,3.591495466668901); (8.802992410991072,4.529605348375684); 
  (8.390715290764524,5.4402111088937); (7.883738619708744,6.313213553818126); (7.285509650677429,7.138721813454147); (6.60051210122843,7.907163840564901); (5.8342362772431775,8.6093952784908); (4.993137743957919,9.236805479702538); (4.084584822181375,9.78141946909576); (3.116795314418377,10.235994674091017); (2.0987629897176134,10.59411128471811); (1.0401744759103595,10.850255161039515); 
  (-0.04868267786856134,10.999892272057739); (-1.1570198982092577,11.03953372906428); (-2.273654474770027,10.966790566494751); (-3.3871223790302714,10.780417523893604); (-4.485795876365959,10.480345192577301); (-5.55800472565957,10.067700008916926); (-6.5921597053085135,9.544811701637009); (-7.576877162250367,8.915207931852676); (-8.501103252091898,8.183596000370041); (-9.35423652407837,7.355831635620695); 
  (-10.126247504789907,6.438875016005216); (-10.807793949262814,5.440734320868656); (-11.390330457710585,4.370397243289288); (-11.866211200005301,3.237751033799349); (-12.22878454825791,2.0534917755590336); (-12.472478489732259,0.8290237168900056); (-12.592875777312662,-0.42365039498635093); (-12.586777872041013,-1.6920469260332949); (-12.452256840927172,-2.963325761299705); (-12.1886944922622,-4.2244202648762466); 
  (-11.796808158852548,-5.462171478746337); (-11.278662675656202,-6.663465183278122); (-10.637668240853934,-7.815370394135368); (-9.8785639969588,-8.905277837214816); (-9.007387319616468,-9.921036925362817); (-8.031428954683536,-10.851089758446886); (-6.9591742973628,-11.68460068204836); (-5.800231258974089,-12.41157996962594); (-4.565245315696842,-13.023000238328649); (-3.265802477703534,-13.510904269390167); 
  (-1.9143210549096634,-13.868502979728184); (-0.5239332255506204,-14.090262381345642); (0.8916424655219932,-14.171978468572336); (2.318236441945978,-14.11083908912697); (3.7413699294780907,-13.905471982309594); (5.146401868436229,-13.555978305107901); (6.5186794146191085,-13.063951113251347); (7.84369048286006,-12.432478417801116); (9.107216747729874,-11.666130597154677); (10.295485492714981,-10.770932107728438); 
  ] in
  let labels_spiral = [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;] in
  let dataset_spiral = List.map (fun (x,y) -> [|x;y|]) points_spiral in
  let forest = DT.RandomForest.train_forest (dataset_spiral, labels_spiral) (DT.RandomForest.Ratio 0.6) 20 20 in
  List.iter (fun pt ->
    let label = DT.RandomForest.predict_forest forest pt in
    Printf.printf "Point (%f, %f) => Class %d\n" (pt.(0)) (pt.(1)) label
  ) test_points_arr;
  let mesh_spiral = { DT.Display2D.x_min = -15.0; y_min = -15.0; x_max = 15.0; y_max = 15.0; x_unit = 0.5; y_unit = 0.5 } in
  let _ = DT.Display2D.plot_DT mesh_spiral (fun pt ->float_of_int(DT.RandomForest.predict_forest forest (convert_point pt))) DT.Display2D.dict_demo in (); *)


open Graphics
open Printf

(** Window and World Settings *)
let window_width = 800
let window_height = 800

(** For decision tree (2D version and array-based version) *)
let world_x_min = 0.0
let world_y_min = 0.0
let world_x_max = 10.0
let world_y_max = 10.0
let world_width = world_x_max -. world_x_min
let world_height = world_y_max -. world_y_min

let rec wait_for_space_or_q () =
  let status = wait_next_event [Key_pressed] in
  if status.key = ' ' || status.key = 'q' then status.key
  else wait_for_space_or_q ()

let draw_center_instructions text =
  let rect_width = window_width / 2 in
  let rect_height = 100 in
  let rect_x = (window_width - rect_width) / 2 in
  let rect_y = (window_height - rect_height) / 2 in
  set_color white;
  fill_rect rect_x rect_y rect_width rect_height;
  set_color black;
  moveto (rect_x + 10) (rect_y + (rect_height / 2));
  draw_string text

let world_to_screen (x, y) =
  let sx = int_of_float (((x -. world_x_min) /. world_width) *. float_of_int window_width) in
  let sy = int_of_float (((y -. world_y_min) /. world_height) *. float_of_int window_height) in
  (sx, sy)

(** For random forest on spiral data *)
let spiral_x_min = -15.0
let spiral_y_min = -15.0
let spiral_x_max = 15.0
let spiral_y_max = 15.0
let spiral_width = spiral_x_max -. spiral_x_min
let spiral_height = spiral_y_max -. spiral_y_min

let world_to_screen_spiral (x, y) =
  let sx = int_of_float (((x -. spiral_x_min) /. spiral_width) *. float_of_int window_width) in
  let sy = int_of_float (((y -. spiral_y_min) /. spiral_height) *. float_of_int window_height) in
  (sx, sy)

(**Color Mapping*)
let color_of_label label =
  match label with
  | 0 -> red
  | 1 -> blue
  | _ -> green

(** Drawing Functions*)

(** Draw a decision boundary on the current Graphics window.
   f is a function mapping world coordinates to a label. *)
let draw_decision_boundary world_min_x world_min_y world_width world_height f =
  let step = 5 in
  for i = 0 to (window_width - 1) / step do
    for j = 0 to (window_height - 1) / step do
      let x = i * step in
      let y = j * step in
      let wx = (float_of_int x) /. float_of_int window_width *. world_width +. world_min_x in
      let wy = (float_of_int y) /. float_of_int window_height *. world_height +. world_min_y in
      let label = f (wx, wy) in
      set_color (color_of_label label);
      fill_rect x y step step
    done
  done

(** Draw a marker at a given world coordinate, along with the predicted label *)
let draw_marker (x, y) label to_screen =
let (sx, sy) = to_screen (x, y) in
set_color black;
fill_circle sx sy 5;
set_color white;  
moveto (sx + 10) (sy + 10);
draw_string (sprintf "Label: %d" label)

(** Main Program *)
let () =
  Random.self_init ();

  (** Open Graphics window *)
  open_graph (sprintf " %dx%d" window_width window_height);

  (** Decision Tree (2D version) *)
  set_window_title "Decision Tree2D - Graphics Interface";

  let dataset = [
    (1.0, 1.0); (2.0, 1.5); (1.5, 2.0);   (* bottom-left quadrant → class 0 *)
    (1.0, 9.0); (2.0, 8.5); (1.5, 7.5);   (* top-left quadrant    → class 1 *)
    (8.0, 1.0); (9.0, 1.5); (7.5, 2.0);   (* bottom-right         → class 1 *)
    (8.0, 9.0); (9.0, 8.0); (7.5, 7.5);   (* top-right            → class 0 *)
    (5.0, 5.0); (5.5, 5.5); (4.5, 4.5)    (* center cluster       → class 0 *)
  ] in
  let labels = [0; 0; 0; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0] in

  let tree = DT.DecisionTree2D.build_tree (dataset, labels) 0 3 in
  let test_points = [(2.5, 2.5); (7.0, 3.0); (7.5, 9.0); (0.5, 9.5); (5.0, 5.0)] in

  (** Draw decision boundary using DecisionTree2D *)
  draw_decision_boundary world_x_min world_y_min world_width world_height
    (fun (x, y) -> DT.DecisionTree2D.predict tree (x, y));
  (** Mark test points *)
  List.iter (fun pt ->
    let label = DT.DecisionTree2D.predict tree pt in
    draw_marker pt label world_to_screen;
    printf "DecisionTree2D: Point (%f, %f) => Class %d\n" (fst pt) (snd pt) label
  ) test_points;

  draw_center_instructions "Decision Tree2D: Press SPACE for next screen or Q to quit.";
  let key = wait_for_space_or_q () in
  if key = 'q' then (close_graph (); exit 0);
  set_color white;
  fill_rect ((window_width - (window_width / 2)) / 2) ((window_height - 100) / 2) (window_width / 2) 100;

  (** Decision Tree (Array version) *)
  clear_graph ();
  set_window_title "Decision Tree (Array) - Graphics Interface";

  let dataset_arr = List.map (fun (x, y) -> [| x; y |]) dataset in
  let tree_arr = DT.DecisionTree.build_tree (dataset_arr, labels) 0 3 in

  draw_decision_boundary world_x_min world_y_min world_width world_height
    (fun (x, y) -> DT.DecisionTree.predict tree_arr [| x; y |]);
  List.iter (fun (x, y) ->
    let label = DT.DecisionTree.predict tree_arr [| x; y |] in
    draw_marker (x, y) label world_to_screen;
    printf "DecisionTree (Array): Point (%f, %f) => Class %d\n" x y label
  ) test_points;
  draw_center_instructions "Decision Tree (Array): Press SPACE for next screen or Q to quit.";
  let key = wait_for_space_or_q () in
  if key = 'q' then (close_graph (); exit 0);
  set_color white;
  fill_rect ((window_width - (window_width / 2)) / 2) ((window_height - 100) / 2) (window_width / 2) 100;

   (** AdaBoost Visualization *)
  clear_graph ();
  set_window_title "AdaBoost - Graphics Interface";
  
  (* Convert dataset to array format for AdaBoost *)
  let adaboost_dataset_arr = List.map (fun (x, y) -> [| x; y |]) dataset in
  
  (** Convert labels to AdaBoost format (-1/+1) *)
  let adaboost_labels = DT.AdaBoost.adaboost_convert_labels labels in
  
  (**Train AdaBoost with 30 weak learners *)
  let adaboost_rounds = 30 in
  let adaboost_model = DT.AdaBoost.adaboost_train_ada (adaboost_dataset_arr, adaboost_labels) adaboost_rounds in
  
  printf "AdaBoost model trained with %d weak learners\n" adaboost_rounds;
  
  (** Function to predict using AdaBoost and convert back to 0/1 labels *)
  let predict_adapter (x, y) =
    let pred = DT.AdaBoost.adaboost_predict_ada adaboost_model [| x; y |] in
    DT.AdaBoost.adaboost_convert_prediction pred
  in
  
  (** Draw decision boundary using AdaBoost *)
  draw_decision_boundary world_x_min world_y_min world_width world_height predict_adapter;
  
  (** Draw test points with AdaBoost predictions *)
  List.iter (fun (x, y) ->
    let label = predict_adapter (x, y) in
    draw_marker (x, y) label world_to_screen;
    printf "AdaBoost: Point (%f, %f) => Class %d\n" x y label
  ) test_points;
  
  draw_center_instructions "AdaBoost: Press SPACE for next screen or Q to quit.";
  let key = wait_for_space_or_q () in
  if key = 'q' then (close_graph (); exit 0);
  set_color white;
  fill_rect ((window_width - (window_width / 2)) / 2) ((window_height - 100) / 2) (window_width / 2) 100;

  (** Random Forest on Spiral Data *)
  clear_graph ();
  set_window_title "Random Forest on Spiral Data - Graphics Interface";

  let generate_two_spirals n =
    let theta_max = 10.0 *. Float.pi in
    let step = theta_max /. float_of_int n in
    let spiral_a = List.init n (fun i ->
      let theta = step *. float_of_int i in
      let r = 0.5 *. theta in  
      (r *. cos theta, r *. sin theta), 0
    ) in
    let spiral_b = List.init n (fun i ->
      let theta = step *. float_of_int i +. Float.pi in
      let r = 0.5 *. theta in
      (r *. cos theta, r *. sin theta), 1
    ) in
    spiral_a @ spiral_b in
  
  let points_labels = generate_two_spirals 100 in
  let points_spiral, labels_spiral = List.split points_labels in
  let dataset_spiral = List.map (fun (x, y) -> [| x; y |]) points_spiral in
  let forest = DT.RandomForest.train_forest (dataset_spiral, labels_spiral)
                  (DT.RandomForest.Ratio 0.6) 50 30 in

  draw_decision_boundary spiral_x_min spiral_y_min spiral_width spiral_height
    (fun (x, y) -> DT.RandomForest.predict_forest forest [| x; y |]);
  List.iter (fun pt ->
    let point_arr = [| fst pt; snd pt |] in
    let label = DT.RandomForest.predict_forest forest point_arr in
    draw_marker pt label world_to_screen_spiral;
    printf "RandomForest: Point (%f, %f) => Class %d\n" (fst pt) (snd pt) label
  ) test_points;

  (** AdaBoost on Spiral Data *)
  draw_center_instructions "Random Forest on Spiral Data: Press SPACE for AdaBoost on Spiral or Q to quit.";
  let key = wait_for_space_or_q () in
  if key = 'q' then (close_graph (); exit 0);
  set_color white;
  fill_rect ((window_width - (window_width / 2)) / 2) ((window_height - 100) / 2) (window_width / 2) 100;
  
  clear_graph ();
  set_window_title "AdaBoost on Spiral Data - Graphics Interface";

  (** Convert spiral labels to AdaBoost format *)
  let adaboost_spiral_labels = DT.AdaBoost.adaboost_convert_labels labels_spiral in
  
  (** Train AdaBoost with more weak learners for complex spiral data *)
  let adaboost_spiral_rounds = 30 in
  let adaboost_spiral_model = DT.AdaBoost.adaboost_train_ada (dataset_spiral, adaboost_spiral_labels) adaboost_spiral_rounds in
  
  (** Function to predict using AdaBoost on spiral data *)
  let predict_spiral_adapter (x, y) =
    let pred = DT.AdaBoost.adaboost_predict_ada adaboost_spiral_model [| x; y |] in
    DT.AdaBoost.adaboost_convert_prediction pred
  in
  
  (** Draw decision boundary using AdaBoost on spiral data *)
  draw_decision_boundary spiral_x_min spiral_y_min spiral_width spiral_height predict_spiral_adapter;
  
  (** Draw test points with AdaBoost predictions *)
  List.iter (fun pt ->
    let x, y = pt in
    let label = predict_spiral_adapter (x, y) in
    draw_marker pt label world_to_screen_spiral;
    printf "AdaBoost on Spiral: Point (%f, %f) => Class %d\n" x y label
  ) test_points;
  
  draw_center_instructions "AdaBoost on Spiral Data: Press SPACE to quit.";
  let key = wait_for_space_or_q () in
  if key = 'q' || key = ' ' then (close_graph (); exit 0);
  close_graph ();
  ()