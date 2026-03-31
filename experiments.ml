open CS51Utils
open Tiles
open Mazes
open Puzzledescription
open Puzzlesolve

(* Time keeper: *)
let time_call f x =
  let start = Unix.gettimeofday () in
  let result = f x in
  let finish = Unix.gettimeofday () in
  (result, finish -. start)

(* Example puzzle setups *)
let cDIMS = 3, 3
let solved : board =
  [| [|Tile 1; Tile 2; Tile 3|];
     [|Tile 4; Tile 5; Tile 6|];
     [|Tile 7; Tile 8; EmptyTile|] |]

let random_tileboard () : board =
  let cINITIAL_MOVE_COUNT = 45 in
  let module Puzzle = MakeTilePuzzleDescription (struct let initial = solved let dims = cDIMS end) in
  let rec make_moves n b =
    if n <= 0 then b else make_moves (n - 1) (fst (List.nth (Puzzle.neighbors b) (Random.int (List.length (Puzzle.neighbors b)))))
  in
  make_moves cINITIAL_MOVE_COUNT Puzzle.initial_state

(* Maze setups *)
let init_maze =
  [|
    [| EmptySpace; EmptySpace; Wall; EmptySpace; EmptySpace|];
    [| Wall; EmptySpace; EmptySpace; EmptySpace; EmptySpace|];
    [| Wall; Wall; EmptySpace; Wall; EmptySpace|];
    [| EmptySpace; EmptySpace; EmptySpace; Wall; EmptySpace|];
    [| EmptySpace; Wall; EmptySpace; EmptySpace; EmptySpace|];
   |]

module TestMazeI : MAZEINFO = struct
  let maze = init_maze
  let initial_pos = (0, 0)
  let goal_pos = (4, 4)
  let dims = (5, 5)
end

(* Experimentation on tile: *)
let experiment_tile () =
  let module Puzzle = MakeTilePuzzleDescription (struct let initial = random_tileboard () let dims = cDIMS end) in
  let module DFSG = DFSSolver(Puzzle) in
  let module BFSG = BFSSolver(Puzzle) in
  let module FastBFSG = FastBFSSolver(Puzzle) in
  Printf.printf "\nTile Puzzle Experiment\n";
  let (_, dfs_time) = time_call DFSG.solve () in
  Printf.printf "DFS time: %fs\n" dfs_time;
  let (_, bfs_time) = time_call BFSG.solve () in
  Printf.printf "BFS time: %fs\n" bfs_time;
  let (_, fastbfs_time) = time_call FastBFSG.solve () in
  Printf.printf "Fast BFS (two-stack queue) time: %fs\n" fastbfs_time

(* Experimentation on maze: *)
let experiment_maze () =
  let module MPuzzle = MakeMazePuzzleDescription(TestMazeI) in
  let module DFSG = DFSSolver(MPuzzle) in
  let module BFSG = BFSSolver(MPuzzle) in
  let module FastBFSG = FastBFSSolver(MPuzzle) in
  Printf.printf "\nMaze Puzzle Experiment\n";
  let (_, dfs_time) = time_call DFSG.solve () in
  Printf.printf "DFS time: %fs\n" dfs_time;
  let (_, bfs_time) = time_call BFSG.solve () in
  Printf.printf "BFS time: %fs\n" bfs_time;
  let (_, fastbfs_time) = time_call FastBFSG.solve () in
  Printf.printf "Fast BFS (two-stack queue) time: %fs\n" fastbfs_time

let () =
  Random.init 0;
  experiment_tile ();
  experiment_maze ()
