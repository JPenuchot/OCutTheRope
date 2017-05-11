# OCutTheRope
OCaml implementation of the Cut The Rope game

## Requirements

`ocamlbuild` and `ocamlfind` are both required to build this project. You can install them using OPAM.  
The level editor needs `python` to upload levels (for the input box)

Dependencies :

-	OCaml Graphics (standard library)
-	OCaml Str (standard library)
-	[Cohttp](https://github.com/mirage/ocaml-cohttp), install with `opam install cohttp lwt js_of_ocaml`

## Build

Compile with one of these options

- `make debug ; ./game.byte`  
  to compile and run main bytecode version
- `make ; ./game.native`  
  to compile and run main native version
- `make test ; ./test.byte`  
  to compile and run tests
- `make expe ; ./expe.byte`  
  to compile and run your experiments (warning: "expe.ml" is excluded by gitignore)  

_Note that you must run `sprites/convert.sh` to generate the PPM files from PNG sprites (need ImageMagick)_

## Execution
Run the game with the command `./game.native [level.lvl] [-s theme]` You can specify a level file (optional) and a theme after `-s`. The defaults themes are `basic` `mario` and `ctr`. Respect the order.

## To Do list

Things to do (including ideas)

- [X] Set up projet/configuration
- [X] Sprites converting/loading (`sprites/{THEME}/convert.sh` `lib/ppm.ml`)
- [X] Main file (`game.ml`)
- [X] Physical engine
- [X] Rendering (`lib/render.ml`)
- [X] Management of game mechanics (`lib/gamemechanics.ml`)
- [X] Levels management (loading/saving/data structure) (`level.ml`)
- [ ] Unit tests (`test.ml`) -- FUCK TESTS, WE'RE USING OCAML NOT JAVA FFS
- [X] WYSIWYG level editor and online level sharing (`editor.ml`)
- [X] Level downloader (`download.ml`)
- [ ] Remove secret NASA hacking code from our code

---

```
(•_•)
( •_•)>⌐■-■
(⌐■_■)
```