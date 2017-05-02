# OCutTheRope
OCaml implementation of the Cut The Rope game  

## Compilation instructions
Compile with one of these options  

- `make ; ./main.byte`  
  to compile and run main bytecode version
- `make ; ./main.native`  
  to compile and run main native version
- `make test ; ./test.byte`  
  to compile and run tests
- `make expe ; ./expe.byte`  
  to compile and run your experiments (warning: "expe.ml" is excluded by gitignore)  

_Note that you must run `sprites/convert.sh` to generate the PPM files from PNG sprites (need ImageMagick)_  

## To Do list
Things to do (including ideas)  
- [X] Set up projet/configuration
- [X] Sprites converting/loading (`sprites/convert.sh` `ppm.ml`)
- [ ] Main file (`main.ml`)
- [ ] Physical engine (`physics.ml`)
- [ ] Rendering (`render.ml`)
- [ ] Management of game mechanics (`gamemechanics.ml`)
- [ ] Levels management (loading/saving/data structure) (`level.ml`)
- [ ] Unit tests (`test.ml`)
- [ ] WYSIWYG level editor and online level sharing (_may need a new `Makefile` target_) (`editor.ml`)
