# Splitting is Stressful but Merging is Manageable

This artifact supports the claims of the companion paper [Splitting is Stressful but Merging is Manageable: Co-contextual Typing for Substructural and Session Types]() by O. Weston and S. Fowler.

## Overview

The artifact, codenamed **ntextual**, provides a co-contextual programming language typechecker representing both theoretical contributions of the companion paper, i.e., generalised co-contextual typechecker for substructural systems, and co-contextual typechecker for session types in the style of GV.

## Getting started

You will need:

1. an internet connection,
2. Docker engine installed (untested on Podman).

### Preparing the environment

First, you will want to extract the provided compressed archives into the same folder, so that this folder structure can be seen:

```
. (root folder)
│
├── Artifact
│   ├─ bin
│   ├─ lib
│   ├─ dune-project
│   ├─ LICENSE
│   ├─ Makefile
│   └─ test
├── Dockerfile
└── README
```

Within the root directory run `docker image build -t ntextual .`, which will build a Docker image using the local Dockerfile (network connection required) and tag it as `ntextual`.

The development environment is almost ready. You may now run `docker container run -it ntextual /bin/bash`, which should position you in an interactive bash session within the container. You will be dropped in `/usr/local/ntextual` where you can find the folder `artifact` (copied in from `Artifact`).

### Building and running the program

To build the OCaml executable, you may use the `Makefile` provided within the artifact's subdirectory within the container. Running `make` or `make all` will build the executable and link to a file called `main` in the artifact's subdirectory. You may run `make clean` to remove all build files.

Below is a quick explanation of the simple CLI.

```
./main -b lin         -o output.txt       test/pcf-terms-1.txt test/pcf-terms-2.txt
       └┬───┘         └─┬─────────┘       └─┬─────────────────────────────────────┘
       -b or --base    -o or --outfile     inputs (filepaths) to source code
       one of: lin ;   takes a file path
       unr ; mix ; 
       aff ; rel .
       Default: mix.
```

### Interpreting results

Below is an example output of typechecking.

```
Typechecking results for test/simple-sess in BaseMixed:
Term type:
<Bool>
Under constraints:
{S(_1), S(_3), S(_6), Unit = Unit, _0 = !Bool._1, _2 = _0, _4 = ?_5._6, _4 =
~(_3), _7 = end?, (_5 * _6) = (_8 * _7), (_2 -> _1) = (_3 ->
end!)}
```

The program will inform you of all the files given to the typechecker ("for `test/simple-sess`"), of the used substructural base ("in `BaseMixed`"), of the produced type ("Term type: `<Bool>`"), and of the constraints generated ("Under constraints: `{[...]}`").

Provided in the `test` subfolder are multiple text files. Below is a table of expected outcomes for each file and substructural base.

| Test Name      | Unrestricted | Linear | Mixed | Affine | Relevant | Expected Type |
|---------------:|:------------:|:------:|:-----:|:------:|:--------:|--------------:|
| pcf-terms-1    | ✅ | ✅ | ✅ | ✅ | ✅ | Int |
| pcf-terms-2    | ✅ | ✅ | ✅ | ✅ | ✅ | Int |
| pcf-terms-3    | ✅ | ✅ | ✅ | ✅ | ✅ | Int * Int |
| pcf-terms-4    | ✅ | ❎ | ✅ | ❎ | ❎ | Int |
| simple-sess    | ✅ | ✅ | ✅ | ✅ | ✅ | Bool |
| shopper        | ✅ | ✅ | ✅ | ✅ | ✅ | Int |
| comm-violation | ❎ | ❎ | ❎ | ❎ | ❎ | N/A |
| tcp            | ✅ | ❎ | ✅ | ❎ | ❎ | Bool |

The `pcf-terms` test is a simple test for the core computational component of the calculus. The first three should succeed regardless of base, but `pcf-terms-4` fails in linear, affine, and relevant. This is because `z` is unused and `y` may be used twice in one branch of computation.

The `simple-sess` and `shopper` are both simple tests for session types. Since variable usage is linear, both are expected to typecheck correctly for all bases.

The `comm-violation` test should fail regardless of base because it introduces a communication violation. The feedback from the typechecker should identify the two protocols it is trying to reconcile.

The `tcp` test simulates a TCP handshake and uses dummy functions. Since the dummy functions are not defined, the typechecker infers their type based on usage. The results of the `tcp` test are identical to the results found in Appendix C of the paper.

## Troubleshooting & inspecting code

All the code for the programming language with typechecker can be found in `artifact/lib` and `artifact/bin`. In `artifact/bin` is just the CLI frontend, while `artifact/lib` contains most of the work. We will continue by omitting the prefix `artifact/`.

> [!TIP]
> If you make changes to `Artifact` (upper-case, on your host machine), the changes will not propagate to your Docker container until you rebuild the image and start a new container based on it.

The core language definitions like terms and types are in `lib/ast.ml`. The full pipeline from typechecking through unification is in `lib/cctx_typechecker.ml`. The lexer, written using `ocamllex`, arises from `lib/lexer.mll`. The parser, written using `menhir`, arises from `lib/parser.mly`. To use in the frontend, parsing is wrapped with helper functions in `lib/parse_wrapper.ml`.

### Typechecking and unification

Below is a small walkthrough of the code sections in `lib/cctx_typechecker.ml`.
1. auxiliary definitions L5,  
   Search with the following comment:
   ```ocaml
   (* Define errors relevant to type checking. *)
   ```
2. generalised merge functions L106,  
   Search with the following comment:
   ```ocaml
   (* Merge operators *)
   ```
3. generalised check functions L244,  
   Search with the following comment:
   ```ocaml
   (* Check operations *)
   ```
4. typechecking algorithm L285,  
   Search with the following comment:
   ```ocaml
   (* Typechecking section *)
   ```
5. unification auxiliaries L475,  
   Search with the following comment:
   ```ocaml
   (* Unification section *)
   ```
6. unification algorithm L620,  
   Search with the following comment:
   ```ocaml
   (* The unification algorithm. *)
   ```
7. generating most general unifier L709,  
   Search with the following definition:
   ```ocaml
   let resolveConstraints (constraints : TypC.t) : substitution list
   ```
8. pipeline wrapper connecting to `bin/main.ml` L758.  
   Search with the following definition:
   ```ocaml
   let finalCheck (l : linearityBase) (tm : term) : tcOut
   ```