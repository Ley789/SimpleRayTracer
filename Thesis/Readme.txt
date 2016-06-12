Contents
---------------------------------------------------------
    - clb.pdf     - a copy of the thesis
    - source.zip  - the source code


Installation instructions
---------------------------------------------------------

Software needed to compile toplevel:
 haskell stack tool version 1.1.2

A introduction of the installation of stack can be found in the web site
http://docs.haskellstack.org/en/stable/README/

Extract source.zip in an arbitrary order. There a folder named "raytracer" will
be created. Execute following commands to compile the program.
- stack setup
    this command will install a local GHC compiler.
- stack build
    this command will compile the source code found in the subfolder src.
- stack exec raytracer-exe
    this will execute the executable. To send command-line arguments args to the executable
    use following command.
- stack exec -- raytracer-exe args

The Main.hs contained in the subfolder app defines different scenes in Diagrams
and is passed to the multirendere defined in the module CmdLine. This allows
to specify the details about the diagram at the start of the program.
