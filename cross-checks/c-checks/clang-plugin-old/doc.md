# Note

Modifying the AST is not what Clang really supports. Fundamentally the idea is procedurally climb over the AST and then generate a text patch, 
which will be emitted as a change when the AST is prettyprinted. You never modify the AST. This seems ok if you intend to make one simple kind 
of change to the code.If you want to do serious transformations, you’ll need to apply more than one transformation. While Clang’s scheme 
technically works, for me it is a bad way to do transformations.Transformations are functions from code to code. In mathematics, functions often 
compose to enable you build more complex functions.It should be that way too with Clang but text patching as a strategy makes that really awkward. 
If you want to run multiple transformations on code (because one transformation might apply to a program change/text patch), you have to parse it 
to an AST, run the first one, build a text patch, regenerate the entire source program back into a buffer, re-parse it, run the next transformation, etc. 
Ignoring the sheer clumsiness of this process, it can’t be very fast so you can’t really consider applying large number of transformations. 
Yet that’s where spectacular code changes (large scale optimizations, architecture changes, …) really occur.

## Solutions which I can think of -

Modifying the clang itself : Regarding the AST modifications, probably the best way to accomplish this is to inherit the TreeTransform class and 
override its Rebuild methods that are invoked to produce new statements for various AST nodes

**A more feasible option and nice to have for large scale code changes, a tool which-**

1. parses (compilable) C source code in a variety of dialects into ASTs,
2. preserves the preprocessor directives in most cases as AST nodes
3. can regenerate compilable C code (with comments and preprocessor directives) from the ASTs
4. provides full symbol table construction and access
5. provides procedural access to ASTs with a large AST manipulation library, including navigate, inspect, insert, delete, replace, match, ...
6. provides source-to-source transformations using patterns written in the C notation that match against the ASTs

