
### Setting up

From the repository `CFG-Checking`, run the following commands to ensure that you have all the necessary files.

    cp target/scala-2.11/cfgchecker_2.11-0.1.jar ../grammar-web/lib/
    cp lib/antlr-4.5-complete.jar ../grammar-web/lib/
    cp exercises-data/GrammarDatabase.xml ../grammar-web/public/resources/
    cp exercises-data/*.gram ../grammar-web/public/resources/


### Running the web interface.

Inside the folder `grammar-web`, run the following command:

`sbt runServer`

and connect in your browser to

`localhost:9000`

### Modifying the source of the javascript

If you plan to work on the javascript source, run inside another shell:

`sbt ~fastOptJS`

This will ensure that sources are recompiled each time.


## Developing and debugging in branch `scala-js-grammar-edit`

### Setting up SBT

1. Open the folder grammar-web in two shells
2. In the first one, type  
   `sbt -jvm-debug 9999 ~runServer`
3. In the other one, type  
   `sbt ~fastOptCopy`

2 runs the server continuously (but you may need to restart it if you modify the server source code), 3 recompile the files to javascript each time you modify it.

### Setting up IntelliJ
1. Make sure the SBT and scala plug-in are installed (`File > Settings > Plug-in > Browse repositories` ...)
2. Import the grammar-web project as an SBT project. It should automatically detect the folder `js` as a sub-module. If not, `File > Project structure > Modules`, the `green +` above modules, and add the `js` folder.
3. If `com.scalawarrior` is not defined in the imports of `grammar.scala`, thanks to SBT there should be a project defining it. `File > Project structure > Modules`, click on `js`, dependencies tab and then click on the `green +` to the right `> JAR or directories`. Look for `[UserHome]/.sbt/0.13/staging/e494b68ee50dee7d201d/scalajs-ace/target/scala-2.11/classes` or equivalent.
4. To debug the backend, create a debug configuration. Go to `Run > Edit configuration`. Click on the `green + > Remote`. Rename it to something like `GrammarWeb`, and make sure the `Settings > Transport` is set to `Socket`, `Debugger mode` set to `Attach`, the `host` to `localhost` and the `Port` to `9999`. Press OK, and then `Run > Debug` or `SHIFT+F9` to connect to the instance.