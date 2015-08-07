
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