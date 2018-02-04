# RMapViewerS
ReactionMapViewer application ported to Scala

# Installation

RMapViewerS is an application that runs on a Java VM.
To install it, please follow the steps below.

1. Install Java Runtime Environment

Please install a Java runtime environment depending on your OS/platform.
If you are not sure, please visit https://www.java.com/ .

2. Download RMapViewerS jar

Please download the latest jar file from [Releases page](https://github.com/ReactionMap/RMapViewerS/releases) and place it in a directory for RMapViewerS.

3. Download runtime jars

Please download the following jar files into the same directory as you placed RMapViewerS.jar.
Please note that each library has its own copyright and license.

* Jmol.jar (version 10.x) (home page: http://jmol.sourceforge.net/ , [download](https://sourceforge.net/projects/jmol/files/Jmol/Version%2010.2.0/))
* RMapJmol.jar (home page: https://github.com/ReactionMap/RMapJmol , [download](https://github.com/ReactionMap/RMapJmol/releases))
* jackson-core-2.3.1.jar (homepage: https://github.com/FasterXML/jackson-core , [download](https://mvnrepository.com/artifact/com.fasterxml.jackson.core/jackson-core/2.3.1))
* jackson-databind-2.3.1.jar (homepage: https://github.com/FasterXML/jackson-databind , [download](https://mvnrepository.com/artifact/com.fasterxml.jackson.core/jackson-databind/2.3.1))
* jackson-annotations-2.3.0.jar (homepage: https://github.com/FasterXML/jackson-annotations , [download](https://mvnrepository.com/artifact/com.fasterxml.jackson.core/jackson-annotations/2.3.0))
* json4s-ast_2.10-3.2.10.jar (homepage: https://github.com/json4s/json4s , [download](https://mvnrepository.com/artifact/org.json4s/json4s-ast_2.10/3.2.10))
* skinny-http-client_2.10-2.5.2.jar (homepage: http://skinny-framework.org/documentation/http-client.html , [download](https://mvnrepository.com/artifact/org.skinny-framework/skinny-http-client_2.10/2.5.2))
* skinny-micro-common_2.10-1.2.8.jar (homepage: http://skinny-framework.org/ , [download](https://mvnrepository.com/artifact/org.skinny-framework/skinny-micro_2.10/1.2.8))
* slf4j-api-1.7.25.jar (homepage: https://www.slf4j.org/ , [download](https://mvnrepository.com/artifact/org.slf4j/slf4j-api/1.7.25))
* slf4j-simple-1.6.4.jar (homepage: https://www.slf4j.org/ , [download](https://mvnrepository.com/artifact/org.slf4j/slf4j-simple/1.7.25))

4. Launch RMapViewerS

Please launch RMapViewerS.jar. On many OSs with GUI, you can just double-click on the RMapViewerS.jar file.
On a command-line terminal, you can go to the directory that you installed RMapViewerS.jar and type

```
java -jar RMapViewerS.jar
```

Enjoy!
