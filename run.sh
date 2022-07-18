scala-cli package plugin.scala -o plugin.jar --library -f &&\
  zip plugin.jar plugin.properties &&\
  scala3-compiler plugin.scala -Xplugin:plugin.jar 