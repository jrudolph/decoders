val scalaV = "2.13.5"
val scodecV = "1.11.7"
val scalatestV = "3.2.5"

libraryDependencies ++= Seq(
  "org.scodec" %% "scodec-core" % scodecV,
  "org.scalatest" %% "scalatest" % scalatestV % "test"
)

scalaVersion := scalaV

// docs

enablePlugins(ParadoxMaterialThemePlugin)

paradoxMaterialTheme in Compile := {
  ParadoxMaterialTheme()
    // choose from https://jonas.github.io/paradox-material-theme/getting-started.html#changing-the-color-palette
    .withColor("light-green", "amber")
    // choose from https://jonas.github.io/paradox-material-theme/getting-started.html#adding-a-logo
    .withLogoIcon("cloud")
    .withCopyright("Copyleft © Johannes Rudolph")
    .withRepository(uri("https://github.com/jrudolph/xyz"))
    .withSocial(
      uri("https://github.com/jrudolph"),
      uri("https://twitter.com/virtualvoid")
    )
}

paradoxProperties ++= Map(
  "github.base_url" -> (paradoxMaterialTheme in Compile).value.properties.getOrElse("repo", "")
)

fork in run := true