import xerial.sbt.Sonatype._

sonatypeProfileName := "io.github.dlomsak"

ThisBuild / publishMavenStyle := true

licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

sonatypeProjectHosting := Some(GitHubHosting("dlomsak", "regex-deriv", "dlomsak@gmail.com"))

// or if you want to set these fields manually
homepage := Some(url("https://dlomsak.github.io/regex-deriv/"))
scmInfo := Some(
  ScmInfo(
    url("https://github.com/dlomsak/regex-deriv"),
    "scm:git@github.com:dlomsak/regex-deriv.git"
  )
)
developers := List(
  Developer(id="dlomsak", name="Dan Lomsak", email="dlomsak@gmail.com", url=url("https://github.com/dlomsak"))
)