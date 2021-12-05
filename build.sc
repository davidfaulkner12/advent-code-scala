import mill._, scalalib._

object aoc2021 extends ScalaModule {
    def scalaVersion = "2.12.11"

    def ivyDeps = Agg(
      ivy"com.lihaoyi::fastparse:2.2.2"
    )

    object test extends Tests with TestModule.Utest {
      override def ammoniteVersion: T[String] = T("2.4.0")
      def ivyDeps = Agg(
        ivy"com.lihaoyi::utest:0.7.10"
      )
    }

    object integration extends Tests with TestModule.Utest {
      override def ammoniteVersion: T[String] = T("2.4.0")
      def ivyDeps = Agg(
        ivy"com.lihaoyi::utest:0.7.10"
      )
    }
}
