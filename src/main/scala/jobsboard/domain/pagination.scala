package jobsboard.domain


object pagination {
  final case class Pagination(limit: Int, offset: Int)

  object Pagination {
    private val defaultSize = 20

    def tr(maybeLimit: Option[Int], maybeOffset: Option[Int]): Pagination = {
      new Pagination(maybeLimit.getOrElse(defaultSize), maybeOffset.getOrElse(0))
    }

      val default: Pagination = new Pagination(defaultSize, 0)
  }

}
