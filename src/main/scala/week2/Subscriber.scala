package week2

/**
 * Created by mateus on 6/9/15.
 */
trait Subscriber {

  def handler(publisher: Publisher)
}
