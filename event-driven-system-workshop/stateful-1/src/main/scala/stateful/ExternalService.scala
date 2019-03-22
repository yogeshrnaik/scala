package stateful

object ExternalService {

  def record(action: Action): Unit = {
    Thread.sleep(1000)
  }
}
