package week2

trait Gates extends Simulation {

  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  class Wire {
    private var signal = false
    private var actions: List[Action] = List()

    def getSignal() = {
      this.signal
    }

    def setSignal(s: Boolean) = {
      if (s != this.signal) {
        this.signal = s
        actions foreach { action => action() }
      }
    }

    def addAction(a: Action) = {
      actions = a :: actions
      a()
    }
  }

  def inverter(in: Wire, out: Wire): Unit = {
    def inverterAction() = {
      val inValue = in.getSignal()
      afterDelay(InverterDelay) {
        out.setSignal(!inValue)
      }
    }

    in addAction inverterAction
  }

  def andGate(in1: Wire, in2: Wire, out: Wire): Unit = {
    def andAction() = {
      val in1Value = in1.getSignal()
      val in2Value = in2.getSignal()
      afterDelay(AndGateDelay) {
        out.setSignal(in1Value & in2Value)
      }
    }

    in1 addAction andAction
    in2 addAction andAction
  }

  def orGate(in1: Wire, in2: Wire, out: Wire): Unit = {
    def orAction() = {
      val in1Value = in1.getSignal()
      val in2Value = in2.getSignal()
      afterDelay(OrGateDelay) {
        out.setSignal(in1Value | in2Value)
      }
    }

    in1 addAction orAction
    in2 addAction orAction
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $getCurrentTime() value = ${wire.getSignal()}")
    }
    wire addAction probeAction
  }
}
