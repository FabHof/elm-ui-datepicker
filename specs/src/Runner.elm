port module Runner exposing (program, browserProgram)

import Spec.Runner exposing (Message, Flags, Model, Msg)
import Spec exposing (Spec)

port elmSpecOut : Message -> Cmd msg
port elmSpecIn : (Message -> msg) -> Sub msg
port elmSpecPick : () -> Cmd msg

config : Spec.Runner.Config msg
config =
  { send = elmSpecOut
  , outlet = elmSpecOut
  , listen = elmSpecIn
  }

pick =
  Spec.Runner.pick elmSpecPick

program specs =
  Spec.Runner.program config specs

browserProgram : List (Spec model msg) -> Program Flags (Model model msg) (Msg msg)
browserProgram specs =
  Spec.Runner.browserProgram config specs