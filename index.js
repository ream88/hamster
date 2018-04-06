import Elm from './src/Main.elm'

const app = Elm.Main.fullscreen()

app.ports.infoForOutside.subscribe((msg) => {
  switch (msg.tag) {
    case 'Eval':
      try {
        eval(msg.data)
      } catch (e) {
        console.log(e)
      }
      break

    default:
      console.warn('Unknown infoForOutside: ' + msg.tag)
  }
})

function vor () {
  app.ports.infoForElm.send({tag: 'CommandCalled', data: 'Go'})
}

function linksUm () {
  app.ports.infoForElm.send({tag: 'CommandCalled', data: 'RotateLeft'})
}
