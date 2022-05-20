import React from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import Stack from './Stack';
import swal from 'sweetalert';
import Logo from "./loading.gif";
/**
 * List of colors.
 */

const colors = ["r", "v", "p", "g", "b", "y"];  // red, violet, pink, green, blue, yellow

/**
 * Returns the CSS representation of the received color.
 */

export function colorToCss(color) {
  switch (color) {
    case "r": return "#23362B"; //was red
    case "v": return "#1BB28C"; //was violet
    case "p": return "#E86A58"; //was pink
    case "g": return "#FED45B"; //was green
    case "b": return "#9BC7C5"; //was blue
    case "y": return "#EFEEEA"; //was yellow
    default: return color;      //we add this line to avoid warning.
  }
}
class Game extends React.Component {

  pengine;

  constructor(props) {
    super(props);
    this.state = {
      turns: 0,
      points: 0,
      pointsWithHelp: 0,
      grid: null,
      cantFilas: 0,
      cantColumnas: 0,
      history: [],
      help: [],
      emoji: "⭐",
      complete: false,  // true if game is complete, false otherwise
      waiting: false,
      playable: false,  // true after origin was picked
      origin: undefined,
      defaultInitialization: false
    };
    this.handleClick = this.handleClick.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    this.pengine = new PengineClient(this.handlePengineCreate);
  }

  handlePengineCreate() {
    const queryS = 'init(Grid)';
    this.pengine.query(queryS, (success, response) => {
      const grilla = response['Grid'];
      if (success) {
        this.setState({
          grid: grilla,
          cantFilas: grilla.length,
          cantColumnas: grilla[0].length //Se asume grilla rectangular
        });
      }
    });
  }

  handleClick(color) {
    // No action on click if game is complete or we are waiting.
    // Also no action if we pushed he button of the current color.
    if (this.state.complete || this.state.waiting || (this.state.history.length > 0 && color === this.state.history[0])) {
      return;
    }

    //if game is not playable when first clicked on a color button
    //We initialize in 0,0
    if(!this.state.playable) {
      const gridString = JSON.stringify(this.state.grid).replaceAll('"', "");
      const queryInit = 'inicializar(' + gridString + ',' + this.state.cantFilas + ',' + this.state.cantColumnas + ', 0, 0, AdyacenciasIniciales)';

      this.setState({waiting: true});

      this.pengine.query(queryInit, (success, responseInit) => {
        if(success) {
          this.setState({
            points: responseInit['AdyacenciasIniciales'],
            complete: responseInit['AdyacenciasIniciales'] === this.state.cantFilas*this.state.cantColumnas,
            playable: true,
            defaultInitialization: true,
            waiting: false
          });
        } else {
          this.setState({waiting: false});
        }
        if (this.state.complete){
          this.handleWin();
        }
      });
    }
    // Build Prolog query to apply the color flick.
    // The query will be like:
    // flick([[g,g,b,g,v,y,p,v,b,p,v,p,v,r],
    //        [r,r,p,p,g,v,v,r,r,b,g,v,p,r],
    //        [b,v,g,y,b,g,r,g,p,g,p,r,y,y],
    //        [r,p,y,y,y,p,y,g,r,g,y,v,y,p],
    //        [y,p,y,v,y,g,g,v,r,b,v,y,r,g],
    //        [r,b,v,g,b,r,y,p,b,p,y,r,y,y],
    //        [p,g,v,y,y,r,b,r,v,r,v,y,p,y],
    //        [b,y,v,g,r,v,r,g,b,y,b,y,p,g],
    //        [r,b,b,v,g,v,p,y,r,v,r,y,p,g],
    //        [v,b,g,v,v,r,g,y,b,b,b,b,r,y],
    //        [v,v,b,r,p,b,g,g,p,p,b,y,v,p],
    //        [r,p,g,y,v,y,r,b,v,r,b,y,r,v],
    //        [r,b,b,v,p,y,p,r,b,g,p,y,b,r],
    //        [v,g,p,b,v,v,g,g,g,b,v,g,g,g]],r, X, Grid)
    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    const queryS = "flick(" + gridS + "," + color + ", NroAdyacencias, Grid)";
    
    //Despues de configurar la consulta, entramos en estado waiting
    this.setState({
      waiting: true
    });

    //Disparamos la consulta
    this.pengine.query(queryS, (success, response) => {
      //Si fue exitosa...
      if (success) {
        //Actualizamos el estado
        this.setState({
          grid: response['Grid'],
          points: response['NroAdyacencias'],
          complete: response['NroAdyacencias'] === this.state.cantFilas*this.state.cantColumnas,
          turns: this.state.turns + 1,
          waiting: false
        });

        //Generamos una consulta para actualizar el historial.
        const stackS = JSON.stringify(this.state.history).replaceAll('"', "");
        const queryStack = 'push(' + stackS + ', ' + color + ', NewStack)'
        
        this.setState({
          waiting: true
        });

        //La disparamos
        this.pengine.query(queryStack, (successStack, responseStack) => {
          if(successStack) {
            //Si tuvimos exito, actualizamos el historial.
            this.setState({history: responseStack['NewStack'], waiting: false});
          } else {
            this.setState({waiting: false});
          }
        });

        if (this.state.complete){
          this.handleWin();
        }
      } else {
        //Si la consulta a prolog fallo, no cambiamos nada, pero aun asi salimos del estado waiting.
        this.setState({ waiting: false });
      }
    });
  }

  handleHelp() {
    var element1 = document.getElementById("loadingGif");
    var element2 = document.getElementById("arrayHistorial");
    element2.style.display = "none";
    element1.style.display = "block";
    var depth= parseInt(document.getElementById("profundidad").value);
    console.log(depth);
    
    if(this.state.waiting || this.state.complete) {
      return;
    }

    if(this.state.origin === undefined) {
      const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
      const queryS = "mejor_origen(" + gridS + ", MF, MC)";
      console.log(queryS);
      this.setState({waiting: true});
      
      this.pengine.query(queryS, (success, response) => {
        if(success) {
          console.log(response);
          console.log(response['MF'] + ", " + response['MC']);
          element1.style.display = "none";
          element2.style.display = "block";

          //ACA :)

          this.setState({waiting: false});
        } else {
          this.setState({waiting: false});
        }
      });

      return;
    }

    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    const queryS = "mejor_camino(" + gridS + "," + depth + ", Soluciones, NroAdyacencias)";
    console.log(queryS);
    this.setState({waiting: true});

    this.pengine.query(queryS, (success, response) => {
      if(success) {
        element1.style.display = "none";
        element2.style.display = "block";
        let ady = response['NroAdyacencias'];
        let sec = response['Soluciones'];
        this.setState({waiting: false, help: sec, pointsWithHelp: ady});
      } else {
        this.setState({waiting: false});
      }
    });
  }

  handleEmoji(emj) {
    console.log(emj);
    this.setState({emoji: emj});
  }

  handleWin(){
    // swal({
    //   title: "🎉Ganaste🎉",
    //   text: "Felicidades, has ganado el juego en "+this.state.turns+" turnos",
    //   buttons: ["Okey","Volver a intentarlo"],
    // });

    swal({
      title: "🎉Ganaste🎉",
      text: "Felicidades, has ganado el juego en "+this.state.turns+" turnos",
      buttons: {
        cancel: "Finalizar juego",
        restart: {
          text: "Volver a intentarlo",
          value: "restart",
        },
      },
    })
    .then((value) => {
      if (value==="restart"){
        window.location.reload()
      }
        });
  }


  render() {
    if (this.state.grid === null) {
      return null;
    }
    return (
      
      <div className="game">
        <div className="leftPanel">
          <div className="buttonsPanel">
            {colors.map(color =>
              <button
                className="colorBtn"
                style={{ backgroundColor: colorToCss(color) }}
                //onClick={() => this.handleWin()}
                onClick={() => this.handleClick(color)}
                key={color}
              />)}
          </div>
          <div className="turnsPanel">
            <div className="turnsLab">Turns:</div>
            <div className="turnsNum">{this.state.turns}</div>
          </div>
          <div className="pointsPanel">
            <div className="pointsLab">Points:</div>
            <div className="pointsNum">{this.state.points}</div>
          </div>
          <div className="emojisPanel">
            <div className="emojisLab">Selector:</div>
            <div className="emojisContainer">
              <button className="emojiButton" onClick={() => this.handleEmoji("⭐​​")}>⭐</button>
              <button className="emojiButton" onClick={() => this.handleEmoji("❤️​")}>❤️</button>
              <button className="emojiButton" onClick={() => this.handleEmoji("😎​")}>😎</button>
              <button className="emojiButton" onClick={() => this.handleEmoji("🐈​")}>🐈</button>
              <button className="emojiButton" onClick={() => this.handleEmoji("🦉​​")}>🦉</button>
              <button className="emojiButton" onClick={() => this.handleEmoji("🚀​​")}>🚀</button>
            </div>
          </div>
        </div>
        <Board
          cf={this.state.cantFilas}
          cc={this.state.cantColumnas}
          emoji={this.state.emoji}
          origin={this.state.origin} 
          grid={this.state.grid}
          defaultInitialization={this.state.defaultInitialization} 
          onOriginSelected = {
            this.state.playable ? undefined :
            origin => {
              //Pasamos como propiedad de Board la funcionalidad necesaria para la inicializacion:
              const fila = origin[0];
              const columna = origin[1];
              this.setState({
                origin: origin
              });

              const gridString = JSON.stringify(this.state.grid).replaceAll('"', "");
              const queryInit = 'inicializar(' + gridString + ',' + this.state.cantFilas + ',' + this.state.cantColumnas + ',' + fila + ',' + columna + ', AdyacenciasIniciales)';

              this.setState({waiting: true});

              this.pengine.query(queryInit, (success, responseInit) => {
                if(success) {
                  this.setState({
                    points: responseInit['AdyacenciasIniciales'],
                    complete: responseInit['AdyacenciasIniciales'] === this.state.cantFilas*this.state.cantColumnas,
                    playable: true,
                    waiting: false
                  });
                } else {
                  this.setState({waiting: false});
                }
                if (this.state.complete){
                  this.handleWin();
                }
              });
            }
          }
        />
        <div className='rightPanel'>
          <div className='stackLabel'>Historial:</div>
          <Stack array={this.state.history}/>
        </div>
        <div className='rightRightPanel'>
          <div className='stackLabel'>Ayuda:</div>
          <input className='inputField' type="number" id="profundidad" min="0" defaultValue="3" />
          <button className='helpButton' onClick={() => this.handleHelp()}>Ayuda</button>
          
          <div className="pointsLab">Resultado:</div>
          <div className="pointsNum">{this.state.pointsWithHelp > 0 ? this.state.pointsWithHelp : '-'}</div>
          
          <div className='bloqueGif' id="loadingGif"><img src={Logo} /></div>
          
          <div id="arrayHistorial">  <Stack  array={this.state.help}/> </div>
        </div>
      </div>
    );
  }
}

export default Game;