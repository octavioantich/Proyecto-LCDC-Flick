import React from 'react';
import { colorToCss } from './Game';

//⭐

class Square extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
          text: ""
        };
    }

    addStar = () => {
        this.setState({text: "⭐"});
     };

    render() {
        return (
            <div 
                onClick={() => {
                    if(this.props.onClick) {
                        this.props.onClick();
                        this.addStar();}
                    }
                } 
            
                style={{ 
                    backgroundColor: colorToCss(this.props.value),
                    textAlign: "center",
                    verticalAlign: "center",
                    fontSize: "30px" 
                    }}>
                
                {this.state.text}
            </div>
        );
    }
}

export default Square;