import React from 'react';
import { colorToCss } from './Game';

//⭐

class Square extends React.Component {
    render() {
        return (
            <div 
                onClick={() => {
                    if(this.props.onClick) {
                            this.props.onClick();
                        }
                    }
                } 
            
                style={{ 
                    backgroundColor: colorToCss(this.props.value),
                    textAlign: "center",
                    verticalAlign: "center",
                    fontSize: "30px",
                    width: "40px",
                    height: "40px",
                    margin: "0 5px 5px" 
                    }}>
                
                {this.props.emoji}
            </div>
        );
    }
}

export default Square;