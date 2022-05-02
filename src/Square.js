import React from 'react';
import { colorToCss } from './Game';

//⭐

class Square extends React.Component {
    render() {
        const sizeFont = 30*this.props.size/40;
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
                    fontSize: sizeFont + "px",
                    width: this.props.size + "px",
                    height: this.props.size + "px",
                    margin: "0 5px 5px" 
                    }}>
                
                {this.props.emoji}
            </div>
        );
    }
}

export default Square;