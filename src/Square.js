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
            <div onClick={this.props.onClick} style={{ backgroundColor: colorToCss(this.props.value) }}>
                {this.state.text}
            </div>
        );
    }
}

export default Square;