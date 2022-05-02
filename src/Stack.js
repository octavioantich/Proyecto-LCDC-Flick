import React from 'react';
import Square from './Square';

class Stack extends React.Component {
    render() {
        return (
            <div className="stack">
                {this.props.array.map((cell, i) =>
                    <Square
                        size={40}
                        value={cell}
                        key={i}
                    />
                )}
            </div>
        );
    }
}

export default Stack;