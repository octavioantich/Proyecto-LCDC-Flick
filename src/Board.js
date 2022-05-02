import React from 'react';
import Square from './Square';

class Board extends React.Component {
    render() {
        const origin = this.props.origin;
        return (
            <div className="board">
                {this.props.grid.map((row, i) =>
                    row.map((cell, j) =>
                        <Square
                            value={cell}
                            key={i + "." + j}
                            onClick={this.props.onOriginSelected && (() => this.props.onOriginSelected([i, j]))}
                            emoji = {origin && origin[0] === i && origin[1] === j ? this.props.emoji : undefined}
                        />
                    )
                )}
            </div>
        );
    }
}

export default Board;