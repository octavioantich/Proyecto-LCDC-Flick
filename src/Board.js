import React from 'react';
import Square from './Square';

class Board extends React.Component {

    render() {
        const origin = this.props.origin;
        const pixelsMax = 560;
        const divisor = Math.max(this.props.cc, this.props.cf);
        const px = Math.floor(pixelsMax / divisor);
        return (
            <div className="board"
                style={{
                    margin: '20px',
                    display: 'grid',
                    gridTemplateColumns: 'repeat(' + this.props.cc + ',' + px + 'px)',
                    gridTemplateRows: 'repeat(' + this.props.cf + ', ' + px + 'px)',
                }}
            >
                {this.props.grid.map((row, i) =>
                    row.map((cell, j) =>
                        <Square
                            size={px}
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