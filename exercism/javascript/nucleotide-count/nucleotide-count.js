'use strict';

function DNA (strand) {
    this.nucleotideCounts = {
        A: strand.replace(/[^A]/g, '').length,
        T: strand.replace(/[^T]/g, '').length,
        C: strand.replace(/[^C]/g, '').length,
        G: strand.replace(/[^G]/g, '').length
    };
}

DNA.prototype.count = function (symbol) {
    var count;

    if (symbol === 'U') {
        count = 0;
    } else if (symbol in this.nucleotideCounts) {
        count = this.nucleotideCounts[ symbol ];
    } else if (!(symbol in this.nucleotideCounts)) {
        throw new Error('Invalid Nucleotide');
    }

    return count;
};

module.exports = DNA;
