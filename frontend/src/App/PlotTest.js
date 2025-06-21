"use strict";

import Plotly from 'plotly.js-dist-min'

import _ from 'lodash'

export const _getStepPlotDivId = (ys) => {
    const plotDivId = "stepsPlot"
    
    const stepsPlot = document.getElementById(plotDivId);
    if(stepsPlot){
        Plotly.newPlot( stepsPlot, 
            [{
                x: _.range(_.size(ys)),
                y: ys
            }],
            { margin: { t: 0 } }
        );
    }

    return plotDivId
}
